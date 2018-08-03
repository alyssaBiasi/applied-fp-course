{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, Method, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types           (ContentType (PlainTextContent), Error (..), RqType (..),
                                           mkCommentText, mkTopic, getTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse s content str = responseLBS s [("Content-Type", renderContentType content)] str

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest txt byteString = AddRq <$> eitherTopic <*> eitherComment
--mkAddRequest txt byteString = pure AddRq <*> eitherTopic <*> eitherComment
  where
    eitherTopic = mkTopic txt
    eitherComment = mkCommentText (lazyByteStringToStrictText byteString)
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict
--mkAddRequest txt byteString = eitherTopic >>=
--                                \t -> eitherComment >>=
--                                    \c -> pure $ AddRq t c
--mkAddRequest txt byteString = do
--    topic <- eitherTopic
--    comment <- eitherComment
--    pure $ AddRq topic comment
--  where
--    eitherTopic = mkTopic txt
--    eitherComment = mkCommentText (lazyByteStringToStrictText byteString)
--    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
--    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.

mkViewRequest :: Text -> Either Error RqType
mkViewRequest = (fmap ViewRq) . mkTopic

mkListRequest :: Either Error RqType
mkListRequest = pure ListRq

--data Error = EmptyTopic | EmptyComment
mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic = resp400 PlainTextContent "No topic"
mkErrorResponse EmptyComment = resp400 PlainTextContent "No comment"
mkErrorResponse UnknownRequest = resp400 PlainTextContent "Unknown request"
--mkErrorResponse _ = resp400 PlainTextContent "Invalid request"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
--POST /<topic>/add
--GET  /<topic>/view
--GET  /list
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest rq = do
  body <- strictRequestBody rq
  let path = pathInfo rq
      method = requestMethod rq
  pure $ getRequest body path method
--mkRequest rq =
--  let path = pathInfo rq
--  in case path of
--        ["list"] -> pure mkListRequest
--        [topic, "view"] -> pure $ mkViewRequest topic
--        [topic, "add"] -> fmap (mkAddRequest topic) (strictRequestBody rq)
--        _ -> pure $ Left EmptyTopic
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.

getRequest :: LBS.ByteString -> [Text] -> Method -> Either Error RqType
getRequest _    ["list"]        "GET" = mkListRequest
getRequest _    [topic, "view"] "GET" = mkViewRequest topic
getRequest body [topic, "add"]  "POST" = mkAddRequest topic body
getRequest _ _ _ = Left UnknownRequest

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest ListRq = Right $ resp200 PlainTextContent "hello"
handleRequest (ViewRq _) = Right $ resp200 PlainTextContent "X not implemented"
handleRequest (AddRq _ _) = Right $ resp200 PlainTextContent "X not implemented"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app =
  error "app not reimplemented"

runApp :: IO ()
runApp = run 3000 app
