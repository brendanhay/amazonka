-- |
-- Module      : Amazonka.Error
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Amazonka.Error where

import Amazonka.Data
import Amazonka.Lens (Choice, Getting, Optic', filtered)
import Amazonka.Prelude
import Amazonka.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Client as Client
import Network.HTTP.Types.Status (Status (..))

-- | Provides a generalised prism for catching a specific service error
-- identified by the opaque service abbreviation and error code.
--
-- This can be used if the generated error prisms provided by
-- @Amazonka.<ServiceName>.Types@ do not cover all the thrown error codes.
-- For example to define a new error prism:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > import Amazonka.S3 (ServiceError, s3)
-- >
-- > _NoSuchBucketPolicy :: AsError a => Getting (First ServiceError) a ServiceError
-- > _NoSuchBucketPolicy = _MatchServiceError s3 "NoSuchBucketPolicy"
--
-- With example usage being:
--
-- >>> import Control.Exception.Lens (trying)
-- >>> :t trying _NoSuchBucketPolicy
-- MonadCatch m => m a -> m (Either ServiceError a)
_MatchServiceError ::
  AsError a =>
  Service ->
  ErrorCode ->
  Getting (First ServiceError) a ServiceError
_MatchServiceError s c = _ServiceError . hasService s . hasCode c

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 400

_HttpStatus :: AsError a => Getting (First Status) a Status
_HttpStatus = _Error . f
  where
    f g = \case
      TransportError (Client.HttpExceptionRequest rq (Client.StatusCodeException rs b)) ->
        (\x -> TransportError (Client.HttpExceptionRequest rq (Client.StatusCodeException (rs {Client.responseStatus = x}) b)))
          <$> g (Client.responseStatus rs)
      --
      TransportError e ->
        pure (TransportError e)
      --
      SerializeError (SerializeError' a s b e) ->
        (\x -> SerializeError (SerializeError' a x b e)) <$> g s
      --
      ServiceError e ->
        (\x -> ServiceError (e {_serviceErrorStatus = x}))
          <$> g (_serviceErrorStatus e)

hasService ::
  (Applicative f, Choice p) =>
  Service ->
  Optic' p f ServiceError ServiceError
hasService s = filtered ((_serviceAbbrev s ==) . _serviceErrorAbbrev)

hasStatus ::
  (Applicative f, Choice p) =>
  Int ->
  Optic' p f ServiceError ServiceError
hasStatus n = filtered ((n ==) . fromEnum . _serviceErrorStatus)

hasCode ::
  (Applicative f, Choice p) =>
  ErrorCode ->
  Optic' p f ServiceError ServiceError
hasCode c = filtered ((c ==) . _serviceErrorCode)

serviceError ::
  Abbrev ->
  Status ->
  [Header] ->
  Maybe ErrorCode ->
  Maybe ErrorMessage ->
  Maybe RequestId ->
  ServiceError
serviceError a s h c m r =
  ServiceError' a s h (fromMaybe (getErrorCode s h) c) m (r <|> getRequestId h)

getRequestId :: [Header] -> Maybe RequestId
getRequestId h =
  either (const Nothing) Just (h .# hAMZRequestId <|> h .# hAMZNRequestId)

getErrorCode :: Status -> [Header] -> ErrorCode
getErrorCode s h =
  case h .# hAMZNErrorType of
    Left _ -> newErrorCode (toText (statusMessage s))
    Right x -> newErrorCode x

parseJSONError ::
  Abbrev ->
  Status ->
  [Header] ->
  ByteStringLazy ->
  Error
parseJSONError a s h bs =
  decodeError a s h bs (parse bs)
  where
    parse =
      eitherDecode'
        >=> Aeson.Types.parseEither (Aeson.withObject "JSONError" go)

    go o = do
      e <- (Just <$> o .: "__type") <|> o .:? "code"
      m <- msg e o

      pure (serviceError a s h e m Nothing)

    msg c o =
      if c == Just "RequestEntityTooLarge"
        then pure (Just "Request body must be less than 1 MB")
        else
          Just <$> o .: "message"
            <|> o .:? "Message"

parseXMLError ::
  Abbrev ->
  Status ->
  [Header] ->
  ByteStringLazy ->
  Error
parseXMLError a s h bs = decodeError a s h bs (decodeXML bs >>= go)
  where
    go x =
      serviceError a s h
        <$> code x
        <*> may' (firstElement "Message" x)
        <*> may' (firstElement "RequestId" x <|> firstElement "RequestID" x)

    code x =
      Just <$> (firstElement "Code" x >>= parseXML)
        <|> return root

    root = newErrorCode <$> rootElementName bs

    may' (Left _) = pure Nothing
    may' (Right x) = Just <$> parseXML x

parseRESTError ::
  Abbrev ->
  Status ->
  [Header] ->
  a ->
  Error
parseRESTError a s h _ =
  ServiceError (serviceError a s h Nothing Nothing Nothing)

decodeError ::
  Abbrev ->
  Status ->
  [Header] ->
  ByteStringLazy ->
  Either String ServiceError ->
  Error
decodeError a s h bs e
  | LBS.null bs = parseRESTError a s h bs
  | otherwise =
    either
      (SerializeError . SerializeError' a s (Just bs))
      ServiceError
      e
