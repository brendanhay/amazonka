{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Error where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types            (parseEither)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.XML
import           Network.AWS.Types
import           Network.HTTP.Client
import           Network.HTTP.Types.Status   (Status (..))

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 300

httpStatus :: AWSError a => Getting (First Status) a Status
httpStatus = _Error . f
  where
    f g = \case
        HTTPError (StatusCodeException s h c)
            -> HTTPError <$> (StatusCodeException <$> g s <*> pure h <*> pure c)
        HTTPError e
            -> pure (HTTPError e)
        SerializerError (SerializerError' a s e)
            -> g s <&> \x -> SerializerError (SerializerError' a x e)
        ServiceError e
            -> g (_errorStatus e) <&> \x -> ServiceError (e { _errorStatus = x })

hasStatus :: (Applicative f, Choice p)
          => Int
          -> Optic' p f ServiceError ServiceError
hasStatus n = filtered ((n ==) . fromEnum . _errorStatus)

hasCode :: (Applicative f, Choice p)
        => ErrorCode
        -> Optic' p f ServiceError ServiceError
hasCode c = filtered ((c ==) . _errorCode)

serviceError :: Abbrev
             -> Status
             -> [Header]
             -> Maybe ErrorCode
             -> Maybe ErrorMessage
             -> Maybe RequestId
             -> ServiceError
serviceError a s h c m r =
    ServiceError' a s h (fromMaybe (getErrorCode s h) c) m (r <|> getRequestId h)

getRequestId :: [Header] -> Maybe RequestId
getRequestId h =
    either (const Nothing) Just (h .# hAMZRequestId <|> h .# hAMZNRequestId)

getErrorCode :: Status -> [Header] -> ErrorCode
getErrorCode s h =
    fromMaybe (ErrorCode . Text.decodeUtf8 $ statusMessage s) $
        case h .# hAMZNErrorType of
            Left  _ -> Nothing
            Right t -> fmap ErrorCode . lastOf traverse $ Text.split (== '#') t

parseJSONError :: Abbrev
               -> Status
               -> [Header]
               -> LazyByteString
               -> Error
parseJSONError a s h bs = decodeError a s h bs (parse bs)
  where
    parse = eitherDecode' >=> parseEither (withObject "JSONError" go)

    go o = do
        c <- (Just <$> o .: "__type") <|> o .:? "code"
        m <- msg c o
        return $! serviceError a s h c m Nothing

    msg c o =
        if c == Just "RequestEntityTooLarge"
            then pure (Just "Request body must be less than 1 MB")
            else Just <$> o .:  "message"
                      <|> o .:? "Message"

parseXMLError :: Abbrev
              -> Status
              -> [Header]
              -> LazyByteString
              -> Error
parseXMLError a s h bs = decodeError a s h bs (decodeXML bs >>= go)
  where
    go x = do
        (c, m, r) <- xml x <|> ec2 x
        return $! serviceError a s h c m r

    xml x = withElement "Error"  (gen x) x <|> gen x x
    ec2 x = withElement "Errors" (gen x) x

    gen x y = (,,)
        <$> y .@? "Code"
        <*> y .@? "Message"
        <*> x .@? "RequestId"

parseRESTError :: Abbrev
               -> Status
               -> [Header]
               -> a
               -> Error
parseRESTError a s h _ =
    ServiceError (serviceError a s h Nothing Nothing Nothing)

decodeError :: Abbrev
            -> Status
            -> [Header]
            -> LazyByteString
            -> Either String ServiceError
            -> Error
decodeError a s h bs e
    | LBS.null bs = parseRESTError a s h bs
    | otherwise   =
        either (SerializerError . SerializerError' a s)
               ServiceError
               e
