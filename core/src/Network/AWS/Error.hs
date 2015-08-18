{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- |
-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
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

httpStatus :: AsError a => Getting (First Status) a Status
httpStatus = _Error . f
  where
    f g = \case
        TransportError (StatusCodeException s h c)
            -> TransportError <$> (StatusCodeException <$> g s <*> pure h <*> pure c)
        TransportError e
            -> pure (TransportError e)
        SerializeError (SerializeError' a s e)
            -> g s <&> \x -> SerializeError (SerializeError' a x e)
        ServiceError e
            -> g (_serviceStatus e) <&> \x -> ServiceError (e { _serviceStatus = x })

hasStatus :: (Applicative f, Choice p)
          => Int
          -> Optic' p f ServiceError ServiceError
hasStatus n = filtered ((n ==) . fromEnum . _serviceStatus)

hasCode :: (Applicative f, Choice p)
        => ErrorCode
        -> Optic' p f ServiceError ServiceError
hasCode c = filtered ((c ==) . _serviceCode)

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
        let e = strip <$> c
        m <- msg e o
        return $! serviceError a s h e m Nothing

    strip (ErrorCode x) = ErrorCode $
        case Text.break (== '#') x of
            (ns, e) | Text.null e -> ns
                    | otherwise   -> Text.drop 1 e

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
        either (SerializeError . SerializeError' a s)
               ServiceError
               e
