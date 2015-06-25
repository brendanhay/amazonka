{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Error where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types            (parseEither)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Data.Typeable
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Headers
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.HTTP.Client
import           Network.HTTP.Types.Status   (Status (..))

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 300

-- | Abbreviated service name.
newtype Abbrev = Abbrev Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromXML
        , FromJSON
        , FromText
        , ToText
        , ToBuilder
        )

newtype ErrorCode = ErrorCode Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromXML
        , FromJSON
        , FromText
        , ToText
        , ToBuilder
        )

newtype ErrorMessage = ErrorMessage Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromXML
        , FromJSON
        , FromText
        , ToText
        , ToBuilder
        )

newtype RequestId = RequestId Text
    deriving
        ( Eq
        , Ord
        , Show
        , Generic
        , IsString
        , FromXML
        , FromJSON
        , FromText
        , ToText
        , ToBuilder
        )

-- | An error type representing the subset of errors that can be directly
-- attributed to this library.
data Error
    = HTTPError       HttpException
    | SerializerError Abbrev Status String
    | ServiceError    ServiceError
      deriving (Show, Typeable)

instance Exception Error

class AWSError a where
    _Error           :: Prism' a Error
    _HTTPError       :: Prism' a HttpException
    _SerializerError :: Prism' a (Abbrev, Status, String)
    _ServiceError    :: Prism' a ServiceError

    _HTTPError       = _Error . _HTTPError
    _SerializerError = _Error . _SerializerError
    _ServiceError    = _Error . _ServiceError

instance AWSError Error where
    _Error = id

    _HTTPError = prism
        HTTPError
        (\case
            HTTPError e -> Right e
            x           -> Left x)

    _SerializerError = prism
        (\(a, s, e) -> SerializerError a s e)
        (\case
            SerializerError a s e -> Right (a, s, e)
            x -> Left x)

    _ServiceError = prism
        ServiceError
        (\case
            ServiceError e -> Right e
            x              -> Left x)

httpStatus :: AWSError a => Getting (First Status) a Status
httpStatus = _Error . f
  where
    f g = \case
        HTTPError (StatusCodeException s h c)
            -> HTTPError <$> (StatusCodeException <$> g s <*> pure h <*> pure c)
        HTTPError e
            -> pure (HTTPError e)
        SerializerError a s e
            -> SerializerError a <$> g s <*> pure e
        ServiceError e
            -> g (_errorStatus e) <&> \x -> ServiceError (e { _errorStatus = x })

instance ToBuilder Error where
    build = \case
        HTTPError           e -> build e
        SerializerError a s x -> buildLines
            [ "[SerializerError] {"
            , "  service = " <> build a
            , "  status  = " <> build s
            , "  message = " <> build x
            ]
        ServiceError        e -> build e

data ServiceError = ServiceError'
    { _errorService   :: Abbrev
    , _errorStatus    :: Status
    , _errorHeaders   :: [Header]
    , _errorCode      :: ErrorCode
    , _errorMessage   :: Maybe ErrorMessage
    , _errorRequestId :: Maybe RequestId
    } deriving (Eq, Show, Typeable)

instance Exception ServiceError

instance ToBuilder ServiceError where
    build ServiceError'{..} = buildLines
        [ "[ServiceError] {"
        , "  service    = " <> build _errorService
        , "  status     = " <> build _errorStatus
        , "  code       = " <> build _errorCode
        , "  message    = " <> build _errorMessage
        , "  request-id = " <> build _errorRequestId
        ]

errorService :: Lens' ServiceError Abbrev
errorService = lens _errorService (\s a -> s { _errorService = a })

errorStatus :: Lens' ServiceError Status
errorStatus = lens _errorStatus (\s a -> s { _errorStatus = a })

errorHeaders :: Lens' ServiceError [Header]
errorHeaders = lens _errorHeaders (\s a -> s { _errorHeaders = a })

errorCode :: Lens' ServiceError ErrorCode
errorCode = lens _errorCode (\s a -> s { _errorCode = a })

errorMessage :: Lens' ServiceError (Maybe ErrorMessage)
errorMessage = lens _errorMessage (\s a -> s { _errorMessage = a })

errorRequestId :: Lens' ServiceError (Maybe RequestId)
errorRequestId = lens _errorRequestId (\s a -> s { _errorRequestId = a })

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
    | otherwise   = either (SerializerError a s) ServiceError e
