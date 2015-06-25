{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
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
    -- (
    -- -- * Status Checks
    --   statusSuccess
    -- , httpStatus
    -- , errorCode

    -- -- * Classes
    -- , AsError      (..)

    -- -- * Types
    -- , Error        (..)
    -- , ErrorCode    (..)
    -- , ErrorType    (..)

    -- -- * REST Errors
    -- , RESTError
    -- , restRequestId
    -- , restType
    -- , restCode
    -- , restMessage
    -- , restError

    -- -- * JSON Errors
    -- , JSONError
    -- , jsonType
    -- , jsonCode
    -- , jsonMessage
    -- , jsonError
    -- ) where

import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import           Data.Monoid
import           Data.String
import           Data.Typeable
import           GHC.Generics                (Generic)
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.AWS.Data.XML
import           Network.HTTP.Client
import           Network.HTTP.Types.Status   (Status (..))

-- | Abbreviated service name.
type Abbrev = Text

newtype RequestId = RequestId Text
    deriving (Eq, Ord, Show, Generic, IsString, FromXML, FromJSON, ToText)

newtype ErrorCode = ErrorCode Text
    deriving (Eq, Ord, Show, Generic, IsString, FromXML, FromJSON, ToText)

newtype ErrorMessage = ErrorMessage Text
    deriving (Eq, Ord, Show, Generic, IsString, FromXML, FromJSON, ToText)

data ErrorType
    = Receiver
    | Sender
    | Unknown Text
      deriving (Eq, Ord, Show, Generic)

instance FromText ErrorType where
    parser = takeLowerText >>= \case
        "receiver" -> pure Receiver
        "sender"   -> pure Sender
        t          -> pure (Unknown t)

instance ToText ErrorType where
    toText = \case
        Receiver  -> "receiver"
        Sender    -> "sender"
        Unknown t -> t

instance FromXML ErrorType where
    parseXML = parseXMLText "Type"

data RESTError = RESTError
    { _restService   :: Abbrev
    , _restStatus    :: Status
    , _restRequestId :: RequestId
    , _restType      :: Maybe ErrorType
    , _restCode      :: Maybe ErrorCode
    , _restMessage   :: ErrorMessage
    } deriving (Eq, Show, Generic)

instance FromXML (Abbrev -> Status -> RESTError) where
    parseXML x = withElement "Error" parse x <|> parse x
      where
        parse y = do
            (r, t, c, m) <- (,,,)
                <$> x .@  "RequestId"
                <*> y .@? "Type"
                <*> y .@? "Code"
                <*> y .@  "Message"
            return $ \a s ->
                RESTError a s r t c m

data JSONError = JSONError
    { _jsonService :: Abbrev
    , _jsonStatus  :: Status
    , _jsonType    :: Maybe Text
    , _jsonCode    :: Maybe ErrorCode
    , _jsonMessage :: ErrorMessage
    } deriving (Eq, Show, Generic)

instance FromJSON (Abbrev -> Status -> JSONError) where
    parseJSON = withObject "JSONServiceError" parse
      where
        parse o = do
            (t, c, m) <- rest o <|> post o
            return $ \a s ->
                JSONError a s t c m

        rest o = (,,)
             <$> o .:? "Type"
             <*> o .:? "Code"
             <*> o .:  "Message"

        post o = (,,)
             <$> o .:? "__type"
             <*> o .:? "code"
             <*> o .:  "message"

-- | An error type representing the subset of errors that can be directly
-- attributed to this library.
data Error
    = HTTPError        HttpException
    | SerializerError  Abbrev Status String
    | RESTServiceError RESTError
    | JSONServiceError JSONError
--    | EC2Error        Abbrev Status EC2Error
--    | Errors          [ServiceError]
      deriving (Show, Typeable)

instance Exception Error

-- instance Monoid Error where
--     mempty      = Errors []
--     mappend a b = Errors (f a <> f b)
--       where
--         f (Errors xs) = xs
--         f x           = [x]

instance ToBuilder Error where
    build = \case
--        Errors          xs  -> buildLines (map build xs)
        HTTPError       x   -> build x
        SerializerError a s x -> buildLines
            [ "[SerializerError] {"
            , "  service = " <> build a
            , "  status  = " <> build s
            , "  message = " <> build x
            ]
        -- ServiceError   a s x -> buildLines
        --     [ "[ServiceError] {"
        --     , "  service = " <> build a
        --     , "  status  = " <> build s
        --     , "  message = " <> build (show x)
        --     ]

-- class AWSError a where
--     awsError :: a -> ServiceError String

-- instance Show a => AWSError (ServiceError a) where
--     awsError = \case
--         HttpError       e     -> HttpError e
--         SerializerError a e   -> SerializerError a e
--         APIError     a s x -> APIError a s (show x)
--         Errors          xs    -> Errors (map awsError xs)

class AsError a where
    _Error           :: Prism' a Error
    _HTTPError       :: Prism' a HttpException
    _SerializerError :: Prism' a (Abbrev, Status, String)
    _RESTError       :: Prism' a RESTError
    _JSONError       :: Prism' a JSONError

    _HTTPError       = _Error . _HTTPError
    _SerializerError = _Error . _SerializerError
    _RESTError       = _Error . _RESTError
    _JSONError       = _Error . _JSONError

instance AsError Error where
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

    _RESTError = prism
        RESTServiceError
        (\case
            RESTServiceError e -> Right e
            x           -> Left x)

    _JSONError = prism
        JSONServiceError
        (\case
            JSONServiceError e -> Right e
            x           -> Left x)

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 400

httpStatus :: AsError a => Getting (First Status) a Status
httpStatus = const (Const . First . f)
  where
    f x = x ^? _SerializerError . _2
      <|> x ^? _RESTError . to _restStatus
      <|> x ^? _JSONError . to _jsonStatus

errorCode :: AsError a => Getting (First ErrorCode) a ErrorCode
errorCode = const (Const . First . f)
  where
    f x = x ^? _RESTError . to _restCode . _Just
      <|> x ^? _JSONError . to _jsonCode . _Just

restError :: (Status -> Bool)
          -> Abbrev
          -> Status
          -> Maybe (LazyByteString -> Either String RESTError)
restError = decodeError decodeXML

jsonError :: (Status -> Bool)
          -> Abbrev
          -> Status
          -> Maybe (LazyByteString -> Either String JSONError)
jsonError = decodeError eitherDecode'

decodeError :: (LazyByteString -> Either String (Abbrev -> Status -> a))
            -> (Status -> Bool)
            -> Abbrev
            -> Status
            -> Maybe (LazyByteString -> Either String a)
decodeError parse check a s
    | check s   = Nothing
    | otherwise = Just (second (\f -> f a s) . parse)
