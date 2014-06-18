{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Generator.Stage1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage1 where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy        as LBS
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as Map
import           Data.Monoid
import           Data.String.CaseConversion
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Unsafe            as Text
import           GHC.Generics
import           Network.AWS.Generator.Types

parse :: Model -> Script Service
parse m = do
    r <- fmapLT show . syncIO . LBS.readFile $ mPath m
    hoistEither (eitherDecode r)

-- From S3:
data Service = Service
    { s1ApiVersion          :: Text
    , s1Type                :: ServiceType
    , s1SignatureVersion    :: Signature
    , s1TimestampFormat     :: Maybe Time
    , s1ChecksumFormat      :: Maybe Checksum
    , s1ServiceFullName     :: Text
    , s1ServiceAbbreviation :: Text
    , s1GlobalEndpoint      :: Maybe Text
    , s1EndpointPrefix      :: Text
    , s1Xmlnamespace        :: Text
    , s1Documentation       :: Maybe Text
    , s1ResultWrapped       :: Maybe Bool
    , s1TargetPrefix        :: Maybe Text
    , s1Operations          :: HashMap Text Operation
    } deriving (Show, Generic)

instance FromJSON Service where
    parseJSON = fromField (recase Camel Under . drop 2)

data Operation = Operation
    { o1Name             :: Text
    , o1Alias            :: Maybe Text
    , o1Documentation    :: Maybe Text
    -- , oDocumentationUrl :: Maybe Text
    , o1Http             :: HTTP
    , o1Input            :: Maybe Shape
    , o1Output           :: Maybe Shape
    -- , oErrors           :: [Shape]
    -- , oPagination       :: Maybe Pagination
    } deriving (Show, Generic)

instance FromJSON Operation where
    parseJSON = fromField (recase Camel Under . drop 2)

data Shape
    = SStruct
      { sFields        :: HashMap Text Shape
      , sOrder         :: [Text]
      , sName          :: Text
      , sRequired      :: !Bool
      , sDocumentation :: Maybe Text
      }

    | SList
      { sItem          :: Shape
      , sFlattened     :: !Bool
      , sLength        :: !Int
      , sName          :: Text
      , sRequired      :: !Bool
      , sDocumentation :: Maybe Text
      }

    | SMap
      { sKey           :: !Shape
      , sValue         :: !Shape
      , sName          :: Text
      , sRequired      :: !Bool
      , sDocumentation :: Maybe Text
      }

    | SPrim
      { sPrim          :: !Prim
      , sLocation      :: Maybe Text
      , sLocationName  :: Maybe Text
      , sMinLength     :: Maybe Int
      , sMaxLength     :: Maybe Int
      , sPattern       :: Maybe Text
      , sName          :: Text
      , sRequired      :: !Bool
      , sDocumentation :: Maybe Text
      }

    | SEnum
      { sEnum          :: HashMap Text Text
      , sName          :: Text
      , sRequired      :: !Bool
      , sDocumentation :: Maybe Text
      }

      deriving (Eq, Show, Generic)

instance FromJSON Shape where
    parseJSON = withObject "shape" $ \o -> do
        t <- o .: "type"
        ctor t o
            <*> name t o
            <*> o .:? "required" .!= False
            <*> o .:? "documentation"
      where
        name t o
            | not (Map.member "enum" o)
            , primitive t = return . Text.pack . show $ fromType t
            | otherwise =
                    o .: "shape_name"
                <|> o .: "alias"
                <|> o .: "name"
                <|> return (Text.pack $ show t)

        ctor Structure o = do
            ns <- names <$> o .:? "members" .!= mempty
            no <- o .:? "member_order" .!= Map.keys ns
            return $ SStruct ns no

        ctor List o = SList
            <$> o .:  "members"
            <*> o .:? "flattened"  .!= False
            <*> o .:? "min_length" .!= 0

        ctor Map o = SMap
            <$> o .: "keys"
            <*> o .: "members"

        ctor t o = prim t o

        names = Map.foldlWithKey' g mempty
          where
            g m k v = Map.insert (h k) v m

            h s | "VPC" `Text.isPrefixOf` s = ("vpc" <>) $ Text.drop 3 s
                | otherwise                 = s

        prim t o = do
            ms <- o .:? "enum"

            let str  = Text.pack . recase Under Camel . Text.unpack
                enum = Map.fromList . map (first str . join (,)) <$> ms

            case enum of
                Just x  -> return (SEnum x)
                Nothing -> SPrim (fromType t)
                    <$> o .:? "location"
                    <*> o .:? "location_name"
                    <*> o .:? "min_length"
                    <*> o .:? "max_length"
                    <*> o .:? "pattern"

data Type
    = Structure
    | List
    | Map
    | String
    | Integer
    | Double
    | Float
    | Boolean
    | Blob
    | Timestamp
    | Long
    | Enum
      deriving (Eq, Ord, Show, Generic)

instance FromJSON Type where
    parseJSON = fromCtor (recase Camel Under)

primitive :: Type -> Bool
primitive = flip notElem [Structure, List, Map, Enum]

data Prim
    = PText
    | PInteger
    | PDouble
    | PBool
    | PByteString
    | PUTCTime
      deriving (Eq, Generic)

instance Show Prim where
    show p = case p of
        PText       -> "Text"
        PInteger    -> "!Integer"
        PDouble     -> "!Double"
        PBool       -> "!Bool"
        PByteString -> "ByteString"
        PUTCTime    -> "UTCTime"

instance ToJSON Prim where
    toJSON = toCtor (drop 1)

fromType :: Type -> Prim
fromType t =
    case t of
        Network.AWS.Generator.Stage1.String -> PText
        Integer   -> PInteger
        Double    -> PDouble
        Float     -> PDouble
        Boolean   -> PBool
        Blob      -> PByteString
        Timestamp -> PUTCTime
        Long      -> PInteger
        _         -> error $ "Unable to create primitive type from: " ++ show t

data HTTP = HTTP
    { hMethod :: Text
    , hUri    :: [Part]
    , hQuery  :: HashMap Text Text
    } deriving (Eq, Show, Generic)

instance FromJSON HTTP where
    parseJSON (Object o) = do
        u <- o .: "uri"
        m <- o .: "method"
        return $ HTTP (Text.toLower m) (uri u) (query u)
      where
        uri = filter (/= T "") . go . Text.takeWhile (/= '?')
          where
            go x | Text.null s = [T l]
                 | otherwise   = T l : I m : go (Text.unsafeTail t)
              where
                (m, t) = Text.span (/= '}') $ Text.unsafeTail s
                (l, s) = Text.span (/= '{') x

        query = Map.fromList . go . Text.dropWhile (/= '?')
          where
            go x | Text.null s
                 , Text.null l = []
                 | Text.null s = [(Text.tail l, "")]
                 | otherwise   = brk : go (Text.unsafeTail t)
              where
                (m, t) = Text.span (/= '}') $ Text.unsafeTail s
                (l, s) = Text.span (/= '{') x

                brk | '=' <- Text.last l = (Text.init $ Text.tail l, m)
                    | otherwise          = (Text.tail l, "")

    parseJSON _ =
        fail "Unable to parse HTTP from Operation."

instance ToJSON HTTP where
    toJSON = toField (recase Camel Under . drop 1)

data Part
    = T !Text
    | I !Text
      deriving (Eq, Show)

instance ToJSON Part where
    toJSON p =
        case p of
            T t -> f "T" t
            I i -> f "I" i
      where
        f k v = object
            [ "type"  .= (k :: Text)
            , "value" .= v
            ]
