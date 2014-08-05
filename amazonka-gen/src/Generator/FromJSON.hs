{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Generator.FromJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.FromJSON where

import           Control.Applicative
import           Control.Error
import           Control.Lens               ((&), (%~), (.~))
import           Control.Monad
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Types           hiding (Error)
import qualified Data.Attoparsec.Text       as AText
import           Data.Bifunctor
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Default
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid                hiding (Sum)
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Unsafe           as Text
import           Data.Text.Util
import           GHC.Generics
import           Generator.AST
import           Generator.Log
import           Generator.Models
import           Generator.Transform
import           Network.HTTP.Types.Method

-- FIXME: considering the pervasive-ness/requirements of having overrides
-- maybe it should error if they don't exist, just global?

-- FIXME: what is 'signing_name' in the ses json model?

parseModel :: Model -> Script Service
parseModel Model{..} = do
    (s, g, l) <- (,,)
        <$> load "Service"  (Just modPath)
        <*> load "Override" modGlobal
        <*> load "Override" modLocal

    -- First merge local, then global overrides.
    parse' . Object $ union (union l g) s
  where
    load :: Text -> Maybe FilePath -> Script Object
    load _ Nothing  = return mempty
    load n (Just f) = do
        say ("Parse " <> n) f
        eitherDecode <$>
            scriptIO (LBS.readFile f) >>= hoistEither

    parse' :: FromJSON a => Value -> Script a
    parse' = hoistEither . parseEither parseJSON

    union :: Object -> Object -> Object
    union = Map.unionWith merge

    merge :: Value -> Value -> Value
    merge a b =
        case (a, b) of
            (Object x, Object y) -> Object (union x y)
            (_,        _)        -> a

instance FromJSON (CI Text) where
    parseJSON = withText "case-insensitive" (return . CI.mk)

instance FromJSON Abbrev where
    parseJSON = withText "abbrev" (return . Abbrev)

instance FromJSON Doc where
    parseJSON = withText "documentation" (return . documentation)

instance FromJSON Time where
    parseJSON = withText "timestamp" $ \t ->
        case t of
            "rfc822"        -> return RFC822
            "unixTimestamp" -> return POSIX
            _               -> return def

instance FromJSON Checksum where
    parseJSON = fromCtor lowered

instance FromJSON ServiceType where
    parseJSON = fromCtor (recase Camel Hyphen)

instance FromJSON Signature where
    parseJSON = fromCtor lowered

instance FromJSON JSONV where
    parseJSON (String t) = return (JSONV t)
    parseJSON (Number n) = return . JSONV . Text.pack $ show n
    parseJSON e          = fail $ "Unrecognised JSONV field: " ++ show e

instance FromJSON Service where
    parseJSON = withObject "service" $ \o -> do
        n   <- o .: "service_full_name"
        a   <- o .: "service_abbreviation"
        rv  <- o .: "api_version"
        t   <- o .: "type"
        ops <- o .: "operations"

        let ver = version rv
            vNS = namespace a ver
            typ | a == "S3" = RestS3
                | otherwise = t
            sEr = serviceError a ops

        Service a n (rootNS vNS) vNS (typeNS vNS) ver rv typ sEr
            <$> o .:? "result_wrapped" .!= False
            <*> o .:  "signature_version"
            <*> o .:  "documentation"
            <*> o .:  "endpoint_prefix"
            <*> o .:? "global_endpoint"
            <*> o .:? "xmlnamespace"
            <*> o .:! "timestamp_format"
            <*> o .:! "checksum_format"
            <*> o .:! "json_version"
            <*> o .:? "target_prefix"
            <*> pure ops
            <*> pure def
            <*> o .:? "required" .!= mempty

instance FromJSON [Operation] where
    parseJSON = withObject "operations" $ \o ->
        forM (Map.toList o) $ \(k, v) ->
            ($ k) <$> parseJSON v

instance FromJSON (Text -> Operation) where
    parseJSON = withObject "operation" $ \o -> do
        op <- Operation ""
            <$> pure def
            <*> o .:? "alias"
            <*> pure def
            <*> pure def
            <*> pure def
            <*> pure def
            <*> (Doc <$> o .:? "documentation")
            <*> o .:? "documentation_url"
            <*> parseJSON (Object o)
            <*> parseJSON (Object o)
            <*> o .:  "errors"
            <*> o .:? "pagination"
        return $ \n -> op & opName .~ n

instance FromJSON Request where
    parseJSON = withObject "request" $ \o ->
        Request "Request" "Request" def def def def
            <$> (setDirection DRequest <$> o .:! "input")
            <*> o .:! "http"

instance FromJSON Response where
    parseJSON = withObject "response" $ \o ->
        Response "Response" def def def def
            <$> (setDirection DResponse <$> o .:! "output")

instance FromJSON Location where
    parseJSON = fromCtor (lowered . drop 1)

instance FromJSON Common where
    parseJSON = withObject "common" $ \o -> do
        n <- name o
        Common n (prefixof n)
            <$> o .:? "xmlname"       .!= n
            <*> o .:! "location"
            <*> o .:? "location_name"
            <*> o .:? "required"      .!= False
            <*> o .:? "documentation"
            <*> o .:? "streaming"     .!= False
            <*> pure def
      where
        name o = upperFirst
           <$> o .: "shape_name"
           <|> o .: "alias"
           <|> o .: "name"
           <|> return defName

instance FromJSON Shape where
    parseJSON = withObject "shape" $ \o -> do
        c <- parseJSON (Object o)
        o .: "type" >>= f c o
      where
        f c o "structure" = do
            xs <- o .:? "members"      .!= mempty
            ys <- o .:? "member_order" .!= Map.keys xs :: Parser [Text]

            let g y x | x == defName = y
                      | otherwise    = x
                h y = second (cmnName %~ g y) . (y,) <$> Map.lookup y xs
                fs  = Map.fromList $ mapMaybe h ys

            return . SStruct $ Struct fs c

        f c o "list" = fmap SList $ List
            <$> o .:  "members"
            <*> o .:? "flattened"  .!=  False
            <*> o .:? "min_length" .!= 0
            <*> o .:? "max_length" .!= 0
            <*> pure c

        f c o "map" = fmap SMap $ Map
            <$> o .: "keys"
            <*> o .: "members"
            <*> pure c

        f c o typ = do
            ms <- o .:? "enum"

            let e = shapeEnums (_cmnName c) <$> ms

            case e of
                Just vs -> return . SSum $ Sum vs c
                Nothing -> fmap SPrim $ Prim
                    <$> parseJSON (String typ)
                    <*> o .:? "min_length" .!= 0
                    <*> o .:? "max_length" .!= 0
                    <*> o .:? "pattern"
                    <*> pure c

instance FromJSON Primitive where
    parseJSON = withText "type" $ \t ->
        case t of
            "string"    -> return PText
            "integer"   -> return PInteger
            "long"      -> return PInteger
            "double"    -> return PDouble
            "float"     -> return PDouble
            "boolean"   -> return PBool
            "blob"      -> return PByteString
            "timestamp" -> return PUTCTime
            _           -> fail ("Unable to parse Prim from: " ++ Text.unpack t)

instance FromJSON Python where
    parseJSON = withText "python" $ \t ->
        either (fail . mappend (msg t)) return (AText.parseOnly python t)
      where
        msg t = "Failed parsing python syntax from: "
            ++ Text.unpack t
            ++ ", with: "

        python = choice <|> index <|> apply <|> keyed

        choice = Choice
            <$> label
             <* AText.string "||"
            <*> python

        keyed = Keyed <$> label

        index = Index
            <$> label
             <* AText.string "[-1]"
            <*> (AText.char '.' *> python <|> return Empty)

        apply = Apply
            <$> label
            <* AText.char '.'
            <*> python

        label = clean (AText.takeWhile1 (not . delim))

        delim c = c == '.'
               || c == '['
               || c == '|'

        clean = fmap Text.strip

instance FromJSON Pagination where
    parseJSON = withObject "pagination" $ \o -> more o <|> next o
      where
        more o = do
            xs <- f "input_token"
            ys <- f "output_token"

            unless (length xs == length ys) $
                fail "input_token and output_token don't contain same number of keys."

            More <$> o .: "more_results"
                 <*> pure (zipWith Token xs ys)
          where
            f k = o .: k <|> (:[]) <$> o .: k

        next o = Next
            <$> o .: "result_key"
            <*> (Token <$> o .: "input_token" <*> o .: "output_token")

instance FromJSON HTTP where
    parseJSON = withObject "http" $ \o -> HTTP
        <$> (o .: "method" >>= method)
        <*> o .: "uri"
        <*> o .: "uri"
      where
        method = either (fail . BS.unpack) return
            . parseMethod
            . Text.encodeUtf8

instance FromJSON [PathPart] where
    parseJSON = withText "uri" $ return . path
       where
        path = filter (/= PConst "") . go . Text.takeWhile (/= '?')

        go x | Text.null s = [PConst l]
             | otherwise   = PConst l : PVar m : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

instance FromJSON [QueryPart] where
    parseJSON = withText "uri" $ return . query
      where
        query = map (uncurry QueryPart) . go . Text.dropWhile (/= '?')

        go x | Text.null s
             , Text.null l = []
             | Text.null s = [(Text.tail l, Nothing)]
             | otherwise   = brk : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

            brk | '=' <- Text.last l = (Text.init $ Text.tail l, Just m)
                | otherwise          = (Text.tail l, Nothing)

(.:!) :: (Default a, FromJSON a) => Object -> Text -> Parser a
(.:!) o k = o .:? k .!= def

fromField :: (Generic a, GFromJSON (Rep a))
          => (String -> String)
          -> Value
          -> Parser a
fromField f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

fromCtor :: (Generic a, GFromJSON (Rep a))
         => (String -> String)
         -> Value
         -> Parser a
fromCtor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }
