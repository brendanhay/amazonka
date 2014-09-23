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
import qualified Data.Attoparsec.Text       as AText
import           Data.Bifunctor
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict        as Map
import           Data.Jason                 hiding (Error)
import           Data.Jason.Types           hiding (Error)
import           Data.List
import           Data.Monoid                hiding (Sum)
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Unsafe           as Text
import           Data.Text.Util
import           Data.Traversable           (traverse)
import           GHC.Generics
import           Generator.AST
import           Generator.Log
import           Generator.Models
import           Generator.Transform
import           Network.HTTP.Types.Method
import           Text.EDE.Filters

-- FIXME: what is 'signing_name' in the ses json model?

parseModel :: Model -> Script Service
parseModel Model{..} = do
    (s, g) <- (,)
        <$> load "Service"  modPath
        <*> load "Override" modOverride

    parse' . Object $ g `unionObject` s
  where
    load :: Text -> FilePath -> Script Object
    load n f = do
        say ("Parse " <> n) f
        eitherDecode <$>
            scriptIO (LBS.readFile f) >>= hoistEither

    parse' :: FromJSON a => Value -> Script a
    parse' = hoistEither . parseEither parseJSON

    unionObject :: Object -> Object -> Object
    unionObject (Obj a) (Obj b) = Obj (assocUnion mergeValue a b)

    mergeValue :: Value -> Value -> Value
    mergeValue a b =
        case (a, b) of
            (Object x, Object y) -> Object (x `unionObject` y)
            (_,        _)        -> a

    assocUnion :: Eq k => (v -> v -> v) -> [(k, v)] -> [(k, v)] -> [(k, v)]
    assocUnion f xs ys = unionBy ((==) `on` fst) (map g xs) ys
      where
        g (k, x) | Just y <- lookup k ys = (k, f x y)
                 | otherwise             = (k, x)

instance FromJSON (CI Text) where
    parseJSON = withText "case-insensitive" (return . CI.mk)

instance FromJSON a => FromJSON (HashMap (CI Text) a) where
    parseJSON = fmap (Map.fromList . map (first CI.mk) . Map.toList) . parseJSON

instance FromJSON Abbrev where
    parseJSON = withText "abbrev" (return . Abbrev)

instance FromJSON NS where
    parseJSON = withText "namespace" (return . namespaceFromText)

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

instance FromJSON Version where
    parseJSON = withText "version" (return . Version)

instance FromJSON (Text -> Doc -> Cabal) where
    parseJSON = withObject "cabal" $ \o -> cabal
        <$> o .:  "version"
        <*> o .:? "synopsis"
        <*> o .:? "documentation"

instance FromJSON Library where
    parseJSON = fmap library . parseJSON

instance FromJSON Override where
    parseJSON = withObject "override" $ \o -> Override
        <$> o .:? "name"
        <*> o .:? "exists"
        <*> o .:? "prefix"
        <*> o .:? "require" .!= mempty
        <*> o .:? "ignore"  .!= mempty
        <*> o .:? "type"    .!= mempty
        <*> o .:? "rename"  .!= mempty

instance FromJSON Service where
    parseJSON = withObject "service" $ \o -> do
        n   <- o .: "service_full_name"
        a   <- o .: "service_abbreviation"
        rv  <- o .: "api_version"
        t   <- o .: "type"
        d   <- o .: "documentation"

        cbl <- fromMaybe (cabal cabalVersion Nothing Nothing)
            <$> (o .:? "cabal")

        let typ | a == "S3" = RestS3
                | otherwise = t

        Service a
            <$> o .:? "library_name" .!= library a
            <*> pure n
            <*> pure (namespacesFromAbbrev a typ)
            <*> pure (version rv)
            <*> pure rv
            <*> pure typ
            <*> pure def
            <*> o .:? "result_wrapped" .!= False
            <*> o .:  "signature_version"
            <*> pure d
            <*> o .:  "endpoint_prefix"
            <*> o .:? "global_endpoint"
            <*> o .:? "xmlnamespace"
            <*> o .:! "timestamp_format"
            <*> o .:! "checksum_format"
            <*> o .:! "json_version"
            <*> o .:? "target_prefix"
            <*> o .:  "operations"
            <*> pure def
            <*> pure (cbl n d)
            <*> o .:! "static"
            <*> o .:? "overrides" .!= mempty

instance FromJSON [Operation] where
    parseJSON = withObject "operations" $
        mapM (\(k, v) -> ($ k) <$> parseJSON v) . unObject

instance FromJSON (Text -> Operation) where
    parseJSON = withObject "operation" $ \o -> do
        op <- Operation ""
            <$> pure def
            <*> pure (namespacesFromAbbrev def def)
            <*> o .:? "alias"
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
        Request <$> (defaultType . setDirection DRequest <$> o .:! "input")
                <*> pure def

instance FromJSON Response where
    parseJSON = withObject "response" $ \o ->
        Response <$> (defaultType . setDirection DResponse <$> o .:! "output")
                 <*> pure def

instance FromJSON Location where
    parseJSON = fromCtor (lowered . drop 1)

instance FromJSON Common where
    parseJSON = withObject "common" $ \o -> do
        n <- name o
        Common n (casedChars n)
            <$> o .:? "xmlname"
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
            Obj xs <- o .:? "members"      .!= mempty
            ys     <- o .:? "member_order" .!= map fst xs :: Parser [Text]
            xs'    <- traverse (\(k, v) -> (k,) <$> parseJSON v) xs

            let g y x | x == defName = y
                      | otherwise    = x
                h y = second (cmnName %~ g y) . (y,) <$> lookup y xs'

            return . SStruct $ Struct (mapMaybe h ys) c

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
            case ms of
                Just vs -> return . SSum $ Sum (enum vs) c
                Nothing -> fmap SPrim $ Prim
                    <$> parseJSON (String typ)
                    <*> o .:? "min_length" .!= 0
                    <*> o .:? "max_length" .!= 0
                    <*> o .:? "pattern"
                    <*> pure c

        enum = Map.fromList . map (join (,))

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

        python = index <|> apply <|> keyed <|> choice

        choice = Choice
            <$> python
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
