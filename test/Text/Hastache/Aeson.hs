{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Adjusts the Mustache spec to correct reflect Haskell's use of maybe
-- for 'falsey' values and add {{n}} for getting the current list index.
module Text.Hastache.Aeson (render) where

import           Control.Applicative
import           Data.Aeson
import           Data.Attoparsec.Number
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char                  (toLower)
import           Data.HashMap.Strict        (foldlWithKey')
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Vector                as V
import           Text.Hastache

render :: ByteString -> Value -> IO ByteString
render tmpl val = LBS.toStrict
    <$> hastacheStr defaultConfig tmpl (valueContext ['n'..] val)

valueContext :: Monad m => String -> Value -> MuContext m
valueContext vs = mapContext . buildMap vs "" Map.empty

arrayContext :: Monad m => String -> (Integer, Value) -> MuContext m
arrayContext vs (idx, val) = mapContext
    . Map.insert (BS.pack $ head vs : []) (MuVariable $ toLByteString idx)
    $ buildMap (tail vs) "" Map.empty val

mapContext :: Monad m => Map ByteString (MuType m) -> ByteString -> m (MuType m)
mapContext m a = return $ ctx
  where
    ctx = fromMaybe
        (if a == "." then maybe MuNothing id $ Map.lookup BS.empty m else MuNothing)
        (Map.lookup a m)

buildMap :: Monad m
         => String
         -> String
         -> Map ByteString (MuType m)
         -> Value
         -> Map ByteString (MuType m)
buildMap vs name m (Object obj) = Map.insert (encodeStr name)
    (MuList [mapContext $ foldlWithKey' (foldObject vs "") Map.empty obj])
    (foldlWithKey' (foldObject vs name) m obj)
buildMap vs name m value = Map.insert (encodeStr name) muValue m
    where
        muValue = case value of
            Array arr        -> MuList . map (arrayContext vs) . zip [1..] $ V.toList arr
            Number (D float) -> MuVariable $ toLByteString float
            Number (I int)   -> MuVariable $ toLByteString int
            String s         -> MuVariable s
            Bool b           -> MuVariable . map toLower $ show b
            Null             -> MuNothing
            t                -> MuVariable $ show t

foldObject :: Monad m
           => String
           -> String
           -> Map ByteString (MuType m)
           -> Text
           -> Value
           -> Map ByteString (MuType m)
foldObject vs name m k v = buildMap vs (buildName $ Text.unpack k) m v
  where
    buildName n
        | null name = n
        | otherwise = name ++ "." ++ n
