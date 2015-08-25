{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

-- |
-- Module      : Network.AWS.Data.Path
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Path
    (
    -- * Path Types
      RawPath
    , EscapedPath

    -- * Constructing Paths
    , ToPath (..)
    , rawPath

    -- * Manipulating Paths
    , escapePath
    , collapsePath
    ) where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import           Data.Monoid
import           Network.AWS.Data.ByteString
import           Network.AWS.Data.Text
import           Network.HTTP.Types.URI

import           Prelude

class ToPath a where
    toPath :: a -> ByteString

instance ToPath ByteString where
    toPath = id

instance ToPath Text where
    toPath = toBS

rawPath :: ToPath a => a -> Path 'NoEncoding
rawPath (toPath -> x) = Raw (filter (not . BS8.null) (BS8.split sep x)) trail
  where
    trail = not (BS.null x || BS8.last x /= sep)

data Encoding = NoEncoding | Percent
    deriving (Eq, Show)

data Path :: Encoding -> * where
    Raw     :: [ByteString] -> Bool -> Path 'NoEncoding
    Encoded :: [ByteString] -> Bool -> Path 'Percent

deriving instance Show (Path a)
deriving instance Eq   (Path a)

type RawPath     = Path 'NoEncoding
type EscapedPath = Path 'Percent

instance Monoid RawPath where
    mempty                        = Raw [] False
    mappend (Raw xs _) (Raw ys t) = Raw (xs ++ ys) t

instance ToByteString EscapedPath where
    toBS (Encoded [] _) = slash
    toBS (Encoded xs t) = slash <> BS8.intercalate slash (xs <> [slash | t])

escapePath :: Path a -> EscapedPath
escapePath (Raw     xs t) = Encoded (map (urlEncode True) xs) t
escapePath (Encoded xs t) = Encoded xs t

collapsePath :: Path a -> Path a
collapsePath = \case
    Raw     xs t -> Raw     (go xs) t
    Encoded xs t -> Encoded (go xs) t
  where
    go = reverse . f . reverse

    f :: [ByteString] -> [ByteString]
    f []            = []
    f (x:xs)
        | x == dot  = f xs
        | x == dots = drop 1 (f xs)
        | otherwise = x : f xs

    dot  = "."
    dots = ".."

slash :: ByteString
slash = BS8.singleton sep

sep :: Char
sep = '/'
