{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.Path
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.Path where

import qualified Data.ByteString.Char8       as BS8
import qualified Data.Foldable               as Fold
import           Data.Monoid                 (mempty)
import           Network.AWS.Data.ByteString
import           Network.HTTP.Types.URI

class ToPath a where
    toPath :: a -> ByteString

    default toPath :: ToByteString a => a -> ByteString
    toPath = encodePath . toBS

instance ToPath ByteString where
    toPath = id

encodePath :: ByteString -> ByteString
encodePath = BS8.intercalate "/" . map (urlEncode False) . BS8.split '/'

collapsePath :: ByteString -> ByteString
collapsePath bs
    | BS8.null bs   = slash
    | BS8.null path = slash
    | otherwise    = tl (hd path)
  where
    path = BS8.intercalate slash
        . reverse
        . Fold.foldl' go []
        . filter (/= mempty)
        $ BS8.split sep bs

    hd x | BS8.head x  == sep = x
         | otherwise         = sep `BS8.cons` x

    tl x | BS8.last x  == sep = x
         | BS8.last bs == sep = x `BS8.snoc` sep
         | otherwise         = x

    go acc c | c == dot  = acc
    go acc c | c == dots = remv acc c
    go acc c             = c : acc

    remv []       _ = []
    remv (x : xs) c
        | x == dot  = c : xs
        | x == dots = c : x : xs
        | otherwise = xs

    dot  = "."
    dots = ".."

    slash = BS8.singleton sep

    sep  = '/'
