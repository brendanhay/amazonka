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

import qualified Data.ByteString.Char8       as BS
import qualified Data.Foldable               as Fold
import           Data.Monoid                 (mempty)
import           Network.AWS.Data.ByteString
import           Network.HTTP.Types.URI

class ToPath a where
    toPath :: a -> ByteString

    default toPath :: ToByteString a => a -> ByteString
    toPath = urlEncode False . toBS

instance ToPath ByteString where
    toPath = id

collapsePath :: ByteString -> ByteString
collapsePath bs
    | BS.null bs   = slash
    | BS.null path = slash
    | otherwise    = tl (hd path)
  where
    path = BS.intercalate slash
        . reverse
        . Fold.foldl' go []
        . filter (/= mempty)
        $ BS.split sep bs

    hd x | BS.head x  == sep = x
         | otherwise         = sep `BS.cons` x

    tl x | BS.last x  == sep = x
         | BS.last bs == sep = x `BS.snoc` sep
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

    slash = BS.singleton sep

    sep  = '/'
