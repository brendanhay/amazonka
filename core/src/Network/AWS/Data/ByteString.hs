{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module      : Network.AWS.Data.ByteString
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

--
module Network.AWS.Data.ByteString
    (
    -- * ByteString
      ByteString
    , LazyByteString
    , ToByteString (..)
    , showBS
    , stripBS
    ) where

import           Data.ByteString              (ByteString)
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Char8        as BS8
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Builder as Build
import           Data.CaseInsensitive         (CI)
import qualified Data.CaseInsensitive         as CI
import           Data.Char
import qualified Data.Text.Encoding           as Text
import           Data.Time                    (UTCTime)
import           Network.AWS.Data.Text
import           Network.HTTP.Types
import           Numeric
import           Numeric.Natural

type LazyByteString = LBS.ByteString

showBS :: ToByteString a => a -> String
showBS = BS8.unpack . toBS

stripBS :: ByteString -> ByteString
stripBS = BS8.dropWhile isSpace . fst . BS8.spanEnd isSpace

class ToByteString a where
    toBS :: a -> ByteString

    default toBS :: ToText a => a -> ByteString
    toBS = Text.encodeUtf8 . toText

instance ToByteString ByteString     where toBS = id
instance ToByteString Builder        where toBS = toBS . Build.toLazyByteString
instance ToByteString LazyByteString where toBS = LBS.toStrict
instance ToByteString Text           where toBS = Text.encodeUtf8
instance ToByteString String         where toBS = BS8.pack
instance ToByteString Int            where toBS = toBS . Build.intDec
instance ToByteString Integer        where toBS = toBS . Build.integerDec
instance ToByteString Natural        where toBS = toBS . toInteger
instance ToByteString Double         where toBS = toBS . ($ "") . showFFloat Nothing
instance ToByteString StdMethod      where toBS = renderStdMethod
instance ToByteString UTCTime        where toBS = BS8.pack . show

instance ToByteString a => ToByteString (CI a) where
    toBS = toBS . CI.original
