{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.Orphans
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Orphans where

import           Data.Bifunctor
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as BS8
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as Map
import           Data.Monoid
import qualified Data.Text                      as Text
import           GHC.Exts                       (toList)
import           Network.AWS.Data.List1
import           Network.AWS.Data.Map
import           Network.AWS.Data.Sensitive
import           Network.AWS.Prelude
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

instance Out Doc where
    docPrec _ = doc
    doc       = id

instance Out (Time a)

instance Out UTCTime where
    docPrec _ = doc
    doc       = doc . show

instance Out a => Out (List1 a) where
    docPrec _ = doc
    doc       = doc . toList . toNonEmpty

instance (Out k, Out v) => Out (Map k v) where
    docPrec _ = doc
    doc       = doc . toMap

instance (Out k, Out v) => Out (HashMap k v) where
    docPrec _ = doc
    doc       =
          mappend "HashMap.fromList "
        . doc
        . map (bimap doc doc)
        . Map.toList

instance Out Text where
    docPrec _ = doc
    doc       = doc . Text.unpack

instance Out Base64 where
    docPrec _ = doc
    doc       = mappend "Base64 " . doc . toText

instance Out ByteString where
    docPrec _ = doc
    doc       = doc . BS8.unpack

instance Out Region where
    docPrec _ = doc
    doc       = doc . toText

instance Out a => Out (Sensitive a) where
    docPrec _ = doc
    doc       = doc . desensitise

instance Out RqBody where
    docPrec _ = doc
    doc       = doc . show

instance Out RsBody where
    docPrec _ = doc
    doc       = doc . show
