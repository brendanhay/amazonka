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

import qualified Data.Text                      as Text
import           Network.AWS.Prelude
import           Text.PrettyPrint.GenericPretty

instance Out (Time a)

instance Out UTCTime where
    docPrec _ = doc
    doc       = doc . show

instance Out Text where
    docPrec _ = doc
    doc       = doc . Text.unpack

