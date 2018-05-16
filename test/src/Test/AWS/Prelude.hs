-- |
-- Module      : Test.AWS.Prelude
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Prelude
    ( module Export
    ) where

import           Data.Traversable as Export (traverse)
import           Network.AWS.Lens as Export
import           Test.AWS.Assert  as Export
import           Test.AWS.Orphans as Export ()
import           Test.AWS.TH      as Export
import           Test.Tasty       as Export (TestTree, testGroup)
import           Test.Tasty.HUnit as Export (testCase)
