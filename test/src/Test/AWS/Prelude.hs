-- |
-- Module      : Test.AWS.Prelude
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Prelude
    ( module Export
    ) where

import           Control.Lens.Operators as Export
import           Data.Traversable       as Export (traverse)
import           Test.AWS.Assert        as Export
import           Test.AWS.Orphans       as Export ()
import           Test.AWS.TH            as Export
import           Test.Tasty             as Export (TestTree, testGroup)
import           Test.Tasty.HUnit       as Export (testCase)
