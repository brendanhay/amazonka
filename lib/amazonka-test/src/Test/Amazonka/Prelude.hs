-- |
-- Module      : Test.Amazonka.Prelude
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Prelude
  ( module Export,
  )
where

import Amazonka.Core.Lens.Internal as Export
import Data.Traversable as Export (traverse)
import Test.Amazonka.Assert as Export
import Test.Amazonka.Orphans as Export ()
import Test.Amazonka.TH as Export
import Test.Tasty as Export (TestTree, testGroup)
import Test.Tasty.HUnit as Export (testCase)
