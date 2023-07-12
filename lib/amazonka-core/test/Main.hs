-- |
-- Module      : Main
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Main (main) where

import qualified Test.Amazonka.Data.Base64 as Base64
import qualified Test.Amazonka.Data.List as List
import qualified Test.Amazonka.Data.Maybe as Maybe
import qualified Test.Amazonka.Data.Numeric as Numeric
import qualified Test.Amazonka.Data.Path as Path
import qualified Test.Amazonka.Data.Query as Query
import qualified Test.Amazonka.Data.Time as Time
import qualified Test.Amazonka.Error as Error
import qualified Test.Amazonka.Sign.V2Header as V2Header
import qualified Test.Amazonka.Sign.V4 as V4
import Test.Tasty
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "amazonka-core"
      [ testGroup
          "primitives"
          [ Numeric.tests,
            Time.tests,
            Base64.tests,
            Maybe.tests,
            Query.tests
          ],
        testGroup
          "paths"
          [ Path.tests
          ],
        testGroup
          "collections"
          [ List.tests
          ],
        testGroup
          "signing"
          [ V2Header.tests,
            V4.tests
          ],
        Error.tests
      ]
