-- |
-- Module      : Main
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Test.Framework
-- import qualified Test.AutoScaling.V20110101 as AutoScaling_V20110101
import qualified Test.Route53.V20121212     as Route53_V20121212

main :: IO ()
main = defaultMain
    [ testGroup "Route53" [Route53_V20121212.tests]
--    , testGroup "Auto Scaling" AutoScaling_V20110101.tests
    ]
