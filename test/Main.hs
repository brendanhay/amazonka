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
-- import qualified Test.AutoScaling as AutoScaling
import qualified Test.EC2         as EC2
import qualified Test.Route53     as Route53
-- import qualified Test.Signing     as Signing

main :: IO ()
main = defaultMain
    [
    -- testGroup "AutoScaling" AutoScaling.tests
      testGroup "Route53"     Route53.tests
    , testGroup "EC2"         EC2.tests
    -- , testGroup "IAM"         IAM.tests
    -- , testGroup "CloudWatch"  CloudWatch.tests
    -- , testGroup "OpsWorks"    OpsWorks.tests
    -- , testGroup "S3"          S3.tests
    -- , testGroup "Signing"     Signing.tests
    ]
