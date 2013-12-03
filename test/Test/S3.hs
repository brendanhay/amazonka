{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.S3
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.S3 (tests) where

import Network.AWS.S3
import Test.Common

tests :: TestTree
tests = testVersion s3
    [ testGroup "Service"
        [ qc "GetService"         (prop :: TRq GetService)
        , qc "GetServiceResponse" (prop :: TRs GetServiceResponse)
        ]

    , testGroup "Bucket"
        [
        ]

    , testGroup "Object"
        [
        ]
        -- [ qc "GetObject" (prop :: TRq GetObject)
        -- -- , qc "PutObject" (prop :: TRq PutObject)
        -- ]
    ]

$(deriveDependency
    [ ''Bucket
    , ''Owner
    ])

$(deriveProperty "test/resources/S3"
    [ ''GetService
    , ''GetServiceResponse
    -- , ''GetObject
    -- , ''PutObject
    ])
