{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.Route53
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Route53 (tests) where

import qualified Data.Text           as Text
import           Data.Text.Encoding
import           Network.AWS.Route53
import           Test.Common

tests :: [Test]
tests = (:[]) $ testVersion route53Version
    [ testGroup "Hosted Zones"
        [ testGroup "Requests"
            [ testProperty "CreateHostedZone" (prop :: Rq CreateHostedZone)
    --         , testProperty "GetHostedZone"    (prop :: Rq Query GetHostedZone)
    --         , testProperty "ListHostedZones"  (prop :: Rq Query ListHostedZones)
    --         , testProperty "DeleteHostedZone" (prop :: Rq Query DeleteHostedZone)
            ]
    --     , testGroup "Responses"
    --         [ testProperty "CreateHostedZoneResponse" (prop :: Rs CreateHostedZoneResponse)
    --         , testProperty "GetHostedZoneResponse"    (prop :: Rs GetHostedZoneResponse)
    --         , testProperty "ListHostedZonesResponse"  (prop :: Rs ListHostedZonesResponse)
    --         , testProperty "DeleteHostedZoneResponse" (prop :: Rs DeleteHostedZoneResponse)
    --         ]
    --     ]

    -- , testGroup "Record Sets"
    --     [ testGroup "Requests"
    --         [ testProperty "ChangeResourceRecordSets" (prop :: Rq XML ChangeResourceRecordSets)
    --         , testProperty "ListResourceRecordSets"   (prop :: Rq Query ListResourceRecordSets)
    --         , testProperty "GetChange"                (prop :: Rq Query GetChange)
    --         ]
    --     , testGroup "Responses"
    --         [ testProperty "ChangeResourceRecordSetsResponse" (prop :: Rs ChangeResourceRecordSetsResponse)
    --         , testProperty "ListResourceRecordSetsResponse"   (prop :: Rs ListResourceRecordSetsResponse)
    --         , testProperty "GetChangeResponse"                (prop :: Rs GetChangeResponse)
    --         ]
    --     ]

    -- , testGroup "Health Checks"
    --     [ testGroup "Requests"
    --         [ testProperty "CreateHealthCheck" (prop :: Rq XML CreateHealthCheck)
    --         , testProperty "GetHealthCheck"    (prop :: Rq Query GetHealthCheck)
    --         , testProperty "ListHealthChecks"  (prop :: Rq Query ListHealthChecks)
    --         , testProperty "DeleteHealthCheck" (prop :: Rq Query DeleteHealthCheck)
    --         ]
    --     , testGroup "Responses"
    --         [ testProperty "CreateHealthCheckResponse" (prop :: Rq XML CreateHealthCheckResponse)
    --         , testProperty "GetHealthCheckResponse"    (prop :: Rs GetHealthCheckResponse)
    --         , testProperty "ListHealthChecksResponse"  (prop :: Rs ListHealthChecksResponse)
    --         , testProperty "DeleteHealthCheckResponse" (prop :: Rs DeleteHealthCheckResponse)
    --         ]
        ]
    ]

-- How to test actual query strings ? like GET /resource/id

instance ToJSON CallerReference where
    toJSON = String . decodeUtf8 . unCallerReference

instance ToJSON ChangeStatus where
    toJSON = String . Text.pack . show

$(deriveJSON
    [ ''DelegationSet
    , ''ChangeInfo
    , ''Config
    , ''HostedZone
--    , ''ResourceRecordSet
    ])

$(deriveArbitrary
    [ ''DelegationSet
    , ''CallerReference
    , ''ChangeInfo
    , ''ChangeStatus
    , ''Config
    , ''HostedZone
    ])

$(deriveProperty "test/resources/Route53"
    [ ''CreateHostedZone
    -- , ''CreateHostedZoneResponse
    -- , ''GetHostedZone
    -- , ''GetHostedZoneResponse
    -- , ''ListHostedZones
    -- , ''ListHostedZonesResponse
    -- , ''DeleteHostedZone
    -- , ''DeleteHostedZoneResponse
    -- , ''ChangeResourceRecordSets
    -- , ''ChangeResourceRecordSetsResponse
    -- , ''ListResourceRecordSets
    -- , ''ListResourceRecordSetsResponse
    -- , ''GetChange
    -- , ''GetChangeResponse
    -- , ''CreateHealthCheck
    -- , ''GetHealthCheck
    -- , ''ListHealthChecks
    -- , ''ListHealthChecksResponse
    -- , ''DeleteHealthCheck
    ])
