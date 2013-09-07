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
import           Network.AWS.Route53
import           Test.Common

tests :: [Test]
tests = (:[]) $ testVersion route53Version
    [ testGroup "Hosted Zones"
        [ testGroup "Requests"
            [ testProperty "CreateHostedZone"                 (prop :: Rq CreateHostedZone)
            , testProperty "GetHostedZone"                    (prop :: Rq GetHostedZone)
            , testProperty "ListHostedZones"                  (prop :: Rq ListHostedZones)
            , testProperty "DeleteHostedZone"                 (prop :: Rq DeleteHostedZone)
            ]
        , testGroup "Responses"
            [ testProperty "CreateHostedZoneResponse"         (prop :: Rs CreateHostedZoneResponse)
            , testProperty "GetHostedZoneResponse"            (prop :: Rs GetHostedZoneResponse)
            , testProperty "ListHostedZonesResponse"          (prop :: Rs ListHostedZonesResponse)
            , testProperty "DeleteHostedZoneResponse"         (prop :: Rs DeleteHostedZoneResponse)
            ]
        ]

    , testGroup "Record Sets"
        [ testGroup "Requests"
            [ -- testProperty "ChangeResourceRecordSets"         (prop :: Rq ChangeResourceRecordSets)
              testProperty "ListResourceRecordSets"           (prop :: Rq ListResourceRecordSets)
            , testProperty "GetChange"                        (prop :: Rq GetChange)
            ]
        , testGroup "Responses"
            [ testProperty "ChangeResourceRecordSetsResponse" (prop :: Rs ChangeResourceRecordSetsResponse)
            , testProperty "ListResourceRecordSetsResponse"   (prop :: Rs ListResourceRecordSetsResponse)
            , testProperty "GetChangeResponse"                (prop :: Rs GetChangeResponse)
            ]
        ]

    , testGroup "Health Checks"
        [ testGroup "Requests"
            [ testProperty "CreateHealthCheck" (prop :: Rq CreateHealthCheck)
            , testProperty "GetHealthCheck"    (prop :: Rq GetHealthCheck)
            , testProperty "ListHealthChecks"  (prop :: Rq ListHealthChecks)
            , testProperty "DeleteHealthCheck" (prop :: Rq DeleteHealthCheck)
            ]
        , testGroup "Responses"
            [ testProperty "CreateHealthCheckResponse" (prop :: Rs CreateHealthCheckResponse)
            , testProperty "GetHealthCheckResponse"    (prop :: Rs GetHealthCheckResponse)
            , testProperty "ListHealthChecksResponse"  (prop :: Rs ListHealthChecksResponse)
            , testProperty "DeleteHealthCheckResponse" (prop :: Rs DeleteHealthCheckResponse)
            ]
        ]
    ]

instance ToJSON CallerReference where
    toJSON = String . unCallerReference

instance ToJSON Protocol where
    toJSON = stringify . show

instance ToJSON ChangeStatus where
    toJSON = stringify . show

instance ToJSON RecordType where
    toJSON = stringify . show

instance ToJSON Failover where
    toJSON = stringify . show

$(deriveArbitrary
    [ ''CallerReference
    , ''Protocol
    , ''ChangeStatus
    , ''RecordType
    , ''Failover
    ])

$(deriveDependency
    [ ''DelegationSet
    , ''ChangeInfo
    , ''Config
    , ''HostedZoneId
    , ''HostedZone
    , ''ResourceRecords
    , ''ResourceRecordSet
    , ''HealthCheckId
    , ''HealthCheck
    , ''HealthCheckConfig
    , ''AliasTarget
    , ''ChangeAction
    , ''ChangeBatch
    , ''ChangeId
    , ''Change
    ])

$(deriveProperty "test/resources/Route53"
    [ ''CreateHostedZone
    , ''CreateHostedZoneResponse
    , ''GetHostedZone
    , ''GetHostedZoneResponse
    , ''ListHostedZones
    , ''ListHostedZonesResponse
    , ''DeleteHostedZone
    , ''DeleteHostedZoneResponse
    , ''ChangeResourceRecordSets
    , ''ChangeResourceRecordSetsResponse
    , ''ListResourceRecordSets
    , ''ListResourceRecordSetsResponse
    , ''GetChange
    , ''GetChangeResponse
    , ''CreateHealthCheck
    , ''CreateHealthCheckResponse
    , ''GetHealthCheck
    , ''GetHealthCheckResponse
    , ''ListHealthChecks
    , ''ListHealthChecksResponse
    , ''DeleteHealthCheck
    , ''DeleteHealthCheckResponse
    ])
