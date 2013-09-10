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

import Network.AWS.Route53
import Test.Common

tests :: [Test]
tests = (:[]) $ testVersion route53Version
    [ testGroup "Hosted Zones"
        [ testGroup "Requests"
            [ testProperty "CreateHostedZone"                 (prop :: TRq CreateHostedZone)
            , testProperty "GetHostedZone"                    (prop :: TRq GetHostedZone)
            , testProperty "ListHostedZones"                  (prop :: TRq ListHostedZones)
            , testProperty "DeleteHostedZone"                 (prop :: TRq DeleteHostedZone)
            ]
        , testGroup "Responses"
            [ testProperty "CreateHostedZoneResponse"         (prop :: TRs CreateHostedZoneResponse)
            , testProperty "GetHostedZoneResponse"            (prop :: TRs GetHostedZoneResponse)
            , testProperty "ListHostedZonesResponse"          (prop :: TRs ListHostedZonesResponse)
            , testProperty "DeleteHostedZoneResponse"         (prop :: TRs DeleteHostedZoneResponse)
            ]
        ]

    , testGroup "Record Sets"
        [ testGroup "Requests"
            [ -- testProperty "ChangeResourceRecordSets"         (prop :: TRq ChangeResourceRecordSets)
              testProperty "ListResourceRecordSets"           (prop :: TRq ListResourceRecordSets)
            , testProperty "GetChange"                        (prop :: TRq GetChange)
            ]
        , testGroup "Responses"
            [ testProperty "ChangeResourceRecordSetsResponse" (prop :: TRs ChangeResourceRecordSetsResponse)
            , testProperty "ListResourceRecordSetsResponse"   (prop :: TRs ListResourceRecordSetsResponse)
            , testProperty "GetChangeResponse"                (prop :: TRs GetChangeResponse)
            ]
        ]

    , testGroup "Health Checks"
        [ testGroup "Requests"
            [ testProperty "CreateHealthCheck" (prop :: TRq CreateHealthCheck)
            , testProperty "GetHealthCheck"    (prop :: TRq GetHealthCheck)
            , testProperty "ListHealthChecks"  (prop :: TRq ListHealthChecks)
            , testProperty "DeleteHealthCheck" (prop :: TRq DeleteHealthCheck)
            ]
        , testGroup "Responses"
            [ testProperty "CreateHealthCheckResponse" (prop :: TRs CreateHealthCheckResponse)
            , testProperty "GetHealthCheckResponse"    (prop :: TRs GetHealthCheckResponse)
            , testProperty "ListHealthChecksResponse"  (prop :: TRs ListHealthChecksResponse)
            , testProperty "DeleteHealthCheckResponse" (prop :: TRs DeleteHealthCheckResponse)
            ]
        ]
    ]

instance ToJSON CallerReference where
    toJSON = String . unCallerReference

instance ToJSON Protocol where
    toJSON = stringify

instance ToJSON ChangeStatus where
    toJSON = stringify

instance ToJSON RecordType where
    toJSON = stringify

instance ToJSON Failover where
    toJSON = stringify

instance ToJSON ChangeId where
    toJSON = String . unChangeId

instance ToJSON HostedZoneId where
    toJSON = String . unHostedZoneId

instance ToJSON HealthCheckId where
    toJSON = String . unHealthCheckId

$(deriveArbitrary
    [ ''CallerReference
    , ''Protocol
    , ''ChangeStatus
    , ''RecordType
    , ''Failover
    , ''ChangeId
    , ''HostedZoneId
    , ''HealthCheckId
    ])

$(deriveDependency
    [ ''DelegationSet
    , ''ChangeInfo
    , ''Config
    , ''HostedZone
    , ''ResourceRecords
    , ''ResourceRecordSet
    , ''HealthCheck
    , ''HealthCheckConfig
    , ''AliasTarget
    , ''ChangeAction
    , ''ChangeBatch
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
