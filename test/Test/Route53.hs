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

tests :: TestTree
tests = testVersion route53
    [ testGroup "Hosted Zones"
        [ testGroup "Requests"
            [ qc "CreateHostedZone"                 (prop :: TRq CreateHostedZone)
            , qc "GetHostedZone"                    (prop :: TRq GetHostedZone)
            , qc "ListHostedZones"                  (prop :: TRq ListHostedZones)
            , qc "DeleteHostedZone"                 (prop :: TRq DeleteHostedZone)
            ]
        , testGroup "Responses"
            [ qc "CreateHostedZoneResponse"         (prop :: TRs CreateHostedZoneResponse)
            , qc "GetHostedZoneResponse"            (prop :: TRs GetHostedZoneResponse)
            , qc "ListHostedZonesResponse"          (prop :: TRs ListHostedZonesResponse)
            , qc "DeleteHostedZoneResponse"         (prop :: TRs DeleteHostedZoneResponse)
            ]
        ]

    , testGroup "Record Sets"
        [ testGroup "Requests"
            [ -- qc "ChangeResourceRecordSets"         (prop :: TRq ChangeResourceRecordSets)
              qc "ListResourceRecordSets"           (prop :: TRq ListResourceRecordSets)
            , qc "GetChange"                        (prop :: TRq GetChange)
            ]
        -- , testGroup "Responses"
        --     [ qc "ChangeResourceRecordSetsResponse" (prop :: TRs ChangeResourceRecordSetsResponse)
        --     , qc "ListResourceRecordSetsResponse"   (prop :: TRs ListResourceRecordSetsResponse)
        --     , qc "GetChangeResponse"                (prop :: TRs GetChangeResponse)
        --     ]
        ]

    , testGroup "Health Checks"
        [ testGroup "Requests"
            [ qc "CreateHealthCheck" (prop :: TRq CreateHealthCheck)
            , qc "GetHealthCheck"    (prop :: TRq GetHealthCheck)
            , qc "ListHealthChecks"  (prop :: TRq ListHealthChecks)
            , qc "DeleteHealthCheck" (prop :: TRq DeleteHealthCheck)
            ]
        -- , testGroup "Responses"
        --     [ qc "CreateHealthCheckResponse" (prop :: TRs CreateHealthCheckResponse)
        --     , qc "GetHealthCheckResponse"    (prop :: TRs GetHealthCheckResponse)
        --     , qc "ListHealthChecksResponse"  (prop :: TRs ListHealthChecksResponse)
        --     , qc "DeleteHealthCheckResponse" (prop :: TRs DeleteHealthCheckResponse)
        --     ]
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
