{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Route53 where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Route53
import Test.AWS.Route53.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testAssociateVPCWithHostedZone $
--             associateVPCWithHostedZone
--
--         , testGetHealthCheckLastFailureReason $
--             getHealthCheckLastFailureReason
--
--         , testListHostedZonesByName $
--             listHostedZonesByName
--
--         , testDeleteReusableDelegationSet $
--             deleteReusableDelegationSet
--
--         , testListReusableDelegationSets $
--             listReusableDelegationSets
--
--         , testGetCheckerIPRanges $
--             getCheckerIPRanges
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testChangeResourceRecordSets $
--             changeResourceRecordSets
--
--         , testGetChange $
--             getChange
--
--         , testCreateHostedZone $
--             createHostedZone
--
--         , testDeleteHealthCheck $
--             deleteHealthCheck
--
--         , testUpdateHealthCheck $
--             updateHealthCheck
--
--         , testChangeTagsForResource $
--             changeTagsForResource
--
--         , testCreateHealthCheck $
--             createHealthCheck
--
--         , testListHostedZones $
--             listHostedZones
--
--         , testDisassociateVPCFromHostedZone $
--             disassociateVPCFromHostedZone
--
--         , testGetHostedZone $
--             getHostedZone
--
--         , testListGeoLocations $
--             listGeoLocations
--
--         , testGetHealthCheck $
--             getHealthCheck
--
--         , testListResourceRecordSets $
--             listResourceRecordSets
--
--         , testGetHealthCheckCount $
--             getHealthCheckCount
--
--         , testCreateReusableDelegationSet $
--             createReusableDelegationSet
--
--         , testGetHostedZoneCount $
--             getHostedZoneCount
--
--         , testGetReusableDelegationSet $
--             getReusableDelegationSet
--
--         , testUpdateHostedZoneComment $
--             updateHostedZoneComment
--
--         , testGetHealthCheckStatus $
--             getHealthCheckStatus
--
--         , testListHealthChecks $
--             listHealthChecks
--
--         , testDeleteHostedZone $
--             deleteHostedZone
--
--         , testGetGeoLocation $
--             getGeoLocation
--
--         , testListTagsForResources $
--             listTagsForResources
--
--           ]

--     , testGroup "response"
--         [ testAssociateVPCWithHostedZoneResponse $
--             associateVPCWithHostedZoneResponse
--
--         , testGetHealthCheckLastFailureReasonResponse $
--             getHealthCheckLastFailureReasonResponse
--
--         , testListHostedZonesByNameResponse $
--             listHostedZonesByNameResponse
--
--         , testDeleteReusableDelegationSetResponse $
--             deleteReusableDelegationSetResponse
--
--         , testListReusableDelegationSetsResponse $
--             listReusableDelegationSetsResponse
--
--         , testGetCheckerIPRangesResponse $
--             getCheckerIPRangesResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testChangeResourceRecordSetsResponse $
--             changeResourceRecordSetsResponse
--
--         , testGetChangeResponse $
--             getChangeResponse
--
--         , testCreateHostedZoneResponse $
--             createHostedZoneResponse
--
--         , testDeleteHealthCheckResponse $
--             deleteHealthCheckResponse
--
--         , testUpdateHealthCheckResponse $
--             updateHealthCheckResponse
--
--         , testChangeTagsForResourceResponse $
--             changeTagsForResourceResponse
--
--         , testCreateHealthCheckResponse $
--             createHealthCheckResponse
--
--         , testListHostedZonesResponse $
--             listHostedZonesResponse
--
--         , testDisassociateVPCFromHostedZoneResponse $
--             disassociateVPCFromHostedZoneResponse
--
--         , testGetHostedZoneResponse $
--             getHostedZoneResponse
--
--         , testListGeoLocationsResponse $
--             listGeoLocationsResponse
--
--         , testGetHealthCheckResponse $
--             getHealthCheckResponse
--
--         , testListResourceRecordSetsResponse $
--             listResourceRecordSetsResponse
--
--         , testGetHealthCheckCountResponse $
--             getHealthCheckCountResponse
--
--         , testCreateReusableDelegationSetResponse $
--             createReusableDelegationSetResponse
--
--         , testGetHostedZoneCountResponse $
--             getHostedZoneCountResponse
--
--         , testGetReusableDelegationSetResponse $
--             getReusableDelegationSetResponse
--
--         , testUpdateHostedZoneCommentResponse $
--             updateHostedZoneCommentResponse
--
--         , testGetHealthCheckStatusResponse $
--             getHealthCheckStatusResponse
--
--         , testListHealthChecksResponse $
--             listHealthChecksResponse
--
--         , testDeleteHostedZoneResponse $
--             deleteHostedZoneResponse
--
--         , testGetGeoLocationResponse $
--             getGeoLocationResponse
--
--         , testListTagsForResourcesResponse $
--             listTagsForResourcesResponse
--
--           ]
--     ]

-- Requests

testAssociateVPCWithHostedZone :: AssociateVPCWithHostedZone -> TestTree
testAssociateVPCWithHostedZone = req
    "AssociateVPCWithHostedZone"
    "fixture/AssociateVPCWithHostedZone"

testGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
testGetHealthCheckLastFailureReason = req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason"

testListHostedZonesByName :: ListHostedZonesByName -> TestTree
testListHostedZonesByName = req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName"

testDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
testDeleteReusableDelegationSet = req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet"

testListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
testListReusableDelegationSets = req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets"

testGetCheckerIPRanges :: GetCheckerIPRanges -> TestTree
testGetCheckerIPRanges = req
    "GetCheckerIPRanges"
    "fixture/GetCheckerIPRanges"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource"

testChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
testChangeResourceRecordSets = req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets"

testGetChange :: GetChange -> TestTree
testGetChange = req
    "GetChange"
    "fixture/GetChange"

testCreateHostedZone :: CreateHostedZone -> TestTree
testCreateHostedZone = req
    "CreateHostedZone"
    "fixture/CreateHostedZone"

testDeleteHealthCheck :: DeleteHealthCheck -> TestTree
testDeleteHealthCheck = req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck"

testUpdateHealthCheck :: UpdateHealthCheck -> TestTree
testUpdateHealthCheck = req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck"

testChangeTagsForResource :: ChangeTagsForResource -> TestTree
testChangeTagsForResource = req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource"

testCreateHealthCheck :: CreateHealthCheck -> TestTree
testCreateHealthCheck = req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck"

testListHostedZones :: ListHostedZones -> TestTree
testListHostedZones = req
    "ListHostedZones"
    "fixture/ListHostedZones"

testDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
testDisassociateVPCFromHostedZone = req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone"

testGetHostedZone :: GetHostedZone -> TestTree
testGetHostedZone = req
    "GetHostedZone"
    "fixture/GetHostedZone"

testListGeoLocations :: ListGeoLocations -> TestTree
testListGeoLocations = req
    "ListGeoLocations"
    "fixture/ListGeoLocations"

testGetHealthCheck :: GetHealthCheck -> TestTree
testGetHealthCheck = req
    "GetHealthCheck"
    "fixture/GetHealthCheck"

testListResourceRecordSets :: ListResourceRecordSets -> TestTree
testListResourceRecordSets = req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets"

testGetHealthCheckCount :: GetHealthCheckCount -> TestTree
testGetHealthCheckCount = req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount"

testCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
testCreateReusableDelegationSet = req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet"

testGetHostedZoneCount :: GetHostedZoneCount -> TestTree
testGetHostedZoneCount = req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount"

testGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
testGetReusableDelegationSet = req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet"

testUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
testUpdateHostedZoneComment = req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment"

testGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
testGetHealthCheckStatus = req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus"

testListHealthChecks :: ListHealthChecks -> TestTree
testListHealthChecks = req
    "ListHealthChecks"
    "fixture/ListHealthChecks"

testDeleteHostedZone :: DeleteHostedZone -> TestTree
testDeleteHostedZone = req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone"

testGetGeoLocation :: GetGeoLocation -> TestTree
testGetGeoLocation = req
    "GetGeoLocation"
    "fixture/GetGeoLocation"

testListTagsForResources :: ListTagsForResources -> TestTree
testListTagsForResources = req
    "ListTagsForResources"
    "fixture/ListTagsForResources"

-- Responses

testAssociateVPCWithHostedZoneResponse :: AssociateVPCWithHostedZoneResponse -> TestTree
testAssociateVPCWithHostedZoneResponse = res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse"
    (Proxy :: Proxy AssociateVPCWithHostedZone)

testGetHealthCheckLastFailureReasonResponse :: GetHealthCheckLastFailureReasonResponse -> TestTree
testGetHealthCheckLastFailureReasonResponse = res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse"
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

testListHostedZonesByNameResponse :: ListHostedZonesByNameResponse -> TestTree
testListHostedZonesByNameResponse = res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse"
    (Proxy :: Proxy ListHostedZonesByName)

testDeleteReusableDelegationSetResponse :: DeleteReusableDelegationSetResponse -> TestTree
testDeleteReusableDelegationSetResponse = res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse"
    (Proxy :: Proxy DeleteReusableDelegationSet)

testListReusableDelegationSetsResponse :: ListReusableDelegationSetsResponse -> TestTree
testListReusableDelegationSetsResponse = res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse"
    (Proxy :: Proxy ListReusableDelegationSets)

testGetCheckerIPRangesResponse :: GetCheckerIPRangesResponse -> TestTree
testGetCheckerIPRangesResponse = res
    "GetCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse"
    (Proxy :: Proxy GetCheckerIPRanges)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

testChangeResourceRecordSetsResponse :: ChangeResourceRecordSetsResponse -> TestTree
testChangeResourceRecordSetsResponse = res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse"
    (Proxy :: Proxy ChangeResourceRecordSets)

testGetChangeResponse :: GetChangeResponse -> TestTree
testGetChangeResponse = res
    "GetChangeResponse"
    "fixture/GetChangeResponse"
    (Proxy :: Proxy GetChange)

testCreateHostedZoneResponse :: CreateHostedZoneResponse -> TestTree
testCreateHostedZoneResponse = res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse"
    (Proxy :: Proxy CreateHostedZone)

testDeleteHealthCheckResponse :: DeleteHealthCheckResponse -> TestTree
testDeleteHealthCheckResponse = res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse"
    (Proxy :: Proxy DeleteHealthCheck)

testUpdateHealthCheckResponse :: UpdateHealthCheckResponse -> TestTree
testUpdateHealthCheckResponse = res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse"
    (Proxy :: Proxy UpdateHealthCheck)

testChangeTagsForResourceResponse :: ChangeTagsForResourceResponse -> TestTree
testChangeTagsForResourceResponse = res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse"
    (Proxy :: Proxy ChangeTagsForResource)

testCreateHealthCheckResponse :: CreateHealthCheckResponse -> TestTree
testCreateHealthCheckResponse = res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse"
    (Proxy :: Proxy CreateHealthCheck)

testListHostedZonesResponse :: ListHostedZonesResponse -> TestTree
testListHostedZonesResponse = res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse"
    (Proxy :: Proxy ListHostedZones)

testDisassociateVPCFromHostedZoneResponse :: DisassociateVPCFromHostedZoneResponse -> TestTree
testDisassociateVPCFromHostedZoneResponse = res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse"
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

testGetHostedZoneResponse :: GetHostedZoneResponse -> TestTree
testGetHostedZoneResponse = res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse"
    (Proxy :: Proxy GetHostedZone)

testListGeoLocationsResponse :: ListGeoLocationsResponse -> TestTree
testListGeoLocationsResponse = res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse"
    (Proxy :: Proxy ListGeoLocations)

testGetHealthCheckResponse :: GetHealthCheckResponse -> TestTree
testGetHealthCheckResponse = res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse"
    (Proxy :: Proxy GetHealthCheck)

testListResourceRecordSetsResponse :: ListResourceRecordSetsResponse -> TestTree
testListResourceRecordSetsResponse = res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse"
    (Proxy :: Proxy ListResourceRecordSets)

testGetHealthCheckCountResponse :: GetHealthCheckCountResponse -> TestTree
testGetHealthCheckCountResponse = res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse"
    (Proxy :: Proxy GetHealthCheckCount)

testCreateReusableDelegationSetResponse :: CreateReusableDelegationSetResponse -> TestTree
testCreateReusableDelegationSetResponse = res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse"
    (Proxy :: Proxy CreateReusableDelegationSet)

testGetHostedZoneCountResponse :: GetHostedZoneCountResponse -> TestTree
testGetHostedZoneCountResponse = res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse"
    (Proxy :: Proxy GetHostedZoneCount)

testGetReusableDelegationSetResponse :: GetReusableDelegationSetResponse -> TestTree
testGetReusableDelegationSetResponse = res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse"
    (Proxy :: Proxy GetReusableDelegationSet)

testUpdateHostedZoneCommentResponse :: UpdateHostedZoneCommentResponse -> TestTree
testUpdateHostedZoneCommentResponse = res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse"
    (Proxy :: Proxy UpdateHostedZoneComment)

testGetHealthCheckStatusResponse :: GetHealthCheckStatusResponse -> TestTree
testGetHealthCheckStatusResponse = res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse"
    (Proxy :: Proxy GetHealthCheckStatus)

testListHealthChecksResponse :: ListHealthChecksResponse -> TestTree
testListHealthChecksResponse = res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse"
    (Proxy :: Proxy ListHealthChecks)

testDeleteHostedZoneResponse :: DeleteHostedZoneResponse -> TestTree
testDeleteHostedZoneResponse = res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse"
    (Proxy :: Proxy DeleteHostedZone)

testGetGeoLocationResponse :: GetGeoLocationResponse -> TestTree
testGetGeoLocationResponse = res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse"
    (Proxy :: Proxy GetGeoLocation)

testListTagsForResourcesResponse :: ListTagsForResourcesResponse -> TestTree
testListTagsForResourcesResponse = res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse"
    (Proxy :: Proxy ListTagsForResources)

instance Out AliasTarget
instance Out AssociateVPCWithHostedZone
instance Out AssociateVPCWithHostedZoneResponse
instance Out Change
instance Out ChangeAction
instance Out ChangeBatch
instance Out ChangeInfo
instance Out ChangeResourceRecordSets
instance Out ChangeResourceRecordSetsResponse
instance Out ChangeStatus
instance Out ChangeTagsForResource
instance Out ChangeTagsForResourceResponse
instance Out CreateHealthCheck
instance Out CreateHealthCheckResponse
instance Out CreateHostedZone
instance Out CreateHostedZoneResponse
instance Out CreateReusableDelegationSet
instance Out CreateReusableDelegationSetResponse
instance Out DelegationSet
instance Out DeleteHealthCheck
instance Out DeleteHealthCheckResponse
instance Out DeleteHostedZone
instance Out DeleteHostedZoneResponse
instance Out DeleteReusableDelegationSet
instance Out DeleteReusableDelegationSetResponse
instance Out DisassociateVPCFromHostedZone
instance Out DisassociateVPCFromHostedZoneResponse
instance Out Failover
instance Out GeoLocation
instance Out GeoLocationDetails
instance Out GetChange
instance Out GetChangeResponse
instance Out GetCheckerIPRanges
instance Out GetCheckerIPRangesResponse
instance Out GetGeoLocation
instance Out GetGeoLocationResponse
instance Out GetHealthCheck
instance Out GetHealthCheckCount
instance Out GetHealthCheckCountResponse
instance Out GetHealthCheckLastFailureReason
instance Out GetHealthCheckLastFailureReasonResponse
instance Out GetHealthCheckResponse
instance Out GetHealthCheckStatus
instance Out GetHealthCheckStatusResponse
instance Out GetHostedZone
instance Out GetHostedZoneCount
instance Out GetHostedZoneCountResponse
instance Out GetHostedZoneResponse
instance Out GetReusableDelegationSet
instance Out GetReusableDelegationSetResponse
instance Out HealthCheck
instance Out HealthCheckConfig
instance Out HealthCheckObservation
instance Out HealthCheckType
instance Out HostedZone
instance Out HostedZoneConfig
instance Out ListGeoLocations
instance Out ListGeoLocationsResponse
instance Out ListHealthChecks
instance Out ListHealthChecksResponse
instance Out ListHostedZones
instance Out ListHostedZonesByName
instance Out ListHostedZonesByNameResponse
instance Out ListHostedZonesResponse
instance Out ListResourceRecordSets
instance Out ListResourceRecordSetsResponse
instance Out ListReusableDelegationSets
instance Out ListReusableDelegationSetsResponse
instance Out ListTagsForResource
instance Out ListTagsForResourceResponse
instance Out ListTagsForResources
instance Out ListTagsForResourcesResponse
instance Out RecordType
instance Out ResourceRecord
instance Out ResourceRecordSet
instance Out ResourceTagSet
instance Out StatusReport
instance Out Tag
instance Out TagResourceType
instance Out UpdateHealthCheck
instance Out UpdateHealthCheckResponse
instance Out UpdateHostedZoneComment
instance Out UpdateHostedZoneCommentResponse
instance Out VPC
instance Out VPCRegion
