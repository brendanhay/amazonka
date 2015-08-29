{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    "fixture/AssociateVPCWithHostedZone.yaml"

testGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
testGetHealthCheckLastFailureReason = req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

testListHostedZonesByName :: ListHostedZonesByName -> TestTree
testListHostedZonesByName = req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

testDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
testDeleteReusableDelegationSet = req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

testListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
testListReusableDelegationSets = req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets.yaml"

testGetCheckerIPRanges :: GetCheckerIPRanges -> TestTree
testGetCheckerIPRanges = req
    "GetCheckerIPRanges"
    "fixture/GetCheckerIPRanges.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
testChangeResourceRecordSets = req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

testGetChange :: GetChange -> TestTree
testGetChange = req
    "GetChange"
    "fixture/GetChange.yaml"

testCreateHostedZone :: CreateHostedZone -> TestTree
testCreateHostedZone = req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

testDeleteHealthCheck :: DeleteHealthCheck -> TestTree
testDeleteHealthCheck = req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck.yaml"

testUpdateHealthCheck :: UpdateHealthCheck -> TestTree
testUpdateHealthCheck = req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck.yaml"

testChangeTagsForResource :: ChangeTagsForResource -> TestTree
testChangeTagsForResource = req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

testCreateHealthCheck :: CreateHealthCheck -> TestTree
testCreateHealthCheck = req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck.yaml"

testListHostedZones :: ListHostedZones -> TestTree
testListHostedZones = req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

testDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
testDisassociateVPCFromHostedZone = req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

testGetHostedZone :: GetHostedZone -> TestTree
testGetHostedZone = req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

testListGeoLocations :: ListGeoLocations -> TestTree
testListGeoLocations = req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

testGetHealthCheck :: GetHealthCheck -> TestTree
testGetHealthCheck = req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

testListResourceRecordSets :: ListResourceRecordSets -> TestTree
testListResourceRecordSets = req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

testGetHealthCheckCount :: GetHealthCheckCount -> TestTree
testGetHealthCheckCount = req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

testCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
testCreateReusableDelegationSet = req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

testGetHostedZoneCount :: GetHostedZoneCount -> TestTree
testGetHostedZoneCount = req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

testGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
testGetReusableDelegationSet = req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet.yaml"

testUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
testUpdateHostedZoneComment = req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

testGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
testGetHealthCheckStatus = req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus.yaml"

testListHealthChecks :: ListHealthChecks -> TestTree
testListHealthChecks = req
    "ListHealthChecks"
    "fixture/ListHealthChecks.yaml"

testDeleteHostedZone :: DeleteHostedZone -> TestTree
testDeleteHostedZone = req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone.yaml"

testGetGeoLocation :: GetGeoLocation -> TestTree
testGetGeoLocation = req
    "GetGeoLocation"
    "fixture/GetGeoLocation.yaml"

testListTagsForResources :: ListTagsForResources -> TestTree
testListTagsForResources = req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

-- Responses

testAssociateVPCWithHostedZoneResponse :: AssociateVPCWithHostedZoneResponse -> TestTree
testAssociateVPCWithHostedZoneResponse = res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy AssociateVPCWithHostedZone)

testGetHealthCheckLastFailureReasonResponse :: GetHealthCheckLastFailureReasonResponse -> TestTree
testGetHealthCheckLastFailureReasonResponse = res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

testListHostedZonesByNameResponse :: ListHostedZonesByNameResponse -> TestTree
testListHostedZonesByNameResponse = res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZonesByName)

testDeleteReusableDelegationSetResponse :: DeleteReusableDelegationSetResponse -> TestTree
testDeleteReusableDelegationSetResponse = res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy DeleteReusableDelegationSet)

testListReusableDelegationSetsResponse :: ListReusableDelegationSetsResponse -> TestTree
testListReusableDelegationSetsResponse = res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    route53
    (Proxy :: Proxy ListReusableDelegationSets)

testGetCheckerIPRangesResponse :: GetCheckerIPRangesResponse -> TestTree
testGetCheckerIPRangesResponse = res
    "GetCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse.proto"
    route53
    (Proxy :: Proxy GetCheckerIPRanges)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ListTagsForResource)

testChangeResourceRecordSetsResponse :: ChangeResourceRecordSetsResponse -> TestTree
testChangeResourceRecordSetsResponse = res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    route53
    (Proxy :: Proxy ChangeResourceRecordSets)

testGetChangeResponse :: GetChangeResponse -> TestTree
testGetChangeResponse = res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    route53
    (Proxy :: Proxy GetChange)

testCreateHostedZoneResponse :: CreateHostedZoneResponse -> TestTree
testCreateHostedZoneResponse = res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy CreateHostedZone)

testDeleteHealthCheckResponse :: DeleteHealthCheckResponse -> TestTree
testDeleteHealthCheckResponse = res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy DeleteHealthCheck)

testUpdateHealthCheckResponse :: UpdateHealthCheckResponse -> TestTree
testUpdateHealthCheckResponse = res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy UpdateHealthCheck)

testChangeTagsForResourceResponse :: ChangeTagsForResourceResponse -> TestTree
testChangeTagsForResourceResponse = res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ChangeTagsForResource)

testCreateHealthCheckResponse :: CreateHealthCheckResponse -> TestTree
testCreateHealthCheckResponse = res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy CreateHealthCheck)

testListHostedZonesResponse :: ListHostedZonesResponse -> TestTree
testListHostedZonesResponse = res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZones)

testDisassociateVPCFromHostedZoneResponse :: DisassociateVPCFromHostedZoneResponse -> TestTree
testDisassociateVPCFromHostedZoneResponse = res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

testGetHostedZoneResponse :: GetHostedZoneResponse -> TestTree
testGetHostedZoneResponse = res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZone)

testListGeoLocationsResponse :: ListGeoLocationsResponse -> TestTree
testListGeoLocationsResponse = res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    route53
    (Proxy :: Proxy ListGeoLocations)

testGetHealthCheckResponse :: GetHealthCheckResponse -> TestTree
testGetHealthCheckResponse = res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheck)

testListResourceRecordSetsResponse :: ListResourceRecordSetsResponse -> TestTree
testListResourceRecordSetsResponse = res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    route53
    (Proxy :: Proxy ListResourceRecordSets)

testGetHealthCheckCountResponse :: GetHealthCheckCountResponse -> TestTree
testGetHealthCheckCountResponse = res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckCount)

testCreateReusableDelegationSetResponse :: CreateReusableDelegationSetResponse -> TestTree
testCreateReusableDelegationSetResponse = res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy CreateReusableDelegationSet)

testGetHostedZoneCountResponse :: GetHostedZoneCountResponse -> TestTree
testGetHostedZoneCountResponse = res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZoneCount)

testGetReusableDelegationSetResponse :: GetReusableDelegationSetResponse -> TestTree
testGetReusableDelegationSetResponse = res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy GetReusableDelegationSet)

testUpdateHostedZoneCommentResponse :: UpdateHostedZoneCommentResponse -> TestTree
testUpdateHostedZoneCommentResponse = res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    route53
    (Proxy :: Proxy UpdateHostedZoneComment)

testGetHealthCheckStatusResponse :: GetHealthCheckStatusResponse -> TestTree
testGetHealthCheckStatusResponse = res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckStatus)

testListHealthChecksResponse :: ListHealthChecksResponse -> TestTree
testListHealthChecksResponse = res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    route53
    (Proxy :: Proxy ListHealthChecks)

testDeleteHostedZoneResponse :: DeleteHostedZoneResponse -> TestTree
testDeleteHostedZoneResponse = res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy DeleteHostedZone)

testGetGeoLocationResponse :: GetGeoLocationResponse -> TestTree
testGetGeoLocationResponse = res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    route53
    (Proxy :: Proxy GetGeoLocation)

testListTagsForResourcesResponse :: ListTagsForResourcesResponse -> TestTree
testListTagsForResourcesResponse = res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    route53
    (Proxy :: Proxy ListTagsForResources)
