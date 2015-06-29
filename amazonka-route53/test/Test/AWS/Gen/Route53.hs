-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.Route53 where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.Route53

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
testAssociateVPCWithHostedZone = undefined

testGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
testGetHealthCheckLastFailureReason = undefined

testListHostedZonesByName :: ListHostedZonesByName -> TestTree
testListHostedZonesByName = undefined

testDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
testDeleteReusableDelegationSet = undefined

testListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
testListReusableDelegationSets = undefined

testGetCheckerIPRanges :: GetCheckerIPRanges -> TestTree
testGetCheckerIPRanges = undefined

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = undefined

testChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
testChangeResourceRecordSets = undefined

testGetChange :: GetChange -> TestTree
testGetChange = undefined

testCreateHostedZone :: CreateHostedZone -> TestTree
testCreateHostedZone = undefined

testDeleteHealthCheck :: DeleteHealthCheck -> TestTree
testDeleteHealthCheck = undefined

testUpdateHealthCheck :: UpdateHealthCheck -> TestTree
testUpdateHealthCheck = undefined

testChangeTagsForResource :: ChangeTagsForResource -> TestTree
testChangeTagsForResource = undefined

testCreateHealthCheck :: CreateHealthCheck -> TestTree
testCreateHealthCheck = undefined

testListHostedZones :: ListHostedZones -> TestTree
testListHostedZones = undefined

testDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
testDisassociateVPCFromHostedZone = undefined

testGetHostedZone :: GetHostedZone -> TestTree
testGetHostedZone = undefined

testListGeoLocations :: ListGeoLocations -> TestTree
testListGeoLocations = undefined

testGetHealthCheck :: GetHealthCheck -> TestTree
testGetHealthCheck = undefined

testListResourceRecordSets :: ListResourceRecordSets -> TestTree
testListResourceRecordSets = undefined

testGetHealthCheckCount :: GetHealthCheckCount -> TestTree
testGetHealthCheckCount = undefined

testCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
testCreateReusableDelegationSet = undefined

testGetHostedZoneCount :: GetHostedZoneCount -> TestTree
testGetHostedZoneCount = undefined

testGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
testGetReusableDelegationSet = undefined

testUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
testUpdateHostedZoneComment = undefined

testGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
testGetHealthCheckStatus = undefined

testListHealthChecks :: ListHealthChecks -> TestTree
testListHealthChecks = undefined

testDeleteHostedZone :: DeleteHostedZone -> TestTree
testDeleteHostedZone = undefined

testGetGeoLocation :: GetGeoLocation -> TestTree
testGetGeoLocation = undefined

testListTagsForResources :: ListTagsForResources -> TestTree
testListTagsForResources = undefined

-- Responses

testAssociateVPCWithHostedZoneResponse :: AssociateVPCWithHostedZoneResponse -> TestTree
testAssociateVPCWithHostedZoneResponse = resp
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse"
    (Proxy :: Proxy AssociateVPCWithHostedZone)

testGetHealthCheckLastFailureReasonResponse :: GetHealthCheckLastFailureReasonResponse -> TestTree
testGetHealthCheckLastFailureReasonResponse = resp
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse"
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

testListHostedZonesByNameResponse :: ListHostedZonesByNameResponse -> TestTree
testListHostedZonesByNameResponse = resp
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse"
    (Proxy :: Proxy ListHostedZonesByName)

testDeleteReusableDelegationSetResponse :: DeleteReusableDelegationSetResponse -> TestTree
testDeleteReusableDelegationSetResponse = resp
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse"
    (Proxy :: Proxy DeleteReusableDelegationSet)

testListReusableDelegationSetsResponse :: ListReusableDelegationSetsResponse -> TestTree
testListReusableDelegationSetsResponse = resp
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse"
    (Proxy :: Proxy ListReusableDelegationSets)

testGetCheckerIPRangesResponse :: GetCheckerIPRangesResponse -> TestTree
testGetCheckerIPRangesResponse = resp
    "GetCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse"
    (Proxy :: Proxy GetCheckerIPRanges)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = resp
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse"
    (Proxy :: Proxy ListTagsForResource)

testChangeResourceRecordSetsResponse :: ChangeResourceRecordSetsResponse -> TestTree
testChangeResourceRecordSetsResponse = resp
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse"
    (Proxy :: Proxy ChangeResourceRecordSets)

testGetChangeResponse :: GetChangeResponse -> TestTree
testGetChangeResponse = resp
    "GetChangeResponse"
    "fixture/GetChangeResponse"
    (Proxy :: Proxy GetChange)

testCreateHostedZoneResponse :: CreateHostedZoneResponse -> TestTree
testCreateHostedZoneResponse = resp
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse"
    (Proxy :: Proxy CreateHostedZone)

testDeleteHealthCheckResponse :: DeleteHealthCheckResponse -> TestTree
testDeleteHealthCheckResponse = resp
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse"
    (Proxy :: Proxy DeleteHealthCheck)

testUpdateHealthCheckResponse :: UpdateHealthCheckResponse -> TestTree
testUpdateHealthCheckResponse = resp
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse"
    (Proxy :: Proxy UpdateHealthCheck)

testChangeTagsForResourceResponse :: ChangeTagsForResourceResponse -> TestTree
testChangeTagsForResourceResponse = resp
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse"
    (Proxy :: Proxy ChangeTagsForResource)

testCreateHealthCheckResponse :: CreateHealthCheckResponse -> TestTree
testCreateHealthCheckResponse = resp
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse"
    (Proxy :: Proxy CreateHealthCheck)

testListHostedZonesResponse :: ListHostedZonesResponse -> TestTree
testListHostedZonesResponse = resp
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse"
    (Proxy :: Proxy ListHostedZones)

testDisassociateVPCFromHostedZoneResponse :: DisassociateVPCFromHostedZoneResponse -> TestTree
testDisassociateVPCFromHostedZoneResponse = resp
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse"
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

testGetHostedZoneResponse :: GetHostedZoneResponse -> TestTree
testGetHostedZoneResponse = resp
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse"
    (Proxy :: Proxy GetHostedZone)

testListGeoLocationsResponse :: ListGeoLocationsResponse -> TestTree
testListGeoLocationsResponse = resp
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse"
    (Proxy :: Proxy ListGeoLocations)

testGetHealthCheckResponse :: GetHealthCheckResponse -> TestTree
testGetHealthCheckResponse = resp
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse"
    (Proxy :: Proxy GetHealthCheck)

testListResourceRecordSetsResponse :: ListResourceRecordSetsResponse -> TestTree
testListResourceRecordSetsResponse = resp
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse"
    (Proxy :: Proxy ListResourceRecordSets)

testGetHealthCheckCountResponse :: GetHealthCheckCountResponse -> TestTree
testGetHealthCheckCountResponse = resp
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse"
    (Proxy :: Proxy GetHealthCheckCount)

testCreateReusableDelegationSetResponse :: CreateReusableDelegationSetResponse -> TestTree
testCreateReusableDelegationSetResponse = resp
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse"
    (Proxy :: Proxy CreateReusableDelegationSet)

testGetHostedZoneCountResponse :: GetHostedZoneCountResponse -> TestTree
testGetHostedZoneCountResponse = resp
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse"
    (Proxy :: Proxy GetHostedZoneCount)

testGetReusableDelegationSetResponse :: GetReusableDelegationSetResponse -> TestTree
testGetReusableDelegationSetResponse = resp
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse"
    (Proxy :: Proxy GetReusableDelegationSet)

testUpdateHostedZoneCommentResponse :: UpdateHostedZoneCommentResponse -> TestTree
testUpdateHostedZoneCommentResponse = resp
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse"
    (Proxy :: Proxy UpdateHostedZoneComment)

testGetHealthCheckStatusResponse :: GetHealthCheckStatusResponse -> TestTree
testGetHealthCheckStatusResponse = resp
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse"
    (Proxy :: Proxy GetHealthCheckStatus)

testListHealthChecksResponse :: ListHealthChecksResponse -> TestTree
testListHealthChecksResponse = resp
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse"
    (Proxy :: Proxy ListHealthChecks)

testDeleteHostedZoneResponse :: DeleteHostedZoneResponse -> TestTree
testDeleteHostedZoneResponse = resp
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse"
    (Proxy :: Proxy DeleteHostedZone)

testGetGeoLocationResponse :: GetGeoLocationResponse -> TestTree
testGetGeoLocationResponse = resp
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse"
    (Proxy :: Proxy GetGeoLocation)

testListTagsForResourcesResponse :: ListTagsForResourcesResponse -> TestTree
testListTagsForResourcesResponse = resp
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse"
    (Proxy :: Proxy ListTagsForResources)
