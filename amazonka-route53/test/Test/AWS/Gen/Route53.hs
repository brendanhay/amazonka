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
--         , testDeleteTrafficPolicy $
--             deleteTrafficPolicy
--
--         , testGetCheckerIPRanges $
--             getCheckerIPRanges
--
--         , testGetTrafficPolicyInstance $
--             getTrafficPolicyInstance
--
--         , testGetHealthCheckLastFailureReason $
--             getHealthCheckLastFailureReason
--
--         , testDeleteReusableDelegationSet $
--             deleteReusableDelegationSet
--
--         , testListHostedZonesByName $
--             listHostedZonesByName
--
--         , testListReusableDelegationSets $
--             listReusableDelegationSets
--
--         , testListTagsForResource $
--             listTagsForResource
--
--         , testListTrafficPolicyInstances $
--             listTrafficPolicyInstances
--
--         , testCreateTrafficPolicyInstance $
--             createTrafficPolicyInstance
--
--         , testGetChange $
--             getChange
--
--         , testChangeResourceRecordSets $
--             changeResourceRecordSets
--
--         , testDeleteHealthCheck $
--             deleteHealthCheck
--
--         , testUpdateHealthCheck $
--             updateHealthCheck
--
--         , testCreateHostedZone $
--             createHostedZone
--
--         , testListTrafficPolicyInstancesByPolicy $
--             listTrafficPolicyInstancesByPolicy
--
--         , testDisassociateVPCFromHostedZone $
--             disassociateVPCFromHostedZone
--
--         , testCreateHealthCheck $
--             createHealthCheck
--
--         , testChangeTagsForResource $
--             changeTagsForResource
--
--         , testListHostedZones $
--             listHostedZones
--
--         , testGetTrafficPolicyInstanceCount $
--             getTrafficPolicyInstanceCount
--
--         , testListGeoLocations $
--             listGeoLocations
--
--         , testGetHostedZone $
--             getHostedZone
--
--         , testGetHealthCheck $
--             getHealthCheck
--
--         , testListResourceRecordSets $
--             listResourceRecordSets
--
--         , testCreateReusableDelegationSet $
--             createReusableDelegationSet
--
--         , testGetHealthCheckCount $
--             getHealthCheckCount
--
--         , testUpdateTrafficPolicyComment $
--             updateTrafficPolicyComment
--
--         , testGetHostedZoneCount $
--             getHostedZoneCount
--
--         , testGetReusableDelegationSet $
--             getReusableDelegationSet
--
--         , testDeleteTrafficPolicyInstance $
--             deleteTrafficPolicyInstance
--
--         , testUpdateTrafficPolicyInstance $
--             updateTrafficPolicyInstance
--
--         , testUpdateHostedZoneComment $
--             updateHostedZoneComment
--
--         , testGetHealthCheckStatus $
--             getHealthCheckStatus
--
--         , testCreateTrafficPolicyVersion $
--             createTrafficPolicyVersion
--
--         , testListHealthChecks $
--             listHealthChecks
--
--         , testGetTrafficPolicy $
--             getTrafficPolicy
--
--         , testListTrafficPolicyVersions $
--             listTrafficPolicyVersions
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
--         , testCreateTrafficPolicy $
--             createTrafficPolicy
--
--         , testListTrafficPolicyInstancesByHostedZone $
--             listTrafficPolicyInstancesByHostedZone
--
--         , testListTrafficPolicies $
--             listTrafficPolicies
--
--           ]

--     , testGroup "response"
--         [ testAssociateVPCWithHostedZoneResponse $
--             associateVPCWithHostedZoneResponse
--
--         , testDeleteTrafficPolicyResponse $
--             deleteTrafficPolicyResponse
--
--         , testGetCheckerIPRangesResponse $
--             getCheckerIPRangesResponse
--
--         , testGetTrafficPolicyInstanceResponse $
--             getTrafficPolicyInstanceResponse
--
--         , testGetHealthCheckLastFailureReasonResponse $
--             getHealthCheckLastFailureReasonResponse
--
--         , testDeleteReusableDelegationSetResponse $
--             deleteReusableDelegationSetResponse
--
--         , testListHostedZonesByNameResponse $
--             listHostedZonesByNameResponse
--
--         , testListReusableDelegationSetsResponse $
--             listReusableDelegationSetsResponse
--
--         , testListTagsForResourceResponse $
--             listTagsForResourceResponse
--
--         , testListTrafficPolicyInstancesResponse $
--             listTrafficPolicyInstancesResponse
--
--         , testCreateTrafficPolicyInstanceResponse $
--             createTrafficPolicyInstanceResponse
--
--         , testGetChangeResponse $
--             getChangeResponse
--
--         , testChangeResourceRecordSetsResponse $
--             changeResourceRecordSetsResponse
--
--         , testDeleteHealthCheckResponse $
--             deleteHealthCheckResponse
--
--         , testUpdateHealthCheckResponse $
--             updateHealthCheckResponse
--
--         , testCreateHostedZoneResponse $
--             createHostedZoneResponse
--
--         , testListTrafficPolicyInstancesByPolicyResponse $
--             listTrafficPolicyInstancesByPolicyResponse
--
--         , testDisassociateVPCFromHostedZoneResponse $
--             disassociateVPCFromHostedZoneResponse
--
--         , testCreateHealthCheckResponse $
--             createHealthCheckResponse
--
--         , testChangeTagsForResourceResponse $
--             changeTagsForResourceResponse
--
--         , testListHostedZonesResponse $
--             listHostedZonesResponse
--
--         , testGetTrafficPolicyInstanceCountResponse $
--             getTrafficPolicyInstanceCountResponse
--
--         , testListGeoLocationsResponse $
--             listGeoLocationsResponse
--
--         , testGetHostedZoneResponse $
--             getHostedZoneResponse
--
--         , testGetHealthCheckResponse $
--             getHealthCheckResponse
--
--         , testListResourceRecordSetsResponse $
--             listResourceRecordSetsResponse
--
--         , testCreateReusableDelegationSetResponse $
--             createReusableDelegationSetResponse
--
--         , testGetHealthCheckCountResponse $
--             getHealthCheckCountResponse
--
--         , testUpdateTrafficPolicyCommentResponse $
--             updateTrafficPolicyCommentResponse
--
--         , testGetHostedZoneCountResponse $
--             getHostedZoneCountResponse
--
--         , testGetReusableDelegationSetResponse $
--             getReusableDelegationSetResponse
--
--         , testDeleteTrafficPolicyInstanceResponse $
--             deleteTrafficPolicyInstanceResponse
--
--         , testUpdateTrafficPolicyInstanceResponse $
--             updateTrafficPolicyInstanceResponse
--
--         , testUpdateHostedZoneCommentResponse $
--             updateHostedZoneCommentResponse
--
--         , testGetHealthCheckStatusResponse $
--             getHealthCheckStatusResponse
--
--         , testCreateTrafficPolicyVersionResponse $
--             createTrafficPolicyVersionResponse
--
--         , testListHealthChecksResponse $
--             listHealthChecksResponse
--
--         , testGetTrafficPolicyResponse $
--             getTrafficPolicyResponse
--
--         , testListTrafficPolicyVersionsResponse $
--             listTrafficPolicyVersionsResponse
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
--         , testCreateTrafficPolicyResponse $
--             createTrafficPolicyResponse
--
--         , testListTrafficPolicyInstancesByHostedZoneResponse $
--             listTrafficPolicyInstancesByHostedZoneResponse
--
--         , testListTrafficPoliciesResponse $
--             listTrafficPoliciesResponse
--
--           ]
--     ]

-- Requests

testAssociateVPCWithHostedZone :: AssociateVPCWithHostedZone -> TestTree
testAssociateVPCWithHostedZone = req
    "AssociateVPCWithHostedZone"
    "fixture/AssociateVPCWithHostedZone.yaml"

testDeleteTrafficPolicy :: DeleteTrafficPolicy -> TestTree
testDeleteTrafficPolicy = req
    "DeleteTrafficPolicy"
    "fixture/DeleteTrafficPolicy.yaml"

testGetCheckerIPRanges :: GetCheckerIPRanges -> TestTree
testGetCheckerIPRanges = req
    "GetCheckerIPRanges"
    "fixture/GetCheckerIPRanges.yaml"

testGetTrafficPolicyInstance :: GetTrafficPolicyInstance -> TestTree
testGetTrafficPolicyInstance = req
    "GetTrafficPolicyInstance"
    "fixture/GetTrafficPolicyInstance.yaml"

testGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
testGetHealthCheckLastFailureReason = req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

testDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
testDeleteReusableDelegationSet = req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

testListHostedZonesByName :: ListHostedZonesByName -> TestTree
testListHostedZonesByName = req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

testListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
testListReusableDelegationSets = req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets.yaml"

testListTagsForResource :: ListTagsForResource -> TestTree
testListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

testListTrafficPolicyInstances :: ListTrafficPolicyInstances -> TestTree
testListTrafficPolicyInstances = req
    "ListTrafficPolicyInstances"
    "fixture/ListTrafficPolicyInstances.yaml"

testCreateTrafficPolicyInstance :: CreateTrafficPolicyInstance -> TestTree
testCreateTrafficPolicyInstance = req
    "CreateTrafficPolicyInstance"
    "fixture/CreateTrafficPolicyInstance.yaml"

testGetChange :: GetChange -> TestTree
testGetChange = req
    "GetChange"
    "fixture/GetChange.yaml"

testChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
testChangeResourceRecordSets = req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

testDeleteHealthCheck :: DeleteHealthCheck -> TestTree
testDeleteHealthCheck = req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck.yaml"

testUpdateHealthCheck :: UpdateHealthCheck -> TestTree
testUpdateHealthCheck = req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck.yaml"

testCreateHostedZone :: CreateHostedZone -> TestTree
testCreateHostedZone = req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

testListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicy -> TestTree
testListTrafficPolicyInstancesByPolicy = req
    "ListTrafficPolicyInstancesByPolicy"
    "fixture/ListTrafficPolicyInstancesByPolicy.yaml"

testDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
testDisassociateVPCFromHostedZone = req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

testCreateHealthCheck :: CreateHealthCheck -> TestTree
testCreateHealthCheck = req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck.yaml"

testChangeTagsForResource :: ChangeTagsForResource -> TestTree
testChangeTagsForResource = req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

testListHostedZones :: ListHostedZones -> TestTree
testListHostedZones = req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

testGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCount -> TestTree
testGetTrafficPolicyInstanceCount = req
    "GetTrafficPolicyInstanceCount"
    "fixture/GetTrafficPolicyInstanceCount.yaml"

testListGeoLocations :: ListGeoLocations -> TestTree
testListGeoLocations = req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

testGetHostedZone :: GetHostedZone -> TestTree
testGetHostedZone = req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

testGetHealthCheck :: GetHealthCheck -> TestTree
testGetHealthCheck = req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

testListResourceRecordSets :: ListResourceRecordSets -> TestTree
testListResourceRecordSets = req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

testCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
testCreateReusableDelegationSet = req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

testGetHealthCheckCount :: GetHealthCheckCount -> TestTree
testGetHealthCheckCount = req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

testUpdateTrafficPolicyComment :: UpdateTrafficPolicyComment -> TestTree
testUpdateTrafficPolicyComment = req
    "UpdateTrafficPolicyComment"
    "fixture/UpdateTrafficPolicyComment.yaml"

testGetHostedZoneCount :: GetHostedZoneCount -> TestTree
testGetHostedZoneCount = req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

testGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
testGetReusableDelegationSet = req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet.yaml"

testDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstance -> TestTree
testDeleteTrafficPolicyInstance = req
    "DeleteTrafficPolicyInstance"
    "fixture/DeleteTrafficPolicyInstance.yaml"

testUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstance -> TestTree
testUpdateTrafficPolicyInstance = req
    "UpdateTrafficPolicyInstance"
    "fixture/UpdateTrafficPolicyInstance.yaml"

testUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
testUpdateHostedZoneComment = req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

testGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
testGetHealthCheckStatus = req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus.yaml"

testCreateTrafficPolicyVersion :: CreateTrafficPolicyVersion -> TestTree
testCreateTrafficPolicyVersion = req
    "CreateTrafficPolicyVersion"
    "fixture/CreateTrafficPolicyVersion.yaml"

testListHealthChecks :: ListHealthChecks -> TestTree
testListHealthChecks = req
    "ListHealthChecks"
    "fixture/ListHealthChecks.yaml"

testGetTrafficPolicy :: GetTrafficPolicy -> TestTree
testGetTrafficPolicy = req
    "GetTrafficPolicy"
    "fixture/GetTrafficPolicy.yaml"

testListTrafficPolicyVersions :: ListTrafficPolicyVersions -> TestTree
testListTrafficPolicyVersions = req
    "ListTrafficPolicyVersions"
    "fixture/ListTrafficPolicyVersions.yaml"

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

testCreateTrafficPolicy :: CreateTrafficPolicy -> TestTree
testCreateTrafficPolicy = req
    "CreateTrafficPolicy"
    "fixture/CreateTrafficPolicy.yaml"

testListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZone -> TestTree
testListTrafficPolicyInstancesByHostedZone = req
    "ListTrafficPolicyInstancesByHostedZone"
    "fixture/ListTrafficPolicyInstancesByHostedZone.yaml"

testListTrafficPolicies :: ListTrafficPolicies -> TestTree
testListTrafficPolicies = req
    "ListTrafficPolicies"
    "fixture/ListTrafficPolicies.yaml"

-- Responses

testAssociateVPCWithHostedZoneResponse :: AssociateVPCWithHostedZoneResponse -> TestTree
testAssociateVPCWithHostedZoneResponse = res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy AssociateVPCWithHostedZone)

testDeleteTrafficPolicyResponse :: DeleteTrafficPolicyResponse -> TestTree
testDeleteTrafficPolicyResponse = res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy DeleteTrafficPolicy)

testGetCheckerIPRangesResponse :: GetCheckerIPRangesResponse -> TestTree
testGetCheckerIPRangesResponse = res
    "GetCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse.proto"
    route53
    (Proxy :: Proxy GetCheckerIPRanges)

testGetTrafficPolicyInstanceResponse :: GetTrafficPolicyInstanceResponse -> TestTree
testGetTrafficPolicyInstanceResponse = res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicyInstance)

testGetHealthCheckLastFailureReasonResponse :: GetHealthCheckLastFailureReasonResponse -> TestTree
testGetHealthCheckLastFailureReasonResponse = res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

testDeleteReusableDelegationSetResponse :: DeleteReusableDelegationSetResponse -> TestTree
testDeleteReusableDelegationSetResponse = res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy DeleteReusableDelegationSet)

testListHostedZonesByNameResponse :: ListHostedZonesByNameResponse -> TestTree
testListHostedZonesByNameResponse = res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZonesByName)

testListReusableDelegationSetsResponse :: ListReusableDelegationSetsResponse -> TestTree
testListReusableDelegationSetsResponse = res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    route53
    (Proxy :: Proxy ListReusableDelegationSets)

testListTagsForResourceResponse :: ListTagsForResourceResponse -> TestTree
testListTagsForResourceResponse = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ListTagsForResource)

testListTrafficPolicyInstancesResponse :: ListTrafficPolicyInstancesResponse -> TestTree
testListTrafficPolicyInstancesResponse = res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstances)

testCreateTrafficPolicyInstanceResponse :: CreateTrafficPolicyInstanceResponse -> TestTree
testCreateTrafficPolicyInstanceResponse = res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicyInstance)

testGetChangeResponse :: GetChangeResponse -> TestTree
testGetChangeResponse = res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    route53
    (Proxy :: Proxy GetChange)

testChangeResourceRecordSetsResponse :: ChangeResourceRecordSetsResponse -> TestTree
testChangeResourceRecordSetsResponse = res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    route53
    (Proxy :: Proxy ChangeResourceRecordSets)

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

testCreateHostedZoneResponse :: CreateHostedZoneResponse -> TestTree
testCreateHostedZoneResponse = res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy CreateHostedZone)

testListTrafficPolicyInstancesByPolicyResponse :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
testListTrafficPolicyInstancesByPolicyResponse = res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstancesByPolicy)

testDisassociateVPCFromHostedZoneResponse :: DisassociateVPCFromHostedZoneResponse -> TestTree
testDisassociateVPCFromHostedZoneResponse = res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

testCreateHealthCheckResponse :: CreateHealthCheckResponse -> TestTree
testCreateHealthCheckResponse = res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy CreateHealthCheck)

testChangeTagsForResourceResponse :: ChangeTagsForResourceResponse -> TestTree
testChangeTagsForResourceResponse = res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ChangeTagsForResource)

testListHostedZonesResponse :: ListHostedZonesResponse -> TestTree
testListHostedZonesResponse = res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZones)

testGetTrafficPolicyInstanceCountResponse :: GetTrafficPolicyInstanceCountResponse -> TestTree
testGetTrafficPolicyInstanceCountResponse = res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicyInstanceCount)

testListGeoLocationsResponse :: ListGeoLocationsResponse -> TestTree
testListGeoLocationsResponse = res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    route53
    (Proxy :: Proxy ListGeoLocations)

testGetHostedZoneResponse :: GetHostedZoneResponse -> TestTree
testGetHostedZoneResponse = res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZone)

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

testCreateReusableDelegationSetResponse :: CreateReusableDelegationSetResponse -> TestTree
testCreateReusableDelegationSetResponse = res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy CreateReusableDelegationSet)

testGetHealthCheckCountResponse :: GetHealthCheckCountResponse -> TestTree
testGetHealthCheckCountResponse = res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckCount)

testUpdateTrafficPolicyCommentResponse :: UpdateTrafficPolicyCommentResponse -> TestTree
testUpdateTrafficPolicyCommentResponse = res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    route53
    (Proxy :: Proxy UpdateTrafficPolicyComment)

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

testDeleteTrafficPolicyInstanceResponse :: DeleteTrafficPolicyInstanceResponse -> TestTree
testDeleteTrafficPolicyInstanceResponse = res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy DeleteTrafficPolicyInstance)

testUpdateTrafficPolicyInstanceResponse :: UpdateTrafficPolicyInstanceResponse -> TestTree
testUpdateTrafficPolicyInstanceResponse = res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy UpdateTrafficPolicyInstance)

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

testCreateTrafficPolicyVersionResponse :: CreateTrafficPolicyVersionResponse -> TestTree
testCreateTrafficPolicyVersionResponse = res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicyVersion)

testListHealthChecksResponse :: ListHealthChecksResponse -> TestTree
testListHealthChecksResponse = res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    route53
    (Proxy :: Proxy ListHealthChecks)

testGetTrafficPolicyResponse :: GetTrafficPolicyResponse -> TestTree
testGetTrafficPolicyResponse = res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicy)

testListTrafficPolicyVersionsResponse :: ListTrafficPolicyVersionsResponse -> TestTree
testListTrafficPolicyVersionsResponse = res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyVersions)

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

testCreateTrafficPolicyResponse :: CreateTrafficPolicyResponse -> TestTree
testCreateTrafficPolicyResponse = res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicy)

testListTrafficPolicyInstancesByHostedZoneResponse :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
testListTrafficPolicyInstancesByHostedZoneResponse = res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstancesByHostedZone)

testListTrafficPoliciesResponse :: ListTrafficPoliciesResponse -> TestTree
testListTrafficPoliciesResponse = res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicies)
