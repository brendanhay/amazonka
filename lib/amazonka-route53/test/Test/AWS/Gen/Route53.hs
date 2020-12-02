{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Route53 where

import Data.Proxy
import Network.AWS.Route53
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.Route53.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetHostedZoneLimit $
--             getHostedZoneLimit
--
--         , requestAssociateVPCWithHostedZone $
--             associateVPCWithHostedZone
--
--         , requestDeleteTrafficPolicy $
--             deleteTrafficPolicy
--
--         , requestGetCheckerIPRanges $
--             getCheckerIPRanges
--
--         , requestGetTrafficPolicyInstance $
--             getTrafficPolicyInstance
--
--         , requestGetHealthCheckLastFailureReason $
--             getHealthCheckLastFailureReason
--
--         , requestDeleteReusableDelegationSet $
--             deleteReusableDelegationSet
--
--         , requestListHostedZonesByName $
--             listHostedZonesByName
--
--         , requestListReusableDelegationSets $
--             listReusableDelegationSets
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestListQueryLoggingConfigs $
--             listQueryLoggingConfigs
--
--         , requestListTrafficPolicyInstances $
--             listTrafficPolicyInstances
--
--         , requestCreateTrafficPolicyInstance $
--             createTrafficPolicyInstance
--
--         , requestGetChange $
--             getChange
--
--         , requestChangeResourceRecordSets $
--             changeResourceRecordSets
--
--         , requestDeleteHealthCheck $
--             deleteHealthCheck
--
--         , requestUpdateHealthCheck $
--             updateHealthCheck
--
--         , requestCreateHostedZone $
--             createHostedZone
--
--         , requestCreateVPCAssociationAuthorization $
--             createVPCAssociationAuthorization
--
--         , requestListVPCAssociationAuthorizations $
--             listVPCAssociationAuthorizations
--
--         , requestListTrafficPolicyInstancesByPolicy $
--             listTrafficPolicyInstancesByPolicy
--
--         , requestDisassociateVPCFromHostedZone $
--             disassociateVPCFromHostedZone
--
--         , requestCreateHealthCheck $
--             createHealthCheck
--
--         , requestDeleteVPCAssociationAuthorization $
--             deleteVPCAssociationAuthorization
--
--         , requestChangeTagsForResource $
--             changeTagsForResource
--
--         , requestListHostedZones $
--             listHostedZones
--
--         , requestGetTrafficPolicyInstanceCount $
--             getTrafficPolicyInstanceCount
--
--         , requestListGeoLocations $
--             listGeoLocations
--
--         , requestGetHostedZone $
--             getHostedZone
--
--         , requestGetHealthCheck $
--             getHealthCheck
--
--         , requestListResourceRecordSets $
--             listResourceRecordSets
--
--         , requestCreateReusableDelegationSet $
--             createReusableDelegationSet
--
--         , requestCreateQueryLoggingConfig $
--             createQueryLoggingConfig
--
--         , requestGetHealthCheckCount $
--             getHealthCheckCount
--
--         , requestUpdateTrafficPolicyComment $
--             updateTrafficPolicyComment
--
--         , requestGetHostedZoneCount $
--             getHostedZoneCount
--
--         , requestGetAccountLimit $
--             getAccountLimit
--
--         , requestDeleteQueryLoggingConfig $
--             deleteQueryLoggingConfig
--
--         , requestGetQueryLoggingConfig $
--             getQueryLoggingConfig
--
--         , requestGetReusableDelegationSet $
--             getReusableDelegationSet
--
--         , requestDeleteTrafficPolicyInstance $
--             deleteTrafficPolicyInstance
--
--         , requestUpdateTrafficPolicyInstance $
--             updateTrafficPolicyInstance
--
--         , requestUpdateHostedZoneComment $
--             updateHostedZoneComment
--
--         , requestGetHealthCheckStatus $
--             getHealthCheckStatus
--
--         , requestGetReusableDelegationSetLimit $
--             getReusableDelegationSetLimit
--
--         , requestCreateTrafficPolicyVersion $
--             createTrafficPolicyVersion
--
--         , requestTestDNSAnswer $
--             testDNSAnswer
--
--         , requestListHealthChecks $
--             listHealthChecks
--
--         , requestGetTrafficPolicy $
--             getTrafficPolicy
--
--         , requestListTrafficPolicyVersions $
--             listTrafficPolicyVersions
--
--         , requestDeleteHostedZone $
--             deleteHostedZone
--
--         , requestGetGeoLocation $
--             getGeoLocation
--
--         , requestListTagsForResources $
--             listTagsForResources
--
--         , requestCreateTrafficPolicy $
--             createTrafficPolicy
--
--         , requestListTrafficPolicyInstancesByHostedZone $
--             listTrafficPolicyInstancesByHostedZone
--
--         , requestListTrafficPolicies $
--             listTrafficPolicies
--
--           ]

--     , testGroup "response"
--         [ responseGetHostedZoneLimit $
--             getHostedZoneLimitResponse
--
--         , responseAssociateVPCWithHostedZone $
--             associateVPCWithHostedZoneResponse
--
--         , responseDeleteTrafficPolicy $
--             deleteTrafficPolicyResponse
--
--         , responseGetCheckerIPRanges $
--             getCheckerIPRangesResponse
--
--         , responseGetTrafficPolicyInstance $
--             getTrafficPolicyInstanceResponse
--
--         , responseGetHealthCheckLastFailureReason $
--             getHealthCheckLastFailureReasonResponse
--
--         , responseDeleteReusableDelegationSet $
--             deleteReusableDelegationSetResponse
--
--         , responseListHostedZonesByName $
--             listHostedZonesByNameResponse
--
--         , responseListReusableDelegationSets $
--             listReusableDelegationSetsResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseListQueryLoggingConfigs $
--             listQueryLoggingConfigsResponse
--
--         , responseListTrafficPolicyInstances $
--             listTrafficPolicyInstancesResponse
--
--         , responseCreateTrafficPolicyInstance $
--             createTrafficPolicyInstanceResponse
--
--         , responseGetChange $
--             getChangeResponse
--
--         , responseChangeResourceRecordSets $
--             changeResourceRecordSetsResponse
--
--         , responseDeleteHealthCheck $
--             deleteHealthCheckResponse
--
--         , responseUpdateHealthCheck $
--             updateHealthCheckResponse
--
--         , responseCreateHostedZone $
--             createHostedZoneResponse
--
--         , responseCreateVPCAssociationAuthorization $
--             createVPCAssociationAuthorizationResponse
--
--         , responseListVPCAssociationAuthorizations $
--             listVPCAssociationAuthorizationsResponse
--
--         , responseListTrafficPolicyInstancesByPolicy $
--             listTrafficPolicyInstancesByPolicyResponse
--
--         , responseDisassociateVPCFromHostedZone $
--             disassociateVPCFromHostedZoneResponse
--
--         , responseCreateHealthCheck $
--             createHealthCheckResponse
--
--         , responseDeleteVPCAssociationAuthorization $
--             deleteVPCAssociationAuthorizationResponse
--
--         , responseChangeTagsForResource $
--             changeTagsForResourceResponse
--
--         , responseListHostedZones $
--             listHostedZonesResponse
--
--         , responseGetTrafficPolicyInstanceCount $
--             getTrafficPolicyInstanceCountResponse
--
--         , responseListGeoLocations $
--             listGeoLocationsResponse
--
--         , responseGetHostedZone $
--             getHostedZoneResponse
--
--         , responseGetHealthCheck $
--             getHealthCheckResponse
--
--         , responseListResourceRecordSets $
--             listResourceRecordSetsResponse
--
--         , responseCreateReusableDelegationSet $
--             createReusableDelegationSetResponse
--
--         , responseCreateQueryLoggingConfig $
--             createQueryLoggingConfigResponse
--
--         , responseGetHealthCheckCount $
--             getHealthCheckCountResponse
--
--         , responseUpdateTrafficPolicyComment $
--             updateTrafficPolicyCommentResponse
--
--         , responseGetHostedZoneCount $
--             getHostedZoneCountResponse
--
--         , responseGetAccountLimit $
--             getAccountLimitResponse
--
--         , responseDeleteQueryLoggingConfig $
--             deleteQueryLoggingConfigResponse
--
--         , responseGetQueryLoggingConfig $
--             getQueryLoggingConfigResponse
--
--         , responseGetReusableDelegationSet $
--             getReusableDelegationSetResponse
--
--         , responseDeleteTrafficPolicyInstance $
--             deleteTrafficPolicyInstanceResponse
--
--         , responseUpdateTrafficPolicyInstance $
--             updateTrafficPolicyInstanceResponse
--
--         , responseUpdateHostedZoneComment $
--             updateHostedZoneCommentResponse
--
--         , responseGetHealthCheckStatus $
--             getHealthCheckStatusResponse
--
--         , responseGetReusableDelegationSetLimit $
--             getReusableDelegationSetLimitResponse
--
--         , responseCreateTrafficPolicyVersion $
--             createTrafficPolicyVersionResponse
--
--         , responseTestDNSAnswer $
--             testDNSAnswerResponse
--
--         , responseListHealthChecks $
--             listHealthChecksResponse
--
--         , responseGetTrafficPolicy $
--             getTrafficPolicyResponse
--
--         , responseListTrafficPolicyVersions $
--             listTrafficPolicyVersionsResponse
--
--         , responseDeleteHostedZone $
--             deleteHostedZoneResponse
--
--         , responseGetGeoLocation $
--             getGeoLocationResponse
--
--         , responseListTagsForResources $
--             listTagsForResourcesResponse
--
--         , responseCreateTrafficPolicy $
--             createTrafficPolicyResponse
--
--         , responseListTrafficPolicyInstancesByHostedZone $
--             listTrafficPolicyInstancesByHostedZoneResponse
--
--         , responseListTrafficPolicies $
--             listTrafficPoliciesResponse
--
--           ]
--     ]

-- Requests

requestGetHostedZoneLimit :: GetHostedZoneLimit -> TestTree
requestGetHostedZoneLimit = req
    "GetHostedZoneLimit"
    "fixture/GetHostedZoneLimit.yaml"

requestAssociateVPCWithHostedZone :: AssociateVPCWithHostedZone -> TestTree
requestAssociateVPCWithHostedZone = req
    "AssociateVPCWithHostedZone"
    "fixture/AssociateVPCWithHostedZone.yaml"

requestDeleteTrafficPolicy :: DeleteTrafficPolicy -> TestTree
requestDeleteTrafficPolicy = req
    "DeleteTrafficPolicy"
    "fixture/DeleteTrafficPolicy.yaml"

requestGetCheckerIPRanges :: GetCheckerIPRanges -> TestTree
requestGetCheckerIPRanges = req
    "GetCheckerIPRanges"
    "fixture/GetCheckerIPRanges.yaml"

requestGetTrafficPolicyInstance :: GetTrafficPolicyInstance -> TestTree
requestGetTrafficPolicyInstance = req
    "GetTrafficPolicyInstance"
    "fixture/GetTrafficPolicyInstance.yaml"

requestGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
requestGetHealthCheckLastFailureReason = req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

requestDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
requestDeleteReusableDelegationSet = req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

requestListHostedZonesByName :: ListHostedZonesByName -> TestTree
requestListHostedZonesByName = req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

requestListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
requestListReusableDelegationSets = req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListQueryLoggingConfigs :: ListQueryLoggingConfigs -> TestTree
requestListQueryLoggingConfigs = req
    "ListQueryLoggingConfigs"
    "fixture/ListQueryLoggingConfigs.yaml"

requestListTrafficPolicyInstances :: ListTrafficPolicyInstances -> TestTree
requestListTrafficPolicyInstances = req
    "ListTrafficPolicyInstances"
    "fixture/ListTrafficPolicyInstances.yaml"

requestCreateTrafficPolicyInstance :: CreateTrafficPolicyInstance -> TestTree
requestCreateTrafficPolicyInstance = req
    "CreateTrafficPolicyInstance"
    "fixture/CreateTrafficPolicyInstance.yaml"

requestGetChange :: GetChange -> TestTree
requestGetChange = req
    "GetChange"
    "fixture/GetChange.yaml"

requestChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
requestChangeResourceRecordSets = req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

requestDeleteHealthCheck :: DeleteHealthCheck -> TestTree
requestDeleteHealthCheck = req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck.yaml"

requestUpdateHealthCheck :: UpdateHealthCheck -> TestTree
requestUpdateHealthCheck = req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck.yaml"

requestCreateHostedZone :: CreateHostedZone -> TestTree
requestCreateHostedZone = req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

requestCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorization -> TestTree
requestCreateVPCAssociationAuthorization = req
    "CreateVPCAssociationAuthorization"
    "fixture/CreateVPCAssociationAuthorization.yaml"

requestListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizations -> TestTree
requestListVPCAssociationAuthorizations = req
    "ListVPCAssociationAuthorizations"
    "fixture/ListVPCAssociationAuthorizations.yaml"

requestListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicy -> TestTree
requestListTrafficPolicyInstancesByPolicy = req
    "ListTrafficPolicyInstancesByPolicy"
    "fixture/ListTrafficPolicyInstancesByPolicy.yaml"

requestDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
requestDisassociateVPCFromHostedZone = req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

requestCreateHealthCheck :: CreateHealthCheck -> TestTree
requestCreateHealthCheck = req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck.yaml"

requestDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorization -> TestTree
requestDeleteVPCAssociationAuthorization = req
    "DeleteVPCAssociationAuthorization"
    "fixture/DeleteVPCAssociationAuthorization.yaml"

requestChangeTagsForResource :: ChangeTagsForResource -> TestTree
requestChangeTagsForResource = req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

requestListHostedZones :: ListHostedZones -> TestTree
requestListHostedZones = req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

requestGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCount -> TestTree
requestGetTrafficPolicyInstanceCount = req
    "GetTrafficPolicyInstanceCount"
    "fixture/GetTrafficPolicyInstanceCount.yaml"

requestListGeoLocations :: ListGeoLocations -> TestTree
requestListGeoLocations = req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

requestGetHostedZone :: GetHostedZone -> TestTree
requestGetHostedZone = req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

requestGetHealthCheck :: GetHealthCheck -> TestTree
requestGetHealthCheck = req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

requestListResourceRecordSets :: ListResourceRecordSets -> TestTree
requestListResourceRecordSets = req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

requestCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
requestCreateReusableDelegationSet = req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

requestCreateQueryLoggingConfig :: CreateQueryLoggingConfig -> TestTree
requestCreateQueryLoggingConfig = req
    "CreateQueryLoggingConfig"
    "fixture/CreateQueryLoggingConfig.yaml"

requestGetHealthCheckCount :: GetHealthCheckCount -> TestTree
requestGetHealthCheckCount = req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

requestUpdateTrafficPolicyComment :: UpdateTrafficPolicyComment -> TestTree
requestUpdateTrafficPolicyComment = req
    "UpdateTrafficPolicyComment"
    "fixture/UpdateTrafficPolicyComment.yaml"

requestGetHostedZoneCount :: GetHostedZoneCount -> TestTree
requestGetHostedZoneCount = req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

requestGetAccountLimit :: GetAccountLimit -> TestTree
requestGetAccountLimit = req
    "GetAccountLimit"
    "fixture/GetAccountLimit.yaml"

requestDeleteQueryLoggingConfig :: DeleteQueryLoggingConfig -> TestTree
requestDeleteQueryLoggingConfig = req
    "DeleteQueryLoggingConfig"
    "fixture/DeleteQueryLoggingConfig.yaml"

requestGetQueryLoggingConfig :: GetQueryLoggingConfig -> TestTree
requestGetQueryLoggingConfig = req
    "GetQueryLoggingConfig"
    "fixture/GetQueryLoggingConfig.yaml"

requestGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
requestGetReusableDelegationSet = req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet.yaml"

requestDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstance -> TestTree
requestDeleteTrafficPolicyInstance = req
    "DeleteTrafficPolicyInstance"
    "fixture/DeleteTrafficPolicyInstance.yaml"

requestUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstance -> TestTree
requestUpdateTrafficPolicyInstance = req
    "UpdateTrafficPolicyInstance"
    "fixture/UpdateTrafficPolicyInstance.yaml"

requestUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
requestUpdateHostedZoneComment = req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

requestGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
requestGetHealthCheckStatus = req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus.yaml"

requestGetReusableDelegationSetLimit :: GetReusableDelegationSetLimit -> TestTree
requestGetReusableDelegationSetLimit = req
    "GetReusableDelegationSetLimit"
    "fixture/GetReusableDelegationSetLimit.yaml"

requestCreateTrafficPolicyVersion :: CreateTrafficPolicyVersion -> TestTree
requestCreateTrafficPolicyVersion = req
    "CreateTrafficPolicyVersion"
    "fixture/CreateTrafficPolicyVersion.yaml"

requestTestDNSAnswer :: TestDNSAnswer -> TestTree
requestTestDNSAnswer = req
    "TestDNSAnswer"
    "fixture/TestDNSAnswer.yaml"

requestListHealthChecks :: ListHealthChecks -> TestTree
requestListHealthChecks = req
    "ListHealthChecks"
    "fixture/ListHealthChecks.yaml"

requestGetTrafficPolicy :: GetTrafficPolicy -> TestTree
requestGetTrafficPolicy = req
    "GetTrafficPolicy"
    "fixture/GetTrafficPolicy.yaml"

requestListTrafficPolicyVersions :: ListTrafficPolicyVersions -> TestTree
requestListTrafficPolicyVersions = req
    "ListTrafficPolicyVersions"
    "fixture/ListTrafficPolicyVersions.yaml"

requestDeleteHostedZone :: DeleteHostedZone -> TestTree
requestDeleteHostedZone = req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone.yaml"

requestGetGeoLocation :: GetGeoLocation -> TestTree
requestGetGeoLocation = req
    "GetGeoLocation"
    "fixture/GetGeoLocation.yaml"

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources = req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

requestCreateTrafficPolicy :: CreateTrafficPolicy -> TestTree
requestCreateTrafficPolicy = req
    "CreateTrafficPolicy"
    "fixture/CreateTrafficPolicy.yaml"

requestListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZone -> TestTree
requestListTrafficPolicyInstancesByHostedZone = req
    "ListTrafficPolicyInstancesByHostedZone"
    "fixture/ListTrafficPolicyInstancesByHostedZone.yaml"

requestListTrafficPolicies :: ListTrafficPolicies -> TestTree
requestListTrafficPolicies = req
    "ListTrafficPolicies"
    "fixture/ListTrafficPolicies.yaml"

-- Responses

responseGetHostedZoneLimit :: GetHostedZoneLimitResponse -> TestTree
responseGetHostedZoneLimit = res
    "GetHostedZoneLimitResponse"
    "fixture/GetHostedZoneLimitResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZoneLimit)

responseAssociateVPCWithHostedZone :: AssociateVPCWithHostedZoneResponse -> TestTree
responseAssociateVPCWithHostedZone = res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy AssociateVPCWithHostedZone)

responseDeleteTrafficPolicy :: DeleteTrafficPolicyResponse -> TestTree
responseDeleteTrafficPolicy = res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy DeleteTrafficPolicy)

responseGetCheckerIPRanges :: GetCheckerIPRangesResponse -> TestTree
responseGetCheckerIPRanges = res
    "GetCheckerIPRangesResponse"
    "fixture/GetCheckerIPRangesResponse.proto"
    route53
    (Proxy :: Proxy GetCheckerIPRanges)

responseGetTrafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TestTree
responseGetTrafficPolicyInstance = res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicyInstance)

responseGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReasonResponse -> TestTree
responseGetHealthCheckLastFailureReason = res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

responseDeleteReusableDelegationSet :: DeleteReusableDelegationSetResponse -> TestTree
responseDeleteReusableDelegationSet = res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy DeleteReusableDelegationSet)

responseListHostedZonesByName :: ListHostedZonesByNameResponse -> TestTree
responseListHostedZonesByName = res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZonesByName)

responseListReusableDelegationSets :: ListReusableDelegationSetsResponse -> TestTree
responseListReusableDelegationSets = res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    route53
    (Proxy :: Proxy ListReusableDelegationSets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ListTagsForResource)

responseListQueryLoggingConfigs :: ListQueryLoggingConfigsResponse -> TestTree
responseListQueryLoggingConfigs = res
    "ListQueryLoggingConfigsResponse"
    "fixture/ListQueryLoggingConfigsResponse.proto"
    route53
    (Proxy :: Proxy ListQueryLoggingConfigs)

responseListTrafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> TestTree
responseListTrafficPolicyInstances = res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstances)

responseCreateTrafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TestTree
responseCreateTrafficPolicyInstance = res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicyInstance)

responseGetChange :: GetChangeResponse -> TestTree
responseGetChange = res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    route53
    (Proxy :: Proxy GetChange)

responseChangeResourceRecordSets :: ChangeResourceRecordSetsResponse -> TestTree
responseChangeResourceRecordSets = res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    route53
    (Proxy :: Proxy ChangeResourceRecordSets)

responseDeleteHealthCheck :: DeleteHealthCheckResponse -> TestTree
responseDeleteHealthCheck = res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy DeleteHealthCheck)

responseUpdateHealthCheck :: UpdateHealthCheckResponse -> TestTree
responseUpdateHealthCheck = res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy UpdateHealthCheck)

responseCreateHostedZone :: CreateHostedZoneResponse -> TestTree
responseCreateHostedZone = res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy CreateHostedZone)

responseCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorizationResponse -> TestTree
responseCreateVPCAssociationAuthorization = res
    "CreateVPCAssociationAuthorizationResponse"
    "fixture/CreateVPCAssociationAuthorizationResponse.proto"
    route53
    (Proxy :: Proxy CreateVPCAssociationAuthorization)

responseListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizationsResponse -> TestTree
responseListVPCAssociationAuthorizations = res
    "ListVPCAssociationAuthorizationsResponse"
    "fixture/ListVPCAssociationAuthorizationsResponse.proto"
    route53
    (Proxy :: Proxy ListVPCAssociationAuthorizations)

responseListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
responseListTrafficPolicyInstancesByPolicy = res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstancesByPolicy)

responseDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZoneResponse -> TestTree
responseDisassociateVPCFromHostedZone = res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

responseCreateHealthCheck :: CreateHealthCheckResponse -> TestTree
responseCreateHealthCheck = res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy CreateHealthCheck)

responseDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorizationResponse -> TestTree
responseDeleteVPCAssociationAuthorization = res
    "DeleteVPCAssociationAuthorizationResponse"
    "fixture/DeleteVPCAssociationAuthorizationResponse.proto"
    route53
    (Proxy :: Proxy DeleteVPCAssociationAuthorization)

responseChangeTagsForResource :: ChangeTagsForResourceResponse -> TestTree
responseChangeTagsForResource = res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    route53
    (Proxy :: Proxy ChangeTagsForResource)

responseListHostedZones :: ListHostedZonesResponse -> TestTree
responseListHostedZones = res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    route53
    (Proxy :: Proxy ListHostedZones)

responseGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> TestTree
responseGetTrafficPolicyInstanceCount = res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicyInstanceCount)

responseListGeoLocations :: ListGeoLocationsResponse -> TestTree
responseListGeoLocations = res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    route53
    (Proxy :: Proxy ListGeoLocations)

responseGetHostedZone :: GetHostedZoneResponse -> TestTree
responseGetHostedZone = res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZone)

responseGetHealthCheck :: GetHealthCheckResponse -> TestTree
responseGetHealthCheck = res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheck)

responseListResourceRecordSets :: ListResourceRecordSetsResponse -> TestTree
responseListResourceRecordSets = res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    route53
    (Proxy :: Proxy ListResourceRecordSets)

responseCreateReusableDelegationSet :: CreateReusableDelegationSetResponse -> TestTree
responseCreateReusableDelegationSet = res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy CreateReusableDelegationSet)

responseCreateQueryLoggingConfig :: CreateQueryLoggingConfigResponse -> TestTree
responseCreateQueryLoggingConfig = res
    "CreateQueryLoggingConfigResponse"
    "fixture/CreateQueryLoggingConfigResponse.proto"
    route53
    (Proxy :: Proxy CreateQueryLoggingConfig)

responseGetHealthCheckCount :: GetHealthCheckCountResponse -> TestTree
responseGetHealthCheckCount = res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckCount)

responseUpdateTrafficPolicyComment :: UpdateTrafficPolicyCommentResponse -> TestTree
responseUpdateTrafficPolicyComment = res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    route53
    (Proxy :: Proxy UpdateTrafficPolicyComment)

responseGetHostedZoneCount :: GetHostedZoneCountResponse -> TestTree
responseGetHostedZoneCount = res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    route53
    (Proxy :: Proxy GetHostedZoneCount)

responseGetAccountLimit :: GetAccountLimitResponse -> TestTree
responseGetAccountLimit = res
    "GetAccountLimitResponse"
    "fixture/GetAccountLimitResponse.proto"
    route53
    (Proxy :: Proxy GetAccountLimit)

responseDeleteQueryLoggingConfig :: DeleteQueryLoggingConfigResponse -> TestTree
responseDeleteQueryLoggingConfig = res
    "DeleteQueryLoggingConfigResponse"
    "fixture/DeleteQueryLoggingConfigResponse.proto"
    route53
    (Proxy :: Proxy DeleteQueryLoggingConfig)

responseGetQueryLoggingConfig :: GetQueryLoggingConfigResponse -> TestTree
responseGetQueryLoggingConfig = res
    "GetQueryLoggingConfigResponse"
    "fixture/GetQueryLoggingConfigResponse.proto"
    route53
    (Proxy :: Proxy GetQueryLoggingConfig)

responseGetReusableDelegationSet :: GetReusableDelegationSetResponse -> TestTree
responseGetReusableDelegationSet = res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse.proto"
    route53
    (Proxy :: Proxy GetReusableDelegationSet)

responseDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstanceResponse -> TestTree
responseDeleteTrafficPolicyInstance = res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy DeleteTrafficPolicyInstance)

responseUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TestTree
responseUpdateTrafficPolicyInstance = res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    route53
    (Proxy :: Proxy UpdateTrafficPolicyInstance)

responseUpdateHostedZoneComment :: UpdateHostedZoneCommentResponse -> TestTree
responseUpdateHostedZoneComment = res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    route53
    (Proxy :: Proxy UpdateHostedZoneComment)

responseGetHealthCheckStatus :: GetHealthCheckStatusResponse -> TestTree
responseGetHealthCheckStatus = res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    route53
    (Proxy :: Proxy GetHealthCheckStatus)

responseGetReusableDelegationSetLimit :: GetReusableDelegationSetLimitResponse -> TestTree
responseGetReusableDelegationSetLimit = res
    "GetReusableDelegationSetLimitResponse"
    "fixture/GetReusableDelegationSetLimitResponse.proto"
    route53
    (Proxy :: Proxy GetReusableDelegationSetLimit)

responseCreateTrafficPolicyVersion :: CreateTrafficPolicyVersionResponse -> TestTree
responseCreateTrafficPolicyVersion = res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicyVersion)

responseTestDNSAnswer :: TestDNSAnswerResponse -> TestTree
responseTestDNSAnswer = res
    "TestDNSAnswerResponse"
    "fixture/TestDNSAnswerResponse.proto"
    route53
    (Proxy :: Proxy TestDNSAnswer)

responseListHealthChecks :: ListHealthChecksResponse -> TestTree
responseListHealthChecks = res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    route53
    (Proxy :: Proxy ListHealthChecks)

responseGetTrafficPolicy :: GetTrafficPolicyResponse -> TestTree
responseGetTrafficPolicy = res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy GetTrafficPolicy)

responseListTrafficPolicyVersions :: ListTrafficPolicyVersionsResponse -> TestTree
responseListTrafficPolicyVersions = res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyVersions)

responseDeleteHostedZone :: DeleteHostedZoneResponse -> TestTree
responseDeleteHostedZone = res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy DeleteHostedZone)

responseGetGeoLocation :: GetGeoLocationResponse -> TestTree
responseGetGeoLocation = res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    route53
    (Proxy :: Proxy GetGeoLocation)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources = res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    route53
    (Proxy :: Proxy ListTagsForResources)

responseCreateTrafficPolicy :: CreateTrafficPolicyResponse -> TestTree
responseCreateTrafficPolicy = res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    route53
    (Proxy :: Proxy CreateTrafficPolicy)

responseListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
responseListTrafficPolicyInstancesByHostedZone = res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicyInstancesByHostedZone)

responseListTrafficPolicies :: ListTrafficPoliciesResponse -> TestTree
responseListTrafficPolicies = res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    route53
    (Proxy :: Proxy ListTrafficPolicies)
