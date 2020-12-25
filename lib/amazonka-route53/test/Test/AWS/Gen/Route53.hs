{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkGetHostedZoneLimit
--
--         , requestAssociateVPCWithHostedZone $
--             mkAssociateVPCWithHostedZone
--
--         , requestDeleteTrafficPolicy $
--             mkDeleteTrafficPolicy
--
--         , requestGetCheckerIpRanges $
--             mkGetCheckerIpRanges
--
--         , requestGetTrafficPolicyInstance $
--             mkGetTrafficPolicyInstance
--
--         , requestGetHealthCheckLastFailureReason $
--             mkGetHealthCheckLastFailureReason
--
--         , requestDeleteReusableDelegationSet $
--             mkDeleteReusableDelegationSet
--
--         , requestListHostedZonesByName $
--             mkListHostedZonesByName
--
--         , requestListReusableDelegationSets $
--             mkListReusableDelegationSets
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestListQueryLoggingConfigs $
--             mkListQueryLoggingConfigs
--
--         , requestListTrafficPolicyInstances $
--             mkListTrafficPolicyInstances
--
--         , requestCreateTrafficPolicyInstance $
--             mkCreateTrafficPolicyInstance
--
--         , requestGetChange $
--             mkGetChange
--
--         , requestChangeResourceRecordSets $
--             mkChangeResourceRecordSets
--
--         , requestDeleteHealthCheck $
--             mkDeleteHealthCheck
--
--         , requestUpdateHealthCheck $
--             mkUpdateHealthCheck
--
--         , requestCreateHostedZone $
--             mkCreateHostedZone
--
--         , requestCreateVPCAssociationAuthorization $
--             mkCreateVPCAssociationAuthorization
--
--         , requestListVPCAssociationAuthorizations $
--             mkListVPCAssociationAuthorizations
--
--         , requestListTrafficPolicyInstancesByPolicy $
--             mkListTrafficPolicyInstancesByPolicy
--
--         , requestDisassociateVPCFromHostedZone $
--             mkDisassociateVPCFromHostedZone
--
--         , requestCreateHealthCheck $
--             mkCreateHealthCheck
--
--         , requestDeleteVPCAssociationAuthorization $
--             mkDeleteVPCAssociationAuthorization
--
--         , requestChangeTagsForResource $
--             mkChangeTagsForResource
--
--         , requestListHostedZones $
--             mkListHostedZones
--
--         , requestGetTrafficPolicyInstanceCount $
--             mkGetTrafficPolicyInstanceCount
--
--         , requestListGeoLocations $
--             mkListGeoLocations
--
--         , requestGetHostedZone $
--             mkGetHostedZone
--
--         , requestGetHealthCheck $
--             mkGetHealthCheck
--
--         , requestListResourceRecordSets $
--             mkListResourceRecordSets
--
--         , requestCreateReusableDelegationSet $
--             mkCreateReusableDelegationSet
--
--         , requestCreateQueryLoggingConfig $
--             mkCreateQueryLoggingConfig
--
--         , requestGetHealthCheckCount $
--             mkGetHealthCheckCount
--
--         , requestUpdateTrafficPolicyComment $
--             mkUpdateTrafficPolicyComment
--
--         , requestGetHostedZoneCount $
--             mkGetHostedZoneCount
--
--         , requestGetAccountLimit $
--             mkGetAccountLimit
--
--         , requestDeleteQueryLoggingConfig $
--             mkDeleteQueryLoggingConfig
--
--         , requestGetQueryLoggingConfig $
--             mkGetQueryLoggingConfig
--
--         , requestGetReusableDelegationSet $
--             mkGetReusableDelegationSet
--
--         , requestDeleteTrafficPolicyInstance $
--             mkDeleteTrafficPolicyInstance
--
--         , requestUpdateTrafficPolicyInstance $
--             mkUpdateTrafficPolicyInstance
--
--         , requestUpdateHostedZoneComment $
--             mkUpdateHostedZoneComment
--
--         , requestGetHealthCheckStatus $
--             mkGetHealthCheckStatus
--
--         , requestListHostedZonesByVPC $
--             mkListHostedZonesByVPC
--
--         , requestGetReusableDelegationSetLimit $
--             mkGetReusableDelegationSetLimit
--
--         , requestCreateTrafficPolicyVersion $
--             mkCreateTrafficPolicyVersion
--
--         , requestTestDNSAnswer $
--             mkTestDNSAnswer
--
--         , requestListHealthChecks $
--             mkListHealthChecks
--
--         , requestGetTrafficPolicy $
--             mkGetTrafficPolicy
--
--         , requestListTrafficPolicyVersions $
--             mkListTrafficPolicyVersions
--
--         , requestDeleteHostedZone $
--             mkDeleteHostedZone
--
--         , requestGetGeoLocation $
--             mkGetGeoLocation
--
--         , requestListTagsForResources $
--             mkListTagsForResources
--
--         , requestCreateTrafficPolicy $
--             mkCreateTrafficPolicy
--
--         , requestListTrafficPolicyInstancesByHostedZone $
--             mkListTrafficPolicyInstancesByHostedZone
--
--         , requestListTrafficPolicies $
--             mkListTrafficPolicies
--
--           ]

--     , testGroup "response"
--         [ responseGetHostedZoneLimit $
--             mkGetHostedZoneLimitResponse
--
--         , responseAssociateVPCWithHostedZone $
--             mkAssociateVPCWithHostedZoneResponse
--
--         , responseDeleteTrafficPolicy $
--             mkDeleteTrafficPolicyResponse
--
--         , responseGetCheckerIpRanges $
--             mkGetCheckerIpRangesResponse
--
--         , responseGetTrafficPolicyInstance $
--             mkGetTrafficPolicyInstanceResponse
--
--         , responseGetHealthCheckLastFailureReason $
--             mkGetHealthCheckLastFailureReasonResponse
--
--         , responseDeleteReusableDelegationSet $
--             mkDeleteReusableDelegationSetResponse
--
--         , responseListHostedZonesByName $
--             mkListHostedZonesByNameResponse
--
--         , responseListReusableDelegationSets $
--             mkListReusableDelegationSetsResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseListQueryLoggingConfigs $
--             mkListQueryLoggingConfigsResponse
--
--         , responseListTrafficPolicyInstances $
--             mkListTrafficPolicyInstancesResponse
--
--         , responseCreateTrafficPolicyInstance $
--             mkCreateTrafficPolicyInstanceResponse
--
--         , responseGetChange $
--             mkGetChangeResponse
--
--         , responseChangeResourceRecordSets $
--             mkChangeResourceRecordSetsResponse
--
--         , responseDeleteHealthCheck $
--             mkDeleteHealthCheckResponse
--
--         , responseUpdateHealthCheck $
--             mkUpdateHealthCheckResponse
--
--         , responseCreateHostedZone $
--             mkCreateHostedZoneResponse
--
--         , responseCreateVPCAssociationAuthorization $
--             mkCreateVPCAssociationAuthorizationResponse
--
--         , responseListVPCAssociationAuthorizations $
--             mkListVPCAssociationAuthorizationsResponse
--
--         , responseListTrafficPolicyInstancesByPolicy $
--             mkListTrafficPolicyInstancesByPolicyResponse
--
--         , responseDisassociateVPCFromHostedZone $
--             mkDisassociateVPCFromHostedZoneResponse
--
--         , responseCreateHealthCheck $
--             mkCreateHealthCheckResponse
--
--         , responseDeleteVPCAssociationAuthorization $
--             mkDeleteVPCAssociationAuthorizationResponse
--
--         , responseChangeTagsForResource $
--             mkChangeTagsForResourceResponse
--
--         , responseListHostedZones $
--             mkListHostedZonesResponse
--
--         , responseGetTrafficPolicyInstanceCount $
--             mkGetTrafficPolicyInstanceCountResponse
--
--         , responseListGeoLocations $
--             mkListGeoLocationsResponse
--
--         , responseGetHostedZone $
--             mkGetHostedZoneResponse
--
--         , responseGetHealthCheck $
--             mkGetHealthCheckResponse
--
--         , responseListResourceRecordSets $
--             mkListResourceRecordSetsResponse
--
--         , responseCreateReusableDelegationSet $
--             mkCreateReusableDelegationSetResponse
--
--         , responseCreateQueryLoggingConfig $
--             mkCreateQueryLoggingConfigResponse
--
--         , responseGetHealthCheckCount $
--             mkGetHealthCheckCountResponse
--
--         , responseUpdateTrafficPolicyComment $
--             mkUpdateTrafficPolicyCommentResponse
--
--         , responseGetHostedZoneCount $
--             mkGetHostedZoneCountResponse
--
--         , responseGetAccountLimit $
--             mkGetAccountLimitResponse
--
--         , responseDeleteQueryLoggingConfig $
--             mkDeleteQueryLoggingConfigResponse
--
--         , responseGetQueryLoggingConfig $
--             mkGetQueryLoggingConfigResponse
--
--         , responseGetReusableDelegationSet $
--             mkGetReusableDelegationSetResponse
--
--         , responseDeleteTrafficPolicyInstance $
--             mkDeleteTrafficPolicyInstanceResponse
--
--         , responseUpdateTrafficPolicyInstance $
--             mkUpdateTrafficPolicyInstanceResponse
--
--         , responseUpdateHostedZoneComment $
--             mkUpdateHostedZoneCommentResponse
--
--         , responseGetHealthCheckStatus $
--             mkGetHealthCheckStatusResponse
--
--         , responseListHostedZonesByVPC $
--             mkListHostedZonesByVPCResponse
--
--         , responseGetReusableDelegationSetLimit $
--             mkGetReusableDelegationSetLimitResponse
--
--         , responseCreateTrafficPolicyVersion $
--             mkCreateTrafficPolicyVersionResponse
--
--         , responseTestDNSAnswer $
--             mkTestDNSAnswerResponse
--
--         , responseListHealthChecks $
--             mkListHealthChecksResponse
--
--         , responseGetTrafficPolicy $
--             mkGetTrafficPolicyResponse
--
--         , responseListTrafficPolicyVersions $
--             mkListTrafficPolicyVersionsResponse
--
--         , responseDeleteHostedZone $
--             mkDeleteHostedZoneResponse
--
--         , responseGetGeoLocation $
--             mkGetGeoLocationResponse
--
--         , responseListTagsForResources $
--             mkListTagsForResourcesResponse
--
--         , responseCreateTrafficPolicy $
--             mkCreateTrafficPolicyResponse
--
--         , responseListTrafficPolicyInstancesByHostedZone $
--             mkListTrafficPolicyInstancesByHostedZoneResponse
--
--         , responseListTrafficPolicies $
--             mkListTrafficPoliciesResponse
--
--           ]
--     ]

-- Requests

requestGetHostedZoneLimit :: GetHostedZoneLimit -> TestTree
requestGetHostedZoneLimit =
  req
    "GetHostedZoneLimit"
    "fixture/GetHostedZoneLimit.yaml"

requestAssociateVPCWithHostedZone :: AssociateVPCWithHostedZone -> TestTree
requestAssociateVPCWithHostedZone =
  req
    "AssociateVPCWithHostedZone"
    "fixture/AssociateVPCWithHostedZone.yaml"

requestDeleteTrafficPolicy :: DeleteTrafficPolicy -> TestTree
requestDeleteTrafficPolicy =
  req
    "DeleteTrafficPolicy"
    "fixture/DeleteTrafficPolicy.yaml"

requestGetCheckerIpRanges :: GetCheckerIpRanges -> TestTree
requestGetCheckerIpRanges =
  req
    "GetCheckerIpRanges"
    "fixture/GetCheckerIpRanges.yaml"

requestGetTrafficPolicyInstance :: GetTrafficPolicyInstance -> TestTree
requestGetTrafficPolicyInstance =
  req
    "GetTrafficPolicyInstance"
    "fixture/GetTrafficPolicyInstance.yaml"

requestGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
requestGetHealthCheckLastFailureReason =
  req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

requestDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
requestDeleteReusableDelegationSet =
  req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

requestListHostedZonesByName :: ListHostedZonesByName -> TestTree
requestListHostedZonesByName =
  req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

requestListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
requestListReusableDelegationSets =
  req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListQueryLoggingConfigs :: ListQueryLoggingConfigs -> TestTree
requestListQueryLoggingConfigs =
  req
    "ListQueryLoggingConfigs"
    "fixture/ListQueryLoggingConfigs.yaml"

requestListTrafficPolicyInstances :: ListTrafficPolicyInstances -> TestTree
requestListTrafficPolicyInstances =
  req
    "ListTrafficPolicyInstances"
    "fixture/ListTrafficPolicyInstances.yaml"

requestCreateTrafficPolicyInstance :: CreateTrafficPolicyInstance -> TestTree
requestCreateTrafficPolicyInstance =
  req
    "CreateTrafficPolicyInstance"
    "fixture/CreateTrafficPolicyInstance.yaml"

requestGetChange :: GetChange -> TestTree
requestGetChange =
  req
    "GetChange"
    "fixture/GetChange.yaml"

requestChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
requestChangeResourceRecordSets =
  req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

requestDeleteHealthCheck :: DeleteHealthCheck -> TestTree
requestDeleteHealthCheck =
  req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck.yaml"

requestUpdateHealthCheck :: UpdateHealthCheck -> TestTree
requestUpdateHealthCheck =
  req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck.yaml"

requestCreateHostedZone :: CreateHostedZone -> TestTree
requestCreateHostedZone =
  req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

requestCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorization -> TestTree
requestCreateVPCAssociationAuthorization =
  req
    "CreateVPCAssociationAuthorization"
    "fixture/CreateVPCAssociationAuthorization.yaml"

requestListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizations -> TestTree
requestListVPCAssociationAuthorizations =
  req
    "ListVPCAssociationAuthorizations"
    "fixture/ListVPCAssociationAuthorizations.yaml"

requestListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicy -> TestTree
requestListTrafficPolicyInstancesByPolicy =
  req
    "ListTrafficPolicyInstancesByPolicy"
    "fixture/ListTrafficPolicyInstancesByPolicy.yaml"

requestDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
requestDisassociateVPCFromHostedZone =
  req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

requestCreateHealthCheck :: CreateHealthCheck -> TestTree
requestCreateHealthCheck =
  req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck.yaml"

requestDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorization -> TestTree
requestDeleteVPCAssociationAuthorization =
  req
    "DeleteVPCAssociationAuthorization"
    "fixture/DeleteVPCAssociationAuthorization.yaml"

requestChangeTagsForResource :: ChangeTagsForResource -> TestTree
requestChangeTagsForResource =
  req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

requestListHostedZones :: ListHostedZones -> TestTree
requestListHostedZones =
  req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

requestGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCount -> TestTree
requestGetTrafficPolicyInstanceCount =
  req
    "GetTrafficPolicyInstanceCount"
    "fixture/GetTrafficPolicyInstanceCount.yaml"

requestListGeoLocations :: ListGeoLocations -> TestTree
requestListGeoLocations =
  req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

requestGetHostedZone :: GetHostedZone -> TestTree
requestGetHostedZone =
  req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

requestGetHealthCheck :: GetHealthCheck -> TestTree
requestGetHealthCheck =
  req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

requestListResourceRecordSets :: ListResourceRecordSets -> TestTree
requestListResourceRecordSets =
  req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

requestCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
requestCreateReusableDelegationSet =
  req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

requestCreateQueryLoggingConfig :: CreateQueryLoggingConfig -> TestTree
requestCreateQueryLoggingConfig =
  req
    "CreateQueryLoggingConfig"
    "fixture/CreateQueryLoggingConfig.yaml"

requestGetHealthCheckCount :: GetHealthCheckCount -> TestTree
requestGetHealthCheckCount =
  req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

requestUpdateTrafficPolicyComment :: UpdateTrafficPolicyComment -> TestTree
requestUpdateTrafficPolicyComment =
  req
    "UpdateTrafficPolicyComment"
    "fixture/UpdateTrafficPolicyComment.yaml"

requestGetHostedZoneCount :: GetHostedZoneCount -> TestTree
requestGetHostedZoneCount =
  req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

requestGetAccountLimit :: GetAccountLimit -> TestTree
requestGetAccountLimit =
  req
    "GetAccountLimit"
    "fixture/GetAccountLimit.yaml"

requestDeleteQueryLoggingConfig :: DeleteQueryLoggingConfig -> TestTree
requestDeleteQueryLoggingConfig =
  req
    "DeleteQueryLoggingConfig"
    "fixture/DeleteQueryLoggingConfig.yaml"

requestGetQueryLoggingConfig :: GetQueryLoggingConfig -> TestTree
requestGetQueryLoggingConfig =
  req
    "GetQueryLoggingConfig"
    "fixture/GetQueryLoggingConfig.yaml"

requestGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
requestGetReusableDelegationSet =
  req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet.yaml"

requestDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstance -> TestTree
requestDeleteTrafficPolicyInstance =
  req
    "DeleteTrafficPolicyInstance"
    "fixture/DeleteTrafficPolicyInstance.yaml"

requestUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstance -> TestTree
requestUpdateTrafficPolicyInstance =
  req
    "UpdateTrafficPolicyInstance"
    "fixture/UpdateTrafficPolicyInstance.yaml"

requestUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
requestUpdateHostedZoneComment =
  req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

requestGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
requestGetHealthCheckStatus =
  req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus.yaml"

requestListHostedZonesByVPC :: ListHostedZonesByVPC -> TestTree
requestListHostedZonesByVPC =
  req
    "ListHostedZonesByVPC"
    "fixture/ListHostedZonesByVPC.yaml"

requestGetReusableDelegationSetLimit :: GetReusableDelegationSetLimit -> TestTree
requestGetReusableDelegationSetLimit =
  req
    "GetReusableDelegationSetLimit"
    "fixture/GetReusableDelegationSetLimit.yaml"

requestCreateTrafficPolicyVersion :: CreateTrafficPolicyVersion -> TestTree
requestCreateTrafficPolicyVersion =
  req
    "CreateTrafficPolicyVersion"
    "fixture/CreateTrafficPolicyVersion.yaml"

requestTestDNSAnswer :: TestDNSAnswer -> TestTree
requestTestDNSAnswer =
  req
    "TestDNSAnswer"
    "fixture/TestDNSAnswer.yaml"

requestListHealthChecks :: ListHealthChecks -> TestTree
requestListHealthChecks =
  req
    "ListHealthChecks"
    "fixture/ListHealthChecks.yaml"

requestGetTrafficPolicy :: GetTrafficPolicy -> TestTree
requestGetTrafficPolicy =
  req
    "GetTrafficPolicy"
    "fixture/GetTrafficPolicy.yaml"

requestListTrafficPolicyVersions :: ListTrafficPolicyVersions -> TestTree
requestListTrafficPolicyVersions =
  req
    "ListTrafficPolicyVersions"
    "fixture/ListTrafficPolicyVersions.yaml"

requestDeleteHostedZone :: DeleteHostedZone -> TestTree
requestDeleteHostedZone =
  req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone.yaml"

requestGetGeoLocation :: GetGeoLocation -> TestTree
requestGetGeoLocation =
  req
    "GetGeoLocation"
    "fixture/GetGeoLocation.yaml"

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources =
  req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

requestCreateTrafficPolicy :: CreateTrafficPolicy -> TestTree
requestCreateTrafficPolicy =
  req
    "CreateTrafficPolicy"
    "fixture/CreateTrafficPolicy.yaml"

requestListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZone -> TestTree
requestListTrafficPolicyInstancesByHostedZone =
  req
    "ListTrafficPolicyInstancesByHostedZone"
    "fixture/ListTrafficPolicyInstancesByHostedZone.yaml"

requestListTrafficPolicies :: ListTrafficPolicies -> TestTree
requestListTrafficPolicies =
  req
    "ListTrafficPolicies"
    "fixture/ListTrafficPolicies.yaml"

-- Responses

responseGetHostedZoneLimit :: GetHostedZoneLimitResponse -> TestTree
responseGetHostedZoneLimit =
  res
    "GetHostedZoneLimitResponse"
    "fixture/GetHostedZoneLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHostedZoneLimit)

responseAssociateVPCWithHostedZone :: AssociateVPCWithHostedZoneResponse -> TestTree
responseAssociateVPCWithHostedZone =
  res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AssociateVPCWithHostedZone)

responseDeleteTrafficPolicy :: DeleteTrafficPolicyResponse -> TestTree
responseDeleteTrafficPolicy =
  res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficPolicy)

responseGetCheckerIpRanges :: GetCheckerIpRangesResponse -> TestTree
responseGetCheckerIpRanges =
  res
    "GetCheckerIpRangesResponse"
    "fixture/GetCheckerIpRangesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCheckerIpRanges)

responseGetTrafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TestTree
responseGetTrafficPolicyInstance =
  res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTrafficPolicyInstance)

responseGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReasonResponse -> TestTree
responseGetHealthCheckLastFailureReason =
  res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

responseDeleteReusableDelegationSet :: DeleteReusableDelegationSetResponse -> TestTree
responseDeleteReusableDelegationSet =
  res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteReusableDelegationSet)

responseListHostedZonesByName :: ListHostedZonesByNameResponse -> TestTree
responseListHostedZonesByName =
  res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHostedZonesByName)

responseListReusableDelegationSets :: ListReusableDelegationSetsResponse -> TestTree
responseListReusableDelegationSets =
  res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListReusableDelegationSets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseListQueryLoggingConfigs :: ListQueryLoggingConfigsResponse -> TestTree
responseListQueryLoggingConfigs =
  res
    "ListQueryLoggingConfigsResponse"
    "fixture/ListQueryLoggingConfigsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListQueryLoggingConfigs)

responseListTrafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> TestTree
responseListTrafficPolicyInstances =
  res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTrafficPolicyInstances)

responseCreateTrafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TestTree
responseCreateTrafficPolicyInstance =
  res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficPolicyInstance)

responseGetChange :: GetChangeResponse -> TestTree
responseGetChange =
  res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetChange)

responseChangeResourceRecordSets :: ChangeResourceRecordSetsResponse -> TestTree
responseChangeResourceRecordSets =
  res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ChangeResourceRecordSets)

responseDeleteHealthCheck :: DeleteHealthCheckResponse -> TestTree
responseDeleteHealthCheck =
  res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteHealthCheck)

responseUpdateHealthCheck :: UpdateHealthCheckResponse -> TestTree
responseUpdateHealthCheck =
  res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateHealthCheck)

responseCreateHostedZone :: CreateHostedZoneResponse -> TestTree
responseCreateHostedZone =
  res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHostedZone)

responseCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorizationResponse -> TestTree
responseCreateVPCAssociationAuthorization =
  res
    "CreateVPCAssociationAuthorizationResponse"
    "fixture/CreateVPCAssociationAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateVPCAssociationAuthorization)

responseListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizationsResponse -> TestTree
responseListVPCAssociationAuthorizations =
  res
    "ListVPCAssociationAuthorizationsResponse"
    "fixture/ListVPCAssociationAuthorizationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListVPCAssociationAuthorizations)

responseListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
responseListTrafficPolicyInstancesByPolicy =
  res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTrafficPolicyInstancesByPolicy)

responseDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZoneResponse -> TestTree
responseDisassociateVPCFromHostedZone =
  res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

responseCreateHealthCheck :: CreateHealthCheckResponse -> TestTree
responseCreateHealthCheck =
  res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateHealthCheck)

responseDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorizationResponse -> TestTree
responseDeleteVPCAssociationAuthorization =
  res
    "DeleteVPCAssociationAuthorizationResponse"
    "fixture/DeleteVPCAssociationAuthorizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteVPCAssociationAuthorization)

responseChangeTagsForResource :: ChangeTagsForResourceResponse -> TestTree
responseChangeTagsForResource =
  res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ChangeTagsForResource)

responseListHostedZones :: ListHostedZonesResponse -> TestTree
responseListHostedZones =
  res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHostedZones)

responseGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> TestTree
responseGetTrafficPolicyInstanceCount =
  res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTrafficPolicyInstanceCount)

responseListGeoLocations :: ListGeoLocationsResponse -> TestTree
responseListGeoLocations =
  res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListGeoLocations)

responseGetHostedZone :: GetHostedZoneResponse -> TestTree
responseGetHostedZone =
  res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHostedZone)

responseGetHealthCheck :: GetHealthCheckResponse -> TestTree
responseGetHealthCheck =
  res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHealthCheck)

responseListResourceRecordSets :: ListResourceRecordSetsResponse -> TestTree
responseListResourceRecordSets =
  res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListResourceRecordSets)

responseCreateReusableDelegationSet :: CreateReusableDelegationSetResponse -> TestTree
responseCreateReusableDelegationSet =
  res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateReusableDelegationSet)

responseCreateQueryLoggingConfig :: CreateQueryLoggingConfigResponse -> TestTree
responseCreateQueryLoggingConfig =
  res
    "CreateQueryLoggingConfigResponse"
    "fixture/CreateQueryLoggingConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateQueryLoggingConfig)

responseGetHealthCheckCount :: GetHealthCheckCountResponse -> TestTree
responseGetHealthCheckCount =
  res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHealthCheckCount)

responseUpdateTrafficPolicyComment :: UpdateTrafficPolicyCommentResponse -> TestTree
responseUpdateTrafficPolicyComment =
  res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTrafficPolicyComment)

responseGetHostedZoneCount :: GetHostedZoneCountResponse -> TestTree
responseGetHostedZoneCount =
  res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHostedZoneCount)

responseGetAccountLimit :: GetAccountLimitResponse -> TestTree
responseGetAccountLimit =
  res
    "GetAccountLimitResponse"
    "fixture/GetAccountLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAccountLimit)

responseDeleteQueryLoggingConfig :: DeleteQueryLoggingConfigResponse -> TestTree
responseDeleteQueryLoggingConfig =
  res
    "DeleteQueryLoggingConfigResponse"
    "fixture/DeleteQueryLoggingConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteQueryLoggingConfig)

responseGetQueryLoggingConfig :: GetQueryLoggingConfigResponse -> TestTree
responseGetQueryLoggingConfig =
  res
    "GetQueryLoggingConfigResponse"
    "fixture/GetQueryLoggingConfigResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetQueryLoggingConfig)

responseGetReusableDelegationSet :: GetReusableDelegationSetResponse -> TestTree
responseGetReusableDelegationSet =
  res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReusableDelegationSet)

responseDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstanceResponse -> TestTree
responseDeleteTrafficPolicyInstance =
  res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTrafficPolicyInstance)

responseUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TestTree
responseUpdateTrafficPolicyInstance =
  res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateTrafficPolicyInstance)

responseUpdateHostedZoneComment :: UpdateHostedZoneCommentResponse -> TestTree
responseUpdateHostedZoneComment =
  res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateHostedZoneComment)

responseGetHealthCheckStatus :: GetHealthCheckStatusResponse -> TestTree
responseGetHealthCheckStatus =
  res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetHealthCheckStatus)

responseListHostedZonesByVPC :: ListHostedZonesByVPCResponse -> TestTree
responseListHostedZonesByVPC =
  res
    "ListHostedZonesByVPCResponse"
    "fixture/ListHostedZonesByVPCResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHostedZonesByVPC)

responseGetReusableDelegationSetLimit :: GetReusableDelegationSetLimitResponse -> TestTree
responseGetReusableDelegationSetLimit =
  res
    "GetReusableDelegationSetLimitResponse"
    "fixture/GetReusableDelegationSetLimitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetReusableDelegationSetLimit)

responseCreateTrafficPolicyVersion :: CreateTrafficPolicyVersionResponse -> TestTree
responseCreateTrafficPolicyVersion =
  res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficPolicyVersion)

responseTestDNSAnswer :: TestDNSAnswerResponse -> TestTree
responseTestDNSAnswer =
  res
    "TestDNSAnswerResponse"
    "fixture/TestDNSAnswerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestDNSAnswer)

responseListHealthChecks :: ListHealthChecksResponse -> TestTree
responseListHealthChecks =
  res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHealthChecks)

responseGetTrafficPolicy :: GetTrafficPolicyResponse -> TestTree
responseGetTrafficPolicy =
  res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTrafficPolicy)

responseListTrafficPolicyVersions :: ListTrafficPolicyVersionsResponse -> TestTree
responseListTrafficPolicyVersions =
  res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTrafficPolicyVersions)

responseDeleteHostedZone :: DeleteHostedZoneResponse -> TestTree
responseDeleteHostedZone =
  res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteHostedZone)

responseGetGeoLocation :: GetGeoLocationResponse -> TestTree
responseGetGeoLocation =
  res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetGeoLocation)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResources)

responseCreateTrafficPolicy :: CreateTrafficPolicyResponse -> TestTree
responseCreateTrafficPolicy =
  res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTrafficPolicy)

responseListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
responseListTrafficPolicyInstancesByHostedZone =
  res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTrafficPolicyInstancesByHostedZone)

responseListTrafficPolicies :: ListTrafficPoliciesResponse -> TestTree
responseListTrafficPolicies =
  res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTrafficPolicies)
