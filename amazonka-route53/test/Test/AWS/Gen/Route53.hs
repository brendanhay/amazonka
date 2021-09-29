{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Route53
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestCreateReusableDelegationSet $
--             newCreateReusableDelegationSet
--
--         , requestGetHealthCheckCount $
--             newGetHealthCheckCount
--
--         , requestGetHostedZoneLimit $
--             newGetHostedZoneLimit
--
--         , requestAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZone
--
--         , requestCreateTrafficPolicy $
--             newCreateTrafficPolicy
--
--         , requestListGeoLocations $
--             newListGeoLocations
--
--         , requestListTrafficPolicies $
--             newListTrafficPolicies
--
--         , requestGetGeoLocation $
--             newGetGeoLocation
--
--         , requestChangeTagsForResource $
--             newChangeTagsForResource
--
--         , requestDeleteHostedZone $
--             newDeleteHostedZone
--
--         , requestCreateHealthCheck $
--             newCreateHealthCheck
--
--         , requestDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorization
--
--         , requestDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZone
--
--         , requestListHostedZones $
--             newListHostedZones
--
--         , requestCreateHostedZone $
--             newCreateHostedZone
--
--         , requestDeactivateKeySigningKey $
--             newDeactivateKeySigningKey
--
--         , requestTestDNSAnswer $
--             newTestDNSAnswer
--
--         , requestChangeResourceRecordSets $
--             newChangeResourceRecordSets
--
--         , requestGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimit
--
--         , requestGetReusableDelegationSet $
--             newGetReusableDelegationSet
--
--         , requestGetAccountLimit $
--             newGetAccountLimit
--
--         , requestGetDNSSEC $
--             newGetDNSSEC
--
--         , requestGetCheckerIpRanges $
--             newGetCheckerIpRanges
--
--         , requestListReusableDelegationSets $
--             newListReusableDelegationSets
--
--         , requestDeleteKeySigningKey $
--             newDeleteKeySigningKey
--
--         , requestGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstance
--
--         , requestCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfig
--
--         , requestDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSEC
--
--         , requestCreateKeySigningKey $
--             newCreateKeySigningKey
--
--         , requestUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyComment
--
--         , requestListResourceRecordSets $
--             newListResourceRecordSets
--
--         , requestGetHealthCheck $
--             newGetHealthCheck
--
--         , requestDeleteTrafficPolicy $
--             newDeleteTrafficPolicy
--
--         , requestGetHostedZone $
--             newGetHostedZone
--
--         , requestGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCount
--
--         , requestListTagsForResources $
--             newListTagsForResources
--
--         , requestListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZone
--
--         , requestListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizations
--
--         , requestListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicy
--
--         , requestListTrafficPolicyVersions $
--             newListTrafficPolicyVersions
--
--         , requestCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorization
--
--         , requestCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersion
--
--         , requestListHealthChecks $
--             newListHealthChecks
--
--         , requestGetTrafficPolicy $
--             newGetTrafficPolicy
--
--         , requestDeleteHealthCheck $
--             newDeleteHealthCheck
--
--         , requestUpdateHealthCheck $
--             newUpdateHealthCheck
--
--         , requestCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstance
--
--         , requestGetHealthCheckStatus $
--             newGetHealthCheckStatus
--
--         , requestListHostedZonesByVPC $
--             newListHostedZonesByVPC
--
--         , requestGetChange $
--             newGetChange
--
--         , requestUpdateHostedZoneComment $
--             newUpdateHostedZoneComment
--
--         , requestListTrafficPolicyInstances $
--             newListTrafficPolicyInstances
--
--         , requestDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstance
--
--         , requestUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstance
--
--         , requestGetQueryLoggingConfig $
--             newGetQueryLoggingConfig
--
--         , requestDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSet
--
--         , requestDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfig
--
--         , requestGetHostedZoneCount $
--             newGetHostedZoneCount
--
--         , requestEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSEC
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReason
--
--         , requestActivateKeySigningKey $
--             newActivateKeySigningKey
--
--         , requestListHostedZonesByName $
--             newListHostedZonesByName
--
--         , requestListQueryLoggingConfigs $
--             newListQueryLoggingConfigs
--
--           ]

--     , testGroup "response"
--         [ responseCreateReusableDelegationSet $
--             newCreateReusableDelegationSetResponse
--
--         , responseGetHealthCheckCount $
--             newGetHealthCheckCountResponse
--
--         , responseGetHostedZoneLimit $
--             newGetHostedZoneLimitResponse
--
--         , responseAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZoneResponse
--
--         , responseCreateTrafficPolicy $
--             newCreateTrafficPolicyResponse
--
--         , responseListGeoLocations $
--             newListGeoLocationsResponse
--
--         , responseListTrafficPolicies $
--             newListTrafficPoliciesResponse
--
--         , responseGetGeoLocation $
--             newGetGeoLocationResponse
--
--         , responseChangeTagsForResource $
--             newChangeTagsForResourceResponse
--
--         , responseDeleteHostedZone $
--             newDeleteHostedZoneResponse
--
--         , responseCreateHealthCheck $
--             newCreateHealthCheckResponse
--
--         , responseDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorizationResponse
--
--         , responseDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZoneResponse
--
--         , responseListHostedZones $
--             newListHostedZonesResponse
--
--         , responseCreateHostedZone $
--             newCreateHostedZoneResponse
--
--         , responseDeactivateKeySigningKey $
--             newDeactivateKeySigningKeyResponse
--
--         , responseTestDNSAnswer $
--             newTestDNSAnswerResponse
--
--         , responseChangeResourceRecordSets $
--             newChangeResourceRecordSetsResponse
--
--         , responseGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimitResponse
--
--         , responseGetReusableDelegationSet $
--             newGetReusableDelegationSetResponse
--
--         , responseGetAccountLimit $
--             newGetAccountLimitResponse
--
--         , responseGetDNSSEC $
--             newGetDNSSECResponse
--
--         , responseGetCheckerIpRanges $
--             newGetCheckerIpRangesResponse
--
--         , responseListReusableDelegationSets $
--             newListReusableDelegationSetsResponse
--
--         , responseDeleteKeySigningKey $
--             newDeleteKeySigningKeyResponse
--
--         , responseGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstanceResponse
--
--         , responseCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfigResponse
--
--         , responseDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSECResponse
--
--         , responseCreateKeySigningKey $
--             newCreateKeySigningKeyResponse
--
--         , responseUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyCommentResponse
--
--         , responseListResourceRecordSets $
--             newListResourceRecordSetsResponse
--
--         , responseGetHealthCheck $
--             newGetHealthCheckResponse
--
--         , responseDeleteTrafficPolicy $
--             newDeleteTrafficPolicyResponse
--
--         , responseGetHostedZone $
--             newGetHostedZoneResponse
--
--         , responseGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCountResponse
--
--         , responseListTagsForResources $
--             newListTagsForResourcesResponse
--
--         , responseListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZoneResponse
--
--         , responseListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizationsResponse
--
--         , responseListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicyResponse
--
--         , responseListTrafficPolicyVersions $
--             newListTrafficPolicyVersionsResponse
--
--         , responseCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorizationResponse
--
--         , responseCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersionResponse
--
--         , responseListHealthChecks $
--             newListHealthChecksResponse
--
--         , responseGetTrafficPolicy $
--             newGetTrafficPolicyResponse
--
--         , responseDeleteHealthCheck $
--             newDeleteHealthCheckResponse
--
--         , responseUpdateHealthCheck $
--             newUpdateHealthCheckResponse
--
--         , responseCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstanceResponse
--
--         , responseGetHealthCheckStatus $
--             newGetHealthCheckStatusResponse
--
--         , responseListHostedZonesByVPC $
--             newListHostedZonesByVPCResponse
--
--         , responseGetChange $
--             newGetChangeResponse
--
--         , responseUpdateHostedZoneComment $
--             newUpdateHostedZoneCommentResponse
--
--         , responseListTrafficPolicyInstances $
--             newListTrafficPolicyInstancesResponse
--
--         , responseDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstanceResponse
--
--         , responseUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstanceResponse
--
--         , responseGetQueryLoggingConfig $
--             newGetQueryLoggingConfigResponse
--
--         , responseDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSetResponse
--
--         , responseDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfigResponse
--
--         , responseGetHostedZoneCount $
--             newGetHostedZoneCountResponse
--
--         , responseEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSECResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReasonResponse
--
--         , responseActivateKeySigningKey $
--             newActivateKeySigningKeyResponse
--
--         , responseListHostedZonesByName $
--             newListHostedZonesByNameResponse
--
--         , responseListQueryLoggingConfigs $
--             newListQueryLoggingConfigsResponse
--
--           ]
--     ]

-- Requests

requestCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
requestCreateReusableDelegationSet =
  req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

requestGetHealthCheckCount :: GetHealthCheckCount -> TestTree
requestGetHealthCheckCount =
  req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

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

requestCreateTrafficPolicy :: CreateTrafficPolicy -> TestTree
requestCreateTrafficPolicy =
  req
    "CreateTrafficPolicy"
    "fixture/CreateTrafficPolicy.yaml"

requestListGeoLocations :: ListGeoLocations -> TestTree
requestListGeoLocations =
  req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

requestListTrafficPolicies :: ListTrafficPolicies -> TestTree
requestListTrafficPolicies =
  req
    "ListTrafficPolicies"
    "fixture/ListTrafficPolicies.yaml"

requestGetGeoLocation :: GetGeoLocation -> TestTree
requestGetGeoLocation =
  req
    "GetGeoLocation"
    "fixture/GetGeoLocation.yaml"

requestChangeTagsForResource :: ChangeTagsForResource -> TestTree
requestChangeTagsForResource =
  req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

requestDeleteHostedZone :: DeleteHostedZone -> TestTree
requestDeleteHostedZone =
  req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone.yaml"

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

requestDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
requestDisassociateVPCFromHostedZone =
  req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

requestListHostedZones :: ListHostedZones -> TestTree
requestListHostedZones =
  req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

requestCreateHostedZone :: CreateHostedZone -> TestTree
requestCreateHostedZone =
  req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

requestDeactivateKeySigningKey :: DeactivateKeySigningKey -> TestTree
requestDeactivateKeySigningKey =
  req
    "DeactivateKeySigningKey"
    "fixture/DeactivateKeySigningKey.yaml"

requestTestDNSAnswer :: TestDNSAnswer -> TestTree
requestTestDNSAnswer =
  req
    "TestDNSAnswer"
    "fixture/TestDNSAnswer.yaml"

requestChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
requestChangeResourceRecordSets =
  req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

requestGetReusableDelegationSetLimit :: GetReusableDelegationSetLimit -> TestTree
requestGetReusableDelegationSetLimit =
  req
    "GetReusableDelegationSetLimit"
    "fixture/GetReusableDelegationSetLimit.yaml"

requestGetReusableDelegationSet :: GetReusableDelegationSet -> TestTree
requestGetReusableDelegationSet =
  req
    "GetReusableDelegationSet"
    "fixture/GetReusableDelegationSet.yaml"

requestGetAccountLimit :: GetAccountLimit -> TestTree
requestGetAccountLimit =
  req
    "GetAccountLimit"
    "fixture/GetAccountLimit.yaml"

requestGetDNSSEC :: GetDNSSEC -> TestTree
requestGetDNSSEC =
  req
    "GetDNSSEC"
    "fixture/GetDNSSEC.yaml"

requestGetCheckerIpRanges :: GetCheckerIpRanges -> TestTree
requestGetCheckerIpRanges =
  req
    "GetCheckerIpRanges"
    "fixture/GetCheckerIpRanges.yaml"

requestListReusableDelegationSets :: ListReusableDelegationSets -> TestTree
requestListReusableDelegationSets =
  req
    "ListReusableDelegationSets"
    "fixture/ListReusableDelegationSets.yaml"

requestDeleteKeySigningKey :: DeleteKeySigningKey -> TestTree
requestDeleteKeySigningKey =
  req
    "DeleteKeySigningKey"
    "fixture/DeleteKeySigningKey.yaml"

requestGetTrafficPolicyInstance :: GetTrafficPolicyInstance -> TestTree
requestGetTrafficPolicyInstance =
  req
    "GetTrafficPolicyInstance"
    "fixture/GetTrafficPolicyInstance.yaml"

requestCreateQueryLoggingConfig :: CreateQueryLoggingConfig -> TestTree
requestCreateQueryLoggingConfig =
  req
    "CreateQueryLoggingConfig"
    "fixture/CreateQueryLoggingConfig.yaml"

requestDisableHostedZoneDNSSEC :: DisableHostedZoneDNSSEC -> TestTree
requestDisableHostedZoneDNSSEC =
  req
    "DisableHostedZoneDNSSEC"
    "fixture/DisableHostedZoneDNSSEC.yaml"

requestCreateKeySigningKey :: CreateKeySigningKey -> TestTree
requestCreateKeySigningKey =
  req
    "CreateKeySigningKey"
    "fixture/CreateKeySigningKey.yaml"

requestUpdateTrafficPolicyComment :: UpdateTrafficPolicyComment -> TestTree
requestUpdateTrafficPolicyComment =
  req
    "UpdateTrafficPolicyComment"
    "fixture/UpdateTrafficPolicyComment.yaml"

requestListResourceRecordSets :: ListResourceRecordSets -> TestTree
requestListResourceRecordSets =
  req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

requestGetHealthCheck :: GetHealthCheck -> TestTree
requestGetHealthCheck =
  req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

requestDeleteTrafficPolicy :: DeleteTrafficPolicy -> TestTree
requestDeleteTrafficPolicy =
  req
    "DeleteTrafficPolicy"
    "fixture/DeleteTrafficPolicy.yaml"

requestGetHostedZone :: GetHostedZone -> TestTree
requestGetHostedZone =
  req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

requestGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCount -> TestTree
requestGetTrafficPolicyInstanceCount =
  req
    "GetTrafficPolicyInstanceCount"
    "fixture/GetTrafficPolicyInstanceCount.yaml"

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources =
  req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

requestListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZone -> TestTree
requestListTrafficPolicyInstancesByHostedZone =
  req
    "ListTrafficPolicyInstancesByHostedZone"
    "fixture/ListTrafficPolicyInstancesByHostedZone.yaml"

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

requestListTrafficPolicyVersions :: ListTrafficPolicyVersions -> TestTree
requestListTrafficPolicyVersions =
  req
    "ListTrafficPolicyVersions"
    "fixture/ListTrafficPolicyVersions.yaml"

requestCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorization -> TestTree
requestCreateVPCAssociationAuthorization =
  req
    "CreateVPCAssociationAuthorization"
    "fixture/CreateVPCAssociationAuthorization.yaml"

requestCreateTrafficPolicyVersion :: CreateTrafficPolicyVersion -> TestTree
requestCreateTrafficPolicyVersion =
  req
    "CreateTrafficPolicyVersion"
    "fixture/CreateTrafficPolicyVersion.yaml"

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

requestCreateTrafficPolicyInstance :: CreateTrafficPolicyInstance -> TestTree
requestCreateTrafficPolicyInstance =
  req
    "CreateTrafficPolicyInstance"
    "fixture/CreateTrafficPolicyInstance.yaml"

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

requestGetChange :: GetChange -> TestTree
requestGetChange =
  req
    "GetChange"
    "fixture/GetChange.yaml"

requestUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
requestUpdateHostedZoneComment =
  req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

requestListTrafficPolicyInstances :: ListTrafficPolicyInstances -> TestTree
requestListTrafficPolicyInstances =
  req
    "ListTrafficPolicyInstances"
    "fixture/ListTrafficPolicyInstances.yaml"

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

requestGetQueryLoggingConfig :: GetQueryLoggingConfig -> TestTree
requestGetQueryLoggingConfig =
  req
    "GetQueryLoggingConfig"
    "fixture/GetQueryLoggingConfig.yaml"

requestDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
requestDeleteReusableDelegationSet =
  req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

requestDeleteQueryLoggingConfig :: DeleteQueryLoggingConfig -> TestTree
requestDeleteQueryLoggingConfig =
  req
    "DeleteQueryLoggingConfig"
    "fixture/DeleteQueryLoggingConfig.yaml"

requestGetHostedZoneCount :: GetHostedZoneCount -> TestTree
requestGetHostedZoneCount =
  req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

requestEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSEC -> TestTree
requestEnableHostedZoneDNSSEC =
  req
    "EnableHostedZoneDNSSEC"
    "fixture/EnableHostedZoneDNSSEC.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
requestGetHealthCheckLastFailureReason =
  req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

requestActivateKeySigningKey :: ActivateKeySigningKey -> TestTree
requestActivateKeySigningKey =
  req
    "ActivateKeySigningKey"
    "fixture/ActivateKeySigningKey.yaml"

requestListHostedZonesByName :: ListHostedZonesByName -> TestTree
requestListHostedZonesByName =
  req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

requestListQueryLoggingConfigs :: ListQueryLoggingConfigs -> TestTree
requestListQueryLoggingConfigs =
  req
    "ListQueryLoggingConfigs"
    "fixture/ListQueryLoggingConfigs.yaml"

-- Responses

responseCreateReusableDelegationSet :: CreateReusableDelegationSetResponse -> TestTree
responseCreateReusableDelegationSet =
  res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateReusableDelegationSet)

responseGetHealthCheckCount :: GetHealthCheckCountResponse -> TestTree
responseGetHealthCheckCount =
  res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    defaultService
    (Proxy :: Proxy GetHealthCheckCount)

responseGetHostedZoneLimit :: GetHostedZoneLimitResponse -> TestTree
responseGetHostedZoneLimit =
  res
    "GetHostedZoneLimitResponse"
    "fixture/GetHostedZoneLimitResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostedZoneLimit)

responseAssociateVPCWithHostedZone :: AssociateVPCWithHostedZoneResponse -> TestTree
responseAssociateVPCWithHostedZone =
  res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateVPCWithHostedZone)

responseCreateTrafficPolicy :: CreateTrafficPolicyResponse -> TestTree
responseCreateTrafficPolicy =
  res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficPolicy)

responseListGeoLocations :: ListGeoLocationsResponse -> TestTree
responseListGeoLocations =
  res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGeoLocations)

responseListTrafficPolicies :: ListTrafficPoliciesResponse -> TestTree
responseListTrafficPolicies =
  res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrafficPolicies)

responseGetGeoLocation :: GetGeoLocationResponse -> TestTree
responseGetGeoLocation =
  res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGeoLocation)

responseChangeTagsForResource :: ChangeTagsForResourceResponse -> TestTree
responseChangeTagsForResource =
  res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ChangeTagsForResource)

responseDeleteHostedZone :: DeleteHostedZoneResponse -> TestTree
responseDeleteHostedZone =
  res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHostedZone)

responseCreateHealthCheck :: CreateHealthCheckResponse -> TestTree
responseCreateHealthCheck =
  res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHealthCheck)

responseDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorizationResponse -> TestTree
responseDeleteVPCAssociationAuthorization =
  res
    "DeleteVPCAssociationAuthorizationResponse"
    "fixture/DeleteVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteVPCAssociationAuthorization)

responseDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZoneResponse -> TestTree
responseDisassociateVPCFromHostedZone =
  res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateVPCFromHostedZone)

responseListHostedZones :: ListHostedZonesResponse -> TestTree
responseListHostedZones =
  res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    defaultService
    (Proxy :: Proxy ListHostedZones)

responseCreateHostedZone :: CreateHostedZoneResponse -> TestTree
responseCreateHostedZone =
  res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy CreateHostedZone)

responseDeactivateKeySigningKey :: DeactivateKeySigningKeyResponse -> TestTree
responseDeactivateKeySigningKey =
  res
    "DeactivateKeySigningKeyResponse"
    "fixture/DeactivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeactivateKeySigningKey)

responseTestDNSAnswer :: TestDNSAnswerResponse -> TestTree
responseTestDNSAnswer =
  res
    "TestDNSAnswerResponse"
    "fixture/TestDNSAnswerResponse.proto"
    defaultService
    (Proxy :: Proxy TestDNSAnswer)

responseChangeResourceRecordSets :: ChangeResourceRecordSetsResponse -> TestTree
responseChangeResourceRecordSets =
  res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ChangeResourceRecordSets)

responseGetReusableDelegationSetLimit :: GetReusableDelegationSetLimitResponse -> TestTree
responseGetReusableDelegationSetLimit =
  res
    "GetReusableDelegationSetLimitResponse"
    "fixture/GetReusableDelegationSetLimitResponse.proto"
    defaultService
    (Proxy :: Proxy GetReusableDelegationSetLimit)

responseGetReusableDelegationSet :: GetReusableDelegationSetResponse -> TestTree
responseGetReusableDelegationSet =
  res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse.proto"
    defaultService
    (Proxy :: Proxy GetReusableDelegationSet)

responseGetAccountLimit :: GetAccountLimitResponse -> TestTree
responseGetAccountLimit =
  res
    "GetAccountLimitResponse"
    "fixture/GetAccountLimitResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccountLimit)

responseGetDNSSEC :: GetDNSSECResponse -> TestTree
responseGetDNSSEC =
  res
    "GetDNSSECResponse"
    "fixture/GetDNSSECResponse.proto"
    defaultService
    (Proxy :: Proxy GetDNSSEC)

responseGetCheckerIpRanges :: GetCheckerIpRangesResponse -> TestTree
responseGetCheckerIpRanges =
  res
    "GetCheckerIpRangesResponse"
    "fixture/GetCheckerIpRangesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCheckerIpRanges)

responseListReusableDelegationSets :: ListReusableDelegationSetsResponse -> TestTree
responseListReusableDelegationSets =
  res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListReusableDelegationSets)

responseDeleteKeySigningKey :: DeleteKeySigningKeyResponse -> TestTree
responseDeleteKeySigningKey =
  res
    "DeleteKeySigningKeyResponse"
    "fixture/DeleteKeySigningKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeySigningKey)

responseGetTrafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TestTree
responseGetTrafficPolicyInstance =
  res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetTrafficPolicyInstance)

responseCreateQueryLoggingConfig :: CreateQueryLoggingConfigResponse -> TestTree
responseCreateQueryLoggingConfig =
  res
    "CreateQueryLoggingConfigResponse"
    "fixture/CreateQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateQueryLoggingConfig)

responseDisableHostedZoneDNSSEC :: DisableHostedZoneDNSSECResponse -> TestTree
responseDisableHostedZoneDNSSEC =
  res
    "DisableHostedZoneDNSSECResponse"
    "fixture/DisableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy :: Proxy DisableHostedZoneDNSSEC)

responseCreateKeySigningKey :: CreateKeySigningKeyResponse -> TestTree
responseCreateKeySigningKey =
  res
    "CreateKeySigningKeyResponse"
    "fixture/CreateKeySigningKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeySigningKey)

responseUpdateTrafficPolicyComment :: UpdateTrafficPolicyCommentResponse -> TestTree
responseUpdateTrafficPolicyComment =
  res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrafficPolicyComment)

responseListResourceRecordSets :: ListResourceRecordSetsResponse -> TestTree
responseListResourceRecordSets =
  res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceRecordSets)

responseGetHealthCheck :: GetHealthCheckResponse -> TestTree
responseGetHealthCheck =
  res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy GetHealthCheck)

responseDeleteTrafficPolicy :: DeleteTrafficPolicyResponse -> TestTree
responseDeleteTrafficPolicy =
  res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficPolicy)

responseGetHostedZone :: GetHostedZoneResponse -> TestTree
responseGetHostedZone =
  res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostedZone)

responseGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> TestTree
responseGetTrafficPolicyInstanceCount =
  res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    defaultService
    (Proxy :: Proxy GetTrafficPolicyInstanceCount)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResources)

responseListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
responseListTrafficPolicyInstancesByHostedZone =
  res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrafficPolicyInstancesByHostedZone)

responseListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizationsResponse -> TestTree
responseListVPCAssociationAuthorizations =
  res
    "ListVPCAssociationAuthorizationsResponse"
    "fixture/ListVPCAssociationAuthorizationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListVPCAssociationAuthorizations)

responseListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
responseListTrafficPolicyInstancesByPolicy =
  res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrafficPolicyInstancesByPolicy)

responseListTrafficPolicyVersions :: ListTrafficPolicyVersionsResponse -> TestTree
responseListTrafficPolicyVersions =
  res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrafficPolicyVersions)

responseCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorizationResponse -> TestTree
responseCreateVPCAssociationAuthorization =
  res
    "CreateVPCAssociationAuthorizationResponse"
    "fixture/CreateVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateVPCAssociationAuthorization)

responseCreateTrafficPolicyVersion :: CreateTrafficPolicyVersionResponse -> TestTree
responseCreateTrafficPolicyVersion =
  res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficPolicyVersion)

responseListHealthChecks :: ListHealthChecksResponse -> TestTree
responseListHealthChecks =
  res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    defaultService
    (Proxy :: Proxy ListHealthChecks)

responseGetTrafficPolicy :: GetTrafficPolicyResponse -> TestTree
responseGetTrafficPolicy =
  res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetTrafficPolicy)

responseDeleteHealthCheck :: DeleteHealthCheckResponse -> TestTree
responseDeleteHealthCheck =
  res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteHealthCheck)

responseUpdateHealthCheck :: UpdateHealthCheckResponse -> TestTree
responseUpdateHealthCheck =
  res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateHealthCheck)

responseCreateTrafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TestTree
responseCreateTrafficPolicyInstance =
  res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTrafficPolicyInstance)

responseGetHealthCheckStatus :: GetHealthCheckStatusResponse -> TestTree
responseGetHealthCheckStatus =
  res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetHealthCheckStatus)

responseListHostedZonesByVPC :: ListHostedZonesByVPCResponse -> TestTree
responseListHostedZonesByVPC =
  res
    "ListHostedZonesByVPCResponse"
    "fixture/ListHostedZonesByVPCResponse.proto"
    defaultService
    (Proxy :: Proxy ListHostedZonesByVPC)

responseGetChange :: GetChangeResponse -> TestTree
responseGetChange =
  res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    defaultService
    (Proxy :: Proxy GetChange)

responseUpdateHostedZoneComment :: UpdateHostedZoneCommentResponse -> TestTree
responseUpdateHostedZoneComment =
  res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateHostedZoneComment)

responseListTrafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> TestTree
responseListTrafficPolicyInstances =
  res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListTrafficPolicyInstances)

responseDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstanceResponse -> TestTree
responseDeleteTrafficPolicyInstance =
  res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTrafficPolicyInstance)

responseUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TestTree
responseUpdateTrafficPolicyInstance =
  res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTrafficPolicyInstance)

responseGetQueryLoggingConfig :: GetQueryLoggingConfigResponse -> TestTree
responseGetQueryLoggingConfig =
  res
    "GetQueryLoggingConfigResponse"
    "fixture/GetQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy :: Proxy GetQueryLoggingConfig)

responseDeleteReusableDelegationSet :: DeleteReusableDelegationSetResponse -> TestTree
responseDeleteReusableDelegationSet =
  res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteReusableDelegationSet)

responseDeleteQueryLoggingConfig :: DeleteQueryLoggingConfigResponse -> TestTree
responseDeleteQueryLoggingConfig =
  res
    "DeleteQueryLoggingConfigResponse"
    "fixture/DeleteQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteQueryLoggingConfig)

responseGetHostedZoneCount :: GetHostedZoneCountResponse -> TestTree
responseGetHostedZoneCount =
  res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    defaultService
    (Proxy :: Proxy GetHostedZoneCount)

responseEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSECResponse -> TestTree
responseEnableHostedZoneDNSSEC =
  res
    "EnableHostedZoneDNSSECResponse"
    "fixture/EnableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy :: Proxy EnableHostedZoneDNSSEC)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReasonResponse -> TestTree
responseGetHealthCheckLastFailureReason =
  res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    defaultService
    (Proxy :: Proxy GetHealthCheckLastFailureReason)

responseActivateKeySigningKey :: ActivateKeySigningKeyResponse -> TestTree
responseActivateKeySigningKey =
  res
    "ActivateKeySigningKeyResponse"
    "fixture/ActivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy :: Proxy ActivateKeySigningKey)

responseListHostedZonesByName :: ListHostedZonesByNameResponse -> TestTree
responseListHostedZonesByName =
  res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    defaultService
    (Proxy :: Proxy ListHostedZonesByName)

responseListQueryLoggingConfigs :: ListQueryLoggingConfigsResponse -> TestTree
responseListQueryLoggingConfigs =
  res
    "ListQueryLoggingConfigsResponse"
    "fixture/ListQueryLoggingConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy ListQueryLoggingConfigs)
