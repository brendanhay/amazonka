{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53 where

import Amazonka.Route53
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetHostedZoneLimit $
--             newGetHostedZoneLimit
--
--         , requestAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZone
--
--         , requestDeleteTrafficPolicy $
--             newDeleteTrafficPolicy
--
--         , requestDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSEC
--
--         , requestCreateKeySigningKey $
--             newCreateKeySigningKey
--
--         , requestGetCheckerIpRanges $
--             newGetCheckerIpRanges
--
--         , requestGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstance
--
--         , requestGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReason
--
--         , requestDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSet
--
--         , requestListHostedZonesByName $
--             newListHostedZonesByName
--
--         , requestActivateKeySigningKey $
--             newActivateKeySigningKey
--
--         , requestListReusableDelegationSets $
--             newListReusableDelegationSets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListQueryLoggingConfigs $
--             newListQueryLoggingConfigs
--
--         , requestListTrafficPolicyInstances $
--             newListTrafficPolicyInstances
--
--         , requestCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstance
--
--         , requestGetChange $
--             newGetChange
--
--         , requestChangeResourceRecordSets $
--             newChangeResourceRecordSets
--
--         , requestDeleteHealthCheck $
--             newDeleteHealthCheck
--
--         , requestUpdateHealthCheck $
--             newUpdateHealthCheck
--
--         , requestCreateHostedZone $
--             newCreateHostedZone
--
--         , requestCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorization
--
--         , requestListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizations
--
--         , requestListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicy
--
--         , requestDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZone
--
--         , requestCreateHealthCheck $
--             newCreateHealthCheck
--
--         , requestDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorization
--
--         , requestChangeTagsForResource $
--             newChangeTagsForResource
--
--         , requestListHostedZones $
--             newListHostedZones
--
--         , requestGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCount
--
--         , requestListGeoLocations $
--             newListGeoLocations
--
--         , requestGetHostedZone $
--             newGetHostedZone
--
--         , requestGetHealthCheck $
--             newGetHealthCheck
--
--         , requestListResourceRecordSets $
--             newListResourceRecordSets
--
--         , requestCreateReusableDelegationSet $
--             newCreateReusableDelegationSet
--
--         , requestCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfig
--
--         , requestGetHealthCheckCount $
--             newGetHealthCheckCount
--
--         , requestUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyComment
--
--         , requestGetHostedZoneCount $
--             newGetHostedZoneCount
--
--         , requestDeleteKeySigningKey $
--             newDeleteKeySigningKey
--
--         , requestGetDNSSEC $
--             newGetDNSSEC
--
--         , requestGetAccountLimit $
--             newGetAccountLimit
--
--         , requestEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSEC
--
--         , requestDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfig
--
--         , requestGetQueryLoggingConfig $
--             newGetQueryLoggingConfig
--
--         , requestGetReusableDelegationSet $
--             newGetReusableDelegationSet
--
--         , requestDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstance
--
--         , requestUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstance
--
--         , requestUpdateHostedZoneComment $
--             newUpdateHostedZoneComment
--
--         , requestGetHealthCheckStatus $
--             newGetHealthCheckStatus
--
--         , requestListHostedZonesByVPC $
--             newListHostedZonesByVPC
--
--         , requestGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimit
--
--         , requestCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersion
--
--         , requestDeactivateKeySigningKey $
--             newDeactivateKeySigningKey
--
--         , requestTestDNSAnswer $
--             newTestDNSAnswer
--
--         , requestListHealthChecks $
--             newListHealthChecks
--
--         , requestGetTrafficPolicy $
--             newGetTrafficPolicy
--
--         , requestListTrafficPolicyVersions $
--             newListTrafficPolicyVersions
--
--         , requestDeleteHostedZone $
--             newDeleteHostedZone
--
--         , requestGetGeoLocation $
--             newGetGeoLocation
--
--         , requestListTagsForResources $
--             newListTagsForResources
--
--         , requestCreateTrafficPolicy $
--             newCreateTrafficPolicy
--
--         , requestListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZone
--
--         , requestListTrafficPolicies $
--             newListTrafficPolicies
--
--           ]

--     , testGroup "response"
--         [ responseGetHostedZoneLimit $
--             newGetHostedZoneLimitResponse
--
--         , responseAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZoneResponse
--
--         , responseDeleteTrafficPolicy $
--             newDeleteTrafficPolicyResponse
--
--         , responseDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSECResponse
--
--         , responseCreateKeySigningKey $
--             newCreateKeySigningKeyResponse
--
--         , responseGetCheckerIpRanges $
--             newGetCheckerIpRangesResponse
--
--         , responseGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstanceResponse
--
--         , responseGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReasonResponse
--
--         , responseDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSetResponse
--
--         , responseListHostedZonesByName $
--             newListHostedZonesByNameResponse
--
--         , responseActivateKeySigningKey $
--             newActivateKeySigningKeyResponse
--
--         , responseListReusableDelegationSets $
--             newListReusableDelegationSetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListQueryLoggingConfigs $
--             newListQueryLoggingConfigsResponse
--
--         , responseListTrafficPolicyInstances $
--             newListTrafficPolicyInstancesResponse
--
--         , responseCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstanceResponse
--
--         , responseGetChange $
--             newGetChangeResponse
--
--         , responseChangeResourceRecordSets $
--             newChangeResourceRecordSetsResponse
--
--         , responseDeleteHealthCheck $
--             newDeleteHealthCheckResponse
--
--         , responseUpdateHealthCheck $
--             newUpdateHealthCheckResponse
--
--         , responseCreateHostedZone $
--             newCreateHostedZoneResponse
--
--         , responseCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorizationResponse
--
--         , responseListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizationsResponse
--
--         , responseListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicyResponse
--
--         , responseDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZoneResponse
--
--         , responseCreateHealthCheck $
--             newCreateHealthCheckResponse
--
--         , responseDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorizationResponse
--
--         , responseChangeTagsForResource $
--             newChangeTagsForResourceResponse
--
--         , responseListHostedZones $
--             newListHostedZonesResponse
--
--         , responseGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCountResponse
--
--         , responseListGeoLocations $
--             newListGeoLocationsResponse
--
--         , responseGetHostedZone $
--             newGetHostedZoneResponse
--
--         , responseGetHealthCheck $
--             newGetHealthCheckResponse
--
--         , responseListResourceRecordSets $
--             newListResourceRecordSetsResponse
--
--         , responseCreateReusableDelegationSet $
--             newCreateReusableDelegationSetResponse
--
--         , responseCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfigResponse
--
--         , responseGetHealthCheckCount $
--             newGetHealthCheckCountResponse
--
--         , responseUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyCommentResponse
--
--         , responseGetHostedZoneCount $
--             newGetHostedZoneCountResponse
--
--         , responseDeleteKeySigningKey $
--             newDeleteKeySigningKeyResponse
--
--         , responseGetDNSSEC $
--             newGetDNSSECResponse
--
--         , responseGetAccountLimit $
--             newGetAccountLimitResponse
--
--         , responseEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSECResponse
--
--         , responseDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfigResponse
--
--         , responseGetQueryLoggingConfig $
--             newGetQueryLoggingConfigResponse
--
--         , responseGetReusableDelegationSet $
--             newGetReusableDelegationSetResponse
--
--         , responseDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstanceResponse
--
--         , responseUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstanceResponse
--
--         , responseUpdateHostedZoneComment $
--             newUpdateHostedZoneCommentResponse
--
--         , responseGetHealthCheckStatus $
--             newGetHealthCheckStatusResponse
--
--         , responseListHostedZonesByVPC $
--             newListHostedZonesByVPCResponse
--
--         , responseGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimitResponse
--
--         , responseCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersionResponse
--
--         , responseDeactivateKeySigningKey $
--             newDeactivateKeySigningKeyResponse
--
--         , responseTestDNSAnswer $
--             newTestDNSAnswerResponse
--
--         , responseListHealthChecks $
--             newListHealthChecksResponse
--
--         , responseGetTrafficPolicy $
--             newGetTrafficPolicyResponse
--
--         , responseListTrafficPolicyVersions $
--             newListTrafficPolicyVersionsResponse
--
--         , responseDeleteHostedZone $
--             newDeleteHostedZoneResponse
--
--         , responseGetGeoLocation $
--             newGetGeoLocationResponse
--
--         , responseListTagsForResources $
--             newListTagsForResourcesResponse
--
--         , responseCreateTrafficPolicy $
--             newCreateTrafficPolicyResponse
--
--         , responseListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZoneResponse
--
--         , responseListTrafficPolicies $
--             newListTrafficPoliciesResponse
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

requestActivateKeySigningKey :: ActivateKeySigningKey -> TestTree
requestActivateKeySigningKey =
  req
    "ActivateKeySigningKey"
    "fixture/ActivateKeySigningKey.yaml"

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

requestDeleteKeySigningKey :: DeleteKeySigningKey -> TestTree
requestDeleteKeySigningKey =
  req
    "DeleteKeySigningKey"
    "fixture/DeleteKeySigningKey.yaml"

requestGetDNSSEC :: GetDNSSEC -> TestTree
requestGetDNSSEC =
  req
    "GetDNSSEC"
    "fixture/GetDNSSEC.yaml"

requestGetAccountLimit :: GetAccountLimit -> TestTree
requestGetAccountLimit =
  req
    "GetAccountLimit"
    "fixture/GetAccountLimit.yaml"

requestEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSEC -> TestTree
requestEnableHostedZoneDNSSEC =
  req
    "EnableHostedZoneDNSSEC"
    "fixture/EnableHostedZoneDNSSEC.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZoneLimit)

responseAssociateVPCWithHostedZone :: AssociateVPCWithHostedZoneResponse -> TestTree
responseAssociateVPCWithHostedZone =
  res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVPCWithHostedZone)

responseDeleteTrafficPolicy :: DeleteTrafficPolicyResponse -> TestTree
responseDeleteTrafficPolicy =
  res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficPolicy)

responseDisableHostedZoneDNSSEC :: DisableHostedZoneDNSSECResponse -> TestTree
responseDisableHostedZoneDNSSEC =
  res
    "DisableHostedZoneDNSSECResponse"
    "fixture/DisableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableHostedZoneDNSSEC)

responseCreateKeySigningKey :: CreateKeySigningKeyResponse -> TestTree
responseCreateKeySigningKey =
  res
    "CreateKeySigningKeyResponse"
    "fixture/CreateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeySigningKey)

responseGetCheckerIpRanges :: GetCheckerIpRangesResponse -> TestTree
responseGetCheckerIpRanges =
  res
    "GetCheckerIpRangesResponse"
    "fixture/GetCheckerIpRangesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCheckerIpRanges)

responseGetTrafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TestTree
responseGetTrafficPolicyInstance =
  res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicyInstance)

responseGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReasonResponse -> TestTree
responseGetHealthCheckLastFailureReason =
  res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckLastFailureReason)

responseDeleteReusableDelegationSet :: DeleteReusableDelegationSetResponse -> TestTree
responseDeleteReusableDelegationSet =
  res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReusableDelegationSet)

responseListHostedZonesByName :: ListHostedZonesByNameResponse -> TestTree
responseListHostedZonesByName =
  res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZonesByName)

responseActivateKeySigningKey :: ActivateKeySigningKeyResponse -> TestTree
responseActivateKeySigningKey =
  res
    "ActivateKeySigningKeyResponse"
    "fixture/ActivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateKeySigningKey)

responseListReusableDelegationSets :: ListReusableDelegationSetsResponse -> TestTree
responseListReusableDelegationSets =
  res
    "ListReusableDelegationSetsResponse"
    "fixture/ListReusableDelegationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReusableDelegationSets)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListQueryLoggingConfigs :: ListQueryLoggingConfigsResponse -> TestTree
responseListQueryLoggingConfigs =
  res
    "ListQueryLoggingConfigsResponse"
    "fixture/ListQueryLoggingConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueryLoggingConfigs)

responseListTrafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> TestTree
responseListTrafficPolicyInstances =
  res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstances)

responseCreateTrafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TestTree
responseCreateTrafficPolicyInstance =
  res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicyInstance)

responseGetChange :: GetChangeResponse -> TestTree
responseGetChange =
  res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChange)

responseChangeResourceRecordSets :: ChangeResourceRecordSetsResponse -> TestTree
responseChangeResourceRecordSets =
  res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeResourceRecordSets)

responseDeleteHealthCheck :: DeleteHealthCheckResponse -> TestTree
responseDeleteHealthCheck =
  res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHealthCheck)

responseUpdateHealthCheck :: UpdateHealthCheckResponse -> TestTree
responseUpdateHealthCheck =
  res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHealthCheck)

responseCreateHostedZone :: CreateHostedZoneResponse -> TestTree
responseCreateHostedZone =
  res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHostedZone)

responseCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorizationResponse -> TestTree
responseCreateVPCAssociationAuthorization =
  res
    "CreateVPCAssociationAuthorizationResponse"
    "fixture/CreateVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVPCAssociationAuthorization)

responseListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizationsResponse -> TestTree
responseListVPCAssociationAuthorizations =
  res
    "ListVPCAssociationAuthorizationsResponse"
    "fixture/ListVPCAssociationAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVPCAssociationAuthorizations)

responseListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
responseListTrafficPolicyInstancesByPolicy =
  res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstancesByPolicy)

responseDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZoneResponse -> TestTree
responseDisassociateVPCFromHostedZone =
  res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateVPCFromHostedZone)

responseCreateHealthCheck :: CreateHealthCheckResponse -> TestTree
responseCreateHealthCheck =
  res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHealthCheck)

responseDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorizationResponse -> TestTree
responseDeleteVPCAssociationAuthorization =
  res
    "DeleteVPCAssociationAuthorizationResponse"
    "fixture/DeleteVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVPCAssociationAuthorization)

responseChangeTagsForResource :: ChangeTagsForResourceResponse -> TestTree
responseChangeTagsForResource =
  res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeTagsForResource)

responseListHostedZones :: ListHostedZonesResponse -> TestTree
responseListHostedZones =
  res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZones)

responseGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> TestTree
responseGetTrafficPolicyInstanceCount =
  res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicyInstanceCount)

responseListGeoLocations :: ListGeoLocationsResponse -> TestTree
responseListGeoLocations =
  res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeoLocations)

responseGetHostedZone :: GetHostedZoneResponse -> TestTree
responseGetHostedZone =
  res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZone)

responseGetHealthCheck :: GetHealthCheckResponse -> TestTree
responseGetHealthCheck =
  res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheck)

responseListResourceRecordSets :: ListResourceRecordSetsResponse -> TestTree
responseListResourceRecordSets =
  res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceRecordSets)

responseCreateReusableDelegationSet :: CreateReusableDelegationSetResponse -> TestTree
responseCreateReusableDelegationSet =
  res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReusableDelegationSet)

responseCreateQueryLoggingConfig :: CreateQueryLoggingConfigResponse -> TestTree
responseCreateQueryLoggingConfig =
  res
    "CreateQueryLoggingConfigResponse"
    "fixture/CreateQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueryLoggingConfig)

responseGetHealthCheckCount :: GetHealthCheckCountResponse -> TestTree
responseGetHealthCheckCount =
  res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckCount)

responseUpdateTrafficPolicyComment :: UpdateTrafficPolicyCommentResponse -> TestTree
responseUpdateTrafficPolicyComment =
  res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrafficPolicyComment)

responseGetHostedZoneCount :: GetHostedZoneCountResponse -> TestTree
responseGetHostedZoneCount =
  res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZoneCount)

responseDeleteKeySigningKey :: DeleteKeySigningKeyResponse -> TestTree
responseDeleteKeySigningKey =
  res
    "DeleteKeySigningKeyResponse"
    "fixture/DeleteKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeySigningKey)

responseGetDNSSEC :: GetDNSSECResponse -> TestTree
responseGetDNSSEC =
  res
    "GetDNSSECResponse"
    "fixture/GetDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDNSSEC)

responseGetAccountLimit :: GetAccountLimitResponse -> TestTree
responseGetAccountLimit =
  res
    "GetAccountLimitResponse"
    "fixture/GetAccountLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountLimit)

responseEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSECResponse -> TestTree
responseEnableHostedZoneDNSSEC =
  res
    "EnableHostedZoneDNSSECResponse"
    "fixture/EnableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableHostedZoneDNSSEC)

responseDeleteQueryLoggingConfig :: DeleteQueryLoggingConfigResponse -> TestTree
responseDeleteQueryLoggingConfig =
  res
    "DeleteQueryLoggingConfigResponse"
    "fixture/DeleteQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueryLoggingConfig)

responseGetQueryLoggingConfig :: GetQueryLoggingConfigResponse -> TestTree
responseGetQueryLoggingConfig =
  res
    "GetQueryLoggingConfigResponse"
    "fixture/GetQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetQueryLoggingConfig)

responseGetReusableDelegationSet :: GetReusableDelegationSetResponse -> TestTree
responseGetReusableDelegationSet =
  res
    "GetReusableDelegationSetResponse"
    "fixture/GetReusableDelegationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReusableDelegationSet)

responseDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstanceResponse -> TestTree
responseDeleteTrafficPolicyInstance =
  res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficPolicyInstance)

responseUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TestTree
responseUpdateTrafficPolicyInstance =
  res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrafficPolicyInstance)

responseUpdateHostedZoneComment :: UpdateHostedZoneCommentResponse -> TestTree
responseUpdateHostedZoneComment =
  res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHostedZoneComment)

responseGetHealthCheckStatus :: GetHealthCheckStatusResponse -> TestTree
responseGetHealthCheckStatus =
  res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckStatus)

responseListHostedZonesByVPC :: ListHostedZonesByVPCResponse -> TestTree
responseListHostedZonesByVPC =
  res
    "ListHostedZonesByVPCResponse"
    "fixture/ListHostedZonesByVPCResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZonesByVPC)

responseGetReusableDelegationSetLimit :: GetReusableDelegationSetLimitResponse -> TestTree
responseGetReusableDelegationSetLimit =
  res
    "GetReusableDelegationSetLimitResponse"
    "fixture/GetReusableDelegationSetLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReusableDelegationSetLimit)

responseCreateTrafficPolicyVersion :: CreateTrafficPolicyVersionResponse -> TestTree
responseCreateTrafficPolicyVersion =
  res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicyVersion)

responseDeactivateKeySigningKey :: DeactivateKeySigningKeyResponse -> TestTree
responseDeactivateKeySigningKey =
  res
    "DeactivateKeySigningKeyResponse"
    "fixture/DeactivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateKeySigningKey)

responseTestDNSAnswer :: TestDNSAnswerResponse -> TestTree
responseTestDNSAnswer =
  res
    "TestDNSAnswerResponse"
    "fixture/TestDNSAnswerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestDNSAnswer)

responseListHealthChecks :: ListHealthChecksResponse -> TestTree
responseListHealthChecks =
  res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHealthChecks)

responseGetTrafficPolicy :: GetTrafficPolicyResponse -> TestTree
responseGetTrafficPolicy =
  res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicy)

responseListTrafficPolicyVersions :: ListTrafficPolicyVersionsResponse -> TestTree
responseListTrafficPolicyVersions =
  res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyVersions)

responseDeleteHostedZone :: DeleteHostedZoneResponse -> TestTree
responseDeleteHostedZone =
  res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHostedZone)

responseGetGeoLocation :: GetGeoLocationResponse -> TestTree
responseGetGeoLocation =
  res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeoLocation)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResources)

responseCreateTrafficPolicy :: CreateTrafficPolicyResponse -> TestTree
responseCreateTrafficPolicy =
  res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicy)

responseListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
responseListTrafficPolicyInstancesByHostedZone =
  res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstancesByHostedZone)

responseListTrafficPolicies :: ListTrafficPoliciesResponse -> TestTree
responseListTrafficPolicies =
  res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicies)
