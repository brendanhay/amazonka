{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         [ requestActivateKeySigningKey $
--             newActivateKeySigningKey
--
--         , requestAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZone
--
--         , requestChangeCidrCollection $
--             newChangeCidrCollection
--
--         , requestChangeResourceRecordSets $
--             newChangeResourceRecordSets
--
--         , requestChangeTagsForResource $
--             newChangeTagsForResource
--
--         , requestCreateCidrCollection $
--             newCreateCidrCollection
--
--         , requestCreateHealthCheck $
--             newCreateHealthCheck
--
--         , requestCreateHostedZone $
--             newCreateHostedZone
--
--         , requestCreateKeySigningKey $
--             newCreateKeySigningKey
--
--         , requestCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfig
--
--         , requestCreateReusableDelegationSet $
--             newCreateReusableDelegationSet
--
--         , requestCreateTrafficPolicy $
--             newCreateTrafficPolicy
--
--         , requestCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstance
--
--         , requestCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersion
--
--         , requestCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorization
--
--         , requestDeactivateKeySigningKey $
--             newDeactivateKeySigningKey
--
--         , requestDeleteCidrCollection $
--             newDeleteCidrCollection
--
--         , requestDeleteHealthCheck $
--             newDeleteHealthCheck
--
--         , requestDeleteHostedZone $
--             newDeleteHostedZone
--
--         , requestDeleteKeySigningKey $
--             newDeleteKeySigningKey
--
--         , requestDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfig
--
--         , requestDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSet
--
--         , requestDeleteTrafficPolicy $
--             newDeleteTrafficPolicy
--
--         , requestDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstance
--
--         , requestDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorization
--
--         , requestDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSEC
--
--         , requestDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZone
--
--         , requestEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSEC
--
--         , requestGetAccountLimit $
--             newGetAccountLimit
--
--         , requestGetChange $
--             newGetChange
--
--         , requestGetCheckerIpRanges $
--             newGetCheckerIpRanges
--
--         , requestGetDNSSEC $
--             newGetDNSSEC
--
--         , requestGetGeoLocation $
--             newGetGeoLocation
--
--         , requestGetHealthCheck $
--             newGetHealthCheck
--
--         , requestGetHealthCheckCount $
--             newGetHealthCheckCount
--
--         , requestGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReason
--
--         , requestGetHealthCheckStatus $
--             newGetHealthCheckStatus
--
--         , requestGetHostedZone $
--             newGetHostedZone
--
--         , requestGetHostedZoneCount $
--             newGetHostedZoneCount
--
--         , requestGetHostedZoneLimit $
--             newGetHostedZoneLimit
--
--         , requestGetQueryLoggingConfig $
--             newGetQueryLoggingConfig
--
--         , requestGetReusableDelegationSet $
--             newGetReusableDelegationSet
--
--         , requestGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimit
--
--         , requestGetTrafficPolicy $
--             newGetTrafficPolicy
--
--         , requestGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstance
--
--         , requestGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCount
--
--         , requestListCidrBlocks $
--             newListCidrBlocks
--
--         , requestListCidrCollections $
--             newListCidrCollections
--
--         , requestListCidrLocations $
--             newListCidrLocations
--
--         , requestListGeoLocations $
--             newListGeoLocations
--
--         , requestListHealthChecks $
--             newListHealthChecks
--
--         , requestListHostedZones $
--             newListHostedZones
--
--         , requestListHostedZonesByName $
--             newListHostedZonesByName
--
--         , requestListHostedZonesByVPC $
--             newListHostedZonesByVPC
--
--         , requestListQueryLoggingConfigs $
--             newListQueryLoggingConfigs
--
--         , requestListResourceRecordSets $
--             newListResourceRecordSets
--
--         , requestListReusableDelegationSets $
--             newListReusableDelegationSets
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTagsForResources $
--             newListTagsForResources
--
--         , requestListTrafficPolicies $
--             newListTrafficPolicies
--
--         , requestListTrafficPolicyInstances $
--             newListTrafficPolicyInstances
--
--         , requestListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZone
--
--         , requestListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicy
--
--         , requestListTrafficPolicyVersions $
--             newListTrafficPolicyVersions
--
--         , requestListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizations
--
--         , requestTestDNSAnswer $
--             newTestDNSAnswer
--
--         , requestUpdateHealthCheck $
--             newUpdateHealthCheck
--
--         , requestUpdateHostedZoneComment $
--             newUpdateHostedZoneComment
--
--         , requestUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyComment
--
--         , requestUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstance
--
--           ]

--     , testGroup "response"
--         [ responseActivateKeySigningKey $
--             newActivateKeySigningKeyResponse
--
--         , responseAssociateVPCWithHostedZone $
--             newAssociateVPCWithHostedZoneResponse
--
--         , responseChangeCidrCollection $
--             newChangeCidrCollectionResponse
--
--         , responseChangeResourceRecordSets $
--             newChangeResourceRecordSetsResponse
--
--         , responseChangeTagsForResource $
--             newChangeTagsForResourceResponse
--
--         , responseCreateCidrCollection $
--             newCreateCidrCollectionResponse
--
--         , responseCreateHealthCheck $
--             newCreateHealthCheckResponse
--
--         , responseCreateHostedZone $
--             newCreateHostedZoneResponse
--
--         , responseCreateKeySigningKey $
--             newCreateKeySigningKeyResponse
--
--         , responseCreateQueryLoggingConfig $
--             newCreateQueryLoggingConfigResponse
--
--         , responseCreateReusableDelegationSet $
--             newCreateReusableDelegationSetResponse
--
--         , responseCreateTrafficPolicy $
--             newCreateTrafficPolicyResponse
--
--         , responseCreateTrafficPolicyInstance $
--             newCreateTrafficPolicyInstanceResponse
--
--         , responseCreateTrafficPolicyVersion $
--             newCreateTrafficPolicyVersionResponse
--
--         , responseCreateVPCAssociationAuthorization $
--             newCreateVPCAssociationAuthorizationResponse
--
--         , responseDeactivateKeySigningKey $
--             newDeactivateKeySigningKeyResponse
--
--         , responseDeleteCidrCollection $
--             newDeleteCidrCollectionResponse
--
--         , responseDeleteHealthCheck $
--             newDeleteHealthCheckResponse
--
--         , responseDeleteHostedZone $
--             newDeleteHostedZoneResponse
--
--         , responseDeleteKeySigningKey $
--             newDeleteKeySigningKeyResponse
--
--         , responseDeleteQueryLoggingConfig $
--             newDeleteQueryLoggingConfigResponse
--
--         , responseDeleteReusableDelegationSet $
--             newDeleteReusableDelegationSetResponse
--
--         , responseDeleteTrafficPolicy $
--             newDeleteTrafficPolicyResponse
--
--         , responseDeleteTrafficPolicyInstance $
--             newDeleteTrafficPolicyInstanceResponse
--
--         , responseDeleteVPCAssociationAuthorization $
--             newDeleteVPCAssociationAuthorizationResponse
--
--         , responseDisableHostedZoneDNSSEC $
--             newDisableHostedZoneDNSSECResponse
--
--         , responseDisassociateVPCFromHostedZone $
--             newDisassociateVPCFromHostedZoneResponse
--
--         , responseEnableHostedZoneDNSSEC $
--             newEnableHostedZoneDNSSECResponse
--
--         , responseGetAccountLimit $
--             newGetAccountLimitResponse
--
--         , responseGetChange $
--             newGetChangeResponse
--
--         , responseGetCheckerIpRanges $
--             newGetCheckerIpRangesResponse
--
--         , responseGetDNSSEC $
--             newGetDNSSECResponse
--
--         , responseGetGeoLocation $
--             newGetGeoLocationResponse
--
--         , responseGetHealthCheck $
--             newGetHealthCheckResponse
--
--         , responseGetHealthCheckCount $
--             newGetHealthCheckCountResponse
--
--         , responseGetHealthCheckLastFailureReason $
--             newGetHealthCheckLastFailureReasonResponse
--
--         , responseGetHealthCheckStatus $
--             newGetHealthCheckStatusResponse
--
--         , responseGetHostedZone $
--             newGetHostedZoneResponse
--
--         , responseGetHostedZoneCount $
--             newGetHostedZoneCountResponse
--
--         , responseGetHostedZoneLimit $
--             newGetHostedZoneLimitResponse
--
--         , responseGetQueryLoggingConfig $
--             newGetQueryLoggingConfigResponse
--
--         , responseGetReusableDelegationSet $
--             newGetReusableDelegationSetResponse
--
--         , responseGetReusableDelegationSetLimit $
--             newGetReusableDelegationSetLimitResponse
--
--         , responseGetTrafficPolicy $
--             newGetTrafficPolicyResponse
--
--         , responseGetTrafficPolicyInstance $
--             newGetTrafficPolicyInstanceResponse
--
--         , responseGetTrafficPolicyInstanceCount $
--             newGetTrafficPolicyInstanceCountResponse
--
--         , responseListCidrBlocks $
--             newListCidrBlocksResponse
--
--         , responseListCidrCollections $
--             newListCidrCollectionsResponse
--
--         , responseListCidrLocations $
--             newListCidrLocationsResponse
--
--         , responseListGeoLocations $
--             newListGeoLocationsResponse
--
--         , responseListHealthChecks $
--             newListHealthChecksResponse
--
--         , responseListHostedZones $
--             newListHostedZonesResponse
--
--         , responseListHostedZonesByName $
--             newListHostedZonesByNameResponse
--
--         , responseListHostedZonesByVPC $
--             newListHostedZonesByVPCResponse
--
--         , responseListQueryLoggingConfigs $
--             newListQueryLoggingConfigsResponse
--
--         , responseListResourceRecordSets $
--             newListResourceRecordSetsResponse
--
--         , responseListReusableDelegationSets $
--             newListReusableDelegationSetsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTagsForResources $
--             newListTagsForResourcesResponse
--
--         , responseListTrafficPolicies $
--             newListTrafficPoliciesResponse
--
--         , responseListTrafficPolicyInstances $
--             newListTrafficPolicyInstancesResponse
--
--         , responseListTrafficPolicyInstancesByHostedZone $
--             newListTrafficPolicyInstancesByHostedZoneResponse
--
--         , responseListTrafficPolicyInstancesByPolicy $
--             newListTrafficPolicyInstancesByPolicyResponse
--
--         , responseListTrafficPolicyVersions $
--             newListTrafficPolicyVersionsResponse
--
--         , responseListVPCAssociationAuthorizations $
--             newListVPCAssociationAuthorizationsResponse
--
--         , responseTestDNSAnswer $
--             newTestDNSAnswerResponse
--
--         , responseUpdateHealthCheck $
--             newUpdateHealthCheckResponse
--
--         , responseUpdateHostedZoneComment $
--             newUpdateHostedZoneCommentResponse
--
--         , responseUpdateTrafficPolicyComment $
--             newUpdateTrafficPolicyCommentResponse
--
--         , responseUpdateTrafficPolicyInstance $
--             newUpdateTrafficPolicyInstanceResponse
--
--           ]
--     ]

-- Requests

requestActivateKeySigningKey :: ActivateKeySigningKey -> TestTree
requestActivateKeySigningKey =
  req
    "ActivateKeySigningKey"
    "fixture/ActivateKeySigningKey.yaml"

requestAssociateVPCWithHostedZone :: AssociateVPCWithHostedZone -> TestTree
requestAssociateVPCWithHostedZone =
  req
    "AssociateVPCWithHostedZone"
    "fixture/AssociateVPCWithHostedZone.yaml"

requestChangeCidrCollection :: ChangeCidrCollection -> TestTree
requestChangeCidrCollection =
  req
    "ChangeCidrCollection"
    "fixture/ChangeCidrCollection.yaml"

requestChangeResourceRecordSets :: ChangeResourceRecordSets -> TestTree
requestChangeResourceRecordSets =
  req
    "ChangeResourceRecordSets"
    "fixture/ChangeResourceRecordSets.yaml"

requestChangeTagsForResource :: ChangeTagsForResource -> TestTree
requestChangeTagsForResource =
  req
    "ChangeTagsForResource"
    "fixture/ChangeTagsForResource.yaml"

requestCreateCidrCollection :: CreateCidrCollection -> TestTree
requestCreateCidrCollection =
  req
    "CreateCidrCollection"
    "fixture/CreateCidrCollection.yaml"

requestCreateHealthCheck :: CreateHealthCheck -> TestTree
requestCreateHealthCheck =
  req
    "CreateHealthCheck"
    "fixture/CreateHealthCheck.yaml"

requestCreateHostedZone :: CreateHostedZone -> TestTree
requestCreateHostedZone =
  req
    "CreateHostedZone"
    "fixture/CreateHostedZone.yaml"

requestCreateKeySigningKey :: CreateKeySigningKey -> TestTree
requestCreateKeySigningKey =
  req
    "CreateKeySigningKey"
    "fixture/CreateKeySigningKey.yaml"

requestCreateQueryLoggingConfig :: CreateQueryLoggingConfig -> TestTree
requestCreateQueryLoggingConfig =
  req
    "CreateQueryLoggingConfig"
    "fixture/CreateQueryLoggingConfig.yaml"

requestCreateReusableDelegationSet :: CreateReusableDelegationSet -> TestTree
requestCreateReusableDelegationSet =
  req
    "CreateReusableDelegationSet"
    "fixture/CreateReusableDelegationSet.yaml"

requestCreateTrafficPolicy :: CreateTrafficPolicy -> TestTree
requestCreateTrafficPolicy =
  req
    "CreateTrafficPolicy"
    "fixture/CreateTrafficPolicy.yaml"

requestCreateTrafficPolicyInstance :: CreateTrafficPolicyInstance -> TestTree
requestCreateTrafficPolicyInstance =
  req
    "CreateTrafficPolicyInstance"
    "fixture/CreateTrafficPolicyInstance.yaml"

requestCreateTrafficPolicyVersion :: CreateTrafficPolicyVersion -> TestTree
requestCreateTrafficPolicyVersion =
  req
    "CreateTrafficPolicyVersion"
    "fixture/CreateTrafficPolicyVersion.yaml"

requestCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorization -> TestTree
requestCreateVPCAssociationAuthorization =
  req
    "CreateVPCAssociationAuthorization"
    "fixture/CreateVPCAssociationAuthorization.yaml"

requestDeactivateKeySigningKey :: DeactivateKeySigningKey -> TestTree
requestDeactivateKeySigningKey =
  req
    "DeactivateKeySigningKey"
    "fixture/DeactivateKeySigningKey.yaml"

requestDeleteCidrCollection :: DeleteCidrCollection -> TestTree
requestDeleteCidrCollection =
  req
    "DeleteCidrCollection"
    "fixture/DeleteCidrCollection.yaml"

requestDeleteHealthCheck :: DeleteHealthCheck -> TestTree
requestDeleteHealthCheck =
  req
    "DeleteHealthCheck"
    "fixture/DeleteHealthCheck.yaml"

requestDeleteHostedZone :: DeleteHostedZone -> TestTree
requestDeleteHostedZone =
  req
    "DeleteHostedZone"
    "fixture/DeleteHostedZone.yaml"

requestDeleteKeySigningKey :: DeleteKeySigningKey -> TestTree
requestDeleteKeySigningKey =
  req
    "DeleteKeySigningKey"
    "fixture/DeleteKeySigningKey.yaml"

requestDeleteQueryLoggingConfig :: DeleteQueryLoggingConfig -> TestTree
requestDeleteQueryLoggingConfig =
  req
    "DeleteQueryLoggingConfig"
    "fixture/DeleteQueryLoggingConfig.yaml"

requestDeleteReusableDelegationSet :: DeleteReusableDelegationSet -> TestTree
requestDeleteReusableDelegationSet =
  req
    "DeleteReusableDelegationSet"
    "fixture/DeleteReusableDelegationSet.yaml"

requestDeleteTrafficPolicy :: DeleteTrafficPolicy -> TestTree
requestDeleteTrafficPolicy =
  req
    "DeleteTrafficPolicy"
    "fixture/DeleteTrafficPolicy.yaml"

requestDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstance -> TestTree
requestDeleteTrafficPolicyInstance =
  req
    "DeleteTrafficPolicyInstance"
    "fixture/DeleteTrafficPolicyInstance.yaml"

requestDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorization -> TestTree
requestDeleteVPCAssociationAuthorization =
  req
    "DeleteVPCAssociationAuthorization"
    "fixture/DeleteVPCAssociationAuthorization.yaml"

requestDisableHostedZoneDNSSEC :: DisableHostedZoneDNSSEC -> TestTree
requestDisableHostedZoneDNSSEC =
  req
    "DisableHostedZoneDNSSEC"
    "fixture/DisableHostedZoneDNSSEC.yaml"

requestDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZone -> TestTree
requestDisassociateVPCFromHostedZone =
  req
    "DisassociateVPCFromHostedZone"
    "fixture/DisassociateVPCFromHostedZone.yaml"

requestEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSEC -> TestTree
requestEnableHostedZoneDNSSEC =
  req
    "EnableHostedZoneDNSSEC"
    "fixture/EnableHostedZoneDNSSEC.yaml"

requestGetAccountLimit :: GetAccountLimit -> TestTree
requestGetAccountLimit =
  req
    "GetAccountLimit"
    "fixture/GetAccountLimit.yaml"

requestGetChange :: GetChange -> TestTree
requestGetChange =
  req
    "GetChange"
    "fixture/GetChange.yaml"

requestGetCheckerIpRanges :: GetCheckerIpRanges -> TestTree
requestGetCheckerIpRanges =
  req
    "GetCheckerIpRanges"
    "fixture/GetCheckerIpRanges.yaml"

requestGetDNSSEC :: GetDNSSEC -> TestTree
requestGetDNSSEC =
  req
    "GetDNSSEC"
    "fixture/GetDNSSEC.yaml"

requestGetGeoLocation :: GetGeoLocation -> TestTree
requestGetGeoLocation =
  req
    "GetGeoLocation"
    "fixture/GetGeoLocation.yaml"

requestGetHealthCheck :: GetHealthCheck -> TestTree
requestGetHealthCheck =
  req
    "GetHealthCheck"
    "fixture/GetHealthCheck.yaml"

requestGetHealthCheckCount :: GetHealthCheckCount -> TestTree
requestGetHealthCheckCount =
  req
    "GetHealthCheckCount"
    "fixture/GetHealthCheckCount.yaml"

requestGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReason -> TestTree
requestGetHealthCheckLastFailureReason =
  req
    "GetHealthCheckLastFailureReason"
    "fixture/GetHealthCheckLastFailureReason.yaml"

requestGetHealthCheckStatus :: GetHealthCheckStatus -> TestTree
requestGetHealthCheckStatus =
  req
    "GetHealthCheckStatus"
    "fixture/GetHealthCheckStatus.yaml"

requestGetHostedZone :: GetHostedZone -> TestTree
requestGetHostedZone =
  req
    "GetHostedZone"
    "fixture/GetHostedZone.yaml"

requestGetHostedZoneCount :: GetHostedZoneCount -> TestTree
requestGetHostedZoneCount =
  req
    "GetHostedZoneCount"
    "fixture/GetHostedZoneCount.yaml"

requestGetHostedZoneLimit :: GetHostedZoneLimit -> TestTree
requestGetHostedZoneLimit =
  req
    "GetHostedZoneLimit"
    "fixture/GetHostedZoneLimit.yaml"

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

requestGetReusableDelegationSetLimit :: GetReusableDelegationSetLimit -> TestTree
requestGetReusableDelegationSetLimit =
  req
    "GetReusableDelegationSetLimit"
    "fixture/GetReusableDelegationSetLimit.yaml"

requestGetTrafficPolicy :: GetTrafficPolicy -> TestTree
requestGetTrafficPolicy =
  req
    "GetTrafficPolicy"
    "fixture/GetTrafficPolicy.yaml"

requestGetTrafficPolicyInstance :: GetTrafficPolicyInstance -> TestTree
requestGetTrafficPolicyInstance =
  req
    "GetTrafficPolicyInstance"
    "fixture/GetTrafficPolicyInstance.yaml"

requestGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCount -> TestTree
requestGetTrafficPolicyInstanceCount =
  req
    "GetTrafficPolicyInstanceCount"
    "fixture/GetTrafficPolicyInstanceCount.yaml"

requestListCidrBlocks :: ListCidrBlocks -> TestTree
requestListCidrBlocks =
  req
    "ListCidrBlocks"
    "fixture/ListCidrBlocks.yaml"

requestListCidrCollections :: ListCidrCollections -> TestTree
requestListCidrCollections =
  req
    "ListCidrCollections"
    "fixture/ListCidrCollections.yaml"

requestListCidrLocations :: ListCidrLocations -> TestTree
requestListCidrLocations =
  req
    "ListCidrLocations"
    "fixture/ListCidrLocations.yaml"

requestListGeoLocations :: ListGeoLocations -> TestTree
requestListGeoLocations =
  req
    "ListGeoLocations"
    "fixture/ListGeoLocations.yaml"

requestListHealthChecks :: ListHealthChecks -> TestTree
requestListHealthChecks =
  req
    "ListHealthChecks"
    "fixture/ListHealthChecks.yaml"

requestListHostedZones :: ListHostedZones -> TestTree
requestListHostedZones =
  req
    "ListHostedZones"
    "fixture/ListHostedZones.yaml"

requestListHostedZonesByName :: ListHostedZonesByName -> TestTree
requestListHostedZonesByName =
  req
    "ListHostedZonesByName"
    "fixture/ListHostedZonesByName.yaml"

requestListHostedZonesByVPC :: ListHostedZonesByVPC -> TestTree
requestListHostedZonesByVPC =
  req
    "ListHostedZonesByVPC"
    "fixture/ListHostedZonesByVPC.yaml"

requestListQueryLoggingConfigs :: ListQueryLoggingConfigs -> TestTree
requestListQueryLoggingConfigs =
  req
    "ListQueryLoggingConfigs"
    "fixture/ListQueryLoggingConfigs.yaml"

requestListResourceRecordSets :: ListResourceRecordSets -> TestTree
requestListResourceRecordSets =
  req
    "ListResourceRecordSets"
    "fixture/ListResourceRecordSets.yaml"

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

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources =
  req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

requestListTrafficPolicies :: ListTrafficPolicies -> TestTree
requestListTrafficPolicies =
  req
    "ListTrafficPolicies"
    "fixture/ListTrafficPolicies.yaml"

requestListTrafficPolicyInstances :: ListTrafficPolicyInstances -> TestTree
requestListTrafficPolicyInstances =
  req
    "ListTrafficPolicyInstances"
    "fixture/ListTrafficPolicyInstances.yaml"

requestListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZone -> TestTree
requestListTrafficPolicyInstancesByHostedZone =
  req
    "ListTrafficPolicyInstancesByHostedZone"
    "fixture/ListTrafficPolicyInstancesByHostedZone.yaml"

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

requestListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizations -> TestTree
requestListVPCAssociationAuthorizations =
  req
    "ListVPCAssociationAuthorizations"
    "fixture/ListVPCAssociationAuthorizations.yaml"

requestTestDNSAnswer :: TestDNSAnswer -> TestTree
requestTestDNSAnswer =
  req
    "TestDNSAnswer"
    "fixture/TestDNSAnswer.yaml"

requestUpdateHealthCheck :: UpdateHealthCheck -> TestTree
requestUpdateHealthCheck =
  req
    "UpdateHealthCheck"
    "fixture/UpdateHealthCheck.yaml"

requestUpdateHostedZoneComment :: UpdateHostedZoneComment -> TestTree
requestUpdateHostedZoneComment =
  req
    "UpdateHostedZoneComment"
    "fixture/UpdateHostedZoneComment.yaml"

requestUpdateTrafficPolicyComment :: UpdateTrafficPolicyComment -> TestTree
requestUpdateTrafficPolicyComment =
  req
    "UpdateTrafficPolicyComment"
    "fixture/UpdateTrafficPolicyComment.yaml"

requestUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstance -> TestTree
requestUpdateTrafficPolicyInstance =
  req
    "UpdateTrafficPolicyInstance"
    "fixture/UpdateTrafficPolicyInstance.yaml"

-- Responses

responseActivateKeySigningKey :: ActivateKeySigningKeyResponse -> TestTree
responseActivateKeySigningKey =
  res
    "ActivateKeySigningKeyResponse"
    "fixture/ActivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateKeySigningKey)

responseAssociateVPCWithHostedZone :: AssociateVPCWithHostedZoneResponse -> TestTree
responseAssociateVPCWithHostedZone =
  res
    "AssociateVPCWithHostedZoneResponse"
    "fixture/AssociateVPCWithHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateVPCWithHostedZone)

responseChangeCidrCollection :: ChangeCidrCollectionResponse -> TestTree
responseChangeCidrCollection =
  res
    "ChangeCidrCollectionResponse"
    "fixture/ChangeCidrCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeCidrCollection)

responseChangeResourceRecordSets :: ChangeResourceRecordSetsResponse -> TestTree
responseChangeResourceRecordSets =
  res
    "ChangeResourceRecordSetsResponse"
    "fixture/ChangeResourceRecordSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeResourceRecordSets)

responseChangeTagsForResource :: ChangeTagsForResourceResponse -> TestTree
responseChangeTagsForResource =
  res
    "ChangeTagsForResourceResponse"
    "fixture/ChangeTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ChangeTagsForResource)

responseCreateCidrCollection :: CreateCidrCollectionResponse -> TestTree
responseCreateCidrCollection =
  res
    "CreateCidrCollectionResponse"
    "fixture/CreateCidrCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCidrCollection)

responseCreateHealthCheck :: CreateHealthCheckResponse -> TestTree
responseCreateHealthCheck =
  res
    "CreateHealthCheckResponse"
    "fixture/CreateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHealthCheck)

responseCreateHostedZone :: CreateHostedZoneResponse -> TestTree
responseCreateHostedZone =
  res
    "CreateHostedZoneResponse"
    "fixture/CreateHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateHostedZone)

responseCreateKeySigningKey :: CreateKeySigningKeyResponse -> TestTree
responseCreateKeySigningKey =
  res
    "CreateKeySigningKeyResponse"
    "fixture/CreateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeySigningKey)

responseCreateQueryLoggingConfig :: CreateQueryLoggingConfigResponse -> TestTree
responseCreateQueryLoggingConfig =
  res
    "CreateQueryLoggingConfigResponse"
    "fixture/CreateQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateQueryLoggingConfig)

responseCreateReusableDelegationSet :: CreateReusableDelegationSetResponse -> TestTree
responseCreateReusableDelegationSet =
  res
    "CreateReusableDelegationSetResponse"
    "fixture/CreateReusableDelegationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReusableDelegationSet)

responseCreateTrafficPolicy :: CreateTrafficPolicyResponse -> TestTree
responseCreateTrafficPolicy =
  res
    "CreateTrafficPolicyResponse"
    "fixture/CreateTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicy)

responseCreateTrafficPolicyInstance :: CreateTrafficPolicyInstanceResponse -> TestTree
responseCreateTrafficPolicyInstance =
  res
    "CreateTrafficPolicyInstanceResponse"
    "fixture/CreateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicyInstance)

responseCreateTrafficPolicyVersion :: CreateTrafficPolicyVersionResponse -> TestTree
responseCreateTrafficPolicyVersion =
  res
    "CreateTrafficPolicyVersionResponse"
    "fixture/CreateTrafficPolicyVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTrafficPolicyVersion)

responseCreateVPCAssociationAuthorization :: CreateVPCAssociationAuthorizationResponse -> TestTree
responseCreateVPCAssociationAuthorization =
  res
    "CreateVPCAssociationAuthorizationResponse"
    "fixture/CreateVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateVPCAssociationAuthorization)

responseDeactivateKeySigningKey :: DeactivateKeySigningKeyResponse -> TestTree
responseDeactivateKeySigningKey =
  res
    "DeactivateKeySigningKeyResponse"
    "fixture/DeactivateKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateKeySigningKey)

responseDeleteCidrCollection :: DeleteCidrCollectionResponse -> TestTree
responseDeleteCidrCollection =
  res
    "DeleteCidrCollectionResponse"
    "fixture/DeleteCidrCollectionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCidrCollection)

responseDeleteHealthCheck :: DeleteHealthCheckResponse -> TestTree
responseDeleteHealthCheck =
  res
    "DeleteHealthCheckResponse"
    "fixture/DeleteHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHealthCheck)

responseDeleteHostedZone :: DeleteHostedZoneResponse -> TestTree
responseDeleteHostedZone =
  res
    "DeleteHostedZoneResponse"
    "fixture/DeleteHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteHostedZone)

responseDeleteKeySigningKey :: DeleteKeySigningKeyResponse -> TestTree
responseDeleteKeySigningKey =
  res
    "DeleteKeySigningKeyResponse"
    "fixture/DeleteKeySigningKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeySigningKey)

responseDeleteQueryLoggingConfig :: DeleteQueryLoggingConfigResponse -> TestTree
responseDeleteQueryLoggingConfig =
  res
    "DeleteQueryLoggingConfigResponse"
    "fixture/DeleteQueryLoggingConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteQueryLoggingConfig)

responseDeleteReusableDelegationSet :: DeleteReusableDelegationSetResponse -> TestTree
responseDeleteReusableDelegationSet =
  res
    "DeleteReusableDelegationSetResponse"
    "fixture/DeleteReusableDelegationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReusableDelegationSet)

responseDeleteTrafficPolicy :: DeleteTrafficPolicyResponse -> TestTree
responseDeleteTrafficPolicy =
  res
    "DeleteTrafficPolicyResponse"
    "fixture/DeleteTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficPolicy)

responseDeleteTrafficPolicyInstance :: DeleteTrafficPolicyInstanceResponse -> TestTree
responseDeleteTrafficPolicyInstance =
  res
    "DeleteTrafficPolicyInstanceResponse"
    "fixture/DeleteTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTrafficPolicyInstance)

responseDeleteVPCAssociationAuthorization :: DeleteVPCAssociationAuthorizationResponse -> TestTree
responseDeleteVPCAssociationAuthorization =
  res
    "DeleteVPCAssociationAuthorizationResponse"
    "fixture/DeleteVPCAssociationAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVPCAssociationAuthorization)

responseDisableHostedZoneDNSSEC :: DisableHostedZoneDNSSECResponse -> TestTree
responseDisableHostedZoneDNSSEC =
  res
    "DisableHostedZoneDNSSECResponse"
    "fixture/DisableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableHostedZoneDNSSEC)

responseDisassociateVPCFromHostedZone :: DisassociateVPCFromHostedZoneResponse -> TestTree
responseDisassociateVPCFromHostedZone =
  res
    "DisassociateVPCFromHostedZoneResponse"
    "fixture/DisassociateVPCFromHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateVPCFromHostedZone)

responseEnableHostedZoneDNSSEC :: EnableHostedZoneDNSSECResponse -> TestTree
responseEnableHostedZoneDNSSEC =
  res
    "EnableHostedZoneDNSSECResponse"
    "fixture/EnableHostedZoneDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableHostedZoneDNSSEC)

responseGetAccountLimit :: GetAccountLimitResponse -> TestTree
responseGetAccountLimit =
  res
    "GetAccountLimitResponse"
    "fixture/GetAccountLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccountLimit)

responseGetChange :: GetChangeResponse -> TestTree
responseGetChange =
  res
    "GetChangeResponse"
    "fixture/GetChangeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetChange)

responseGetCheckerIpRanges :: GetCheckerIpRangesResponse -> TestTree
responseGetCheckerIpRanges =
  res
    "GetCheckerIpRangesResponse"
    "fixture/GetCheckerIpRangesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCheckerIpRanges)

responseGetDNSSEC :: GetDNSSECResponse -> TestTree
responseGetDNSSEC =
  res
    "GetDNSSECResponse"
    "fixture/GetDNSSECResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDNSSEC)

responseGetGeoLocation :: GetGeoLocationResponse -> TestTree
responseGetGeoLocation =
  res
    "GetGeoLocationResponse"
    "fixture/GetGeoLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeoLocation)

responseGetHealthCheck :: GetHealthCheckResponse -> TestTree
responseGetHealthCheck =
  res
    "GetHealthCheckResponse"
    "fixture/GetHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheck)

responseGetHealthCheckCount :: GetHealthCheckCountResponse -> TestTree
responseGetHealthCheckCount =
  res
    "GetHealthCheckCountResponse"
    "fixture/GetHealthCheckCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckCount)

responseGetHealthCheckLastFailureReason :: GetHealthCheckLastFailureReasonResponse -> TestTree
responseGetHealthCheckLastFailureReason =
  res
    "GetHealthCheckLastFailureReasonResponse"
    "fixture/GetHealthCheckLastFailureReasonResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckLastFailureReason)

responseGetHealthCheckStatus :: GetHealthCheckStatusResponse -> TestTree
responseGetHealthCheckStatus =
  res
    "GetHealthCheckStatusResponse"
    "fixture/GetHealthCheckStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHealthCheckStatus)

responseGetHostedZone :: GetHostedZoneResponse -> TestTree
responseGetHostedZone =
  res
    "GetHostedZoneResponse"
    "fixture/GetHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZone)

responseGetHostedZoneCount :: GetHostedZoneCountResponse -> TestTree
responseGetHostedZoneCount =
  res
    "GetHostedZoneCountResponse"
    "fixture/GetHostedZoneCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZoneCount)

responseGetHostedZoneLimit :: GetHostedZoneLimitResponse -> TestTree
responseGetHostedZoneLimit =
  res
    "GetHostedZoneLimitResponse"
    "fixture/GetHostedZoneLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetHostedZoneLimit)

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

responseGetReusableDelegationSetLimit :: GetReusableDelegationSetLimitResponse -> TestTree
responseGetReusableDelegationSetLimit =
  res
    "GetReusableDelegationSetLimitResponse"
    "fixture/GetReusableDelegationSetLimitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReusableDelegationSetLimit)

responseGetTrafficPolicy :: GetTrafficPolicyResponse -> TestTree
responseGetTrafficPolicy =
  res
    "GetTrafficPolicyResponse"
    "fixture/GetTrafficPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicy)

responseGetTrafficPolicyInstance :: GetTrafficPolicyInstanceResponse -> TestTree
responseGetTrafficPolicyInstance =
  res
    "GetTrafficPolicyInstanceResponse"
    "fixture/GetTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicyInstance)

responseGetTrafficPolicyInstanceCount :: GetTrafficPolicyInstanceCountResponse -> TestTree
responseGetTrafficPolicyInstanceCount =
  res
    "GetTrafficPolicyInstanceCountResponse"
    "fixture/GetTrafficPolicyInstanceCountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTrafficPolicyInstanceCount)

responseListCidrBlocks :: ListCidrBlocksResponse -> TestTree
responseListCidrBlocks =
  res
    "ListCidrBlocksResponse"
    "fixture/ListCidrBlocksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCidrBlocks)

responseListCidrCollections :: ListCidrCollectionsResponse -> TestTree
responseListCidrCollections =
  res
    "ListCidrCollectionsResponse"
    "fixture/ListCidrCollectionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCidrCollections)

responseListCidrLocations :: ListCidrLocationsResponse -> TestTree
responseListCidrLocations =
  res
    "ListCidrLocationsResponse"
    "fixture/ListCidrLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCidrLocations)

responseListGeoLocations :: ListGeoLocationsResponse -> TestTree
responseListGeoLocations =
  res
    "ListGeoLocationsResponse"
    "fixture/ListGeoLocationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeoLocations)

responseListHealthChecks :: ListHealthChecksResponse -> TestTree
responseListHealthChecks =
  res
    "ListHealthChecksResponse"
    "fixture/ListHealthChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHealthChecks)

responseListHostedZones :: ListHostedZonesResponse -> TestTree
responseListHostedZones =
  res
    "ListHostedZonesResponse"
    "fixture/ListHostedZonesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZones)

responseListHostedZonesByName :: ListHostedZonesByNameResponse -> TestTree
responseListHostedZonesByName =
  res
    "ListHostedZonesByNameResponse"
    "fixture/ListHostedZonesByNameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZonesByName)

responseListHostedZonesByVPC :: ListHostedZonesByVPCResponse -> TestTree
responseListHostedZonesByVPC =
  res
    "ListHostedZonesByVPCResponse"
    "fixture/ListHostedZonesByVPCResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHostedZonesByVPC)

responseListQueryLoggingConfigs :: ListQueryLoggingConfigsResponse -> TestTree
responseListQueryLoggingConfigs =
  res
    "ListQueryLoggingConfigsResponse"
    "fixture/ListQueryLoggingConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListQueryLoggingConfigs)

responseListResourceRecordSets :: ListResourceRecordSetsResponse -> TestTree
responseListResourceRecordSets =
  res
    "ListResourceRecordSetsResponse"
    "fixture/ListResourceRecordSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceRecordSets)

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

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResources)

responseListTrafficPolicies :: ListTrafficPoliciesResponse -> TestTree
responseListTrafficPolicies =
  res
    "ListTrafficPoliciesResponse"
    "fixture/ListTrafficPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicies)

responseListTrafficPolicyInstances :: ListTrafficPolicyInstancesResponse -> TestTree
responseListTrafficPolicyInstances =
  res
    "ListTrafficPolicyInstancesResponse"
    "fixture/ListTrafficPolicyInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstances)

responseListTrafficPolicyInstancesByHostedZone :: ListTrafficPolicyInstancesByHostedZoneResponse -> TestTree
responseListTrafficPolicyInstancesByHostedZone =
  res
    "ListTrafficPolicyInstancesByHostedZoneResponse"
    "fixture/ListTrafficPolicyInstancesByHostedZoneResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstancesByHostedZone)

responseListTrafficPolicyInstancesByPolicy :: ListTrafficPolicyInstancesByPolicyResponse -> TestTree
responseListTrafficPolicyInstancesByPolicy =
  res
    "ListTrafficPolicyInstancesByPolicyResponse"
    "fixture/ListTrafficPolicyInstancesByPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyInstancesByPolicy)

responseListTrafficPolicyVersions :: ListTrafficPolicyVersionsResponse -> TestTree
responseListTrafficPolicyVersions =
  res
    "ListTrafficPolicyVersionsResponse"
    "fixture/ListTrafficPolicyVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTrafficPolicyVersions)

responseListVPCAssociationAuthorizations :: ListVPCAssociationAuthorizationsResponse -> TestTree
responseListVPCAssociationAuthorizations =
  res
    "ListVPCAssociationAuthorizationsResponse"
    "fixture/ListVPCAssociationAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListVPCAssociationAuthorizations)

responseTestDNSAnswer :: TestDNSAnswerResponse -> TestTree
responseTestDNSAnswer =
  res
    "TestDNSAnswerResponse"
    "fixture/TestDNSAnswerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestDNSAnswer)

responseUpdateHealthCheck :: UpdateHealthCheckResponse -> TestTree
responseUpdateHealthCheck =
  res
    "UpdateHealthCheckResponse"
    "fixture/UpdateHealthCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHealthCheck)

responseUpdateHostedZoneComment :: UpdateHostedZoneCommentResponse -> TestTree
responseUpdateHostedZoneComment =
  res
    "UpdateHostedZoneCommentResponse"
    "fixture/UpdateHostedZoneCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateHostedZoneComment)

responseUpdateTrafficPolicyComment :: UpdateTrafficPolicyCommentResponse -> TestTree
responseUpdateTrafficPolicyComment =
  res
    "UpdateTrafficPolicyCommentResponse"
    "fixture/UpdateTrafficPolicyCommentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrafficPolicyComment)

responseUpdateTrafficPolicyInstance :: UpdateTrafficPolicyInstanceResponse -> TestTree
responseUpdateTrafficPolicyInstance =
  res
    "UpdateTrafficPolicyInstanceResponse"
    "fixture/UpdateTrafficPolicyInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTrafficPolicyInstance)
