{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lightsail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Lightsail where

import Data.Proxy
import Network.AWS.Lightsail
import Test.AWS.Fixture
import Test.AWS.Lightsail.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetContainerServices $
--             newGetContainerServices
--
--         , requestGetStaticIp $
--             newGetStaticIp
--
--         , requestGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshots
--
--         , requestGetDistributions $
--             newGetDistributions
--
--         , requestGetDiskSnapshot $
--             newGetDiskSnapshot
--
--         , requestCreateContainerServiceDeployment $
--             newCreateContainerServiceDeployment
--
--         , requestPeerVpc $
--             newPeerVpc
--
--         , requestUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttribute
--
--         , requestUpdateDistributionBundle $
--             newUpdateDistributionBundle
--
--         , requestAllocateStaticIp $
--             newAllocateStaticIp
--
--         , requestCloseInstancePublicPorts $
--             newCloseInstancePublicPorts
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDisableAddOn $
--             newDisableAddOn
--
--         , requestGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecords
--
--         , requestIsVpcPeered $
--             newIsVpcPeered
--
--         , requestGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshot
--
--         , requestGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprints
--
--         , requestDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshot
--
--         , requestUnpeerVpc $
--             newUnpeerVpc
--
--         , requestGetContainerAPIMetadata $
--             newGetContainerAPIMetadata
--
--         , requestGetInstances $
--             newGetInstances
--
--         , requestCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshot
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestGetInstanceAccessDetails $
--             newGetInstanceAccessDetails
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestCreateInstanceSnapshot $
--             newCreateInstanceSnapshot
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestRegisterContainerImage $
--             newRegisterContainerImage
--
--         , requestDetachCertificateFromDistribution $
--             newDetachCertificateFromDistribution
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestGetContainerServiceDeployments $
--             newGetContainerServiceDeployments
--
--         , requestDeleteContactMethod $
--             newDeleteContactMethod
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestDetachStaticIp $
--             newDetachStaticIp
--
--         , requestAttachDisk $
--             newAttachDisk
--
--         , requestGetDisk $
--             newGetDisk
--
--         , requestGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEvents
--
--         , requestGetRelationalDatabases $
--             newGetRelationalDatabases
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshot
--
--         , requestGetInstanceMetricData $
--             newGetInstanceMetricData
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestExportSnapshot $
--             newExportSnapshot
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshot
--
--         , requestGetOperations $
--             newGetOperations
--
--         , requestGetExportSnapshotRecords $
--             newGetExportSnapshotRecords
--
--         , requestGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricData
--
--         , requestGetInstanceSnapshots $
--             newGetInstanceSnapshots
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificate
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetRelationalDatabase $
--             newGetRelationalDatabase
--
--         , requestGetKeyPairs $
--             newGetKeyPairs
--
--         , requestAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancer
--
--         , requestGetRegions $
--             newGetRegions
--
--         , requestTestAlarm $
--             newTestAlarm
--
--         , requestCreateDiskSnapshot $
--             newCreateDiskSnapshot
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestDeleteAlarm $
--             newDeleteAlarm
--
--         , requestSendContactMethodVerification $
--             newSendContactMethodVerification
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPassword
--
--         , requestGetBlueprints $
--             newGetBlueprints
--
--         , requestDetachDisk $
--             newDetachDisk
--
--         , requestGetInstancePortStates $
--             newGetInstancePortStates
--
--         , requestAttachStaticIp $
--             newAttachStaticIp
--
--         , requestDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPair
--
--         , requestGetLoadBalancers $
--             newGetLoadBalancers
--
--         , requestUpdateRelationalDatabase $
--             newUpdateRelationalDatabase
--
--         , requestGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundles
--
--         , requestAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificate
--
--         , requestAttachCertificateToDistribution $
--             newAttachCertificateToDistribution
--
--         , requestDeleteRelationalDatabase $
--             newDeleteRelationalDatabase
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestRebootRelationalDatabase $
--             newRebootRelationalDatabase
--
--         , requestGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEvents
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestGetStaticIps $
--             newGetStaticIps
--
--         , requestDeleteDisk $
--             newDeleteDisk
--
--         , requestGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricData
--
--         , requestGetDiskSnapshots $
--             newGetDiskSnapshots
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestGetLoadBalancer $
--             newGetLoadBalancer
--
--         , requestGetBundles $
--             newGetBundles
--
--         , requestCreateCertificate $
--             newCreateCertificate
--
--         , requestDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancer
--
--         , requestGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificates
--
--         , requestDeleteContainerImage $
--             newDeleteContainerImage
--
--         , requestGetOperationsForResource $
--             newGetOperationsForResource
--
--         , requestCreateDisk $
--             newCreateDisk
--
--         , requestEnableAddOn $
--             newEnableAddOn
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestStartRelationalDatabase $
--             newStartRelationalDatabase
--
--         , requestCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshot
--
--         , requestGetAlarms $
--             newGetAlarms
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestCreateInstances $
--             newCreateInstances
--
--         , requestCreateContainerService $
--             newCreateContainerService
--
--         , requestGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheReset
--
--         , requestStopRelationalDatabase $
--             newStopRelationalDatabase
--
--         , requestDeleteKnownHostKeys $
--             newDeleteKnownHostKeys
--
--         , requestOpenInstancePublicPorts $
--             newOpenInstancePublicPorts
--
--         , requestGetActiveNames $
--             newGetActiveNames
--
--         , requestGetAutoSnapshots $
--             newGetAutoSnapshots
--
--         , requestGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreams
--
--         , requestGetDistributionBundles $
--             newGetDistributionBundles
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshot
--
--         , requestGetInstanceSnapshot $
--             newGetInstanceSnapshot
--
--         , requestDeleteContainerService $
--             newDeleteContainerService
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestPutInstancePublicPorts $
--             newPutInstancePublicPorts
--
--         , requestResetDistributionCache $
--             newResetDistributionCache
--
--         , requestCreateContactMethod $
--             newCreateContactMethod
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestUpdateContainerService $
--             newUpdateContainerService
--
--         , requestGetKeyPair $
--             newGetKeyPair
--
--         , requestCreateCloudFormationStack $
--             newCreateCloudFormationStack
--
--         , requestCreateDomainEntry $
--             newCreateDomainEntry
--
--         , requestGetInstanceState $
--             newGetInstanceState
--
--         , requestGetDistributionMetricData $
--             newGetDistributionMetricData
--
--         , requestGetDisks $
--             newGetDisks
--
--         , requestGetContainerServiceMetricData $
--             newGetContainerServiceMetricData
--
--         , requestCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLogin
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestGetContainerServicePowers $
--             newGetContainerServicePowers
--
--         , requestDeleteDiskSnapshot $
--             newDeleteDiskSnapshot
--
--         , requestGetCertificates $
--             newGetCertificates
--
--         , requestReleaseStaticIp $
--             newReleaseStaticIp
--
--         , requestUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParameters
--
--         , requestDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificate
--
--         , requestUpdateDomainEntry $
--             newUpdateDomainEntry
--
--         , requestGetContainerLog $
--             newGetContainerLog
--
--         , requestDeleteDomainEntry $
--             newDeleteDomainEntry
--
--         , requestGetContainerImages $
--             newGetContainerImages
--
--         , requestGetDomains $
--             newGetDomains
--
--         , requestPutAlarm $
--             newPutAlarm
--
--         , requestDeleteAutoSnapshot $
--             newDeleteAutoSnapshot
--
--         , requestGetContactMethods $
--             newGetContactMethods
--
--         , requestGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParameters
--
--         , requestCreateRelationalDatabase $
--             newCreateRelationalDatabase
--
--           ]

--     , testGroup "response"
--         [ responseGetContainerServices $
--             newGetContainerServicesResponse
--
--         , responseGetStaticIp $
--             newGetStaticIpResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshotsResponse
--
--         , responseGetDistributions $
--             newGetDistributionsResponse
--
--         , responseGetDiskSnapshot $
--             newGetDiskSnapshotResponse
--
--         , responseCreateContainerServiceDeployment $
--             newCreateContainerServiceDeploymentResponse
--
--         , responsePeerVpc $
--             newPeerVpcResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttributeResponse
--
--         , responseUpdateDistributionBundle $
--             newUpdateDistributionBundleResponse
--
--         , responseAllocateStaticIp $
--             newAllocateStaticIpResponse
--
--         , responseCloseInstancePublicPorts $
--             newCloseInstancePublicPortsResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDisableAddOn $
--             newDisableAddOnResponse
--
--         , responseGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecordsResponse
--
--         , responseIsVpcPeered $
--             newIsVpcPeeredResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshotResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprintsResponse
--
--         , responseDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshotResponse
--
--         , responseUnpeerVpc $
--             newUnpeerVpcResponse
--
--         , responseGetContainerAPIMetadata $
--             newGetContainerAPIMetadataResponse
--
--         , responseGetInstances $
--             newGetInstancesResponse
--
--         , responseCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshotResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseGetInstanceAccessDetails $
--             newGetInstanceAccessDetailsResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseCreateInstanceSnapshot $
--             newCreateInstanceSnapshotResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseRegisterContainerImage $
--             newRegisterContainerImageResponse
--
--         , responseDetachCertificateFromDistribution $
--             newDetachCertificateFromDistributionResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseGetContainerServiceDeployments $
--             newGetContainerServiceDeploymentsResponse
--
--         , responseDeleteContactMethod $
--             newDeleteContactMethodResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseDetachStaticIp $
--             newDetachStaticIpResponse
--
--         , responseAttachDisk $
--             newAttachDiskResponse
--
--         , responseGetDisk $
--             newGetDiskResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEventsResponse
--
--         , responseGetRelationalDatabases $
--             newGetRelationalDatabasesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshotResponse
--
--         , responseGetInstanceMetricData $
--             newGetInstanceMetricDataResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseExportSnapshot $
--             newExportSnapshotResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshotResponse
--
--         , responseGetOperations $
--             newGetOperationsResponse
--
--         , responseGetExportSnapshotRecords $
--             newGetExportSnapshotRecordsResponse
--
--         , responseGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricDataResponse
--
--         , responseGetInstanceSnapshots $
--             newGetInstanceSnapshotsResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificateResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetRelationalDatabase $
--             newGetRelationalDatabaseResponse
--
--         , responseGetKeyPairs $
--             newGetKeyPairsResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancerResponse
--
--         , responseGetRegions $
--             newGetRegionsResponse
--
--         , responseTestAlarm $
--             newTestAlarmResponse
--
--         , responseCreateDiskSnapshot $
--             newCreateDiskSnapshotResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseDeleteAlarm $
--             newDeleteAlarmResponse
--
--         , responseSendContactMethodVerification $
--             newSendContactMethodVerificationResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetBlueprints $
--             newGetBlueprintsResponse
--
--         , responseDetachDisk $
--             newDetachDiskResponse
--
--         , responseGetInstancePortStates $
--             newGetInstancePortStatesResponse
--
--         , responseAttachStaticIp $
--             newAttachStaticIpResponse
--
--         , responseDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPairResponse
--
--         , responseGetLoadBalancers $
--             newGetLoadBalancersResponse
--
--         , responseUpdateRelationalDatabase $
--             newUpdateRelationalDatabaseResponse
--
--         , responseGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundlesResponse
--
--         , responseAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificateResponse
--
--         , responseAttachCertificateToDistribution $
--             newAttachCertificateToDistributionResponse
--
--         , responseDeleteRelationalDatabase $
--             newDeleteRelationalDatabaseResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseRebootRelationalDatabase $
--             newRebootRelationalDatabaseResponse
--
--         , responseGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEventsResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseGetStaticIps $
--             newGetStaticIpsResponse
--
--         , responseDeleteDisk $
--             newDeleteDiskResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricDataResponse
--
--         , responseGetDiskSnapshots $
--             newGetDiskSnapshotsResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseGetLoadBalancer $
--             newGetLoadBalancerResponse
--
--         , responseGetBundles $
--             newGetBundlesResponse
--
--         , responseCreateCertificate $
--             newCreateCertificateResponse
--
--         , responseDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancerResponse
--
--         , responseGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificatesResponse
--
--         , responseDeleteContainerImage $
--             newDeleteContainerImageResponse
--
--         , responseGetOperationsForResource $
--             newGetOperationsForResourceResponse
--
--         , responseCreateDisk $
--             newCreateDiskResponse
--
--         , responseEnableAddOn $
--             newEnableAddOnResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseStartRelationalDatabase $
--             newStartRelationalDatabaseResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshotResponse
--
--         , responseGetAlarms $
--             newGetAlarmsResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseCreateInstances $
--             newCreateInstancesResponse
--
--         , responseCreateContainerService $
--             newCreateContainerServiceResponse
--
--         , responseGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheResetResponse
--
--         , responseStopRelationalDatabase $
--             newStopRelationalDatabaseResponse
--
--         , responseDeleteKnownHostKeys $
--             newDeleteKnownHostKeysResponse
--
--         , responseOpenInstancePublicPorts $
--             newOpenInstancePublicPortsResponse
--
--         , responseGetActiveNames $
--             newGetActiveNamesResponse
--
--         , responseGetAutoSnapshots $
--             newGetAutoSnapshotsResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreamsResponse
--
--         , responseGetDistributionBundles $
--             newGetDistributionBundlesResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshotResponse
--
--         , responseGetInstanceSnapshot $
--             newGetInstanceSnapshotResponse
--
--         , responseDeleteContainerService $
--             newDeleteContainerServiceResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responsePutInstancePublicPorts $
--             newPutInstancePublicPortsResponse
--
--         , responseResetDistributionCache $
--             newResetDistributionCacheResponse
--
--         , responseCreateContactMethod $
--             newCreateContactMethodResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseUpdateContainerService $
--             newUpdateContainerServiceResponse
--
--         , responseGetKeyPair $
--             newGetKeyPairResponse
--
--         , responseCreateCloudFormationStack $
--             newCreateCloudFormationStackResponse
--
--         , responseCreateDomainEntry $
--             newCreateDomainEntryResponse
--
--         , responseGetInstanceState $
--             newGetInstanceStateResponse
--
--         , responseGetDistributionMetricData $
--             newGetDistributionMetricDataResponse
--
--         , responseGetDisks $
--             newGetDisksResponse
--
--         , responseGetContainerServiceMetricData $
--             newGetContainerServiceMetricDataResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLoginResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseGetContainerServicePowers $
--             newGetContainerServicePowersResponse
--
--         , responseDeleteDiskSnapshot $
--             newDeleteDiskSnapshotResponse
--
--         , responseGetCertificates $
--             newGetCertificatesResponse
--
--         , responseReleaseStaticIp $
--             newReleaseStaticIpResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParametersResponse
--
--         , responseDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificateResponse
--
--         , responseUpdateDomainEntry $
--             newUpdateDomainEntryResponse
--
--         , responseGetContainerLog $
--             newGetContainerLogResponse
--
--         , responseDeleteDomainEntry $
--             newDeleteDomainEntryResponse
--
--         , responseGetContainerImages $
--             newGetContainerImagesResponse
--
--         , responseGetDomains $
--             newGetDomainsResponse
--
--         , responsePutAlarm $
--             newPutAlarmResponse
--
--         , responseDeleteAutoSnapshot $
--             newDeleteAutoSnapshotResponse
--
--         , responseGetContactMethods $
--             newGetContactMethodsResponse
--
--         , responseGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParametersResponse
--
--         , responseCreateRelationalDatabase $
--             newCreateRelationalDatabaseResponse
--
--           ]
--     ]

-- Requests

requestGetContainerServices :: GetContainerServices -> TestTree
requestGetContainerServices =
  req
    "GetContainerServices"
    "fixture/GetContainerServices.yaml"

requestGetStaticIp :: GetStaticIp -> TestTree
requestGetStaticIp =
  req
    "GetStaticIp"
    "fixture/GetStaticIp.yaml"

requestGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshots -> TestTree
requestGetRelationalDatabaseSnapshots =
  req
    "GetRelationalDatabaseSnapshots"
    "fixture/GetRelationalDatabaseSnapshots.yaml"

requestGetDistributions :: GetDistributions -> TestTree
requestGetDistributions =
  req
    "GetDistributions"
    "fixture/GetDistributions.yaml"

requestGetDiskSnapshot :: GetDiskSnapshot -> TestTree
requestGetDiskSnapshot =
  req
    "GetDiskSnapshot"
    "fixture/GetDiskSnapshot.yaml"

requestCreateContainerServiceDeployment :: CreateContainerServiceDeployment -> TestTree
requestCreateContainerServiceDeployment =
  req
    "CreateContainerServiceDeployment"
    "fixture/CreateContainerServiceDeployment.yaml"

requestPeerVpc :: PeerVpc -> TestTree
requestPeerVpc =
  req
    "PeerVpc"
    "fixture/PeerVpc.yaml"

requestUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttribute -> TestTree
requestUpdateLoadBalancerAttribute =
  req
    "UpdateLoadBalancerAttribute"
    "fixture/UpdateLoadBalancerAttribute.yaml"

requestUpdateDistributionBundle :: UpdateDistributionBundle -> TestTree
requestUpdateDistributionBundle =
  req
    "UpdateDistributionBundle"
    "fixture/UpdateDistributionBundle.yaml"

requestAllocateStaticIp :: AllocateStaticIp -> TestTree
requestAllocateStaticIp =
  req
    "AllocateStaticIp"
    "fixture/AllocateStaticIp.yaml"

requestCloseInstancePublicPorts :: CloseInstancePublicPorts -> TestTree
requestCloseInstancePublicPorts =
  req
    "CloseInstancePublicPorts"
    "fixture/CloseInstancePublicPorts.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDisableAddOn :: DisableAddOn -> TestTree
requestDisableAddOn =
  req
    "DisableAddOn"
    "fixture/DisableAddOn.yaml"

requestGetCloudFormationStackRecords :: GetCloudFormationStackRecords -> TestTree
requestGetCloudFormationStackRecords =
  req
    "GetCloudFormationStackRecords"
    "fixture/GetCloudFormationStackRecords.yaml"

requestIsVpcPeered :: IsVpcPeered -> TestTree
requestIsVpcPeered =
  req
    "IsVpcPeered"
    "fixture/IsVpcPeered.yaml"

requestGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshot -> TestTree
requestGetRelationalDatabaseSnapshot =
  req
    "GetRelationalDatabaseSnapshot"
    "fixture/GetRelationalDatabaseSnapshot.yaml"

requestGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprints -> TestTree
requestGetRelationalDatabaseBlueprints =
  req
    "GetRelationalDatabaseBlueprints"
    "fixture/GetRelationalDatabaseBlueprints.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot =
  req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

requestUnpeerVpc :: UnpeerVpc -> TestTree
requestUnpeerVpc =
  req
    "UnpeerVpc"
    "fixture/UnpeerVpc.yaml"

requestGetContainerAPIMetadata :: GetContainerAPIMetadata -> TestTree
requestGetContainerAPIMetadata =
  req
    "GetContainerAPIMetadata"
    "fixture/GetContainerAPIMetadata.yaml"

requestGetInstances :: GetInstances -> TestTree
requestGetInstances =
  req
    "GetInstances"
    "fixture/GetInstances.yaml"

requestCreateInstancesFromSnapshot :: CreateInstancesFromSnapshot -> TestTree
requestCreateInstancesFromSnapshot =
  req
    "CreateInstancesFromSnapshot"
    "fixture/CreateInstancesFromSnapshot.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails =
  req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot =
  req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestRegisterContainerImage :: RegisterContainerImage -> TestTree
requestRegisterContainerImage =
  req
    "RegisterContainerImage"
    "fixture/RegisterContainerImage.yaml"

requestDetachCertificateFromDistribution :: DetachCertificateFromDistribution -> TestTree
requestDetachCertificateFromDistribution =
  req
    "DetachCertificateFromDistribution"
    "fixture/DetachCertificateFromDistribution.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestGetContainerServiceDeployments :: GetContainerServiceDeployments -> TestTree
requestGetContainerServiceDeployments =
  req
    "GetContainerServiceDeployments"
    "fixture/GetContainerServiceDeployments.yaml"

requestDeleteContactMethod :: DeleteContactMethod -> TestTree
requestDeleteContactMethod =
  req
    "DeleteContactMethod"
    "fixture/DeleteContactMethod.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestDetachStaticIp :: DetachStaticIp -> TestTree
requestDetachStaticIp =
  req
    "DetachStaticIp"
    "fixture/DetachStaticIp.yaml"

requestAttachDisk :: AttachDisk -> TestTree
requestAttachDisk =
  req
    "AttachDisk"
    "fixture/AttachDisk.yaml"

requestGetDisk :: GetDisk -> TestTree
requestGetDisk =
  req
    "GetDisk"
    "fixture/GetDisk.yaml"

requestGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEvents -> TestTree
requestGetRelationalDatabaseLogEvents =
  req
    "GetRelationalDatabaseLogEvents"
    "fixture/GetRelationalDatabaseLogEvents.yaml"

requestGetRelationalDatabases :: GetRelationalDatabases -> TestTree
requestGetRelationalDatabases =
  req
    "GetRelationalDatabases"
    "fixture/GetRelationalDatabases.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateDiskFromSnapshot :: CreateDiskFromSnapshot -> TestTree
requestCreateDiskFromSnapshot =
  req
    "CreateDiskFromSnapshot"
    "fixture/CreateDiskFromSnapshot.yaml"

requestGetInstanceMetricData :: GetInstanceMetricData -> TestTree
requestGetInstanceMetricData =
  req
    "GetInstanceMetricData"
    "fixture/GetInstanceMetricData.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestExportSnapshot :: ExportSnapshot -> TestTree
requestExportSnapshot =
  req
    "ExportSnapshot"
    "fixture/ExportSnapshot.yaml"

requestCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshot -> TestTree
requestCreateRelationalDatabaseFromSnapshot =
  req
    "CreateRelationalDatabaseFromSnapshot"
    "fixture/CreateRelationalDatabaseFromSnapshot.yaml"

requestGetOperations :: GetOperations -> TestTree
requestGetOperations =
  req
    "GetOperations"
    "fixture/GetOperations.yaml"

requestGetExportSnapshotRecords :: GetExportSnapshotRecords -> TestTree
requestGetExportSnapshotRecords =
  req
    "GetExportSnapshotRecords"
    "fixture/GetExportSnapshotRecords.yaml"

requestGetLoadBalancerMetricData :: GetLoadBalancerMetricData -> TestTree
requestGetLoadBalancerMetricData =
  req
    "GetLoadBalancerMetricData"
    "fixture/GetLoadBalancerMetricData.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots =
  req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificate -> TestTree
requestCreateLoadBalancerTlsCertificate =
  req
    "CreateLoadBalancerTlsCertificate"
    "fixture/CreateLoadBalancerTlsCertificate.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetRelationalDatabase :: GetRelationalDatabase -> TestTree
requestGetRelationalDatabase =
  req
    "GetRelationalDatabase"
    "fixture/GetRelationalDatabase.yaml"

requestGetKeyPairs :: GetKeyPairs -> TestTree
requestGetKeyPairs =
  req
    "GetKeyPairs"
    "fixture/GetKeyPairs.yaml"

requestAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancer -> TestTree
requestAttachInstancesToLoadBalancer =
  req
    "AttachInstancesToLoadBalancer"
    "fixture/AttachInstancesToLoadBalancer.yaml"

requestGetRegions :: GetRegions -> TestTree
requestGetRegions =
  req
    "GetRegions"
    "fixture/GetRegions.yaml"

requestTestAlarm :: TestAlarm -> TestTree
requestTestAlarm =
  req
    "TestAlarm"
    "fixture/TestAlarm.yaml"

requestCreateDiskSnapshot :: CreateDiskSnapshot -> TestTree
requestCreateDiskSnapshot =
  req
    "CreateDiskSnapshot"
    "fixture/CreateDiskSnapshot.yaml"

requestSetIpAddressType :: SetIpAddressType -> TestTree
requestSetIpAddressType =
  req
    "SetIpAddressType"
    "fixture/SetIpAddressType.yaml"

requestDeleteAlarm :: DeleteAlarm -> TestTree
requestDeleteAlarm =
  req
    "DeleteAlarm"
    "fixture/DeleteAlarm.yaml"

requestSendContactMethodVerification :: SendContactMethodVerification -> TestTree
requestSendContactMethodVerification =
  req
    "SendContactMethodVerification"
    "fixture/SendContactMethodVerification.yaml"

requestGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPassword -> TestTree
requestGetRelationalDatabaseMasterUserPassword =
  req
    "GetRelationalDatabaseMasterUserPassword"
    "fixture/GetRelationalDatabaseMasterUserPassword.yaml"

requestGetBlueprints :: GetBlueprints -> TestTree
requestGetBlueprints =
  req
    "GetBlueprints"
    "fixture/GetBlueprints.yaml"

requestDetachDisk :: DetachDisk -> TestTree
requestDetachDisk =
  req
    "DetachDisk"
    "fixture/DetachDisk.yaml"

requestGetInstancePortStates :: GetInstancePortStates -> TestTree
requestGetInstancePortStates =
  req
    "GetInstancePortStates"
    "fixture/GetInstancePortStates.yaml"

requestAttachStaticIp :: AttachStaticIp -> TestTree
requestAttachStaticIp =
  req
    "AttachStaticIp"
    "fixture/AttachStaticIp.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair =
  req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestGetLoadBalancers :: GetLoadBalancers -> TestTree
requestGetLoadBalancers =
  req
    "GetLoadBalancers"
    "fixture/GetLoadBalancers.yaml"

requestUpdateRelationalDatabase :: UpdateRelationalDatabase -> TestTree
requestUpdateRelationalDatabase =
  req
    "UpdateRelationalDatabase"
    "fixture/UpdateRelationalDatabase.yaml"

requestGetRelationalDatabaseBundles :: GetRelationalDatabaseBundles -> TestTree
requestGetRelationalDatabaseBundles =
  req
    "GetRelationalDatabaseBundles"
    "fixture/GetRelationalDatabaseBundles.yaml"

requestAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificate -> TestTree
requestAttachLoadBalancerTlsCertificate =
  req
    "AttachLoadBalancerTlsCertificate"
    "fixture/AttachLoadBalancerTlsCertificate.yaml"

requestAttachCertificateToDistribution :: AttachCertificateToDistribution -> TestTree
requestAttachCertificateToDistribution =
  req
    "AttachCertificateToDistribution"
    "fixture/AttachCertificateToDistribution.yaml"

requestDeleteRelationalDatabase :: DeleteRelationalDatabase -> TestTree
requestDeleteRelationalDatabase =
  req
    "DeleteRelationalDatabase"
    "fixture/DeleteRelationalDatabase.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestRebootRelationalDatabase :: RebootRelationalDatabase -> TestTree
requestRebootRelationalDatabase =
  req
    "RebootRelationalDatabase"
    "fixture/RebootRelationalDatabase.yaml"

requestGetRelationalDatabaseEvents :: GetRelationalDatabaseEvents -> TestTree
requestGetRelationalDatabaseEvents =
  req
    "GetRelationalDatabaseEvents"
    "fixture/GetRelationalDatabaseEvents.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestGetStaticIps :: GetStaticIps -> TestTree
requestGetStaticIps =
  req
    "GetStaticIps"
    "fixture/GetStaticIps.yaml"

requestDeleteDisk :: DeleteDisk -> TestTree
requestDeleteDisk =
  req
    "DeleteDisk"
    "fixture/DeleteDisk.yaml"

requestGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricData -> TestTree
requestGetRelationalDatabaseMetricData =
  req
    "GetRelationalDatabaseMetricData"
    "fixture/GetRelationalDatabaseMetricData.yaml"

requestGetDiskSnapshots :: GetDiskSnapshots -> TestTree
requestGetDiskSnapshots =
  req
    "GetDiskSnapshots"
    "fixture/GetDiskSnapshots.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestGetLoadBalancer :: GetLoadBalancer -> TestTree
requestGetLoadBalancer =
  req
    "GetLoadBalancer"
    "fixture/GetLoadBalancer.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles =
  req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestCreateCertificate :: CreateCertificate -> TestTree
requestCreateCertificate =
  req
    "CreateCertificate"
    "fixture/CreateCertificate.yaml"

requestDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancer -> TestTree
requestDetachInstancesFromLoadBalancer =
  req
    "DetachInstancesFromLoadBalancer"
    "fixture/DetachInstancesFromLoadBalancer.yaml"

requestGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificates -> TestTree
requestGetLoadBalancerTlsCertificates =
  req
    "GetLoadBalancerTlsCertificates"
    "fixture/GetLoadBalancerTlsCertificates.yaml"

requestDeleteContainerImage :: DeleteContainerImage -> TestTree
requestDeleteContainerImage =
  req
    "DeleteContainerImage"
    "fixture/DeleteContainerImage.yaml"

requestGetOperationsForResource :: GetOperationsForResource -> TestTree
requestGetOperationsForResource =
  req
    "GetOperationsForResource"
    "fixture/GetOperationsForResource.yaml"

requestCreateDisk :: CreateDisk -> TestTree
requestCreateDisk =
  req
    "CreateDisk"
    "fixture/CreateDisk.yaml"

requestEnableAddOn :: EnableAddOn -> TestTree
requestEnableAddOn =
  req
    "EnableAddOn"
    "fixture/EnableAddOn.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestStartRelationalDatabase :: StartRelationalDatabase -> TestTree
requestStartRelationalDatabase =
  req
    "StartRelationalDatabase"
    "fixture/StartRelationalDatabase.yaml"

requestCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshot -> TestTree
requestCreateRelationalDatabaseSnapshot =
  req
    "CreateRelationalDatabaseSnapshot"
    "fixture/CreateRelationalDatabaseSnapshot.yaml"

requestGetAlarms :: GetAlarms -> TestTree
requestGetAlarms =
  req
    "GetAlarms"
    "fixture/GetAlarms.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances =
  req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestCreateContainerService :: CreateContainerService -> TestTree
requestCreateContainerService =
  req
    "CreateContainerService"
    "fixture/CreateContainerService.yaml"

requestGetDistributionLatestCacheReset :: GetDistributionLatestCacheReset -> TestTree
requestGetDistributionLatestCacheReset =
  req
    "GetDistributionLatestCacheReset"
    "fixture/GetDistributionLatestCacheReset.yaml"

requestStopRelationalDatabase :: StopRelationalDatabase -> TestTree
requestStopRelationalDatabase =
  req
    "StopRelationalDatabase"
    "fixture/StopRelationalDatabase.yaml"

requestDeleteKnownHostKeys :: DeleteKnownHostKeys -> TestTree
requestDeleteKnownHostKeys =
  req
    "DeleteKnownHostKeys"
    "fixture/DeleteKnownHostKeys.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts =
  req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames =
  req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestGetAutoSnapshots :: GetAutoSnapshots -> TestTree
requestGetAutoSnapshots =
  req
    "GetAutoSnapshots"
    "fixture/GetAutoSnapshots.yaml"

requestGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreams -> TestTree
requestGetRelationalDatabaseLogStreams =
  req
    "GetRelationalDatabaseLogStreams"
    "fixture/GetRelationalDatabaseLogStreams.yaml"

requestGetDistributionBundles :: GetDistributionBundles -> TestTree
requestGetDistributionBundles =
  req
    "GetDistributionBundles"
    "fixture/GetDistributionBundles.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshot -> TestTree
requestDeleteRelationalDatabaseSnapshot =
  req
    "DeleteRelationalDatabaseSnapshot"
    "fixture/DeleteRelationalDatabaseSnapshot.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot =
  req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestDeleteContainerService :: DeleteContainerService -> TestTree
requestDeleteContainerService =
  req
    "DeleteContainerService"
    "fixture/DeleteContainerService.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestPutInstancePublicPorts :: PutInstancePublicPorts -> TestTree
requestPutInstancePublicPorts =
  req
    "PutInstancePublicPorts"
    "fixture/PutInstancePublicPorts.yaml"

requestResetDistributionCache :: ResetDistributionCache -> TestTree
requestResetDistributionCache =
  req
    "ResetDistributionCache"
    "fixture/ResetDistributionCache.yaml"

requestCreateContactMethod :: CreateContactMethod -> TestTree
requestCreateContactMethod =
  req
    "CreateContactMethod"
    "fixture/CreateContactMethod.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestUpdateContainerService :: UpdateContainerService -> TestTree
requestUpdateContainerService =
  req
    "UpdateContainerService"
    "fixture/UpdateContainerService.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair =
  req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestCreateCloudFormationStack :: CreateCloudFormationStack -> TestTree
requestCreateCloudFormationStack =
  req
    "CreateCloudFormationStack"
    "fixture/CreateCloudFormationStack.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry =
  req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState =
  req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

requestGetDistributionMetricData :: GetDistributionMetricData -> TestTree
requestGetDistributionMetricData =
  req
    "GetDistributionMetricData"
    "fixture/GetDistributionMetricData.yaml"

requestGetDisks :: GetDisks -> TestTree
requestGetDisks =
  req
    "GetDisks"
    "fixture/GetDisks.yaml"

requestGetContainerServiceMetricData :: GetContainerServiceMetricData -> TestTree
requestGetContainerServiceMetricData =
  req
    "GetContainerServiceMetricData"
    "fixture/GetContainerServiceMetricData.yaml"

requestCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLogin -> TestTree
requestCreateContainerServiceRegistryLogin =
  req
    "CreateContainerServiceRegistryLogin"
    "fixture/CreateContainerServiceRegistryLogin.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestGetContainerServicePowers :: GetContainerServicePowers -> TestTree
requestGetContainerServicePowers =
  req
    "GetContainerServicePowers"
    "fixture/GetContainerServicePowers.yaml"

requestDeleteDiskSnapshot :: DeleteDiskSnapshot -> TestTree
requestDeleteDiskSnapshot =
  req
    "DeleteDiskSnapshot"
    "fixture/DeleteDiskSnapshot.yaml"

requestGetCertificates :: GetCertificates -> TestTree
requestGetCertificates =
  req
    "GetCertificates"
    "fixture/GetCertificates.yaml"

requestReleaseStaticIp :: ReleaseStaticIp -> TestTree
requestReleaseStaticIp =
  req
    "ReleaseStaticIp"
    "fixture/ReleaseStaticIp.yaml"

requestUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParameters -> TestTree
requestUpdateRelationalDatabaseParameters =
  req
    "UpdateRelationalDatabaseParameters"
    "fixture/UpdateRelationalDatabaseParameters.yaml"

requestDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificate -> TestTree
requestDeleteLoadBalancerTlsCertificate =
  req
    "DeleteLoadBalancerTlsCertificate"
    "fixture/DeleteLoadBalancerTlsCertificate.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry =
  req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

requestGetContainerLog :: GetContainerLog -> TestTree
requestGetContainerLog =
  req
    "GetContainerLog"
    "fixture/GetContainerLog.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry =
  req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestGetContainerImages :: GetContainerImages -> TestTree
requestGetContainerImages =
  req
    "GetContainerImages"
    "fixture/GetContainerImages.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains =
  req
    "GetDomains"
    "fixture/GetDomains.yaml"

requestPutAlarm :: PutAlarm -> TestTree
requestPutAlarm =
  req
    "PutAlarm"
    "fixture/PutAlarm.yaml"

requestDeleteAutoSnapshot :: DeleteAutoSnapshot -> TestTree
requestDeleteAutoSnapshot =
  req
    "DeleteAutoSnapshot"
    "fixture/DeleteAutoSnapshot.yaml"

requestGetContactMethods :: GetContactMethods -> TestTree
requestGetContactMethods =
  req
    "GetContactMethods"
    "fixture/GetContactMethods.yaml"

requestGetRelationalDatabaseParameters :: GetRelationalDatabaseParameters -> TestTree
requestGetRelationalDatabaseParameters =
  req
    "GetRelationalDatabaseParameters"
    "fixture/GetRelationalDatabaseParameters.yaml"

requestCreateRelationalDatabase :: CreateRelationalDatabase -> TestTree
requestCreateRelationalDatabase =
  req
    "CreateRelationalDatabase"
    "fixture/CreateRelationalDatabase.yaml"

-- Responses

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices =
  res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServices)

responseGetStaticIp :: GetStaticIpResponse -> TestTree
responseGetStaticIp =
  res
    "GetStaticIpResponse"
    "fixture/GetStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy GetStaticIp)

responseGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> TestTree
responseGetRelationalDatabaseSnapshots =
  res
    "GetRelationalDatabaseSnapshotsResponse"
    "fixture/GetRelationalDatabaseSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseSnapshots)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions =
  res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributions)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot =
  res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiskSnapshot)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment =
  res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerServiceDeployment)

responsePeerVpc :: PeerVpcResponse -> TestTree
responsePeerVpc =
  res
    "PeerVpcResponse"
    "fixture/PeerVpcResponse.proto"
    defaultService
    (Proxy :: Proxy PeerVpc)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute =
  res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoadBalancerAttribute)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle =
  res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistributionBundle)

responseAllocateStaticIp :: AllocateStaticIpResponse -> TestTree
responseAllocateStaticIp =
  res
    "AllocateStaticIpResponse"
    "fixture/AllocateStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy AllocateStaticIp)

responseCloseInstancePublicPorts :: CloseInstancePublicPortsResponse -> TestTree
responseCloseInstancePublicPorts =
  res
    "CloseInstancePublicPortsResponse"
    "fixture/CloseInstancePublicPortsResponse.proto"
    defaultService
    (Proxy :: Proxy CloseInstancePublicPorts)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificate)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn =
  res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAddOn)

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords =
  res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFormationStackRecords)

responseIsVpcPeered :: IsVpcPeeredResponse -> TestTree
responseIsVpcPeered =
  res
    "IsVpcPeeredResponse"
    "fixture/IsVpcPeeredResponse.proto"
    defaultService
    (Proxy :: Proxy IsVpcPeered)

responseGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> TestTree
responseGetRelationalDatabaseSnapshot =
  res
    "GetRelationalDatabaseSnapshotResponse"
    "fixture/GetRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseSnapshot)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints =
  res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseBlueprints)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot =
  res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceSnapshot)

responseUnpeerVpc :: UnpeerVpcResponse -> TestTree
responseUnpeerVpc =
  res
    "UnpeerVpcResponse"
    "fixture/UnpeerVpcResponse.proto"
    defaultService
    (Proxy :: Proxy UnpeerVpc)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata =
  res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerAPIMetadata)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances =
  res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstances)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot =
  res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstancesFromSnapshot)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstance)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeyPair)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails =
  res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceAccessDetails)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot =
  res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceSnapshot)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstance)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage =
  res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterContainerImage)

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution =
  res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DetachCertificateFromDistribution)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancer)

responseGetContainerServiceDeployments :: GetContainerServiceDeploymentsResponse -> TestTree
responseGetContainerServiceDeployments =
  res
    "GetContainerServiceDeploymentsResponse"
    "fixture/GetContainerServiceDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServiceDeployments)

responseDeleteContactMethod :: DeleteContactMethodResponse -> TestTree
responseDeleteContactMethod =
  res
    "DeleteContactMethodResponse"
    "fixture/DeleteContactMethodResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContactMethod)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomain)

responseDetachStaticIp :: DetachStaticIpResponse -> TestTree
responseDetachStaticIp =
  res
    "DetachStaticIpResponse"
    "fixture/DetachStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy DetachStaticIp)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk =
  res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    defaultService
    (Proxy :: Proxy AttachDisk)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk =
  res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    defaultService
    (Proxy :: Proxy GetDisk)

responseGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEventsResponse -> TestTree
responseGetRelationalDatabaseLogEvents =
  res
    "GetRelationalDatabaseLogEventsResponse"
    "fixture/GetRelationalDatabaseLogEventsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseLogEvents)

responseGetRelationalDatabases :: GetRelationalDatabasesResponse -> TestTree
responseGetRelationalDatabases =
  res
    "GetRelationalDatabasesResponse"
    "fixture/GetRelationalDatabasesResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabases)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateDiskFromSnapshot :: CreateDiskFromSnapshotResponse -> TestTree
responseCreateDiskFromSnapshot =
  res
    "CreateDiskFromSnapshotResponse"
    "fixture/CreateDiskFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDiskFromSnapshot)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData =
  res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceMetricData)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancer)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ExportSnapshot)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot =
  res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabaseFromSnapshot)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations =
  res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperations)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords =
  res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportSnapshotRecords)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData =
  res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancerMetricData)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots =
  res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceSnapshots)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstance)

responseCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificateResponse -> TestTree
responseCreateLoadBalancerTlsCertificate =
  res
    "CreateLoadBalancerTlsCertificateResponse"
    "fixture/CreateLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoadBalancerTlsCertificate)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstance)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase =
  res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabase)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs =
  res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyPairs)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer =
  res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy AttachInstancesToLoadBalancer)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions =
  res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegions)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm =
  res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy TestAlarm)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot =
  res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDiskSnapshot)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy :: Proxy SetIpAddressType)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm =
  res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlarm)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification =
  res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    defaultService
    (Proxy :: Proxy SendContactMethodVerification)

responseGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> TestTree
responseGetRelationalDatabaseMasterUserPassword =
  res
    "GetRelationalDatabaseMasterUserPasswordResponse"
    "fixture/GetRelationalDatabaseMasterUserPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseMasterUserPassword)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints =
  res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBlueprints)

responseDetachDisk :: DetachDiskResponse -> TestTree
responseDetachDisk =
  res
    "DetachDiskResponse"
    "fixture/DetachDiskResponse.proto"
    defaultService
    (Proxy :: Proxy DetachDisk)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates =
  res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstancePortStates)

responseAttachStaticIp :: AttachStaticIpResponse -> TestTree
responseAttachStaticIp =
  res
    "AttachStaticIpResponse"
    "fixture/AttachStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy AttachStaticIp)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair =
  res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy DownloadDefaultKeyPair)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers =
  res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancers)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase =
  res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRelationalDatabase)

responseGetRelationalDatabaseBundles :: GetRelationalDatabaseBundlesResponse -> TestTree
responseGetRelationalDatabaseBundles =
  res
    "GetRelationalDatabaseBundlesResponse"
    "fixture/GetRelationalDatabaseBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseBundles)

responseAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificateResponse -> TestTree
responseAttachLoadBalancerTlsCertificate =
  res
    "AttachLoadBalancerTlsCertificateResponse"
    "fixture/AttachLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy AttachLoadBalancerTlsCertificate)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution =
  res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy AttachCertificateToDistribution)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase =
  res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRelationalDatabase)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstance)

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase =
  res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy RebootRelationalDatabase)

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents =
  res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseEvents)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

responseGetStaticIps :: GetStaticIpsResponse -> TestTree
responseGetStaticIps =
  res
    "GetStaticIpsResponse"
    "fixture/GetStaticIpsResponse.proto"
    defaultService
    (Proxy :: Proxy GetStaticIps)

responseDeleteDisk :: DeleteDiskResponse -> TestTree
responseDeleteDisk =
  res
    "DeleteDiskResponse"
    "fixture/DeleteDiskResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDisk)

responseGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricDataResponse -> TestTree
responseGetRelationalDatabaseMetricData =
  res
    "GetRelationalDatabaseMetricDataResponse"
    "fixture/GetRelationalDatabaseMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseMetricData)

responseGetDiskSnapshots :: GetDiskSnapshotsResponse -> TestTree
responseGetDiskSnapshots =
  res
    "GetDiskSnapshotsResponse"
    "fixture/GetDiskSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiskSnapshots)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKeyPair)

responseGetLoadBalancer :: GetLoadBalancerResponse -> TestTree
responseGetLoadBalancer =
  res
    "GetLoadBalancerResponse"
    "fixture/GetLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancer)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles =
  res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBundles)

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate =
  res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificate)

responseDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancerResponse -> TestTree
responseDetachInstancesFromLoadBalancer =
  res
    "DetachInstancesFromLoadBalancerResponse"
    "fixture/DetachInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DetachInstancesFromLoadBalancer)

responseGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificatesResponse -> TestTree
responseGetLoadBalancerTlsCertificates =
  res
    "GetLoadBalancerTlsCertificatesResponse"
    "fixture/GetLoadBalancerTlsCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancerTlsCertificates)

responseDeleteContainerImage :: DeleteContainerImageResponse -> TestTree
responseDeleteContainerImage =
  res
    "DeleteContainerImageResponse"
    "fixture/DeleteContainerImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerImage)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource =
  res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperationsForResource)

responseCreateDisk :: CreateDiskResponse -> TestTree
responseCreateDisk =
  res
    "CreateDiskResponse"
    "fixture/CreateDiskResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDisk)

responseEnableAddOn :: EnableAddOnResponse -> TestTree
responseEnableAddOn =
  res
    "EnableAddOnResponse"
    "fixture/EnableAddOnResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAddOn)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase =
  res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy StartRelationalDatabase)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot =
  res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabaseSnapshot)

responseGetAlarms :: GetAlarmsResponse -> TestTree
responseGetAlarms =
  res
    "GetAlarmsResponse"
    "fixture/GetAlarmsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAlarms)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDistribution)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances =
  res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstances)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService =
  res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerService)

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset =
  res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionLatestCacheReset)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase =
  res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy StopRelationalDatabase)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys =
  res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKnownHostKeys)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts =
  res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    defaultService
    (Proxy :: Proxy OpenInstancePublicPorts)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames =
  res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    defaultService
    (Proxy :: Proxy GetActiveNames)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots =
  res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAutoSnapshots)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams =
  res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseLogStreams)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles =
  res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionBundles)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperation)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot =
  res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRelationalDatabaseSnapshot)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot =
  res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceSnapshot)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService =
  res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerService)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistribution)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts =
  res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    defaultService
    (Proxy :: Proxy PutInstancePublicPorts)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache =
  res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDistributionCache)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod =
  res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactMethod)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDistribution)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService =
  res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContainerService)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair =
  res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyPair)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack =
  res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFormationStack)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry =
  res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomainEntry)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState =
  res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceState)

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData =
  res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionMetricData)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks =
  res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    defaultService
    (Proxy :: Proxy GetDisks)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData =
  res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServiceMetricData)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin =
  res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerServiceRegistryLogin)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy ImportKeyPair)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers =
  res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServicePowers)

responseDeleteDiskSnapshot :: DeleteDiskSnapshotResponse -> TestTree
responseDeleteDiskSnapshot =
  res
    "DeleteDiskSnapshotResponse"
    "fixture/DeleteDiskSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDiskSnapshot)

responseGetCertificates :: GetCertificatesResponse -> TestTree
responseGetCertificates =
  res
    "GetCertificatesResponse"
    "fixture/GetCertificatesResponse.proto"
    defaultService
    (Proxy :: Proxy GetCertificates)

responseReleaseStaticIp :: ReleaseStaticIpResponse -> TestTree
responseReleaseStaticIp =
  res
    "ReleaseStaticIpResponse"
    "fixture/ReleaseStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy ReleaseStaticIp)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters =
  res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRelationalDatabaseParameters)

responseDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificateResponse -> TestTree
responseDeleteLoadBalancerTlsCertificate =
  res
    "DeleteLoadBalancerTlsCertificateResponse"
    "fixture/DeleteLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerTlsCertificate)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry =
  res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainEntry)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog =
  res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerLog)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry =
  res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainEntry)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages =
  res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerImages)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains =
  res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomains)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm =
  res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy PutAlarm)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot =
  res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutoSnapshot)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods =
  res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactMethods)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters =
  res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseParameters)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase =
  res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabase)
