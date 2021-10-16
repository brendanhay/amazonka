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
--         , requestGetDistributions $
--             newGetDistributions
--
--         , requestCreateContainerServiceDeployment $
--             newCreateContainerServiceDeployment
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestGetDiskSnapshot $
--             newGetDiskSnapshot
--
--         , requestUpdateDistributionBundle $
--             newUpdateDistributionBundle
--
--         , requestUpdateBucket $
--             newUpdateBucket
--
--         , requestUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttribute
--
--         , requestGetStaticIp $
--             newGetStaticIp
--
--         , requestGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshots
--
--         , requestGetBucketBundles $
--             newGetBucketBundles
--
--         , requestPeerVpc $
--             newPeerVpc
--
--         , requestGetBucketMetricData $
--             newGetBucketMetricData
--
--         , requestUnpeerVpc $
--             newUnpeerVpc
--
--         , requestDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshot
--
--         , requestAllocateStaticIp $
--             newAllocateStaticIp
--
--         , requestCloseInstancePublicPorts $
--             newCloseInstancePublicPorts
--
--         , requestGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecords
--
--         , requestDisableAddOn $
--             newDisableAddOn
--
--         , requestIsVpcPeered $
--             newIsVpcPeered
--
--         , requestGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshot
--
--         , requestGetInstances $
--             newGetInstances
--
--         , requestCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshot
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestGetContainerAPIMetadata $
--             newGetContainerAPIMetadata
--
--         , requestGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprints
--
--         , requestRegisterContainerImage $
--             newRegisterContainerImage
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestCreateInstanceSnapshot $
--             newCreateInstanceSnapshot
--
--         , requestCreateBucketAccessKey $
--             newCreateBucketAccessKey
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestGetInstanceAccessDetails $
--             newGetInstanceAccessDetails
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestDetachStaticIp $
--             newDetachStaticIp
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
--         , requestDetachCertificateFromDistribution $
--             newDetachCertificateFromDistribution
--
--         , requestAttachDisk $
--             newAttachDisk
--
--         , requestGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEvents
--
--         , requestGetRelationalDatabases $
--             newGetRelationalDatabases
--
--         , requestCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshot
--
--         , requestGetInstanceMetricData $
--             newGetInstanceMetricData
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestGetDisk $
--             newGetDisk
--
--         , requestGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricData
--
--         , requestGetKeyPairs $
--             newGetKeyPairs
--
--         , requestGetOperations $
--             newGetOperations
--
--         , requestAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancer
--
--         , requestTagResource $
--             newTagResource
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshot
--
--         , requestGetExportSnapshotRecords $
--             newGetExportSnapshotRecords
--
--         , requestGetRelationalDatabase $
--             newGetRelationalDatabase
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificate
--
--         , requestExportSnapshot $
--             newExportSnapshot
--
--         , requestGetInstanceSnapshots $
--             newGetInstanceSnapshots
--
--         , requestGetBucketAccessKeys $
--             newGetBucketAccessKeys
--
--         , requestGetRegions $
--             newGetRegions
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestCreateDiskSnapshot $
--             newCreateDiskSnapshot
--
--         , requestTestAlarm $
--             newTestAlarm
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPassword
--
--         , requestGetBlueprints $
--             newGetBlueprints
--
--         , requestAttachStaticIp $
--             newAttachStaticIp
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestSendContactMethodVerification $
--             newSendContactMethodVerification
--
--         , requestDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPair
--
--         , requestDeleteAlarm $
--             newDeleteAlarm
--
--         , requestDetachDisk $
--             newDetachDisk
--
--         , requestGetInstancePortStates $
--             newGetInstancePortStates
--
--         , requestGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEvents
--
--         , requestUpdateRelationalDatabase $
--             newUpdateRelationalDatabase
--
--         , requestDeleteRelationalDatabase $
--             newDeleteRelationalDatabase
--
--         , requestGetLoadBalancers $
--             newGetLoadBalancers
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestAttachCertificateToDistribution $
--             newAttachCertificateToDistribution
--
--         , requestGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundles
--
--         , requestAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificate
--
--         , requestRebootRelationalDatabase $
--             newRebootRelationalDatabase
--
--         , requestGetStaticIps $
--             newGetStaticIps
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
--         , requestDeleteBucketAccessKey $
--             newDeleteBucketAccessKey
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDeleteDisk $
--             newDeleteDisk
--
--         , requestGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricData
--
--         , requestSetResourceAccessForBucket $
--             newSetResourceAccessForBucket
--
--         , requestDeleteDomain $
--             newDeleteDomain
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
--         , requestDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancer
--
--         , requestGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificates
--
--         , requestDeleteContainerImage $
--             newDeleteContainerImage
--
--         , requestCreateCertificate $
--             newCreateCertificate
--
--         , requestGetBundles $
--             newGetBundles
--
--         , requestDeleteKnownHostKeys $
--             newDeleteKnownHostKeys
--
--         , requestCreateInstances $
--             newCreateInstances
--
--         , requestGetActiveNames $
--             newGetActiveNames
--
--         , requestCreateContainerService $
--             newCreateContainerService
--
--         , requestStopRelationalDatabase $
--             newStopRelationalDatabase
--
--         , requestGetAlarms $
--             newGetAlarms
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheReset
--
--         , requestOpenInstancePublicPorts $
--             newOpenInstancePublicPorts
--
--         , requestCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshot
--
--         , requestGetAutoSnapshots $
--             newGetAutoSnapshots
--
--         , requestStartRelationalDatabase $
--             newStartRelationalDatabase
--
--         , requestUpdateContainerService $
--             newUpdateContainerService
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestGetKeyPair $
--             newGetKeyPair
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreams
--
--         , requestUpdateBucketBundle $
--             newUpdateBucketBundle
--
--         , requestGetBuckets $
--             newGetBuckets
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshot
--
--         , requestResetDistributionCache $
--             newResetDistributionCache
--
--         , requestGetInstanceSnapshot $
--             newGetInstanceSnapshot
--
--         , requestCreateContactMethod $
--             newCreateContactMethod
--
--         , requestPutInstancePublicPorts $
--             newPutInstancePublicPorts
--
--         , requestGetDistributionBundles $
--             newGetDistributionBundles
--
--         , requestDeleteContainerService $
--             newDeleteContainerService
--
--         , requestCreateCloudFormationStack $
--             newCreateCloudFormationStack
--
--         , requestGetContainerServicePowers $
--             newGetContainerServicePowers
--
--         , requestReleaseStaticIp $
--             newReleaseStaticIp
--
--         , requestGetDisks $
--             newGetDisks
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestCreateDomainEntry $
--             newCreateDomainEntry
--
--         , requestCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLogin
--
--         , requestDeleteDiskSnapshot $
--             newDeleteDiskSnapshot
--
--         , requestGetCertificates $
--             newGetCertificates
--
--         , requestGetDistributionMetricData $
--             newGetDistributionMetricData
--
--         , requestGetInstanceState $
--             newGetInstanceState
--
--         , requestGetContainerServiceMetricData $
--             newGetContainerServiceMetricData
--
--         , requestUpdateDomainEntry $
--             newUpdateDomainEntry
--
--         , requestDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificate
--
--         , requestGetContainerImages $
--             newGetContainerImages
--
--         , requestUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParameters
--
--         , requestGetDomains $
--             newGetDomains
--
--         , requestDeleteDomainEntry $
--             newDeleteDomainEntry
--
--         , requestGetContainerLog $
--             newGetContainerLog
--
--         , requestPutAlarm $
--             newPutAlarm
--
--         , requestGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParameters
--
--         , requestDeleteAutoSnapshot $
--             newDeleteAutoSnapshot
--
--         , requestCreateRelationalDatabase $
--             newCreateRelationalDatabase
--
--         , requestGetContactMethods $
--             newGetContactMethods
--
--           ]

--     , testGroup "response"
--         [ responseGetContainerServices $
--             newGetContainerServicesResponse
--
--         , responseGetDistributions $
--             newGetDistributionsResponse
--
--         , responseCreateContainerServiceDeployment $
--             newCreateContainerServiceDeploymentResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseGetDiskSnapshot $
--             newGetDiskSnapshotResponse
--
--         , responseUpdateDistributionBundle $
--             newUpdateDistributionBundleResponse
--
--         , responseUpdateBucket $
--             newUpdateBucketResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttributeResponse
--
--         , responseGetStaticIp $
--             newGetStaticIpResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshotsResponse
--
--         , responseGetBucketBundles $
--             newGetBucketBundlesResponse
--
--         , responsePeerVpc $
--             newPeerVpcResponse
--
--         , responseGetBucketMetricData $
--             newGetBucketMetricDataResponse
--
--         , responseUnpeerVpc $
--             newUnpeerVpcResponse
--
--         , responseDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshotResponse
--
--         , responseAllocateStaticIp $
--             newAllocateStaticIpResponse
--
--         , responseCloseInstancePublicPorts $
--             newCloseInstancePublicPortsResponse
--
--         , responseGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecordsResponse
--
--         , responseDisableAddOn $
--             newDisableAddOnResponse
--
--         , responseIsVpcPeered $
--             newIsVpcPeeredResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshotResponse
--
--         , responseGetInstances $
--             newGetInstancesResponse
--
--         , responseCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshotResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseGetContainerAPIMetadata $
--             newGetContainerAPIMetadataResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprintsResponse
--
--         , responseRegisterContainerImage $
--             newRegisterContainerImageResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseCreateInstanceSnapshot $
--             newCreateInstanceSnapshotResponse
--
--         , responseCreateBucketAccessKey $
--             newCreateBucketAccessKeyResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseGetInstanceAccessDetails $
--             newGetInstanceAccessDetailsResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseDetachStaticIp $
--             newDetachStaticIpResponse
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
--         , responseDetachCertificateFromDistribution $
--             newDetachCertificateFromDistributionResponse
--
--         , responseAttachDisk $
--             newAttachDiskResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEventsResponse
--
--         , responseGetRelationalDatabases $
--             newGetRelationalDatabasesResponse
--
--         , responseCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshotResponse
--
--         , responseGetInstanceMetricData $
--             newGetInstanceMetricDataResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseGetDisk $
--             newGetDiskResponse
--
--         , responseGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricDataResponse
--
--         , responseGetKeyPairs $
--             newGetKeyPairsResponse
--
--         , responseGetOperations $
--             newGetOperationsResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshotResponse
--
--         , responseGetExportSnapshotRecords $
--             newGetExportSnapshotRecordsResponse
--
--         , responseGetRelationalDatabase $
--             newGetRelationalDatabaseResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificateResponse
--
--         , responseExportSnapshot $
--             newExportSnapshotResponse
--
--         , responseGetInstanceSnapshots $
--             newGetInstanceSnapshotsResponse
--
--         , responseGetBucketAccessKeys $
--             newGetBucketAccessKeysResponse
--
--         , responseGetRegions $
--             newGetRegionsResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseCreateDiskSnapshot $
--             newCreateDiskSnapshotResponse
--
--         , responseTestAlarm $
--             newTestAlarmResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetBlueprints $
--             newGetBlueprintsResponse
--
--         , responseAttachStaticIp $
--             newAttachStaticIpResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseSendContactMethodVerification $
--             newSendContactMethodVerificationResponse
--
--         , responseDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPairResponse
--
--         , responseDeleteAlarm $
--             newDeleteAlarmResponse
--
--         , responseDetachDisk $
--             newDetachDiskResponse
--
--         , responseGetInstancePortStates $
--             newGetInstancePortStatesResponse
--
--         , responseGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEventsResponse
--
--         , responseUpdateRelationalDatabase $
--             newUpdateRelationalDatabaseResponse
--
--         , responseDeleteRelationalDatabase $
--             newDeleteRelationalDatabaseResponse
--
--         , responseGetLoadBalancers $
--             newGetLoadBalancersResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseAttachCertificateToDistribution $
--             newAttachCertificateToDistributionResponse
--
--         , responseGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundlesResponse
--
--         , responseAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificateResponse
--
--         , responseRebootRelationalDatabase $
--             newRebootRelationalDatabaseResponse
--
--         , responseGetStaticIps $
--             newGetStaticIpsResponse
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
--         , responseDeleteBucketAccessKey $
--             newDeleteBucketAccessKeyResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDeleteDisk $
--             newDeleteDiskResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricDataResponse
--
--         , responseSetResourceAccessForBucket $
--             newSetResourceAccessForBucketResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
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
--         , responseDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancerResponse
--
--         , responseGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificatesResponse
--
--         , responseDeleteContainerImage $
--             newDeleteContainerImageResponse
--
--         , responseCreateCertificate $
--             newCreateCertificateResponse
--
--         , responseGetBundles $
--             newGetBundlesResponse
--
--         , responseDeleteKnownHostKeys $
--             newDeleteKnownHostKeysResponse
--
--         , responseCreateInstances $
--             newCreateInstancesResponse
--
--         , responseGetActiveNames $
--             newGetActiveNamesResponse
--
--         , responseCreateContainerService $
--             newCreateContainerServiceResponse
--
--         , responseStopRelationalDatabase $
--             newStopRelationalDatabaseResponse
--
--         , responseGetAlarms $
--             newGetAlarmsResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheResetResponse
--
--         , responseOpenInstancePublicPorts $
--             newOpenInstancePublicPortsResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshotResponse
--
--         , responseGetAutoSnapshots $
--             newGetAutoSnapshotsResponse
--
--         , responseStartRelationalDatabase $
--             newStartRelationalDatabaseResponse
--
--         , responseUpdateContainerService $
--             newUpdateContainerServiceResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseGetKeyPair $
--             newGetKeyPairResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreamsResponse
--
--         , responseUpdateBucketBundle $
--             newUpdateBucketBundleResponse
--
--         , responseGetBuckets $
--             newGetBucketsResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshotResponse
--
--         , responseResetDistributionCache $
--             newResetDistributionCacheResponse
--
--         , responseGetInstanceSnapshot $
--             newGetInstanceSnapshotResponse
--
--         , responseCreateContactMethod $
--             newCreateContactMethodResponse
--
--         , responsePutInstancePublicPorts $
--             newPutInstancePublicPortsResponse
--
--         , responseGetDistributionBundles $
--             newGetDistributionBundlesResponse
--
--         , responseDeleteContainerService $
--             newDeleteContainerServiceResponse
--
--         , responseCreateCloudFormationStack $
--             newCreateCloudFormationStackResponse
--
--         , responseGetContainerServicePowers $
--             newGetContainerServicePowersResponse
--
--         , responseReleaseStaticIp $
--             newReleaseStaticIpResponse
--
--         , responseGetDisks $
--             newGetDisksResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseCreateDomainEntry $
--             newCreateDomainEntryResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLoginResponse
--
--         , responseDeleteDiskSnapshot $
--             newDeleteDiskSnapshotResponse
--
--         , responseGetCertificates $
--             newGetCertificatesResponse
--
--         , responseGetDistributionMetricData $
--             newGetDistributionMetricDataResponse
--
--         , responseGetInstanceState $
--             newGetInstanceStateResponse
--
--         , responseGetContainerServiceMetricData $
--             newGetContainerServiceMetricDataResponse
--
--         , responseUpdateDomainEntry $
--             newUpdateDomainEntryResponse
--
--         , responseDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificateResponse
--
--         , responseGetContainerImages $
--             newGetContainerImagesResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParametersResponse
--
--         , responseGetDomains $
--             newGetDomainsResponse
--
--         , responseDeleteDomainEntry $
--             newDeleteDomainEntryResponse
--
--         , responseGetContainerLog $
--             newGetContainerLogResponse
--
--         , responsePutAlarm $
--             newPutAlarmResponse
--
--         , responseGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParametersResponse
--
--         , responseDeleteAutoSnapshot $
--             newDeleteAutoSnapshotResponse
--
--         , responseCreateRelationalDatabase $
--             newCreateRelationalDatabaseResponse
--
--         , responseGetContactMethods $
--             newGetContactMethodsResponse
--
--           ]
--     ]

-- Requests

requestGetContainerServices :: GetContainerServices -> TestTree
requestGetContainerServices =
  req
    "GetContainerServices"
    "fixture/GetContainerServices.yaml"

requestGetDistributions :: GetDistributions -> TestTree
requestGetDistributions =
  req
    "GetDistributions"
    "fixture/GetDistributions.yaml"

requestCreateContainerServiceDeployment :: CreateContainerServiceDeployment -> TestTree
requestCreateContainerServiceDeployment =
  req
    "CreateContainerServiceDeployment"
    "fixture/CreateContainerServiceDeployment.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestGetDiskSnapshot :: GetDiskSnapshot -> TestTree
requestGetDiskSnapshot =
  req
    "GetDiskSnapshot"
    "fixture/GetDiskSnapshot.yaml"

requestUpdateDistributionBundle :: UpdateDistributionBundle -> TestTree
requestUpdateDistributionBundle =
  req
    "UpdateDistributionBundle"
    "fixture/UpdateDistributionBundle.yaml"

requestUpdateBucket :: UpdateBucket -> TestTree
requestUpdateBucket =
  req
    "UpdateBucket"
    "fixture/UpdateBucket.yaml"

requestUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttribute -> TestTree
requestUpdateLoadBalancerAttribute =
  req
    "UpdateLoadBalancerAttribute"
    "fixture/UpdateLoadBalancerAttribute.yaml"

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

requestGetBucketBundles :: GetBucketBundles -> TestTree
requestGetBucketBundles =
  req
    "GetBucketBundles"
    "fixture/GetBucketBundles.yaml"

requestPeerVpc :: PeerVpc -> TestTree
requestPeerVpc =
  req
    "PeerVpc"
    "fixture/PeerVpc.yaml"

requestGetBucketMetricData :: GetBucketMetricData -> TestTree
requestGetBucketMetricData =
  req
    "GetBucketMetricData"
    "fixture/GetBucketMetricData.yaml"

requestUnpeerVpc :: UnpeerVpc -> TestTree
requestUnpeerVpc =
  req
    "UnpeerVpc"
    "fixture/UnpeerVpc.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot =
  req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

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

requestGetCloudFormationStackRecords :: GetCloudFormationStackRecords -> TestTree
requestGetCloudFormationStackRecords =
  req
    "GetCloudFormationStackRecords"
    "fixture/GetCloudFormationStackRecords.yaml"

requestDisableAddOn :: DisableAddOn -> TestTree
requestDisableAddOn =
  req
    "DisableAddOn"
    "fixture/DisableAddOn.yaml"

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

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestGetContainerAPIMetadata :: GetContainerAPIMetadata -> TestTree
requestGetContainerAPIMetadata =
  req
    "GetContainerAPIMetadata"
    "fixture/GetContainerAPIMetadata.yaml"

requestGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprints -> TestTree
requestGetRelationalDatabaseBlueprints =
  req
    "GetRelationalDatabaseBlueprints"
    "fixture/GetRelationalDatabaseBlueprints.yaml"

requestRegisterContainerImage :: RegisterContainerImage -> TestTree
requestRegisterContainerImage =
  req
    "RegisterContainerImage"
    "fixture/RegisterContainerImage.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot =
  req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestCreateBucketAccessKey :: CreateBucketAccessKey -> TestTree
requestCreateBucketAccessKey =
  req
    "CreateBucketAccessKey"
    "fixture/CreateBucketAccessKey.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails =
  req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

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

requestDetachCertificateFromDistribution :: DetachCertificateFromDistribution -> TestTree
requestDetachCertificateFromDistribution =
  req
    "DetachCertificateFromDistribution"
    "fixture/DetachCertificateFromDistribution.yaml"

requestAttachDisk :: AttachDisk -> TestTree
requestAttachDisk =
  req
    "AttachDisk"
    "fixture/AttachDisk.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestGetDisk :: GetDisk -> TestTree
requestGetDisk =
  req
    "GetDisk"
    "fixture/GetDisk.yaml"

requestGetLoadBalancerMetricData :: GetLoadBalancerMetricData -> TestTree
requestGetLoadBalancerMetricData =
  req
    "GetLoadBalancerMetricData"
    "fixture/GetLoadBalancerMetricData.yaml"

requestGetKeyPairs :: GetKeyPairs -> TestTree
requestGetKeyPairs =
  req
    "GetKeyPairs"
    "fixture/GetKeyPairs.yaml"

requestGetOperations :: GetOperations -> TestTree
requestGetOperations =
  req
    "GetOperations"
    "fixture/GetOperations.yaml"

requestAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancer -> TestTree
requestAttachInstancesToLoadBalancer =
  req
    "AttachInstancesToLoadBalancer"
    "fixture/AttachInstancesToLoadBalancer.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshot -> TestTree
requestCreateRelationalDatabaseFromSnapshot =
  req
    "CreateRelationalDatabaseFromSnapshot"
    "fixture/CreateRelationalDatabaseFromSnapshot.yaml"

requestGetExportSnapshotRecords :: GetExportSnapshotRecords -> TestTree
requestGetExportSnapshotRecords =
  req
    "GetExportSnapshotRecords"
    "fixture/GetExportSnapshotRecords.yaml"

requestGetRelationalDatabase :: GetRelationalDatabase -> TestTree
requestGetRelationalDatabase =
  req
    "GetRelationalDatabase"
    "fixture/GetRelationalDatabase.yaml"

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

requestExportSnapshot :: ExportSnapshot -> TestTree
requestExportSnapshot =
  req
    "ExportSnapshot"
    "fixture/ExportSnapshot.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots =
  req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

requestGetBucketAccessKeys :: GetBucketAccessKeys -> TestTree
requestGetBucketAccessKeys =
  req
    "GetBucketAccessKeys"
    "fixture/GetBucketAccessKeys.yaml"

requestGetRegions :: GetRegions -> TestTree
requestGetRegions =
  req
    "GetRegions"
    "fixture/GetRegions.yaml"

requestSetIpAddressType :: SetIpAddressType -> TestTree
requestSetIpAddressType =
  req
    "SetIpAddressType"
    "fixture/SetIpAddressType.yaml"

requestCreateDiskSnapshot :: CreateDiskSnapshot -> TestTree
requestCreateDiskSnapshot =
  req
    "CreateDiskSnapshot"
    "fixture/CreateDiskSnapshot.yaml"

requestTestAlarm :: TestAlarm -> TestTree
requestTestAlarm =
  req
    "TestAlarm"
    "fixture/TestAlarm.yaml"

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

requestAttachStaticIp :: AttachStaticIp -> TestTree
requestAttachStaticIp =
  req
    "AttachStaticIp"
    "fixture/AttachStaticIp.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestSendContactMethodVerification :: SendContactMethodVerification -> TestTree
requestSendContactMethodVerification =
  req
    "SendContactMethodVerification"
    "fixture/SendContactMethodVerification.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair =
  req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestDeleteAlarm :: DeleteAlarm -> TestTree
requestDeleteAlarm =
  req
    "DeleteAlarm"
    "fixture/DeleteAlarm.yaml"

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

requestGetRelationalDatabaseEvents :: GetRelationalDatabaseEvents -> TestTree
requestGetRelationalDatabaseEvents =
  req
    "GetRelationalDatabaseEvents"
    "fixture/GetRelationalDatabaseEvents.yaml"

requestUpdateRelationalDatabase :: UpdateRelationalDatabase -> TestTree
requestUpdateRelationalDatabase =
  req
    "UpdateRelationalDatabase"
    "fixture/UpdateRelationalDatabase.yaml"

requestDeleteRelationalDatabase :: DeleteRelationalDatabase -> TestTree
requestDeleteRelationalDatabase =
  req
    "DeleteRelationalDatabase"
    "fixture/DeleteRelationalDatabase.yaml"

requestGetLoadBalancers :: GetLoadBalancers -> TestTree
requestGetLoadBalancers =
  req
    "GetLoadBalancers"
    "fixture/GetLoadBalancers.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestAttachCertificateToDistribution :: AttachCertificateToDistribution -> TestTree
requestAttachCertificateToDistribution =
  req
    "AttachCertificateToDistribution"
    "fixture/AttachCertificateToDistribution.yaml"

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

requestRebootRelationalDatabase :: RebootRelationalDatabase -> TestTree
requestRebootRelationalDatabase =
  req
    "RebootRelationalDatabase"
    "fixture/RebootRelationalDatabase.yaml"

requestGetStaticIps :: GetStaticIps -> TestTree
requestGetStaticIps =
  req
    "GetStaticIps"
    "fixture/GetStaticIps.yaml"

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

requestDeleteBucketAccessKey :: DeleteBucketAccessKey -> TestTree
requestDeleteBucketAccessKey =
  req
    "DeleteBucketAccessKey"
    "fixture/DeleteBucketAccessKey.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

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

requestSetResourceAccessForBucket :: SetResourceAccessForBucket -> TestTree
requestSetResourceAccessForBucket =
  req
    "SetResourceAccessForBucket"
    "fixture/SetResourceAccessForBucket.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

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

requestCreateCertificate :: CreateCertificate -> TestTree
requestCreateCertificate =
  req
    "CreateCertificate"
    "fixture/CreateCertificate.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles =
  req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestDeleteKnownHostKeys :: DeleteKnownHostKeys -> TestTree
requestDeleteKnownHostKeys =
  req
    "DeleteKnownHostKeys"
    "fixture/DeleteKnownHostKeys.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances =
  req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames =
  req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestCreateContainerService :: CreateContainerService -> TestTree
requestCreateContainerService =
  req
    "CreateContainerService"
    "fixture/CreateContainerService.yaml"

requestStopRelationalDatabase :: StopRelationalDatabase -> TestTree
requestStopRelationalDatabase =
  req
    "StopRelationalDatabase"
    "fixture/StopRelationalDatabase.yaml"

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

requestGetDistributionLatestCacheReset :: GetDistributionLatestCacheReset -> TestTree
requestGetDistributionLatestCacheReset =
  req
    "GetDistributionLatestCacheReset"
    "fixture/GetDistributionLatestCacheReset.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts =
  req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshot -> TestTree
requestCreateRelationalDatabaseSnapshot =
  req
    "CreateRelationalDatabaseSnapshot"
    "fixture/CreateRelationalDatabaseSnapshot.yaml"

requestGetAutoSnapshots :: GetAutoSnapshots -> TestTree
requestGetAutoSnapshots =
  req
    "GetAutoSnapshots"
    "fixture/GetAutoSnapshots.yaml"

requestStartRelationalDatabase :: StartRelationalDatabase -> TestTree
requestStartRelationalDatabase =
  req
    "StartRelationalDatabase"
    "fixture/StartRelationalDatabase.yaml"

requestUpdateContainerService :: UpdateContainerService -> TestTree
requestUpdateContainerService =
  req
    "UpdateContainerService"
    "fixture/UpdateContainerService.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair =
  req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreams -> TestTree
requestGetRelationalDatabaseLogStreams =
  req
    "GetRelationalDatabaseLogStreams"
    "fixture/GetRelationalDatabaseLogStreams.yaml"

requestUpdateBucketBundle :: UpdateBucketBundle -> TestTree
requestUpdateBucketBundle =
  req
    "UpdateBucketBundle"
    "fixture/UpdateBucketBundle.yaml"

requestGetBuckets :: GetBuckets -> TestTree
requestGetBuckets =
  req
    "GetBuckets"
    "fixture/GetBuckets.yaml"

requestDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshot -> TestTree
requestDeleteRelationalDatabaseSnapshot =
  req
    "DeleteRelationalDatabaseSnapshot"
    "fixture/DeleteRelationalDatabaseSnapshot.yaml"

requestResetDistributionCache :: ResetDistributionCache -> TestTree
requestResetDistributionCache =
  req
    "ResetDistributionCache"
    "fixture/ResetDistributionCache.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot =
  req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestCreateContactMethod :: CreateContactMethod -> TestTree
requestCreateContactMethod =
  req
    "CreateContactMethod"
    "fixture/CreateContactMethod.yaml"

requestPutInstancePublicPorts :: PutInstancePublicPorts -> TestTree
requestPutInstancePublicPorts =
  req
    "PutInstancePublicPorts"
    "fixture/PutInstancePublicPorts.yaml"

requestGetDistributionBundles :: GetDistributionBundles -> TestTree
requestGetDistributionBundles =
  req
    "GetDistributionBundles"
    "fixture/GetDistributionBundles.yaml"

requestDeleteContainerService :: DeleteContainerService -> TestTree
requestDeleteContainerService =
  req
    "DeleteContainerService"
    "fixture/DeleteContainerService.yaml"

requestCreateCloudFormationStack :: CreateCloudFormationStack -> TestTree
requestCreateCloudFormationStack =
  req
    "CreateCloudFormationStack"
    "fixture/CreateCloudFormationStack.yaml"

requestGetContainerServicePowers :: GetContainerServicePowers -> TestTree
requestGetContainerServicePowers =
  req
    "GetContainerServicePowers"
    "fixture/GetContainerServicePowers.yaml"

requestReleaseStaticIp :: ReleaseStaticIp -> TestTree
requestReleaseStaticIp =
  req
    "ReleaseStaticIp"
    "fixture/ReleaseStaticIp.yaml"

requestGetDisks :: GetDisks -> TestTree
requestGetDisks =
  req
    "GetDisks"
    "fixture/GetDisks.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry =
  req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLogin -> TestTree
requestCreateContainerServiceRegistryLogin =
  req
    "CreateContainerServiceRegistryLogin"
    "fixture/CreateContainerServiceRegistryLogin.yaml"

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

requestGetDistributionMetricData :: GetDistributionMetricData -> TestTree
requestGetDistributionMetricData =
  req
    "GetDistributionMetricData"
    "fixture/GetDistributionMetricData.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState =
  req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

requestGetContainerServiceMetricData :: GetContainerServiceMetricData -> TestTree
requestGetContainerServiceMetricData =
  req
    "GetContainerServiceMetricData"
    "fixture/GetContainerServiceMetricData.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry =
  req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

requestDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificate -> TestTree
requestDeleteLoadBalancerTlsCertificate =
  req
    "DeleteLoadBalancerTlsCertificate"
    "fixture/DeleteLoadBalancerTlsCertificate.yaml"

requestGetContainerImages :: GetContainerImages -> TestTree
requestGetContainerImages =
  req
    "GetContainerImages"
    "fixture/GetContainerImages.yaml"

requestUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParameters -> TestTree
requestUpdateRelationalDatabaseParameters =
  req
    "UpdateRelationalDatabaseParameters"
    "fixture/UpdateRelationalDatabaseParameters.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains =
  req
    "GetDomains"
    "fixture/GetDomains.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry =
  req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestGetContainerLog :: GetContainerLog -> TestTree
requestGetContainerLog =
  req
    "GetContainerLog"
    "fixture/GetContainerLog.yaml"

requestPutAlarm :: PutAlarm -> TestTree
requestPutAlarm =
  req
    "PutAlarm"
    "fixture/PutAlarm.yaml"

requestGetRelationalDatabaseParameters :: GetRelationalDatabaseParameters -> TestTree
requestGetRelationalDatabaseParameters =
  req
    "GetRelationalDatabaseParameters"
    "fixture/GetRelationalDatabaseParameters.yaml"

requestDeleteAutoSnapshot :: DeleteAutoSnapshot -> TestTree
requestDeleteAutoSnapshot =
  req
    "DeleteAutoSnapshot"
    "fixture/DeleteAutoSnapshot.yaml"

requestCreateRelationalDatabase :: CreateRelationalDatabase -> TestTree
requestCreateRelationalDatabase =
  req
    "CreateRelationalDatabase"
    "fixture/CreateRelationalDatabase.yaml"

requestGetContactMethods :: GetContactMethods -> TestTree
requestGetContactMethods =
  req
    "GetContactMethods"
    "fixture/GetContactMethods.yaml"

-- Responses

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices =
  res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServices)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions =
  res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributions)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment =
  res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerServiceDeployment)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucket)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot =
  res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetDiskSnapshot)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle =
  res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistributionBundle)

responseUpdateBucket :: UpdateBucketResponse -> TestTree
responseUpdateBucket =
  res
    "UpdateBucketResponse"
    "fixture/UpdateBucketResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBucket)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute =
  res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoadBalancerAttribute)

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

responseGetBucketBundles :: GetBucketBundlesResponse -> TestTree
responseGetBucketBundles =
  res
    "GetBucketBundlesResponse"
    "fixture/GetBucketBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketBundles)

responsePeerVpc :: PeerVpcResponse -> TestTree
responsePeerVpc =
  res
    "PeerVpcResponse"
    "fixture/PeerVpcResponse.proto"
    defaultService
    (Proxy :: Proxy PeerVpc)

responseGetBucketMetricData :: GetBucketMetricDataResponse -> TestTree
responseGetBucketMetricData =
  res
    "GetBucketMetricDataResponse"
    "fixture/GetBucketMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketMetricData)

responseUnpeerVpc :: UnpeerVpcResponse -> TestTree
responseUnpeerVpc =
  res
    "UnpeerVpcResponse"
    "fixture/UnpeerVpcResponse.proto"
    defaultService
    (Proxy :: Proxy UnpeerVpc)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot =
  res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteInstanceSnapshot)

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

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords =
  res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCloudFormationStackRecords)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn =
  res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAddOn)

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

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCertificate)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata =
  res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerAPIMetadata)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints =
  res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseBlueprints)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage =
  res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterContainerImage)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy CreateKeyPair)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StartInstance)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot =
  res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstanceSnapshot)

responseCreateBucketAccessKey :: CreateBucketAccessKeyResponse -> TestTree
responseCreateBucketAccessKey =
  res
    "CreateBucketAccessKeyResponse"
    "fixture/CreateBucketAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBucketAccessKey)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CopySnapshot)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails =
  res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceAccessDetails)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy StopInstance)

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

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution =
  res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DetachCertificateFromDistribution)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk =
  res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    defaultService
    (Proxy :: Proxy AttachDisk)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancer)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk =
  res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    defaultService
    (Proxy :: Proxy GetDisk)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData =
  res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancerMetricData)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs =
  res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyPairs)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations =
  res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperations)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer =
  res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    defaultService
    (Proxy :: Proxy AttachInstancesToLoadBalancer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RebootInstance)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot =
  res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabaseFromSnapshot)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords =
  res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy GetExportSnapshotRecords)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase =
  res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabase)

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

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy ExportSnapshot)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots =
  res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceSnapshots)

responseGetBucketAccessKeys :: GetBucketAccessKeysResponse -> TestTree
responseGetBucketAccessKeys =
  res
    "GetBucketAccessKeysResponse"
    "fixture/GetBucketAccessKeysResponse.proto"
    defaultService
    (Proxy :: Proxy GetBucketAccessKeys)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions =
  res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRegions)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy :: Proxy SetIpAddressType)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot =
  res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDiskSnapshot)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm =
  res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy TestAlarm)

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

responseAttachStaticIp :: AttachStaticIpResponse -> TestTree
responseAttachStaticIp =
  res
    "AttachStaticIpResponse"
    "fixture/AttachStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy AttachStaticIp)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBucket)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification =
  res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    defaultService
    (Proxy :: Proxy SendContactMethodVerification)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair =
  res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy DownloadDefaultKeyPair)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm =
  res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlarm)

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

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents =
  res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseEvents)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase =
  res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRelationalDatabase)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase =
  res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRelationalDatabase)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers =
  res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoadBalancers)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstance)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution =
  res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy AttachCertificateToDistribution)

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

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase =
  res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy RebootRelationalDatabase)

responseGetStaticIps :: GetStaticIpsResponse -> TestTree
responseGetStaticIps =
  res
    "GetStaticIpsResponse"
    "fixture/GetStaticIpsResponse.proto"
    defaultService
    (Proxy :: Proxy GetStaticIps)

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

responseDeleteBucketAccessKey :: DeleteBucketAccessKeyResponse -> TestTree
responseDeleteBucketAccessKey =
  res
    "DeleteBucketAccessKeyResponse"
    "fixture/DeleteBucketAccessKeyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBucketAccessKey)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomain)

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

responseSetResourceAccessForBucket :: SetResourceAccessForBucketResponse -> TestTree
responseSetResourceAccessForBucket =
  res
    "SetResourceAccessForBucketResponse"
    "fixture/SetResourceAccessForBucketResponse.proto"
    defaultService
    (Proxy :: Proxy SetResourceAccessForBucket)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomain)

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

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate =
  res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCertificate)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles =
  res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBundles)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys =
  res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteKnownHostKeys)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances =
  res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateInstances)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames =
  res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    defaultService
    (Proxy :: Proxy GetActiveNames)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService =
  res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerService)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase =
  res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy StopRelationalDatabase)

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

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset =
  res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionLatestCacheReset)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts =
  res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    defaultService
    (Proxy :: Proxy OpenInstancePublicPorts)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot =
  res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabaseSnapshot)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots =
  res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAutoSnapshots)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase =
  res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy StartRelationalDatabase)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService =
  res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateContainerService)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDistribution)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy :: Proxy GetOperation)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair =
  res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy GetKeyPair)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDistribution)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams =
  res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseLogStreams)

responseUpdateBucketBundle :: UpdateBucketBundleResponse -> TestTree
responseUpdateBucketBundle =
  res
    "UpdateBucketBundleResponse"
    "fixture/UpdateBucketBundleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBucketBundle)

responseGetBuckets :: GetBucketsResponse -> TestTree
responseGetBuckets =
  res
    "GetBucketsResponse"
    "fixture/GetBucketsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBuckets)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot =
  res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRelationalDatabaseSnapshot)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache =
  res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDistributionCache)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot =
  res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceSnapshot)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod =
  res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContactMethod)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts =
  res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    defaultService
    (Proxy :: Proxy PutInstancePublicPorts)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles =
  res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionBundles)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService =
  res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteContainerService)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack =
  res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCloudFormationStack)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers =
  res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServicePowers)

responseReleaseStaticIp :: ReleaseStaticIpResponse -> TestTree
responseReleaseStaticIp =
  res
    "ReleaseStaticIpResponse"
    "fixture/ReleaseStaticIpResponse.proto"
    defaultService
    (Proxy :: Proxy ReleaseStaticIp)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks =
  res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    defaultService
    (Proxy :: Proxy GetDisks)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy :: Proxy ImportKeyPair)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry =
  res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDomainEntry)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin =
  res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    defaultService
    (Proxy :: Proxy CreateContainerServiceRegistryLogin)

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

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData =
  res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetDistributionMetricData)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState =
  res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    defaultService
    (Proxy :: Proxy GetInstanceState)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData =
  res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerServiceMetricData)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry =
  res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDomainEntry)

responseDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificateResponse -> TestTree
responseDeleteLoadBalancerTlsCertificate =
  res
    "DeleteLoadBalancerTlsCertificateResponse"
    "fixture/DeleteLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoadBalancerTlsCertificate)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages =
  res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerImages)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters =
  res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRelationalDatabaseParameters)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains =
  res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    defaultService
    (Proxy :: Proxy GetDomains)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry =
  res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDomainEntry)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog =
  res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    defaultService
    (Proxy :: Proxy GetContainerLog)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm =
  res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    defaultService
    (Proxy :: Proxy PutAlarm)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters =
  res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy :: Proxy GetRelationalDatabaseParameters)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot =
  res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAutoSnapshot)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase =
  res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy :: Proxy CreateRelationalDatabase)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods =
  res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    defaultService
    (Proxy :: Proxy GetContactMethods)
