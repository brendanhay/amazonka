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

import qualified Data.Proxy as Proxy
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
--         [ requestCloseInstancePublicPorts $
--             newCloseInstancePublicPorts
--
--         , requestGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricData
--
--         , requestDeleteBucketAccessKey $
--             newDeleteBucketAccessKey
--
--         , requestAllocateStaticIp $
--             newAllocateStaticIp
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshot
--
--         , requestGetInstances $
--             newGetInstances
--
--         , requestGetLoadBalancer $
--             newGetLoadBalancer
--
--         , requestDisableAddOn $
--             newDisableAddOn
--
--         , requestGetDistributions $
--             newGetDistributions
--
--         , requestCreateContainerServiceDeployment $
--             newCreateContainerServiceDeployment
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestUpdateBucket $
--             newUpdateBucket
--
--         , requestGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEvents
--
--         , requestAttachCertificateToDistribution $
--             newAttachCertificateToDistribution
--
--         , requestGetContainerServices $
--             newGetContainerServices
--
--         , requestUpdateDistributionBundle $
--             newUpdateDistributionBundle
--
--         , requestGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshots
--
--         , requestGetBucketBundles $
--             newGetBucketBundles
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestAttachStaticIp $
--             newAttachStaticIp
--
--         , requestGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParameters
--
--         , requestDetachDisk $
--             newDetachDisk
--
--         , requestGetContactMethods $
--             newGetContactMethods
--
--         , requestDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPair
--
--         , requestDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificate
--
--         , requestTestAlarm $
--             newTestAlarm
--
--         , requestGetDomains $
--             newGetDomains
--
--         , requestGetContainerImages $
--             newGetContainerImages
--
--         , requestUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParameters
--
--         , requestCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificate
--
--         , requestCreateDomainEntry $
--             newCreateDomainEntry
--
--         , requestGetContainerServicePowers $
--             newGetContainerServicePowers
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestGetInstanceSnapshots $
--             newGetInstanceSnapshots
--
--         , requestExportSnapshot $
--             newExportSnapshot
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshot
--
--         , requestCreateCloudFormationStack $
--             newCreateCloudFormationStack
--
--         , requestGetExportSnapshotRecords $
--             newGetExportSnapshotRecords
--
--         , requestReleaseStaticIp $
--             newReleaseStaticIp
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLogin
--
--         , requestGetCertificates $
--             newGetCertificates
--
--         , requestGetContainerServiceMetricData $
--             newGetContainerServiceMetricData
--
--         , requestGetDistributionMetricData $
--             newGetDistributionMetricData
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshot
--
--         , requestGetRelationalDatabases $
--             newGetRelationalDatabases
--
--         , requestGetInstanceSnapshot $
--             newGetInstanceSnapshot
--
--         , requestGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEvents
--
--         , requestCreateContactMethod $
--             newCreateContactMethod
--
--         , requestGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreams
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestGetAutoSnapshots $
--             newGetAutoSnapshots
--
--         , requestGetActiveNames $
--             newGetActiveNames
--
--         , requestDeleteContactMethod $
--             newDeleteContactMethod
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestStopRelationalDatabase $
--             newStopRelationalDatabase
--
--         , requestCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshot
--
--         , requestDetachCertificateFromDistribution $
--             newDetachCertificateFromDistribution
--
--         , requestCreateContainerService $
--             newCreateContainerService
--
--         , requestGetInstanceAccessDetails $
--             newGetInstanceAccessDetails
--
--         , requestEnableAddOn $
--             newEnableAddOn
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancer
--
--         , requestRegisterContainerImage $
--             newRegisterContainerImage
--
--         , requestCreateCertificate $
--             newCreateCertificate
--
--         , requestCreateInstanceSnapshot $
--             newCreateInstanceSnapshot
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshot
--
--         , requestIsVpcPeered $
--             newIsVpcPeered
--
--         , requestGetStaticIps $
--             newGetStaticIps
--
--         , requestUnpeerVpc $
--             newUnpeerVpc
--
--         , requestDeleteDisk $
--             newDeleteDisk
--
--         , requestCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshot
--
--         , requestGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecords
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprints
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestGetDiskSnapshots $
--             newGetDiskSnapshots
--
--         , requestGetContainerAPIMetadata $
--             newGetContainerAPIMetadata
--
--         , requestGetBucketMetricData $
--             newGetBucketMetricData
--
--         , requestPeerVpc $
--             newPeerVpc
--
--         , requestGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundles
--
--         , requestGetLoadBalancers $
--             newGetLoadBalancers
--
--         , requestRebootRelationalDatabase $
--             newRebootRelationalDatabase
--
--         , requestAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificate
--
--         , requestUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttribute
--
--         , requestDeleteRelationalDatabase $
--             newDeleteRelationalDatabase
--
--         , requestGetDiskSnapshot $
--             newGetDiskSnapshot
--
--         , requestUpdateRelationalDatabase $
--             newUpdateRelationalDatabase
--
--         , requestGetStaticIp $
--             newGetStaticIp
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPassword
--
--         , requestGetBlueprints $
--             newGetBlueprints
--
--         , requestPutAlarm $
--             newPutAlarm
--
--         , requestDeleteAlarm $
--             newDeleteAlarm
--
--         , requestGetInstancePortStates $
--             newGetInstancePortStates
--
--         , requestDeleteAutoSnapshot $
--             newDeleteAutoSnapshot
--
--         , requestCreateRelationalDatabase $
--             newCreateRelationalDatabase
--
--         , requestSendContactMethodVerification $
--             newSendContactMethodVerification
--
--         , requestGetContainerLog $
--             newGetContainerLog
--
--         , requestCreateDiskSnapshot $
--             newCreateDiskSnapshot
--
--         , requestDeleteDomainEntry $
--             newDeleteDomainEntry
--
--         , requestUpdateDomainEntry $
--             newUpdateDomainEntry
--
--         , requestGetRegions $
--             newGetRegions
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestDeleteDiskSnapshot $
--             newDeleteDiskSnapshot
--
--         , requestGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricData
--
--         , requestGetInstanceState $
--             newGetInstanceState
--
--         , requestGetKeyPairs $
--             newGetKeyPairs
--
--         , requestGetOperations $
--             newGetOperations
--
--         , requestGetBucketAccessKeys $
--             newGetBucketAccessKeys
--
--         , requestGetDisks $
--             newGetDisks
--
--         , requestGetRelationalDatabase $
--             newGetRelationalDatabase
--
--         , requestAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancer
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestResetDistributionCache $
--             newResetDistributionCache
--
--         , requestUpdateBucketBundle $
--             newUpdateBucketBundle
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestGetBuckets $
--             newGetBuckets
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestUpdateContainerService $
--             newUpdateContainerService
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshot
--
--         , requestDeleteContainerService $
--             newDeleteContainerService
--
--         , requestGetInstanceMetricData $
--             newGetInstanceMetricData
--
--         , requestGetKeyPair $
--             newGetKeyPair
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestPutInstancePublicPorts $
--             newPutInstancePublicPorts
--
--         , requestGetDistributionBundles $
--             newGetDistributionBundles
--
--         , requestGetDisk $
--             newGetDisk
--
--         , requestGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheReset
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestGetContainerServiceDeployments $
--             newGetContainerServiceDeployments
--
--         , requestDeleteKnownHostKeys $
--             newDeleteKnownHostKeys
--
--         , requestAttachDisk $
--             newAttachDisk
--
--         , requestDetachStaticIp $
--             newDetachStaticIp
--
--         , requestCreateInstances $
--             newCreateInstances
--
--         , requestGetAlarms $
--             newGetAlarms
--
--         , requestOpenInstancePublicPorts $
--             newOpenInstancePublicPorts
--
--         , requestStartRelationalDatabase $
--             newStartRelationalDatabase
--
--         , requestDeleteContainerImage $
--             newDeleteContainerImage
--
--         , requestGetBundles $
--             newGetBundles
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificates
--
--         , requestSetResourceAccessForBucket $
--             newSetResourceAccessForBucket
--
--         , requestCreateDisk $
--             newCreateDisk
--
--         , requestCreateBucketAccessKey $
--             newCreateBucketAccessKey
--
--         , requestGetOperationsForResource $
--             newGetOperationsForResource
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestStartInstance $
--             newStartInstance
--
--           ]

--     , testGroup "response"
--         [ responseCloseInstancePublicPorts $
--             newCloseInstancePublicPortsResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricDataResponse
--
--         , responseDeleteBucketAccessKey $
--             newDeleteBucketAccessKeyResponse
--
--         , responseAllocateStaticIp $
--             newAllocateStaticIpResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshotResponse
--
--         , responseGetInstances $
--             newGetInstancesResponse
--
--         , responseGetLoadBalancer $
--             newGetLoadBalancerResponse
--
--         , responseDisableAddOn $
--             newDisableAddOnResponse
--
--         , responseGetDistributions $
--             newGetDistributionsResponse
--
--         , responseCreateContainerServiceDeployment $
--             newCreateContainerServiceDeploymentResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseUpdateBucket $
--             newUpdateBucketResponse
--
--         , responseGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEventsResponse
--
--         , responseAttachCertificateToDistribution $
--             newAttachCertificateToDistributionResponse
--
--         , responseGetContainerServices $
--             newGetContainerServicesResponse
--
--         , responseUpdateDistributionBundle $
--             newUpdateDistributionBundleResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshotsResponse
--
--         , responseGetBucketBundles $
--             newGetBucketBundlesResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseAttachStaticIp $
--             newAttachStaticIpResponse
--
--         , responseGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParametersResponse
--
--         , responseDetachDisk $
--             newDetachDiskResponse
--
--         , responseGetContactMethods $
--             newGetContactMethodsResponse
--
--         , responseDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPairResponse
--
--         , responseDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificateResponse
--
--         , responseTestAlarm $
--             newTestAlarmResponse
--
--         , responseGetDomains $
--             newGetDomainsResponse
--
--         , responseGetContainerImages $
--             newGetContainerImagesResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParametersResponse
--
--         , responseCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificateResponse
--
--         , responseCreateDomainEntry $
--             newCreateDomainEntryResponse
--
--         , responseGetContainerServicePowers $
--             newGetContainerServicePowersResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseGetInstanceSnapshots $
--             newGetInstanceSnapshotsResponse
--
--         , responseExportSnapshot $
--             newExportSnapshotResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshotResponse
--
--         , responseCreateCloudFormationStack $
--             newCreateCloudFormationStackResponse
--
--         , responseGetExportSnapshotRecords $
--             newGetExportSnapshotRecordsResponse
--
--         , responseReleaseStaticIp $
--             newReleaseStaticIpResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLoginResponse
--
--         , responseGetCertificates $
--             newGetCertificatesResponse
--
--         , responseGetContainerServiceMetricData $
--             newGetContainerServiceMetricDataResponse
--
--         , responseGetDistributionMetricData $
--             newGetDistributionMetricDataResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshotResponse
--
--         , responseGetRelationalDatabases $
--             newGetRelationalDatabasesResponse
--
--         , responseGetInstanceSnapshot $
--             newGetInstanceSnapshotResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEventsResponse
--
--         , responseCreateContactMethod $
--             newCreateContactMethodResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreamsResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseGetAutoSnapshots $
--             newGetAutoSnapshotsResponse
--
--         , responseGetActiveNames $
--             newGetActiveNamesResponse
--
--         , responseDeleteContactMethod $
--             newDeleteContactMethodResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseStopRelationalDatabase $
--             newStopRelationalDatabaseResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshotResponse
--
--         , responseDetachCertificateFromDistribution $
--             newDetachCertificateFromDistributionResponse
--
--         , responseCreateContainerService $
--             newCreateContainerServiceResponse
--
--         , responseGetInstanceAccessDetails $
--             newGetInstanceAccessDetailsResponse
--
--         , responseEnableAddOn $
--             newEnableAddOnResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancerResponse
--
--         , responseRegisterContainerImage $
--             newRegisterContainerImageResponse
--
--         , responseCreateCertificate $
--             newCreateCertificateResponse
--
--         , responseCreateInstanceSnapshot $
--             newCreateInstanceSnapshotResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshotResponse
--
--         , responseIsVpcPeered $
--             newIsVpcPeeredResponse
--
--         , responseGetStaticIps $
--             newGetStaticIpsResponse
--
--         , responseUnpeerVpc $
--             newUnpeerVpcResponse
--
--         , responseDeleteDisk $
--             newDeleteDiskResponse
--
--         , responseCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshotResponse
--
--         , responseGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecordsResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprintsResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseGetDiskSnapshots $
--             newGetDiskSnapshotsResponse
--
--         , responseGetContainerAPIMetadata $
--             newGetContainerAPIMetadataResponse
--
--         , responseGetBucketMetricData $
--             newGetBucketMetricDataResponse
--
--         , responsePeerVpc $
--             newPeerVpcResponse
--
--         , responseGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundlesResponse
--
--         , responseGetLoadBalancers $
--             newGetLoadBalancersResponse
--
--         , responseRebootRelationalDatabase $
--             newRebootRelationalDatabaseResponse
--
--         , responseAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificateResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttributeResponse
--
--         , responseDeleteRelationalDatabase $
--             newDeleteRelationalDatabaseResponse
--
--         , responseGetDiskSnapshot $
--             newGetDiskSnapshotResponse
--
--         , responseUpdateRelationalDatabase $
--             newUpdateRelationalDatabaseResponse
--
--         , responseGetStaticIp $
--             newGetStaticIpResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetBlueprints $
--             newGetBlueprintsResponse
--
--         , responsePutAlarm $
--             newPutAlarmResponse
--
--         , responseDeleteAlarm $
--             newDeleteAlarmResponse
--
--         , responseGetInstancePortStates $
--             newGetInstancePortStatesResponse
--
--         , responseDeleteAutoSnapshot $
--             newDeleteAutoSnapshotResponse
--
--         , responseCreateRelationalDatabase $
--             newCreateRelationalDatabaseResponse
--
--         , responseSendContactMethodVerification $
--             newSendContactMethodVerificationResponse
--
--         , responseGetContainerLog $
--             newGetContainerLogResponse
--
--         , responseCreateDiskSnapshot $
--             newCreateDiskSnapshotResponse
--
--         , responseDeleteDomainEntry $
--             newDeleteDomainEntryResponse
--
--         , responseUpdateDomainEntry $
--             newUpdateDomainEntryResponse
--
--         , responseGetRegions $
--             newGetRegionsResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseDeleteDiskSnapshot $
--             newDeleteDiskSnapshotResponse
--
--         , responseGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricDataResponse
--
--         , responseGetInstanceState $
--             newGetInstanceStateResponse
--
--         , responseGetKeyPairs $
--             newGetKeyPairsResponse
--
--         , responseGetOperations $
--             newGetOperationsResponse
--
--         , responseGetBucketAccessKeys $
--             newGetBucketAccessKeysResponse
--
--         , responseGetDisks $
--             newGetDisksResponse
--
--         , responseGetRelationalDatabase $
--             newGetRelationalDatabaseResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancerResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseResetDistributionCache $
--             newResetDistributionCacheResponse
--
--         , responseUpdateBucketBundle $
--             newUpdateBucketBundleResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseGetBuckets $
--             newGetBucketsResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseUpdateContainerService $
--             newUpdateContainerServiceResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshotResponse
--
--         , responseDeleteContainerService $
--             newDeleteContainerServiceResponse
--
--         , responseGetInstanceMetricData $
--             newGetInstanceMetricDataResponse
--
--         , responseGetKeyPair $
--             newGetKeyPairResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responsePutInstancePublicPorts $
--             newPutInstancePublicPortsResponse
--
--         , responseGetDistributionBundles $
--             newGetDistributionBundlesResponse
--
--         , responseGetDisk $
--             newGetDiskResponse
--
--         , responseGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheResetResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseGetContainerServiceDeployments $
--             newGetContainerServiceDeploymentsResponse
--
--         , responseDeleteKnownHostKeys $
--             newDeleteKnownHostKeysResponse
--
--         , responseAttachDisk $
--             newAttachDiskResponse
--
--         , responseDetachStaticIp $
--             newDetachStaticIpResponse
--
--         , responseCreateInstances $
--             newCreateInstancesResponse
--
--         , responseGetAlarms $
--             newGetAlarmsResponse
--
--         , responseOpenInstancePublicPorts $
--             newOpenInstancePublicPortsResponse
--
--         , responseStartRelationalDatabase $
--             newStartRelationalDatabaseResponse
--
--         , responseDeleteContainerImage $
--             newDeleteContainerImageResponse
--
--         , responseGetBundles $
--             newGetBundlesResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificatesResponse
--
--         , responseSetResourceAccessForBucket $
--             newSetResourceAccessForBucketResponse
--
--         , responseCreateDisk $
--             newCreateDiskResponse
--
--         , responseCreateBucketAccessKey $
--             newCreateBucketAccessKeyResponse
--
--         , responseGetOperationsForResource $
--             newGetOperationsForResourceResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--           ]
--     ]

-- Requests

requestCloseInstancePublicPorts :: CloseInstancePublicPorts -> TestTree
requestCloseInstancePublicPorts =
  req
    "CloseInstancePublicPorts"
    "fixture/CloseInstancePublicPorts.yaml"

requestGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricData -> TestTree
requestGetRelationalDatabaseMetricData =
  req
    "GetRelationalDatabaseMetricData"
    "fixture/GetRelationalDatabaseMetricData.yaml"

requestDeleteBucketAccessKey :: DeleteBucketAccessKey -> TestTree
requestDeleteBucketAccessKey =
  req
    "DeleteBucketAccessKey"
    "fixture/DeleteBucketAccessKey.yaml"

requestAllocateStaticIp :: AllocateStaticIp -> TestTree
requestAllocateStaticIp =
  req
    "AllocateStaticIp"
    "fixture/AllocateStaticIp.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot =
  req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

requestGetInstances :: GetInstances -> TestTree
requestGetInstances =
  req
    "GetInstances"
    "fixture/GetInstances.yaml"

requestGetLoadBalancer :: GetLoadBalancer -> TestTree
requestGetLoadBalancer =
  req
    "GetLoadBalancer"
    "fixture/GetLoadBalancer.yaml"

requestDisableAddOn :: DisableAddOn -> TestTree
requestDisableAddOn =
  req
    "DisableAddOn"
    "fixture/DisableAddOn.yaml"

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

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestUpdateBucket :: UpdateBucket -> TestTree
requestUpdateBucket =
  req
    "UpdateBucket"
    "fixture/UpdateBucket.yaml"

requestGetRelationalDatabaseEvents :: GetRelationalDatabaseEvents -> TestTree
requestGetRelationalDatabaseEvents =
  req
    "GetRelationalDatabaseEvents"
    "fixture/GetRelationalDatabaseEvents.yaml"

requestAttachCertificateToDistribution :: AttachCertificateToDistribution -> TestTree
requestAttachCertificateToDistribution =
  req
    "AttachCertificateToDistribution"
    "fixture/AttachCertificateToDistribution.yaml"

requestGetContainerServices :: GetContainerServices -> TestTree
requestGetContainerServices =
  req
    "GetContainerServices"
    "fixture/GetContainerServices.yaml"

requestUpdateDistributionBundle :: UpdateDistributionBundle -> TestTree
requestUpdateDistributionBundle =
  req
    "UpdateDistributionBundle"
    "fixture/UpdateDistributionBundle.yaml"

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

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestAttachStaticIp :: AttachStaticIp -> TestTree
requestAttachStaticIp =
  req
    "AttachStaticIp"
    "fixture/AttachStaticIp.yaml"

requestGetRelationalDatabaseParameters :: GetRelationalDatabaseParameters -> TestTree
requestGetRelationalDatabaseParameters =
  req
    "GetRelationalDatabaseParameters"
    "fixture/GetRelationalDatabaseParameters.yaml"

requestDetachDisk :: DetachDisk -> TestTree
requestDetachDisk =
  req
    "DetachDisk"
    "fixture/DetachDisk.yaml"

requestGetContactMethods :: GetContactMethods -> TestTree
requestGetContactMethods =
  req
    "GetContactMethods"
    "fixture/GetContactMethods.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair =
  req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificate -> TestTree
requestDeleteLoadBalancerTlsCertificate =
  req
    "DeleteLoadBalancerTlsCertificate"
    "fixture/DeleteLoadBalancerTlsCertificate.yaml"

requestTestAlarm :: TestAlarm -> TestTree
requestTestAlarm =
  req
    "TestAlarm"
    "fixture/TestAlarm.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains =
  req
    "GetDomains"
    "fixture/GetDomains.yaml"

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

requestCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificate -> TestTree
requestCreateLoadBalancerTlsCertificate =
  req
    "CreateLoadBalancerTlsCertificate"
    "fixture/CreateLoadBalancerTlsCertificate.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry =
  req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestGetContainerServicePowers :: GetContainerServicePowers -> TestTree
requestGetContainerServicePowers =
  req
    "GetContainerServicePowers"
    "fixture/GetContainerServicePowers.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots =
  req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

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

requestCreateCloudFormationStack :: CreateCloudFormationStack -> TestTree
requestCreateCloudFormationStack =
  req
    "CreateCloudFormationStack"
    "fixture/CreateCloudFormationStack.yaml"

requestGetExportSnapshotRecords :: GetExportSnapshotRecords -> TestTree
requestGetExportSnapshotRecords =
  req
    "GetExportSnapshotRecords"
    "fixture/GetExportSnapshotRecords.yaml"

requestReleaseStaticIp :: ReleaseStaticIp -> TestTree
requestReleaseStaticIp =
  req
    "ReleaseStaticIp"
    "fixture/ReleaseStaticIp.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLogin -> TestTree
requestCreateContainerServiceRegistryLogin =
  req
    "CreateContainerServiceRegistryLogin"
    "fixture/CreateContainerServiceRegistryLogin.yaml"

requestGetCertificates :: GetCertificates -> TestTree
requestGetCertificates =
  req
    "GetCertificates"
    "fixture/GetCertificates.yaml"

requestGetContainerServiceMetricData :: GetContainerServiceMetricData -> TestTree
requestGetContainerServiceMetricData =
  req
    "GetContainerServiceMetricData"
    "fixture/GetContainerServiceMetricData.yaml"

requestGetDistributionMetricData :: GetDistributionMetricData -> TestTree
requestGetDistributionMetricData =
  req
    "GetDistributionMetricData"
    "fixture/GetDistributionMetricData.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestCreateDiskFromSnapshot :: CreateDiskFromSnapshot -> TestTree
requestCreateDiskFromSnapshot =
  req
    "CreateDiskFromSnapshot"
    "fixture/CreateDiskFromSnapshot.yaml"

requestGetRelationalDatabases :: GetRelationalDatabases -> TestTree
requestGetRelationalDatabases =
  req
    "GetRelationalDatabases"
    "fixture/GetRelationalDatabases.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot =
  req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEvents -> TestTree
requestGetRelationalDatabaseLogEvents =
  req
    "GetRelationalDatabaseLogEvents"
    "fixture/GetRelationalDatabaseLogEvents.yaml"

requestCreateContactMethod :: CreateContactMethod -> TestTree
requestCreateContactMethod =
  req
    "CreateContactMethod"
    "fixture/CreateContactMethod.yaml"

requestGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreams -> TestTree
requestGetRelationalDatabaseLogStreams =
  req
    "GetRelationalDatabaseLogStreams"
    "fixture/GetRelationalDatabaseLogStreams.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetAutoSnapshots :: GetAutoSnapshots -> TestTree
requestGetAutoSnapshots =
  req
    "GetAutoSnapshots"
    "fixture/GetAutoSnapshots.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames =
  req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestDeleteContactMethod :: DeleteContactMethod -> TestTree
requestDeleteContactMethod =
  req
    "DeleteContactMethod"
    "fixture/DeleteContactMethod.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestStopRelationalDatabase :: StopRelationalDatabase -> TestTree
requestStopRelationalDatabase =
  req
    "StopRelationalDatabase"
    "fixture/StopRelationalDatabase.yaml"

requestCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshot -> TestTree
requestCreateRelationalDatabaseSnapshot =
  req
    "CreateRelationalDatabaseSnapshot"
    "fixture/CreateRelationalDatabaseSnapshot.yaml"

requestDetachCertificateFromDistribution :: DetachCertificateFromDistribution -> TestTree
requestDetachCertificateFromDistribution =
  req
    "DetachCertificateFromDistribution"
    "fixture/DetachCertificateFromDistribution.yaml"

requestCreateContainerService :: CreateContainerService -> TestTree
requestCreateContainerService =
  req
    "CreateContainerService"
    "fixture/CreateContainerService.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails =
  req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestEnableAddOn :: EnableAddOn -> TestTree
requestEnableAddOn =
  req
    "EnableAddOn"
    "fixture/EnableAddOn.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancer -> TestTree
requestDetachInstancesFromLoadBalancer =
  req
    "DetachInstancesFromLoadBalancer"
    "fixture/DetachInstancesFromLoadBalancer.yaml"

requestRegisterContainerImage :: RegisterContainerImage -> TestTree
requestRegisterContainerImage =
  req
    "RegisterContainerImage"
    "fixture/RegisterContainerImage.yaml"

requestCreateCertificate :: CreateCertificate -> TestTree
requestCreateCertificate =
  req
    "CreateCertificate"
    "fixture/CreateCertificate.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot =
  req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshot -> TestTree
requestGetRelationalDatabaseSnapshot =
  req
    "GetRelationalDatabaseSnapshot"
    "fixture/GetRelationalDatabaseSnapshot.yaml"

requestIsVpcPeered :: IsVpcPeered -> TestTree
requestIsVpcPeered =
  req
    "IsVpcPeered"
    "fixture/IsVpcPeered.yaml"

requestGetStaticIps :: GetStaticIps -> TestTree
requestGetStaticIps =
  req
    "GetStaticIps"
    "fixture/GetStaticIps.yaml"

requestUnpeerVpc :: UnpeerVpc -> TestTree
requestUnpeerVpc =
  req
    "UnpeerVpc"
    "fixture/UnpeerVpc.yaml"

requestDeleteDisk :: DeleteDisk -> TestTree
requestDeleteDisk =
  req
    "DeleteDisk"
    "fixture/DeleteDisk.yaml"

requestCreateInstancesFromSnapshot :: CreateInstancesFromSnapshot -> TestTree
requestCreateInstancesFromSnapshot =
  req
    "CreateInstancesFromSnapshot"
    "fixture/CreateInstancesFromSnapshot.yaml"

requestGetCloudFormationStackRecords :: GetCloudFormationStackRecords -> TestTree
requestGetCloudFormationStackRecords =
  req
    "GetCloudFormationStackRecords"
    "fixture/GetCloudFormationStackRecords.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprints -> TestTree
requestGetRelationalDatabaseBlueprints =
  req
    "GetRelationalDatabaseBlueprints"
    "fixture/GetRelationalDatabaseBlueprints.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestGetDiskSnapshots :: GetDiskSnapshots -> TestTree
requestGetDiskSnapshots =
  req
    "GetDiskSnapshots"
    "fixture/GetDiskSnapshots.yaml"

requestGetContainerAPIMetadata :: GetContainerAPIMetadata -> TestTree
requestGetContainerAPIMetadata =
  req
    "GetContainerAPIMetadata"
    "fixture/GetContainerAPIMetadata.yaml"

requestGetBucketMetricData :: GetBucketMetricData -> TestTree
requestGetBucketMetricData =
  req
    "GetBucketMetricData"
    "fixture/GetBucketMetricData.yaml"

requestPeerVpc :: PeerVpc -> TestTree
requestPeerVpc =
  req
    "PeerVpc"
    "fixture/PeerVpc.yaml"

requestGetRelationalDatabaseBundles :: GetRelationalDatabaseBundles -> TestTree
requestGetRelationalDatabaseBundles =
  req
    "GetRelationalDatabaseBundles"
    "fixture/GetRelationalDatabaseBundles.yaml"

requestGetLoadBalancers :: GetLoadBalancers -> TestTree
requestGetLoadBalancers =
  req
    "GetLoadBalancers"
    "fixture/GetLoadBalancers.yaml"

requestRebootRelationalDatabase :: RebootRelationalDatabase -> TestTree
requestRebootRelationalDatabase =
  req
    "RebootRelationalDatabase"
    "fixture/RebootRelationalDatabase.yaml"

requestAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificate -> TestTree
requestAttachLoadBalancerTlsCertificate =
  req
    "AttachLoadBalancerTlsCertificate"
    "fixture/AttachLoadBalancerTlsCertificate.yaml"

requestUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttribute -> TestTree
requestUpdateLoadBalancerAttribute =
  req
    "UpdateLoadBalancerAttribute"
    "fixture/UpdateLoadBalancerAttribute.yaml"

requestDeleteRelationalDatabase :: DeleteRelationalDatabase -> TestTree
requestDeleteRelationalDatabase =
  req
    "DeleteRelationalDatabase"
    "fixture/DeleteRelationalDatabase.yaml"

requestGetDiskSnapshot :: GetDiskSnapshot -> TestTree
requestGetDiskSnapshot =
  req
    "GetDiskSnapshot"
    "fixture/GetDiskSnapshot.yaml"

requestUpdateRelationalDatabase :: UpdateRelationalDatabase -> TestTree
requestUpdateRelationalDatabase =
  req
    "UpdateRelationalDatabase"
    "fixture/UpdateRelationalDatabase.yaml"

requestGetStaticIp :: GetStaticIp -> TestTree
requestGetStaticIp =
  req
    "GetStaticIp"
    "fixture/GetStaticIp.yaml"

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

requestPutAlarm :: PutAlarm -> TestTree
requestPutAlarm =
  req
    "PutAlarm"
    "fixture/PutAlarm.yaml"

requestDeleteAlarm :: DeleteAlarm -> TestTree
requestDeleteAlarm =
  req
    "DeleteAlarm"
    "fixture/DeleteAlarm.yaml"

requestGetInstancePortStates :: GetInstancePortStates -> TestTree
requestGetInstancePortStates =
  req
    "GetInstancePortStates"
    "fixture/GetInstancePortStates.yaml"

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

requestSendContactMethodVerification :: SendContactMethodVerification -> TestTree
requestSendContactMethodVerification =
  req
    "SendContactMethodVerification"
    "fixture/SendContactMethodVerification.yaml"

requestGetContainerLog :: GetContainerLog -> TestTree
requestGetContainerLog =
  req
    "GetContainerLog"
    "fixture/GetContainerLog.yaml"

requestCreateDiskSnapshot :: CreateDiskSnapshot -> TestTree
requestCreateDiskSnapshot =
  req
    "CreateDiskSnapshot"
    "fixture/CreateDiskSnapshot.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry =
  req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry =
  req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

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

requestDeleteDiskSnapshot :: DeleteDiskSnapshot -> TestTree
requestDeleteDiskSnapshot =
  req
    "DeleteDiskSnapshot"
    "fixture/DeleteDiskSnapshot.yaml"

requestGetLoadBalancerMetricData :: GetLoadBalancerMetricData -> TestTree
requestGetLoadBalancerMetricData =
  req
    "GetLoadBalancerMetricData"
    "fixture/GetLoadBalancerMetricData.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState =
  req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

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

requestGetBucketAccessKeys :: GetBucketAccessKeys -> TestTree
requestGetBucketAccessKeys =
  req
    "GetBucketAccessKeys"
    "fixture/GetBucketAccessKeys.yaml"

requestGetDisks :: GetDisks -> TestTree
requestGetDisks =
  req
    "GetDisks"
    "fixture/GetDisks.yaml"

requestGetRelationalDatabase :: GetRelationalDatabase -> TestTree
requestGetRelationalDatabase =
  req
    "GetRelationalDatabase"
    "fixture/GetRelationalDatabase.yaml"

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

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestResetDistributionCache :: ResetDistributionCache -> TestTree
requestResetDistributionCache =
  req
    "ResetDistributionCache"
    "fixture/ResetDistributionCache.yaml"

requestUpdateBucketBundle :: UpdateBucketBundle -> TestTree
requestUpdateBucketBundle =
  req
    "UpdateBucketBundle"
    "fixture/UpdateBucketBundle.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestGetBuckets :: GetBuckets -> TestTree
requestGetBuckets =
  req
    "GetBuckets"
    "fixture/GetBuckets.yaml"

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

requestDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshot -> TestTree
requestDeleteRelationalDatabaseSnapshot =
  req
    "DeleteRelationalDatabaseSnapshot"
    "fixture/DeleteRelationalDatabaseSnapshot.yaml"

requestDeleteContainerService :: DeleteContainerService -> TestTree
requestDeleteContainerService =
  req
    "DeleteContainerService"
    "fixture/DeleteContainerService.yaml"

requestGetInstanceMetricData :: GetInstanceMetricData -> TestTree
requestGetInstanceMetricData =
  req
    "GetInstanceMetricData"
    "fixture/GetInstanceMetricData.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair =
  req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestGetDisk :: GetDisk -> TestTree
requestGetDisk =
  req
    "GetDisk"
    "fixture/GetDisk.yaml"

requestGetDistributionLatestCacheReset :: GetDistributionLatestCacheReset -> TestTree
requestGetDistributionLatestCacheReset =
  req
    "GetDistributionLatestCacheReset"
    "fixture/GetDistributionLatestCacheReset.yaml"

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

requestDeleteKnownHostKeys :: DeleteKnownHostKeys -> TestTree
requestDeleteKnownHostKeys =
  req
    "DeleteKnownHostKeys"
    "fixture/DeleteKnownHostKeys.yaml"

requestAttachDisk :: AttachDisk -> TestTree
requestAttachDisk =
  req
    "AttachDisk"
    "fixture/AttachDisk.yaml"

requestDetachStaticIp :: DetachStaticIp -> TestTree
requestDetachStaticIp =
  req
    "DetachStaticIp"
    "fixture/DetachStaticIp.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances =
  req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestGetAlarms :: GetAlarms -> TestTree
requestGetAlarms =
  req
    "GetAlarms"
    "fixture/GetAlarms.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts =
  req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestStartRelationalDatabase :: StartRelationalDatabase -> TestTree
requestStartRelationalDatabase =
  req
    "StartRelationalDatabase"
    "fixture/StartRelationalDatabase.yaml"

requestDeleteContainerImage :: DeleteContainerImage -> TestTree
requestDeleteContainerImage =
  req
    "DeleteContainerImage"
    "fixture/DeleteContainerImage.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles =
  req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificates -> TestTree
requestGetLoadBalancerTlsCertificates =
  req
    "GetLoadBalancerTlsCertificates"
    "fixture/GetLoadBalancerTlsCertificates.yaml"

requestSetResourceAccessForBucket :: SetResourceAccessForBucket -> TestTree
requestSetResourceAccessForBucket =
  req
    "SetResourceAccessForBucket"
    "fixture/SetResourceAccessForBucket.yaml"

requestCreateDisk :: CreateDisk -> TestTree
requestCreateDisk =
  req
    "CreateDisk"
    "fixture/CreateDisk.yaml"

requestCreateBucketAccessKey :: CreateBucketAccessKey -> TestTree
requestCreateBucketAccessKey =
  req
    "CreateBucketAccessKey"
    "fixture/CreateBucketAccessKey.yaml"

requestGetOperationsForResource :: GetOperationsForResource -> TestTree
requestGetOperationsForResource =
  req
    "GetOperationsForResource"
    "fixture/GetOperationsForResource.yaml"

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

-- Responses

responseCloseInstancePublicPorts :: CloseInstancePublicPortsResponse -> TestTree
responseCloseInstancePublicPorts =
  res
    "CloseInstancePublicPortsResponse"
    "fixture/CloseInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloseInstancePublicPorts)

responseGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricDataResponse -> TestTree
responseGetRelationalDatabaseMetricData =
  res
    "GetRelationalDatabaseMetricDataResponse"
    "fixture/GetRelationalDatabaseMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseMetricData)

responseDeleteBucketAccessKey :: DeleteBucketAccessKeyResponse -> TestTree
responseDeleteBucketAccessKey =
  res
    "DeleteBucketAccessKeyResponse"
    "fixture/DeleteBucketAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketAccessKey)

responseAllocateStaticIp :: AllocateStaticIpResponse -> TestTree
responseAllocateStaticIp =
  res
    "AllocateStaticIpResponse"
    "fixture/AllocateStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateStaticIp)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyPair)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot =
  res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceSnapshot)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances =
  res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstances)

responseGetLoadBalancer :: GetLoadBalancerResponse -> TestTree
responseGetLoadBalancer =
  res
    "GetLoadBalancerResponse"
    "fixture/GetLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancer)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn =
  res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAddOn)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions =
  res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributions)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment =
  res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerServiceDeployment)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstance)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucket)

responseUpdateBucket :: UpdateBucketResponse -> TestTree
responseUpdateBucket =
  res
    "UpdateBucketResponse"
    "fixture/UpdateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBucket)

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents =
  res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseEvents)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution =
  res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachCertificateToDistribution)

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices =
  res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServices)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle =
  res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistributionBundle)

responseGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> TestTree
responseGetRelationalDatabaseSnapshots =
  res
    "GetRelationalDatabaseSnapshotsResponse"
    "fixture/GetRelationalDatabaseSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseSnapshots)

responseGetBucketBundles :: GetBucketBundlesResponse -> TestTree
responseGetBucketBundles =
  res
    "GetBucketBundlesResponse"
    "fixture/GetBucketBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketBundles)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucket)

responseAttachStaticIp :: AttachStaticIpResponse -> TestTree
responseAttachStaticIp =
  res
    "AttachStaticIpResponse"
    "fixture/AttachStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachStaticIp)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters =
  res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseParameters)

responseDetachDisk :: DetachDiskResponse -> TestTree
responseDetachDisk =
  res
    "DetachDiskResponse"
    "fixture/DetachDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachDisk)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods =
  res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactMethods)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair =
  res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DownloadDefaultKeyPair)

responseDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificateResponse -> TestTree
responseDeleteLoadBalancerTlsCertificate =
  res
    "DeleteLoadBalancerTlsCertificateResponse"
    "fixture/DeleteLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancerTlsCertificate)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm =
  res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestAlarm)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains =
  res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomains)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages =
  res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerImages)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters =
  res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelationalDatabaseParameters)

responseCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificateResponse -> TestTree
responseCreateLoadBalancerTlsCertificate =
  res
    "CreateLoadBalancerTlsCertificateResponse"
    "fixture/CreateLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancerTlsCertificate)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry =
  res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainEntry)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers =
  res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServicePowers)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyPair)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots =
  res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceSnapshots)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportSnapshot)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot =
  res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabaseFromSnapshot)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack =
  res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFormationStack)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords =
  res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportSnapshotRecords)

responseReleaseStaticIp :: ReleaseStaticIpResponse -> TestTree
responseReleaseStaticIp =
  res
    "ReleaseStaticIpResponse"
    "fixture/ReleaseStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseStaticIp)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin =
  res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerServiceRegistryLogin)

responseGetCertificates :: GetCertificatesResponse -> TestTree
responseGetCertificates =
  res
    "GetCertificatesResponse"
    "fixture/GetCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificates)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData =
  res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServiceMetricData)

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData =
  res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionMetricData)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstance)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancer)

responseCreateDiskFromSnapshot :: CreateDiskFromSnapshotResponse -> TestTree
responseCreateDiskFromSnapshot =
  res
    "CreateDiskFromSnapshotResponse"
    "fixture/CreateDiskFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDiskFromSnapshot)

responseGetRelationalDatabases :: GetRelationalDatabasesResponse -> TestTree
responseGetRelationalDatabases =
  res
    "GetRelationalDatabasesResponse"
    "fixture/GetRelationalDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabases)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot =
  res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceSnapshot)

responseGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEventsResponse -> TestTree
responseGetRelationalDatabaseLogEvents =
  res
    "GetRelationalDatabaseLogEventsResponse"
    "fixture/GetRelationalDatabaseLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseLogEvents)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod =
  res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactMethod)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams =
  res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseLogStreams)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots =
  res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoSnapshots)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames =
  res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetActiveNames)

responseDeleteContactMethod :: DeleteContactMethodResponse -> TestTree
responseDeleteContactMethod =
  res
    "DeleteContactMethodResponse"
    "fixture/DeleteContactMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactMethod)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistribution)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase =
  res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRelationalDatabase)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot =
  res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabaseSnapshot)

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution =
  res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachCertificateFromDistribution)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService =
  res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerService)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails =
  res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceAccessDetails)

responseEnableAddOn :: EnableAddOnResponse -> TestTree
responseEnableAddOn =
  res
    "EnableAddOnResponse"
    "fixture/EnableAddOnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAddOn)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstance)

responseDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancerResponse -> TestTree
responseDetachInstancesFromLoadBalancer =
  res
    "DetachInstancesFromLoadBalancerResponse"
    "fixture/DetachInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInstancesFromLoadBalancer)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage =
  res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterContainerImage)

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate =
  res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificate)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot =
  res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceSnapshot)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> TestTree
responseGetRelationalDatabaseSnapshot =
  res
    "GetRelationalDatabaseSnapshotResponse"
    "fixture/GetRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseSnapshot)

responseIsVpcPeered :: IsVpcPeeredResponse -> TestTree
responseIsVpcPeered =
  res
    "IsVpcPeeredResponse"
    "fixture/IsVpcPeeredResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IsVpcPeered)

responseGetStaticIps :: GetStaticIpsResponse -> TestTree
responseGetStaticIps =
  res
    "GetStaticIpsResponse"
    "fixture/GetStaticIpsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStaticIps)

responseUnpeerVpc :: UnpeerVpcResponse -> TestTree
responseUnpeerVpc =
  res
    "UnpeerVpcResponse"
    "fixture/UnpeerVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnpeerVpc)

responseDeleteDisk :: DeleteDiskResponse -> TestTree
responseDeleteDisk =
  res
    "DeleteDiskResponse"
    "fixture/DeleteDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDisk)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot =
  res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstancesFromSnapshot)

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords =
  res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFormationStackRecords)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints =
  res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseBlueprints)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseGetDiskSnapshots :: GetDiskSnapshotsResponse -> TestTree
responseGetDiskSnapshots =
  res
    "GetDiskSnapshotsResponse"
    "fixture/GetDiskSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiskSnapshots)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata =
  res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerAPIMetadata)

responseGetBucketMetricData :: GetBucketMetricDataResponse -> TestTree
responseGetBucketMetricData =
  res
    "GetBucketMetricDataResponse"
    "fixture/GetBucketMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketMetricData)

responsePeerVpc :: PeerVpcResponse -> TestTree
responsePeerVpc =
  res
    "PeerVpcResponse"
    "fixture/PeerVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PeerVpc)

responseGetRelationalDatabaseBundles :: GetRelationalDatabaseBundlesResponse -> TestTree
responseGetRelationalDatabaseBundles =
  res
    "GetRelationalDatabaseBundlesResponse"
    "fixture/GetRelationalDatabaseBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseBundles)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers =
  res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancers)

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase =
  res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootRelationalDatabase)

responseAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificateResponse -> TestTree
responseAttachLoadBalancerTlsCertificate =
  res
    "AttachLoadBalancerTlsCertificateResponse"
    "fixture/AttachLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancerTlsCertificate)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute =
  res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoadBalancerAttribute)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase =
  res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRelationalDatabase)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot =
  res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiskSnapshot)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase =
  res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelationalDatabase)

responseGetStaticIp :: GetStaticIpResponse -> TestTree
responseGetStaticIp =
  res
    "GetStaticIpResponse"
    "fixture/GetStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStaticIp)

responseGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> TestTree
responseGetRelationalDatabaseMasterUserPassword =
  res
    "GetRelationalDatabaseMasterUserPasswordResponse"
    "fixture/GetRelationalDatabaseMasterUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseMasterUserPassword)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints =
  res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprints)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm =
  res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlarm)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm =
  res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlarm)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates =
  res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstancePortStates)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot =
  res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutoSnapshot)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase =
  res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabase)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification =
  res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendContactMethodVerification)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog =
  res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerLog)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot =
  res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDiskSnapshot)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry =
  res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainEntry)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry =
  res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainEntry)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions =
  res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegions)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIpAddressType)

responseDeleteDiskSnapshot :: DeleteDiskSnapshotResponse -> TestTree
responseDeleteDiskSnapshot =
  res
    "DeleteDiskSnapshotResponse"
    "fixture/DeleteDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDiskSnapshot)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData =
  res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancerMetricData)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState =
  res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceState)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs =
  res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPairs)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations =
  res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperations)

responseGetBucketAccessKeys :: GetBucketAccessKeysResponse -> TestTree
responseGetBucketAccessKeys =
  res
    "GetBucketAccessKeysResponse"
    "fixture/GetBucketAccessKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAccessKeys)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks =
  res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDisks)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase =
  res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabase)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer =
  res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInstancesToLoadBalancer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperation)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache =
  res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDistributionCache)

responseUpdateBucketBundle :: UpdateBucketBundleResponse -> TestTree
responseUpdateBucketBundle =
  res
    "UpdateBucketBundleResponse"
    "fixture/UpdateBucketBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBucketBundle)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistribution)

responseGetBuckets :: GetBucketsResponse -> TestTree
responseGetBuckets =
  res
    "GetBucketsResponse"
    "fixture/GetBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuckets)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDistribution)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService =
  res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerService)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot =
  res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRelationalDatabaseSnapshot)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService =
  res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerService)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData =
  res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceMetricData)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair =
  res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPair)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts =
  res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInstancePublicPorts)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles =
  res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionBundles)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk =
  res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDisk)

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset =
  res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionLatestCacheReset)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancer)

responseGetContainerServiceDeployments :: GetContainerServiceDeploymentsResponse -> TestTree
responseGetContainerServiceDeployments =
  res
    "GetContainerServiceDeploymentsResponse"
    "fixture/GetContainerServiceDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServiceDeployments)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys =
  res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKnownHostKeys)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk =
  res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachDisk)

responseDetachStaticIp :: DetachStaticIpResponse -> TestTree
responseDetachStaticIp =
  res
    "DetachStaticIpResponse"
    "fixture/DetachStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachStaticIp)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances =
  res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstances)

responseGetAlarms :: GetAlarmsResponse -> TestTree
responseGetAlarms =
  res
    "GetAlarmsResponse"
    "fixture/GetAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlarms)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts =
  res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OpenInstancePublicPorts)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase =
  res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRelationalDatabase)

responseDeleteContainerImage :: DeleteContainerImageResponse -> TestTree
responseDeleteContainerImage =
  res
    "DeleteContainerImageResponse"
    "fixture/DeleteContainerImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerImage)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles =
  res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBundles)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificatesResponse -> TestTree
responseGetLoadBalancerTlsCertificates =
  res
    "GetLoadBalancerTlsCertificatesResponse"
    "fixture/GetLoadBalancerTlsCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancerTlsCertificates)

responseSetResourceAccessForBucket :: SetResourceAccessForBucketResponse -> TestTree
responseSetResourceAccessForBucket =
  res
    "SetResourceAccessForBucketResponse"
    "fixture/SetResourceAccessForBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetResourceAccessForBucket)

responseCreateDisk :: CreateDiskResponse -> TestTree
responseCreateDisk =
  res
    "CreateDiskResponse"
    "fixture/CreateDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDisk)

responseCreateBucketAccessKey :: CreateBucketAccessKeyResponse -> TestTree
responseCreateBucketAccessKey =
  res
    "CreateBucketAccessKeyResponse"
    "fixture/CreateBucketAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucketAccessKey)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource =
  res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperationsForResource)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyPair)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstance)
