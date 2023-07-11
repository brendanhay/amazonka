{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Lightsail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Lightsail where

import Amazonka.Lightsail
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Lightsail.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAllocateStaticIp $
--             newAllocateStaticIp
--
--         , requestAttachCertificateToDistribution $
--             newAttachCertificateToDistribution
--
--         , requestAttachDisk $
--             newAttachDisk
--
--         , requestAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancer
--
--         , requestAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificate
--
--         , requestAttachStaticIp $
--             newAttachStaticIp
--
--         , requestCloseInstancePublicPorts $
--             newCloseInstancePublicPorts
--
--         , requestCopySnapshot $
--             newCopySnapshot
--
--         , requestCreateBucket $
--             newCreateBucket
--
--         , requestCreateBucketAccessKey $
--             newCreateBucketAccessKey
--
--         , requestCreateCertificate $
--             newCreateCertificate
--
--         , requestCreateCloudFormationStack $
--             newCreateCloudFormationStack
--
--         , requestCreateContactMethod $
--             newCreateContactMethod
--
--         , requestCreateContainerService $
--             newCreateContainerService
--
--         , requestCreateContainerServiceDeployment $
--             newCreateContainerServiceDeployment
--
--         , requestCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLogin
--
--         , requestCreateDisk $
--             newCreateDisk
--
--         , requestCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshot
--
--         , requestCreateDiskSnapshot $
--             newCreateDiskSnapshot
--
--         , requestCreateDistribution $
--             newCreateDistribution
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateDomainEntry $
--             newCreateDomainEntry
--
--         , requestCreateInstanceSnapshot $
--             newCreateInstanceSnapshot
--
--         , requestCreateInstances $
--             newCreateInstances
--
--         , requestCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshot
--
--         , requestCreateKeyPair $
--             newCreateKeyPair
--
--         , requestCreateLoadBalancer $
--             newCreateLoadBalancer
--
--         , requestCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificate
--
--         , requestCreateRelationalDatabase $
--             newCreateRelationalDatabase
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshot
--
--         , requestCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshot
--
--         , requestDeleteAlarm $
--             newDeleteAlarm
--
--         , requestDeleteAutoSnapshot $
--             newDeleteAutoSnapshot
--
--         , requestDeleteBucket $
--             newDeleteBucket
--
--         , requestDeleteBucketAccessKey $
--             newDeleteBucketAccessKey
--
--         , requestDeleteCertificate $
--             newDeleteCertificate
--
--         , requestDeleteContactMethod $
--             newDeleteContactMethod
--
--         , requestDeleteContainerImage $
--             newDeleteContainerImage
--
--         , requestDeleteContainerService $
--             newDeleteContainerService
--
--         , requestDeleteDisk $
--             newDeleteDisk
--
--         , requestDeleteDiskSnapshot $
--             newDeleteDiskSnapshot
--
--         , requestDeleteDistribution $
--             newDeleteDistribution
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteDomainEntry $
--             newDeleteDomainEntry
--
--         , requestDeleteInstance $
--             newDeleteInstance
--
--         , requestDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshot
--
--         , requestDeleteKeyPair $
--             newDeleteKeyPair
--
--         , requestDeleteKnownHostKeys $
--             newDeleteKnownHostKeys
--
--         , requestDeleteLoadBalancer $
--             newDeleteLoadBalancer
--
--         , requestDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificate
--
--         , requestDeleteRelationalDatabase $
--             newDeleteRelationalDatabase
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshot
--
--         , requestDetachCertificateFromDistribution $
--             newDetachCertificateFromDistribution
--
--         , requestDetachDisk $
--             newDetachDisk
--
--         , requestDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancer
--
--         , requestDetachStaticIp $
--             newDetachStaticIp
--
--         , requestDisableAddOn $
--             newDisableAddOn
--
--         , requestDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPair
--
--         , requestEnableAddOn $
--             newEnableAddOn
--
--         , requestExportSnapshot $
--             newExportSnapshot
--
--         , requestGetActiveNames $
--             newGetActiveNames
--
--         , requestGetAlarms $
--             newGetAlarms
--
--         , requestGetAutoSnapshots $
--             newGetAutoSnapshots
--
--         , requestGetBlueprints $
--             newGetBlueprints
--
--         , requestGetBucketAccessKeys $
--             newGetBucketAccessKeys
--
--         , requestGetBucketBundles $
--             newGetBucketBundles
--
--         , requestGetBucketMetricData $
--             newGetBucketMetricData
--
--         , requestGetBuckets $
--             newGetBuckets
--
--         , requestGetBundles $
--             newGetBundles
--
--         , requestGetCertificates $
--             newGetCertificates
--
--         , requestGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecords
--
--         , requestGetContactMethods $
--             newGetContactMethods
--
--         , requestGetContainerAPIMetadata $
--             newGetContainerAPIMetadata
--
--         , requestGetContainerImages $
--             newGetContainerImages
--
--         , requestGetContainerLog $
--             newGetContainerLog
--
--         , requestGetContainerServiceDeployments $
--             newGetContainerServiceDeployments
--
--         , requestGetContainerServiceMetricData $
--             newGetContainerServiceMetricData
--
--         , requestGetContainerServicePowers $
--             newGetContainerServicePowers
--
--         , requestGetContainerServices $
--             newGetContainerServices
--
--         , requestGetDisk $
--             newGetDisk
--
--         , requestGetDiskSnapshot $
--             newGetDiskSnapshot
--
--         , requestGetDiskSnapshots $
--             newGetDiskSnapshots
--
--         , requestGetDisks $
--             newGetDisks
--
--         , requestGetDistributionBundles $
--             newGetDistributionBundles
--
--         , requestGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheReset
--
--         , requestGetDistributionMetricData $
--             newGetDistributionMetricData
--
--         , requestGetDistributions $
--             newGetDistributions
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestGetDomains $
--             newGetDomains
--
--         , requestGetExportSnapshotRecords $
--             newGetExportSnapshotRecords
--
--         , requestGetInstance $
--             newGetInstance
--
--         , requestGetInstanceAccessDetails $
--             newGetInstanceAccessDetails
--
--         , requestGetInstanceMetricData $
--             newGetInstanceMetricData
--
--         , requestGetInstancePortStates $
--             newGetInstancePortStates
--
--         , requestGetInstanceSnapshot $
--             newGetInstanceSnapshot
--
--         , requestGetInstanceSnapshots $
--             newGetInstanceSnapshots
--
--         , requestGetInstanceState $
--             newGetInstanceState
--
--         , requestGetInstances $
--             newGetInstances
--
--         , requestGetKeyPair $
--             newGetKeyPair
--
--         , requestGetKeyPairs $
--             newGetKeyPairs
--
--         , requestGetLoadBalancer $
--             newGetLoadBalancer
--
--         , requestGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricData
--
--         , requestGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificates
--
--         , requestGetLoadBalancerTlsPolicies $
--             newGetLoadBalancerTlsPolicies
--
--         , requestGetLoadBalancers $
--             newGetLoadBalancers
--
--         , requestGetOperation $
--             newGetOperation
--
--         , requestGetOperations $
--             newGetOperations
--
--         , requestGetOperationsForResource $
--             newGetOperationsForResource
--
--         , requestGetRegions $
--             newGetRegions
--
--         , requestGetRelationalDatabase $
--             newGetRelationalDatabase
--
--         , requestGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprints
--
--         , requestGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundles
--
--         , requestGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEvents
--
--         , requestGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEvents
--
--         , requestGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreams
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPassword
--
--         , requestGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricData
--
--         , requestGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParameters
--
--         , requestGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshot
--
--         , requestGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshots
--
--         , requestGetRelationalDatabases $
--             newGetRelationalDatabases
--
--         , requestGetStaticIp $
--             newGetStaticIp
--
--         , requestGetStaticIps $
--             newGetStaticIps
--
--         , requestImportKeyPair $
--             newImportKeyPair
--
--         , requestIsVpcPeered $
--             newIsVpcPeered
--
--         , requestOpenInstancePublicPorts $
--             newOpenInstancePublicPorts
--
--         , requestPeerVpc $
--             newPeerVpc
--
--         , requestPutAlarm $
--             newPutAlarm
--
--         , requestPutInstancePublicPorts $
--             newPutInstancePublicPorts
--
--         , requestRebootInstance $
--             newRebootInstance
--
--         , requestRebootRelationalDatabase $
--             newRebootRelationalDatabase
--
--         , requestRegisterContainerImage $
--             newRegisterContainerImage
--
--         , requestReleaseStaticIp $
--             newReleaseStaticIp
--
--         , requestResetDistributionCache $
--             newResetDistributionCache
--
--         , requestSendContactMethodVerification $
--             newSendContactMethodVerification
--
--         , requestSetIpAddressType $
--             newSetIpAddressType
--
--         , requestSetResourceAccessForBucket $
--             newSetResourceAccessForBucket
--
--         , requestStartInstance $
--             newStartInstance
--
--         , requestStartRelationalDatabase $
--             newStartRelationalDatabase
--
--         , requestStopInstance $
--             newStopInstance
--
--         , requestStopRelationalDatabase $
--             newStopRelationalDatabase
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestAlarm $
--             newTestAlarm
--
--         , requestUnpeerVpc $
--             newUnpeerVpc
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBucket $
--             newUpdateBucket
--
--         , requestUpdateBucketBundle $
--             newUpdateBucketBundle
--
--         , requestUpdateContainerService $
--             newUpdateContainerService
--
--         , requestUpdateDistribution $
--             newUpdateDistribution
--
--         , requestUpdateDistributionBundle $
--             newUpdateDistributionBundle
--
--         , requestUpdateDomainEntry $
--             newUpdateDomainEntry
--
--         , requestUpdateInstanceMetadataOptions $
--             newUpdateInstanceMetadataOptions
--
--         , requestUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttribute
--
--         , requestUpdateRelationalDatabase $
--             newUpdateRelationalDatabase
--
--         , requestUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParameters
--
--           ]

--     , testGroup "response"
--         [ responseAllocateStaticIp $
--             newAllocateStaticIpResponse
--
--         , responseAttachCertificateToDistribution $
--             newAttachCertificateToDistributionResponse
--
--         , responseAttachDisk $
--             newAttachDiskResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             newAttachInstancesToLoadBalancerResponse
--
--         , responseAttachLoadBalancerTlsCertificate $
--             newAttachLoadBalancerTlsCertificateResponse
--
--         , responseAttachStaticIp $
--             newAttachStaticIpResponse
--
--         , responseCloseInstancePublicPorts $
--             newCloseInstancePublicPortsResponse
--
--         , responseCopySnapshot $
--             newCopySnapshotResponse
--
--         , responseCreateBucket $
--             newCreateBucketResponse
--
--         , responseCreateBucketAccessKey $
--             newCreateBucketAccessKeyResponse
--
--         , responseCreateCertificate $
--             newCreateCertificateResponse
--
--         , responseCreateCloudFormationStack $
--             newCreateCloudFormationStackResponse
--
--         , responseCreateContactMethod $
--             newCreateContactMethodResponse
--
--         , responseCreateContainerService $
--             newCreateContainerServiceResponse
--
--         , responseCreateContainerServiceDeployment $
--             newCreateContainerServiceDeploymentResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             newCreateContainerServiceRegistryLoginResponse
--
--         , responseCreateDisk $
--             newCreateDiskResponse
--
--         , responseCreateDiskFromSnapshot $
--             newCreateDiskFromSnapshotResponse
--
--         , responseCreateDiskSnapshot $
--             newCreateDiskSnapshotResponse
--
--         , responseCreateDistribution $
--             newCreateDistributionResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateDomainEntry $
--             newCreateDomainEntryResponse
--
--         , responseCreateInstanceSnapshot $
--             newCreateInstanceSnapshotResponse
--
--         , responseCreateInstances $
--             newCreateInstancesResponse
--
--         , responseCreateInstancesFromSnapshot $
--             newCreateInstancesFromSnapshotResponse
--
--         , responseCreateKeyPair $
--             newCreateKeyPairResponse
--
--         , responseCreateLoadBalancer $
--             newCreateLoadBalancerResponse
--
--         , responseCreateLoadBalancerTlsCertificate $
--             newCreateLoadBalancerTlsCertificateResponse
--
--         , responseCreateRelationalDatabase $
--             newCreateRelationalDatabaseResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             newCreateRelationalDatabaseFromSnapshotResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             newCreateRelationalDatabaseSnapshotResponse
--
--         , responseDeleteAlarm $
--             newDeleteAlarmResponse
--
--         , responseDeleteAutoSnapshot $
--             newDeleteAutoSnapshotResponse
--
--         , responseDeleteBucket $
--             newDeleteBucketResponse
--
--         , responseDeleteBucketAccessKey $
--             newDeleteBucketAccessKeyResponse
--
--         , responseDeleteCertificate $
--             newDeleteCertificateResponse
--
--         , responseDeleteContactMethod $
--             newDeleteContactMethodResponse
--
--         , responseDeleteContainerImage $
--             newDeleteContainerImageResponse
--
--         , responseDeleteContainerService $
--             newDeleteContainerServiceResponse
--
--         , responseDeleteDisk $
--             newDeleteDiskResponse
--
--         , responseDeleteDiskSnapshot $
--             newDeleteDiskSnapshotResponse
--
--         , responseDeleteDistribution $
--             newDeleteDistributionResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteDomainEntry $
--             newDeleteDomainEntryResponse
--
--         , responseDeleteInstance $
--             newDeleteInstanceResponse
--
--         , responseDeleteInstanceSnapshot $
--             newDeleteInstanceSnapshotResponse
--
--         , responseDeleteKeyPair $
--             newDeleteKeyPairResponse
--
--         , responseDeleteKnownHostKeys $
--             newDeleteKnownHostKeysResponse
--
--         , responseDeleteLoadBalancer $
--             newDeleteLoadBalancerResponse
--
--         , responseDeleteLoadBalancerTlsCertificate $
--             newDeleteLoadBalancerTlsCertificateResponse
--
--         , responseDeleteRelationalDatabase $
--             newDeleteRelationalDatabaseResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             newDeleteRelationalDatabaseSnapshotResponse
--
--         , responseDetachCertificateFromDistribution $
--             newDetachCertificateFromDistributionResponse
--
--         , responseDetachDisk $
--             newDetachDiskResponse
--
--         , responseDetachInstancesFromLoadBalancer $
--             newDetachInstancesFromLoadBalancerResponse
--
--         , responseDetachStaticIp $
--             newDetachStaticIpResponse
--
--         , responseDisableAddOn $
--             newDisableAddOnResponse
--
--         , responseDownloadDefaultKeyPair $
--             newDownloadDefaultKeyPairResponse
--
--         , responseEnableAddOn $
--             newEnableAddOnResponse
--
--         , responseExportSnapshot $
--             newExportSnapshotResponse
--
--         , responseGetActiveNames $
--             newGetActiveNamesResponse
--
--         , responseGetAlarms $
--             newGetAlarmsResponse
--
--         , responseGetAutoSnapshots $
--             newGetAutoSnapshotsResponse
--
--         , responseGetBlueprints $
--             newGetBlueprintsResponse
--
--         , responseGetBucketAccessKeys $
--             newGetBucketAccessKeysResponse
--
--         , responseGetBucketBundles $
--             newGetBucketBundlesResponse
--
--         , responseGetBucketMetricData $
--             newGetBucketMetricDataResponse
--
--         , responseGetBuckets $
--             newGetBucketsResponse
--
--         , responseGetBundles $
--             newGetBundlesResponse
--
--         , responseGetCertificates $
--             newGetCertificatesResponse
--
--         , responseGetCloudFormationStackRecords $
--             newGetCloudFormationStackRecordsResponse
--
--         , responseGetContactMethods $
--             newGetContactMethodsResponse
--
--         , responseGetContainerAPIMetadata $
--             newGetContainerAPIMetadataResponse
--
--         , responseGetContainerImages $
--             newGetContainerImagesResponse
--
--         , responseGetContainerLog $
--             newGetContainerLogResponse
--
--         , responseGetContainerServiceDeployments $
--             newGetContainerServiceDeploymentsResponse
--
--         , responseGetContainerServiceMetricData $
--             newGetContainerServiceMetricDataResponse
--
--         , responseGetContainerServicePowers $
--             newGetContainerServicePowersResponse
--
--         , responseGetContainerServices $
--             newGetContainerServicesResponse
--
--         , responseGetDisk $
--             newGetDiskResponse
--
--         , responseGetDiskSnapshot $
--             newGetDiskSnapshotResponse
--
--         , responseGetDiskSnapshots $
--             newGetDiskSnapshotsResponse
--
--         , responseGetDisks $
--             newGetDisksResponse
--
--         , responseGetDistributionBundles $
--             newGetDistributionBundlesResponse
--
--         , responseGetDistributionLatestCacheReset $
--             newGetDistributionLatestCacheResetResponse
--
--         , responseGetDistributionMetricData $
--             newGetDistributionMetricDataResponse
--
--         , responseGetDistributions $
--             newGetDistributionsResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseGetDomains $
--             newGetDomainsResponse
--
--         , responseGetExportSnapshotRecords $
--             newGetExportSnapshotRecordsResponse
--
--         , responseGetInstance $
--             newGetInstanceResponse
--
--         , responseGetInstanceAccessDetails $
--             newGetInstanceAccessDetailsResponse
--
--         , responseGetInstanceMetricData $
--             newGetInstanceMetricDataResponse
--
--         , responseGetInstancePortStates $
--             newGetInstancePortStatesResponse
--
--         , responseGetInstanceSnapshot $
--             newGetInstanceSnapshotResponse
--
--         , responseGetInstanceSnapshots $
--             newGetInstanceSnapshotsResponse
--
--         , responseGetInstanceState $
--             newGetInstanceStateResponse
--
--         , responseGetInstances $
--             newGetInstancesResponse
--
--         , responseGetKeyPair $
--             newGetKeyPairResponse
--
--         , responseGetKeyPairs $
--             newGetKeyPairsResponse
--
--         , responseGetLoadBalancer $
--             newGetLoadBalancerResponse
--
--         , responseGetLoadBalancerMetricData $
--             newGetLoadBalancerMetricDataResponse
--
--         , responseGetLoadBalancerTlsCertificates $
--             newGetLoadBalancerTlsCertificatesResponse
--
--         , responseGetLoadBalancerTlsPolicies $
--             newGetLoadBalancerTlsPoliciesResponse
--
--         , responseGetLoadBalancers $
--             newGetLoadBalancersResponse
--
--         , responseGetOperation $
--             newGetOperationResponse
--
--         , responseGetOperations $
--             newGetOperationsResponse
--
--         , responseGetOperationsForResource $
--             newGetOperationsForResourceResponse
--
--         , responseGetRegions $
--             newGetRegionsResponse
--
--         , responseGetRelationalDatabase $
--             newGetRelationalDatabaseResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             newGetRelationalDatabaseBlueprintsResponse
--
--         , responseGetRelationalDatabaseBundles $
--             newGetRelationalDatabaseBundlesResponse
--
--         , responseGetRelationalDatabaseEvents $
--             newGetRelationalDatabaseEventsResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             newGetRelationalDatabaseLogEventsResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             newGetRelationalDatabaseLogStreamsResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             newGetRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             newGetRelationalDatabaseMetricDataResponse
--
--         , responseGetRelationalDatabaseParameters $
--             newGetRelationalDatabaseParametersResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             newGetRelationalDatabaseSnapshotResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             newGetRelationalDatabaseSnapshotsResponse
--
--         , responseGetRelationalDatabases $
--             newGetRelationalDatabasesResponse
--
--         , responseGetStaticIp $
--             newGetStaticIpResponse
--
--         , responseGetStaticIps $
--             newGetStaticIpsResponse
--
--         , responseImportKeyPair $
--             newImportKeyPairResponse
--
--         , responseIsVpcPeered $
--             newIsVpcPeeredResponse
--
--         , responseOpenInstancePublicPorts $
--             newOpenInstancePublicPortsResponse
--
--         , responsePeerVpc $
--             newPeerVpcResponse
--
--         , responsePutAlarm $
--             newPutAlarmResponse
--
--         , responsePutInstancePublicPorts $
--             newPutInstancePublicPortsResponse
--
--         , responseRebootInstance $
--             newRebootInstanceResponse
--
--         , responseRebootRelationalDatabase $
--             newRebootRelationalDatabaseResponse
--
--         , responseRegisterContainerImage $
--             newRegisterContainerImageResponse
--
--         , responseReleaseStaticIp $
--             newReleaseStaticIpResponse
--
--         , responseResetDistributionCache $
--             newResetDistributionCacheResponse
--
--         , responseSendContactMethodVerification $
--             newSendContactMethodVerificationResponse
--
--         , responseSetIpAddressType $
--             newSetIpAddressTypeResponse
--
--         , responseSetResourceAccessForBucket $
--             newSetResourceAccessForBucketResponse
--
--         , responseStartInstance $
--             newStartInstanceResponse
--
--         , responseStartRelationalDatabase $
--             newStartRelationalDatabaseResponse
--
--         , responseStopInstance $
--             newStopInstanceResponse
--
--         , responseStopRelationalDatabase $
--             newStopRelationalDatabaseResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestAlarm $
--             newTestAlarmResponse
--
--         , responseUnpeerVpc $
--             newUnpeerVpcResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBucket $
--             newUpdateBucketResponse
--
--         , responseUpdateBucketBundle $
--             newUpdateBucketBundleResponse
--
--         , responseUpdateContainerService $
--             newUpdateContainerServiceResponse
--
--         , responseUpdateDistribution $
--             newUpdateDistributionResponse
--
--         , responseUpdateDistributionBundle $
--             newUpdateDistributionBundleResponse
--
--         , responseUpdateDomainEntry $
--             newUpdateDomainEntryResponse
--
--         , responseUpdateInstanceMetadataOptions $
--             newUpdateInstanceMetadataOptionsResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             newUpdateLoadBalancerAttributeResponse
--
--         , responseUpdateRelationalDatabase $
--             newUpdateRelationalDatabaseResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             newUpdateRelationalDatabaseParametersResponse
--
--           ]
--     ]

-- Requests

requestAllocateStaticIp :: AllocateStaticIp -> TestTree
requestAllocateStaticIp =
  req
    "AllocateStaticIp"
    "fixture/AllocateStaticIp.yaml"

requestAttachCertificateToDistribution :: AttachCertificateToDistribution -> TestTree
requestAttachCertificateToDistribution =
  req
    "AttachCertificateToDistribution"
    "fixture/AttachCertificateToDistribution.yaml"

requestAttachDisk :: AttachDisk -> TestTree
requestAttachDisk =
  req
    "AttachDisk"
    "fixture/AttachDisk.yaml"

requestAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancer -> TestTree
requestAttachInstancesToLoadBalancer =
  req
    "AttachInstancesToLoadBalancer"
    "fixture/AttachInstancesToLoadBalancer.yaml"

requestAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificate -> TestTree
requestAttachLoadBalancerTlsCertificate =
  req
    "AttachLoadBalancerTlsCertificate"
    "fixture/AttachLoadBalancerTlsCertificate.yaml"

requestAttachStaticIp :: AttachStaticIp -> TestTree
requestAttachStaticIp =
  req
    "AttachStaticIp"
    "fixture/AttachStaticIp.yaml"

requestCloseInstancePublicPorts :: CloseInstancePublicPorts -> TestTree
requestCloseInstancePublicPorts =
  req
    "CloseInstancePublicPorts"
    "fixture/CloseInstancePublicPorts.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot =
  req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestCreateBucket :: CreateBucket -> TestTree
requestCreateBucket =
  req
    "CreateBucket"
    "fixture/CreateBucket.yaml"

requestCreateBucketAccessKey :: CreateBucketAccessKey -> TestTree
requestCreateBucketAccessKey =
  req
    "CreateBucketAccessKey"
    "fixture/CreateBucketAccessKey.yaml"

requestCreateCertificate :: CreateCertificate -> TestTree
requestCreateCertificate =
  req
    "CreateCertificate"
    "fixture/CreateCertificate.yaml"

requestCreateCloudFormationStack :: CreateCloudFormationStack -> TestTree
requestCreateCloudFormationStack =
  req
    "CreateCloudFormationStack"
    "fixture/CreateCloudFormationStack.yaml"

requestCreateContactMethod :: CreateContactMethod -> TestTree
requestCreateContactMethod =
  req
    "CreateContactMethod"
    "fixture/CreateContactMethod.yaml"

requestCreateContainerService :: CreateContainerService -> TestTree
requestCreateContainerService =
  req
    "CreateContainerService"
    "fixture/CreateContainerService.yaml"

requestCreateContainerServiceDeployment :: CreateContainerServiceDeployment -> TestTree
requestCreateContainerServiceDeployment =
  req
    "CreateContainerServiceDeployment"
    "fixture/CreateContainerServiceDeployment.yaml"

requestCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLogin -> TestTree
requestCreateContainerServiceRegistryLogin =
  req
    "CreateContainerServiceRegistryLogin"
    "fixture/CreateContainerServiceRegistryLogin.yaml"

requestCreateDisk :: CreateDisk -> TestTree
requestCreateDisk =
  req
    "CreateDisk"
    "fixture/CreateDisk.yaml"

requestCreateDiskFromSnapshot :: CreateDiskFromSnapshot -> TestTree
requestCreateDiskFromSnapshot =
  req
    "CreateDiskFromSnapshot"
    "fixture/CreateDiskFromSnapshot.yaml"

requestCreateDiskSnapshot :: CreateDiskSnapshot -> TestTree
requestCreateDiskSnapshot =
  req
    "CreateDiskSnapshot"
    "fixture/CreateDiskSnapshot.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution =
  req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry =
  req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot =
  req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances =
  req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestCreateInstancesFromSnapshot :: CreateInstancesFromSnapshot -> TestTree
requestCreateInstancesFromSnapshot =
  req
    "CreateInstancesFromSnapshot"
    "fixture/CreateInstancesFromSnapshot.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair =
  req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer =
  req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificate -> TestTree
requestCreateLoadBalancerTlsCertificate =
  req
    "CreateLoadBalancerTlsCertificate"
    "fixture/CreateLoadBalancerTlsCertificate.yaml"

requestCreateRelationalDatabase :: CreateRelationalDatabase -> TestTree
requestCreateRelationalDatabase =
  req
    "CreateRelationalDatabase"
    "fixture/CreateRelationalDatabase.yaml"

requestCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshot -> TestTree
requestCreateRelationalDatabaseFromSnapshot =
  req
    "CreateRelationalDatabaseFromSnapshot"
    "fixture/CreateRelationalDatabaseFromSnapshot.yaml"

requestCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshot -> TestTree
requestCreateRelationalDatabaseSnapshot =
  req
    "CreateRelationalDatabaseSnapshot"
    "fixture/CreateRelationalDatabaseSnapshot.yaml"

requestDeleteAlarm :: DeleteAlarm -> TestTree
requestDeleteAlarm =
  req
    "DeleteAlarm"
    "fixture/DeleteAlarm.yaml"

requestDeleteAutoSnapshot :: DeleteAutoSnapshot -> TestTree
requestDeleteAutoSnapshot =
  req
    "DeleteAutoSnapshot"
    "fixture/DeleteAutoSnapshot.yaml"

requestDeleteBucket :: DeleteBucket -> TestTree
requestDeleteBucket =
  req
    "DeleteBucket"
    "fixture/DeleteBucket.yaml"

requestDeleteBucketAccessKey :: DeleteBucketAccessKey -> TestTree
requestDeleteBucketAccessKey =
  req
    "DeleteBucketAccessKey"
    "fixture/DeleteBucketAccessKey.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate =
  req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestDeleteContactMethod :: DeleteContactMethod -> TestTree
requestDeleteContactMethod =
  req
    "DeleteContactMethod"
    "fixture/DeleteContactMethod.yaml"

requestDeleteContainerImage :: DeleteContainerImage -> TestTree
requestDeleteContainerImage =
  req
    "DeleteContainerImage"
    "fixture/DeleteContainerImage.yaml"

requestDeleteContainerService :: DeleteContainerService -> TestTree
requestDeleteContainerService =
  req
    "DeleteContainerService"
    "fixture/DeleteContainerService.yaml"

requestDeleteDisk :: DeleteDisk -> TestTree
requestDeleteDisk =
  req
    "DeleteDisk"
    "fixture/DeleteDisk.yaml"

requestDeleteDiskSnapshot :: DeleteDiskSnapshot -> TestTree
requestDeleteDiskSnapshot =
  req
    "DeleteDiskSnapshot"
    "fixture/DeleteDiskSnapshot.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution =
  req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry =
  req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance =
  req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot =
  req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair =
  req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDeleteKnownHostKeys :: DeleteKnownHostKeys -> TestTree
requestDeleteKnownHostKeys =
  req
    "DeleteKnownHostKeys"
    "fixture/DeleteKnownHostKeys.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer =
  req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificate -> TestTree
requestDeleteLoadBalancerTlsCertificate =
  req
    "DeleteLoadBalancerTlsCertificate"
    "fixture/DeleteLoadBalancerTlsCertificate.yaml"

requestDeleteRelationalDatabase :: DeleteRelationalDatabase -> TestTree
requestDeleteRelationalDatabase =
  req
    "DeleteRelationalDatabase"
    "fixture/DeleteRelationalDatabase.yaml"

requestDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshot -> TestTree
requestDeleteRelationalDatabaseSnapshot =
  req
    "DeleteRelationalDatabaseSnapshot"
    "fixture/DeleteRelationalDatabaseSnapshot.yaml"

requestDetachCertificateFromDistribution :: DetachCertificateFromDistribution -> TestTree
requestDetachCertificateFromDistribution =
  req
    "DetachCertificateFromDistribution"
    "fixture/DetachCertificateFromDistribution.yaml"

requestDetachDisk :: DetachDisk -> TestTree
requestDetachDisk =
  req
    "DetachDisk"
    "fixture/DetachDisk.yaml"

requestDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancer -> TestTree
requestDetachInstancesFromLoadBalancer =
  req
    "DetachInstancesFromLoadBalancer"
    "fixture/DetachInstancesFromLoadBalancer.yaml"

requestDetachStaticIp :: DetachStaticIp -> TestTree
requestDetachStaticIp =
  req
    "DetachStaticIp"
    "fixture/DetachStaticIp.yaml"

requestDisableAddOn :: DisableAddOn -> TestTree
requestDisableAddOn =
  req
    "DisableAddOn"
    "fixture/DisableAddOn.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair =
  req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestEnableAddOn :: EnableAddOn -> TestTree
requestEnableAddOn =
  req
    "EnableAddOn"
    "fixture/EnableAddOn.yaml"

requestExportSnapshot :: ExportSnapshot -> TestTree
requestExportSnapshot =
  req
    "ExportSnapshot"
    "fixture/ExportSnapshot.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames =
  req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestGetAlarms :: GetAlarms -> TestTree
requestGetAlarms =
  req
    "GetAlarms"
    "fixture/GetAlarms.yaml"

requestGetAutoSnapshots :: GetAutoSnapshots -> TestTree
requestGetAutoSnapshots =
  req
    "GetAutoSnapshots"
    "fixture/GetAutoSnapshots.yaml"

requestGetBlueprints :: GetBlueprints -> TestTree
requestGetBlueprints =
  req
    "GetBlueprints"
    "fixture/GetBlueprints.yaml"

requestGetBucketAccessKeys :: GetBucketAccessKeys -> TestTree
requestGetBucketAccessKeys =
  req
    "GetBucketAccessKeys"
    "fixture/GetBucketAccessKeys.yaml"

requestGetBucketBundles :: GetBucketBundles -> TestTree
requestGetBucketBundles =
  req
    "GetBucketBundles"
    "fixture/GetBucketBundles.yaml"

requestGetBucketMetricData :: GetBucketMetricData -> TestTree
requestGetBucketMetricData =
  req
    "GetBucketMetricData"
    "fixture/GetBucketMetricData.yaml"

requestGetBuckets :: GetBuckets -> TestTree
requestGetBuckets =
  req
    "GetBuckets"
    "fixture/GetBuckets.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles =
  req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestGetCertificates :: GetCertificates -> TestTree
requestGetCertificates =
  req
    "GetCertificates"
    "fixture/GetCertificates.yaml"

requestGetCloudFormationStackRecords :: GetCloudFormationStackRecords -> TestTree
requestGetCloudFormationStackRecords =
  req
    "GetCloudFormationStackRecords"
    "fixture/GetCloudFormationStackRecords.yaml"

requestGetContactMethods :: GetContactMethods -> TestTree
requestGetContactMethods =
  req
    "GetContactMethods"
    "fixture/GetContactMethods.yaml"

requestGetContainerAPIMetadata :: GetContainerAPIMetadata -> TestTree
requestGetContainerAPIMetadata =
  req
    "GetContainerAPIMetadata"
    "fixture/GetContainerAPIMetadata.yaml"

requestGetContainerImages :: GetContainerImages -> TestTree
requestGetContainerImages =
  req
    "GetContainerImages"
    "fixture/GetContainerImages.yaml"

requestGetContainerLog :: GetContainerLog -> TestTree
requestGetContainerLog =
  req
    "GetContainerLog"
    "fixture/GetContainerLog.yaml"

requestGetContainerServiceDeployments :: GetContainerServiceDeployments -> TestTree
requestGetContainerServiceDeployments =
  req
    "GetContainerServiceDeployments"
    "fixture/GetContainerServiceDeployments.yaml"

requestGetContainerServiceMetricData :: GetContainerServiceMetricData -> TestTree
requestGetContainerServiceMetricData =
  req
    "GetContainerServiceMetricData"
    "fixture/GetContainerServiceMetricData.yaml"

requestGetContainerServicePowers :: GetContainerServicePowers -> TestTree
requestGetContainerServicePowers =
  req
    "GetContainerServicePowers"
    "fixture/GetContainerServicePowers.yaml"

requestGetContainerServices :: GetContainerServices -> TestTree
requestGetContainerServices =
  req
    "GetContainerServices"
    "fixture/GetContainerServices.yaml"

requestGetDisk :: GetDisk -> TestTree
requestGetDisk =
  req
    "GetDisk"
    "fixture/GetDisk.yaml"

requestGetDiskSnapshot :: GetDiskSnapshot -> TestTree
requestGetDiskSnapshot =
  req
    "GetDiskSnapshot"
    "fixture/GetDiskSnapshot.yaml"

requestGetDiskSnapshots :: GetDiskSnapshots -> TestTree
requestGetDiskSnapshots =
  req
    "GetDiskSnapshots"
    "fixture/GetDiskSnapshots.yaml"

requestGetDisks :: GetDisks -> TestTree
requestGetDisks =
  req
    "GetDisks"
    "fixture/GetDisks.yaml"

requestGetDistributionBundles :: GetDistributionBundles -> TestTree
requestGetDistributionBundles =
  req
    "GetDistributionBundles"
    "fixture/GetDistributionBundles.yaml"

requestGetDistributionLatestCacheReset :: GetDistributionLatestCacheReset -> TestTree
requestGetDistributionLatestCacheReset =
  req
    "GetDistributionLatestCacheReset"
    "fixture/GetDistributionLatestCacheReset.yaml"

requestGetDistributionMetricData :: GetDistributionMetricData -> TestTree
requestGetDistributionMetricData =
  req
    "GetDistributionMetricData"
    "fixture/GetDistributionMetricData.yaml"

requestGetDistributions :: GetDistributions -> TestTree
requestGetDistributions =
  req
    "GetDistributions"
    "fixture/GetDistributions.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains =
  req
    "GetDomains"
    "fixture/GetDomains.yaml"

requestGetExportSnapshotRecords :: GetExportSnapshotRecords -> TestTree
requestGetExportSnapshotRecords =
  req
    "GetExportSnapshotRecords"
    "fixture/GetExportSnapshotRecords.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance =
  req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails =
  req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestGetInstanceMetricData :: GetInstanceMetricData -> TestTree
requestGetInstanceMetricData =
  req
    "GetInstanceMetricData"
    "fixture/GetInstanceMetricData.yaml"

requestGetInstancePortStates :: GetInstancePortStates -> TestTree
requestGetInstancePortStates =
  req
    "GetInstancePortStates"
    "fixture/GetInstancePortStates.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot =
  req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots =
  req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState =
  req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

requestGetInstances :: GetInstances -> TestTree
requestGetInstances =
  req
    "GetInstances"
    "fixture/GetInstances.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair =
  req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestGetKeyPairs :: GetKeyPairs -> TestTree
requestGetKeyPairs =
  req
    "GetKeyPairs"
    "fixture/GetKeyPairs.yaml"

requestGetLoadBalancer :: GetLoadBalancer -> TestTree
requestGetLoadBalancer =
  req
    "GetLoadBalancer"
    "fixture/GetLoadBalancer.yaml"

requestGetLoadBalancerMetricData :: GetLoadBalancerMetricData -> TestTree
requestGetLoadBalancerMetricData =
  req
    "GetLoadBalancerMetricData"
    "fixture/GetLoadBalancerMetricData.yaml"

requestGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificates -> TestTree
requestGetLoadBalancerTlsCertificates =
  req
    "GetLoadBalancerTlsCertificates"
    "fixture/GetLoadBalancerTlsCertificates.yaml"

requestGetLoadBalancerTlsPolicies :: GetLoadBalancerTlsPolicies -> TestTree
requestGetLoadBalancerTlsPolicies =
  req
    "GetLoadBalancerTlsPolicies"
    "fixture/GetLoadBalancerTlsPolicies.yaml"

requestGetLoadBalancers :: GetLoadBalancers -> TestTree
requestGetLoadBalancers =
  req
    "GetLoadBalancers"
    "fixture/GetLoadBalancers.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation =
  req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestGetOperations :: GetOperations -> TestTree
requestGetOperations =
  req
    "GetOperations"
    "fixture/GetOperations.yaml"

requestGetOperationsForResource :: GetOperationsForResource -> TestTree
requestGetOperationsForResource =
  req
    "GetOperationsForResource"
    "fixture/GetOperationsForResource.yaml"

requestGetRegions :: GetRegions -> TestTree
requestGetRegions =
  req
    "GetRegions"
    "fixture/GetRegions.yaml"

requestGetRelationalDatabase :: GetRelationalDatabase -> TestTree
requestGetRelationalDatabase =
  req
    "GetRelationalDatabase"
    "fixture/GetRelationalDatabase.yaml"

requestGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprints -> TestTree
requestGetRelationalDatabaseBlueprints =
  req
    "GetRelationalDatabaseBlueprints"
    "fixture/GetRelationalDatabaseBlueprints.yaml"

requestGetRelationalDatabaseBundles :: GetRelationalDatabaseBundles -> TestTree
requestGetRelationalDatabaseBundles =
  req
    "GetRelationalDatabaseBundles"
    "fixture/GetRelationalDatabaseBundles.yaml"

requestGetRelationalDatabaseEvents :: GetRelationalDatabaseEvents -> TestTree
requestGetRelationalDatabaseEvents =
  req
    "GetRelationalDatabaseEvents"
    "fixture/GetRelationalDatabaseEvents.yaml"

requestGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEvents -> TestTree
requestGetRelationalDatabaseLogEvents =
  req
    "GetRelationalDatabaseLogEvents"
    "fixture/GetRelationalDatabaseLogEvents.yaml"

requestGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreams -> TestTree
requestGetRelationalDatabaseLogStreams =
  req
    "GetRelationalDatabaseLogStreams"
    "fixture/GetRelationalDatabaseLogStreams.yaml"

requestGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPassword -> TestTree
requestGetRelationalDatabaseMasterUserPassword =
  req
    "GetRelationalDatabaseMasterUserPassword"
    "fixture/GetRelationalDatabaseMasterUserPassword.yaml"

requestGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricData -> TestTree
requestGetRelationalDatabaseMetricData =
  req
    "GetRelationalDatabaseMetricData"
    "fixture/GetRelationalDatabaseMetricData.yaml"

requestGetRelationalDatabaseParameters :: GetRelationalDatabaseParameters -> TestTree
requestGetRelationalDatabaseParameters =
  req
    "GetRelationalDatabaseParameters"
    "fixture/GetRelationalDatabaseParameters.yaml"

requestGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshot -> TestTree
requestGetRelationalDatabaseSnapshot =
  req
    "GetRelationalDatabaseSnapshot"
    "fixture/GetRelationalDatabaseSnapshot.yaml"

requestGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshots -> TestTree
requestGetRelationalDatabaseSnapshots =
  req
    "GetRelationalDatabaseSnapshots"
    "fixture/GetRelationalDatabaseSnapshots.yaml"

requestGetRelationalDatabases :: GetRelationalDatabases -> TestTree
requestGetRelationalDatabases =
  req
    "GetRelationalDatabases"
    "fixture/GetRelationalDatabases.yaml"

requestGetStaticIp :: GetStaticIp -> TestTree
requestGetStaticIp =
  req
    "GetStaticIp"
    "fixture/GetStaticIp.yaml"

requestGetStaticIps :: GetStaticIps -> TestTree
requestGetStaticIps =
  req
    "GetStaticIps"
    "fixture/GetStaticIps.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair =
  req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestIsVpcPeered :: IsVpcPeered -> TestTree
requestIsVpcPeered =
  req
    "IsVpcPeered"
    "fixture/IsVpcPeered.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts =
  req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestPeerVpc :: PeerVpc -> TestTree
requestPeerVpc =
  req
    "PeerVpc"
    "fixture/PeerVpc.yaml"

requestPutAlarm :: PutAlarm -> TestTree
requestPutAlarm =
  req
    "PutAlarm"
    "fixture/PutAlarm.yaml"

requestPutInstancePublicPorts :: PutInstancePublicPorts -> TestTree
requestPutInstancePublicPorts =
  req
    "PutInstancePublicPorts"
    "fixture/PutInstancePublicPorts.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance =
  req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestRebootRelationalDatabase :: RebootRelationalDatabase -> TestTree
requestRebootRelationalDatabase =
  req
    "RebootRelationalDatabase"
    "fixture/RebootRelationalDatabase.yaml"

requestRegisterContainerImage :: RegisterContainerImage -> TestTree
requestRegisterContainerImage =
  req
    "RegisterContainerImage"
    "fixture/RegisterContainerImage.yaml"

requestReleaseStaticIp :: ReleaseStaticIp -> TestTree
requestReleaseStaticIp =
  req
    "ReleaseStaticIp"
    "fixture/ReleaseStaticIp.yaml"

requestResetDistributionCache :: ResetDistributionCache -> TestTree
requestResetDistributionCache =
  req
    "ResetDistributionCache"
    "fixture/ResetDistributionCache.yaml"

requestSendContactMethodVerification :: SendContactMethodVerification -> TestTree
requestSendContactMethodVerification =
  req
    "SendContactMethodVerification"
    "fixture/SendContactMethodVerification.yaml"

requestSetIpAddressType :: SetIpAddressType -> TestTree
requestSetIpAddressType =
  req
    "SetIpAddressType"
    "fixture/SetIpAddressType.yaml"

requestSetResourceAccessForBucket :: SetResourceAccessForBucket -> TestTree
requestSetResourceAccessForBucket =
  req
    "SetResourceAccessForBucket"
    "fixture/SetResourceAccessForBucket.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance =
  req
    "StartInstance"
    "fixture/StartInstance.yaml"

requestStartRelationalDatabase :: StartRelationalDatabase -> TestTree
requestStartRelationalDatabase =
  req
    "StartRelationalDatabase"
    "fixture/StartRelationalDatabase.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance =
  req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestStopRelationalDatabase :: StopRelationalDatabase -> TestTree
requestStopRelationalDatabase =
  req
    "StopRelationalDatabase"
    "fixture/StopRelationalDatabase.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestAlarm :: TestAlarm -> TestTree
requestTestAlarm =
  req
    "TestAlarm"
    "fixture/TestAlarm.yaml"

requestUnpeerVpc :: UnpeerVpc -> TestTree
requestUnpeerVpc =
  req
    "UnpeerVpc"
    "fixture/UnpeerVpc.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateBucket :: UpdateBucket -> TestTree
requestUpdateBucket =
  req
    "UpdateBucket"
    "fixture/UpdateBucket.yaml"

requestUpdateBucketBundle :: UpdateBucketBundle -> TestTree
requestUpdateBucketBundle =
  req
    "UpdateBucketBundle"
    "fixture/UpdateBucketBundle.yaml"

requestUpdateContainerService :: UpdateContainerService -> TestTree
requestUpdateContainerService =
  req
    "UpdateContainerService"
    "fixture/UpdateContainerService.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestUpdateDistributionBundle :: UpdateDistributionBundle -> TestTree
requestUpdateDistributionBundle =
  req
    "UpdateDistributionBundle"
    "fixture/UpdateDistributionBundle.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry =
  req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

requestUpdateInstanceMetadataOptions :: UpdateInstanceMetadataOptions -> TestTree
requestUpdateInstanceMetadataOptions =
  req
    "UpdateInstanceMetadataOptions"
    "fixture/UpdateInstanceMetadataOptions.yaml"

requestUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttribute -> TestTree
requestUpdateLoadBalancerAttribute =
  req
    "UpdateLoadBalancerAttribute"
    "fixture/UpdateLoadBalancerAttribute.yaml"

requestUpdateRelationalDatabase :: UpdateRelationalDatabase -> TestTree
requestUpdateRelationalDatabase =
  req
    "UpdateRelationalDatabase"
    "fixture/UpdateRelationalDatabase.yaml"

requestUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParameters -> TestTree
requestUpdateRelationalDatabaseParameters =
  req
    "UpdateRelationalDatabaseParameters"
    "fixture/UpdateRelationalDatabaseParameters.yaml"

-- Responses

responseAllocateStaticIp :: AllocateStaticIpResponse -> TestTree
responseAllocateStaticIp =
  res
    "AllocateStaticIpResponse"
    "fixture/AllocateStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AllocateStaticIp)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution =
  res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachCertificateToDistribution)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk =
  res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachDisk)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer =
  res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachInstancesToLoadBalancer)

responseAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificateResponse -> TestTree
responseAttachLoadBalancerTlsCertificate =
  res
    "AttachLoadBalancerTlsCertificateResponse"
    "fixture/AttachLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachLoadBalancerTlsCertificate)

responseAttachStaticIp :: AttachStaticIpResponse -> TestTree
responseAttachStaticIp =
  res
    "AttachStaticIpResponse"
    "fixture/AttachStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachStaticIp)

responseCloseInstancePublicPorts :: CloseInstancePublicPortsResponse -> TestTree
responseCloseInstancePublicPorts =
  res
    "CloseInstancePublicPortsResponse"
    "fixture/CloseInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloseInstancePublicPorts)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopySnapshot)

responseCreateBucket :: CreateBucketResponse -> TestTree
responseCreateBucket =
  res
    "CreateBucketResponse"
    "fixture/CreateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucket)

responseCreateBucketAccessKey :: CreateBucketAccessKeyResponse -> TestTree
responseCreateBucketAccessKey =
  res
    "CreateBucketAccessKeyResponse"
    "fixture/CreateBucketAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBucketAccessKey)

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate =
  res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCertificate)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack =
  res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCloudFormationStack)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod =
  res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactMethod)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService =
  res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerService)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment =
  res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerServiceDeployment)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin =
  res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContainerServiceRegistryLogin)

responseCreateDisk :: CreateDiskResponse -> TestTree
responseCreateDisk =
  res
    "CreateDiskResponse"
    "fixture/CreateDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDisk)

responseCreateDiskFromSnapshot :: CreateDiskFromSnapshotResponse -> TestTree
responseCreateDiskFromSnapshot =
  res
    "CreateDiskFromSnapshotResponse"
    "fixture/CreateDiskFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDiskFromSnapshot)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot =
  res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDiskSnapshot)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDistribution)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry =
  res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomainEntry)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot =
  res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstanceSnapshot)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances =
  res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstances)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot =
  res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateInstancesFromSnapshot)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateKeyPair)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancer)

responseCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificateResponse -> TestTree
responseCreateLoadBalancerTlsCertificate =
  res
    "CreateLoadBalancerTlsCertificateResponse"
    "fixture/CreateLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoadBalancerTlsCertificate)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase =
  res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabase)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot =
  res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabaseFromSnapshot)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot =
  res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRelationalDatabaseSnapshot)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm =
  res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlarm)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot =
  res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAutoSnapshot)

responseDeleteBucket :: DeleteBucketResponse -> TestTree
responseDeleteBucket =
  res
    "DeleteBucketResponse"
    "fixture/DeleteBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucket)

responseDeleteBucketAccessKey :: DeleteBucketAccessKeyResponse -> TestTree
responseDeleteBucketAccessKey =
  res
    "DeleteBucketAccessKeyResponse"
    "fixture/DeleteBucketAccessKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBucketAccessKey)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCertificate)

responseDeleteContactMethod :: DeleteContactMethodResponse -> TestTree
responseDeleteContactMethod =
  res
    "DeleteContactMethodResponse"
    "fixture/DeleteContactMethodResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactMethod)

responseDeleteContainerImage :: DeleteContainerImageResponse -> TestTree
responseDeleteContainerImage =
  res
    "DeleteContainerImageResponse"
    "fixture/DeleteContainerImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerImage)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService =
  res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContainerService)

responseDeleteDisk :: DeleteDiskResponse -> TestTree
responseDeleteDisk =
  res
    "DeleteDiskResponse"
    "fixture/DeleteDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDisk)

responseDeleteDiskSnapshot :: DeleteDiskSnapshotResponse -> TestTree
responseDeleteDiskSnapshot =
  res
    "DeleteDiskSnapshotResponse"
    "fixture/DeleteDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDiskSnapshot)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDistribution)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry =
  res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomainEntry)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstance)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot =
  res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteInstanceSnapshot)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyPair)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys =
  res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKnownHostKeys)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancer)

responseDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificateResponse -> TestTree
responseDeleteLoadBalancerTlsCertificate =
  res
    "DeleteLoadBalancerTlsCertificateResponse"
    "fixture/DeleteLoadBalancerTlsCertificateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoadBalancerTlsCertificate)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase =
  res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRelationalDatabase)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot =
  res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRelationalDatabaseSnapshot)

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution =
  res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachCertificateFromDistribution)

responseDetachDisk :: DetachDiskResponse -> TestTree
responseDetachDisk =
  res
    "DetachDiskResponse"
    "fixture/DetachDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachDisk)

responseDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancerResponse -> TestTree
responseDetachInstancesFromLoadBalancer =
  res
    "DetachInstancesFromLoadBalancerResponse"
    "fixture/DetachInstancesFromLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachInstancesFromLoadBalancer)

responseDetachStaticIp :: DetachStaticIpResponse -> TestTree
responseDetachStaticIp =
  res
    "DetachStaticIpResponse"
    "fixture/DetachStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachStaticIp)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn =
  res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAddOn)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair =
  res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DownloadDefaultKeyPair)

responseEnableAddOn :: EnableAddOnResponse -> TestTree
responseEnableAddOn =
  res
    "EnableAddOnResponse"
    "fixture/EnableAddOnResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAddOn)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportSnapshot)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames =
  res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetActiveNames)

responseGetAlarms :: GetAlarmsResponse -> TestTree
responseGetAlarms =
  res
    "GetAlarmsResponse"
    "fixture/GetAlarmsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAlarms)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots =
  res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoSnapshots)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints =
  res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBlueprints)

responseGetBucketAccessKeys :: GetBucketAccessKeysResponse -> TestTree
responseGetBucketAccessKeys =
  res
    "GetBucketAccessKeysResponse"
    "fixture/GetBucketAccessKeysResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketAccessKeys)

responseGetBucketBundles :: GetBucketBundlesResponse -> TestTree
responseGetBucketBundles =
  res
    "GetBucketBundlesResponse"
    "fixture/GetBucketBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketBundles)

responseGetBucketMetricData :: GetBucketMetricDataResponse -> TestTree
responseGetBucketMetricData =
  res
    "GetBucketMetricDataResponse"
    "fixture/GetBucketMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBucketMetricData)

responseGetBuckets :: GetBucketsResponse -> TestTree
responseGetBuckets =
  res
    "GetBucketsResponse"
    "fixture/GetBucketsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuckets)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles =
  res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBundles)

responseGetCertificates :: GetCertificatesResponse -> TestTree
responseGetCertificates =
  res
    "GetCertificatesResponse"
    "fixture/GetCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCertificates)

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords =
  res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCloudFormationStackRecords)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods =
  res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactMethods)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata =
  res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerAPIMetadata)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages =
  res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerImages)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog =
  res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerLog)

responseGetContainerServiceDeployments :: GetContainerServiceDeploymentsResponse -> TestTree
responseGetContainerServiceDeployments =
  res
    "GetContainerServiceDeploymentsResponse"
    "fixture/GetContainerServiceDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServiceDeployments)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData =
  res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServiceMetricData)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers =
  res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServicePowers)

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices =
  res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContainerServices)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk =
  res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDisk)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot =
  res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiskSnapshot)

responseGetDiskSnapshots :: GetDiskSnapshotsResponse -> TestTree
responseGetDiskSnapshots =
  res
    "GetDiskSnapshotsResponse"
    "fixture/GetDiskSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDiskSnapshots)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks =
  res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDisks)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles =
  res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionBundles)

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset =
  res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionLatestCacheReset)

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData =
  res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributionMetricData)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions =
  res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDistributions)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains =
  res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomains)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords =
  res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExportSnapshotRecords)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstance)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails =
  res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceAccessDetails)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData =
  res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceMetricData)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates =
  res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstancePortStates)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot =
  res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceSnapshot)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots =
  res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceSnapshots)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState =
  res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstanceState)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances =
  res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetInstances)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair =
  res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPair)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs =
  res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetKeyPairs)

responseGetLoadBalancer :: GetLoadBalancerResponse -> TestTree
responseGetLoadBalancer =
  res
    "GetLoadBalancerResponse"
    "fixture/GetLoadBalancerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancer)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData =
  res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancerMetricData)

responseGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificatesResponse -> TestTree
responseGetLoadBalancerTlsCertificates =
  res
    "GetLoadBalancerTlsCertificatesResponse"
    "fixture/GetLoadBalancerTlsCertificatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancerTlsCertificates)

responseGetLoadBalancerTlsPolicies :: GetLoadBalancerTlsPoliciesResponse -> TestTree
responseGetLoadBalancerTlsPolicies =
  res
    "GetLoadBalancerTlsPoliciesResponse"
    "fixture/GetLoadBalancerTlsPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancerTlsPolicies)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers =
  res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoadBalancers)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperation)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations =
  res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperations)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource =
  res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetOperationsForResource)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions =
  res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRegions)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase =
  res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabase)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints =
  res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseBlueprints)

responseGetRelationalDatabaseBundles :: GetRelationalDatabaseBundlesResponse -> TestTree
responseGetRelationalDatabaseBundles =
  res
    "GetRelationalDatabaseBundlesResponse"
    "fixture/GetRelationalDatabaseBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseBundles)

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents =
  res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseEvents)

responseGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEventsResponse -> TestTree
responseGetRelationalDatabaseLogEvents =
  res
    "GetRelationalDatabaseLogEventsResponse"
    "fixture/GetRelationalDatabaseLogEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseLogEvents)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams =
  res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseLogStreams)

responseGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> TestTree
responseGetRelationalDatabaseMasterUserPassword =
  res
    "GetRelationalDatabaseMasterUserPasswordResponse"
    "fixture/GetRelationalDatabaseMasterUserPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseMasterUserPassword)

responseGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricDataResponse -> TestTree
responseGetRelationalDatabaseMetricData =
  res
    "GetRelationalDatabaseMetricDataResponse"
    "fixture/GetRelationalDatabaseMetricDataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseMetricData)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters =
  res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseParameters)

responseGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> TestTree
responseGetRelationalDatabaseSnapshot =
  res
    "GetRelationalDatabaseSnapshotResponse"
    "fixture/GetRelationalDatabaseSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseSnapshot)

responseGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> TestTree
responseGetRelationalDatabaseSnapshots =
  res
    "GetRelationalDatabaseSnapshotsResponse"
    "fixture/GetRelationalDatabaseSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabaseSnapshots)

responseGetRelationalDatabases :: GetRelationalDatabasesResponse -> TestTree
responseGetRelationalDatabases =
  res
    "GetRelationalDatabasesResponse"
    "fixture/GetRelationalDatabasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRelationalDatabases)

responseGetStaticIp :: GetStaticIpResponse -> TestTree
responseGetStaticIp =
  res
    "GetStaticIpResponse"
    "fixture/GetStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStaticIp)

responseGetStaticIps :: GetStaticIpsResponse -> TestTree
responseGetStaticIps =
  res
    "GetStaticIpsResponse"
    "fixture/GetStaticIpsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStaticIps)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportKeyPair)

responseIsVpcPeered :: IsVpcPeeredResponse -> TestTree
responseIsVpcPeered =
  res
    "IsVpcPeeredResponse"
    "fixture/IsVpcPeeredResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy IsVpcPeered)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts =
  res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OpenInstancePublicPorts)

responsePeerVpc :: PeerVpcResponse -> TestTree
responsePeerVpc =
  res
    "PeerVpcResponse"
    "fixture/PeerVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PeerVpc)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm =
  res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlarm)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts =
  res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInstancePublicPorts)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootInstance)

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase =
  res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebootRelationalDatabase)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage =
  res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterContainerImage)

responseReleaseStaticIp :: ReleaseStaticIpResponse -> TestTree
responseReleaseStaticIp =
  res
    "ReleaseStaticIpResponse"
    "fixture/ReleaseStaticIpResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleaseStaticIp)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache =
  res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDistributionCache)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification =
  res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendContactMethodVerification)

responseSetIpAddressType :: SetIpAddressTypeResponse -> TestTree
responseSetIpAddressType =
  res
    "SetIpAddressTypeResponse"
    "fixture/SetIpAddressTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIpAddressType)

responseSetResourceAccessForBucket :: SetResourceAccessForBucketResponse -> TestTree
responseSetResourceAccessForBucket =
  res
    "SetResourceAccessForBucketResponse"
    "fixture/SetResourceAccessForBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetResourceAccessForBucket)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartInstance)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase =
  res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRelationalDatabase)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopInstance)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase =
  res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopRelationalDatabase)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm =
  res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestAlarm)

responseUnpeerVpc :: UnpeerVpcResponse -> TestTree
responseUnpeerVpc =
  res
    "UnpeerVpcResponse"
    "fixture/UnpeerVpcResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnpeerVpc)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateBucket :: UpdateBucketResponse -> TestTree
responseUpdateBucket =
  res
    "UpdateBucketResponse"
    "fixture/UpdateBucketResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBucket)

responseUpdateBucketBundle :: UpdateBucketBundleResponse -> TestTree
responseUpdateBucketBundle =
  res
    "UpdateBucketBundleResponse"
    "fixture/UpdateBucketBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBucketBundle)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService =
  res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContainerService)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistribution)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle =
  res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDistributionBundle)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry =
  res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomainEntry)

responseUpdateInstanceMetadataOptions :: UpdateInstanceMetadataOptionsResponse -> TestTree
responseUpdateInstanceMetadataOptions =
  res
    "UpdateInstanceMetadataOptionsResponse"
    "fixture/UpdateInstanceMetadataOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateInstanceMetadataOptions)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute =
  res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoadBalancerAttribute)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase =
  res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelationalDatabase)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters =
  res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRelationalDatabaseParameters)
