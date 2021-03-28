{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lightsail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Lightsail where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Lightsail
import Test.AWS.Lightsail.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCloseInstancePublicPorts $
--             mkCloseInstancePublicPorts
--
--         , requestGetRelationalDatabaseMetricData $
--             mkGetRelationalDatabaseMetricData
--
--         , requestAllocateStaticIp $
--             mkAllocateStaticIp
--
--         , requestDeleteKeyPair $
--             mkDeleteKeyPair
--
--         , requestDeleteInstanceSnapshot $
--             mkDeleteInstanceSnapshot
--
--         , requestGetInstances $
--             mkGetInstances
--
--         , requestGetLoadBalancer $
--             mkGetLoadBalancer
--
--         , requestDisableAddOn $
--             mkDisableAddOn
--
--         , requestGetDistributions $
--             mkGetDistributions
--
--         , requestCreateContainerServiceDeployment $
--             mkCreateContainerServiceDeployment
--
--         , requestGetInstance $
--             mkGetInstance
--
--         , requestGetRelationalDatabaseEvents $
--             mkGetRelationalDatabaseEvents
--
--         , requestAttachCertificateToDistribution $
--             mkAttachCertificateToDistribution
--
--         , requestGetContainerServices $
--             mkGetContainerServices
--
--         , requestUpdateDistributionBundle $
--             mkUpdateDistributionBundle
--
--         , requestGetRelationalDatabaseSnapshots $
--             mkGetRelationalDatabaseSnapshots
--
--         , requestAttachStaticIp $
--             mkAttachStaticIp
--
--         , requestGetRelationalDatabaseParameters $
--             mkGetRelationalDatabaseParameters
--
--         , requestDetachDisk $
--             mkDetachDisk
--
--         , requestGetContactMethods $
--             mkGetContactMethods
--
--         , requestDownloadDefaultKeyPair $
--             mkDownloadDefaultKeyPair
--
--         , requestDeleteLoadBalancerTlsCertificate $
--             mkDeleteLoadBalancerTlsCertificate
--
--         , requestTestAlarm $
--             mkTestAlarm
--
--         , requestGetDomains $
--             mkGetDomains
--
--         , requestGetContainerImages $
--             mkGetContainerImages
--
--         , requestUpdateRelationalDatabaseParameters $
--             mkUpdateRelationalDatabaseParameters
--
--         , requestCreateLoadBalancerTlsCertificate $
--             mkCreateLoadBalancerTlsCertificate
--
--         , requestCreateDomainEntry $
--             mkCreateDomainEntry
--
--         , requestGetContainerServicePowers $
--             mkGetContainerServicePowers
--
--         , requestImportKeyPair $
--             mkImportKeyPair
--
--         , requestGetInstanceSnapshots $
--             mkGetInstanceSnapshots
--
--         , requestExportSnapshot $
--             mkExportSnapshot
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             mkCreateRelationalDatabaseFromSnapshot
--
--         , requestCreateCloudFormationStack $
--             mkCreateCloudFormationStack
--
--         , requestGetExportSnapshotRecords $
--             mkGetExportSnapshotRecords
--
--         , requestReleaseStaticIp $
--             mkReleaseStaticIp
--
--         , requestDeleteInstance $
--             mkDeleteInstance
--
--         , requestCreateContainerServiceRegistryLogin $
--             mkCreateContainerServiceRegistryLogin
--
--         , requestGetCertificates $
--             mkGetCertificates
--
--         , requestGetContainerServiceMetricData $
--             mkGetContainerServiceMetricData
--
--         , requestGetDistributionMetricData $
--             mkGetDistributionMetricData
--
--         , requestRebootInstance $
--             mkRebootInstance
--
--         , requestDeleteLoadBalancer $
--             mkDeleteLoadBalancer
--
--         , requestCreateDiskFromSnapshot $
--             mkCreateDiskFromSnapshot
--
--         , requestGetRelationalDatabases $
--             mkGetRelationalDatabases
--
--         , requestGetInstanceSnapshot $
--             mkGetInstanceSnapshot
--
--         , requestGetRelationalDatabaseLogEvents $
--             mkGetRelationalDatabaseLogEvents
--
--         , requestCreateContactMethod $
--             mkCreateContactMethod
--
--         , requestGetRelationalDatabaseLogStreams $
--             mkGetRelationalDatabaseLogStreams
--
--         , requestGetDomain $
--             mkGetDomain
--
--         , requestGetAutoSnapshots $
--             mkGetAutoSnapshots
--
--         , requestGetActiveNames $
--             mkGetActiveNames
--
--         , requestDeleteContactMethod $
--             mkDeleteContactMethod
--
--         , requestCreateDistribution $
--             mkCreateDistribution
--
--         , requestStopRelationalDatabase $
--             mkStopRelationalDatabase
--
--         , requestCreateRelationalDatabaseSnapshot $
--             mkCreateRelationalDatabaseSnapshot
--
--         , requestDetachCertificateFromDistribution $
--             mkDetachCertificateFromDistribution
--
--         , requestCreateContainerService $
--             mkCreateContainerService
--
--         , requestGetInstanceAccessDetails $
--             mkGetInstanceAccessDetails
--
--         , requestEnableAddOn $
--             mkEnableAddOn
--
--         , requestStopInstance $
--             mkStopInstance
--
--         , requestDetachInstancesFromLoadBalancer $
--             mkDetachInstancesFromLoadBalancer
--
--         , requestRegisterContainerImage $
--             mkRegisterContainerImage
--
--         , requestCreateCertificate $
--             mkCreateCertificate
--
--         , requestCreateInstanceSnapshot $
--             mkCreateInstanceSnapshot
--
--         , requestCopySnapshot $
--             mkCopySnapshot
--
--         , requestGetRelationalDatabaseSnapshot $
--             mkGetRelationalDatabaseSnapshot
--
--         , requestIsVpcPeered $
--             mkIsVpcPeered
--
--         , requestGetStaticIps $
--             mkGetStaticIps
--
--         , requestUnpeerVpc $
--             mkUnpeerVpc
--
--         , requestDeleteDisk $
--             mkDeleteDisk
--
--         , requestCreateInstancesFromSnapshot $
--             mkCreateInstancesFromSnapshot
--
--         , requestGetCloudFormationStackRecords $
--             mkGetCloudFormationStackRecords
--
--         , requestCreateDomain $
--             mkCreateDomain
--
--         , requestGetRelationalDatabaseBlueprints $
--             mkGetRelationalDatabaseBlueprints
--
--         , requestDeleteCertificate $
--             mkDeleteCertificate
--
--         , requestGetDiskSnapshots $
--             mkGetDiskSnapshots
--
--         , requestGetContainerAPIMetadata $
--             mkGetContainerAPIMetadata
--
--         , requestPeerVpc $
--             mkPeerVpc
--
--         , requestGetRelationalDatabaseBundles $
--             mkGetRelationalDatabaseBundles
--
--         , requestGetLoadBalancers $
--             mkGetLoadBalancers
--
--         , requestRebootRelationalDatabase $
--             mkRebootRelationalDatabase
--
--         , requestAttachLoadBalancerTlsCertificate $
--             mkAttachLoadBalancerTlsCertificate
--
--         , requestUpdateLoadBalancerAttribute $
--             mkUpdateLoadBalancerAttribute
--
--         , requestDeleteRelationalDatabase $
--             mkDeleteRelationalDatabase
--
--         , requestGetDiskSnapshot $
--             mkGetDiskSnapshot
--
--         , requestUpdateRelationalDatabase $
--             mkUpdateRelationalDatabase
--
--         , requestGetStaticIp $
--             mkGetStaticIp
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             mkGetRelationalDatabaseMasterUserPassword
--
--         , requestGetBlueprints $
--             mkGetBlueprints
--
--         , requestPutAlarm $
--             mkPutAlarm
--
--         , requestDeleteAlarm $
--             mkDeleteAlarm
--
--         , requestGetInstancePortStates $
--             mkGetInstancePortStates
--
--         , requestDeleteAutoSnapshot $
--             mkDeleteAutoSnapshot
--
--         , requestCreateRelationalDatabase $
--             mkCreateRelationalDatabase
--
--         , requestSendContactMethodVerification $
--             mkSendContactMethodVerification
--
--         , requestGetContainerLog $
--             mkGetContainerLog
--
--         , requestCreateDiskSnapshot $
--             mkCreateDiskSnapshot
--
--         , requestDeleteDomainEntry $
--             mkDeleteDomainEntry
--
--         , requestUpdateDomainEntry $
--             mkUpdateDomainEntry
--
--         , requestGetRegions $
--             mkGetRegions
--
--         , requestDeleteDiskSnapshot $
--             mkDeleteDiskSnapshot
--
--         , requestGetLoadBalancerMetricData $
--             mkGetLoadBalancerMetricData
--
--         , requestGetInstanceState $
--             mkGetInstanceState
--
--         , requestGetKeyPairs $
--             mkGetKeyPairs
--
--         , requestGetOperations $
--             mkGetOperations
--
--         , requestGetDisks $
--             mkGetDisks
--
--         , requestGetRelationalDatabase $
--             mkGetRelationalDatabase
--
--         , requestAttachInstancesToLoadBalancer $
--             mkAttachInstancesToLoadBalancer
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetOperation $
--             mkGetOperation
--
--         , requestResetDistributionCache $
--             mkResetDistributionCache
--
--         , requestUpdateDistribution $
--             mkUpdateDistribution
--
--         , requestDeleteDistribution $
--             mkDeleteDistribution
--
--         , requestUpdateContainerService $
--             mkUpdateContainerService
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             mkDeleteRelationalDatabaseSnapshot
--
--         , requestDeleteContainerService $
--             mkDeleteContainerService
--
--         , requestGetInstanceMetricData $
--             mkGetInstanceMetricData
--
--         , requestGetKeyPair $
--             mkGetKeyPair
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestPutInstancePublicPorts $
--             mkPutInstancePublicPorts
--
--         , requestGetDistributionBundles $
--             mkGetDistributionBundles
--
--         , requestGetDisk $
--             mkGetDisk
--
--         , requestGetDistributionLatestCacheReset $
--             mkGetDistributionLatestCacheReset
--
--         , requestCreateLoadBalancer $
--             mkCreateLoadBalancer
--
--         , requestGetContainerServiceDeployments $
--             mkGetContainerServiceDeployments
--
--         , requestDeleteKnownHostKeys $
--             mkDeleteKnownHostKeys
--
--         , requestAttachDisk $
--             mkAttachDisk
--
--         , requestDetachStaticIp $
--             mkDetachStaticIp
--
--         , requestCreateInstances $
--             mkCreateInstances
--
--         , requestGetAlarms $
--             mkGetAlarms
--
--         , requestOpenInstancePublicPorts $
--             mkOpenInstancePublicPorts
--
--         , requestStartRelationalDatabase $
--             mkStartRelationalDatabase
--
--         , requestDeleteContainerImage $
--             mkDeleteContainerImage
--
--         , requestGetBundles $
--             mkGetBundles
--
--         , requestDeleteDomain $
--             mkDeleteDomain
--
--         , requestGetLoadBalancerTlsCertificates $
--             mkGetLoadBalancerTlsCertificates
--
--         , requestCreateDisk $
--             mkCreateDisk
--
--         , requestGetOperationsForResource $
--             mkGetOperationsForResource
--
--         , requestCreateKeyPair $
--             mkCreateKeyPair
--
--         , requestStartInstance $
--             mkStartInstance
--
--           ]

--     , testGroup "response"
--         [ responseCloseInstancePublicPorts $
--             mkCloseInstancePublicPortsResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             mkGetRelationalDatabaseMetricDataResponse
--
--         , responseAllocateStaticIp $
--             mkAllocateStaticIpResponse
--
--         , responseDeleteKeyPair $
--             mkDeleteKeyPairResponse
--
--         , responseDeleteInstanceSnapshot $
--             mkDeleteInstanceSnapshotResponse
--
--         , responseGetInstances $
--             mkGetInstancesResponse
--
--         , responseGetLoadBalancer $
--             mkGetLoadBalancerResponse
--
--         , responseDisableAddOn $
--             mkDisableAddOnResponse
--
--         , responseGetDistributions $
--             mkGetDistributionsResponse
--
--         , responseCreateContainerServiceDeployment $
--             mkCreateContainerServiceDeploymentResponse
--
--         , responseGetInstance $
--             mkGetInstanceResponse
--
--         , responseGetRelationalDatabaseEvents $
--             mkGetRelationalDatabaseEventsResponse
--
--         , responseAttachCertificateToDistribution $
--             mkAttachCertificateToDistributionResponse
--
--         , responseGetContainerServices $
--             mkGetContainerServicesResponse
--
--         , responseUpdateDistributionBundle $
--             mkUpdateDistributionBundleResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             mkGetRelationalDatabaseSnapshotsResponse
--
--         , responseAttachStaticIp $
--             mkAttachStaticIpResponse
--
--         , responseGetRelationalDatabaseParameters $
--             mkGetRelationalDatabaseParametersResponse
--
--         , responseDetachDisk $
--             mkDetachDiskResponse
--
--         , responseGetContactMethods $
--             mkGetContactMethodsResponse
--
--         , responseDownloadDefaultKeyPair $
--             mkDownloadDefaultKeyPairResponse
--
--         , responseDeleteLoadBalancerTlsCertificate $
--             mkDeleteLoadBalancerTlsCertificateResponse
--
--         , responseTestAlarm $
--             mkTestAlarmResponse
--
--         , responseGetDomains $
--             mkGetDomainsResponse
--
--         , responseGetContainerImages $
--             mkGetContainerImagesResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             mkUpdateRelationalDatabaseParametersResponse
--
--         , responseCreateLoadBalancerTlsCertificate $
--             mkCreateLoadBalancerTlsCertificateResponse
--
--         , responseCreateDomainEntry $
--             mkCreateDomainEntryResponse
--
--         , responseGetContainerServicePowers $
--             mkGetContainerServicePowersResponse
--
--         , responseImportKeyPair $
--             mkImportKeyPairResponse
--
--         , responseGetInstanceSnapshots $
--             mkGetInstanceSnapshotsResponse
--
--         , responseExportSnapshot $
--             mkExportSnapshotResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             mkCreateRelationalDatabaseFromSnapshotResponse
--
--         , responseCreateCloudFormationStack $
--             mkCreateCloudFormationStackResponse
--
--         , responseGetExportSnapshotRecords $
--             mkGetExportSnapshotRecordsResponse
--
--         , responseReleaseStaticIp $
--             mkReleaseStaticIpResponse
--
--         , responseDeleteInstance $
--             mkDeleteInstanceResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             mkCreateContainerServiceRegistryLoginResponse
--
--         , responseGetCertificates $
--             mkGetCertificatesResponse
--
--         , responseGetContainerServiceMetricData $
--             mkGetContainerServiceMetricDataResponse
--
--         , responseGetDistributionMetricData $
--             mkGetDistributionMetricDataResponse
--
--         , responseRebootInstance $
--             mkRebootInstanceResponse
--
--         , responseDeleteLoadBalancer $
--             mkDeleteLoadBalancerResponse
--
--         , responseCreateDiskFromSnapshot $
--             mkCreateDiskFromSnapshotResponse
--
--         , responseGetRelationalDatabases $
--             mkGetRelationalDatabasesResponse
--
--         , responseGetInstanceSnapshot $
--             mkGetInstanceSnapshotResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             mkGetRelationalDatabaseLogEventsResponse
--
--         , responseCreateContactMethod $
--             mkCreateContactMethodResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             mkGetRelationalDatabaseLogStreamsResponse
--
--         , responseGetDomain $
--             mkGetDomainResponse
--
--         , responseGetAutoSnapshots $
--             mkGetAutoSnapshotsResponse
--
--         , responseGetActiveNames $
--             mkGetActiveNamesResponse
--
--         , responseDeleteContactMethod $
--             mkDeleteContactMethodResponse
--
--         , responseCreateDistribution $
--             mkCreateDistributionResponse
--
--         , responseStopRelationalDatabase $
--             mkStopRelationalDatabaseResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             mkCreateRelationalDatabaseSnapshotResponse
--
--         , responseDetachCertificateFromDistribution $
--             mkDetachCertificateFromDistributionResponse
--
--         , responseCreateContainerService $
--             mkCreateContainerServiceResponse
--
--         , responseGetInstanceAccessDetails $
--             mkGetInstanceAccessDetailsResponse
--
--         , responseEnableAddOn $
--             mkEnableAddOnResponse
--
--         , responseStopInstance $
--             mkStopInstanceResponse
--
--         , responseDetachInstancesFromLoadBalancer $
--             mkDetachInstancesFromLoadBalancerResponse
--
--         , responseRegisterContainerImage $
--             mkRegisterContainerImageResponse
--
--         , responseCreateCertificate $
--             mkCreateCertificateResponse
--
--         , responseCreateInstanceSnapshot $
--             mkCreateInstanceSnapshotResponse
--
--         , responseCopySnapshot $
--             mkCopySnapshotResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             mkGetRelationalDatabaseSnapshotResponse
--
--         , responseIsVpcPeered $
--             mkIsVpcPeeredResponse
--
--         , responseGetStaticIps $
--             mkGetStaticIpsResponse
--
--         , responseUnpeerVpc $
--             mkUnpeerVpcResponse
--
--         , responseDeleteDisk $
--             mkDeleteDiskResponse
--
--         , responseCreateInstancesFromSnapshot $
--             mkCreateInstancesFromSnapshotResponse
--
--         , responseGetCloudFormationStackRecords $
--             mkGetCloudFormationStackRecordsResponse
--
--         , responseCreateDomain $
--             mkCreateDomainResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             mkGetRelationalDatabaseBlueprintsResponse
--
--         , responseDeleteCertificate $
--             mkDeleteCertificateResponse
--
--         , responseGetDiskSnapshots $
--             mkGetDiskSnapshotsResponse
--
--         , responseGetContainerAPIMetadata $
--             mkGetContainerAPIMetadataResponse
--
--         , responsePeerVpc $
--             mkPeerVpcResponse
--
--         , responseGetRelationalDatabaseBundles $
--             mkGetRelationalDatabaseBundlesResponse
--
--         , responseGetLoadBalancers $
--             mkGetLoadBalancersResponse
--
--         , responseRebootRelationalDatabase $
--             mkRebootRelationalDatabaseResponse
--
--         , responseAttachLoadBalancerTlsCertificate $
--             mkAttachLoadBalancerTlsCertificateResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             mkUpdateLoadBalancerAttributeResponse
--
--         , responseDeleteRelationalDatabase $
--             mkDeleteRelationalDatabaseResponse
--
--         , responseGetDiskSnapshot $
--             mkGetDiskSnapshotResponse
--
--         , responseUpdateRelationalDatabase $
--             mkUpdateRelationalDatabaseResponse
--
--         , responseGetStaticIp $
--             mkGetStaticIpResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             mkGetRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetBlueprints $
--             mkGetBlueprintsResponse
--
--         , responsePutAlarm $
--             mkPutAlarmResponse
--
--         , responseDeleteAlarm $
--             mkDeleteAlarmResponse
--
--         , responseGetInstancePortStates $
--             mkGetInstancePortStatesResponse
--
--         , responseDeleteAutoSnapshot $
--             mkDeleteAutoSnapshotResponse
--
--         , responseCreateRelationalDatabase $
--             mkCreateRelationalDatabaseResponse
--
--         , responseSendContactMethodVerification $
--             mkSendContactMethodVerificationResponse
--
--         , responseGetContainerLog $
--             mkGetContainerLogResponse
--
--         , responseCreateDiskSnapshot $
--             mkCreateDiskSnapshotResponse
--
--         , responseDeleteDomainEntry $
--             mkDeleteDomainEntryResponse
--
--         , responseUpdateDomainEntry $
--             mkUpdateDomainEntryResponse
--
--         , responseGetRegions $
--             mkGetRegionsResponse
--
--         , responseDeleteDiskSnapshot $
--             mkDeleteDiskSnapshotResponse
--
--         , responseGetLoadBalancerMetricData $
--             mkGetLoadBalancerMetricDataResponse
--
--         , responseGetInstanceState $
--             mkGetInstanceStateResponse
--
--         , responseGetKeyPairs $
--             mkGetKeyPairsResponse
--
--         , responseGetOperations $
--             mkGetOperationsResponse
--
--         , responseGetDisks $
--             mkGetDisksResponse
--
--         , responseGetRelationalDatabase $
--             mkGetRelationalDatabaseResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             mkAttachInstancesToLoadBalancerResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetOperation $
--             mkGetOperationResponse
--
--         , responseResetDistributionCache $
--             mkResetDistributionCacheResponse
--
--         , responseUpdateDistribution $
--             mkUpdateDistributionResponse
--
--         , responseDeleteDistribution $
--             mkDeleteDistributionResponse
--
--         , responseUpdateContainerService $
--             mkUpdateContainerServiceResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             mkDeleteRelationalDatabaseSnapshotResponse
--
--         , responseDeleteContainerService $
--             mkDeleteContainerServiceResponse
--
--         , responseGetInstanceMetricData $
--             mkGetInstanceMetricDataResponse
--
--         , responseGetKeyPair $
--             mkGetKeyPairResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responsePutInstancePublicPorts $
--             mkPutInstancePublicPortsResponse
--
--         , responseGetDistributionBundles $
--             mkGetDistributionBundlesResponse
--
--         , responseGetDisk $
--             mkGetDiskResponse
--
--         , responseGetDistributionLatestCacheReset $
--             mkGetDistributionLatestCacheResetResponse
--
--         , responseCreateLoadBalancer $
--             mkCreateLoadBalancerResponse
--
--         , responseGetContainerServiceDeployments $
--             mkGetContainerServiceDeploymentsResponse
--
--         , responseDeleteKnownHostKeys $
--             mkDeleteKnownHostKeysResponse
--
--         , responseAttachDisk $
--             mkAttachDiskResponse
--
--         , responseDetachStaticIp $
--             mkDetachStaticIpResponse
--
--         , responseCreateInstances $
--             mkCreateInstancesResponse
--
--         , responseGetAlarms $
--             mkGetAlarmsResponse
--
--         , responseOpenInstancePublicPorts $
--             mkOpenInstancePublicPortsResponse
--
--         , responseStartRelationalDatabase $
--             mkStartRelationalDatabaseResponse
--
--         , responseDeleteContainerImage $
--             mkDeleteContainerImageResponse
--
--         , responseGetBundles $
--             mkGetBundlesResponse
--
--         , responseDeleteDomain $
--             mkDeleteDomainResponse
--
--         , responseGetLoadBalancerTlsCertificates $
--             mkGetLoadBalancerTlsCertificatesResponse
--
--         , responseCreateDisk $
--             mkCreateDiskResponse
--
--         , responseGetOperationsForResource $
--             mkGetOperationsForResourceResponse
--
--         , responseCreateKeyPair $
--             mkCreateKeyPairResponse
--
--         , responseStartInstance $
--             mkStartInstanceResponse
--
--           ]
--     ]

-- Requests

requestCloseInstancePublicPorts :: CloseInstancePublicPorts -> TestTree
requestCloseInstancePublicPorts = req
    "CloseInstancePublicPorts"
    "fixture/CloseInstancePublicPorts.yaml"

requestGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricData -> TestTree
requestGetRelationalDatabaseMetricData = req
    "GetRelationalDatabaseMetricData"
    "fixture/GetRelationalDatabaseMetricData.yaml"

requestAllocateStaticIp :: AllocateStaticIp -> TestTree
requestAllocateStaticIp = req
    "AllocateStaticIp"
    "fixture/AllocateStaticIp.yaml"

requestDeleteKeyPair :: DeleteKeyPair -> TestTree
requestDeleteKeyPair = req
    "DeleteKeyPair"
    "fixture/DeleteKeyPair.yaml"

requestDeleteInstanceSnapshot :: DeleteInstanceSnapshot -> TestTree
requestDeleteInstanceSnapshot = req
    "DeleteInstanceSnapshot"
    "fixture/DeleteInstanceSnapshot.yaml"

requestGetInstances :: GetInstances -> TestTree
requestGetInstances = req
    "GetInstances"
    "fixture/GetInstances.yaml"

requestGetLoadBalancer :: GetLoadBalancer -> TestTree
requestGetLoadBalancer = req
    "GetLoadBalancer"
    "fixture/GetLoadBalancer.yaml"

requestDisableAddOn :: DisableAddOn -> TestTree
requestDisableAddOn = req
    "DisableAddOn"
    "fixture/DisableAddOn.yaml"

requestGetDistributions :: GetDistributions -> TestTree
requestGetDistributions = req
    "GetDistributions"
    "fixture/GetDistributions.yaml"

requestCreateContainerServiceDeployment :: CreateContainerServiceDeployment -> TestTree
requestCreateContainerServiceDeployment = req
    "CreateContainerServiceDeployment"
    "fixture/CreateContainerServiceDeployment.yaml"

requestGetInstance :: GetInstance -> TestTree
requestGetInstance = req
    "GetInstance"
    "fixture/GetInstance.yaml"

requestGetRelationalDatabaseEvents :: GetRelationalDatabaseEvents -> TestTree
requestGetRelationalDatabaseEvents = req
    "GetRelationalDatabaseEvents"
    "fixture/GetRelationalDatabaseEvents.yaml"

requestAttachCertificateToDistribution :: AttachCertificateToDistribution -> TestTree
requestAttachCertificateToDistribution = req
    "AttachCertificateToDistribution"
    "fixture/AttachCertificateToDistribution.yaml"

requestGetContainerServices :: GetContainerServices -> TestTree
requestGetContainerServices = req
    "GetContainerServices"
    "fixture/GetContainerServices.yaml"

requestUpdateDistributionBundle :: UpdateDistributionBundle -> TestTree
requestUpdateDistributionBundle = req
    "UpdateDistributionBundle"
    "fixture/UpdateDistributionBundle.yaml"

requestGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshots -> TestTree
requestGetRelationalDatabaseSnapshots = req
    "GetRelationalDatabaseSnapshots"
    "fixture/GetRelationalDatabaseSnapshots.yaml"

requestAttachStaticIp :: AttachStaticIp -> TestTree
requestAttachStaticIp = req
    "AttachStaticIp"
    "fixture/AttachStaticIp.yaml"

requestGetRelationalDatabaseParameters :: GetRelationalDatabaseParameters -> TestTree
requestGetRelationalDatabaseParameters = req
    "GetRelationalDatabaseParameters"
    "fixture/GetRelationalDatabaseParameters.yaml"

requestDetachDisk :: DetachDisk -> TestTree
requestDetachDisk = req
    "DetachDisk"
    "fixture/DetachDisk.yaml"

requestGetContactMethods :: GetContactMethods -> TestTree
requestGetContactMethods = req
    "GetContactMethods"
    "fixture/GetContactMethods.yaml"

requestDownloadDefaultKeyPair :: DownloadDefaultKeyPair -> TestTree
requestDownloadDefaultKeyPair = req
    "DownloadDefaultKeyPair"
    "fixture/DownloadDefaultKeyPair.yaml"

requestDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificate -> TestTree
requestDeleteLoadBalancerTlsCertificate = req
    "DeleteLoadBalancerTlsCertificate"
    "fixture/DeleteLoadBalancerTlsCertificate.yaml"

requestTestAlarm :: TestAlarm -> TestTree
requestTestAlarm = req
    "TestAlarm"
    "fixture/TestAlarm.yaml"

requestGetDomains :: GetDomains -> TestTree
requestGetDomains = req
    "GetDomains"
    "fixture/GetDomains.yaml"

requestGetContainerImages :: GetContainerImages -> TestTree
requestGetContainerImages = req
    "GetContainerImages"
    "fixture/GetContainerImages.yaml"

requestUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParameters -> TestTree
requestUpdateRelationalDatabaseParameters = req
    "UpdateRelationalDatabaseParameters"
    "fixture/UpdateRelationalDatabaseParameters.yaml"

requestCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificate -> TestTree
requestCreateLoadBalancerTlsCertificate = req
    "CreateLoadBalancerTlsCertificate"
    "fixture/CreateLoadBalancerTlsCertificate.yaml"

requestCreateDomainEntry :: CreateDomainEntry -> TestTree
requestCreateDomainEntry = req
    "CreateDomainEntry"
    "fixture/CreateDomainEntry.yaml"

requestGetContainerServicePowers :: GetContainerServicePowers -> TestTree
requestGetContainerServicePowers = req
    "GetContainerServicePowers"
    "fixture/GetContainerServicePowers.yaml"

requestImportKeyPair :: ImportKeyPair -> TestTree
requestImportKeyPair = req
    "ImportKeyPair"
    "fixture/ImportKeyPair.yaml"

requestGetInstanceSnapshots :: GetInstanceSnapshots -> TestTree
requestGetInstanceSnapshots = req
    "GetInstanceSnapshots"
    "fixture/GetInstanceSnapshots.yaml"

requestExportSnapshot :: ExportSnapshot -> TestTree
requestExportSnapshot = req
    "ExportSnapshot"
    "fixture/ExportSnapshot.yaml"

requestCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshot -> TestTree
requestCreateRelationalDatabaseFromSnapshot = req
    "CreateRelationalDatabaseFromSnapshot"
    "fixture/CreateRelationalDatabaseFromSnapshot.yaml"

requestCreateCloudFormationStack :: CreateCloudFormationStack -> TestTree
requestCreateCloudFormationStack = req
    "CreateCloudFormationStack"
    "fixture/CreateCloudFormationStack.yaml"

requestGetExportSnapshotRecords :: GetExportSnapshotRecords -> TestTree
requestGetExportSnapshotRecords = req
    "GetExportSnapshotRecords"
    "fixture/GetExportSnapshotRecords.yaml"

requestReleaseStaticIp :: ReleaseStaticIp -> TestTree
requestReleaseStaticIp = req
    "ReleaseStaticIp"
    "fixture/ReleaseStaticIp.yaml"

requestDeleteInstance :: DeleteInstance -> TestTree
requestDeleteInstance = req
    "DeleteInstance"
    "fixture/DeleteInstance.yaml"

requestCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLogin -> TestTree
requestCreateContainerServiceRegistryLogin = req
    "CreateContainerServiceRegistryLogin"
    "fixture/CreateContainerServiceRegistryLogin.yaml"

requestGetCertificates :: GetCertificates -> TestTree
requestGetCertificates = req
    "GetCertificates"
    "fixture/GetCertificates.yaml"

requestGetContainerServiceMetricData :: GetContainerServiceMetricData -> TestTree
requestGetContainerServiceMetricData = req
    "GetContainerServiceMetricData"
    "fixture/GetContainerServiceMetricData.yaml"

requestGetDistributionMetricData :: GetDistributionMetricData -> TestTree
requestGetDistributionMetricData = req
    "GetDistributionMetricData"
    "fixture/GetDistributionMetricData.yaml"

requestRebootInstance :: RebootInstance -> TestTree
requestRebootInstance = req
    "RebootInstance"
    "fixture/RebootInstance.yaml"

requestDeleteLoadBalancer :: DeleteLoadBalancer -> TestTree
requestDeleteLoadBalancer = req
    "DeleteLoadBalancer"
    "fixture/DeleteLoadBalancer.yaml"

requestCreateDiskFromSnapshot :: CreateDiskFromSnapshot -> TestTree
requestCreateDiskFromSnapshot = req
    "CreateDiskFromSnapshot"
    "fixture/CreateDiskFromSnapshot.yaml"

requestGetRelationalDatabases :: GetRelationalDatabases -> TestTree
requestGetRelationalDatabases = req
    "GetRelationalDatabases"
    "fixture/GetRelationalDatabases.yaml"

requestGetInstanceSnapshot :: GetInstanceSnapshot -> TestTree
requestGetInstanceSnapshot = req
    "GetInstanceSnapshot"
    "fixture/GetInstanceSnapshot.yaml"

requestGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEvents -> TestTree
requestGetRelationalDatabaseLogEvents = req
    "GetRelationalDatabaseLogEvents"
    "fixture/GetRelationalDatabaseLogEvents.yaml"

requestCreateContactMethod :: CreateContactMethod -> TestTree
requestCreateContactMethod = req
    "CreateContactMethod"
    "fixture/CreateContactMethod.yaml"

requestGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreams -> TestTree
requestGetRelationalDatabaseLogStreams = req
    "GetRelationalDatabaseLogStreams"
    "fixture/GetRelationalDatabaseLogStreams.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain = req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetAutoSnapshots :: GetAutoSnapshots -> TestTree
requestGetAutoSnapshots = req
    "GetAutoSnapshots"
    "fixture/GetAutoSnapshots.yaml"

requestGetActiveNames :: GetActiveNames -> TestTree
requestGetActiveNames = req
    "GetActiveNames"
    "fixture/GetActiveNames.yaml"

requestDeleteContactMethod :: DeleteContactMethod -> TestTree
requestDeleteContactMethod = req
    "DeleteContactMethod"
    "fixture/DeleteContactMethod.yaml"

requestCreateDistribution :: CreateDistribution -> TestTree
requestCreateDistribution = req
    "CreateDistribution"
    "fixture/CreateDistribution.yaml"

requestStopRelationalDatabase :: StopRelationalDatabase -> TestTree
requestStopRelationalDatabase = req
    "StopRelationalDatabase"
    "fixture/StopRelationalDatabase.yaml"

requestCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshot -> TestTree
requestCreateRelationalDatabaseSnapshot = req
    "CreateRelationalDatabaseSnapshot"
    "fixture/CreateRelationalDatabaseSnapshot.yaml"

requestDetachCertificateFromDistribution :: DetachCertificateFromDistribution -> TestTree
requestDetachCertificateFromDistribution = req
    "DetachCertificateFromDistribution"
    "fixture/DetachCertificateFromDistribution.yaml"

requestCreateContainerService :: CreateContainerService -> TestTree
requestCreateContainerService = req
    "CreateContainerService"
    "fixture/CreateContainerService.yaml"

requestGetInstanceAccessDetails :: GetInstanceAccessDetails -> TestTree
requestGetInstanceAccessDetails = req
    "GetInstanceAccessDetails"
    "fixture/GetInstanceAccessDetails.yaml"

requestEnableAddOn :: EnableAddOn -> TestTree
requestEnableAddOn = req
    "EnableAddOn"
    "fixture/EnableAddOn.yaml"

requestStopInstance :: StopInstance -> TestTree
requestStopInstance = req
    "StopInstance"
    "fixture/StopInstance.yaml"

requestDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancer -> TestTree
requestDetachInstancesFromLoadBalancer = req
    "DetachInstancesFromLoadBalancer"
    "fixture/DetachInstancesFromLoadBalancer.yaml"

requestRegisterContainerImage :: RegisterContainerImage -> TestTree
requestRegisterContainerImage = req
    "RegisterContainerImage"
    "fixture/RegisterContainerImage.yaml"

requestCreateCertificate :: CreateCertificate -> TestTree
requestCreateCertificate = req
    "CreateCertificate"
    "fixture/CreateCertificate.yaml"

requestCreateInstanceSnapshot :: CreateInstanceSnapshot -> TestTree
requestCreateInstanceSnapshot = req
    "CreateInstanceSnapshot"
    "fixture/CreateInstanceSnapshot.yaml"

requestCopySnapshot :: CopySnapshot -> TestTree
requestCopySnapshot = req
    "CopySnapshot"
    "fixture/CopySnapshot.yaml"

requestGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshot -> TestTree
requestGetRelationalDatabaseSnapshot = req
    "GetRelationalDatabaseSnapshot"
    "fixture/GetRelationalDatabaseSnapshot.yaml"

requestIsVpcPeered :: IsVpcPeered -> TestTree
requestIsVpcPeered = req
    "IsVpcPeered"
    "fixture/IsVpcPeered.yaml"

requestGetStaticIps :: GetStaticIps -> TestTree
requestGetStaticIps = req
    "GetStaticIps"
    "fixture/GetStaticIps.yaml"

requestUnpeerVpc :: UnpeerVpc -> TestTree
requestUnpeerVpc = req
    "UnpeerVpc"
    "fixture/UnpeerVpc.yaml"

requestDeleteDisk :: DeleteDisk -> TestTree
requestDeleteDisk = req
    "DeleteDisk"
    "fixture/DeleteDisk.yaml"

requestCreateInstancesFromSnapshot :: CreateInstancesFromSnapshot -> TestTree
requestCreateInstancesFromSnapshot = req
    "CreateInstancesFromSnapshot"
    "fixture/CreateInstancesFromSnapshot.yaml"

requestGetCloudFormationStackRecords :: GetCloudFormationStackRecords -> TestTree
requestGetCloudFormationStackRecords = req
    "GetCloudFormationStackRecords"
    "fixture/GetCloudFormationStackRecords.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain = req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprints -> TestTree
requestGetRelationalDatabaseBlueprints = req
    "GetRelationalDatabaseBlueprints"
    "fixture/GetRelationalDatabaseBlueprints.yaml"

requestDeleteCertificate :: DeleteCertificate -> TestTree
requestDeleteCertificate = req
    "DeleteCertificate"
    "fixture/DeleteCertificate.yaml"

requestGetDiskSnapshots :: GetDiskSnapshots -> TestTree
requestGetDiskSnapshots = req
    "GetDiskSnapshots"
    "fixture/GetDiskSnapshots.yaml"

requestGetContainerAPIMetadata :: GetContainerAPIMetadata -> TestTree
requestGetContainerAPIMetadata = req
    "GetContainerAPIMetadata"
    "fixture/GetContainerAPIMetadata.yaml"

requestPeerVpc :: PeerVpc -> TestTree
requestPeerVpc = req
    "PeerVpc"
    "fixture/PeerVpc.yaml"

requestGetRelationalDatabaseBundles :: GetRelationalDatabaseBundles -> TestTree
requestGetRelationalDatabaseBundles = req
    "GetRelationalDatabaseBundles"
    "fixture/GetRelationalDatabaseBundles.yaml"

requestGetLoadBalancers :: GetLoadBalancers -> TestTree
requestGetLoadBalancers = req
    "GetLoadBalancers"
    "fixture/GetLoadBalancers.yaml"

requestRebootRelationalDatabase :: RebootRelationalDatabase -> TestTree
requestRebootRelationalDatabase = req
    "RebootRelationalDatabase"
    "fixture/RebootRelationalDatabase.yaml"

requestAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificate -> TestTree
requestAttachLoadBalancerTlsCertificate = req
    "AttachLoadBalancerTlsCertificate"
    "fixture/AttachLoadBalancerTlsCertificate.yaml"

requestUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttribute -> TestTree
requestUpdateLoadBalancerAttribute = req
    "UpdateLoadBalancerAttribute"
    "fixture/UpdateLoadBalancerAttribute.yaml"

requestDeleteRelationalDatabase :: DeleteRelationalDatabase -> TestTree
requestDeleteRelationalDatabase = req
    "DeleteRelationalDatabase"
    "fixture/DeleteRelationalDatabase.yaml"

requestGetDiskSnapshot :: GetDiskSnapshot -> TestTree
requestGetDiskSnapshot = req
    "GetDiskSnapshot"
    "fixture/GetDiskSnapshot.yaml"

requestUpdateRelationalDatabase :: UpdateRelationalDatabase -> TestTree
requestUpdateRelationalDatabase = req
    "UpdateRelationalDatabase"
    "fixture/UpdateRelationalDatabase.yaml"

requestGetStaticIp :: GetStaticIp -> TestTree
requestGetStaticIp = req
    "GetStaticIp"
    "fixture/GetStaticIp.yaml"

requestGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPassword -> TestTree
requestGetRelationalDatabaseMasterUserPassword = req
    "GetRelationalDatabaseMasterUserPassword"
    "fixture/GetRelationalDatabaseMasterUserPassword.yaml"

requestGetBlueprints :: GetBlueprints -> TestTree
requestGetBlueprints = req
    "GetBlueprints"
    "fixture/GetBlueprints.yaml"

requestPutAlarm :: PutAlarm -> TestTree
requestPutAlarm = req
    "PutAlarm"
    "fixture/PutAlarm.yaml"

requestDeleteAlarm :: DeleteAlarm -> TestTree
requestDeleteAlarm = req
    "DeleteAlarm"
    "fixture/DeleteAlarm.yaml"

requestGetInstancePortStates :: GetInstancePortStates -> TestTree
requestGetInstancePortStates = req
    "GetInstancePortStates"
    "fixture/GetInstancePortStates.yaml"

requestDeleteAutoSnapshot :: DeleteAutoSnapshot -> TestTree
requestDeleteAutoSnapshot = req
    "DeleteAutoSnapshot"
    "fixture/DeleteAutoSnapshot.yaml"

requestCreateRelationalDatabase :: CreateRelationalDatabase -> TestTree
requestCreateRelationalDatabase = req
    "CreateRelationalDatabase"
    "fixture/CreateRelationalDatabase.yaml"

requestSendContactMethodVerification :: SendContactMethodVerification -> TestTree
requestSendContactMethodVerification = req
    "SendContactMethodVerification"
    "fixture/SendContactMethodVerification.yaml"

requestGetContainerLog :: GetContainerLog -> TestTree
requestGetContainerLog = req
    "GetContainerLog"
    "fixture/GetContainerLog.yaml"

requestCreateDiskSnapshot :: CreateDiskSnapshot -> TestTree
requestCreateDiskSnapshot = req
    "CreateDiskSnapshot"
    "fixture/CreateDiskSnapshot.yaml"

requestDeleteDomainEntry :: DeleteDomainEntry -> TestTree
requestDeleteDomainEntry = req
    "DeleteDomainEntry"
    "fixture/DeleteDomainEntry.yaml"

requestUpdateDomainEntry :: UpdateDomainEntry -> TestTree
requestUpdateDomainEntry = req
    "UpdateDomainEntry"
    "fixture/UpdateDomainEntry.yaml"

requestGetRegions :: GetRegions -> TestTree
requestGetRegions = req
    "GetRegions"
    "fixture/GetRegions.yaml"

requestDeleteDiskSnapshot :: DeleteDiskSnapshot -> TestTree
requestDeleteDiskSnapshot = req
    "DeleteDiskSnapshot"
    "fixture/DeleteDiskSnapshot.yaml"

requestGetLoadBalancerMetricData :: GetLoadBalancerMetricData -> TestTree
requestGetLoadBalancerMetricData = req
    "GetLoadBalancerMetricData"
    "fixture/GetLoadBalancerMetricData.yaml"

requestGetInstanceState :: GetInstanceState -> TestTree
requestGetInstanceState = req
    "GetInstanceState"
    "fixture/GetInstanceState.yaml"

requestGetKeyPairs :: GetKeyPairs -> TestTree
requestGetKeyPairs = req
    "GetKeyPairs"
    "fixture/GetKeyPairs.yaml"

requestGetOperations :: GetOperations -> TestTree
requestGetOperations = req
    "GetOperations"
    "fixture/GetOperations.yaml"

requestGetDisks :: GetDisks -> TestTree
requestGetDisks = req
    "GetDisks"
    "fixture/GetDisks.yaml"

requestGetRelationalDatabase :: GetRelationalDatabase -> TestTree
requestGetRelationalDatabase = req
    "GetRelationalDatabase"
    "fixture/GetRelationalDatabase.yaml"

requestAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancer -> TestTree
requestAttachInstancesToLoadBalancer = req
    "AttachInstancesToLoadBalancer"
    "fixture/AttachInstancesToLoadBalancer.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetOperation :: GetOperation -> TestTree
requestGetOperation = req
    "GetOperation"
    "fixture/GetOperation.yaml"

requestResetDistributionCache :: ResetDistributionCache -> TestTree
requestResetDistributionCache = req
    "ResetDistributionCache"
    "fixture/ResetDistributionCache.yaml"

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution = req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

requestDeleteDistribution :: DeleteDistribution -> TestTree
requestDeleteDistribution = req
    "DeleteDistribution"
    "fixture/DeleteDistribution.yaml"

requestUpdateContainerService :: UpdateContainerService -> TestTree
requestUpdateContainerService = req
    "UpdateContainerService"
    "fixture/UpdateContainerService.yaml"

requestDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshot -> TestTree
requestDeleteRelationalDatabaseSnapshot = req
    "DeleteRelationalDatabaseSnapshot"
    "fixture/DeleteRelationalDatabaseSnapshot.yaml"

requestDeleteContainerService :: DeleteContainerService -> TestTree
requestDeleteContainerService = req
    "DeleteContainerService"
    "fixture/DeleteContainerService.yaml"

requestGetInstanceMetricData :: GetInstanceMetricData -> TestTree
requestGetInstanceMetricData = req
    "GetInstanceMetricData"
    "fixture/GetInstanceMetricData.yaml"

requestGetKeyPair :: GetKeyPair -> TestTree
requestGetKeyPair = req
    "GetKeyPair"
    "fixture/GetKeyPair.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestPutInstancePublicPorts :: PutInstancePublicPorts -> TestTree
requestPutInstancePublicPorts = req
    "PutInstancePublicPorts"
    "fixture/PutInstancePublicPorts.yaml"

requestGetDistributionBundles :: GetDistributionBundles -> TestTree
requestGetDistributionBundles = req
    "GetDistributionBundles"
    "fixture/GetDistributionBundles.yaml"

requestGetDisk :: GetDisk -> TestTree
requestGetDisk = req
    "GetDisk"
    "fixture/GetDisk.yaml"

requestGetDistributionLatestCacheReset :: GetDistributionLatestCacheReset -> TestTree
requestGetDistributionLatestCacheReset = req
    "GetDistributionLatestCacheReset"
    "fixture/GetDistributionLatestCacheReset.yaml"

requestCreateLoadBalancer :: CreateLoadBalancer -> TestTree
requestCreateLoadBalancer = req
    "CreateLoadBalancer"
    "fixture/CreateLoadBalancer.yaml"

requestGetContainerServiceDeployments :: GetContainerServiceDeployments -> TestTree
requestGetContainerServiceDeployments = req
    "GetContainerServiceDeployments"
    "fixture/GetContainerServiceDeployments.yaml"

requestDeleteKnownHostKeys :: DeleteKnownHostKeys -> TestTree
requestDeleteKnownHostKeys = req
    "DeleteKnownHostKeys"
    "fixture/DeleteKnownHostKeys.yaml"

requestAttachDisk :: AttachDisk -> TestTree
requestAttachDisk = req
    "AttachDisk"
    "fixture/AttachDisk.yaml"

requestDetachStaticIp :: DetachStaticIp -> TestTree
requestDetachStaticIp = req
    "DetachStaticIp"
    "fixture/DetachStaticIp.yaml"

requestCreateInstances :: CreateInstances -> TestTree
requestCreateInstances = req
    "CreateInstances"
    "fixture/CreateInstances.yaml"

requestGetAlarms :: GetAlarms -> TestTree
requestGetAlarms = req
    "GetAlarms"
    "fixture/GetAlarms.yaml"

requestOpenInstancePublicPorts :: OpenInstancePublicPorts -> TestTree
requestOpenInstancePublicPorts = req
    "OpenInstancePublicPorts"
    "fixture/OpenInstancePublicPorts.yaml"

requestStartRelationalDatabase :: StartRelationalDatabase -> TestTree
requestStartRelationalDatabase = req
    "StartRelationalDatabase"
    "fixture/StartRelationalDatabase.yaml"

requestDeleteContainerImage :: DeleteContainerImage -> TestTree
requestDeleteContainerImage = req
    "DeleteContainerImage"
    "fixture/DeleteContainerImage.yaml"

requestGetBundles :: GetBundles -> TestTree
requestGetBundles = req
    "GetBundles"
    "fixture/GetBundles.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain = req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificates -> TestTree
requestGetLoadBalancerTlsCertificates = req
    "GetLoadBalancerTlsCertificates"
    "fixture/GetLoadBalancerTlsCertificates.yaml"

requestCreateDisk :: CreateDisk -> TestTree
requestCreateDisk = req
    "CreateDisk"
    "fixture/CreateDisk.yaml"

requestGetOperationsForResource :: GetOperationsForResource -> TestTree
requestGetOperationsForResource = req
    "GetOperationsForResource"
    "fixture/GetOperationsForResource.yaml"

requestCreateKeyPair :: CreateKeyPair -> TestTree
requestCreateKeyPair = req
    "CreateKeyPair"
    "fixture/CreateKeyPair.yaml"

requestStartInstance :: StartInstance -> TestTree
requestStartInstance = req
    "StartInstance"
    "fixture/StartInstance.yaml"

-- Responses

responseCloseInstancePublicPorts :: CloseInstancePublicPortsResponse -> TestTree
responseCloseInstancePublicPorts = res
    "CloseInstancePublicPortsResponse"
    "fixture/CloseInstancePublicPortsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CloseInstancePublicPorts)

responseGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricDataResponse -> TestTree
responseGetRelationalDatabaseMetricData = res
    "GetRelationalDatabaseMetricDataResponse"
    "fixture/GetRelationalDatabaseMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseMetricData)

responseAllocateStaticIp :: AllocateStaticIpResponse -> TestTree
responseAllocateStaticIp = res
    "AllocateStaticIpResponse"
    "fixture/AllocateStaticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AllocateStaticIp)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair = res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteKeyPair)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot = res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInstanceSnapshot)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances = res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstances)

responseGetLoadBalancer :: GetLoadBalancerResponse -> TestTree
responseGetLoadBalancer = res
    "GetLoadBalancerResponse"
    "fixture/GetLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoadBalancer)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn = res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableAddOn)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions = res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDistributions)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment = res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContainerServiceDeployment)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance = res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstance)

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents = res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseEvents)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution = res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachCertificateToDistribution)

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices = res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerServices)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle = res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDistributionBundle)

responseGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> TestTree
responseGetRelationalDatabaseSnapshots = res
    "GetRelationalDatabaseSnapshotsResponse"
    "fixture/GetRelationalDatabaseSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseSnapshots)

responseAttachStaticIp :: AttachStaticIpResponse -> TestTree
responseAttachStaticIp = res
    "AttachStaticIpResponse"
    "fixture/AttachStaticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachStaticIp)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters = res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseParameters)

responseDetachDisk :: DetachDiskResponse -> TestTree
responseDetachDisk = res
    "DetachDiskResponse"
    "fixture/DetachDiskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachDisk)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods = res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContactMethods)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair = res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DownloadDefaultKeyPair)

responseDeleteLoadBalancerTlsCertificate :: DeleteLoadBalancerTlsCertificateResponse -> TestTree
responseDeleteLoadBalancerTlsCertificate = res
    "DeleteLoadBalancerTlsCertificateResponse"
    "fixture/DeleteLoadBalancerTlsCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoadBalancerTlsCertificate)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm = res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TestAlarm)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains = res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomains)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages = res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerImages)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters = res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRelationalDatabaseParameters)

responseCreateLoadBalancerTlsCertificate :: CreateLoadBalancerTlsCertificateResponse -> TestTree
responseCreateLoadBalancerTlsCertificate = res
    "CreateLoadBalancerTlsCertificateResponse"
    "fixture/CreateLoadBalancerTlsCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoadBalancerTlsCertificate)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry = res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDomainEntry)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers = res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerServicePowers)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair = res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ImportKeyPair)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots = res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceSnapshots)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot = res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ExportSnapshot)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot = res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRelationalDatabaseFromSnapshot)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack = res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCloudFormationStack)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords = res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExportSnapshotRecords)

responseReleaseStaticIp :: ReleaseStaticIpResponse -> TestTree
responseReleaseStaticIp = res
    "ReleaseStaticIpResponse"
    "fixture/ReleaseStaticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ReleaseStaticIp)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance = res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteInstance)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin = res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContainerServiceRegistryLogin)

responseGetCertificates :: GetCertificatesResponse -> TestTree
responseGetCertificates = res
    "GetCertificatesResponse"
    "fixture/GetCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCertificates)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData = res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerServiceMetricData)

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData = res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDistributionMetricData)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance = res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootInstance)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer = res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteLoadBalancer)

responseCreateDiskFromSnapshot :: CreateDiskFromSnapshotResponse -> TestTree
responseCreateDiskFromSnapshot = res
    "CreateDiskFromSnapshotResponse"
    "fixture/CreateDiskFromSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDiskFromSnapshot)

responseGetRelationalDatabases :: GetRelationalDatabasesResponse -> TestTree
responseGetRelationalDatabases = res
    "GetRelationalDatabasesResponse"
    "fixture/GetRelationalDatabasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabases)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot = res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceSnapshot)

responseGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEventsResponse -> TestTree
responseGetRelationalDatabaseLogEvents = res
    "GetRelationalDatabaseLogEventsResponse"
    "fixture/GetRelationalDatabaseLogEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseLogEvents)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod = res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContactMethod)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams = res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseLogStreams)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain = res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDomain)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots = res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAutoSnapshots)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames = res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetActiveNames)

responseDeleteContactMethod :: DeleteContactMethodResponse -> TestTree
responseDeleteContactMethod = res
    "DeleteContactMethodResponse"
    "fixture/DeleteContactMethodResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContactMethod)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution = res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDistribution)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase = res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopRelationalDatabase)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot = res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRelationalDatabaseSnapshot)

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution = res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachCertificateFromDistribution)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService = res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateContainerService)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails = res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceAccessDetails)

responseEnableAddOn :: EnableAddOnResponse -> TestTree
responseEnableAddOn = res
    "EnableAddOnResponse"
    "fixture/EnableAddOnResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableAddOn)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance = res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StopInstance)

responseDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancerResponse -> TestTree
responseDetachInstancesFromLoadBalancer = res
    "DetachInstancesFromLoadBalancerResponse"
    "fixture/DetachInstancesFromLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachInstancesFromLoadBalancer)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage = res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterContainerImage)

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate = res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateCertificate)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot = res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstanceSnapshot)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot = res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CopySnapshot)

responseGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> TestTree
responseGetRelationalDatabaseSnapshot = res
    "GetRelationalDatabaseSnapshotResponse"
    "fixture/GetRelationalDatabaseSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseSnapshot)

responseIsVpcPeered :: IsVpcPeeredResponse -> TestTree
responseIsVpcPeered = res
    "IsVpcPeeredResponse"
    "fixture/IsVpcPeeredResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy IsVpcPeered)

responseGetStaticIps :: GetStaticIpsResponse -> TestTree
responseGetStaticIps = res
    "GetStaticIpsResponse"
    "fixture/GetStaticIpsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStaticIps)

responseUnpeerVpc :: UnpeerVpcResponse -> TestTree
responseUnpeerVpc = res
    "UnpeerVpcResponse"
    "fixture/UnpeerVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnpeerVpc)

responseDeleteDisk :: DeleteDiskResponse -> TestTree
responseDeleteDisk = res
    "DeleteDiskResponse"
    "fixture/DeleteDiskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDisk)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot = res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstancesFromSnapshot)

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords = res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCloudFormationStackRecords)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain = res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDomain)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints = res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseBlueprints)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate = res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteCertificate)

responseGetDiskSnapshots :: GetDiskSnapshotsResponse -> TestTree
responseGetDiskSnapshots = res
    "GetDiskSnapshotsResponse"
    "fixture/GetDiskSnapshotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDiskSnapshots)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata = res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerAPIMetadata)

responsePeerVpc :: PeerVpcResponse -> TestTree
responsePeerVpc = res
    "PeerVpcResponse"
    "fixture/PeerVpcResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PeerVpc)

responseGetRelationalDatabaseBundles :: GetRelationalDatabaseBundlesResponse -> TestTree
responseGetRelationalDatabaseBundles = res
    "GetRelationalDatabaseBundlesResponse"
    "fixture/GetRelationalDatabaseBundlesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseBundles)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers = res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoadBalancers)

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase = res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RebootRelationalDatabase)

responseAttachLoadBalancerTlsCertificate :: AttachLoadBalancerTlsCertificateResponse -> TestTree
responseAttachLoadBalancerTlsCertificate = res
    "AttachLoadBalancerTlsCertificateResponse"
    "fixture/AttachLoadBalancerTlsCertificateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachLoadBalancerTlsCertificate)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute = res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateLoadBalancerAttribute)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase = res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRelationalDatabase)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot = res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDiskSnapshot)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase = res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRelationalDatabase)

responseGetStaticIp :: GetStaticIpResponse -> TestTree
responseGetStaticIp = res
    "GetStaticIpResponse"
    "fixture/GetStaticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetStaticIp)

responseGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> TestTree
responseGetRelationalDatabaseMasterUserPassword = res
    "GetRelationalDatabaseMasterUserPasswordResponse"
    "fixture/GetRelationalDatabaseMasterUserPasswordResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabaseMasterUserPassword)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints = res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBlueprints)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm = res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutAlarm)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm = res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAlarm)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates = res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstancePortStates)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot = res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteAutoSnapshot)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase = res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateRelationalDatabase)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification = res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SendContactMethodVerification)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog = res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerLog)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot = res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDiskSnapshot)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry = res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDomainEntry)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry = res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDomainEntry)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions = res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRegions)

responseDeleteDiskSnapshot :: DeleteDiskSnapshotResponse -> TestTree
responseDeleteDiskSnapshot = res
    "DeleteDiskSnapshotResponse"
    "fixture/DeleteDiskSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDiskSnapshot)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData = res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoadBalancerMetricData)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState = res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceState)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs = res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetKeyPairs)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations = res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOperations)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks = res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDisks)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase = res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetRelationalDatabase)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer = res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachInstancesToLoadBalancer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation = res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOperation)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache = res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ResetDistributionCache)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution = res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateDistribution)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution = res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDistribution)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService = res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateContainerService)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot = res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteRelationalDatabaseSnapshot)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService = res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContainerService)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData = res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetInstanceMetricData)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair = res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetKeyPair)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts = res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutInstancePublicPorts)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles = res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDistributionBundles)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk = res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDisk)

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset = res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetDistributionLatestCacheReset)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer = res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateLoadBalancer)

responseGetContainerServiceDeployments :: GetContainerServiceDeploymentsResponse -> TestTree
responseGetContainerServiceDeployments = res
    "GetContainerServiceDeploymentsResponse"
    "fixture/GetContainerServiceDeploymentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetContainerServiceDeployments)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys = res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteKnownHostKeys)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk = res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachDisk)

responseDetachStaticIp :: DetachStaticIpResponse -> TestTree
responseDetachStaticIp = res
    "DetachStaticIpResponse"
    "fixture/DetachStaticIpResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachStaticIp)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances = res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateInstances)

responseGetAlarms :: GetAlarmsResponse -> TestTree
responseGetAlarms = res
    "GetAlarmsResponse"
    "fixture/GetAlarmsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetAlarms)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts = res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy OpenInstancePublicPorts)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase = res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartRelationalDatabase)

responseDeleteContainerImage :: DeleteContainerImageResponse -> TestTree
responseDeleteContainerImage = res
    "DeleteContainerImageResponse"
    "fixture/DeleteContainerImageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteContainerImage)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles = res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBundles)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain = res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDomain)

responseGetLoadBalancerTlsCertificates :: GetLoadBalancerTlsCertificatesResponse -> TestTree
responseGetLoadBalancerTlsCertificates = res
    "GetLoadBalancerTlsCertificatesResponse"
    "fixture/GetLoadBalancerTlsCertificatesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetLoadBalancerTlsCertificates)

responseCreateDisk :: CreateDiskResponse -> TestTree
responseCreateDisk = res
    "CreateDiskResponse"
    "fixture/CreateDiskResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateDisk)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource = res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetOperationsForResource)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair = res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateKeyPair)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance = res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartInstance)
