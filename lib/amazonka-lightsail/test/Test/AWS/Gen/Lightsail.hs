{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Lightsail
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestCloseInstancePublicPorts $
--             closeInstancePublicPorts
--
--         , requestGetRelationalDatabaseMetricData $
--             getRelationalDatabaseMetricData
--
--         , requestAllocateStaticIP $
--             allocateStaticIP
--
--         , requestDeleteKeyPair $
--             deleteKeyPair
--
--         , requestDeleteInstanceSnapshot $
--             deleteInstanceSnapshot
--
--         , requestGetInstances $
--             getInstances
--
--         , requestGetLoadBalancer $
--             getLoadBalancer
--
--         , requestDisableAddOn $
--             disableAddOn
--
--         , requestGetDistributions $
--             getDistributions
--
--         , requestCreateContainerServiceDeployment $
--             createContainerServiceDeployment
--
--         , requestGetInstance $
--             getInstance
--
--         , requestGetRelationalDatabaseEvents $
--             getRelationalDatabaseEvents
--
--         , requestAttachCertificateToDistribution $
--             attachCertificateToDistribution
--
--         , requestGetContainerServices $
--             getContainerServices
--
--         , requestUpdateDistributionBundle $
--             updateDistributionBundle
--
--         , requestGetRelationalDatabaseSnapshots $
--             getRelationalDatabaseSnapshots
--
--         , requestAttachStaticIP $
--             attachStaticIP
--
--         , requestGetRelationalDatabaseParameters $
--             getRelationalDatabaseParameters
--
--         , requestDetachDisk $
--             detachDisk
--
--         , requestGetContactMethods $
--             getContactMethods
--
--         , requestDownloadDefaultKeyPair $
--             downloadDefaultKeyPair
--
--         , requestDeleteLoadBalancerTLSCertificate $
--             deleteLoadBalancerTLSCertificate
--
--         , requestTestAlarm $
--             testAlarm
--
--         , requestGetDomains $
--             getDomains
--
--         , requestGetContainerImages $
--             getContainerImages
--
--         , requestUpdateRelationalDatabaseParameters $
--             updateRelationalDatabaseParameters
--
--         , requestCreateLoadBalancerTLSCertificate $
--             createLoadBalancerTLSCertificate
--
--         , requestCreateDomainEntry $
--             createDomainEntry
--
--         , requestGetContainerServicePowers $
--             getContainerServicePowers
--
--         , requestImportKeyPair $
--             importKeyPair
--
--         , requestGetInstanceSnapshots $
--             getInstanceSnapshots
--
--         , requestExportSnapshot $
--             exportSnapshot
--
--         , requestCreateRelationalDatabaseFromSnapshot $
--             createRelationalDatabaseFromSnapshot
--
--         , requestCreateCloudFormationStack $
--             createCloudFormationStack
--
--         , requestGetExportSnapshotRecords $
--             getExportSnapshotRecords
--
--         , requestReleaseStaticIP $
--             releaseStaticIP
--
--         , requestDeleteInstance $
--             deleteInstance
--
--         , requestCreateContainerServiceRegistryLogin $
--             createContainerServiceRegistryLogin
--
--         , requestGetCertificates $
--             getCertificates
--
--         , requestGetContainerServiceMetricData $
--             getContainerServiceMetricData
--
--         , requestGetDistributionMetricData $
--             getDistributionMetricData
--
--         , requestRebootInstance $
--             rebootInstance
--
--         , requestDeleteLoadBalancer $
--             deleteLoadBalancer
--
--         , requestCreateDiskFromSnapshot $
--             createDiskFromSnapshot
--
--         , requestGetRelationalDatabases $
--             getRelationalDatabases
--
--         , requestGetInstanceSnapshot $
--             getInstanceSnapshot
--
--         , requestGetRelationalDatabaseLogEvents $
--             getRelationalDatabaseLogEvents
--
--         , requestCreateContactMethod $
--             createContactMethod
--
--         , requestGetRelationalDatabaseLogStreams $
--             getRelationalDatabaseLogStreams
--
--         , requestGetDomain $
--             getDomain
--
--         , requestGetAutoSnapshots $
--             getAutoSnapshots
--
--         , requestGetActiveNames $
--             getActiveNames
--
--         , requestDeleteContactMethod $
--             deleteContactMethod
--
--         , requestCreateDistribution $
--             createDistribution
--
--         , requestStopRelationalDatabase $
--             stopRelationalDatabase
--
--         , requestCreateRelationalDatabaseSnapshot $
--             createRelationalDatabaseSnapshot
--
--         , requestDetachCertificateFromDistribution $
--             detachCertificateFromDistribution
--
--         , requestCreateContainerService $
--             createContainerService
--
--         , requestGetInstanceAccessDetails $
--             getInstanceAccessDetails
--
--         , requestEnableAddOn $
--             enableAddOn
--
--         , requestStopInstance $
--             stopInstance
--
--         , requestDetachInstancesFromLoadBalancer $
--             detachInstancesFromLoadBalancer
--
--         , requestRegisterContainerImage $
--             registerContainerImage
--
--         , requestCreateCertificate $
--             createCertificate
--
--         , requestCreateInstanceSnapshot $
--             createInstanceSnapshot
--
--         , requestCopySnapshot $
--             copySnapshot
--
--         , requestGetRelationalDatabaseSnapshot $
--             getRelationalDatabaseSnapshot
--
--         , requestIsVPCPeered $
--             isVPCPeered
--
--         , requestGetStaticIPs $
--             getStaticIPs
--
--         , requestUnpeerVPC $
--             unpeerVPC
--
--         , requestDeleteDisk $
--             deleteDisk
--
--         , requestCreateInstancesFromSnapshot $
--             createInstancesFromSnapshot
--
--         , requestGetCloudFormationStackRecords $
--             getCloudFormationStackRecords
--
--         , requestCreateDomain $
--             createDomain
--
--         , requestGetRelationalDatabaseBlueprints $
--             getRelationalDatabaseBlueprints
--
--         , requestDeleteCertificate $
--             deleteCertificate
--
--         , requestGetDiskSnapshots $
--             getDiskSnapshots
--
--         , requestGetContainerAPIMetadata $
--             getContainerAPIMetadata
--
--         , requestPeerVPC $
--             peerVPC
--
--         , requestGetRelationalDatabaseBundles $
--             getRelationalDatabaseBundles
--
--         , requestGetLoadBalancers $
--             getLoadBalancers
--
--         , requestRebootRelationalDatabase $
--             rebootRelationalDatabase
--
--         , requestAttachLoadBalancerTLSCertificate $
--             attachLoadBalancerTLSCertificate
--
--         , requestUpdateLoadBalancerAttribute $
--             updateLoadBalancerAttribute
--
--         , requestDeleteRelationalDatabase $
--             deleteRelationalDatabase
--
--         , requestGetDiskSnapshot $
--             getDiskSnapshot
--
--         , requestUpdateRelationalDatabase $
--             updateRelationalDatabase
--
--         , requestGetStaticIP $
--             getStaticIP
--
--         , requestGetRelationalDatabaseMasterUserPassword $
--             getRelationalDatabaseMasterUserPassword
--
--         , requestGetBlueprints $
--             getBlueprints
--
--         , requestPutAlarm $
--             putAlarm
--
--         , requestDeleteAlarm $
--             deleteAlarm
--
--         , requestGetInstancePortStates $
--             getInstancePortStates
--
--         , requestDeleteAutoSnapshot $
--             deleteAutoSnapshot
--
--         , requestCreateRelationalDatabase $
--             createRelationalDatabase
--
--         , requestSendContactMethodVerification $
--             sendContactMethodVerification
--
--         , requestGetContainerLog $
--             getContainerLog
--
--         , requestCreateDiskSnapshot $
--             createDiskSnapshot
--
--         , requestDeleteDomainEntry $
--             deleteDomainEntry
--
--         , requestUpdateDomainEntry $
--             updateDomainEntry
--
--         , requestGetRegions $
--             getRegions
--
--         , requestDeleteDiskSnapshot $
--             deleteDiskSnapshot
--
--         , requestGetLoadBalancerMetricData $
--             getLoadBalancerMetricData
--
--         , requestGetInstanceState $
--             getInstanceState
--
--         , requestGetKeyPairs $
--             getKeyPairs
--
--         , requestGetOperations $
--             getOperations
--
--         , requestGetDisks $
--             getDisks
--
--         , requestGetRelationalDatabase $
--             getRelationalDatabase
--
--         , requestAttachInstancesToLoadBalancer $
--             attachInstancesToLoadBalancer
--
--         , requestTagResource $
--             tagResource
--
--         , requestGetOperation $
--             getOperation
--
--         , requestResetDistributionCache $
--             resetDistributionCache
--
--         , requestUpdateDistribution $
--             updateDistribution
--
--         , requestDeleteDistribution $
--             deleteDistribution
--
--         , requestUpdateContainerService $
--             updateContainerService
--
--         , requestDeleteRelationalDatabaseSnapshot $
--             deleteRelationalDatabaseSnapshot
--
--         , requestDeleteContainerService $
--             deleteContainerService
--
--         , requestGetInstanceMetricData $
--             getInstanceMetricData
--
--         , requestGetKeyPair $
--             getKeyPair
--
--         , requestUntagResource $
--             untagResource
--
--         , requestPutInstancePublicPorts $
--             putInstancePublicPorts
--
--         , requestGetDistributionBundles $
--             getDistributionBundles
--
--         , requestGetDisk $
--             getDisk
--
--         , requestGetDistributionLatestCacheReset $
--             getDistributionLatestCacheReset
--
--         , requestCreateLoadBalancer $
--             createLoadBalancer
--
--         , requestGetContainerServiceDeployments $
--             getContainerServiceDeployments
--
--         , requestDeleteKnownHostKeys $
--             deleteKnownHostKeys
--
--         , requestAttachDisk $
--             attachDisk
--
--         , requestDetachStaticIP $
--             detachStaticIP
--
--         , requestCreateInstances $
--             createInstances
--
--         , requestGetAlarms $
--             getAlarms
--
--         , requestOpenInstancePublicPorts $
--             openInstancePublicPorts
--
--         , requestStartRelationalDatabase $
--             startRelationalDatabase
--
--         , requestDeleteContainerImage $
--             deleteContainerImage
--
--         , requestGetBundles $
--             getBundles
--
--         , requestDeleteDomain $
--             deleteDomain
--
--         , requestGetLoadBalancerTLSCertificates $
--             getLoadBalancerTLSCertificates
--
--         , requestCreateDisk $
--             createDisk
--
--         , requestGetOperationsForResource $
--             getOperationsForResource
--
--         , requestCreateKeyPair $
--             createKeyPair
--
--         , requestStartInstance $
--             startInstance
--
--           ]

--     , testGroup "response"
--         [ responseCloseInstancePublicPorts $
--             closeInstancePublicPortsResponse
--
--         , responseGetRelationalDatabaseMetricData $
--             getRelationalDatabaseMetricDataResponse
--
--         , responseAllocateStaticIP $
--             allocateStaticIPResponse
--
--         , responseDeleteKeyPair $
--             deleteKeyPairResponse
--
--         , responseDeleteInstanceSnapshot $
--             deleteInstanceSnapshotResponse
--
--         , responseGetInstances $
--             getInstancesResponse
--
--         , responseGetLoadBalancer $
--             getLoadBalancerResponse
--
--         , responseDisableAddOn $
--             disableAddOnResponse
--
--         , responseGetDistributions $
--             getDistributionsResponse
--
--         , responseCreateContainerServiceDeployment $
--             createContainerServiceDeploymentResponse
--
--         , responseGetInstance $
--             getInstanceResponse
--
--         , responseGetRelationalDatabaseEvents $
--             getRelationalDatabaseEventsResponse
--
--         , responseAttachCertificateToDistribution $
--             attachCertificateToDistributionResponse
--
--         , responseGetContainerServices $
--             getContainerServicesResponse
--
--         , responseUpdateDistributionBundle $
--             updateDistributionBundleResponse
--
--         , responseGetRelationalDatabaseSnapshots $
--             getRelationalDatabaseSnapshotsResponse
--
--         , responseAttachStaticIP $
--             attachStaticIPResponse
--
--         , responseGetRelationalDatabaseParameters $
--             getRelationalDatabaseParametersResponse
--
--         , responseDetachDisk $
--             detachDiskResponse
--
--         , responseGetContactMethods $
--             getContactMethodsResponse
--
--         , responseDownloadDefaultKeyPair $
--             downloadDefaultKeyPairResponse
--
--         , responseDeleteLoadBalancerTLSCertificate $
--             deleteLoadBalancerTLSCertificateResponse
--
--         , responseTestAlarm $
--             testAlarmResponse
--
--         , responseGetDomains $
--             getDomainsResponse
--
--         , responseGetContainerImages $
--             getContainerImagesResponse
--
--         , responseUpdateRelationalDatabaseParameters $
--             updateRelationalDatabaseParametersResponse
--
--         , responseCreateLoadBalancerTLSCertificate $
--             createLoadBalancerTLSCertificateResponse
--
--         , responseCreateDomainEntry $
--             createDomainEntryResponse
--
--         , responseGetContainerServicePowers $
--             getContainerServicePowersResponse
--
--         , responseImportKeyPair $
--             importKeyPairResponse
--
--         , responseGetInstanceSnapshots $
--             getInstanceSnapshotsResponse
--
--         , responseExportSnapshot $
--             exportSnapshotResponse
--
--         , responseCreateRelationalDatabaseFromSnapshot $
--             createRelationalDatabaseFromSnapshotResponse
--
--         , responseCreateCloudFormationStack $
--             createCloudFormationStackResponse
--
--         , responseGetExportSnapshotRecords $
--             getExportSnapshotRecordsResponse
--
--         , responseReleaseStaticIP $
--             releaseStaticIPResponse
--
--         , responseDeleteInstance $
--             deleteInstanceResponse
--
--         , responseCreateContainerServiceRegistryLogin $
--             createContainerServiceRegistryLoginResponse
--
--         , responseGetCertificates $
--             getCertificatesResponse
--
--         , responseGetContainerServiceMetricData $
--             getContainerServiceMetricDataResponse
--
--         , responseGetDistributionMetricData $
--             getDistributionMetricDataResponse
--
--         , responseRebootInstance $
--             rebootInstanceResponse
--
--         , responseDeleteLoadBalancer $
--             deleteLoadBalancerResponse
--
--         , responseCreateDiskFromSnapshot $
--             createDiskFromSnapshotResponse
--
--         , responseGetRelationalDatabases $
--             getRelationalDatabasesResponse
--
--         , responseGetInstanceSnapshot $
--             getInstanceSnapshotResponse
--
--         , responseGetRelationalDatabaseLogEvents $
--             getRelationalDatabaseLogEventsResponse
--
--         , responseCreateContactMethod $
--             createContactMethodResponse
--
--         , responseGetRelationalDatabaseLogStreams $
--             getRelationalDatabaseLogStreamsResponse
--
--         , responseGetDomain $
--             getDomainResponse
--
--         , responseGetAutoSnapshots $
--             getAutoSnapshotsResponse
--
--         , responseGetActiveNames $
--             getActiveNamesResponse
--
--         , responseDeleteContactMethod $
--             deleteContactMethodResponse
--
--         , responseCreateDistribution $
--             createDistributionResponse
--
--         , responseStopRelationalDatabase $
--             stopRelationalDatabaseResponse
--
--         , responseCreateRelationalDatabaseSnapshot $
--             createRelationalDatabaseSnapshotResponse
--
--         , responseDetachCertificateFromDistribution $
--             detachCertificateFromDistributionResponse
--
--         , responseCreateContainerService $
--             createContainerServiceResponse
--
--         , responseGetInstanceAccessDetails $
--             getInstanceAccessDetailsResponse
--
--         , responseEnableAddOn $
--             enableAddOnResponse
--
--         , responseStopInstance $
--             stopInstanceResponse
--
--         , responseDetachInstancesFromLoadBalancer $
--             detachInstancesFromLoadBalancerResponse
--
--         , responseRegisterContainerImage $
--             registerContainerImageResponse
--
--         , responseCreateCertificate $
--             createCertificateResponse
--
--         , responseCreateInstanceSnapshot $
--             createInstanceSnapshotResponse
--
--         , responseCopySnapshot $
--             copySnapshotResponse
--
--         , responseGetRelationalDatabaseSnapshot $
--             getRelationalDatabaseSnapshotResponse
--
--         , responseIsVPCPeered $
--             isVPCPeeredResponse
--
--         , responseGetStaticIPs $
--             getStaticIPsResponse
--
--         , responseUnpeerVPC $
--             unpeerVPCResponse
--
--         , responseDeleteDisk $
--             deleteDiskResponse
--
--         , responseCreateInstancesFromSnapshot $
--             createInstancesFromSnapshotResponse
--
--         , responseGetCloudFormationStackRecords $
--             getCloudFormationStackRecordsResponse
--
--         , responseCreateDomain $
--             createDomainResponse
--
--         , responseGetRelationalDatabaseBlueprints $
--             getRelationalDatabaseBlueprintsResponse
--
--         , responseDeleteCertificate $
--             deleteCertificateResponse
--
--         , responseGetDiskSnapshots $
--             getDiskSnapshotsResponse
--
--         , responseGetContainerAPIMetadata $
--             getContainerAPIMetadataResponse
--
--         , responsePeerVPC $
--             peerVPCResponse
--
--         , responseGetRelationalDatabaseBundles $
--             getRelationalDatabaseBundlesResponse
--
--         , responseGetLoadBalancers $
--             getLoadBalancersResponse
--
--         , responseRebootRelationalDatabase $
--             rebootRelationalDatabaseResponse
--
--         , responseAttachLoadBalancerTLSCertificate $
--             attachLoadBalancerTLSCertificateResponse
--
--         , responseUpdateLoadBalancerAttribute $
--             updateLoadBalancerAttributeResponse
--
--         , responseDeleteRelationalDatabase $
--             deleteRelationalDatabaseResponse
--
--         , responseGetDiskSnapshot $
--             getDiskSnapshotResponse
--
--         , responseUpdateRelationalDatabase $
--             updateRelationalDatabaseResponse
--
--         , responseGetStaticIP $
--             getStaticIPResponse
--
--         , responseGetRelationalDatabaseMasterUserPassword $
--             getRelationalDatabaseMasterUserPasswordResponse
--
--         , responseGetBlueprints $
--             getBlueprintsResponse
--
--         , responsePutAlarm $
--             putAlarmResponse
--
--         , responseDeleteAlarm $
--             deleteAlarmResponse
--
--         , responseGetInstancePortStates $
--             getInstancePortStatesResponse
--
--         , responseDeleteAutoSnapshot $
--             deleteAutoSnapshotResponse
--
--         , responseCreateRelationalDatabase $
--             createRelationalDatabaseResponse
--
--         , responseSendContactMethodVerification $
--             sendContactMethodVerificationResponse
--
--         , responseGetContainerLog $
--             getContainerLogResponse
--
--         , responseCreateDiskSnapshot $
--             createDiskSnapshotResponse
--
--         , responseDeleteDomainEntry $
--             deleteDomainEntryResponse
--
--         , responseUpdateDomainEntry $
--             updateDomainEntryResponse
--
--         , responseGetRegions $
--             getRegionsResponse
--
--         , responseDeleteDiskSnapshot $
--             deleteDiskSnapshotResponse
--
--         , responseGetLoadBalancerMetricData $
--             getLoadBalancerMetricDataResponse
--
--         , responseGetInstanceState $
--             getInstanceStateResponse
--
--         , responseGetKeyPairs $
--             getKeyPairsResponse
--
--         , responseGetOperations $
--             getOperationsResponse
--
--         , responseGetDisks $
--             getDisksResponse
--
--         , responseGetRelationalDatabase $
--             getRelationalDatabaseResponse
--
--         , responseAttachInstancesToLoadBalancer $
--             attachInstancesToLoadBalancerResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseGetOperation $
--             getOperationResponse
--
--         , responseResetDistributionCache $
--             resetDistributionCacheResponse
--
--         , responseUpdateDistribution $
--             updateDistributionResponse
--
--         , responseDeleteDistribution $
--             deleteDistributionResponse
--
--         , responseUpdateContainerService $
--             updateContainerServiceResponse
--
--         , responseDeleteRelationalDatabaseSnapshot $
--             deleteRelationalDatabaseSnapshotResponse
--
--         , responseDeleteContainerService $
--             deleteContainerServiceResponse
--
--         , responseGetInstanceMetricData $
--             getInstanceMetricDataResponse
--
--         , responseGetKeyPair $
--             getKeyPairResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responsePutInstancePublicPorts $
--             putInstancePublicPortsResponse
--
--         , responseGetDistributionBundles $
--             getDistributionBundlesResponse
--
--         , responseGetDisk $
--             getDiskResponse
--
--         , responseGetDistributionLatestCacheReset $
--             getDistributionLatestCacheResetResponse
--
--         , responseCreateLoadBalancer $
--             createLoadBalancerResponse
--
--         , responseGetContainerServiceDeployments $
--             getContainerServiceDeploymentsResponse
--
--         , responseDeleteKnownHostKeys $
--             deleteKnownHostKeysResponse
--
--         , responseAttachDisk $
--             attachDiskResponse
--
--         , responseDetachStaticIP $
--             detachStaticIPResponse
--
--         , responseCreateInstances $
--             createInstancesResponse
--
--         , responseGetAlarms $
--             getAlarmsResponse
--
--         , responseOpenInstancePublicPorts $
--             openInstancePublicPortsResponse
--
--         , responseStartRelationalDatabase $
--             startRelationalDatabaseResponse
--
--         , responseDeleteContainerImage $
--             deleteContainerImageResponse
--
--         , responseGetBundles $
--             getBundlesResponse
--
--         , responseDeleteDomain $
--             deleteDomainResponse
--
--         , responseGetLoadBalancerTLSCertificates $
--             getLoadBalancerTLSCertificatesResponse
--
--         , responseCreateDisk $
--             createDiskResponse
--
--         , responseGetOperationsForResource $
--             getOperationsForResourceResponse
--
--         , responseCreateKeyPair $
--             createKeyPairResponse
--
--         , responseStartInstance $
--             startInstanceResponse
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

requestAllocateStaticIP :: AllocateStaticIP -> TestTree
requestAllocateStaticIP =
  req
    "AllocateStaticIP"
    "fixture/AllocateStaticIP.yaml"

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

requestAttachStaticIP :: AttachStaticIP -> TestTree
requestAttachStaticIP =
  req
    "AttachStaticIP"
    "fixture/AttachStaticIP.yaml"

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

requestDeleteLoadBalancerTLSCertificate :: DeleteLoadBalancerTLSCertificate -> TestTree
requestDeleteLoadBalancerTLSCertificate =
  req
    "DeleteLoadBalancerTLSCertificate"
    "fixture/DeleteLoadBalancerTLSCertificate.yaml"

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

requestCreateLoadBalancerTLSCertificate :: CreateLoadBalancerTLSCertificate -> TestTree
requestCreateLoadBalancerTLSCertificate =
  req
    "CreateLoadBalancerTLSCertificate"
    "fixture/CreateLoadBalancerTLSCertificate.yaml"

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

requestReleaseStaticIP :: ReleaseStaticIP -> TestTree
requestReleaseStaticIP =
  req
    "ReleaseStaticIP"
    "fixture/ReleaseStaticIP.yaml"

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

requestIsVPCPeered :: IsVPCPeered -> TestTree
requestIsVPCPeered =
  req
    "IsVPCPeered"
    "fixture/IsVPCPeered.yaml"

requestGetStaticIPs :: GetStaticIPs -> TestTree
requestGetStaticIPs =
  req
    "GetStaticIPs"
    "fixture/GetStaticIPs.yaml"

requestUnpeerVPC :: UnpeerVPC -> TestTree
requestUnpeerVPC =
  req
    "UnpeerVPC"
    "fixture/UnpeerVPC.yaml"

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

requestPeerVPC :: PeerVPC -> TestTree
requestPeerVPC =
  req
    "PeerVPC"
    "fixture/PeerVPC.yaml"

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

requestAttachLoadBalancerTLSCertificate :: AttachLoadBalancerTLSCertificate -> TestTree
requestAttachLoadBalancerTLSCertificate =
  req
    "AttachLoadBalancerTLSCertificate"
    "fixture/AttachLoadBalancerTLSCertificate.yaml"

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

requestGetStaticIP :: GetStaticIP -> TestTree
requestGetStaticIP =
  req
    "GetStaticIP"
    "fixture/GetStaticIP.yaml"

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

requestUpdateDistribution :: UpdateDistribution -> TestTree
requestUpdateDistribution =
  req
    "UpdateDistribution"
    "fixture/UpdateDistribution.yaml"

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

requestDetachStaticIP :: DetachStaticIP -> TestTree
requestDetachStaticIP =
  req
    "DetachStaticIP"
    "fixture/DetachStaticIP.yaml"

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

requestGetLoadBalancerTLSCertificates :: GetLoadBalancerTLSCertificates -> TestTree
requestGetLoadBalancerTLSCertificates =
  req
    "GetLoadBalancerTLSCertificates"
    "fixture/GetLoadBalancerTLSCertificates.yaml"

requestCreateDisk :: CreateDisk -> TestTree
requestCreateDisk =
  req
    "CreateDisk"
    "fixture/CreateDisk.yaml"

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
    lightsail
    (Proxy :: Proxy CloseInstancePublicPorts)

responseGetRelationalDatabaseMetricData :: GetRelationalDatabaseMetricDataResponse -> TestTree
responseGetRelationalDatabaseMetricData =
  res
    "GetRelationalDatabaseMetricDataResponse"
    "fixture/GetRelationalDatabaseMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseMetricData)

responseAllocateStaticIP :: AllocateStaticIPResponse -> TestTree
responseAllocateStaticIP =
  res
    "AllocateStaticIPResponse"
    "fixture/AllocateStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy AllocateStaticIP)

responseDeleteKeyPair :: DeleteKeyPairResponse -> TestTree
responseDeleteKeyPair =
  res
    "DeleteKeyPairResponse"
    "fixture/DeleteKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteKeyPair)

responseDeleteInstanceSnapshot :: DeleteInstanceSnapshotResponse -> TestTree
responseDeleteInstanceSnapshot =
  res
    "DeleteInstanceSnapshotResponse"
    "fixture/DeleteInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteInstanceSnapshot)

responseGetInstances :: GetInstancesResponse -> TestTree
responseGetInstances =
  res
    "GetInstancesResponse"
    "fixture/GetInstancesResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstances)

responseGetLoadBalancer :: GetLoadBalancerResponse -> TestTree
responseGetLoadBalancer =
  res
    "GetLoadBalancerResponse"
    "fixture/GetLoadBalancerResponse.proto"
    lightsail
    (Proxy :: Proxy GetLoadBalancer)

responseDisableAddOn :: DisableAddOnResponse -> TestTree
responseDisableAddOn =
  res
    "DisableAddOnResponse"
    "fixture/DisableAddOnResponse.proto"
    lightsail
    (Proxy :: Proxy DisableAddOn)

responseGetDistributions :: GetDistributionsResponse -> TestTree
responseGetDistributions =
  res
    "GetDistributionsResponse"
    "fixture/GetDistributionsResponse.proto"
    lightsail
    (Proxy :: Proxy GetDistributions)

responseCreateContainerServiceDeployment :: CreateContainerServiceDeploymentResponse -> TestTree
responseCreateContainerServiceDeployment =
  res
    "CreateContainerServiceDeploymentResponse"
    "fixture/CreateContainerServiceDeploymentResponse.proto"
    lightsail
    (Proxy :: Proxy CreateContainerServiceDeployment)

responseGetInstance :: GetInstanceResponse -> TestTree
responseGetInstance =
  res
    "GetInstanceResponse"
    "fixture/GetInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstance)

responseGetRelationalDatabaseEvents :: GetRelationalDatabaseEventsResponse -> TestTree
responseGetRelationalDatabaseEvents =
  res
    "GetRelationalDatabaseEventsResponse"
    "fixture/GetRelationalDatabaseEventsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseEvents)

responseAttachCertificateToDistribution :: AttachCertificateToDistributionResponse -> TestTree
responseAttachCertificateToDistribution =
  res
    "AttachCertificateToDistributionResponse"
    "fixture/AttachCertificateToDistributionResponse.proto"
    lightsail
    (Proxy :: Proxy AttachCertificateToDistribution)

responseGetContainerServices :: GetContainerServicesResponse -> TestTree
responseGetContainerServices =
  res
    "GetContainerServicesResponse"
    "fixture/GetContainerServicesResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerServices)

responseUpdateDistributionBundle :: UpdateDistributionBundleResponse -> TestTree
responseUpdateDistributionBundle =
  res
    "UpdateDistributionBundleResponse"
    "fixture/UpdateDistributionBundleResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateDistributionBundle)

responseGetRelationalDatabaseSnapshots :: GetRelationalDatabaseSnapshotsResponse -> TestTree
responseGetRelationalDatabaseSnapshots =
  res
    "GetRelationalDatabaseSnapshotsResponse"
    "fixture/GetRelationalDatabaseSnapshotsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseSnapshots)

responseAttachStaticIP :: AttachStaticIPResponse -> TestTree
responseAttachStaticIP =
  res
    "AttachStaticIPResponse"
    "fixture/AttachStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy AttachStaticIP)

responseGetRelationalDatabaseParameters :: GetRelationalDatabaseParametersResponse -> TestTree
responseGetRelationalDatabaseParameters =
  res
    "GetRelationalDatabaseParametersResponse"
    "fixture/GetRelationalDatabaseParametersResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseParameters)

responseDetachDisk :: DetachDiskResponse -> TestTree
responseDetachDisk =
  res
    "DetachDiskResponse"
    "fixture/DetachDiskResponse.proto"
    lightsail
    (Proxy :: Proxy DetachDisk)

responseGetContactMethods :: GetContactMethodsResponse -> TestTree
responseGetContactMethods =
  res
    "GetContactMethodsResponse"
    "fixture/GetContactMethodsResponse.proto"
    lightsail
    (Proxy :: Proxy GetContactMethods)

responseDownloadDefaultKeyPair :: DownloadDefaultKeyPairResponse -> TestTree
responseDownloadDefaultKeyPair =
  res
    "DownloadDefaultKeyPairResponse"
    "fixture/DownloadDefaultKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy DownloadDefaultKeyPair)

responseDeleteLoadBalancerTLSCertificate :: DeleteLoadBalancerTLSCertificateResponse -> TestTree
responseDeleteLoadBalancerTLSCertificate =
  res
    "DeleteLoadBalancerTLSCertificateResponse"
    "fixture/DeleteLoadBalancerTLSCertificateResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteLoadBalancerTLSCertificate)

responseTestAlarm :: TestAlarmResponse -> TestTree
responseTestAlarm =
  res
    "TestAlarmResponse"
    "fixture/TestAlarmResponse.proto"
    lightsail
    (Proxy :: Proxy TestAlarm)

responseGetDomains :: GetDomainsResponse -> TestTree
responseGetDomains =
  res
    "GetDomainsResponse"
    "fixture/GetDomainsResponse.proto"
    lightsail
    (Proxy :: Proxy GetDomains)

responseGetContainerImages :: GetContainerImagesResponse -> TestTree
responseGetContainerImages =
  res
    "GetContainerImagesResponse"
    "fixture/GetContainerImagesResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerImages)

responseUpdateRelationalDatabaseParameters :: UpdateRelationalDatabaseParametersResponse -> TestTree
responseUpdateRelationalDatabaseParameters =
  res
    "UpdateRelationalDatabaseParametersResponse"
    "fixture/UpdateRelationalDatabaseParametersResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateRelationalDatabaseParameters)

responseCreateLoadBalancerTLSCertificate :: CreateLoadBalancerTLSCertificateResponse -> TestTree
responseCreateLoadBalancerTLSCertificate =
  res
    "CreateLoadBalancerTLSCertificateResponse"
    "fixture/CreateLoadBalancerTLSCertificateResponse.proto"
    lightsail
    (Proxy :: Proxy CreateLoadBalancerTLSCertificate)

responseCreateDomainEntry :: CreateDomainEntryResponse -> TestTree
responseCreateDomainEntry =
  res
    "CreateDomainEntryResponse"
    "fixture/CreateDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDomainEntry)

responseGetContainerServicePowers :: GetContainerServicePowersResponse -> TestTree
responseGetContainerServicePowers =
  res
    "GetContainerServicePowersResponse"
    "fixture/GetContainerServicePowersResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerServicePowers)

responseImportKeyPair :: ImportKeyPairResponse -> TestTree
responseImportKeyPair =
  res
    "ImportKeyPairResponse"
    "fixture/ImportKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy ImportKeyPair)

responseGetInstanceSnapshots :: GetInstanceSnapshotsResponse -> TestTree
responseGetInstanceSnapshots =
  res
    "GetInstanceSnapshotsResponse"
    "fixture/GetInstanceSnapshotsResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceSnapshots)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy ExportSnapshot)

responseCreateRelationalDatabaseFromSnapshot :: CreateRelationalDatabaseFromSnapshotResponse -> TestTree
responseCreateRelationalDatabaseFromSnapshot =
  res
    "CreateRelationalDatabaseFromSnapshotResponse"
    "fixture/CreateRelationalDatabaseFromSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateRelationalDatabaseFromSnapshot)

responseCreateCloudFormationStack :: CreateCloudFormationStackResponse -> TestTree
responseCreateCloudFormationStack =
  res
    "CreateCloudFormationStackResponse"
    "fixture/CreateCloudFormationStackResponse.proto"
    lightsail
    (Proxy :: Proxy CreateCloudFormationStack)

responseGetExportSnapshotRecords :: GetExportSnapshotRecordsResponse -> TestTree
responseGetExportSnapshotRecords =
  res
    "GetExportSnapshotRecordsResponse"
    "fixture/GetExportSnapshotRecordsResponse.proto"
    lightsail
    (Proxy :: Proxy GetExportSnapshotRecords)

responseReleaseStaticIP :: ReleaseStaticIPResponse -> TestTree
responseReleaseStaticIP =
  res
    "ReleaseStaticIPResponse"
    "fixture/ReleaseStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy ReleaseStaticIP)

responseDeleteInstance :: DeleteInstanceResponse -> TestTree
responseDeleteInstance =
  res
    "DeleteInstanceResponse"
    "fixture/DeleteInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteInstance)

responseCreateContainerServiceRegistryLogin :: CreateContainerServiceRegistryLoginResponse -> TestTree
responseCreateContainerServiceRegistryLogin =
  res
    "CreateContainerServiceRegistryLoginResponse"
    "fixture/CreateContainerServiceRegistryLoginResponse.proto"
    lightsail
    (Proxy :: Proxy CreateContainerServiceRegistryLogin)

responseGetCertificates :: GetCertificatesResponse -> TestTree
responseGetCertificates =
  res
    "GetCertificatesResponse"
    "fixture/GetCertificatesResponse.proto"
    lightsail
    (Proxy :: Proxy GetCertificates)

responseGetContainerServiceMetricData :: GetContainerServiceMetricDataResponse -> TestTree
responseGetContainerServiceMetricData =
  res
    "GetContainerServiceMetricDataResponse"
    "fixture/GetContainerServiceMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerServiceMetricData)

responseGetDistributionMetricData :: GetDistributionMetricDataResponse -> TestTree
responseGetDistributionMetricData =
  res
    "GetDistributionMetricDataResponse"
    "fixture/GetDistributionMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetDistributionMetricData)

responseRebootInstance :: RebootInstanceResponse -> TestTree
responseRebootInstance =
  res
    "RebootInstanceResponse"
    "fixture/RebootInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy RebootInstance)

responseDeleteLoadBalancer :: DeleteLoadBalancerResponse -> TestTree
responseDeleteLoadBalancer =
  res
    "DeleteLoadBalancerResponse"
    "fixture/DeleteLoadBalancerResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteLoadBalancer)

responseCreateDiskFromSnapshot :: CreateDiskFromSnapshotResponse -> TestTree
responseCreateDiskFromSnapshot =
  res
    "CreateDiskFromSnapshotResponse"
    "fixture/CreateDiskFromSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDiskFromSnapshot)

responseGetRelationalDatabases :: GetRelationalDatabasesResponse -> TestTree
responseGetRelationalDatabases =
  res
    "GetRelationalDatabasesResponse"
    "fixture/GetRelationalDatabasesResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabases)

responseGetInstanceSnapshot :: GetInstanceSnapshotResponse -> TestTree
responseGetInstanceSnapshot =
  res
    "GetInstanceSnapshotResponse"
    "fixture/GetInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceSnapshot)

responseGetRelationalDatabaseLogEvents :: GetRelationalDatabaseLogEventsResponse -> TestTree
responseGetRelationalDatabaseLogEvents =
  res
    "GetRelationalDatabaseLogEventsResponse"
    "fixture/GetRelationalDatabaseLogEventsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseLogEvents)

responseCreateContactMethod :: CreateContactMethodResponse -> TestTree
responseCreateContactMethod =
  res
    "CreateContactMethodResponse"
    "fixture/CreateContactMethodResponse.proto"
    lightsail
    (Proxy :: Proxy CreateContactMethod)

responseGetRelationalDatabaseLogStreams :: GetRelationalDatabaseLogStreamsResponse -> TestTree
responseGetRelationalDatabaseLogStreams =
  res
    "GetRelationalDatabaseLogStreamsResponse"
    "fixture/GetRelationalDatabaseLogStreamsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseLogStreams)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    lightsail
    (Proxy :: Proxy GetDomain)

responseGetAutoSnapshots :: GetAutoSnapshotsResponse -> TestTree
responseGetAutoSnapshots =
  res
    "GetAutoSnapshotsResponse"
    "fixture/GetAutoSnapshotsResponse.proto"
    lightsail
    (Proxy :: Proxy GetAutoSnapshots)

responseGetActiveNames :: GetActiveNamesResponse -> TestTree
responseGetActiveNames =
  res
    "GetActiveNamesResponse"
    "fixture/GetActiveNamesResponse.proto"
    lightsail
    (Proxy :: Proxy GetActiveNames)

responseDeleteContactMethod :: DeleteContactMethodResponse -> TestTree
responseDeleteContactMethod =
  res
    "DeleteContactMethodResponse"
    "fixture/DeleteContactMethodResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteContactMethod)

responseCreateDistribution :: CreateDistributionResponse -> TestTree
responseCreateDistribution =
  res
    "CreateDistributionResponse"
    "fixture/CreateDistributionResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDistribution)

responseStopRelationalDatabase :: StopRelationalDatabaseResponse -> TestTree
responseStopRelationalDatabase =
  res
    "StopRelationalDatabaseResponse"
    "fixture/StopRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy StopRelationalDatabase)

responseCreateRelationalDatabaseSnapshot :: CreateRelationalDatabaseSnapshotResponse -> TestTree
responseCreateRelationalDatabaseSnapshot =
  res
    "CreateRelationalDatabaseSnapshotResponse"
    "fixture/CreateRelationalDatabaseSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateRelationalDatabaseSnapshot)

responseDetachCertificateFromDistribution :: DetachCertificateFromDistributionResponse -> TestTree
responseDetachCertificateFromDistribution =
  res
    "DetachCertificateFromDistributionResponse"
    "fixture/DetachCertificateFromDistributionResponse.proto"
    lightsail
    (Proxy :: Proxy DetachCertificateFromDistribution)

responseCreateContainerService :: CreateContainerServiceResponse -> TestTree
responseCreateContainerService =
  res
    "CreateContainerServiceResponse"
    "fixture/CreateContainerServiceResponse.proto"
    lightsail
    (Proxy :: Proxy CreateContainerService)

responseGetInstanceAccessDetails :: GetInstanceAccessDetailsResponse -> TestTree
responseGetInstanceAccessDetails =
  res
    "GetInstanceAccessDetailsResponse"
    "fixture/GetInstanceAccessDetailsResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceAccessDetails)

responseEnableAddOn :: EnableAddOnResponse -> TestTree
responseEnableAddOn =
  res
    "EnableAddOnResponse"
    "fixture/EnableAddOnResponse.proto"
    lightsail
    (Proxy :: Proxy EnableAddOn)

responseStopInstance :: StopInstanceResponse -> TestTree
responseStopInstance =
  res
    "StopInstanceResponse"
    "fixture/StopInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy StopInstance)

responseDetachInstancesFromLoadBalancer :: DetachInstancesFromLoadBalancerResponse -> TestTree
responseDetachInstancesFromLoadBalancer =
  res
    "DetachInstancesFromLoadBalancerResponse"
    "fixture/DetachInstancesFromLoadBalancerResponse.proto"
    lightsail
    (Proxy :: Proxy DetachInstancesFromLoadBalancer)

responseRegisterContainerImage :: RegisterContainerImageResponse -> TestTree
responseRegisterContainerImage =
  res
    "RegisterContainerImageResponse"
    "fixture/RegisterContainerImageResponse.proto"
    lightsail
    (Proxy :: Proxy RegisterContainerImage)

responseCreateCertificate :: CreateCertificateResponse -> TestTree
responseCreateCertificate =
  res
    "CreateCertificateResponse"
    "fixture/CreateCertificateResponse.proto"
    lightsail
    (Proxy :: Proxy CreateCertificate)

responseCreateInstanceSnapshot :: CreateInstanceSnapshotResponse -> TestTree
responseCreateInstanceSnapshot =
  res
    "CreateInstanceSnapshotResponse"
    "fixture/CreateInstanceSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstanceSnapshot)

responseCopySnapshot :: CopySnapshotResponse -> TestTree
responseCopySnapshot =
  res
    "CopySnapshotResponse"
    "fixture/CopySnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CopySnapshot)

responseGetRelationalDatabaseSnapshot :: GetRelationalDatabaseSnapshotResponse -> TestTree
responseGetRelationalDatabaseSnapshot =
  res
    "GetRelationalDatabaseSnapshotResponse"
    "fixture/GetRelationalDatabaseSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseSnapshot)

responseIsVPCPeered :: IsVPCPeeredResponse -> TestTree
responseIsVPCPeered =
  res
    "IsVPCPeeredResponse"
    "fixture/IsVPCPeeredResponse.proto"
    lightsail
    (Proxy :: Proxy IsVPCPeered)

responseGetStaticIPs :: GetStaticIPsResponse -> TestTree
responseGetStaticIPs =
  res
    "GetStaticIPsResponse"
    "fixture/GetStaticIPsResponse.proto"
    lightsail
    (Proxy :: Proxy GetStaticIPs)

responseUnpeerVPC :: UnpeerVPCResponse -> TestTree
responseUnpeerVPC =
  res
    "UnpeerVPCResponse"
    "fixture/UnpeerVPCResponse.proto"
    lightsail
    (Proxy :: Proxy UnpeerVPC)

responseDeleteDisk :: DeleteDiskResponse -> TestTree
responseDeleteDisk =
  res
    "DeleteDiskResponse"
    "fixture/DeleteDiskResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDisk)

responseCreateInstancesFromSnapshot :: CreateInstancesFromSnapshotResponse -> TestTree
responseCreateInstancesFromSnapshot =
  res
    "CreateInstancesFromSnapshotResponse"
    "fixture/CreateInstancesFromSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstancesFromSnapshot)

responseGetCloudFormationStackRecords :: GetCloudFormationStackRecordsResponse -> TestTree
responseGetCloudFormationStackRecords =
  res
    "GetCloudFormationStackRecordsResponse"
    "fixture/GetCloudFormationStackRecordsResponse.proto"
    lightsail
    (Proxy :: Proxy GetCloudFormationStackRecords)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDomain)

responseGetRelationalDatabaseBlueprints :: GetRelationalDatabaseBlueprintsResponse -> TestTree
responseGetRelationalDatabaseBlueprints =
  res
    "GetRelationalDatabaseBlueprintsResponse"
    "fixture/GetRelationalDatabaseBlueprintsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseBlueprints)

responseDeleteCertificate :: DeleteCertificateResponse -> TestTree
responseDeleteCertificate =
  res
    "DeleteCertificateResponse"
    "fixture/DeleteCertificateResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteCertificate)

responseGetDiskSnapshots :: GetDiskSnapshotsResponse -> TestTree
responseGetDiskSnapshots =
  res
    "GetDiskSnapshotsResponse"
    "fixture/GetDiskSnapshotsResponse.proto"
    lightsail
    (Proxy :: Proxy GetDiskSnapshots)

responseGetContainerAPIMetadata :: GetContainerAPIMetadataResponse -> TestTree
responseGetContainerAPIMetadata =
  res
    "GetContainerAPIMetadataResponse"
    "fixture/GetContainerAPIMetadataResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerAPIMetadata)

responsePeerVPC :: PeerVPCResponse -> TestTree
responsePeerVPC =
  res
    "PeerVPCResponse"
    "fixture/PeerVPCResponse.proto"
    lightsail
    (Proxy :: Proxy PeerVPC)

responseGetRelationalDatabaseBundles :: GetRelationalDatabaseBundlesResponse -> TestTree
responseGetRelationalDatabaseBundles =
  res
    "GetRelationalDatabaseBundlesResponse"
    "fixture/GetRelationalDatabaseBundlesResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseBundles)

responseGetLoadBalancers :: GetLoadBalancersResponse -> TestTree
responseGetLoadBalancers =
  res
    "GetLoadBalancersResponse"
    "fixture/GetLoadBalancersResponse.proto"
    lightsail
    (Proxy :: Proxy GetLoadBalancers)

responseRebootRelationalDatabase :: RebootRelationalDatabaseResponse -> TestTree
responseRebootRelationalDatabase =
  res
    "RebootRelationalDatabaseResponse"
    "fixture/RebootRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy RebootRelationalDatabase)

responseAttachLoadBalancerTLSCertificate :: AttachLoadBalancerTLSCertificateResponse -> TestTree
responseAttachLoadBalancerTLSCertificate =
  res
    "AttachLoadBalancerTLSCertificateResponse"
    "fixture/AttachLoadBalancerTLSCertificateResponse.proto"
    lightsail
    (Proxy :: Proxy AttachLoadBalancerTLSCertificate)

responseUpdateLoadBalancerAttribute :: UpdateLoadBalancerAttributeResponse -> TestTree
responseUpdateLoadBalancerAttribute =
  res
    "UpdateLoadBalancerAttributeResponse"
    "fixture/UpdateLoadBalancerAttributeResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateLoadBalancerAttribute)

responseDeleteRelationalDatabase :: DeleteRelationalDatabaseResponse -> TestTree
responseDeleteRelationalDatabase =
  res
    "DeleteRelationalDatabaseResponse"
    "fixture/DeleteRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteRelationalDatabase)

responseGetDiskSnapshot :: GetDiskSnapshotResponse -> TestTree
responseGetDiskSnapshot =
  res
    "GetDiskSnapshotResponse"
    "fixture/GetDiskSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy GetDiskSnapshot)

responseUpdateRelationalDatabase :: UpdateRelationalDatabaseResponse -> TestTree
responseUpdateRelationalDatabase =
  res
    "UpdateRelationalDatabaseResponse"
    "fixture/UpdateRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateRelationalDatabase)

responseGetStaticIP :: GetStaticIPResponse -> TestTree
responseGetStaticIP =
  res
    "GetStaticIPResponse"
    "fixture/GetStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy GetStaticIP)

responseGetRelationalDatabaseMasterUserPassword :: GetRelationalDatabaseMasterUserPasswordResponse -> TestTree
responseGetRelationalDatabaseMasterUserPassword =
  res
    "GetRelationalDatabaseMasterUserPasswordResponse"
    "fixture/GetRelationalDatabaseMasterUserPasswordResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabaseMasterUserPassword)

responseGetBlueprints :: GetBlueprintsResponse -> TestTree
responseGetBlueprints =
  res
    "GetBlueprintsResponse"
    "fixture/GetBlueprintsResponse.proto"
    lightsail
    (Proxy :: Proxy GetBlueprints)

responsePutAlarm :: PutAlarmResponse -> TestTree
responsePutAlarm =
  res
    "PutAlarmResponse"
    "fixture/PutAlarmResponse.proto"
    lightsail
    (Proxy :: Proxy PutAlarm)

responseDeleteAlarm :: DeleteAlarmResponse -> TestTree
responseDeleteAlarm =
  res
    "DeleteAlarmResponse"
    "fixture/DeleteAlarmResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteAlarm)

responseGetInstancePortStates :: GetInstancePortStatesResponse -> TestTree
responseGetInstancePortStates =
  res
    "GetInstancePortStatesResponse"
    "fixture/GetInstancePortStatesResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstancePortStates)

responseDeleteAutoSnapshot :: DeleteAutoSnapshotResponse -> TestTree
responseDeleteAutoSnapshot =
  res
    "DeleteAutoSnapshotResponse"
    "fixture/DeleteAutoSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteAutoSnapshot)

responseCreateRelationalDatabase :: CreateRelationalDatabaseResponse -> TestTree
responseCreateRelationalDatabase =
  res
    "CreateRelationalDatabaseResponse"
    "fixture/CreateRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy CreateRelationalDatabase)

responseSendContactMethodVerification :: SendContactMethodVerificationResponse -> TestTree
responseSendContactMethodVerification =
  res
    "SendContactMethodVerificationResponse"
    "fixture/SendContactMethodVerificationResponse.proto"
    lightsail
    (Proxy :: Proxy SendContactMethodVerification)

responseGetContainerLog :: GetContainerLogResponse -> TestTree
responseGetContainerLog =
  res
    "GetContainerLogResponse"
    "fixture/GetContainerLogResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerLog)

responseCreateDiskSnapshot :: CreateDiskSnapshotResponse -> TestTree
responseCreateDiskSnapshot =
  res
    "CreateDiskSnapshotResponse"
    "fixture/CreateDiskSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDiskSnapshot)

responseDeleteDomainEntry :: DeleteDomainEntryResponse -> TestTree
responseDeleteDomainEntry =
  res
    "DeleteDomainEntryResponse"
    "fixture/DeleteDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDomainEntry)

responseUpdateDomainEntry :: UpdateDomainEntryResponse -> TestTree
responseUpdateDomainEntry =
  res
    "UpdateDomainEntryResponse"
    "fixture/UpdateDomainEntryResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateDomainEntry)

responseGetRegions :: GetRegionsResponse -> TestTree
responseGetRegions =
  res
    "GetRegionsResponse"
    "fixture/GetRegionsResponse.proto"
    lightsail
    (Proxy :: Proxy GetRegions)

responseDeleteDiskSnapshot :: DeleteDiskSnapshotResponse -> TestTree
responseDeleteDiskSnapshot =
  res
    "DeleteDiskSnapshotResponse"
    "fixture/DeleteDiskSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDiskSnapshot)

responseGetLoadBalancerMetricData :: GetLoadBalancerMetricDataResponse -> TestTree
responseGetLoadBalancerMetricData =
  res
    "GetLoadBalancerMetricDataResponse"
    "fixture/GetLoadBalancerMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetLoadBalancerMetricData)

responseGetInstanceState :: GetInstanceStateResponse -> TestTree
responseGetInstanceState =
  res
    "GetInstanceStateResponse"
    "fixture/GetInstanceStateResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceState)

responseGetKeyPairs :: GetKeyPairsResponse -> TestTree
responseGetKeyPairs =
  res
    "GetKeyPairsResponse"
    "fixture/GetKeyPairsResponse.proto"
    lightsail
    (Proxy :: Proxy GetKeyPairs)

responseGetOperations :: GetOperationsResponse -> TestTree
responseGetOperations =
  res
    "GetOperationsResponse"
    "fixture/GetOperationsResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperations)

responseGetDisks :: GetDisksResponse -> TestTree
responseGetDisks =
  res
    "GetDisksResponse"
    "fixture/GetDisksResponse.proto"
    lightsail
    (Proxy :: Proxy GetDisks)

responseGetRelationalDatabase :: GetRelationalDatabaseResponse -> TestTree
responseGetRelationalDatabase =
  res
    "GetRelationalDatabaseResponse"
    "fixture/GetRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy GetRelationalDatabase)

responseAttachInstancesToLoadBalancer :: AttachInstancesToLoadBalancerResponse -> TestTree
responseAttachInstancesToLoadBalancer =
  res
    "AttachInstancesToLoadBalancerResponse"
    "fixture/AttachInstancesToLoadBalancerResponse.proto"
    lightsail
    (Proxy :: Proxy AttachInstancesToLoadBalancer)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    lightsail
    (Proxy :: Proxy TagResource)

responseGetOperation :: GetOperationResponse -> TestTree
responseGetOperation =
  res
    "GetOperationResponse"
    "fixture/GetOperationResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperation)

responseResetDistributionCache :: ResetDistributionCacheResponse -> TestTree
responseResetDistributionCache =
  res
    "ResetDistributionCacheResponse"
    "fixture/ResetDistributionCacheResponse.proto"
    lightsail
    (Proxy :: Proxy ResetDistributionCache)

responseUpdateDistribution :: UpdateDistributionResponse -> TestTree
responseUpdateDistribution =
  res
    "UpdateDistributionResponse"
    "fixture/UpdateDistributionResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateDistribution)

responseDeleteDistribution :: DeleteDistributionResponse -> TestTree
responseDeleteDistribution =
  res
    "DeleteDistributionResponse"
    "fixture/DeleteDistributionResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDistribution)

responseUpdateContainerService :: UpdateContainerServiceResponse -> TestTree
responseUpdateContainerService =
  res
    "UpdateContainerServiceResponse"
    "fixture/UpdateContainerServiceResponse.proto"
    lightsail
    (Proxy :: Proxy UpdateContainerService)

responseDeleteRelationalDatabaseSnapshot :: DeleteRelationalDatabaseSnapshotResponse -> TestTree
responseDeleteRelationalDatabaseSnapshot =
  res
    "DeleteRelationalDatabaseSnapshotResponse"
    "fixture/DeleteRelationalDatabaseSnapshotResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteRelationalDatabaseSnapshot)

responseDeleteContainerService :: DeleteContainerServiceResponse -> TestTree
responseDeleteContainerService =
  res
    "DeleteContainerServiceResponse"
    "fixture/DeleteContainerServiceResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteContainerService)

responseGetInstanceMetricData :: GetInstanceMetricDataResponse -> TestTree
responseGetInstanceMetricData =
  res
    "GetInstanceMetricDataResponse"
    "fixture/GetInstanceMetricDataResponse.proto"
    lightsail
    (Proxy :: Proxy GetInstanceMetricData)

responseGetKeyPair :: GetKeyPairResponse -> TestTree
responseGetKeyPair =
  res
    "GetKeyPairResponse"
    "fixture/GetKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy GetKeyPair)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    lightsail
    (Proxy :: Proxy UntagResource)

responsePutInstancePublicPorts :: PutInstancePublicPortsResponse -> TestTree
responsePutInstancePublicPorts =
  res
    "PutInstancePublicPortsResponse"
    "fixture/PutInstancePublicPortsResponse.proto"
    lightsail
    (Proxy :: Proxy PutInstancePublicPorts)

responseGetDistributionBundles :: GetDistributionBundlesResponse -> TestTree
responseGetDistributionBundles =
  res
    "GetDistributionBundlesResponse"
    "fixture/GetDistributionBundlesResponse.proto"
    lightsail
    (Proxy :: Proxy GetDistributionBundles)

responseGetDisk :: GetDiskResponse -> TestTree
responseGetDisk =
  res
    "GetDiskResponse"
    "fixture/GetDiskResponse.proto"
    lightsail
    (Proxy :: Proxy GetDisk)

responseGetDistributionLatestCacheReset :: GetDistributionLatestCacheResetResponse -> TestTree
responseGetDistributionLatestCacheReset =
  res
    "GetDistributionLatestCacheResetResponse"
    "fixture/GetDistributionLatestCacheResetResponse.proto"
    lightsail
    (Proxy :: Proxy GetDistributionLatestCacheReset)

responseCreateLoadBalancer :: CreateLoadBalancerResponse -> TestTree
responseCreateLoadBalancer =
  res
    "CreateLoadBalancerResponse"
    "fixture/CreateLoadBalancerResponse.proto"
    lightsail
    (Proxy :: Proxy CreateLoadBalancer)

responseGetContainerServiceDeployments :: GetContainerServiceDeploymentsResponse -> TestTree
responseGetContainerServiceDeployments =
  res
    "GetContainerServiceDeploymentsResponse"
    "fixture/GetContainerServiceDeploymentsResponse.proto"
    lightsail
    (Proxy :: Proxy GetContainerServiceDeployments)

responseDeleteKnownHostKeys :: DeleteKnownHostKeysResponse -> TestTree
responseDeleteKnownHostKeys =
  res
    "DeleteKnownHostKeysResponse"
    "fixture/DeleteKnownHostKeysResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteKnownHostKeys)

responseAttachDisk :: AttachDiskResponse -> TestTree
responseAttachDisk =
  res
    "AttachDiskResponse"
    "fixture/AttachDiskResponse.proto"
    lightsail
    (Proxy :: Proxy AttachDisk)

responseDetachStaticIP :: DetachStaticIPResponse -> TestTree
responseDetachStaticIP =
  res
    "DetachStaticIPResponse"
    "fixture/DetachStaticIPResponse.proto"
    lightsail
    (Proxy :: Proxy DetachStaticIP)

responseCreateInstances :: CreateInstancesResponse -> TestTree
responseCreateInstances =
  res
    "CreateInstancesResponse"
    "fixture/CreateInstancesResponse.proto"
    lightsail
    (Proxy :: Proxy CreateInstances)

responseGetAlarms :: GetAlarmsResponse -> TestTree
responseGetAlarms =
  res
    "GetAlarmsResponse"
    "fixture/GetAlarmsResponse.proto"
    lightsail
    (Proxy :: Proxy GetAlarms)

responseOpenInstancePublicPorts :: OpenInstancePublicPortsResponse -> TestTree
responseOpenInstancePublicPorts =
  res
    "OpenInstancePublicPortsResponse"
    "fixture/OpenInstancePublicPortsResponse.proto"
    lightsail
    (Proxy :: Proxy OpenInstancePublicPorts)

responseStartRelationalDatabase :: StartRelationalDatabaseResponse -> TestTree
responseStartRelationalDatabase =
  res
    "StartRelationalDatabaseResponse"
    "fixture/StartRelationalDatabaseResponse.proto"
    lightsail
    (Proxy :: Proxy StartRelationalDatabase)

responseDeleteContainerImage :: DeleteContainerImageResponse -> TestTree
responseDeleteContainerImage =
  res
    "DeleteContainerImageResponse"
    "fixture/DeleteContainerImageResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteContainerImage)

responseGetBundles :: GetBundlesResponse -> TestTree
responseGetBundles =
  res
    "GetBundlesResponse"
    "fixture/GetBundlesResponse.proto"
    lightsail
    (Proxy :: Proxy GetBundles)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    lightsail
    (Proxy :: Proxy DeleteDomain)

responseGetLoadBalancerTLSCertificates :: GetLoadBalancerTLSCertificatesResponse -> TestTree
responseGetLoadBalancerTLSCertificates =
  res
    "GetLoadBalancerTLSCertificatesResponse"
    "fixture/GetLoadBalancerTLSCertificatesResponse.proto"
    lightsail
    (Proxy :: Proxy GetLoadBalancerTLSCertificates)

responseCreateDisk :: CreateDiskResponse -> TestTree
responseCreateDisk =
  res
    "CreateDiskResponse"
    "fixture/CreateDiskResponse.proto"
    lightsail
    (Proxy :: Proxy CreateDisk)

responseGetOperationsForResource :: GetOperationsForResourceResponse -> TestTree
responseGetOperationsForResource =
  res
    "GetOperationsForResourceResponse"
    "fixture/GetOperationsForResourceResponse.proto"
    lightsail
    (Proxy :: Proxy GetOperationsForResource)

responseCreateKeyPair :: CreateKeyPairResponse -> TestTree
responseCreateKeyPair =
  res
    "CreateKeyPairResponse"
    "fixture/CreateKeyPairResponse.proto"
    lightsail
    (Proxy :: Proxy CreateKeyPair)

responseStartInstance :: StartInstanceResponse -> TestTree
responseStartInstance =
  res
    "StartInstanceResponse"
    "fixture/StartInstanceResponse.proto"
    lightsail
    (Proxy :: Proxy StartInstance)
