{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lightsail is the easiest way to get started with Amazon Web
-- Services (AWS) for developers who need to build websites or web
-- applications. It includes everything you need to launch your project
-- quickly - instances (virtual private servers), container services,
-- managed databases, SSD-based block storage, static IP addresses, load
-- balancers, content delivery network (CDN) distributions, DNS management
-- of registered domains, and resource snapshots (backups) - for a low,
-- predictable monthly price.
--
-- You can manage your Lightsail resources using the Lightsail console,
-- Lightsail API, AWS Command Line Interface (AWS CLI), or SDKs. For more
-- information about Lightsail concepts and tasks, see the
-- <http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Lightsail Dev Guide>.
--
-- This API Reference provides detailed information about the actions, data
-- types, parameters, and errors of the Lightsail service. For more
-- information about the supported AWS Regions, endpoints, and service
-- quotas of the Lightsail service, see
-- <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail Endpoints and Quotas>
-- in the /AWS General Reference/.
module Network.AWS.Lightsail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** NotFoundException
    _NotFoundException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** UnauthenticatedException
    _UnauthenticatedException,

    -- ** OperationFailureException
    _OperationFailureException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccountSetupInProgressException
    _AccountSetupInProgressException,

    -- ** ServiceException
    _ServiceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetContainerServices
    GetContainerServices (GetContainerServices'),
    newGetContainerServices,
    GetContainerServicesResponse (GetContainerServicesResponse'),
    newGetContainerServicesResponse,

    -- ** GetStaticIp
    GetStaticIp (GetStaticIp'),
    newGetStaticIp,
    GetStaticIpResponse (GetStaticIpResponse'),
    newGetStaticIpResponse,

    -- ** GetRelationalDatabaseSnapshots (Paginated)
    GetRelationalDatabaseSnapshots (GetRelationalDatabaseSnapshots'),
    newGetRelationalDatabaseSnapshots,
    GetRelationalDatabaseSnapshotsResponse (GetRelationalDatabaseSnapshotsResponse'),
    newGetRelationalDatabaseSnapshotsResponse,

    -- ** GetDistributions
    GetDistributions (GetDistributions'),
    newGetDistributions,
    GetDistributionsResponse (GetDistributionsResponse'),
    newGetDistributionsResponse,

    -- ** GetDiskSnapshot
    GetDiskSnapshot (GetDiskSnapshot'),
    newGetDiskSnapshot,
    GetDiskSnapshotResponse (GetDiskSnapshotResponse'),
    newGetDiskSnapshotResponse,

    -- ** CreateContainerServiceDeployment
    CreateContainerServiceDeployment (CreateContainerServiceDeployment'),
    newCreateContainerServiceDeployment,
    CreateContainerServiceDeploymentResponse (CreateContainerServiceDeploymentResponse'),
    newCreateContainerServiceDeploymentResponse,

    -- ** PeerVpc
    PeerVpc (PeerVpc'),
    newPeerVpc,
    PeerVpcResponse (PeerVpcResponse'),
    newPeerVpcResponse,

    -- ** UpdateLoadBalancerAttribute
    UpdateLoadBalancerAttribute (UpdateLoadBalancerAttribute'),
    newUpdateLoadBalancerAttribute,
    UpdateLoadBalancerAttributeResponse (UpdateLoadBalancerAttributeResponse'),
    newUpdateLoadBalancerAttributeResponse,

    -- ** UpdateDistributionBundle
    UpdateDistributionBundle (UpdateDistributionBundle'),
    newUpdateDistributionBundle,
    UpdateDistributionBundleResponse (UpdateDistributionBundleResponse'),
    newUpdateDistributionBundleResponse,

    -- ** AllocateStaticIp
    AllocateStaticIp (AllocateStaticIp'),
    newAllocateStaticIp,
    AllocateStaticIpResponse (AllocateStaticIpResponse'),
    newAllocateStaticIpResponse,

    -- ** CloseInstancePublicPorts
    CloseInstancePublicPorts (CloseInstancePublicPorts'),
    newCloseInstancePublicPorts,
    CloseInstancePublicPortsResponse (CloseInstancePublicPortsResponse'),
    newCloseInstancePublicPortsResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DisableAddOn
    DisableAddOn (DisableAddOn'),
    newDisableAddOn,
    DisableAddOnResponse (DisableAddOnResponse'),
    newDisableAddOnResponse,

    -- ** GetCloudFormationStackRecords (Paginated)
    GetCloudFormationStackRecords (GetCloudFormationStackRecords'),
    newGetCloudFormationStackRecords,
    GetCloudFormationStackRecordsResponse (GetCloudFormationStackRecordsResponse'),
    newGetCloudFormationStackRecordsResponse,

    -- ** IsVpcPeered
    IsVpcPeered (IsVpcPeered'),
    newIsVpcPeered,
    IsVpcPeeredResponse (IsVpcPeeredResponse'),
    newIsVpcPeeredResponse,

    -- ** GetRelationalDatabaseSnapshot
    GetRelationalDatabaseSnapshot (GetRelationalDatabaseSnapshot'),
    newGetRelationalDatabaseSnapshot,
    GetRelationalDatabaseSnapshotResponse (GetRelationalDatabaseSnapshotResponse'),
    newGetRelationalDatabaseSnapshotResponse,

    -- ** GetRelationalDatabaseBlueprints (Paginated)
    GetRelationalDatabaseBlueprints (GetRelationalDatabaseBlueprints'),
    newGetRelationalDatabaseBlueprints,
    GetRelationalDatabaseBlueprintsResponse (GetRelationalDatabaseBlueprintsResponse'),
    newGetRelationalDatabaseBlueprintsResponse,

    -- ** DeleteInstanceSnapshot
    DeleteInstanceSnapshot (DeleteInstanceSnapshot'),
    newDeleteInstanceSnapshot,
    DeleteInstanceSnapshotResponse (DeleteInstanceSnapshotResponse'),
    newDeleteInstanceSnapshotResponse,

    -- ** UnpeerVpc
    UnpeerVpc (UnpeerVpc'),
    newUnpeerVpc,
    UnpeerVpcResponse (UnpeerVpcResponse'),
    newUnpeerVpcResponse,

    -- ** GetContainerAPIMetadata
    GetContainerAPIMetadata (GetContainerAPIMetadata'),
    newGetContainerAPIMetadata,
    GetContainerAPIMetadataResponse (GetContainerAPIMetadataResponse'),
    newGetContainerAPIMetadataResponse,

    -- ** GetInstances (Paginated)
    GetInstances (GetInstances'),
    newGetInstances,
    GetInstancesResponse (GetInstancesResponse'),
    newGetInstancesResponse,

    -- ** CreateInstancesFromSnapshot
    CreateInstancesFromSnapshot (CreateInstancesFromSnapshot'),
    newCreateInstancesFromSnapshot,
    CreateInstancesFromSnapshotResponse (CreateInstancesFromSnapshotResponse'),
    newCreateInstancesFromSnapshotResponse,

    -- ** StartInstance
    StartInstance (StartInstance'),
    newStartInstance,
    StartInstanceResponse (StartInstanceResponse'),
    newStartInstanceResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** GetInstanceAccessDetails
    GetInstanceAccessDetails (GetInstanceAccessDetails'),
    newGetInstanceAccessDetails,
    GetInstanceAccessDetailsResponse (GetInstanceAccessDetailsResponse'),
    newGetInstanceAccessDetailsResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** CreateInstanceSnapshot
    CreateInstanceSnapshot (CreateInstanceSnapshot'),
    newCreateInstanceSnapshot,
    CreateInstanceSnapshotResponse (CreateInstanceSnapshotResponse'),
    newCreateInstanceSnapshotResponse,

    -- ** StopInstance
    StopInstance (StopInstance'),
    newStopInstance,
    StopInstanceResponse (StopInstanceResponse'),
    newStopInstanceResponse,

    -- ** RegisterContainerImage
    RegisterContainerImage (RegisterContainerImage'),
    newRegisterContainerImage,
    RegisterContainerImageResponse (RegisterContainerImageResponse'),
    newRegisterContainerImageResponse,

    -- ** DetachCertificateFromDistribution
    DetachCertificateFromDistribution (DetachCertificateFromDistribution'),
    newDetachCertificateFromDistribution,
    DetachCertificateFromDistributionResponse (DetachCertificateFromDistributionResponse'),
    newDetachCertificateFromDistributionResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

    -- ** GetContainerServiceDeployments
    GetContainerServiceDeployments (GetContainerServiceDeployments'),
    newGetContainerServiceDeployments,
    GetContainerServiceDeploymentsResponse (GetContainerServiceDeploymentsResponse'),
    newGetContainerServiceDeploymentsResponse,

    -- ** DeleteContactMethod
    DeleteContactMethod (DeleteContactMethod'),
    newDeleteContactMethod,
    DeleteContactMethodResponse (DeleteContactMethodResponse'),
    newDeleteContactMethodResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** DetachStaticIp
    DetachStaticIp (DetachStaticIp'),
    newDetachStaticIp,
    DetachStaticIpResponse (DetachStaticIpResponse'),
    newDetachStaticIpResponse,

    -- ** AttachDisk
    AttachDisk (AttachDisk'),
    newAttachDisk,
    AttachDiskResponse (AttachDiskResponse'),
    newAttachDiskResponse,

    -- ** GetDisk
    GetDisk (GetDisk'),
    newGetDisk,
    GetDiskResponse (GetDiskResponse'),
    newGetDiskResponse,

    -- ** GetRelationalDatabaseLogEvents
    GetRelationalDatabaseLogEvents (GetRelationalDatabaseLogEvents'),
    newGetRelationalDatabaseLogEvents,
    GetRelationalDatabaseLogEventsResponse (GetRelationalDatabaseLogEventsResponse'),
    newGetRelationalDatabaseLogEventsResponse,

    -- ** GetRelationalDatabases (Paginated)
    GetRelationalDatabases (GetRelationalDatabases'),
    newGetRelationalDatabases,
    GetRelationalDatabasesResponse (GetRelationalDatabasesResponse'),
    newGetRelationalDatabasesResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** CreateDiskFromSnapshot
    CreateDiskFromSnapshot (CreateDiskFromSnapshot'),
    newCreateDiskFromSnapshot,
    CreateDiskFromSnapshotResponse (CreateDiskFromSnapshotResponse'),
    newCreateDiskFromSnapshotResponse,

    -- ** GetInstanceMetricData
    GetInstanceMetricData (GetInstanceMetricData'),
    newGetInstanceMetricData,
    GetInstanceMetricDataResponse (GetInstanceMetricDataResponse'),
    newGetInstanceMetricDataResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** ExportSnapshot
    ExportSnapshot (ExportSnapshot'),
    newExportSnapshot,
    ExportSnapshotResponse (ExportSnapshotResponse'),
    newExportSnapshotResponse,

    -- ** CreateRelationalDatabaseFromSnapshot
    CreateRelationalDatabaseFromSnapshot (CreateRelationalDatabaseFromSnapshot'),
    newCreateRelationalDatabaseFromSnapshot,
    CreateRelationalDatabaseFromSnapshotResponse (CreateRelationalDatabaseFromSnapshotResponse'),
    newCreateRelationalDatabaseFromSnapshotResponse,

    -- ** GetOperations (Paginated)
    GetOperations (GetOperations'),
    newGetOperations,
    GetOperationsResponse (GetOperationsResponse'),
    newGetOperationsResponse,

    -- ** GetExportSnapshotRecords (Paginated)
    GetExportSnapshotRecords (GetExportSnapshotRecords'),
    newGetExportSnapshotRecords,
    GetExportSnapshotRecordsResponse (GetExportSnapshotRecordsResponse'),
    newGetExportSnapshotRecordsResponse,

    -- ** GetLoadBalancerMetricData
    GetLoadBalancerMetricData (GetLoadBalancerMetricData'),
    newGetLoadBalancerMetricData,
    GetLoadBalancerMetricDataResponse (GetLoadBalancerMetricDataResponse'),
    newGetLoadBalancerMetricDataResponse,

    -- ** GetInstanceSnapshots (Paginated)
    GetInstanceSnapshots (GetInstanceSnapshots'),
    newGetInstanceSnapshots,
    GetInstanceSnapshotsResponse (GetInstanceSnapshotsResponse'),
    newGetInstanceSnapshotsResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** CreateLoadBalancerTlsCertificate
    CreateLoadBalancerTlsCertificate (CreateLoadBalancerTlsCertificate'),
    newCreateLoadBalancerTlsCertificate,
    CreateLoadBalancerTlsCertificateResponse (CreateLoadBalancerTlsCertificateResponse'),
    newCreateLoadBalancerTlsCertificateResponse,

    -- ** RebootInstance
    RebootInstance (RebootInstance'),
    newRebootInstance,
    RebootInstanceResponse (RebootInstanceResponse'),
    newRebootInstanceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetRelationalDatabase
    GetRelationalDatabase (GetRelationalDatabase'),
    newGetRelationalDatabase,
    GetRelationalDatabaseResponse (GetRelationalDatabaseResponse'),
    newGetRelationalDatabaseResponse,

    -- ** GetKeyPairs (Paginated)
    GetKeyPairs (GetKeyPairs'),
    newGetKeyPairs,
    GetKeyPairsResponse (GetKeyPairsResponse'),
    newGetKeyPairsResponse,

    -- ** AttachInstancesToLoadBalancer
    AttachInstancesToLoadBalancer (AttachInstancesToLoadBalancer'),
    newAttachInstancesToLoadBalancer,
    AttachInstancesToLoadBalancerResponse (AttachInstancesToLoadBalancerResponse'),
    newAttachInstancesToLoadBalancerResponse,

    -- ** GetRegions
    GetRegions (GetRegions'),
    newGetRegions,
    GetRegionsResponse (GetRegionsResponse'),
    newGetRegionsResponse,

    -- ** TestAlarm
    TestAlarm (TestAlarm'),
    newTestAlarm,
    TestAlarmResponse (TestAlarmResponse'),
    newTestAlarmResponse,

    -- ** CreateDiskSnapshot
    CreateDiskSnapshot (CreateDiskSnapshot'),
    newCreateDiskSnapshot,
    CreateDiskSnapshotResponse (CreateDiskSnapshotResponse'),
    newCreateDiskSnapshotResponse,

    -- ** SetIpAddressType
    SetIpAddressType (SetIpAddressType'),
    newSetIpAddressType,
    SetIpAddressTypeResponse (SetIpAddressTypeResponse'),
    newSetIpAddressTypeResponse,

    -- ** DeleteAlarm
    DeleteAlarm (DeleteAlarm'),
    newDeleteAlarm,
    DeleteAlarmResponse (DeleteAlarmResponse'),
    newDeleteAlarmResponse,

    -- ** SendContactMethodVerification
    SendContactMethodVerification (SendContactMethodVerification'),
    newSendContactMethodVerification,
    SendContactMethodVerificationResponse (SendContactMethodVerificationResponse'),
    newSendContactMethodVerificationResponse,

    -- ** GetRelationalDatabaseMasterUserPassword
    GetRelationalDatabaseMasterUserPassword (GetRelationalDatabaseMasterUserPassword'),
    newGetRelationalDatabaseMasterUserPassword,
    GetRelationalDatabaseMasterUserPasswordResponse (GetRelationalDatabaseMasterUserPasswordResponse'),
    newGetRelationalDatabaseMasterUserPasswordResponse,

    -- ** GetBlueprints (Paginated)
    GetBlueprints (GetBlueprints'),
    newGetBlueprints,
    GetBlueprintsResponse (GetBlueprintsResponse'),
    newGetBlueprintsResponse,

    -- ** DetachDisk
    DetachDisk (DetachDisk'),
    newDetachDisk,
    DetachDiskResponse (DetachDiskResponse'),
    newDetachDiskResponse,

    -- ** GetInstancePortStates
    GetInstancePortStates (GetInstancePortStates'),
    newGetInstancePortStates,
    GetInstancePortStatesResponse (GetInstancePortStatesResponse'),
    newGetInstancePortStatesResponse,

    -- ** AttachStaticIp
    AttachStaticIp (AttachStaticIp'),
    newAttachStaticIp,
    AttachStaticIpResponse (AttachStaticIpResponse'),
    newAttachStaticIpResponse,

    -- ** DownloadDefaultKeyPair
    DownloadDefaultKeyPair (DownloadDefaultKeyPair'),
    newDownloadDefaultKeyPair,
    DownloadDefaultKeyPairResponse (DownloadDefaultKeyPairResponse'),
    newDownloadDefaultKeyPairResponse,

    -- ** GetLoadBalancers (Paginated)
    GetLoadBalancers (GetLoadBalancers'),
    newGetLoadBalancers,
    GetLoadBalancersResponse (GetLoadBalancersResponse'),
    newGetLoadBalancersResponse,

    -- ** UpdateRelationalDatabase
    UpdateRelationalDatabase (UpdateRelationalDatabase'),
    newUpdateRelationalDatabase,
    UpdateRelationalDatabaseResponse (UpdateRelationalDatabaseResponse'),
    newUpdateRelationalDatabaseResponse,

    -- ** GetRelationalDatabaseBundles (Paginated)
    GetRelationalDatabaseBundles (GetRelationalDatabaseBundles'),
    newGetRelationalDatabaseBundles,
    GetRelationalDatabaseBundlesResponse (GetRelationalDatabaseBundlesResponse'),
    newGetRelationalDatabaseBundlesResponse,

    -- ** AttachLoadBalancerTlsCertificate
    AttachLoadBalancerTlsCertificate (AttachLoadBalancerTlsCertificate'),
    newAttachLoadBalancerTlsCertificate,
    AttachLoadBalancerTlsCertificateResponse (AttachLoadBalancerTlsCertificateResponse'),
    newAttachLoadBalancerTlsCertificateResponse,

    -- ** AttachCertificateToDistribution
    AttachCertificateToDistribution (AttachCertificateToDistribution'),
    newAttachCertificateToDistribution,
    AttachCertificateToDistributionResponse (AttachCertificateToDistributionResponse'),
    newAttachCertificateToDistributionResponse,

    -- ** DeleteRelationalDatabase
    DeleteRelationalDatabase (DeleteRelationalDatabase'),
    newDeleteRelationalDatabase,
    DeleteRelationalDatabaseResponse (DeleteRelationalDatabaseResponse'),
    newDeleteRelationalDatabaseResponse,

    -- ** GetInstance
    GetInstance (GetInstance'),
    newGetInstance,
    GetInstanceResponse (GetInstanceResponse'),
    newGetInstanceResponse,

    -- ** RebootRelationalDatabase
    RebootRelationalDatabase (RebootRelationalDatabase'),
    newRebootRelationalDatabase,
    RebootRelationalDatabaseResponse (RebootRelationalDatabaseResponse'),
    newRebootRelationalDatabaseResponse,

    -- ** GetRelationalDatabaseEvents (Paginated)
    GetRelationalDatabaseEvents (GetRelationalDatabaseEvents'),
    newGetRelationalDatabaseEvents,
    GetRelationalDatabaseEventsResponse (GetRelationalDatabaseEventsResponse'),
    newGetRelationalDatabaseEventsResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** GetStaticIps (Paginated)
    GetStaticIps (GetStaticIps'),
    newGetStaticIps,
    GetStaticIpsResponse (GetStaticIpsResponse'),
    newGetStaticIpsResponse,

    -- ** DeleteDisk
    DeleteDisk (DeleteDisk'),
    newDeleteDisk,
    DeleteDiskResponse (DeleteDiskResponse'),
    newDeleteDiskResponse,

    -- ** GetRelationalDatabaseMetricData
    GetRelationalDatabaseMetricData (GetRelationalDatabaseMetricData'),
    newGetRelationalDatabaseMetricData,
    GetRelationalDatabaseMetricDataResponse (GetRelationalDatabaseMetricDataResponse'),
    newGetRelationalDatabaseMetricDataResponse,

    -- ** GetDiskSnapshots (Paginated)
    GetDiskSnapshots (GetDiskSnapshots'),
    newGetDiskSnapshots,
    GetDiskSnapshotsResponse (GetDiskSnapshotsResponse'),
    newGetDiskSnapshotsResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** GetLoadBalancer
    GetLoadBalancer (GetLoadBalancer'),
    newGetLoadBalancer,
    GetLoadBalancerResponse (GetLoadBalancerResponse'),
    newGetLoadBalancerResponse,

    -- ** GetBundles (Paginated)
    GetBundles (GetBundles'),
    newGetBundles,
    GetBundlesResponse (GetBundlesResponse'),
    newGetBundlesResponse,

    -- ** CreateCertificate
    CreateCertificate (CreateCertificate'),
    newCreateCertificate,
    CreateCertificateResponse (CreateCertificateResponse'),
    newCreateCertificateResponse,

    -- ** DetachInstancesFromLoadBalancer
    DetachInstancesFromLoadBalancer (DetachInstancesFromLoadBalancer'),
    newDetachInstancesFromLoadBalancer,
    DetachInstancesFromLoadBalancerResponse (DetachInstancesFromLoadBalancerResponse'),
    newDetachInstancesFromLoadBalancerResponse,

    -- ** GetLoadBalancerTlsCertificates
    GetLoadBalancerTlsCertificates (GetLoadBalancerTlsCertificates'),
    newGetLoadBalancerTlsCertificates,
    GetLoadBalancerTlsCertificatesResponse (GetLoadBalancerTlsCertificatesResponse'),
    newGetLoadBalancerTlsCertificatesResponse,

    -- ** DeleteContainerImage
    DeleteContainerImage (DeleteContainerImage'),
    newDeleteContainerImage,
    DeleteContainerImageResponse (DeleteContainerImageResponse'),
    newDeleteContainerImageResponse,

    -- ** GetOperationsForResource
    GetOperationsForResource (GetOperationsForResource'),
    newGetOperationsForResource,
    GetOperationsForResourceResponse (GetOperationsForResourceResponse'),
    newGetOperationsForResourceResponse,

    -- ** CreateDisk
    CreateDisk (CreateDisk'),
    newCreateDisk,
    CreateDiskResponse (CreateDiskResponse'),
    newCreateDiskResponse,

    -- ** EnableAddOn
    EnableAddOn (EnableAddOn'),
    newEnableAddOn,
    EnableAddOnResponse (EnableAddOnResponse'),
    newEnableAddOnResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** StartRelationalDatabase
    StartRelationalDatabase (StartRelationalDatabase'),
    newStartRelationalDatabase,
    StartRelationalDatabaseResponse (StartRelationalDatabaseResponse'),
    newStartRelationalDatabaseResponse,

    -- ** CreateRelationalDatabaseSnapshot
    CreateRelationalDatabaseSnapshot (CreateRelationalDatabaseSnapshot'),
    newCreateRelationalDatabaseSnapshot,
    CreateRelationalDatabaseSnapshotResponse (CreateRelationalDatabaseSnapshotResponse'),
    newCreateRelationalDatabaseSnapshotResponse,

    -- ** GetAlarms
    GetAlarms (GetAlarms'),
    newGetAlarms,
    GetAlarmsResponse (GetAlarmsResponse'),
    newGetAlarmsResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

    -- ** CreateInstances
    CreateInstances (CreateInstances'),
    newCreateInstances,
    CreateInstancesResponse (CreateInstancesResponse'),
    newCreateInstancesResponse,

    -- ** CreateContainerService
    CreateContainerService (CreateContainerService'),
    newCreateContainerService,
    CreateContainerServiceResponse (CreateContainerServiceResponse'),
    newCreateContainerServiceResponse,

    -- ** GetDistributionLatestCacheReset
    GetDistributionLatestCacheReset (GetDistributionLatestCacheReset'),
    newGetDistributionLatestCacheReset,
    GetDistributionLatestCacheResetResponse (GetDistributionLatestCacheResetResponse'),
    newGetDistributionLatestCacheResetResponse,

    -- ** StopRelationalDatabase
    StopRelationalDatabase (StopRelationalDatabase'),
    newStopRelationalDatabase,
    StopRelationalDatabaseResponse (StopRelationalDatabaseResponse'),
    newStopRelationalDatabaseResponse,

    -- ** DeleteKnownHostKeys
    DeleteKnownHostKeys (DeleteKnownHostKeys'),
    newDeleteKnownHostKeys,
    DeleteKnownHostKeysResponse (DeleteKnownHostKeysResponse'),
    newDeleteKnownHostKeysResponse,

    -- ** OpenInstancePublicPorts
    OpenInstancePublicPorts (OpenInstancePublicPorts'),
    newOpenInstancePublicPorts,
    OpenInstancePublicPortsResponse (OpenInstancePublicPortsResponse'),
    newOpenInstancePublicPortsResponse,

    -- ** GetActiveNames (Paginated)
    GetActiveNames (GetActiveNames'),
    newGetActiveNames,
    GetActiveNamesResponse (GetActiveNamesResponse'),
    newGetActiveNamesResponse,

    -- ** GetAutoSnapshots
    GetAutoSnapshots (GetAutoSnapshots'),
    newGetAutoSnapshots,
    GetAutoSnapshotsResponse (GetAutoSnapshotsResponse'),
    newGetAutoSnapshotsResponse,

    -- ** GetRelationalDatabaseLogStreams
    GetRelationalDatabaseLogStreams (GetRelationalDatabaseLogStreams'),
    newGetRelationalDatabaseLogStreams,
    GetRelationalDatabaseLogStreamsResponse (GetRelationalDatabaseLogStreamsResponse'),
    newGetRelationalDatabaseLogStreamsResponse,

    -- ** GetDistributionBundles
    GetDistributionBundles (GetDistributionBundles'),
    newGetDistributionBundles,
    GetDistributionBundlesResponse (GetDistributionBundlesResponse'),
    newGetDistributionBundlesResponse,

    -- ** GetOperation
    GetOperation (GetOperation'),
    newGetOperation,
    GetOperationResponse (GetOperationResponse'),
    newGetOperationResponse,

    -- ** DeleteRelationalDatabaseSnapshot
    DeleteRelationalDatabaseSnapshot (DeleteRelationalDatabaseSnapshot'),
    newDeleteRelationalDatabaseSnapshot,
    DeleteRelationalDatabaseSnapshotResponse (DeleteRelationalDatabaseSnapshotResponse'),
    newDeleteRelationalDatabaseSnapshotResponse,

    -- ** GetInstanceSnapshot
    GetInstanceSnapshot (GetInstanceSnapshot'),
    newGetInstanceSnapshot,
    GetInstanceSnapshotResponse (GetInstanceSnapshotResponse'),
    newGetInstanceSnapshotResponse,

    -- ** DeleteContainerService
    DeleteContainerService (DeleteContainerService'),
    newDeleteContainerService,
    DeleteContainerServiceResponse (DeleteContainerServiceResponse'),
    newDeleteContainerServiceResponse,

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** PutInstancePublicPorts
    PutInstancePublicPorts (PutInstancePublicPorts'),
    newPutInstancePublicPorts,
    PutInstancePublicPortsResponse (PutInstancePublicPortsResponse'),
    newPutInstancePublicPortsResponse,

    -- ** ResetDistributionCache
    ResetDistributionCache (ResetDistributionCache'),
    newResetDistributionCache,
    ResetDistributionCacheResponse (ResetDistributionCacheResponse'),
    newResetDistributionCacheResponse,

    -- ** CreateContactMethod
    CreateContactMethod (CreateContactMethod'),
    newCreateContactMethod,
    CreateContactMethodResponse (CreateContactMethodResponse'),
    newCreateContactMethodResponse,

    -- ** DeleteDistribution
    DeleteDistribution (DeleteDistribution'),
    newDeleteDistribution,
    DeleteDistributionResponse (DeleteDistributionResponse'),
    newDeleteDistributionResponse,

    -- ** UpdateContainerService
    UpdateContainerService (UpdateContainerService'),
    newUpdateContainerService,
    UpdateContainerServiceResponse (UpdateContainerServiceResponse'),
    newUpdateContainerServiceResponse,

    -- ** GetKeyPair
    GetKeyPair (GetKeyPair'),
    newGetKeyPair,
    GetKeyPairResponse (GetKeyPairResponse'),
    newGetKeyPairResponse,

    -- ** CreateCloudFormationStack
    CreateCloudFormationStack (CreateCloudFormationStack'),
    newCreateCloudFormationStack,
    CreateCloudFormationStackResponse (CreateCloudFormationStackResponse'),
    newCreateCloudFormationStackResponse,

    -- ** CreateDomainEntry
    CreateDomainEntry (CreateDomainEntry'),
    newCreateDomainEntry,
    CreateDomainEntryResponse (CreateDomainEntryResponse'),
    newCreateDomainEntryResponse,

    -- ** GetInstanceState
    GetInstanceState (GetInstanceState'),
    newGetInstanceState,
    GetInstanceStateResponse (GetInstanceStateResponse'),
    newGetInstanceStateResponse,

    -- ** GetDistributionMetricData
    GetDistributionMetricData (GetDistributionMetricData'),
    newGetDistributionMetricData,
    GetDistributionMetricDataResponse (GetDistributionMetricDataResponse'),
    newGetDistributionMetricDataResponse,

    -- ** GetDisks (Paginated)
    GetDisks (GetDisks'),
    newGetDisks,
    GetDisksResponse (GetDisksResponse'),
    newGetDisksResponse,

    -- ** GetContainerServiceMetricData
    GetContainerServiceMetricData (GetContainerServiceMetricData'),
    newGetContainerServiceMetricData,
    GetContainerServiceMetricDataResponse (GetContainerServiceMetricDataResponse'),
    newGetContainerServiceMetricDataResponse,

    -- ** CreateContainerServiceRegistryLogin
    CreateContainerServiceRegistryLogin (CreateContainerServiceRegistryLogin'),
    newCreateContainerServiceRegistryLogin,
    CreateContainerServiceRegistryLoginResponse (CreateContainerServiceRegistryLoginResponse'),
    newCreateContainerServiceRegistryLoginResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** GetContainerServicePowers
    GetContainerServicePowers (GetContainerServicePowers'),
    newGetContainerServicePowers,
    GetContainerServicePowersResponse (GetContainerServicePowersResponse'),
    newGetContainerServicePowersResponse,

    -- ** DeleteDiskSnapshot
    DeleteDiskSnapshot (DeleteDiskSnapshot'),
    newDeleteDiskSnapshot,
    DeleteDiskSnapshotResponse (DeleteDiskSnapshotResponse'),
    newDeleteDiskSnapshotResponse,

    -- ** GetCertificates
    GetCertificates (GetCertificates'),
    newGetCertificates,
    GetCertificatesResponse (GetCertificatesResponse'),
    newGetCertificatesResponse,

    -- ** ReleaseStaticIp
    ReleaseStaticIp (ReleaseStaticIp'),
    newReleaseStaticIp,
    ReleaseStaticIpResponse (ReleaseStaticIpResponse'),
    newReleaseStaticIpResponse,

    -- ** UpdateRelationalDatabaseParameters
    UpdateRelationalDatabaseParameters (UpdateRelationalDatabaseParameters'),
    newUpdateRelationalDatabaseParameters,
    UpdateRelationalDatabaseParametersResponse (UpdateRelationalDatabaseParametersResponse'),
    newUpdateRelationalDatabaseParametersResponse,

    -- ** DeleteLoadBalancerTlsCertificate
    DeleteLoadBalancerTlsCertificate (DeleteLoadBalancerTlsCertificate'),
    newDeleteLoadBalancerTlsCertificate,
    DeleteLoadBalancerTlsCertificateResponse (DeleteLoadBalancerTlsCertificateResponse'),
    newDeleteLoadBalancerTlsCertificateResponse,

    -- ** UpdateDomainEntry
    UpdateDomainEntry (UpdateDomainEntry'),
    newUpdateDomainEntry,
    UpdateDomainEntryResponse (UpdateDomainEntryResponse'),
    newUpdateDomainEntryResponse,

    -- ** GetContainerLog
    GetContainerLog (GetContainerLog'),
    newGetContainerLog,
    GetContainerLogResponse (GetContainerLogResponse'),
    newGetContainerLogResponse,

    -- ** DeleteDomainEntry
    DeleteDomainEntry (DeleteDomainEntry'),
    newDeleteDomainEntry,
    DeleteDomainEntryResponse (DeleteDomainEntryResponse'),
    newDeleteDomainEntryResponse,

    -- ** GetContainerImages
    GetContainerImages (GetContainerImages'),
    newGetContainerImages,
    GetContainerImagesResponse (GetContainerImagesResponse'),
    newGetContainerImagesResponse,

    -- ** GetDomains (Paginated)
    GetDomains (GetDomains'),
    newGetDomains,
    GetDomainsResponse (GetDomainsResponse'),
    newGetDomainsResponse,

    -- ** PutAlarm
    PutAlarm (PutAlarm'),
    newPutAlarm,
    PutAlarmResponse (PutAlarmResponse'),
    newPutAlarmResponse,

    -- ** DeleteAutoSnapshot
    DeleteAutoSnapshot (DeleteAutoSnapshot'),
    newDeleteAutoSnapshot,
    DeleteAutoSnapshotResponse (DeleteAutoSnapshotResponse'),
    newDeleteAutoSnapshotResponse,

    -- ** GetContactMethods
    GetContactMethods (GetContactMethods'),
    newGetContactMethods,
    GetContactMethodsResponse (GetContactMethodsResponse'),
    newGetContactMethodsResponse,

    -- ** GetRelationalDatabaseParameters (Paginated)
    GetRelationalDatabaseParameters (GetRelationalDatabaseParameters'),
    newGetRelationalDatabaseParameters,
    GetRelationalDatabaseParametersResponse (GetRelationalDatabaseParametersResponse'),
    newGetRelationalDatabaseParametersResponse,

    -- ** CreateRelationalDatabase
    CreateRelationalDatabase (CreateRelationalDatabase'),
    newCreateRelationalDatabase,
    CreateRelationalDatabaseResponse (CreateRelationalDatabaseResponse'),
    newCreateRelationalDatabaseResponse,

    -- * Types

    -- ** AccessDirection
    AccessDirection (..),

    -- ** AddOnType
    AddOnType (..),

    -- ** AlarmState
    AlarmState (..),

    -- ** AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- ** BehaviorEnum
    BehaviorEnum (..),

    -- ** BlueprintType
    BlueprintType (..),

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** CloudFormationStackRecordSourceType
    CloudFormationStackRecordSourceType (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** ContactMethodStatus
    ContactMethodStatus (..),

    -- ** ContactMethodVerificationProtocol
    ContactMethodVerificationProtocol (..),

    -- ** ContactProtocol
    ContactProtocol (..),

    -- ** ContainerServiceDeploymentState
    ContainerServiceDeploymentState (..),

    -- ** ContainerServiceMetricName
    ContainerServiceMetricName (..),

    -- ** ContainerServicePowerName
    ContainerServicePowerName (..),

    -- ** ContainerServiceProtocol
    ContainerServiceProtocol (..),

    -- ** ContainerServiceState
    ContainerServiceState (..),

    -- ** DiskSnapshotState
    DiskSnapshotState (..),

    -- ** DiskState
    DiskState (..),

    -- ** DistributionMetricName
    DistributionMetricName (..),

    -- ** ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- ** ForwardValues
    ForwardValues (..),

    -- ** HeaderEnum
    HeaderEnum (..),

    -- ** InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- ** InstanceHealthReason
    InstanceHealthReason (..),

    -- ** InstanceHealthState
    InstanceHealthState (..),

    -- ** InstanceMetricName
    InstanceMetricName (..),

    -- ** InstancePlatform
    InstancePlatform (..),

    -- ** InstanceSnapshotState
    InstanceSnapshotState (..),

    -- ** IpAddressType
    IpAddressType (..),

    -- ** LoadBalancerAttributeName
    LoadBalancerAttributeName (..),

    -- ** LoadBalancerMetricName
    LoadBalancerMetricName (..),

    -- ** LoadBalancerProtocol
    LoadBalancerProtocol (..),

    -- ** LoadBalancerState
    LoadBalancerState (..),

    -- ** LoadBalancerTlsCertificateDomainStatus
    LoadBalancerTlsCertificateDomainStatus (..),

    -- ** LoadBalancerTlsCertificateFailureReason
    LoadBalancerTlsCertificateFailureReason (..),

    -- ** LoadBalancerTlsCertificateRenewalStatus
    LoadBalancerTlsCertificateRenewalStatus (..),

    -- ** LoadBalancerTlsCertificateRevocationReason
    LoadBalancerTlsCertificateRevocationReason (..),

    -- ** LoadBalancerTlsCertificateStatus
    LoadBalancerTlsCertificateStatus (..),

    -- ** MetricName
    MetricName (..),

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** MetricUnit
    MetricUnit (..),

    -- ** NetworkProtocol
    NetworkProtocol (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** OperationType
    OperationType (..),

    -- ** OriginProtocolPolicyEnum
    OriginProtocolPolicyEnum (..),

    -- ** PortAccessType
    PortAccessType (..),

    -- ** PortInfoSourceType
    PortInfoSourceType (..),

    -- ** PortState
    PortState (..),

    -- ** RecordState
    RecordState (..),

    -- ** RegionName
    RegionName (..),

    -- ** RelationalDatabaseEngine
    RelationalDatabaseEngine (..),

    -- ** RelationalDatabaseMetricName
    RelationalDatabaseMetricName (..),

    -- ** RelationalDatabasePasswordVersion
    RelationalDatabasePasswordVersion (..),

    -- ** RenewalStatus
    RenewalStatus (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** TreatMissingData
    TreatMissingData (..),

    -- ** AddOn
    AddOn (AddOn'),
    newAddOn,

    -- ** AddOnRequest
    AddOnRequest (AddOnRequest'),
    newAddOnRequest,

    -- ** Alarm
    Alarm (Alarm'),
    newAlarm,

    -- ** AttachedDisk
    AttachedDisk (AttachedDisk'),
    newAttachedDisk,

    -- ** AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest (AutoSnapshotAddOnRequest'),
    newAutoSnapshotAddOnRequest,

    -- ** AutoSnapshotDetails
    AutoSnapshotDetails (AutoSnapshotDetails'),
    newAutoSnapshotDetails,

    -- ** AvailabilityZone
    AvailabilityZone (AvailabilityZone'),
    newAvailabilityZone,

    -- ** Blueprint
    Blueprint (Blueprint'),
    newBlueprint,

    -- ** Bundle
    Bundle (Bundle'),
    newBundle,

    -- ** CacheBehavior
    CacheBehavior (CacheBehavior'),
    newCacheBehavior,

    -- ** CacheBehaviorPerPath
    CacheBehaviorPerPath (CacheBehaviorPerPath'),
    newCacheBehaviorPerPath,

    -- ** CacheSettings
    CacheSettings (CacheSettings'),
    newCacheSettings,

    -- ** Certificate
    Certificate (Certificate'),
    newCertificate,

    -- ** CertificateSummary
    CertificateSummary (CertificateSummary'),
    newCertificateSummary,

    -- ** CloudFormationStackRecord
    CloudFormationStackRecord (CloudFormationStackRecord'),
    newCloudFormationStackRecord,

    -- ** CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (CloudFormationStackRecordSourceInfo'),
    newCloudFormationStackRecordSourceInfo,

    -- ** ContactMethod
    ContactMethod (ContactMethod'),
    newContactMethod,

    -- ** Container
    Container (Container'),
    newContainer,

    -- ** ContainerImage
    ContainerImage (ContainerImage'),
    newContainerImage,

    -- ** ContainerService
    ContainerService (ContainerService'),
    newContainerService,

    -- ** ContainerServiceDeployment
    ContainerServiceDeployment (ContainerServiceDeployment'),
    newContainerServiceDeployment,

    -- ** ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (ContainerServiceDeploymentRequest'),
    newContainerServiceDeploymentRequest,

    -- ** ContainerServiceEndpoint
    ContainerServiceEndpoint (ContainerServiceEndpoint'),
    newContainerServiceEndpoint,

    -- ** ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (ContainerServiceHealthCheckConfig'),
    newContainerServiceHealthCheckConfig,

    -- ** ContainerServiceLogEvent
    ContainerServiceLogEvent (ContainerServiceLogEvent'),
    newContainerServiceLogEvent,

    -- ** ContainerServicePower
    ContainerServicePower (ContainerServicePower'),
    newContainerServicePower,

    -- ** ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (ContainerServiceRegistryLogin'),
    newContainerServiceRegistryLogin,

    -- ** CookieObject
    CookieObject (CookieObject'),
    newCookieObject,

    -- ** DestinationInfo
    DestinationInfo (DestinationInfo'),
    newDestinationInfo,

    -- ** Disk
    Disk (Disk'),
    newDisk,

    -- ** DiskInfo
    DiskInfo (DiskInfo'),
    newDiskInfo,

    -- ** DiskMap
    DiskMap (DiskMap'),
    newDiskMap,

    -- ** DiskSnapshot
    DiskSnapshot (DiskSnapshot'),
    newDiskSnapshot,

    -- ** DiskSnapshotInfo
    DiskSnapshotInfo (DiskSnapshotInfo'),
    newDiskSnapshotInfo,

    -- ** DistributionBundle
    DistributionBundle (DistributionBundle'),
    newDistributionBundle,

    -- ** Domain
    Domain (Domain'),
    newDomain,

    -- ** DomainEntry
    DomainEntry (DomainEntry'),
    newDomainEntry,

    -- ** DomainValidationRecord
    DomainValidationRecord (DomainValidationRecord'),
    newDomainValidationRecord,

    -- ** EndpointRequest
    EndpointRequest (EndpointRequest'),
    newEndpointRequest,

    -- ** ExportSnapshotRecord
    ExportSnapshotRecord (ExportSnapshotRecord'),
    newExportSnapshotRecord,

    -- ** ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (ExportSnapshotRecordSourceInfo'),
    newExportSnapshotRecordSourceInfo,

    -- ** HeaderObject
    HeaderObject (HeaderObject'),
    newHeaderObject,

    -- ** HostKeyAttributes
    HostKeyAttributes (HostKeyAttributes'),
    newHostKeyAttributes,

    -- ** InputOrigin
    InputOrigin (InputOrigin'),
    newInputOrigin,

    -- ** Instance
    Instance (Instance'),
    newInstance,

    -- ** InstanceAccessDetails
    InstanceAccessDetails (InstanceAccessDetails'),
    newInstanceAccessDetails,

    -- ** InstanceEntry
    InstanceEntry (InstanceEntry'),
    newInstanceEntry,

    -- ** InstanceHardware
    InstanceHardware (InstanceHardware'),
    newInstanceHardware,

    -- ** InstanceHealthSummary
    InstanceHealthSummary (InstanceHealthSummary'),
    newInstanceHealthSummary,

    -- ** InstanceNetworking
    InstanceNetworking (InstanceNetworking'),
    newInstanceNetworking,

    -- ** InstancePortInfo
    InstancePortInfo (InstancePortInfo'),
    newInstancePortInfo,

    -- ** InstancePortState
    InstancePortState (InstancePortState'),
    newInstancePortState,

    -- ** InstanceSnapshot
    InstanceSnapshot (InstanceSnapshot'),
    newInstanceSnapshot,

    -- ** InstanceSnapshotInfo
    InstanceSnapshotInfo (InstanceSnapshotInfo'),
    newInstanceSnapshotInfo,

    -- ** InstanceState
    InstanceState (InstanceState'),
    newInstanceState,

    -- ** KeyPair
    KeyPair (KeyPair'),
    newKeyPair,

    -- ** LightsailDistribution
    LightsailDistribution (LightsailDistribution'),
    newLightsailDistribution,

    -- ** LoadBalancer
    LoadBalancer (LoadBalancer'),
    newLoadBalancer,

    -- ** LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (LoadBalancerTlsCertificate'),
    newLoadBalancerTlsCertificate,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (LoadBalancerTlsCertificateDomainValidationOption'),
    newLoadBalancerTlsCertificateDomainValidationOption,

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (LoadBalancerTlsCertificateDomainValidationRecord'),
    newLoadBalancerTlsCertificateDomainValidationRecord,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    LoadBalancerTlsCertificateRenewalSummary (LoadBalancerTlsCertificateRenewalSummary'),
    newLoadBalancerTlsCertificateRenewalSummary,

    -- ** LoadBalancerTlsCertificateSummary
    LoadBalancerTlsCertificateSummary (LoadBalancerTlsCertificateSummary'),
    newLoadBalancerTlsCertificateSummary,

    -- ** LogEvent
    LogEvent (LogEvent'),
    newLogEvent,

    -- ** MetricDatapoint
    MetricDatapoint (MetricDatapoint'),
    newMetricDatapoint,

    -- ** MonitoredResourceInfo
    MonitoredResourceInfo (MonitoredResourceInfo'),
    newMonitoredResourceInfo,

    -- ** MonthlyTransfer
    MonthlyTransfer (MonthlyTransfer'),
    newMonthlyTransfer,

    -- ** Operation
    Operation (Operation'),
    newOperation,

    -- ** Origin
    Origin (Origin'),
    newOrigin,

    -- ** PasswordData
    PasswordData (PasswordData'),
    newPasswordData,

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (PendingMaintenanceAction'),
    newPendingMaintenanceAction,

    -- ** PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (PendingModifiedRelationalDatabaseValues'),
    newPendingModifiedRelationalDatabaseValues,

    -- ** PortInfo
    PortInfo (PortInfo'),
    newPortInfo,

    -- ** QueryStringObject
    QueryStringObject (QueryStringObject'),
    newQueryStringObject,

    -- ** RegionInfo
    RegionInfo (RegionInfo'),
    newRegionInfo,

    -- ** RelationalDatabase
    RelationalDatabase (RelationalDatabase'),
    newRelationalDatabase,

    -- ** RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (RelationalDatabaseBlueprint'),
    newRelationalDatabaseBlueprint,

    -- ** RelationalDatabaseBundle
    RelationalDatabaseBundle (RelationalDatabaseBundle'),
    newRelationalDatabaseBundle,

    -- ** RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (RelationalDatabaseEndpoint'),
    newRelationalDatabaseEndpoint,

    -- ** RelationalDatabaseEvent
    RelationalDatabaseEvent (RelationalDatabaseEvent'),
    newRelationalDatabaseEvent,

    -- ** RelationalDatabaseHardware
    RelationalDatabaseHardware (RelationalDatabaseHardware'),
    newRelationalDatabaseHardware,

    -- ** RelationalDatabaseParameter
    RelationalDatabaseParameter (RelationalDatabaseParameter'),
    newRelationalDatabaseParameter,

    -- ** RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (RelationalDatabaseSnapshot'),
    newRelationalDatabaseSnapshot,

    -- ** RenewalSummary
    RenewalSummary (RenewalSummary'),
    newRenewalSummary,

    -- ** ResourceLocation
    ResourceLocation (ResourceLocation'),
    newResourceLocation,

    -- ** ResourceRecord
    ResourceRecord (ResourceRecord'),
    newResourceRecord,

    -- ** StaticIp
    StaticIp (StaticIp'),
    newStaticIp,

    -- ** Tag
    Tag (Tag'),
    newTag,
  )
where

import Network.AWS.Lightsail.AllocateStaticIp
import Network.AWS.Lightsail.AttachCertificateToDistribution
import Network.AWS.Lightsail.AttachDisk
import Network.AWS.Lightsail.AttachInstancesToLoadBalancer
import Network.AWS.Lightsail.AttachLoadBalancerTlsCertificate
import Network.AWS.Lightsail.AttachStaticIp
import Network.AWS.Lightsail.CloseInstancePublicPorts
import Network.AWS.Lightsail.CopySnapshot
import Network.AWS.Lightsail.CreateCertificate
import Network.AWS.Lightsail.CreateCloudFormationStack
import Network.AWS.Lightsail.CreateContactMethod
import Network.AWS.Lightsail.CreateContainerService
import Network.AWS.Lightsail.CreateContainerServiceDeployment
import Network.AWS.Lightsail.CreateContainerServiceRegistryLogin
import Network.AWS.Lightsail.CreateDisk
import Network.AWS.Lightsail.CreateDiskFromSnapshot
import Network.AWS.Lightsail.CreateDiskSnapshot
import Network.AWS.Lightsail.CreateDistribution
import Network.AWS.Lightsail.CreateDomain
import Network.AWS.Lightsail.CreateDomainEntry
import Network.AWS.Lightsail.CreateInstanceSnapshot
import Network.AWS.Lightsail.CreateInstances
import Network.AWS.Lightsail.CreateInstancesFromSnapshot
import Network.AWS.Lightsail.CreateKeyPair
import Network.AWS.Lightsail.CreateLoadBalancer
import Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate
import Network.AWS.Lightsail.CreateRelationalDatabase
import Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot
import Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DeleteAlarm
import Network.AWS.Lightsail.DeleteAutoSnapshot
import Network.AWS.Lightsail.DeleteCertificate
import Network.AWS.Lightsail.DeleteContactMethod
import Network.AWS.Lightsail.DeleteContainerImage
import Network.AWS.Lightsail.DeleteContainerService
import Network.AWS.Lightsail.DeleteDisk
import Network.AWS.Lightsail.DeleteDiskSnapshot
import Network.AWS.Lightsail.DeleteDistribution
import Network.AWS.Lightsail.DeleteDomain
import Network.AWS.Lightsail.DeleteDomainEntry
import Network.AWS.Lightsail.DeleteInstance
import Network.AWS.Lightsail.DeleteInstanceSnapshot
import Network.AWS.Lightsail.DeleteKeyPair
import Network.AWS.Lightsail.DeleteKnownHostKeys
import Network.AWS.Lightsail.DeleteLoadBalancer
import Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate
import Network.AWS.Lightsail.DeleteRelationalDatabase
import Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DetachCertificateFromDistribution
import Network.AWS.Lightsail.DetachDisk
import Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
import Network.AWS.Lightsail.DetachStaticIp
import Network.AWS.Lightsail.DisableAddOn
import Network.AWS.Lightsail.DownloadDefaultKeyPair
import Network.AWS.Lightsail.EnableAddOn
import Network.AWS.Lightsail.ExportSnapshot
import Network.AWS.Lightsail.GetActiveNames
import Network.AWS.Lightsail.GetAlarms
import Network.AWS.Lightsail.GetAutoSnapshots
import Network.AWS.Lightsail.GetBlueprints
import Network.AWS.Lightsail.GetBundles
import Network.AWS.Lightsail.GetCertificates
import Network.AWS.Lightsail.GetCloudFormationStackRecords
import Network.AWS.Lightsail.GetContactMethods
import Network.AWS.Lightsail.GetContainerAPIMetadata
import Network.AWS.Lightsail.GetContainerImages
import Network.AWS.Lightsail.GetContainerLog
import Network.AWS.Lightsail.GetContainerServiceDeployments
import Network.AWS.Lightsail.GetContainerServiceMetricData
import Network.AWS.Lightsail.GetContainerServicePowers
import Network.AWS.Lightsail.GetContainerServices
import Network.AWS.Lightsail.GetDisk
import Network.AWS.Lightsail.GetDiskSnapshot
import Network.AWS.Lightsail.GetDiskSnapshots
import Network.AWS.Lightsail.GetDisks
import Network.AWS.Lightsail.GetDistributionBundles
import Network.AWS.Lightsail.GetDistributionLatestCacheReset
import Network.AWS.Lightsail.GetDistributionMetricData
import Network.AWS.Lightsail.GetDistributions
import Network.AWS.Lightsail.GetDomain
import Network.AWS.Lightsail.GetDomains
import Network.AWS.Lightsail.GetExportSnapshotRecords
import Network.AWS.Lightsail.GetInstance
import Network.AWS.Lightsail.GetInstanceAccessDetails
import Network.AWS.Lightsail.GetInstanceMetricData
import Network.AWS.Lightsail.GetInstancePortStates
import Network.AWS.Lightsail.GetInstanceSnapshot
import Network.AWS.Lightsail.GetInstanceSnapshots
import Network.AWS.Lightsail.GetInstanceState
import Network.AWS.Lightsail.GetInstances
import Network.AWS.Lightsail.GetKeyPair
import Network.AWS.Lightsail.GetKeyPairs
import Network.AWS.Lightsail.GetLoadBalancer
import Network.AWS.Lightsail.GetLoadBalancerMetricData
import Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
import Network.AWS.Lightsail.GetLoadBalancers
import Network.AWS.Lightsail.GetOperation
import Network.AWS.Lightsail.GetOperations
import Network.AWS.Lightsail.GetOperationsForResource
import Network.AWS.Lightsail.GetRegions
import Network.AWS.Lightsail.GetRelationalDatabase
import Network.AWS.Lightsail.GetRelationalDatabaseBlueprints
import Network.AWS.Lightsail.GetRelationalDatabaseBundles
import Network.AWS.Lightsail.GetRelationalDatabaseEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogEvents
import Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
import Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
import Network.AWS.Lightsail.GetRelationalDatabaseMetricData
import Network.AWS.Lightsail.GetRelationalDatabaseParameters
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshot
import Network.AWS.Lightsail.GetRelationalDatabaseSnapshots
import Network.AWS.Lightsail.GetRelationalDatabases
import Network.AWS.Lightsail.GetStaticIp
import Network.AWS.Lightsail.GetStaticIps
import Network.AWS.Lightsail.ImportKeyPair
import Network.AWS.Lightsail.IsVpcPeered
import Network.AWS.Lightsail.Lens
import Network.AWS.Lightsail.OpenInstancePublicPorts
import Network.AWS.Lightsail.PeerVpc
import Network.AWS.Lightsail.PutAlarm
import Network.AWS.Lightsail.PutInstancePublicPorts
import Network.AWS.Lightsail.RebootInstance
import Network.AWS.Lightsail.RebootRelationalDatabase
import Network.AWS.Lightsail.RegisterContainerImage
import Network.AWS.Lightsail.ReleaseStaticIp
import Network.AWS.Lightsail.ResetDistributionCache
import Network.AWS.Lightsail.SendContactMethodVerification
import Network.AWS.Lightsail.SetIpAddressType
import Network.AWS.Lightsail.StartInstance
import Network.AWS.Lightsail.StartRelationalDatabase
import Network.AWS.Lightsail.StopInstance
import Network.AWS.Lightsail.StopRelationalDatabase
import Network.AWS.Lightsail.TagResource
import Network.AWS.Lightsail.TestAlarm
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.UnpeerVpc
import Network.AWS.Lightsail.UntagResource
import Network.AWS.Lightsail.UpdateContainerService
import Network.AWS.Lightsail.UpdateDistribution
import Network.AWS.Lightsail.UpdateDistributionBundle
import Network.AWS.Lightsail.UpdateDomainEntry
import Network.AWS.Lightsail.UpdateLoadBalancerAttribute
import Network.AWS.Lightsail.UpdateRelationalDatabase
import Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
import Network.AWS.Lightsail.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Lightsail'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
