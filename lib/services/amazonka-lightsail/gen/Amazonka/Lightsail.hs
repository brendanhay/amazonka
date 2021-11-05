{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Lightsail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Lightsail is the easiest way to get started with Amazon Web
-- Services (AWS) for developers who need to build websites or web
-- applications. It includes everything you need to launch your project
-- quickly - instances (virtual private servers), container services,
-- storage buckets, managed databases, SSD-based block storage, static IP
-- addresses, load balancers, content delivery network (CDN) distributions,
-- DNS management of registered domains, and resource snapshots (backups) -
-- for a low, predictable monthly price.
--
-- You can manage your Lightsail resources using the Lightsail console,
-- Lightsail API, AWS Command Line Interface (AWS CLI), or SDKs. For more
-- information about Lightsail concepts and tasks, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Amazon Lightsail Developer Guide>.
--
-- This API Reference provides detailed information about the actions, data
-- types, parameters, and errors of the Lightsail service. For more
-- information about the supported AWS Regions, endpoints, and service
-- quotas of the Lightsail service, see
-- <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail Endpoints and Quotas>
-- in the /AWS General Reference/.
module Amazonka.Lightsail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccountSetupInProgressException
    _AccountSetupInProgressException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** OperationFailureException
    _OperationFailureException,

    -- ** ServiceException
    _ServiceException,

    -- ** UnauthenticatedException
    _UnauthenticatedException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloseInstancePublicPorts
    CloseInstancePublicPorts (CloseInstancePublicPorts'),
    newCloseInstancePublicPorts,
    CloseInstancePublicPortsResponse (CloseInstancePublicPortsResponse'),
    newCloseInstancePublicPortsResponse,

    -- ** GetRelationalDatabaseMetricData
    GetRelationalDatabaseMetricData (GetRelationalDatabaseMetricData'),
    newGetRelationalDatabaseMetricData,
    GetRelationalDatabaseMetricDataResponse (GetRelationalDatabaseMetricDataResponse'),
    newGetRelationalDatabaseMetricDataResponse,

    -- ** DeleteBucketAccessKey
    DeleteBucketAccessKey (DeleteBucketAccessKey'),
    newDeleteBucketAccessKey,
    DeleteBucketAccessKeyResponse (DeleteBucketAccessKeyResponse'),
    newDeleteBucketAccessKeyResponse,

    -- ** AllocateStaticIp
    AllocateStaticIp (AllocateStaticIp'),
    newAllocateStaticIp,
    AllocateStaticIpResponse (AllocateStaticIpResponse'),
    newAllocateStaticIpResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** DeleteInstanceSnapshot
    DeleteInstanceSnapshot (DeleteInstanceSnapshot'),
    newDeleteInstanceSnapshot,
    DeleteInstanceSnapshotResponse (DeleteInstanceSnapshotResponse'),
    newDeleteInstanceSnapshotResponse,

    -- ** GetInstances (Paginated)
    GetInstances (GetInstances'),
    newGetInstances,
    GetInstancesResponse (GetInstancesResponse'),
    newGetInstancesResponse,

    -- ** GetLoadBalancer
    GetLoadBalancer (GetLoadBalancer'),
    newGetLoadBalancer,
    GetLoadBalancerResponse (GetLoadBalancerResponse'),
    newGetLoadBalancerResponse,

    -- ** DisableAddOn
    DisableAddOn (DisableAddOn'),
    newDisableAddOn,
    DisableAddOnResponse (DisableAddOnResponse'),
    newDisableAddOnResponse,

    -- ** GetDistributions
    GetDistributions (GetDistributions'),
    newGetDistributions,
    GetDistributionsResponse (GetDistributionsResponse'),
    newGetDistributionsResponse,

    -- ** CreateContainerServiceDeployment
    CreateContainerServiceDeployment (CreateContainerServiceDeployment'),
    newCreateContainerServiceDeployment,
    CreateContainerServiceDeploymentResponse (CreateContainerServiceDeploymentResponse'),
    newCreateContainerServiceDeploymentResponse,

    -- ** GetInstance
    GetInstance (GetInstance'),
    newGetInstance,
    GetInstanceResponse (GetInstanceResponse'),
    newGetInstanceResponse,

    -- ** DeleteBucket
    DeleteBucket (DeleteBucket'),
    newDeleteBucket,
    DeleteBucketResponse (DeleteBucketResponse'),
    newDeleteBucketResponse,

    -- ** UpdateBucket
    UpdateBucket (UpdateBucket'),
    newUpdateBucket,
    UpdateBucketResponse (UpdateBucketResponse'),
    newUpdateBucketResponse,

    -- ** GetRelationalDatabaseEvents (Paginated)
    GetRelationalDatabaseEvents (GetRelationalDatabaseEvents'),
    newGetRelationalDatabaseEvents,
    GetRelationalDatabaseEventsResponse (GetRelationalDatabaseEventsResponse'),
    newGetRelationalDatabaseEventsResponse,

    -- ** AttachCertificateToDistribution
    AttachCertificateToDistribution (AttachCertificateToDistribution'),
    newAttachCertificateToDistribution,
    AttachCertificateToDistributionResponse (AttachCertificateToDistributionResponse'),
    newAttachCertificateToDistributionResponse,

    -- ** GetContainerServices
    GetContainerServices (GetContainerServices'),
    newGetContainerServices,
    GetContainerServicesResponse (GetContainerServicesResponse'),
    newGetContainerServicesResponse,

    -- ** UpdateDistributionBundle
    UpdateDistributionBundle (UpdateDistributionBundle'),
    newUpdateDistributionBundle,
    UpdateDistributionBundleResponse (UpdateDistributionBundleResponse'),
    newUpdateDistributionBundleResponse,

    -- ** GetRelationalDatabaseSnapshots (Paginated)
    GetRelationalDatabaseSnapshots (GetRelationalDatabaseSnapshots'),
    newGetRelationalDatabaseSnapshots,
    GetRelationalDatabaseSnapshotsResponse (GetRelationalDatabaseSnapshotsResponse'),
    newGetRelationalDatabaseSnapshotsResponse,

    -- ** GetBucketBundles
    GetBucketBundles (GetBucketBundles'),
    newGetBucketBundles,
    GetBucketBundlesResponse (GetBucketBundlesResponse'),
    newGetBucketBundlesResponse,

    -- ** CreateBucket
    CreateBucket (CreateBucket'),
    newCreateBucket,
    CreateBucketResponse (CreateBucketResponse'),
    newCreateBucketResponse,

    -- ** AttachStaticIp
    AttachStaticIp (AttachStaticIp'),
    newAttachStaticIp,
    AttachStaticIpResponse (AttachStaticIpResponse'),
    newAttachStaticIpResponse,

    -- ** GetRelationalDatabaseParameters (Paginated)
    GetRelationalDatabaseParameters (GetRelationalDatabaseParameters'),
    newGetRelationalDatabaseParameters,
    GetRelationalDatabaseParametersResponse (GetRelationalDatabaseParametersResponse'),
    newGetRelationalDatabaseParametersResponse,

    -- ** DetachDisk
    DetachDisk (DetachDisk'),
    newDetachDisk,
    DetachDiskResponse (DetachDiskResponse'),
    newDetachDiskResponse,

    -- ** GetContactMethods
    GetContactMethods (GetContactMethods'),
    newGetContactMethods,
    GetContactMethodsResponse (GetContactMethodsResponse'),
    newGetContactMethodsResponse,

    -- ** DownloadDefaultKeyPair
    DownloadDefaultKeyPair (DownloadDefaultKeyPair'),
    newDownloadDefaultKeyPair,
    DownloadDefaultKeyPairResponse (DownloadDefaultKeyPairResponse'),
    newDownloadDefaultKeyPairResponse,

    -- ** DeleteLoadBalancerTlsCertificate
    DeleteLoadBalancerTlsCertificate (DeleteLoadBalancerTlsCertificate'),
    newDeleteLoadBalancerTlsCertificate,
    DeleteLoadBalancerTlsCertificateResponse (DeleteLoadBalancerTlsCertificateResponse'),
    newDeleteLoadBalancerTlsCertificateResponse,

    -- ** TestAlarm
    TestAlarm (TestAlarm'),
    newTestAlarm,
    TestAlarmResponse (TestAlarmResponse'),
    newTestAlarmResponse,

    -- ** GetDomains (Paginated)
    GetDomains (GetDomains'),
    newGetDomains,
    GetDomainsResponse (GetDomainsResponse'),
    newGetDomainsResponse,

    -- ** GetContainerImages
    GetContainerImages (GetContainerImages'),
    newGetContainerImages,
    GetContainerImagesResponse (GetContainerImagesResponse'),
    newGetContainerImagesResponse,

    -- ** UpdateRelationalDatabaseParameters
    UpdateRelationalDatabaseParameters (UpdateRelationalDatabaseParameters'),
    newUpdateRelationalDatabaseParameters,
    UpdateRelationalDatabaseParametersResponse (UpdateRelationalDatabaseParametersResponse'),
    newUpdateRelationalDatabaseParametersResponse,

    -- ** CreateLoadBalancerTlsCertificate
    CreateLoadBalancerTlsCertificate (CreateLoadBalancerTlsCertificate'),
    newCreateLoadBalancerTlsCertificate,
    CreateLoadBalancerTlsCertificateResponse (CreateLoadBalancerTlsCertificateResponse'),
    newCreateLoadBalancerTlsCertificateResponse,

    -- ** CreateDomainEntry
    CreateDomainEntry (CreateDomainEntry'),
    newCreateDomainEntry,
    CreateDomainEntryResponse (CreateDomainEntryResponse'),
    newCreateDomainEntryResponse,

    -- ** GetContainerServicePowers
    GetContainerServicePowers (GetContainerServicePowers'),
    newGetContainerServicePowers,
    GetContainerServicePowersResponse (GetContainerServicePowersResponse'),
    newGetContainerServicePowersResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** GetInstanceSnapshots (Paginated)
    GetInstanceSnapshots (GetInstanceSnapshots'),
    newGetInstanceSnapshots,
    GetInstanceSnapshotsResponse (GetInstanceSnapshotsResponse'),
    newGetInstanceSnapshotsResponse,

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

    -- ** CreateCloudFormationStack
    CreateCloudFormationStack (CreateCloudFormationStack'),
    newCreateCloudFormationStack,
    CreateCloudFormationStackResponse (CreateCloudFormationStackResponse'),
    newCreateCloudFormationStackResponse,

    -- ** GetExportSnapshotRecords (Paginated)
    GetExportSnapshotRecords (GetExportSnapshotRecords'),
    newGetExportSnapshotRecords,
    GetExportSnapshotRecordsResponse (GetExportSnapshotRecordsResponse'),
    newGetExportSnapshotRecordsResponse,

    -- ** ReleaseStaticIp
    ReleaseStaticIp (ReleaseStaticIp'),
    newReleaseStaticIp,
    ReleaseStaticIpResponse (ReleaseStaticIpResponse'),
    newReleaseStaticIpResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** CreateContainerServiceRegistryLogin
    CreateContainerServiceRegistryLogin (CreateContainerServiceRegistryLogin'),
    newCreateContainerServiceRegistryLogin,
    CreateContainerServiceRegistryLoginResponse (CreateContainerServiceRegistryLoginResponse'),
    newCreateContainerServiceRegistryLoginResponse,

    -- ** GetCertificates
    GetCertificates (GetCertificates'),
    newGetCertificates,
    GetCertificatesResponse (GetCertificatesResponse'),
    newGetCertificatesResponse,

    -- ** GetContainerServiceMetricData
    GetContainerServiceMetricData (GetContainerServiceMetricData'),
    newGetContainerServiceMetricData,
    GetContainerServiceMetricDataResponse (GetContainerServiceMetricDataResponse'),
    newGetContainerServiceMetricDataResponse,

    -- ** GetDistributionMetricData
    GetDistributionMetricData (GetDistributionMetricData'),
    newGetDistributionMetricData,
    GetDistributionMetricDataResponse (GetDistributionMetricDataResponse'),
    newGetDistributionMetricDataResponse,

    -- ** RebootInstance
    RebootInstance (RebootInstance'),
    newRebootInstance,
    RebootInstanceResponse (RebootInstanceResponse'),
    newRebootInstanceResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** CreateDiskFromSnapshot
    CreateDiskFromSnapshot (CreateDiskFromSnapshot'),
    newCreateDiskFromSnapshot,
    CreateDiskFromSnapshotResponse (CreateDiskFromSnapshotResponse'),
    newCreateDiskFromSnapshotResponse,

    -- ** GetRelationalDatabases (Paginated)
    GetRelationalDatabases (GetRelationalDatabases'),
    newGetRelationalDatabases,
    GetRelationalDatabasesResponse (GetRelationalDatabasesResponse'),
    newGetRelationalDatabasesResponse,

    -- ** GetInstanceSnapshot
    GetInstanceSnapshot (GetInstanceSnapshot'),
    newGetInstanceSnapshot,
    GetInstanceSnapshotResponse (GetInstanceSnapshotResponse'),
    newGetInstanceSnapshotResponse,

    -- ** GetRelationalDatabaseLogEvents
    GetRelationalDatabaseLogEvents (GetRelationalDatabaseLogEvents'),
    newGetRelationalDatabaseLogEvents,
    GetRelationalDatabaseLogEventsResponse (GetRelationalDatabaseLogEventsResponse'),
    newGetRelationalDatabaseLogEventsResponse,

    -- ** CreateContactMethod
    CreateContactMethod (CreateContactMethod'),
    newCreateContactMethod,
    CreateContactMethodResponse (CreateContactMethodResponse'),
    newCreateContactMethodResponse,

    -- ** GetRelationalDatabaseLogStreams
    GetRelationalDatabaseLogStreams (GetRelationalDatabaseLogStreams'),
    newGetRelationalDatabaseLogStreams,
    GetRelationalDatabaseLogStreamsResponse (GetRelationalDatabaseLogStreamsResponse'),
    newGetRelationalDatabaseLogStreamsResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** GetAutoSnapshots
    GetAutoSnapshots (GetAutoSnapshots'),
    newGetAutoSnapshots,
    GetAutoSnapshotsResponse (GetAutoSnapshotsResponse'),
    newGetAutoSnapshotsResponse,

    -- ** GetActiveNames (Paginated)
    GetActiveNames (GetActiveNames'),
    newGetActiveNames,
    GetActiveNamesResponse (GetActiveNamesResponse'),
    newGetActiveNamesResponse,

    -- ** DeleteContactMethod
    DeleteContactMethod (DeleteContactMethod'),
    newDeleteContactMethod,
    DeleteContactMethodResponse (DeleteContactMethodResponse'),
    newDeleteContactMethodResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

    -- ** StopRelationalDatabase
    StopRelationalDatabase (StopRelationalDatabase'),
    newStopRelationalDatabase,
    StopRelationalDatabaseResponse (StopRelationalDatabaseResponse'),
    newStopRelationalDatabaseResponse,

    -- ** CreateRelationalDatabaseSnapshot
    CreateRelationalDatabaseSnapshot (CreateRelationalDatabaseSnapshot'),
    newCreateRelationalDatabaseSnapshot,
    CreateRelationalDatabaseSnapshotResponse (CreateRelationalDatabaseSnapshotResponse'),
    newCreateRelationalDatabaseSnapshotResponse,

    -- ** DetachCertificateFromDistribution
    DetachCertificateFromDistribution (DetachCertificateFromDistribution'),
    newDetachCertificateFromDistribution,
    DetachCertificateFromDistributionResponse (DetachCertificateFromDistributionResponse'),
    newDetachCertificateFromDistributionResponse,

    -- ** CreateContainerService
    CreateContainerService (CreateContainerService'),
    newCreateContainerService,
    CreateContainerServiceResponse (CreateContainerServiceResponse'),
    newCreateContainerServiceResponse,

    -- ** GetInstanceAccessDetails
    GetInstanceAccessDetails (GetInstanceAccessDetails'),
    newGetInstanceAccessDetails,
    GetInstanceAccessDetailsResponse (GetInstanceAccessDetailsResponse'),
    newGetInstanceAccessDetailsResponse,

    -- ** EnableAddOn
    EnableAddOn (EnableAddOn'),
    newEnableAddOn,
    EnableAddOnResponse (EnableAddOnResponse'),
    newEnableAddOnResponse,

    -- ** StopInstance
    StopInstance (StopInstance'),
    newStopInstance,
    StopInstanceResponse (StopInstanceResponse'),
    newStopInstanceResponse,

    -- ** DetachInstancesFromLoadBalancer
    DetachInstancesFromLoadBalancer (DetachInstancesFromLoadBalancer'),
    newDetachInstancesFromLoadBalancer,
    DetachInstancesFromLoadBalancerResponse (DetachInstancesFromLoadBalancerResponse'),
    newDetachInstancesFromLoadBalancerResponse,

    -- ** RegisterContainerImage
    RegisterContainerImage (RegisterContainerImage'),
    newRegisterContainerImage,
    RegisterContainerImageResponse (RegisterContainerImageResponse'),
    newRegisterContainerImageResponse,

    -- ** CreateCertificate
    CreateCertificate (CreateCertificate'),
    newCreateCertificate,
    CreateCertificateResponse (CreateCertificateResponse'),
    newCreateCertificateResponse,

    -- ** CreateInstanceSnapshot
    CreateInstanceSnapshot (CreateInstanceSnapshot'),
    newCreateInstanceSnapshot,
    CreateInstanceSnapshotResponse (CreateInstanceSnapshotResponse'),
    newCreateInstanceSnapshotResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** GetRelationalDatabaseSnapshot
    GetRelationalDatabaseSnapshot (GetRelationalDatabaseSnapshot'),
    newGetRelationalDatabaseSnapshot,
    GetRelationalDatabaseSnapshotResponse (GetRelationalDatabaseSnapshotResponse'),
    newGetRelationalDatabaseSnapshotResponse,

    -- ** IsVpcPeered
    IsVpcPeered (IsVpcPeered'),
    newIsVpcPeered,
    IsVpcPeeredResponse (IsVpcPeeredResponse'),
    newIsVpcPeeredResponse,

    -- ** GetStaticIps (Paginated)
    GetStaticIps (GetStaticIps'),
    newGetStaticIps,
    GetStaticIpsResponse (GetStaticIpsResponse'),
    newGetStaticIpsResponse,

    -- ** UnpeerVpc
    UnpeerVpc (UnpeerVpc'),
    newUnpeerVpc,
    UnpeerVpcResponse (UnpeerVpcResponse'),
    newUnpeerVpcResponse,

    -- ** DeleteDisk
    DeleteDisk (DeleteDisk'),
    newDeleteDisk,
    DeleteDiskResponse (DeleteDiskResponse'),
    newDeleteDiskResponse,

    -- ** CreateInstancesFromSnapshot
    CreateInstancesFromSnapshot (CreateInstancesFromSnapshot'),
    newCreateInstancesFromSnapshot,
    CreateInstancesFromSnapshotResponse (CreateInstancesFromSnapshotResponse'),
    newCreateInstancesFromSnapshotResponse,

    -- ** GetCloudFormationStackRecords (Paginated)
    GetCloudFormationStackRecords (GetCloudFormationStackRecords'),
    newGetCloudFormationStackRecords,
    GetCloudFormationStackRecordsResponse (GetCloudFormationStackRecordsResponse'),
    newGetCloudFormationStackRecordsResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** GetRelationalDatabaseBlueprints (Paginated)
    GetRelationalDatabaseBlueprints (GetRelationalDatabaseBlueprints'),
    newGetRelationalDatabaseBlueprints,
    GetRelationalDatabaseBlueprintsResponse (GetRelationalDatabaseBlueprintsResponse'),
    newGetRelationalDatabaseBlueprintsResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** GetDiskSnapshots (Paginated)
    GetDiskSnapshots (GetDiskSnapshots'),
    newGetDiskSnapshots,
    GetDiskSnapshotsResponse (GetDiskSnapshotsResponse'),
    newGetDiskSnapshotsResponse,

    -- ** GetContainerAPIMetadata
    GetContainerAPIMetadata (GetContainerAPIMetadata'),
    newGetContainerAPIMetadata,
    GetContainerAPIMetadataResponse (GetContainerAPIMetadataResponse'),
    newGetContainerAPIMetadataResponse,

    -- ** GetBucketMetricData
    GetBucketMetricData (GetBucketMetricData'),
    newGetBucketMetricData,
    GetBucketMetricDataResponse (GetBucketMetricDataResponse'),
    newGetBucketMetricDataResponse,

    -- ** PeerVpc
    PeerVpc (PeerVpc'),
    newPeerVpc,
    PeerVpcResponse (PeerVpcResponse'),
    newPeerVpcResponse,

    -- ** GetRelationalDatabaseBundles (Paginated)
    GetRelationalDatabaseBundles (GetRelationalDatabaseBundles'),
    newGetRelationalDatabaseBundles,
    GetRelationalDatabaseBundlesResponse (GetRelationalDatabaseBundlesResponse'),
    newGetRelationalDatabaseBundlesResponse,

    -- ** GetLoadBalancers (Paginated)
    GetLoadBalancers (GetLoadBalancers'),
    newGetLoadBalancers,
    GetLoadBalancersResponse (GetLoadBalancersResponse'),
    newGetLoadBalancersResponse,

    -- ** RebootRelationalDatabase
    RebootRelationalDatabase (RebootRelationalDatabase'),
    newRebootRelationalDatabase,
    RebootRelationalDatabaseResponse (RebootRelationalDatabaseResponse'),
    newRebootRelationalDatabaseResponse,

    -- ** AttachLoadBalancerTlsCertificate
    AttachLoadBalancerTlsCertificate (AttachLoadBalancerTlsCertificate'),
    newAttachLoadBalancerTlsCertificate,
    AttachLoadBalancerTlsCertificateResponse (AttachLoadBalancerTlsCertificateResponse'),
    newAttachLoadBalancerTlsCertificateResponse,

    -- ** UpdateLoadBalancerAttribute
    UpdateLoadBalancerAttribute (UpdateLoadBalancerAttribute'),
    newUpdateLoadBalancerAttribute,
    UpdateLoadBalancerAttributeResponse (UpdateLoadBalancerAttributeResponse'),
    newUpdateLoadBalancerAttributeResponse,

    -- ** DeleteRelationalDatabase
    DeleteRelationalDatabase (DeleteRelationalDatabase'),
    newDeleteRelationalDatabase,
    DeleteRelationalDatabaseResponse (DeleteRelationalDatabaseResponse'),
    newDeleteRelationalDatabaseResponse,

    -- ** GetDiskSnapshot
    GetDiskSnapshot (GetDiskSnapshot'),
    newGetDiskSnapshot,
    GetDiskSnapshotResponse (GetDiskSnapshotResponse'),
    newGetDiskSnapshotResponse,

    -- ** UpdateRelationalDatabase
    UpdateRelationalDatabase (UpdateRelationalDatabase'),
    newUpdateRelationalDatabase,
    UpdateRelationalDatabaseResponse (UpdateRelationalDatabaseResponse'),
    newUpdateRelationalDatabaseResponse,

    -- ** GetStaticIp
    GetStaticIp (GetStaticIp'),
    newGetStaticIp,
    GetStaticIpResponse (GetStaticIpResponse'),
    newGetStaticIpResponse,

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

    -- ** PutAlarm
    PutAlarm (PutAlarm'),
    newPutAlarm,
    PutAlarmResponse (PutAlarmResponse'),
    newPutAlarmResponse,

    -- ** DeleteAlarm
    DeleteAlarm (DeleteAlarm'),
    newDeleteAlarm,
    DeleteAlarmResponse (DeleteAlarmResponse'),
    newDeleteAlarmResponse,

    -- ** GetInstancePortStates
    GetInstancePortStates (GetInstancePortStates'),
    newGetInstancePortStates,
    GetInstancePortStatesResponse (GetInstancePortStatesResponse'),
    newGetInstancePortStatesResponse,

    -- ** DeleteAutoSnapshot
    DeleteAutoSnapshot (DeleteAutoSnapshot'),
    newDeleteAutoSnapshot,
    DeleteAutoSnapshotResponse (DeleteAutoSnapshotResponse'),
    newDeleteAutoSnapshotResponse,

    -- ** CreateRelationalDatabase
    CreateRelationalDatabase (CreateRelationalDatabase'),
    newCreateRelationalDatabase,
    CreateRelationalDatabaseResponse (CreateRelationalDatabaseResponse'),
    newCreateRelationalDatabaseResponse,

    -- ** SendContactMethodVerification
    SendContactMethodVerification (SendContactMethodVerification'),
    newSendContactMethodVerification,
    SendContactMethodVerificationResponse (SendContactMethodVerificationResponse'),
    newSendContactMethodVerificationResponse,

    -- ** GetContainerLog
    GetContainerLog (GetContainerLog'),
    newGetContainerLog,
    GetContainerLogResponse (GetContainerLogResponse'),
    newGetContainerLogResponse,

    -- ** CreateDiskSnapshot
    CreateDiskSnapshot (CreateDiskSnapshot'),
    newCreateDiskSnapshot,
    CreateDiskSnapshotResponse (CreateDiskSnapshotResponse'),
    newCreateDiskSnapshotResponse,

    -- ** DeleteDomainEntry
    DeleteDomainEntry (DeleteDomainEntry'),
    newDeleteDomainEntry,
    DeleteDomainEntryResponse (DeleteDomainEntryResponse'),
    newDeleteDomainEntryResponse,

    -- ** UpdateDomainEntry
    UpdateDomainEntry (UpdateDomainEntry'),
    newUpdateDomainEntry,
    UpdateDomainEntryResponse (UpdateDomainEntryResponse'),
    newUpdateDomainEntryResponse,

    -- ** GetRegions
    GetRegions (GetRegions'),
    newGetRegions,
    GetRegionsResponse (GetRegionsResponse'),
    newGetRegionsResponse,

    -- ** SetIpAddressType
    SetIpAddressType (SetIpAddressType'),
    newSetIpAddressType,
    SetIpAddressTypeResponse (SetIpAddressTypeResponse'),
    newSetIpAddressTypeResponse,

    -- ** DeleteDiskSnapshot
    DeleteDiskSnapshot (DeleteDiskSnapshot'),
    newDeleteDiskSnapshot,
    DeleteDiskSnapshotResponse (DeleteDiskSnapshotResponse'),
    newDeleteDiskSnapshotResponse,

    -- ** GetLoadBalancerMetricData
    GetLoadBalancerMetricData (GetLoadBalancerMetricData'),
    newGetLoadBalancerMetricData,
    GetLoadBalancerMetricDataResponse (GetLoadBalancerMetricDataResponse'),
    newGetLoadBalancerMetricDataResponse,

    -- ** GetInstanceState
    GetInstanceState (GetInstanceState'),
    newGetInstanceState,
    GetInstanceStateResponse (GetInstanceStateResponse'),
    newGetInstanceStateResponse,

    -- ** GetKeyPairs (Paginated)
    GetKeyPairs (GetKeyPairs'),
    newGetKeyPairs,
    GetKeyPairsResponse (GetKeyPairsResponse'),
    newGetKeyPairsResponse,

    -- ** GetOperations (Paginated)
    GetOperations (GetOperations'),
    newGetOperations,
    GetOperationsResponse (GetOperationsResponse'),
    newGetOperationsResponse,

    -- ** GetBucketAccessKeys
    GetBucketAccessKeys (GetBucketAccessKeys'),
    newGetBucketAccessKeys,
    GetBucketAccessKeysResponse (GetBucketAccessKeysResponse'),
    newGetBucketAccessKeysResponse,

    -- ** GetDisks (Paginated)
    GetDisks (GetDisks'),
    newGetDisks,
    GetDisksResponse (GetDisksResponse'),
    newGetDisksResponse,

    -- ** GetRelationalDatabase
    GetRelationalDatabase (GetRelationalDatabase'),
    newGetRelationalDatabase,
    GetRelationalDatabaseResponse (GetRelationalDatabaseResponse'),
    newGetRelationalDatabaseResponse,

    -- ** AttachInstancesToLoadBalancer
    AttachInstancesToLoadBalancer (AttachInstancesToLoadBalancer'),
    newAttachInstancesToLoadBalancer,
    AttachInstancesToLoadBalancerResponse (AttachInstancesToLoadBalancerResponse'),
    newAttachInstancesToLoadBalancerResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetOperation
    GetOperation (GetOperation'),
    newGetOperation,
    GetOperationResponse (GetOperationResponse'),
    newGetOperationResponse,

    -- ** ResetDistributionCache
    ResetDistributionCache (ResetDistributionCache'),
    newResetDistributionCache,
    ResetDistributionCacheResponse (ResetDistributionCacheResponse'),
    newResetDistributionCacheResponse,

    -- ** UpdateBucketBundle
    UpdateBucketBundle (UpdateBucketBundle'),
    newUpdateBucketBundle,
    UpdateBucketBundleResponse (UpdateBucketBundleResponse'),
    newUpdateBucketBundleResponse,

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** GetBuckets
    GetBuckets (GetBuckets'),
    newGetBuckets,
    GetBucketsResponse (GetBucketsResponse'),
    newGetBucketsResponse,

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

    -- ** DeleteRelationalDatabaseSnapshot
    DeleteRelationalDatabaseSnapshot (DeleteRelationalDatabaseSnapshot'),
    newDeleteRelationalDatabaseSnapshot,
    DeleteRelationalDatabaseSnapshotResponse (DeleteRelationalDatabaseSnapshotResponse'),
    newDeleteRelationalDatabaseSnapshotResponse,

    -- ** DeleteContainerService
    DeleteContainerService (DeleteContainerService'),
    newDeleteContainerService,
    DeleteContainerServiceResponse (DeleteContainerServiceResponse'),
    newDeleteContainerServiceResponse,

    -- ** GetInstanceMetricData
    GetInstanceMetricData (GetInstanceMetricData'),
    newGetInstanceMetricData,
    GetInstanceMetricDataResponse (GetInstanceMetricDataResponse'),
    newGetInstanceMetricDataResponse,

    -- ** GetKeyPair
    GetKeyPair (GetKeyPair'),
    newGetKeyPair,
    GetKeyPairResponse (GetKeyPairResponse'),
    newGetKeyPairResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** PutInstancePublicPorts
    PutInstancePublicPorts (PutInstancePublicPorts'),
    newPutInstancePublicPorts,
    PutInstancePublicPortsResponse (PutInstancePublicPortsResponse'),
    newPutInstancePublicPortsResponse,

    -- ** GetDistributionBundles
    GetDistributionBundles (GetDistributionBundles'),
    newGetDistributionBundles,
    GetDistributionBundlesResponse (GetDistributionBundlesResponse'),
    newGetDistributionBundlesResponse,

    -- ** GetDisk
    GetDisk (GetDisk'),
    newGetDisk,
    GetDiskResponse (GetDiskResponse'),
    newGetDiskResponse,

    -- ** GetDistributionLatestCacheReset
    GetDistributionLatestCacheReset (GetDistributionLatestCacheReset'),
    newGetDistributionLatestCacheReset,
    GetDistributionLatestCacheResetResponse (GetDistributionLatestCacheResetResponse'),
    newGetDistributionLatestCacheResetResponse,

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

    -- ** DeleteKnownHostKeys
    DeleteKnownHostKeys (DeleteKnownHostKeys'),
    newDeleteKnownHostKeys,
    DeleteKnownHostKeysResponse (DeleteKnownHostKeysResponse'),
    newDeleteKnownHostKeysResponse,

    -- ** AttachDisk
    AttachDisk (AttachDisk'),
    newAttachDisk,
    AttachDiskResponse (AttachDiskResponse'),
    newAttachDiskResponse,

    -- ** DetachStaticIp
    DetachStaticIp (DetachStaticIp'),
    newDetachStaticIp,
    DetachStaticIpResponse (DetachStaticIpResponse'),
    newDetachStaticIpResponse,

    -- ** CreateInstances
    CreateInstances (CreateInstances'),
    newCreateInstances,
    CreateInstancesResponse (CreateInstancesResponse'),
    newCreateInstancesResponse,

    -- ** GetAlarms
    GetAlarms (GetAlarms'),
    newGetAlarms,
    GetAlarmsResponse (GetAlarmsResponse'),
    newGetAlarmsResponse,

    -- ** OpenInstancePublicPorts
    OpenInstancePublicPorts (OpenInstancePublicPorts'),
    newOpenInstancePublicPorts,
    OpenInstancePublicPortsResponse (OpenInstancePublicPortsResponse'),
    newOpenInstancePublicPortsResponse,

    -- ** StartRelationalDatabase
    StartRelationalDatabase (StartRelationalDatabase'),
    newStartRelationalDatabase,
    StartRelationalDatabaseResponse (StartRelationalDatabaseResponse'),
    newStartRelationalDatabaseResponse,

    -- ** DeleteContainerImage
    DeleteContainerImage (DeleteContainerImage'),
    newDeleteContainerImage,
    DeleteContainerImageResponse (DeleteContainerImageResponse'),
    newDeleteContainerImageResponse,

    -- ** GetBundles (Paginated)
    GetBundles (GetBundles'),
    newGetBundles,
    GetBundlesResponse (GetBundlesResponse'),
    newGetBundlesResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** GetLoadBalancerTlsCertificates
    GetLoadBalancerTlsCertificates (GetLoadBalancerTlsCertificates'),
    newGetLoadBalancerTlsCertificates,
    GetLoadBalancerTlsCertificatesResponse (GetLoadBalancerTlsCertificatesResponse'),
    newGetLoadBalancerTlsCertificatesResponse,

    -- ** SetResourceAccessForBucket
    SetResourceAccessForBucket (SetResourceAccessForBucket'),
    newSetResourceAccessForBucket,
    SetResourceAccessForBucketResponse (SetResourceAccessForBucketResponse'),
    newSetResourceAccessForBucketResponse,

    -- ** CreateDisk
    CreateDisk (CreateDisk'),
    newCreateDisk,
    CreateDiskResponse (CreateDiskResponse'),
    newCreateDiskResponse,

    -- ** CreateBucketAccessKey
    CreateBucketAccessKey (CreateBucketAccessKey'),
    newCreateBucketAccessKey,
    CreateBucketAccessKeyResponse (CreateBucketAccessKeyResponse'),
    newCreateBucketAccessKeyResponse,

    -- ** GetOperationsForResource
    GetOperationsForResource (GetOperationsForResource'),
    newGetOperationsForResource,
    GetOperationsForResourceResponse (GetOperationsForResourceResponse'),
    newGetOperationsForResourceResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** StartInstance
    StartInstance (StartInstance'),
    newStartInstance,
    StartInstanceResponse (StartInstanceResponse'),
    newStartInstanceResponse,

    -- * Types

    -- ** AccessDirection
    AccessDirection (..),

    -- ** AccessType
    AccessType (..),

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

    -- ** BucketMetricName
    BucketMetricName (..),

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

    -- ** ContainerServiceStateDetailCode
    ContainerServiceStateDetailCode (..),

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

    -- ** ResourceBucketAccess
    ResourceBucketAccess (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** StatusType
    StatusType (..),

    -- ** TreatMissingData
    TreatMissingData (..),

    -- ** AccessKey
    AccessKey (AccessKey'),
    newAccessKey,

    -- ** AccessKeyLastUsed
    AccessKeyLastUsed (AccessKeyLastUsed'),
    newAccessKeyLastUsed,

    -- ** AccessRules
    AccessRules (AccessRules'),
    newAccessRules,

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

    -- ** Bucket
    Bucket (Bucket'),
    newBucket,

    -- ** BucketBundle
    BucketBundle (BucketBundle'),
    newBucketBundle,

    -- ** BucketState
    BucketState (BucketState'),
    newBucketState,

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

    -- ** ContainerServiceStateDetail
    ContainerServiceStateDetail (ContainerServiceStateDetail'),
    newContainerServiceStateDetail,

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

    -- ** ResourceReceivingAccess
    ResourceReceivingAccess (ResourceReceivingAccess'),
    newResourceReceivingAccess,

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

import Amazonka.Lightsail.AllocateStaticIp
import Amazonka.Lightsail.AttachCertificateToDistribution
import Amazonka.Lightsail.AttachDisk
import Amazonka.Lightsail.AttachInstancesToLoadBalancer
import Amazonka.Lightsail.AttachLoadBalancerTlsCertificate
import Amazonka.Lightsail.AttachStaticIp
import Amazonka.Lightsail.CloseInstancePublicPorts
import Amazonka.Lightsail.CopySnapshot
import Amazonka.Lightsail.CreateBucket
import Amazonka.Lightsail.CreateBucketAccessKey
import Amazonka.Lightsail.CreateCertificate
import Amazonka.Lightsail.CreateCloudFormationStack
import Amazonka.Lightsail.CreateContactMethod
import Amazonka.Lightsail.CreateContainerService
import Amazonka.Lightsail.CreateContainerServiceDeployment
import Amazonka.Lightsail.CreateContainerServiceRegistryLogin
import Amazonka.Lightsail.CreateDisk
import Amazonka.Lightsail.CreateDiskFromSnapshot
import Amazonka.Lightsail.CreateDiskSnapshot
import Amazonka.Lightsail.CreateDistribution
import Amazonka.Lightsail.CreateDomain
import Amazonka.Lightsail.CreateDomainEntry
import Amazonka.Lightsail.CreateInstanceSnapshot
import Amazonka.Lightsail.CreateInstances
import Amazonka.Lightsail.CreateInstancesFromSnapshot
import Amazonka.Lightsail.CreateKeyPair
import Amazonka.Lightsail.CreateLoadBalancer
import Amazonka.Lightsail.CreateLoadBalancerTlsCertificate
import Amazonka.Lightsail.CreateRelationalDatabase
import Amazonka.Lightsail.CreateRelationalDatabaseFromSnapshot
import Amazonka.Lightsail.CreateRelationalDatabaseSnapshot
import Amazonka.Lightsail.DeleteAlarm
import Amazonka.Lightsail.DeleteAutoSnapshot
import Amazonka.Lightsail.DeleteBucket
import Amazonka.Lightsail.DeleteBucketAccessKey
import Amazonka.Lightsail.DeleteCertificate
import Amazonka.Lightsail.DeleteContactMethod
import Amazonka.Lightsail.DeleteContainerImage
import Amazonka.Lightsail.DeleteContainerService
import Amazonka.Lightsail.DeleteDisk
import Amazonka.Lightsail.DeleteDiskSnapshot
import Amazonka.Lightsail.DeleteDistribution
import Amazonka.Lightsail.DeleteDomain
import Amazonka.Lightsail.DeleteDomainEntry
import Amazonka.Lightsail.DeleteInstance
import Amazonka.Lightsail.DeleteInstanceSnapshot
import Amazonka.Lightsail.DeleteKeyPair
import Amazonka.Lightsail.DeleteKnownHostKeys
import Amazonka.Lightsail.DeleteLoadBalancer
import Amazonka.Lightsail.DeleteLoadBalancerTlsCertificate
import Amazonka.Lightsail.DeleteRelationalDatabase
import Amazonka.Lightsail.DeleteRelationalDatabaseSnapshot
import Amazonka.Lightsail.DetachCertificateFromDistribution
import Amazonka.Lightsail.DetachDisk
import Amazonka.Lightsail.DetachInstancesFromLoadBalancer
import Amazonka.Lightsail.DetachStaticIp
import Amazonka.Lightsail.DisableAddOn
import Amazonka.Lightsail.DownloadDefaultKeyPair
import Amazonka.Lightsail.EnableAddOn
import Amazonka.Lightsail.ExportSnapshot
import Amazonka.Lightsail.GetActiveNames
import Amazonka.Lightsail.GetAlarms
import Amazonka.Lightsail.GetAutoSnapshots
import Amazonka.Lightsail.GetBlueprints
import Amazonka.Lightsail.GetBucketAccessKeys
import Amazonka.Lightsail.GetBucketBundles
import Amazonka.Lightsail.GetBucketMetricData
import Amazonka.Lightsail.GetBuckets
import Amazonka.Lightsail.GetBundles
import Amazonka.Lightsail.GetCertificates
import Amazonka.Lightsail.GetCloudFormationStackRecords
import Amazonka.Lightsail.GetContactMethods
import Amazonka.Lightsail.GetContainerAPIMetadata
import Amazonka.Lightsail.GetContainerImages
import Amazonka.Lightsail.GetContainerLog
import Amazonka.Lightsail.GetContainerServiceDeployments
import Amazonka.Lightsail.GetContainerServiceMetricData
import Amazonka.Lightsail.GetContainerServicePowers
import Amazonka.Lightsail.GetContainerServices
import Amazonka.Lightsail.GetDisk
import Amazonka.Lightsail.GetDiskSnapshot
import Amazonka.Lightsail.GetDiskSnapshots
import Amazonka.Lightsail.GetDisks
import Amazonka.Lightsail.GetDistributionBundles
import Amazonka.Lightsail.GetDistributionLatestCacheReset
import Amazonka.Lightsail.GetDistributionMetricData
import Amazonka.Lightsail.GetDistributions
import Amazonka.Lightsail.GetDomain
import Amazonka.Lightsail.GetDomains
import Amazonka.Lightsail.GetExportSnapshotRecords
import Amazonka.Lightsail.GetInstance
import Amazonka.Lightsail.GetInstanceAccessDetails
import Amazonka.Lightsail.GetInstanceMetricData
import Amazonka.Lightsail.GetInstancePortStates
import Amazonka.Lightsail.GetInstanceSnapshot
import Amazonka.Lightsail.GetInstanceSnapshots
import Amazonka.Lightsail.GetInstanceState
import Amazonka.Lightsail.GetInstances
import Amazonka.Lightsail.GetKeyPair
import Amazonka.Lightsail.GetKeyPairs
import Amazonka.Lightsail.GetLoadBalancer
import Amazonka.Lightsail.GetLoadBalancerMetricData
import Amazonka.Lightsail.GetLoadBalancerTlsCertificates
import Amazonka.Lightsail.GetLoadBalancers
import Amazonka.Lightsail.GetOperation
import Amazonka.Lightsail.GetOperations
import Amazonka.Lightsail.GetOperationsForResource
import Amazonka.Lightsail.GetRegions
import Amazonka.Lightsail.GetRelationalDatabase
import Amazonka.Lightsail.GetRelationalDatabaseBlueprints
import Amazonka.Lightsail.GetRelationalDatabaseBundles
import Amazonka.Lightsail.GetRelationalDatabaseEvents
import Amazonka.Lightsail.GetRelationalDatabaseLogEvents
import Amazonka.Lightsail.GetRelationalDatabaseLogStreams
import Amazonka.Lightsail.GetRelationalDatabaseMasterUserPassword
import Amazonka.Lightsail.GetRelationalDatabaseMetricData
import Amazonka.Lightsail.GetRelationalDatabaseParameters
import Amazonka.Lightsail.GetRelationalDatabaseSnapshot
import Amazonka.Lightsail.GetRelationalDatabaseSnapshots
import Amazonka.Lightsail.GetRelationalDatabases
import Amazonka.Lightsail.GetStaticIp
import Amazonka.Lightsail.GetStaticIps
import Amazonka.Lightsail.ImportKeyPair
import Amazonka.Lightsail.IsVpcPeered
import Amazonka.Lightsail.Lens
import Amazonka.Lightsail.OpenInstancePublicPorts
import Amazonka.Lightsail.PeerVpc
import Amazonka.Lightsail.PutAlarm
import Amazonka.Lightsail.PutInstancePublicPorts
import Amazonka.Lightsail.RebootInstance
import Amazonka.Lightsail.RebootRelationalDatabase
import Amazonka.Lightsail.RegisterContainerImage
import Amazonka.Lightsail.ReleaseStaticIp
import Amazonka.Lightsail.ResetDistributionCache
import Amazonka.Lightsail.SendContactMethodVerification
import Amazonka.Lightsail.SetIpAddressType
import Amazonka.Lightsail.SetResourceAccessForBucket
import Amazonka.Lightsail.StartInstance
import Amazonka.Lightsail.StartRelationalDatabase
import Amazonka.Lightsail.StopInstance
import Amazonka.Lightsail.StopRelationalDatabase
import Amazonka.Lightsail.TagResource
import Amazonka.Lightsail.TestAlarm
import Amazonka.Lightsail.Types
import Amazonka.Lightsail.UnpeerVpc
import Amazonka.Lightsail.UntagResource
import Amazonka.Lightsail.UpdateBucket
import Amazonka.Lightsail.UpdateBucketBundle
import Amazonka.Lightsail.UpdateContainerService
import Amazonka.Lightsail.UpdateDistribution
import Amazonka.Lightsail.UpdateDistributionBundle
import Amazonka.Lightsail.UpdateDomainEntry
import Amazonka.Lightsail.UpdateLoadBalancerAttribute
import Amazonka.Lightsail.UpdateRelationalDatabase
import Amazonka.Lightsail.UpdateRelationalDatabaseParameters
import Amazonka.Lightsail.Waiters

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
