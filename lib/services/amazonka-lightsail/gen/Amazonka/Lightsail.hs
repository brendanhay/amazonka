{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Lightsail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2016-11-28@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Lightsail is the easiest way to get started with Amazon Web
-- Services (Amazon Web Services) for developers who need to build websites
-- or web applications. It includes everything you need to launch your
-- project quickly - instances (virtual private servers), container
-- services, storage buckets, managed databases, SSD-based block storage,
-- static IP addresses, load balancers, content delivery network (CDN)
-- distributions, DNS management of registered domains, and resource
-- snapshots (backups) - for a low, predictable monthly price.
--
-- You can manage your Lightsail resources using the Lightsail console,
-- Lightsail API, Command Line Interface (CLI), or SDKs. For more
-- information about Lightsail concepts and tasks, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Amazon Lightsail Developer Guide>.
--
-- This API Reference provides detailed information about the actions, data
-- types, parameters, and errors of the Lightsail service. For more
-- information about the supported Amazon Web Services Regions, endpoints,
-- and service quotas of the Lightsail service, see
-- <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail Endpoints and Quotas>
-- in the /Amazon Web Services General Reference/.
module Amazonka.Lightsail
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** AccountSetupInProgressException
    _AccountSetupInProgressException,

    -- ** InvalidInputException
    _InvalidInputException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** OperationFailureException
    _OperationFailureException,

    -- ** ServiceException
    _ServiceException,

    -- ** UnauthenticatedException
    _UnauthenticatedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AllocateStaticIp
    AllocateStaticIp (AllocateStaticIp'),
    newAllocateStaticIp,
    AllocateStaticIpResponse (AllocateStaticIpResponse'),
    newAllocateStaticIpResponse,

    -- ** AttachCertificateToDistribution
    AttachCertificateToDistribution (AttachCertificateToDistribution'),
    newAttachCertificateToDistribution,
    AttachCertificateToDistributionResponse (AttachCertificateToDistributionResponse'),
    newAttachCertificateToDistributionResponse,

    -- ** AttachDisk
    AttachDisk (AttachDisk'),
    newAttachDisk,
    AttachDiskResponse (AttachDiskResponse'),
    newAttachDiskResponse,

    -- ** AttachInstancesToLoadBalancer
    AttachInstancesToLoadBalancer (AttachInstancesToLoadBalancer'),
    newAttachInstancesToLoadBalancer,
    AttachInstancesToLoadBalancerResponse (AttachInstancesToLoadBalancerResponse'),
    newAttachInstancesToLoadBalancerResponse,

    -- ** AttachLoadBalancerTlsCertificate
    AttachLoadBalancerTlsCertificate (AttachLoadBalancerTlsCertificate'),
    newAttachLoadBalancerTlsCertificate,
    AttachLoadBalancerTlsCertificateResponse (AttachLoadBalancerTlsCertificateResponse'),
    newAttachLoadBalancerTlsCertificateResponse,

    -- ** AttachStaticIp
    AttachStaticIp (AttachStaticIp'),
    newAttachStaticIp,
    AttachStaticIpResponse (AttachStaticIpResponse'),
    newAttachStaticIpResponse,

    -- ** CloseInstancePublicPorts
    CloseInstancePublicPorts (CloseInstancePublicPorts'),
    newCloseInstancePublicPorts,
    CloseInstancePublicPortsResponse (CloseInstancePublicPortsResponse'),
    newCloseInstancePublicPortsResponse,

    -- ** CopySnapshot
    CopySnapshot (CopySnapshot'),
    newCopySnapshot,
    CopySnapshotResponse (CopySnapshotResponse'),
    newCopySnapshotResponse,

    -- ** CreateBucket
    CreateBucket (CreateBucket'),
    newCreateBucket,
    CreateBucketResponse (CreateBucketResponse'),
    newCreateBucketResponse,

    -- ** CreateBucketAccessKey
    CreateBucketAccessKey (CreateBucketAccessKey'),
    newCreateBucketAccessKey,
    CreateBucketAccessKeyResponse (CreateBucketAccessKeyResponse'),
    newCreateBucketAccessKeyResponse,

    -- ** CreateCertificate
    CreateCertificate (CreateCertificate'),
    newCreateCertificate,
    CreateCertificateResponse (CreateCertificateResponse'),
    newCreateCertificateResponse,

    -- ** CreateCloudFormationStack
    CreateCloudFormationStack (CreateCloudFormationStack'),
    newCreateCloudFormationStack,
    CreateCloudFormationStackResponse (CreateCloudFormationStackResponse'),
    newCreateCloudFormationStackResponse,

    -- ** CreateContactMethod
    CreateContactMethod (CreateContactMethod'),
    newCreateContactMethod,
    CreateContactMethodResponse (CreateContactMethodResponse'),
    newCreateContactMethodResponse,

    -- ** CreateContainerService
    CreateContainerService (CreateContainerService'),
    newCreateContainerService,
    CreateContainerServiceResponse (CreateContainerServiceResponse'),
    newCreateContainerServiceResponse,

    -- ** CreateContainerServiceDeployment
    CreateContainerServiceDeployment (CreateContainerServiceDeployment'),
    newCreateContainerServiceDeployment,
    CreateContainerServiceDeploymentResponse (CreateContainerServiceDeploymentResponse'),
    newCreateContainerServiceDeploymentResponse,

    -- ** CreateContainerServiceRegistryLogin
    CreateContainerServiceRegistryLogin (CreateContainerServiceRegistryLogin'),
    newCreateContainerServiceRegistryLogin,
    CreateContainerServiceRegistryLoginResponse (CreateContainerServiceRegistryLoginResponse'),
    newCreateContainerServiceRegistryLoginResponse,

    -- ** CreateDisk
    CreateDisk (CreateDisk'),
    newCreateDisk,
    CreateDiskResponse (CreateDiskResponse'),
    newCreateDiskResponse,

    -- ** CreateDiskFromSnapshot
    CreateDiskFromSnapshot (CreateDiskFromSnapshot'),
    newCreateDiskFromSnapshot,
    CreateDiskFromSnapshotResponse (CreateDiskFromSnapshotResponse'),
    newCreateDiskFromSnapshotResponse,

    -- ** CreateDiskSnapshot
    CreateDiskSnapshot (CreateDiskSnapshot'),
    newCreateDiskSnapshot,
    CreateDiskSnapshotResponse (CreateDiskSnapshotResponse'),
    newCreateDiskSnapshotResponse,

    -- ** CreateDistribution
    CreateDistribution (CreateDistribution'),
    newCreateDistribution,
    CreateDistributionResponse (CreateDistributionResponse'),
    newCreateDistributionResponse,

    -- ** CreateDomain
    CreateDomain (CreateDomain'),
    newCreateDomain,
    CreateDomainResponse (CreateDomainResponse'),
    newCreateDomainResponse,

    -- ** CreateDomainEntry
    CreateDomainEntry (CreateDomainEntry'),
    newCreateDomainEntry,
    CreateDomainEntryResponse (CreateDomainEntryResponse'),
    newCreateDomainEntryResponse,

    -- ** CreateGUISessionAccessDetails
    CreateGUISessionAccessDetails (CreateGUISessionAccessDetails'),
    newCreateGUISessionAccessDetails,
    CreateGUISessionAccessDetailsResponse (CreateGUISessionAccessDetailsResponse'),
    newCreateGUISessionAccessDetailsResponse,

    -- ** CreateInstanceSnapshot
    CreateInstanceSnapshot (CreateInstanceSnapshot'),
    newCreateInstanceSnapshot,
    CreateInstanceSnapshotResponse (CreateInstanceSnapshotResponse'),
    newCreateInstanceSnapshotResponse,

    -- ** CreateInstances
    CreateInstances (CreateInstances'),
    newCreateInstances,
    CreateInstancesResponse (CreateInstancesResponse'),
    newCreateInstancesResponse,

    -- ** CreateInstancesFromSnapshot
    CreateInstancesFromSnapshot (CreateInstancesFromSnapshot'),
    newCreateInstancesFromSnapshot,
    CreateInstancesFromSnapshotResponse (CreateInstancesFromSnapshotResponse'),
    newCreateInstancesFromSnapshotResponse,

    -- ** CreateKeyPair
    CreateKeyPair (CreateKeyPair'),
    newCreateKeyPair,
    CreateKeyPairResponse (CreateKeyPairResponse'),
    newCreateKeyPairResponse,

    -- ** CreateLoadBalancer
    CreateLoadBalancer (CreateLoadBalancer'),
    newCreateLoadBalancer,
    CreateLoadBalancerResponse (CreateLoadBalancerResponse'),
    newCreateLoadBalancerResponse,

    -- ** CreateLoadBalancerTlsCertificate
    CreateLoadBalancerTlsCertificate (CreateLoadBalancerTlsCertificate'),
    newCreateLoadBalancerTlsCertificate,
    CreateLoadBalancerTlsCertificateResponse (CreateLoadBalancerTlsCertificateResponse'),
    newCreateLoadBalancerTlsCertificateResponse,

    -- ** CreateRelationalDatabase
    CreateRelationalDatabase (CreateRelationalDatabase'),
    newCreateRelationalDatabase,
    CreateRelationalDatabaseResponse (CreateRelationalDatabaseResponse'),
    newCreateRelationalDatabaseResponse,

    -- ** CreateRelationalDatabaseFromSnapshot
    CreateRelationalDatabaseFromSnapshot (CreateRelationalDatabaseFromSnapshot'),
    newCreateRelationalDatabaseFromSnapshot,
    CreateRelationalDatabaseFromSnapshotResponse (CreateRelationalDatabaseFromSnapshotResponse'),
    newCreateRelationalDatabaseFromSnapshotResponse,

    -- ** CreateRelationalDatabaseSnapshot
    CreateRelationalDatabaseSnapshot (CreateRelationalDatabaseSnapshot'),
    newCreateRelationalDatabaseSnapshot,
    CreateRelationalDatabaseSnapshotResponse (CreateRelationalDatabaseSnapshotResponse'),
    newCreateRelationalDatabaseSnapshotResponse,

    -- ** DeleteAlarm
    DeleteAlarm (DeleteAlarm'),
    newDeleteAlarm,
    DeleteAlarmResponse (DeleteAlarmResponse'),
    newDeleteAlarmResponse,

    -- ** DeleteAutoSnapshot
    DeleteAutoSnapshot (DeleteAutoSnapshot'),
    newDeleteAutoSnapshot,
    DeleteAutoSnapshotResponse (DeleteAutoSnapshotResponse'),
    newDeleteAutoSnapshotResponse,

    -- ** DeleteBucket
    DeleteBucket (DeleteBucket'),
    newDeleteBucket,
    DeleteBucketResponse (DeleteBucketResponse'),
    newDeleteBucketResponse,

    -- ** DeleteBucketAccessKey
    DeleteBucketAccessKey (DeleteBucketAccessKey'),
    newDeleteBucketAccessKey,
    DeleteBucketAccessKeyResponse (DeleteBucketAccessKeyResponse'),
    newDeleteBucketAccessKeyResponse,

    -- ** DeleteCertificate
    DeleteCertificate (DeleteCertificate'),
    newDeleteCertificate,
    DeleteCertificateResponse (DeleteCertificateResponse'),
    newDeleteCertificateResponse,

    -- ** DeleteContactMethod
    DeleteContactMethod (DeleteContactMethod'),
    newDeleteContactMethod,
    DeleteContactMethodResponse (DeleteContactMethodResponse'),
    newDeleteContactMethodResponse,

    -- ** DeleteContainerImage
    DeleteContainerImage (DeleteContainerImage'),
    newDeleteContainerImage,
    DeleteContainerImageResponse (DeleteContainerImageResponse'),
    newDeleteContainerImageResponse,

    -- ** DeleteContainerService
    DeleteContainerService (DeleteContainerService'),
    newDeleteContainerService,
    DeleteContainerServiceResponse (DeleteContainerServiceResponse'),
    newDeleteContainerServiceResponse,

    -- ** DeleteDisk
    DeleteDisk (DeleteDisk'),
    newDeleteDisk,
    DeleteDiskResponse (DeleteDiskResponse'),
    newDeleteDiskResponse,

    -- ** DeleteDiskSnapshot
    DeleteDiskSnapshot (DeleteDiskSnapshot'),
    newDeleteDiskSnapshot,
    DeleteDiskSnapshotResponse (DeleteDiskSnapshotResponse'),
    newDeleteDiskSnapshotResponse,

    -- ** DeleteDistribution
    DeleteDistribution (DeleteDistribution'),
    newDeleteDistribution,
    DeleteDistributionResponse (DeleteDistributionResponse'),
    newDeleteDistributionResponse,

    -- ** DeleteDomain
    DeleteDomain (DeleteDomain'),
    newDeleteDomain,
    DeleteDomainResponse (DeleteDomainResponse'),
    newDeleteDomainResponse,

    -- ** DeleteDomainEntry
    DeleteDomainEntry (DeleteDomainEntry'),
    newDeleteDomainEntry,
    DeleteDomainEntryResponse (DeleteDomainEntryResponse'),
    newDeleteDomainEntryResponse,

    -- ** DeleteInstance
    DeleteInstance (DeleteInstance'),
    newDeleteInstance,
    DeleteInstanceResponse (DeleteInstanceResponse'),
    newDeleteInstanceResponse,

    -- ** DeleteInstanceSnapshot
    DeleteInstanceSnapshot (DeleteInstanceSnapshot'),
    newDeleteInstanceSnapshot,
    DeleteInstanceSnapshotResponse (DeleteInstanceSnapshotResponse'),
    newDeleteInstanceSnapshotResponse,

    -- ** DeleteKeyPair
    DeleteKeyPair (DeleteKeyPair'),
    newDeleteKeyPair,
    DeleteKeyPairResponse (DeleteKeyPairResponse'),
    newDeleteKeyPairResponse,

    -- ** DeleteKnownHostKeys
    DeleteKnownHostKeys (DeleteKnownHostKeys'),
    newDeleteKnownHostKeys,
    DeleteKnownHostKeysResponse (DeleteKnownHostKeysResponse'),
    newDeleteKnownHostKeysResponse,

    -- ** DeleteLoadBalancer
    DeleteLoadBalancer (DeleteLoadBalancer'),
    newDeleteLoadBalancer,
    DeleteLoadBalancerResponse (DeleteLoadBalancerResponse'),
    newDeleteLoadBalancerResponse,

    -- ** DeleteLoadBalancerTlsCertificate
    DeleteLoadBalancerTlsCertificate (DeleteLoadBalancerTlsCertificate'),
    newDeleteLoadBalancerTlsCertificate,
    DeleteLoadBalancerTlsCertificateResponse (DeleteLoadBalancerTlsCertificateResponse'),
    newDeleteLoadBalancerTlsCertificateResponse,

    -- ** DeleteRelationalDatabase
    DeleteRelationalDatabase (DeleteRelationalDatabase'),
    newDeleteRelationalDatabase,
    DeleteRelationalDatabaseResponse (DeleteRelationalDatabaseResponse'),
    newDeleteRelationalDatabaseResponse,

    -- ** DeleteRelationalDatabaseSnapshot
    DeleteRelationalDatabaseSnapshot (DeleteRelationalDatabaseSnapshot'),
    newDeleteRelationalDatabaseSnapshot,
    DeleteRelationalDatabaseSnapshotResponse (DeleteRelationalDatabaseSnapshotResponse'),
    newDeleteRelationalDatabaseSnapshotResponse,

    -- ** DetachCertificateFromDistribution
    DetachCertificateFromDistribution (DetachCertificateFromDistribution'),
    newDetachCertificateFromDistribution,
    DetachCertificateFromDistributionResponse (DetachCertificateFromDistributionResponse'),
    newDetachCertificateFromDistributionResponse,

    -- ** DetachDisk
    DetachDisk (DetachDisk'),
    newDetachDisk,
    DetachDiskResponse (DetachDiskResponse'),
    newDetachDiskResponse,

    -- ** DetachInstancesFromLoadBalancer
    DetachInstancesFromLoadBalancer (DetachInstancesFromLoadBalancer'),
    newDetachInstancesFromLoadBalancer,
    DetachInstancesFromLoadBalancerResponse (DetachInstancesFromLoadBalancerResponse'),
    newDetachInstancesFromLoadBalancerResponse,

    -- ** DetachStaticIp
    DetachStaticIp (DetachStaticIp'),
    newDetachStaticIp,
    DetachStaticIpResponse (DetachStaticIpResponse'),
    newDetachStaticIpResponse,

    -- ** DisableAddOn
    DisableAddOn (DisableAddOn'),
    newDisableAddOn,
    DisableAddOnResponse (DisableAddOnResponse'),
    newDisableAddOnResponse,

    -- ** DownloadDefaultKeyPair
    DownloadDefaultKeyPair (DownloadDefaultKeyPair'),
    newDownloadDefaultKeyPair,
    DownloadDefaultKeyPairResponse (DownloadDefaultKeyPairResponse'),
    newDownloadDefaultKeyPairResponse,

    -- ** EnableAddOn
    EnableAddOn (EnableAddOn'),
    newEnableAddOn,
    EnableAddOnResponse (EnableAddOnResponse'),
    newEnableAddOnResponse,

    -- ** ExportSnapshot
    ExportSnapshot (ExportSnapshot'),
    newExportSnapshot,
    ExportSnapshotResponse (ExportSnapshotResponse'),
    newExportSnapshotResponse,

    -- ** GetActiveNames (Paginated)
    GetActiveNames (GetActiveNames'),
    newGetActiveNames,
    GetActiveNamesResponse (GetActiveNamesResponse'),
    newGetActiveNamesResponse,

    -- ** GetAlarms
    GetAlarms (GetAlarms'),
    newGetAlarms,
    GetAlarmsResponse (GetAlarmsResponse'),
    newGetAlarmsResponse,

    -- ** GetAutoSnapshots
    GetAutoSnapshots (GetAutoSnapshots'),
    newGetAutoSnapshots,
    GetAutoSnapshotsResponse (GetAutoSnapshotsResponse'),
    newGetAutoSnapshotsResponse,

    -- ** GetBlueprints (Paginated)
    GetBlueprints (GetBlueprints'),
    newGetBlueprints,
    GetBlueprintsResponse (GetBlueprintsResponse'),
    newGetBlueprintsResponse,

    -- ** GetBucketAccessKeys
    GetBucketAccessKeys (GetBucketAccessKeys'),
    newGetBucketAccessKeys,
    GetBucketAccessKeysResponse (GetBucketAccessKeysResponse'),
    newGetBucketAccessKeysResponse,

    -- ** GetBucketBundles
    GetBucketBundles (GetBucketBundles'),
    newGetBucketBundles,
    GetBucketBundlesResponse (GetBucketBundlesResponse'),
    newGetBucketBundlesResponse,

    -- ** GetBucketMetricData
    GetBucketMetricData (GetBucketMetricData'),
    newGetBucketMetricData,
    GetBucketMetricDataResponse (GetBucketMetricDataResponse'),
    newGetBucketMetricDataResponse,

    -- ** GetBuckets
    GetBuckets (GetBuckets'),
    newGetBuckets,
    GetBucketsResponse (GetBucketsResponse'),
    newGetBucketsResponse,

    -- ** GetBundles (Paginated)
    GetBundles (GetBundles'),
    newGetBundles,
    GetBundlesResponse (GetBundlesResponse'),
    newGetBundlesResponse,

    -- ** GetCertificates
    GetCertificates (GetCertificates'),
    newGetCertificates,
    GetCertificatesResponse (GetCertificatesResponse'),
    newGetCertificatesResponse,

    -- ** GetCloudFormationStackRecords (Paginated)
    GetCloudFormationStackRecords (GetCloudFormationStackRecords'),
    newGetCloudFormationStackRecords,
    GetCloudFormationStackRecordsResponse (GetCloudFormationStackRecordsResponse'),
    newGetCloudFormationStackRecordsResponse,

    -- ** GetContactMethods
    GetContactMethods (GetContactMethods'),
    newGetContactMethods,
    GetContactMethodsResponse (GetContactMethodsResponse'),
    newGetContactMethodsResponse,

    -- ** GetContainerAPIMetadata
    GetContainerAPIMetadata (GetContainerAPIMetadata'),
    newGetContainerAPIMetadata,
    GetContainerAPIMetadataResponse (GetContainerAPIMetadataResponse'),
    newGetContainerAPIMetadataResponse,

    -- ** GetContainerImages
    GetContainerImages (GetContainerImages'),
    newGetContainerImages,
    GetContainerImagesResponse (GetContainerImagesResponse'),
    newGetContainerImagesResponse,

    -- ** GetContainerLog
    GetContainerLog (GetContainerLog'),
    newGetContainerLog,
    GetContainerLogResponse (GetContainerLogResponse'),
    newGetContainerLogResponse,

    -- ** GetContainerServiceDeployments
    GetContainerServiceDeployments (GetContainerServiceDeployments'),
    newGetContainerServiceDeployments,
    GetContainerServiceDeploymentsResponse (GetContainerServiceDeploymentsResponse'),
    newGetContainerServiceDeploymentsResponse,

    -- ** GetContainerServiceMetricData
    GetContainerServiceMetricData (GetContainerServiceMetricData'),
    newGetContainerServiceMetricData,
    GetContainerServiceMetricDataResponse (GetContainerServiceMetricDataResponse'),
    newGetContainerServiceMetricDataResponse,

    -- ** GetContainerServicePowers
    GetContainerServicePowers (GetContainerServicePowers'),
    newGetContainerServicePowers,
    GetContainerServicePowersResponse (GetContainerServicePowersResponse'),
    newGetContainerServicePowersResponse,

    -- ** GetContainerServices
    GetContainerServices (GetContainerServices'),
    newGetContainerServices,
    GetContainerServicesResponse (GetContainerServicesResponse'),
    newGetContainerServicesResponse,

    -- ** GetCostEstimate
    GetCostEstimate (GetCostEstimate'),
    newGetCostEstimate,
    GetCostEstimateResponse (GetCostEstimateResponse'),
    newGetCostEstimateResponse,

    -- ** GetDisk
    GetDisk (GetDisk'),
    newGetDisk,
    GetDiskResponse (GetDiskResponse'),
    newGetDiskResponse,

    -- ** GetDiskSnapshot
    GetDiskSnapshot (GetDiskSnapshot'),
    newGetDiskSnapshot,
    GetDiskSnapshotResponse (GetDiskSnapshotResponse'),
    newGetDiskSnapshotResponse,

    -- ** GetDiskSnapshots (Paginated)
    GetDiskSnapshots (GetDiskSnapshots'),
    newGetDiskSnapshots,
    GetDiskSnapshotsResponse (GetDiskSnapshotsResponse'),
    newGetDiskSnapshotsResponse,

    -- ** GetDisks (Paginated)
    GetDisks (GetDisks'),
    newGetDisks,
    GetDisksResponse (GetDisksResponse'),
    newGetDisksResponse,

    -- ** GetDistributionBundles
    GetDistributionBundles (GetDistributionBundles'),
    newGetDistributionBundles,
    GetDistributionBundlesResponse (GetDistributionBundlesResponse'),
    newGetDistributionBundlesResponse,

    -- ** GetDistributionLatestCacheReset
    GetDistributionLatestCacheReset (GetDistributionLatestCacheReset'),
    newGetDistributionLatestCacheReset,
    GetDistributionLatestCacheResetResponse (GetDistributionLatestCacheResetResponse'),
    newGetDistributionLatestCacheResetResponse,

    -- ** GetDistributionMetricData
    GetDistributionMetricData (GetDistributionMetricData'),
    newGetDistributionMetricData,
    GetDistributionMetricDataResponse (GetDistributionMetricDataResponse'),
    newGetDistributionMetricDataResponse,

    -- ** GetDistributions
    GetDistributions (GetDistributions'),
    newGetDistributions,
    GetDistributionsResponse (GetDistributionsResponse'),
    newGetDistributionsResponse,

    -- ** GetDomain
    GetDomain (GetDomain'),
    newGetDomain,
    GetDomainResponse (GetDomainResponse'),
    newGetDomainResponse,

    -- ** GetDomains (Paginated)
    GetDomains (GetDomains'),
    newGetDomains,
    GetDomainsResponse (GetDomainsResponse'),
    newGetDomainsResponse,

    -- ** GetExportSnapshotRecords (Paginated)
    GetExportSnapshotRecords (GetExportSnapshotRecords'),
    newGetExportSnapshotRecords,
    GetExportSnapshotRecordsResponse (GetExportSnapshotRecordsResponse'),
    newGetExportSnapshotRecordsResponse,

    -- ** GetInstance
    GetInstance (GetInstance'),
    newGetInstance,
    GetInstanceResponse (GetInstanceResponse'),
    newGetInstanceResponse,

    -- ** GetInstanceAccessDetails
    GetInstanceAccessDetails (GetInstanceAccessDetails'),
    newGetInstanceAccessDetails,
    GetInstanceAccessDetailsResponse (GetInstanceAccessDetailsResponse'),
    newGetInstanceAccessDetailsResponse,

    -- ** GetInstanceMetricData
    GetInstanceMetricData (GetInstanceMetricData'),
    newGetInstanceMetricData,
    GetInstanceMetricDataResponse (GetInstanceMetricDataResponse'),
    newGetInstanceMetricDataResponse,

    -- ** GetInstancePortStates
    GetInstancePortStates (GetInstancePortStates'),
    newGetInstancePortStates,
    GetInstancePortStatesResponse (GetInstancePortStatesResponse'),
    newGetInstancePortStatesResponse,

    -- ** GetInstanceSnapshot
    GetInstanceSnapshot (GetInstanceSnapshot'),
    newGetInstanceSnapshot,
    GetInstanceSnapshotResponse (GetInstanceSnapshotResponse'),
    newGetInstanceSnapshotResponse,

    -- ** GetInstanceSnapshots (Paginated)
    GetInstanceSnapshots (GetInstanceSnapshots'),
    newGetInstanceSnapshots,
    GetInstanceSnapshotsResponse (GetInstanceSnapshotsResponse'),
    newGetInstanceSnapshotsResponse,

    -- ** GetInstanceState
    GetInstanceState (GetInstanceState'),
    newGetInstanceState,
    GetInstanceStateResponse (GetInstanceStateResponse'),
    newGetInstanceStateResponse,

    -- ** GetInstances (Paginated)
    GetInstances (GetInstances'),
    newGetInstances,
    GetInstancesResponse (GetInstancesResponse'),
    newGetInstancesResponse,

    -- ** GetKeyPair
    GetKeyPair (GetKeyPair'),
    newGetKeyPair,
    GetKeyPairResponse (GetKeyPairResponse'),
    newGetKeyPairResponse,

    -- ** GetKeyPairs (Paginated)
    GetKeyPairs (GetKeyPairs'),
    newGetKeyPairs,
    GetKeyPairsResponse (GetKeyPairsResponse'),
    newGetKeyPairsResponse,

    -- ** GetLoadBalancer
    GetLoadBalancer (GetLoadBalancer'),
    newGetLoadBalancer,
    GetLoadBalancerResponse (GetLoadBalancerResponse'),
    newGetLoadBalancerResponse,

    -- ** GetLoadBalancerMetricData
    GetLoadBalancerMetricData (GetLoadBalancerMetricData'),
    newGetLoadBalancerMetricData,
    GetLoadBalancerMetricDataResponse (GetLoadBalancerMetricDataResponse'),
    newGetLoadBalancerMetricDataResponse,

    -- ** GetLoadBalancerTlsCertificates
    GetLoadBalancerTlsCertificates (GetLoadBalancerTlsCertificates'),
    newGetLoadBalancerTlsCertificates,
    GetLoadBalancerTlsCertificatesResponse (GetLoadBalancerTlsCertificatesResponse'),
    newGetLoadBalancerTlsCertificatesResponse,

    -- ** GetLoadBalancerTlsPolicies
    GetLoadBalancerTlsPolicies (GetLoadBalancerTlsPolicies'),
    newGetLoadBalancerTlsPolicies,
    GetLoadBalancerTlsPoliciesResponse (GetLoadBalancerTlsPoliciesResponse'),
    newGetLoadBalancerTlsPoliciesResponse,

    -- ** GetLoadBalancers (Paginated)
    GetLoadBalancers (GetLoadBalancers'),
    newGetLoadBalancers,
    GetLoadBalancersResponse (GetLoadBalancersResponse'),
    newGetLoadBalancersResponse,

    -- ** GetOperation
    GetOperation (GetOperation'),
    newGetOperation,
    GetOperationResponse (GetOperationResponse'),
    newGetOperationResponse,

    -- ** GetOperations (Paginated)
    GetOperations (GetOperations'),
    newGetOperations,
    GetOperationsResponse (GetOperationsResponse'),
    newGetOperationsResponse,

    -- ** GetOperationsForResource
    GetOperationsForResource (GetOperationsForResource'),
    newGetOperationsForResource,
    GetOperationsForResourceResponse (GetOperationsForResourceResponse'),
    newGetOperationsForResourceResponse,

    -- ** GetRegions
    GetRegions (GetRegions'),
    newGetRegions,
    GetRegionsResponse (GetRegionsResponse'),
    newGetRegionsResponse,

    -- ** GetRelationalDatabase
    GetRelationalDatabase (GetRelationalDatabase'),
    newGetRelationalDatabase,
    GetRelationalDatabaseResponse (GetRelationalDatabaseResponse'),
    newGetRelationalDatabaseResponse,

    -- ** GetRelationalDatabaseBlueprints (Paginated)
    GetRelationalDatabaseBlueprints (GetRelationalDatabaseBlueprints'),
    newGetRelationalDatabaseBlueprints,
    GetRelationalDatabaseBlueprintsResponse (GetRelationalDatabaseBlueprintsResponse'),
    newGetRelationalDatabaseBlueprintsResponse,

    -- ** GetRelationalDatabaseBundles (Paginated)
    GetRelationalDatabaseBundles (GetRelationalDatabaseBundles'),
    newGetRelationalDatabaseBundles,
    GetRelationalDatabaseBundlesResponse (GetRelationalDatabaseBundlesResponse'),
    newGetRelationalDatabaseBundlesResponse,

    -- ** GetRelationalDatabaseEvents (Paginated)
    GetRelationalDatabaseEvents (GetRelationalDatabaseEvents'),
    newGetRelationalDatabaseEvents,
    GetRelationalDatabaseEventsResponse (GetRelationalDatabaseEventsResponse'),
    newGetRelationalDatabaseEventsResponse,

    -- ** GetRelationalDatabaseLogEvents
    GetRelationalDatabaseLogEvents (GetRelationalDatabaseLogEvents'),
    newGetRelationalDatabaseLogEvents,
    GetRelationalDatabaseLogEventsResponse (GetRelationalDatabaseLogEventsResponse'),
    newGetRelationalDatabaseLogEventsResponse,

    -- ** GetRelationalDatabaseLogStreams
    GetRelationalDatabaseLogStreams (GetRelationalDatabaseLogStreams'),
    newGetRelationalDatabaseLogStreams,
    GetRelationalDatabaseLogStreamsResponse (GetRelationalDatabaseLogStreamsResponse'),
    newGetRelationalDatabaseLogStreamsResponse,

    -- ** GetRelationalDatabaseMasterUserPassword
    GetRelationalDatabaseMasterUserPassword (GetRelationalDatabaseMasterUserPassword'),
    newGetRelationalDatabaseMasterUserPassword,
    GetRelationalDatabaseMasterUserPasswordResponse (GetRelationalDatabaseMasterUserPasswordResponse'),
    newGetRelationalDatabaseMasterUserPasswordResponse,

    -- ** GetRelationalDatabaseMetricData
    GetRelationalDatabaseMetricData (GetRelationalDatabaseMetricData'),
    newGetRelationalDatabaseMetricData,
    GetRelationalDatabaseMetricDataResponse (GetRelationalDatabaseMetricDataResponse'),
    newGetRelationalDatabaseMetricDataResponse,

    -- ** GetRelationalDatabaseParameters (Paginated)
    GetRelationalDatabaseParameters (GetRelationalDatabaseParameters'),
    newGetRelationalDatabaseParameters,
    GetRelationalDatabaseParametersResponse (GetRelationalDatabaseParametersResponse'),
    newGetRelationalDatabaseParametersResponse,

    -- ** GetRelationalDatabaseSnapshot
    GetRelationalDatabaseSnapshot (GetRelationalDatabaseSnapshot'),
    newGetRelationalDatabaseSnapshot,
    GetRelationalDatabaseSnapshotResponse (GetRelationalDatabaseSnapshotResponse'),
    newGetRelationalDatabaseSnapshotResponse,

    -- ** GetRelationalDatabaseSnapshots (Paginated)
    GetRelationalDatabaseSnapshots (GetRelationalDatabaseSnapshots'),
    newGetRelationalDatabaseSnapshots,
    GetRelationalDatabaseSnapshotsResponse (GetRelationalDatabaseSnapshotsResponse'),
    newGetRelationalDatabaseSnapshotsResponse,

    -- ** GetRelationalDatabases (Paginated)
    GetRelationalDatabases (GetRelationalDatabases'),
    newGetRelationalDatabases,
    GetRelationalDatabasesResponse (GetRelationalDatabasesResponse'),
    newGetRelationalDatabasesResponse,

    -- ** GetStaticIp
    GetStaticIp (GetStaticIp'),
    newGetStaticIp,
    GetStaticIpResponse (GetStaticIpResponse'),
    newGetStaticIpResponse,

    -- ** GetStaticIps (Paginated)
    GetStaticIps (GetStaticIps'),
    newGetStaticIps,
    GetStaticIpsResponse (GetStaticIpsResponse'),
    newGetStaticIpsResponse,

    -- ** ImportKeyPair
    ImportKeyPair (ImportKeyPair'),
    newImportKeyPair,
    ImportKeyPairResponse (ImportKeyPairResponse'),
    newImportKeyPairResponse,

    -- ** IsVpcPeered
    IsVpcPeered (IsVpcPeered'),
    newIsVpcPeered,
    IsVpcPeeredResponse (IsVpcPeeredResponse'),
    newIsVpcPeeredResponse,

    -- ** OpenInstancePublicPorts
    OpenInstancePublicPorts (OpenInstancePublicPorts'),
    newOpenInstancePublicPorts,
    OpenInstancePublicPortsResponse (OpenInstancePublicPortsResponse'),
    newOpenInstancePublicPortsResponse,

    -- ** PeerVpc
    PeerVpc (PeerVpc'),
    newPeerVpc,
    PeerVpcResponse (PeerVpcResponse'),
    newPeerVpcResponse,

    -- ** PutAlarm
    PutAlarm (PutAlarm'),
    newPutAlarm,
    PutAlarmResponse (PutAlarmResponse'),
    newPutAlarmResponse,

    -- ** PutInstancePublicPorts
    PutInstancePublicPorts (PutInstancePublicPorts'),
    newPutInstancePublicPorts,
    PutInstancePublicPortsResponse (PutInstancePublicPortsResponse'),
    newPutInstancePublicPortsResponse,

    -- ** RebootInstance
    RebootInstance (RebootInstance'),
    newRebootInstance,
    RebootInstanceResponse (RebootInstanceResponse'),
    newRebootInstanceResponse,

    -- ** RebootRelationalDatabase
    RebootRelationalDatabase (RebootRelationalDatabase'),
    newRebootRelationalDatabase,
    RebootRelationalDatabaseResponse (RebootRelationalDatabaseResponse'),
    newRebootRelationalDatabaseResponse,

    -- ** RegisterContainerImage
    RegisterContainerImage (RegisterContainerImage'),
    newRegisterContainerImage,
    RegisterContainerImageResponse (RegisterContainerImageResponse'),
    newRegisterContainerImageResponse,

    -- ** ReleaseStaticIp
    ReleaseStaticIp (ReleaseStaticIp'),
    newReleaseStaticIp,
    ReleaseStaticIpResponse (ReleaseStaticIpResponse'),
    newReleaseStaticIpResponse,

    -- ** ResetDistributionCache
    ResetDistributionCache (ResetDistributionCache'),
    newResetDistributionCache,
    ResetDistributionCacheResponse (ResetDistributionCacheResponse'),
    newResetDistributionCacheResponse,

    -- ** SendContactMethodVerification
    SendContactMethodVerification (SendContactMethodVerification'),
    newSendContactMethodVerification,
    SendContactMethodVerificationResponse (SendContactMethodVerificationResponse'),
    newSendContactMethodVerificationResponse,

    -- ** SetIpAddressType
    SetIpAddressType (SetIpAddressType'),
    newSetIpAddressType,
    SetIpAddressTypeResponse (SetIpAddressTypeResponse'),
    newSetIpAddressTypeResponse,

    -- ** SetResourceAccessForBucket
    SetResourceAccessForBucket (SetResourceAccessForBucket'),
    newSetResourceAccessForBucket,
    SetResourceAccessForBucketResponse (SetResourceAccessForBucketResponse'),
    newSetResourceAccessForBucketResponse,

    -- ** StartGUISession
    StartGUISession (StartGUISession'),
    newStartGUISession,
    StartGUISessionResponse (StartGUISessionResponse'),
    newStartGUISessionResponse,

    -- ** StartInstance
    StartInstance (StartInstance'),
    newStartInstance,
    StartInstanceResponse (StartInstanceResponse'),
    newStartInstanceResponse,

    -- ** StartRelationalDatabase
    StartRelationalDatabase (StartRelationalDatabase'),
    newStartRelationalDatabase,
    StartRelationalDatabaseResponse (StartRelationalDatabaseResponse'),
    newStartRelationalDatabaseResponse,

    -- ** StopGUISession
    StopGUISession (StopGUISession'),
    newStopGUISession,
    StopGUISessionResponse (StopGUISessionResponse'),
    newStopGUISessionResponse,

    -- ** StopInstance
    StopInstance (StopInstance'),
    newStopInstance,
    StopInstanceResponse (StopInstanceResponse'),
    newStopInstanceResponse,

    -- ** StopRelationalDatabase
    StopRelationalDatabase (StopRelationalDatabase'),
    newStopRelationalDatabase,
    StopRelationalDatabaseResponse (StopRelationalDatabaseResponse'),
    newStopRelationalDatabaseResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestAlarm
    TestAlarm (TestAlarm'),
    newTestAlarm,
    TestAlarmResponse (TestAlarmResponse'),
    newTestAlarmResponse,

    -- ** UnpeerVpc
    UnpeerVpc (UnpeerVpc'),
    newUnpeerVpc,
    UnpeerVpcResponse (UnpeerVpcResponse'),
    newUnpeerVpcResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateBucket
    UpdateBucket (UpdateBucket'),
    newUpdateBucket,
    UpdateBucketResponse (UpdateBucketResponse'),
    newUpdateBucketResponse,

    -- ** UpdateBucketBundle
    UpdateBucketBundle (UpdateBucketBundle'),
    newUpdateBucketBundle,
    UpdateBucketBundleResponse (UpdateBucketBundleResponse'),
    newUpdateBucketBundleResponse,

    -- ** UpdateContainerService
    UpdateContainerService (UpdateContainerService'),
    newUpdateContainerService,
    UpdateContainerServiceResponse (UpdateContainerServiceResponse'),
    newUpdateContainerServiceResponse,

    -- ** UpdateDistribution
    UpdateDistribution (UpdateDistribution'),
    newUpdateDistribution,
    UpdateDistributionResponse (UpdateDistributionResponse'),
    newUpdateDistributionResponse,

    -- ** UpdateDistributionBundle
    UpdateDistributionBundle (UpdateDistributionBundle'),
    newUpdateDistributionBundle,
    UpdateDistributionBundleResponse (UpdateDistributionBundleResponse'),
    newUpdateDistributionBundleResponse,

    -- ** UpdateDomainEntry
    UpdateDomainEntry (UpdateDomainEntry'),
    newUpdateDomainEntry,
    UpdateDomainEntryResponse (UpdateDomainEntryResponse'),
    newUpdateDomainEntryResponse,

    -- ** UpdateInstanceMetadataOptions
    UpdateInstanceMetadataOptions (UpdateInstanceMetadataOptions'),
    newUpdateInstanceMetadataOptions,
    UpdateInstanceMetadataOptionsResponse (UpdateInstanceMetadataOptionsResponse'),
    newUpdateInstanceMetadataOptionsResponse,

    -- ** UpdateLoadBalancerAttribute
    UpdateLoadBalancerAttribute (UpdateLoadBalancerAttribute'),
    newUpdateLoadBalancerAttribute,
    UpdateLoadBalancerAttributeResponse (UpdateLoadBalancerAttributeResponse'),
    newUpdateLoadBalancerAttributeResponse,

    -- ** UpdateRelationalDatabase
    UpdateRelationalDatabase (UpdateRelationalDatabase'),
    newUpdateRelationalDatabase,
    UpdateRelationalDatabaseResponse (UpdateRelationalDatabaseResponse'),
    newUpdateRelationalDatabaseResponse,

    -- ** UpdateRelationalDatabaseParameters
    UpdateRelationalDatabaseParameters (UpdateRelationalDatabaseParameters'),
    newUpdateRelationalDatabaseParameters,
    UpdateRelationalDatabaseParametersResponse (UpdateRelationalDatabaseParametersResponse'),
    newUpdateRelationalDatabaseParametersResponse,

    -- * Types

    -- ** AccessDirection
    AccessDirection (..),

    -- ** AccessType
    AccessType (..),

    -- ** AccountLevelBpaSyncStatus
    AccountLevelBpaSyncStatus (..),

    -- ** AddOnType
    AddOnType (..),

    -- ** AlarmState
    AlarmState (..),

    -- ** AppCategory
    AppCategory (..),

    -- ** AutoMountStatus
    AutoMountStatus (..),

    -- ** AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- ** BPAStatusMessage
    BPAStatusMessage (..),

    -- ** BehaviorEnum
    BehaviorEnum (..),

    -- ** BlueprintType
    BlueprintType (..),

    -- ** BucketMetricName
    BucketMetricName (..),

    -- ** CertificateDomainValidationStatus
    CertificateDomainValidationStatus (..),

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

    -- ** Currency
    Currency (..),

    -- ** DiskSnapshotState
    DiskSnapshotState (..),

    -- ** DiskState
    DiskState (..),

    -- ** DistributionMetricName
    DistributionMetricName (..),

    -- ** DnsRecordCreationStateCode
    DnsRecordCreationStateCode (..),

    -- ** ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- ** ForwardValues
    ForwardValues (..),

    -- ** HeaderEnum
    HeaderEnum (..),

    -- ** HttpEndpoint
    HttpEndpoint (..),

    -- ** HttpProtocolIpv6
    HttpProtocolIpv6 (..),

    -- ** HttpTokens
    HttpTokens (..),

    -- ** InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- ** InstanceHealthReason
    InstanceHealthReason (..),

    -- ** InstanceHealthState
    InstanceHealthState (..),

    -- ** InstanceMetadataState
    InstanceMetadataState (..),

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

    -- ** LoadBalancerTlsCertificateDnsRecordCreationStateCode
    LoadBalancerTlsCertificateDnsRecordCreationStateCode (..),

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

    -- ** NameServersUpdateStateCode
    NameServersUpdateStateCode (..),

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

    -- ** PricingUnit
    PricingUnit (..),

    -- ** R53HostedZoneDeletionStateCode
    R53HostedZoneDeletionStateCode (..),

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

    -- ** Status
    Status (..),

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

    -- ** AccountLevelBpaSync
    AccountLevelBpaSync (AccountLevelBpaSync'),
    newAccountLevelBpaSync,

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

    -- ** BucketAccessLogConfig
    BucketAccessLogConfig (BucketAccessLogConfig'),
    newBucketAccessLogConfig,

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

    -- ** ContainerServiceECRImagePullerRole
    ContainerServiceECRImagePullerRole (ContainerServiceECRImagePullerRole'),
    newContainerServiceECRImagePullerRole,

    -- ** ContainerServiceECRImagePullerRoleRequest
    ContainerServiceECRImagePullerRoleRequest (ContainerServiceECRImagePullerRoleRequest'),
    newContainerServiceECRImagePullerRoleRequest,

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

    -- ** CostEstimate
    CostEstimate (CostEstimate'),
    newCostEstimate,

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

    -- ** DnsRecordCreationState
    DnsRecordCreationState (DnsRecordCreationState'),
    newDnsRecordCreationState,

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

    -- ** EstimateByTime
    EstimateByTime (EstimateByTime'),
    newEstimateByTime,

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

    -- ** InstanceMetadataOptions
    InstanceMetadataOptions (InstanceMetadataOptions'),
    newInstanceMetadataOptions,

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

    -- ** LoadBalancerTlsCertificateDnsRecordCreationState
    LoadBalancerTlsCertificateDnsRecordCreationState (LoadBalancerTlsCertificateDnsRecordCreationState'),
    newLoadBalancerTlsCertificateDnsRecordCreationState,

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

    -- ** LoadBalancerTlsPolicy
    LoadBalancerTlsPolicy (LoadBalancerTlsPolicy'),
    newLoadBalancerTlsPolicy,

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

    -- ** NameServersUpdateState
    NameServersUpdateState (NameServersUpdateState'),
    newNameServersUpdateState,

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

    -- ** PrivateRegistryAccess
    PrivateRegistryAccess (PrivateRegistryAccess'),
    newPrivateRegistryAccess,

    -- ** PrivateRegistryAccessRequest
    PrivateRegistryAccessRequest (PrivateRegistryAccessRequest'),
    newPrivateRegistryAccessRequest,

    -- ** QueryStringObject
    QueryStringObject (QueryStringObject'),
    newQueryStringObject,

    -- ** R53HostedZoneDeletionState
    R53HostedZoneDeletionState (R53HostedZoneDeletionState'),
    newR53HostedZoneDeletionState,

    -- ** RegionInfo
    RegionInfo (RegionInfo'),
    newRegionInfo,

    -- ** RegisteredDomainDelegationInfo
    RegisteredDomainDelegationInfo (RegisteredDomainDelegationInfo'),
    newRegisteredDomainDelegationInfo,

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

    -- ** ResourceBudgetEstimate
    ResourceBudgetEstimate (ResourceBudgetEstimate'),
    newResourceBudgetEstimate,

    -- ** ResourceLocation
    ResourceLocation (ResourceLocation'),
    newResourceLocation,

    -- ** ResourceReceivingAccess
    ResourceReceivingAccess (ResourceReceivingAccess'),
    newResourceReceivingAccess,

    -- ** ResourceRecord
    ResourceRecord (ResourceRecord'),
    newResourceRecord,

    -- ** Session
    Session (Session'),
    newSession,

    -- ** StaticIp
    StaticIp (StaticIp'),
    newStaticIp,

    -- ** StopInstanceOnIdleRequest
    StopInstanceOnIdleRequest (StopInstanceOnIdleRequest'),
    newStopInstanceOnIdleRequest,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TimePeriod
    TimePeriod (TimePeriod'),
    newTimePeriod,
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
import Amazonka.Lightsail.CreateGUISessionAccessDetails
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
import Amazonka.Lightsail.GetCostEstimate
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
import Amazonka.Lightsail.GetLoadBalancerTlsPolicies
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
import Amazonka.Lightsail.StartGUISession
import Amazonka.Lightsail.StartInstance
import Amazonka.Lightsail.StartRelationalDatabase
import Amazonka.Lightsail.StopGUISession
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
import Amazonka.Lightsail.UpdateInstanceMetadataOptions
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
