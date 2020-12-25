{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Amazon Lightsail is the easiest way to get started with Amazon Web Services (AWS) for developers who need to build websites or web applications. It includes everything you need to launch your project quickly - instances (virtual private servers), container services, managed databases, SSD-based block storage, static IP addresses, load balancers, content delivery network (CDN) distributions, DNS management of registered domains, and resource snapshots (backups) - for a low, predictable monthly price.
--
-- You can manage your Lightsail resources using the Lightsail console, Lightsail API, AWS Command Line Interface (AWS CLI), or SDKs. For more information about Lightsail concepts and tasks, see the <http://lightsail.aws.amazon.com/ls/docs/how-to/article/lightsail-how-to-set-up-access-keys-to-use-sdk-api-cli Lightsail Dev Guide> .
-- This API Reference provides detailed information about the actions, data types, parameters, and errors of the Lightsail service. For more information about the supported AWS Regions, endpoints, and service quotas of the Lightsail service, see <https://docs.aws.amazon.com/general/latest/gr/lightsail.html Amazon Lightsail Endpoints and Quotas> in the /AWS General Reference/ .
module Network.AWS.Lightsail
  ( -- * Service configuration
    mkServiceConfig,

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
    module Network.AWS.Lightsail.CloseInstancePublicPorts,

    -- ** GetRelationalDatabaseMetricData
    module Network.AWS.Lightsail.GetRelationalDatabaseMetricData,

    -- ** AllocateStaticIp
    module Network.AWS.Lightsail.AllocateStaticIp,

    -- ** DeleteKeyPair
    module Network.AWS.Lightsail.DeleteKeyPair,

    -- ** DeleteInstanceSnapshot
    module Network.AWS.Lightsail.DeleteInstanceSnapshot,

    -- ** GetInstances (Paginated)
    module Network.AWS.Lightsail.GetInstances,

    -- ** GetLoadBalancer
    module Network.AWS.Lightsail.GetLoadBalancer,

    -- ** DisableAddOn
    module Network.AWS.Lightsail.DisableAddOn,

    -- ** GetDistributions
    module Network.AWS.Lightsail.GetDistributions,

    -- ** CreateContainerServiceDeployment
    module Network.AWS.Lightsail.CreateContainerServiceDeployment,

    -- ** GetInstance
    module Network.AWS.Lightsail.GetInstance,

    -- ** GetRelationalDatabaseEvents (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseEvents,

    -- ** AttachCertificateToDistribution
    module Network.AWS.Lightsail.AttachCertificateToDistribution,

    -- ** GetContainerServices
    module Network.AWS.Lightsail.GetContainerServices,

    -- ** UpdateDistributionBundle
    module Network.AWS.Lightsail.UpdateDistributionBundle,

    -- ** GetRelationalDatabaseSnapshots (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseSnapshots,

    -- ** AttachStaticIp
    module Network.AWS.Lightsail.AttachStaticIp,

    -- ** GetRelationalDatabaseParameters (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseParameters,

    -- ** DetachDisk
    module Network.AWS.Lightsail.DetachDisk,

    -- ** GetContactMethods
    module Network.AWS.Lightsail.GetContactMethods,

    -- ** DownloadDefaultKeyPair
    module Network.AWS.Lightsail.DownloadDefaultKeyPair,

    -- ** DeleteLoadBalancerTlsCertificate
    module Network.AWS.Lightsail.DeleteLoadBalancerTlsCertificate,

    -- ** TestAlarm
    module Network.AWS.Lightsail.TestAlarm,

    -- ** GetDomains (Paginated)
    module Network.AWS.Lightsail.GetDomains,

    -- ** GetContainerImages
    module Network.AWS.Lightsail.GetContainerImages,

    -- ** UpdateRelationalDatabaseParameters
    module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters,

    -- ** CreateLoadBalancerTlsCertificate
    module Network.AWS.Lightsail.CreateLoadBalancerTlsCertificate,

    -- ** CreateDomainEntry
    module Network.AWS.Lightsail.CreateDomainEntry,

    -- ** GetContainerServicePowers
    module Network.AWS.Lightsail.GetContainerServicePowers,

    -- ** ImportKeyPair
    module Network.AWS.Lightsail.ImportKeyPair,

    -- ** GetInstanceSnapshots (Paginated)
    module Network.AWS.Lightsail.GetInstanceSnapshots,

    -- ** ExportSnapshot
    module Network.AWS.Lightsail.ExportSnapshot,

    -- ** CreateRelationalDatabaseFromSnapshot
    module Network.AWS.Lightsail.CreateRelationalDatabaseFromSnapshot,

    -- ** CreateCloudFormationStack
    module Network.AWS.Lightsail.CreateCloudFormationStack,

    -- ** GetExportSnapshotRecords (Paginated)
    module Network.AWS.Lightsail.GetExportSnapshotRecords,

    -- ** ReleaseStaticIp
    module Network.AWS.Lightsail.ReleaseStaticIp,

    -- ** DeleteInstance
    module Network.AWS.Lightsail.DeleteInstance,

    -- ** CreateContainerServiceRegistryLogin
    module Network.AWS.Lightsail.CreateContainerServiceRegistryLogin,

    -- ** GetCertificates
    module Network.AWS.Lightsail.GetCertificates,

    -- ** GetContainerServiceMetricData
    module Network.AWS.Lightsail.GetContainerServiceMetricData,

    -- ** GetDistributionMetricData
    module Network.AWS.Lightsail.GetDistributionMetricData,

    -- ** RebootInstance
    module Network.AWS.Lightsail.RebootInstance,

    -- ** DeleteLoadBalancer
    module Network.AWS.Lightsail.DeleteLoadBalancer,

    -- ** CreateDiskFromSnapshot
    module Network.AWS.Lightsail.CreateDiskFromSnapshot,

    -- ** GetRelationalDatabases (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabases,

    -- ** GetInstanceSnapshot
    module Network.AWS.Lightsail.GetInstanceSnapshot,

    -- ** GetRelationalDatabaseLogEvents
    module Network.AWS.Lightsail.GetRelationalDatabaseLogEvents,

    -- ** CreateContactMethod
    module Network.AWS.Lightsail.CreateContactMethod,

    -- ** GetRelationalDatabaseLogStreams
    module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams,

    -- ** GetDomain
    module Network.AWS.Lightsail.GetDomain,

    -- ** GetAutoSnapshots
    module Network.AWS.Lightsail.GetAutoSnapshots,

    -- ** GetActiveNames (Paginated)
    module Network.AWS.Lightsail.GetActiveNames,

    -- ** DeleteContactMethod
    module Network.AWS.Lightsail.DeleteContactMethod,

    -- ** CreateDistribution
    module Network.AWS.Lightsail.CreateDistribution,

    -- ** StopRelationalDatabase
    module Network.AWS.Lightsail.StopRelationalDatabase,

    -- ** CreateRelationalDatabaseSnapshot
    module Network.AWS.Lightsail.CreateRelationalDatabaseSnapshot,

    -- ** DetachCertificateFromDistribution
    module Network.AWS.Lightsail.DetachCertificateFromDistribution,

    -- ** CreateContainerService
    module Network.AWS.Lightsail.CreateContainerService,

    -- ** GetInstanceAccessDetails
    module Network.AWS.Lightsail.GetInstanceAccessDetails,

    -- ** EnableAddOn
    module Network.AWS.Lightsail.EnableAddOn,

    -- ** StopInstance
    module Network.AWS.Lightsail.StopInstance,

    -- ** DetachInstancesFromLoadBalancer
    module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer,

    -- ** RegisterContainerImage
    module Network.AWS.Lightsail.RegisterContainerImage,

    -- ** CreateCertificate
    module Network.AWS.Lightsail.CreateCertificate,

    -- ** CreateInstanceSnapshot
    module Network.AWS.Lightsail.CreateInstanceSnapshot,

    -- ** CopySnapshot
    module Network.AWS.Lightsail.CopySnapshot,

    -- ** GetRelationalDatabaseSnapshot
    module Network.AWS.Lightsail.GetRelationalDatabaseSnapshot,

    -- ** IsVpcPeered
    module Network.AWS.Lightsail.IsVpcPeered,

    -- ** GetStaticIps (Paginated)
    module Network.AWS.Lightsail.GetStaticIps,

    -- ** UnpeerVpc
    module Network.AWS.Lightsail.UnpeerVpc,

    -- ** DeleteDisk
    module Network.AWS.Lightsail.DeleteDisk,

    -- ** CreateInstancesFromSnapshot
    module Network.AWS.Lightsail.CreateInstancesFromSnapshot,

    -- ** GetCloudFormationStackRecords (Paginated)
    module Network.AWS.Lightsail.GetCloudFormationStackRecords,

    -- ** CreateDomain
    module Network.AWS.Lightsail.CreateDomain,

    -- ** GetRelationalDatabaseBlueprints (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseBlueprints,

    -- ** DeleteCertificate
    module Network.AWS.Lightsail.DeleteCertificate,

    -- ** GetDiskSnapshots (Paginated)
    module Network.AWS.Lightsail.GetDiskSnapshots,

    -- ** GetContainerAPIMetadata
    module Network.AWS.Lightsail.GetContainerAPIMetadata,

    -- ** PeerVpc
    module Network.AWS.Lightsail.PeerVpc,

    -- ** GetRelationalDatabaseBundles (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseBundles,

    -- ** GetLoadBalancers (Paginated)
    module Network.AWS.Lightsail.GetLoadBalancers,

    -- ** RebootRelationalDatabase
    module Network.AWS.Lightsail.RebootRelationalDatabase,

    -- ** AttachLoadBalancerTlsCertificate
    module Network.AWS.Lightsail.AttachLoadBalancerTlsCertificate,

    -- ** UpdateLoadBalancerAttribute
    module Network.AWS.Lightsail.UpdateLoadBalancerAttribute,

    -- ** DeleteRelationalDatabase
    module Network.AWS.Lightsail.DeleteRelationalDatabase,

    -- ** GetDiskSnapshot
    module Network.AWS.Lightsail.GetDiskSnapshot,

    -- ** UpdateRelationalDatabase
    module Network.AWS.Lightsail.UpdateRelationalDatabase,

    -- ** GetStaticIp
    module Network.AWS.Lightsail.GetStaticIp,

    -- ** GetRelationalDatabaseMasterUserPassword
    module Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword,

    -- ** GetBlueprints (Paginated)
    module Network.AWS.Lightsail.GetBlueprints,

    -- ** PutAlarm
    module Network.AWS.Lightsail.PutAlarm,

    -- ** DeleteAlarm
    module Network.AWS.Lightsail.DeleteAlarm,

    -- ** GetInstancePortStates
    module Network.AWS.Lightsail.GetInstancePortStates,

    -- ** DeleteAutoSnapshot
    module Network.AWS.Lightsail.DeleteAutoSnapshot,

    -- ** CreateRelationalDatabase
    module Network.AWS.Lightsail.CreateRelationalDatabase,

    -- ** SendContactMethodVerification
    module Network.AWS.Lightsail.SendContactMethodVerification,

    -- ** GetContainerLog
    module Network.AWS.Lightsail.GetContainerLog,

    -- ** CreateDiskSnapshot
    module Network.AWS.Lightsail.CreateDiskSnapshot,

    -- ** DeleteDomainEntry
    module Network.AWS.Lightsail.DeleteDomainEntry,

    -- ** UpdateDomainEntry
    module Network.AWS.Lightsail.UpdateDomainEntry,

    -- ** GetRegions
    module Network.AWS.Lightsail.GetRegions,

    -- ** DeleteDiskSnapshot
    module Network.AWS.Lightsail.DeleteDiskSnapshot,

    -- ** GetLoadBalancerMetricData
    module Network.AWS.Lightsail.GetLoadBalancerMetricData,

    -- ** GetInstanceState
    module Network.AWS.Lightsail.GetInstanceState,

    -- ** GetKeyPairs (Paginated)
    module Network.AWS.Lightsail.GetKeyPairs,

    -- ** GetOperations (Paginated)
    module Network.AWS.Lightsail.GetOperations,

    -- ** GetDisks (Paginated)
    module Network.AWS.Lightsail.GetDisks,

    -- ** GetRelationalDatabase
    module Network.AWS.Lightsail.GetRelationalDatabase,

    -- ** AttachInstancesToLoadBalancer
    module Network.AWS.Lightsail.AttachInstancesToLoadBalancer,

    -- ** TagResource
    module Network.AWS.Lightsail.TagResource,

    -- ** GetOperation
    module Network.AWS.Lightsail.GetOperation,

    -- ** ResetDistributionCache
    module Network.AWS.Lightsail.ResetDistributionCache,

    -- ** UpdateDistribution
    module Network.AWS.Lightsail.UpdateDistribution,

    -- ** DeleteDistribution
    module Network.AWS.Lightsail.DeleteDistribution,

    -- ** UpdateContainerService
    module Network.AWS.Lightsail.UpdateContainerService,

    -- ** DeleteRelationalDatabaseSnapshot
    module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot,

    -- ** DeleteContainerService
    module Network.AWS.Lightsail.DeleteContainerService,

    -- ** GetInstanceMetricData
    module Network.AWS.Lightsail.GetInstanceMetricData,

    -- ** GetKeyPair
    module Network.AWS.Lightsail.GetKeyPair,

    -- ** UntagResource
    module Network.AWS.Lightsail.UntagResource,

    -- ** PutInstancePublicPorts
    module Network.AWS.Lightsail.PutInstancePublicPorts,

    -- ** GetDistributionBundles
    module Network.AWS.Lightsail.GetDistributionBundles,

    -- ** GetDisk
    module Network.AWS.Lightsail.GetDisk,

    -- ** GetDistributionLatestCacheReset
    module Network.AWS.Lightsail.GetDistributionLatestCacheReset,

    -- ** CreateLoadBalancer
    module Network.AWS.Lightsail.CreateLoadBalancer,

    -- ** GetContainerServiceDeployments
    module Network.AWS.Lightsail.GetContainerServiceDeployments,

    -- ** DeleteKnownHostKeys
    module Network.AWS.Lightsail.DeleteKnownHostKeys,

    -- ** AttachDisk
    module Network.AWS.Lightsail.AttachDisk,

    -- ** DetachStaticIp
    module Network.AWS.Lightsail.DetachStaticIp,

    -- ** CreateInstances
    module Network.AWS.Lightsail.CreateInstances,

    -- ** GetAlarms
    module Network.AWS.Lightsail.GetAlarms,

    -- ** OpenInstancePublicPorts
    module Network.AWS.Lightsail.OpenInstancePublicPorts,

    -- ** StartRelationalDatabase
    module Network.AWS.Lightsail.StartRelationalDatabase,

    -- ** DeleteContainerImage
    module Network.AWS.Lightsail.DeleteContainerImage,

    -- ** GetBundles (Paginated)
    module Network.AWS.Lightsail.GetBundles,

    -- ** DeleteDomain
    module Network.AWS.Lightsail.DeleteDomain,

    -- ** GetLoadBalancerTlsCertificates
    module Network.AWS.Lightsail.GetLoadBalancerTlsCertificates,

    -- ** CreateDisk
    module Network.AWS.Lightsail.CreateDisk,

    -- ** GetOperationsForResource
    module Network.AWS.Lightsail.GetOperationsForResource,

    -- ** CreateKeyPair
    module Network.AWS.Lightsail.CreateKeyPair,

    -- ** StartInstance
    module Network.AWS.Lightsail.StartInstance,

    -- * Types

    -- ** AlarmState
    AlarmState (..),

    -- ** HeaderEnum
    HeaderEnum (..),

    -- ** DomainEntryType
    DomainEntryType (..),

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (..),
    mkPendingMaintenanceAction,
    pmaAction,
    pmaCurrentApplyDate,
    pmaDescription,

    -- ** LoadBalancerProtocol
    LoadBalancerProtocol (..),

    -- ** Origin
    Origin (..),
    mkOrigin,
    oName,
    oProtocolPolicy,
    oRegionName,
    oResourceType,

    -- ** CloudFormationStackRecordSourceType
    CloudFormationStackRecordSourceType (..),

    -- ** ContainerServiceDeploymentState
    ContainerServiceDeploymentState (..),

    -- ** IpV6Address
    IpV6Address (..),

    -- ** AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- ** BehaviorEnum
    BehaviorEnum (..),

    -- ** Blueprint
    Blueprint (..),
    mkBlueprint,
    bBlueprintId,
    bDescription,
    bGroup,
    bIsActive,
    bLicenseUrl,
    bMinPower,
    bName,
    bPlatform,
    bProductUrl,
    bType,
    bVersion,
    bVersionCode,

    -- ** InstancePortState
    InstancePortState (..),
    mkInstancePortState,
    ipsCidrListAliases,
    ipsCidrs,
    ipsFromPort,
    ipsProtocol,
    ipsState,
    ipsToPort,

    -- ** InstanceNetworking
    InstanceNetworking (..),
    mkInstanceNetworking,
    inMonthlyTransfer,
    inPorts,

    -- ** IpAddress
    IpAddress (..),

    -- ** ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrName,
    rrType,
    rrValue,

    -- ** RegionName
    RegionName (..),

    -- ** DiskSnapshotInfo
    DiskSnapshotInfo (..),
    mkDiskSnapshotInfo,
    dsiSizeInGb,

    -- ** InstanceSnapshotState
    InstanceSnapshotState (..),

    -- ** Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- ** TreatMissingData
    TreatMissingData (..),

    -- ** ContainerService
    ContainerService (..),
    mkContainerService,
    csArn,
    csContainerServiceName,
    csCreatedAt,
    csCurrentDeployment,
    csIsDisabled,
    csLocation,
    csNextDeployment,
    csPower,
    csPowerId,
    csPrincipalArn,
    csPrivateDomainName,
    csPublicDomainNames,
    csResourceType,
    csScale,
    csState,
    csTags,
    csUrl,

    -- ** CertificateName
    CertificateName (..),

    -- ** ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    mkContainerServiceHealthCheckConfig,
    cshccHealthyThreshold,
    cshccIntervalSeconds,
    cshccPath,
    cshccSuccessCodes,
    cshccTimeoutSeconds,
    cshccUnhealthyThreshold,

    -- ** RecordState
    RecordState (..),

    -- ** RelationalDatabaseEvent
    RelationalDatabaseEvent (..),
    mkRelationalDatabaseEvent,
    rdeCreatedAt,
    rdeEventCategories,
    rdeMessage,
    rdeResource,

    -- ** AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest (..),
    mkAutoSnapshotAddOnRequest,
    asaorSnapshotTimeOfDay,

    -- ** HeaderObject
    HeaderObject (..),
    mkHeaderObject,
    hoHeadersAllowList,
    hoOption,

    -- ** ResourceType
    ResourceType (..),

    -- ** DestinationInfo
    DestinationInfo (..),
    mkDestinationInfo,
    diId,
    diService,

    -- ** QueryStringObject
    QueryStringObject (..),
    mkQueryStringObject,
    qsoOption,
    qsoQueryStringsAllowList,

    -- ** RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    mkRelationalDatabaseSnapshot,
    rdsArn,
    rdsCreatedAt,
    rdsEngine,
    rdsEngineVersion,
    rdsFromRelationalDatabaseArn,
    rdsFromRelationalDatabaseBlueprintId,
    rdsFromRelationalDatabaseBundleId,
    rdsFromRelationalDatabaseName,
    rdsLocation,
    rdsName,
    rdsResourceType,
    rdsSizeInGb,
    rdsState,
    rdsSupportCode,
    rdsTags,

    -- ** ContainerServiceLogEvent
    ContainerServiceLogEvent (..),
    mkContainerServiceLogEvent,
    csleCreatedAt,
    csleMessage,

    -- ** ContainerServicePowerName
    ContainerServicePowerName (..),

    -- ** PortAccessType
    PortAccessType (..),

    -- ** Disk
    Disk (..),
    mkDisk,
    dAddOns,
    dArn,
    dAttachedTo,
    dAttachmentState,
    dCreatedAt,
    dGbInUse,
    dIops,
    dIsAttached,
    dIsSystemDisk,
    dLocation,
    dName,
    dPath,
    dResourceType,
    dSizeInGb,
    dState,
    dSupportCode,
    dTags,

    -- ** EligibleToRenew
    EligibleToRenew (..),

    -- ** ContainerServiceState
    ContainerServiceState (..),

    -- ** ResourceName
    ResourceName (..),

    -- ** RelationalDatabaseMetricName
    RelationalDatabaseMetricName (..),

    -- ** KeyPair
    KeyPair (..),
    mkKeyPair,
    kpArn,
    kpCreatedAt,
    kpFingerprint,
    kpLocation,
    kpName,
    kpResourceType,
    kpSupportCode,
    kpTags,

    -- ** RequestFailureReason
    RequestFailureReason (..),

    -- ** Operation
    Operation (..),
    mkOperation,
    ofCreatedAt,
    ofErrorCode,
    ofErrorDetails,
    ofId,
    ofIsTerminal,
    ofLocation,
    ofOperationDetails,
    ofOperationType,
    ofResourceName,
    ofResourceType,
    ofStatus,
    ofStatusChangedAt,

    -- ** String
    String (..),

    -- ** ContainerName
    ContainerName (..),

    -- ** MetricDatapoint
    MetricDatapoint (..),
    mkMetricDatapoint,
    mdAverage,
    mdMaximum,
    mdMinimum,
    mdSampleCount,
    mdSum,
    mdTimestamp,
    mdUnit,

    -- ** LogEvent
    LogEvent (..),
    mkLogEvent,
    leCreatedAt,
    leMessage,

    -- ** RelationalDatabaseHardware
    RelationalDatabaseHardware (..),
    mkRelationalDatabaseHardware,
    rdhCpuCount,
    rdhDiskSizeInGb,
    rdhRamSizeInGb,

    -- ** NetworkProtocol
    NetworkProtocol (..),

    -- ** PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    mkPendingModifiedRelationalDatabaseValues,
    pmrdvBackupRetentionEnabled,
    pmrdvEngineVersion,
    pmrdvMasterUserPassword,

    -- ** LoadBalancerTlsCertificateRevocationReason
    LoadBalancerTlsCertificateRevocationReason (..),

    -- ** Domain
    Domain (..),
    mkDomain,
    dfArn,
    dfCreatedAt,
    dfDomainEntries,
    dfLocation,
    dfName,
    dfResourceType,
    dfSupportCode,
    dfTags,

    -- ** ContainerImage
    ContainerImage (..),
    mkContainerImage,
    ciCreatedAt,
    ciDigest,
    ciImage,

    -- ** AddOnType
    AddOnType (..),

    -- ** CacheBehavior
    CacheBehavior (..),
    mkCacheBehavior,
    cbBehavior,

    -- ** MetricName
    MetricName (..),

    -- ** DiskInfo
    DiskInfo (..),
    mkDiskInfo,
    diIsSystemDisk,
    diName,
    diPath,
    diSizeInGb,

    -- ** LightsailDistribution
    LightsailDistribution (..),
    mkLightsailDistribution,
    ldAbleToUpdateBundle,
    ldAlternativeDomainNames,
    ldArn,
    ldBundleId,
    ldCacheBehaviorSettings,
    ldCacheBehaviors,
    ldCertificateName,
    ldCreatedAt,
    ldDefaultCacheBehavior,
    ldDomainName,
    ldIsEnabled,
    ldLocation,
    ldName,
    ldOrigin,
    ldOriginPublicDNS,
    ldResourceType,
    ldStatus,
    ldSupportCode,
    ldTags,

    -- ** ForwardValues
    ForwardValues (..),

    -- ** InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- ** DiskSnapshotState
    DiskSnapshotState (..),

    -- ** InstancePlatform
    InstancePlatform (..),

    -- ** ContactProtocol
    ContactProtocol (..),

    -- ** DistributionBundle
    DistributionBundle (..),
    mkDistributionBundle,
    dbBundleId,
    dbIsActive,
    dbName,
    dbPrice,
    dbTransferPerMonthInGb,

    -- ** MonthlyTransfer
    MonthlyTransfer (..),
    mkMonthlyTransfer,
    mtGbPerMonthAllocated,

    -- ** LoadBalancerAttributeName
    LoadBalancerAttributeName (..),

    -- ** ContainerServiceMetricName
    ContainerServiceMetricName (..),

    -- ** OperationStatus
    OperationStatus (..),

    -- ** ComparisonOperator
    ComparisonOperator (..),

    -- ** DistributionMetricName
    DistributionMetricName (..),

    -- ** AutoSnapshotDetails
    AutoSnapshotDetails (..),
    mkAutoSnapshotDetails,
    asdCreatedAt,
    asdDate,
    asdFromAttachedDisks,
    asdStatus,

    -- ** DiskMap
    DiskMap (..),
    mkDiskMap,
    dmNewDiskName,
    dmOriginalDiskPath,

    -- ** RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    mkRelationalDatabaseBlueprint,
    rdbBlueprintId,
    rdbEngine,
    rdbEngineDescription,
    rdbEngineVersion,
    rdbEngineVersionDescription,
    rdbIsEngineDefault,

    -- ** AddOnRequest
    AddOnRequest (..),
    mkAddOnRequest,
    aorAddOnType,
    aorAutoSnapshotAddOnRequest,

    -- ** DiskSnapshot
    DiskSnapshot (..),
    mkDiskSnapshot,
    dsArn,
    dsCreatedAt,
    dsFromDiskArn,
    dsFromDiskName,
    dsFromInstanceArn,
    dsFromInstanceName,
    dsIsFromAutoSnapshot,
    dsLocation,
    dsName,
    dsProgress,
    dsResourceType,
    dsSizeInGb,
    dsState,
    dsSupportCode,
    dsTags,

    -- ** StaticIp
    StaticIp (..),
    mkStaticIp,
    siArn,
    siAttachedTo,
    siCreatedAt,
    siIpAddress,
    siIsAttached,
    siLocation,
    siName,
    siResourceType,
    siSupportCode,

    -- ** SensitiveString
    SensitiveString (..),

    -- ** CertificateSummary
    CertificateSummary (..),
    mkCertificateSummary,
    cCertificateArn,
    cCertificateDetail,
    cCertificateName,
    cDomainName,
    cTags,

    -- ** CloudFormationStackRecord
    CloudFormationStackRecord (..),
    mkCloudFormationStackRecord,
    cfsrArn,
    cfsrCreatedAt,
    cfsrDestinationInfo,
    cfsrLocation,
    cfsrName,
    cfsrResourceType,
    cfsrSourceInfo,
    cfsrState,

    -- ** AccessDirection
    AccessDirection (..),

    -- ** ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    mkContainerServiceRegistryLogin,
    csrlExpiresAt,
    csrlPassword,
    csrlRegistry,
    csrlUsername,

    -- ** LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (..),
    mkLoadBalancerTlsCertificateDomainValidationOption,
    lbtcdvoDomainName,
    lbtcdvoValidationStatus,

    -- ** DomainEntry
    DomainEntry (..),
    mkDomainEntry,
    deId,
    deIsAlias,
    deName,
    deOptions,
    deTarget,
    deType,

    -- ** ContainerServiceProtocol
    ContainerServiceProtocol (..),

    -- ** Bundle
    Bundle (..),
    mkBundle,
    bfBundleId,
    bfCpuCount,
    bfDiskSizeInGb,
    bfInstanceType,
    bfIsActive,
    bfName,
    bfPower,
    bfPrice,
    bfRamSizeInGb,
    bfSupportedPlatforms,
    bfTransferPerMonthInGb,

    -- ** RevocationReason
    RevocationReason (..),

    -- ** LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (..),
    mkLoadBalancerTlsCertificate,
    lbtcArn,
    lbtcCreatedAt,
    lbtcDomainName,
    lbtcDomainValidationRecords,
    lbtcFailureReason,
    lbtcIsAttached,
    lbtcIssuedAt,
    lbtcIssuer,
    lbtcKeyAlgorithm,
    lbtcLoadBalancerName,
    lbtcLocation,
    lbtcName,
    lbtcNotAfter,
    lbtcNotBefore,
    lbtcRenewalSummary,
    lbtcResourceType,
    lbtcRevocationReason,
    lbtcRevokedAt,
    lbtcSerial,
    lbtcSignatureAlgorithm,
    lbtcStatus,
    lbtcSubject,
    lbtcSubjectAlternativeNames,
    lbtcSupportCode,
    lbtcTags,

    -- ** NonEmptyString
    NonEmptyString (..),

    -- ** PortInfo
    PortInfo (..),
    mkPortInfo,
    piCidrListAliases,
    piCidrs,
    piFromPort,
    piProtocol,
    piToPort,

    -- ** ResourceArn
    ResourceArn (..),

    -- ** DomainName
    DomainName (..),

    -- ** InstanceHardware
    InstanceHardware (..),
    mkInstanceHardware,
    ihCpuCount,
    ihDisks,
    ihRamSizeInGb,

    -- ** AttachedDisk
    AttachedDisk (..),
    mkAttachedDisk,
    adPath,
    adSizeInGb,

    -- ** RelationalDatabaseParameter
    RelationalDatabaseParameter (..),
    mkRelationalDatabaseParameter,
    rdpAllowedValues,
    rdpApplyMethod,
    rdpApplyType,
    rdpDataType,
    rdpDescription,
    rdpIsModifiable,
    rdpParameterName,
    rdpParameterValue,

    -- ** ResourceLocation
    ResourceLocation (..),
    mkResourceLocation,
    rlAvailabilityZone,
    rlRegionName,

    -- ** PasswordData
    PasswordData (..),
    mkPasswordData,
    pdCiphertext,
    pdKeyPairName,

    -- ** Base64
    Base64 (..),

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azState,
    azZoneName,

    -- ** InstanceAccessDetails
    InstanceAccessDetails (..),
    mkInstanceAccessDetails,
    iadCertKey,
    iadExpiresAt,
    iadHostKeys,
    iadInstanceName,
    iadIpAddress,
    iadPassword,
    iadPasswordData,
    iadPrivateKey,
    iadProtocol,
    iadUsername,

    -- ** ContactMethod
    ContactMethod (..),
    mkContactMethod,
    cmArn,
    cmContactEndpoint,
    cmCreatedAt,
    cmLocation,
    cmName,
    cmProtocol,
    cmResourceType,
    cmStatus,
    cmSupportCode,

    -- ** BlueprintType
    BlueprintType (..),

    -- ** Container
    Container (..),
    mkContainer,
    cCommand,
    cEnvironment,
    cImage,
    cPorts,

    -- ** MetricUnit
    MetricUnit (..),

    -- ** LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbArn,
    lbConfigurationOptions,
    lbCreatedAt,
    lbDnsName,
    lbHealthCheckPath,
    lbInstanceHealthSummary,
    lbInstancePort,
    lbLocation,
    lbName,
    lbProtocol,
    lbPublicPorts,
    lbResourceType,
    lbState,
    lbSupportCode,
    lbTags,
    lbTlsCertificateSummaries,

    -- ** MetricStatistic
    MetricStatistic (..),

    -- ** DiskState
    DiskState (..),

    -- ** InstanceMetricName
    InstanceMetricName (..),

    -- ** EndpointRequest
    EndpointRequest (..),
    mkEndpointRequest,
    erContainerName,
    erContainerPort,
    erHealthCheck,

    -- ** RenewalStatus
    RenewalStatus (..),

    -- ** PortInfoSourceType
    PortInfoSourceType (..),

    -- ** RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    mkRelationalDatabaseBundle,
    rdbBundleId,
    rdbCpuCount,
    rdbDiskSizeInGb,
    rdbIsActive,
    rdbIsEncrypted,
    rdbName,
    rdbPrice,
    rdbRamSizeInGb,
    rdbTransferPerMonthInGb,

    -- ** CacheBehaviorPerPath
    CacheBehaviorPerPath (..),
    mkCacheBehaviorPerPath,
    cbppBehavior,
    cbppPath,

    -- ** InputOrigin
    InputOrigin (..),
    mkInputOrigin,
    ioName,
    ioProtocolPolicy,
    ioRegionName,

    -- ** RenewalSummary
    RenewalSummary (..),
    mkRenewalSummary,
    rsDomainValidationRecords,
    rsRenewalStatus,
    rsRenewalStatusReason,
    rsUpdatedAt,

    -- ** ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (..),
    mkContainerServiceDeploymentRequest,
    csdrContainers,
    csdrPublicEndpoint,

    -- ** InstanceHealthState
    InstanceHealthState (..),

    -- ** LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (..),
    mkLoadBalancerTlsCertificateDomainValidationRecord,
    lbtcdvrDomainName,
    lbtcdvrName,
    lbtcdvrType,
    lbtcdvrValidationStatus,
    lbtcdvrValue,

    -- ** CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    mkCloudFormationStackRecordSourceInfo,
    cfsrsiArn,
    cfsrsiName,
    cfsrsiResourceType,

    -- ** Certificate
    Certificate (..),
    mkCertificate,
    cfArn,
    cfCreatedAt,
    cfDomainName,
    cfDomainValidationRecords,
    cfEligibleToRenew,
    cfInUseResourceCount,
    cfIssuedAt,
    cfIssuerCA,
    cfKeyAlgorithm,
    cfName,
    cfNotAfter,
    cfNotBefore,
    cfRenewalSummary,
    cfRequestFailureReason,
    cfRevocationReason,
    cfRevokedAt,
    cfSerialNumber,
    cfStatus,
    cfSubjectAlternativeNames,
    cfSupportCode,
    cfTags,

    -- ** InstanceEntry
    InstanceEntry (..),
    mkInstanceEntry,
    ieSourceName,
    ieInstanceType,
    iePortInfoSource,
    ieAvailabilityZone,
    ieUserData,

    -- ** LoadBalancerState
    LoadBalancerState (..),

    -- ** OperationType
    OperationType (..),

    -- ** ContainerServiceName
    ContainerServiceName (..),

    -- ** ExportSnapshotRecord
    ExportSnapshotRecord (..),
    mkExportSnapshotRecord,
    esrArn,
    esrCreatedAt,
    esrDestinationInfo,
    esrLocation,
    esrName,
    esrResourceType,
    esrSourceInfo,
    esrState,

    -- ** InstanceSnapshot
    InstanceSnapshot (..),
    mkInstanceSnapshot,
    isArn,
    isCreatedAt,
    isFromAttachedDisks,
    isFromBlueprintId,
    isFromBundleId,
    isFromInstanceArn,
    isFromInstanceName,
    isIsFromAutoSnapshot,
    isLocation,
    isName,
    isProgress,
    isResourceType,
    isSizeInGb,
    isState,
    isSupportCode,
    isTags,

    -- ** ContainerServicePower
    ContainerServicePower (..),
    mkContainerServicePower,
    cspCpuCount,
    cspIsActive,
    cspName,
    cspPowerId,
    cspPrice,
    cspRamSizeInGb,

    -- ** RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (..),
    mkRelationalDatabaseEndpoint,
    rdeAddress,
    rdePort,

    -- ** TagKey
    TagKey (..),

    -- ** RegionInfo
    RegionInfo (..),
    mkRegionInfo,
    riAvailabilityZones,
    riContinentCode,
    riDescription,
    riDisplayName,
    riName,
    riRelationalDatabaseAvailabilityZones,

    -- ** ContactMethodVerificationProtocol
    ContactMethodVerificationProtocol (..),

    -- ** HostKeyAttributes
    HostKeyAttributes (..),
    mkHostKeyAttributes,
    hkaAlgorithm,
    hkaFingerprintSHA1,
    hkaFingerprintSHA256,
    hkaNotValidAfter,
    hkaNotValidBefore,
    hkaPublicKey,
    hkaWitnessedAt,

    -- ** DomainEntryOptionsKeys
    DomainEntryOptionsKeys (..),

    -- ** KeyAlgorithm
    KeyAlgorithm (..),

    -- ** RelationalDatabaseEngine
    RelationalDatabaseEngine (..),

    -- ** LoadBalancerTlsCertificateDomainStatus
    LoadBalancerTlsCertificateDomainStatus (..),

    -- ** LoadBalancerTlsCertificateSummary
    LoadBalancerTlsCertificateSummary (..),
    mkLoadBalancerTlsCertificateSummary,
    lbtcsIsAttached,
    lbtcsName,

    -- ** LoadBalancerTlsCertificateFailureReason
    LoadBalancerTlsCertificateFailureReason (..),

    -- ** ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- ** TimeOfDay
    TimeOfDay (..),

    -- ** LoadBalancerTlsCertificateStatus
    LoadBalancerTlsCertificateStatus (..),

    -- ** AddOn
    AddOn (..),
    mkAddOn,
    aoName,
    aoNextSnapshotTimeOfDay,
    aoSnapshotTimeOfDay,
    aoStatus,

    -- ** ContainerServiceDeployment
    ContainerServiceDeployment (..),
    mkContainerServiceDeployment,
    csdContainers,
    csdCreatedAt,
    csdPublicEndpoint,
    csdState,
    csdVersion,

    -- ** InstanceSnapshotInfo
    InstanceSnapshotInfo (..),
    mkInstanceSnapshotInfo,
    isiFromBlueprintId,
    isiFromBundleId,
    isiFromDiskInfo,

    -- ** SerialNumber
    SerialNumber (..),

    -- ** ContactMethodStatus
    ContactMethodStatus (..),

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aArn,
    aComparisonOperator,
    aContactProtocols,
    aCreatedAt,
    aDatapointsToAlarm,
    aEvaluationPeriods,
    aLocation,
    aMetricName,
    aMonitoredResourceInfo,
    aName,
    aNotificationEnabled,
    aNotificationTriggers,
    aPeriod,
    aResourceType,
    aState,
    aStatistic,
    aSupportCode,
    aThreshold,
    aTreatMissingData,
    aUnit,

    -- ** RelationalDatabasePasswordVersion
    RelationalDatabasePasswordVersion (..),

    -- ** PortState
    PortState (..),

    -- ** RenewalStatusReason
    RenewalStatusReason (..),

    -- ** InstancePortInfo
    InstancePortInfo (..),
    mkInstancePortInfo,
    ipiAccessDirection,
    ipiAccessFrom,
    ipiAccessType,
    ipiCidrListAliases,
    ipiCidrs,
    ipiCommonName,
    ipiFromPort,
    ipiProtocol,
    ipiToPort,

    -- ** DomainValidationRecord
    DomainValidationRecord (..),
    mkDomainValidationRecord,
    dvrDomainName,
    dvrResourceRecord,

    -- ** RelationalDatabase
    RelationalDatabase (..),
    mkRelationalDatabase,
    rdArn,
    rdBackupRetentionEnabled,
    rdCaCertificateIdentifier,
    rdCreatedAt,
    rdEngine,
    rdEngineVersion,
    rdHardware,
    rdLatestRestorableTime,
    rdLocation,
    rdMasterDatabaseName,
    rdMasterEndpoint,
    rdMasterUsername,
    rdName,
    rdParameterApplyStatus,
    rdPendingMaintenanceActions,
    rdPendingModifiedValues,
    rdPreferredBackupWindow,
    rdPreferredMaintenanceWindow,
    rdPubliclyAccessible,
    rdRelationalDatabaseBlueprintId,
    rdRelationalDatabaseBundleId,
    rdResourceType,
    rdSecondaryAvailabilityZone,
    rdState,
    rdSupportCode,
    rdTags,

    -- ** LoadBalancerTlsCertificateRenewalSummary
    LoadBalancerTlsCertificateRenewalSummary (..),
    mkLoadBalancerTlsCertificateRenewalSummary,
    lbtcrsDomainValidationOptions,
    lbtcrsRenewalStatus,

    -- ** ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    mkExportSnapshotRecordSourceInfo,
    esrsiArn,
    esrsiCreatedAt,
    esrsiDiskSnapshotInfo,
    esrsiFromResourceArn,
    esrsiFromResourceName,
    esrsiInstanceSnapshotInfo,
    esrsiName,
    esrsiResourceType,

    -- ** LoadBalancerTlsCertificateRenewalStatus
    LoadBalancerTlsCertificateRenewalStatus (..),

    -- ** InstanceState
    InstanceState (..),
    mkInstanceState,
    isfCode,
    isfName,

    -- ** CertificateStatus
    CertificateStatus (..),

    -- ** IssuerCA
    IssuerCA (..),

    -- ** CacheSettings
    CacheSettings (..),
    mkCacheSettings,
    csAllowedHTTPMethods,
    csCachedHTTPMethods,
    csDefaultTTL,
    csForwardedCookies,
    csForwardedHeaders,
    csForwardedQueryStrings,
    csMaximumTTL,
    csMinimumTTL,

    -- ** InstanceHealthReason
    InstanceHealthReason (..),

    -- ** Instance
    Instance (..),
    mkInstance,
    iAddOns,
    iArn,
    iBlueprintId,
    iBlueprintName,
    iBundleId,
    iCreatedAt,
    iHardware,
    iIpv6Address,
    iIsStaticIp,
    iLocation,
    iName,
    iNetworking,
    iPrivateIpAddress,
    iPublicIpAddress,
    iResourceType,
    iSshKeyName,
    iState,
    iSupportCode,
    iTags,
    iUsername,

    -- ** MonitoredResourceInfo
    MonitoredResourceInfo (..),
    mkMonitoredResourceInfo,
    mriArn,
    mriName,
    mriResourceType,

    -- ** ContainerServiceEndpoint
    ContainerServiceEndpoint (..),
    mkContainerServiceEndpoint,
    cseContainerName,
    cseContainerPort,
    cseHealthCheck,

    -- ** OriginProtocolPolicyEnum
    OriginProtocolPolicyEnum (..),

    -- ** CookieObject
    CookieObject (..),
    mkCookieObject,
    coCookiesAllowList,
    coOption,

    -- ** LoadBalancerMetricName
    LoadBalancerMetricName (..),

    -- ** InstanceHealthSummary
    InstanceHealthSummary (..),
    mkInstanceHealthSummary,
    ihsInstanceHealth,
    ihsInstanceHealthReason,
    ihsInstanceName,

    -- ** LoadBalancerName
    LoadBalancerName (..),

    -- ** NextPageToken
    NextPageToken (..),

    -- ** ServiceName
    ServiceName (..),

    -- ** Action
    Action (..),

    -- ** Description
    Description (..),

    -- ** Name
    Name (..),

    -- ** RelationalDatabaseName
    RelationalDatabaseName (..),

    -- ** PageToken
    PageToken (..),

    -- ** Message
    Message (..),

    -- ** AlarmName
    AlarmName (..),

    -- ** DistributionName
    DistributionName (..),

    -- ** RelationalDatabaseSnapshotName
    RelationalDatabaseSnapshotName (..),

    -- ** Status
    Status (..),

    -- ** BlueprintId
    BlueprintId (..),

    -- ** Group
    Group (..),

    -- ** LicenseUrl
    LicenseUrl (..),

    -- ** ProductUrl
    ProductUrl (..),

    -- ** Version
    Version (..),

    -- ** VersionCode
    VersionCode (..),

    -- ** Type
    Type (..),

    -- ** Value
    Value (..),

    -- ** BundleId
    BundleId (..),

    -- ** Key
    Key (..),

    -- ** DiskName
    DiskName (..),

    -- ** DiskSnapshotName
    DiskSnapshotName (..),

    -- ** RestoreDate
    RestoreDate (..),

    -- ** SourceDiskName
    SourceDiskName (..),

    -- ** NextBackwardToken
    NextBackwardToken (..),

    -- ** NextForwardToken
    NextForwardToken (..),

    -- ** Arn
    Arn (..),

    -- ** PowerId
    PowerId (..),

    -- ** PrincipalArn
    PrincipalArn (..),

    -- ** PrivateDomainName
    PrivateDomainName (..),

    -- ** Url
    Url (..),

    -- ** Path
    Path (..),

    -- ** SuccessCodes
    SuccessCodes (..),

    -- ** InstanceSnapshotName
    InstanceSnapshotName (..),

    -- ** Resource
    Resource (..),

    -- ** SnapshotTimeOfDay
    SnapshotTimeOfDay (..),

    -- ** Id
    Id (..),

    -- ** Service
    Service (..),

    -- ** Engine
    Engine (..),

    -- ** EngineVersion
    EngineVersion (..),

    -- ** FromRelationalDatabaseArn
    FromRelationalDatabaseArn (..),

    -- ** FromRelationalDatabaseBlueprintId
    FromRelationalDatabaseBlueprintId (..),

    -- ** FromRelationalDatabaseBundleId
    FromRelationalDatabaseBundleId (..),

    -- ** FromRelationalDatabaseName
    FromRelationalDatabaseName (..),

    -- ** State
    State (..),

    -- ** SupportCode
    SupportCode (..),

    -- ** LogStreamName
    LogStreamName (..),

    -- ** ContactEndpoint
    ContactEndpoint (..),

    -- ** AttachedTo
    AttachedTo (..),

    -- ** AttachmentState
    AttachmentState (..),

    -- ** Fingerprint
    Fingerprint (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** ErrorDetails
    ErrorDetails (..),

    -- ** OperationDetails
    OperationDetails (..),

    -- ** Label
    Label (..),

    -- ** MasterUserPassword
    MasterUserPassword (..),

    -- ** Date
    Date (..),

    -- ** PrivateKeyBase64
    PrivateKeyBase64 (..),

    -- ** PublicKeyBase64
    PublicKeyBase64 (..),

    -- ** AttributeValue
    AttributeValue (..),

    -- ** OriginalDiskPath
    OriginalDiskPath (..),

    -- ** FromDiskArn
    FromDiskArn (..),

    -- ** FromInstanceArn
    FromInstanceArn (..),

    -- ** CertificateArn
    CertificateArn (..),

    -- ** Issuer
    Issuer (..),

    -- ** Serial
    Serial (..),

    -- ** SignatureAlgorithm
    SignatureAlgorithm (..),

    -- ** Subject
    Subject (..),

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.UTCTime,
    Lude.NominalDiffTime,
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
import qualified Network.AWS.Prelude as Lude

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
