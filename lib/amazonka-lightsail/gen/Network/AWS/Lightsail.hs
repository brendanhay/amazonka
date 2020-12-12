{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
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
    lightsailService,

    -- * Errors
    -- $errors

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CloseInstancePublicPorts
    module Network.AWS.Lightsail.CloseInstancePublicPorts,

    -- ** GetRelationalDatabaseMetricData
    module Network.AWS.Lightsail.GetRelationalDatabaseMetricData,

    -- ** AllocateStaticIP
    module Network.AWS.Lightsail.AllocateStaticIP,

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

    -- ** AttachStaticIP
    module Network.AWS.Lightsail.AttachStaticIP,

    -- ** GetRelationalDatabaseParameters (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseParameters,

    -- ** DetachDisk
    module Network.AWS.Lightsail.DetachDisk,

    -- ** GetContactMethods
    module Network.AWS.Lightsail.GetContactMethods,

    -- ** DownloadDefaultKeyPair
    module Network.AWS.Lightsail.DownloadDefaultKeyPair,

    -- ** DeleteLoadBalancerTLSCertificate
    module Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate,

    -- ** TestAlarm
    module Network.AWS.Lightsail.TestAlarm,

    -- ** GetDomains (Paginated)
    module Network.AWS.Lightsail.GetDomains,

    -- ** GetContainerImages
    module Network.AWS.Lightsail.GetContainerImages,

    -- ** UpdateRelationalDatabaseParameters
    module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters,

    -- ** CreateLoadBalancerTLSCertificate
    module Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate,

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

    -- ** ReleaseStaticIP
    module Network.AWS.Lightsail.ReleaseStaticIP,

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

    -- ** IsVPCPeered
    module Network.AWS.Lightsail.IsVPCPeered,

    -- ** GetStaticIPs (Paginated)
    module Network.AWS.Lightsail.GetStaticIPs,

    -- ** UnpeerVPC
    module Network.AWS.Lightsail.UnpeerVPC,

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

    -- ** PeerVPC
    module Network.AWS.Lightsail.PeerVPC,

    -- ** GetRelationalDatabaseBundles (Paginated)
    module Network.AWS.Lightsail.GetRelationalDatabaseBundles,

    -- ** GetLoadBalancers (Paginated)
    module Network.AWS.Lightsail.GetLoadBalancers,

    -- ** RebootRelationalDatabase
    module Network.AWS.Lightsail.RebootRelationalDatabase,

    -- ** AttachLoadBalancerTLSCertificate
    module Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate,

    -- ** UpdateLoadBalancerAttribute
    module Network.AWS.Lightsail.UpdateLoadBalancerAttribute,

    -- ** DeleteRelationalDatabase
    module Network.AWS.Lightsail.DeleteRelationalDatabase,

    -- ** GetDiskSnapshot
    module Network.AWS.Lightsail.GetDiskSnapshot,

    -- ** UpdateRelationalDatabase
    module Network.AWS.Lightsail.UpdateRelationalDatabase,

    -- ** GetStaticIP
    module Network.AWS.Lightsail.GetStaticIP,

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

    -- ** DetachStaticIP
    module Network.AWS.Lightsail.DetachStaticIP,

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

    -- ** GetLoadBalancerTLSCertificates
    module Network.AWS.Lightsail.GetLoadBalancerTLSCertificates,

    -- ** CreateDisk
    module Network.AWS.Lightsail.CreateDisk,

    -- ** GetOperationsForResource
    module Network.AWS.Lightsail.GetOperationsForResource,

    -- ** CreateKeyPair
    module Network.AWS.Lightsail.CreateKeyPair,

    -- ** StartInstance
    module Network.AWS.Lightsail.StartInstance,

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

    -- ** LoadBalancerAttributeName
    LoadBalancerAttributeName (..),

    -- ** LoadBalancerMetricName
    LoadBalancerMetricName (..),

    -- ** LoadBalancerProtocol
    LoadBalancerProtocol (..),

    -- ** LoadBalancerState
    LoadBalancerState (..),

    -- ** LoadBalancerTLSCertificateDomainStatus
    LoadBalancerTLSCertificateDomainStatus (..),

    -- ** LoadBalancerTLSCertificateFailureReason
    LoadBalancerTLSCertificateFailureReason (..),

    -- ** LoadBalancerTLSCertificateRenewalStatus
    LoadBalancerTLSCertificateRenewalStatus (..),

    -- ** LoadBalancerTLSCertificateRevocationReason
    LoadBalancerTLSCertificateRevocationReason (..),

    -- ** LoadBalancerTLSCertificateStatus
    LoadBalancerTLSCertificateStatus (..),

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
    AddOn (..),
    mkAddOn,
    aoStatus,
    aoNextSnapshotTimeOfDay,
    aoSnapshotTimeOfDay,
    aoName,

    -- ** AddOnRequest
    AddOnRequest (..),
    mkAddOnRequest,
    aorAutoSnapshotAddOnRequest,
    aorAddOnType,

    -- ** Alarm
    Alarm (..),
    mkAlarm,
    aState,
    aTreatMissingData,
    aResourceType,
    aArn,
    aCreatedAt,
    aLocation,
    aContactProtocols,
    aPeriod,
    aEvaluationPeriods,
    aMetricName,
    aComparisonOperator,
    aName,
    aThreshold,
    aDatapointsToAlarm,
    aSupportCode,
    aNotificationEnabled,
    aNotificationTriggers,
    aStatistic,
    aUnit,
    aMonitoredResourceInfo,

    -- ** AttachedDisk
    AttachedDisk (..),
    mkAttachedDisk,
    adPath,
    adSizeInGb,

    -- ** AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest (..),
    mkAutoSnapshotAddOnRequest,
    asaorSnapshotTimeOfDay,

    -- ** AutoSnapshotDetails
    AutoSnapshotDetails (..),
    mkAutoSnapshotDetails,
    asdStatus,
    asdFromAttachedDisks,
    asdCreatedAt,
    asdDate,

    -- ** AvailabilityZone
    AvailabilityZone (..),
    mkAvailabilityZone,
    azState,
    azZoneName,

    -- ** Blueprint
    Blueprint (..),
    mkBlueprint,
    bVersionCode,
    bPlatform,
    bGroup,
    bMinPower,
    bProductURL,
    bLicenseURL,
    bName,
    bVersion,
    bBlueprintId,
    bType,
    bIsActive,
    bDescription,

    -- ** Bundle
    Bundle (..),
    mkBundle,
    bunCpuCount,
    bunTransferPerMonthInGb,
    bunBundleId,
    bunInstanceType,
    bunName,
    bunPower,
    bunDiskSizeInGb,
    bunSupportedPlatforms,
    bunPrice,
    bunIsActive,
    bunRamSizeInGb,

    -- ** CacheBehavior
    CacheBehavior (..),
    mkCacheBehavior,
    cbBehavior,

    -- ** CacheBehaviorPerPath
    CacheBehaviorPerPath (..),
    mkCacheBehaviorPerPath,
    cbppPath,
    cbppBehavior,

    -- ** CacheSettings
    CacheSettings (..),
    mkCacheSettings,
    csMaximumTTL,
    csCachedHTTPMethods,
    csForwardedCookies,
    csAllowedHTTPMethods,
    csDefaultTTL,
    csMinimumTTL,
    csForwardedHeaders,
    csForwardedQueryStrings,

    -- ** Certificate
    Certificate (..),
    mkCertificate,
    cerStatus,
    cerSubjectAlternativeNames,
    cerArn,
    cerCreatedAt,
    cerEligibleToRenew,
    cerRequestFailureReason,
    cerRevokedAt,
    cerNotBefore,
    cerRevocationReason,
    cerDomainName,
    cerName,
    cerRenewalSummary,
    cerSupportCode,
    cerDomainValidationRecords,
    cerInUseResourceCount,
    cerIssuedAt,
    cerKeyAlgorithm,
    cerSerialNumber,
    cerIssuerCA,
    cerTags,
    cerNotAfter,

    -- ** CertificateSummary
    CertificateSummary (..),
    mkCertificateSummary,
    cCertificateDetail,
    cCertificateName,
    cCertificateARN,
    cDomainName,
    cTags,

    -- ** CloudFormationStackRecord
    CloudFormationStackRecord (..),
    mkCloudFormationStackRecord,
    cfsrState,
    cfsrDestinationInfo,
    cfsrResourceType,
    cfsrArn,
    cfsrCreatedAt,
    cfsrLocation,
    cfsrName,
    cfsrSourceInfo,

    -- ** CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    mkCloudFormationStackRecordSourceInfo,
    cfsrsiResourceType,
    cfsrsiArn,
    cfsrsiName,

    -- ** ContactMethod
    ContactMethod (..),
    mkContactMethod,
    cmStatus,
    cmResourceType,
    cmArn,
    cmCreatedAt,
    cmLocation,
    cmProtocol,
    cmName,
    cmSupportCode,
    cmContactEndpoint,

    -- ** Container
    Container (..),
    mkContainer,
    cImage,
    cCommand,
    cEnvironment,
    cPorts,

    -- ** ContainerImage
    ContainerImage (..),
    mkContainerImage,
    ciImage,
    ciCreatedAt,
    ciDigest,

    -- ** ContainerService
    ContainerService (..),
    mkContainerService,
    csState,
    csPowerId,
    csResourceType,
    csArn,
    csCreatedAt,
    csLocation,
    csScale,
    csUrl,
    csNextDeployment,
    csPrincipalARN,
    csPower,
    csPrivateDomainName,
    csIsDisabled,
    csPublicDomainNames,
    csContainerServiceName,
    csCurrentDeployment,
    csTags,

    -- ** ContainerServiceDeployment
    ContainerServiceDeployment (..),
    mkContainerServiceDeployment,
    csdState,
    csdPublicEndpoint,
    csdCreatedAt,
    csdContainers,
    csdVersion,

    -- ** ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (..),
    mkContainerServiceDeploymentRequest,
    csdrPublicEndpoint,
    csdrContainers,

    -- ** ContainerServiceEndpoint
    ContainerServiceEndpoint (..),
    mkContainerServiceEndpoint,
    cseHealthCheck,
    cseContainerName,
    cseContainerPort,

    -- ** ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    mkContainerServiceHealthCheckConfig,
    cshccHealthyThreshold,
    cshccPath,
    cshccSuccessCodes,
    cshccIntervalSeconds,
    cshccTimeoutSeconds,
    cshccUnhealthyThreshold,

    -- ** ContainerServiceLogEvent
    ContainerServiceLogEvent (..),
    mkContainerServiceLogEvent,
    csleCreatedAt,
    csleMessage,

    -- ** ContainerServicePower
    ContainerServicePower (..),
    mkContainerServicePower,
    cspPowerId,
    cspCpuCount,
    cspName,
    cspPrice,
    cspIsActive,
    cspRamSizeInGb,

    -- ** ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    mkContainerServiceRegistryLogin,
    csrlExpiresAt,
    csrlUsername,
    csrlPassword,
    csrlRegistry,

    -- ** CookieObject
    CookieObject (..),
    mkCookieObject,
    coCookiesAllowList,
    coOption,

    -- ** DestinationInfo
    DestinationInfo (..),
    mkDestinationInfo,
    diService,
    diId,

    -- ** Disk
    Disk (..),
    mkDisk,
    dState,
    dResourceType,
    dArn,
    dPath,
    dCreatedAt,
    dLocation,
    dIops,
    dIsAttached,
    dAddOns,
    dAttachmentState,
    dName,
    dSizeInGb,
    dSupportCode,
    dIsSystemDisk,
    dAttachedTo,
    dGbInUse,
    dTags,

    -- ** DiskInfo
    DiskInfo (..),
    mkDiskInfo,
    diPath,
    diName,
    diSizeInGb,
    diIsSystemDisk,

    -- ** DiskMap
    DiskMap (..),
    mkDiskMap,
    dmNewDiskName,
    dmOriginalDiskPath,

    -- ** DiskSnapshot
    DiskSnapshot (..),
    mkDiskSnapshot,
    dsFromDiskName,
    dsIsFromAutoSnapshot,
    dsState,
    dsResourceType,
    dsArn,
    dsCreatedAt,
    dsLocation,
    dsProgress,
    dsName,
    dsSizeInGb,
    dsSupportCode,
    dsFromInstanceARN,
    dsFromInstanceName,
    dsFromDiskARN,
    dsTags,

    -- ** DiskSnapshotInfo
    DiskSnapshotInfo (..),
    mkDiskSnapshotInfo,
    dsiSizeInGb,

    -- ** DistributionBundle
    DistributionBundle (..),
    mkDistributionBundle,
    dbTransferPerMonthInGb,
    dbBundleId,
    dbName,
    dbPrice,
    dbIsActive,

    -- ** Domain
    Domain (..),
    mkDomain,
    domResourceType,
    domDomainEntries,
    domArn,
    domCreatedAt,
    domLocation,
    domName,
    domSupportCode,
    domTags,

    -- ** DomainEntry
    DomainEntry (..),
    mkDomainEntry,
    deIsAlias,
    deName,
    deId,
    deOptions,
    deType,
    deTarget,

    -- ** DomainValidationRecord
    DomainValidationRecord (..),
    mkDomainValidationRecord,
    dvrResourceRecord,
    dvrDomainName,

    -- ** EndpointRequest
    EndpointRequest (..),
    mkEndpointRequest,
    erHealthCheck,
    erContainerName,
    erContainerPort,

    -- ** ExportSnapshotRecord
    ExportSnapshotRecord (..),
    mkExportSnapshotRecord,
    esrState,
    esrDestinationInfo,
    esrResourceType,
    esrArn,
    esrCreatedAt,
    esrLocation,
    esrName,
    esrSourceInfo,

    -- ** ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    mkExportSnapshotRecordSourceInfo,
    esrsiDiskSnapshotInfo,
    esrsiResourceType,
    esrsiArn,
    esrsiCreatedAt,
    esrsiFromResourceARN,
    esrsiName,
    esrsiInstanceSnapshotInfo,
    esrsiFromResourceName,

    -- ** HeaderObject
    HeaderObject (..),
    mkHeaderObject,
    hoHeadersAllowList,
    hoOption,

    -- ** HostKeyAttributes
    HostKeyAttributes (..),
    mkHostKeyAttributes,
    hkaNotValidAfter,
    hkaNotValidBefore,
    hkaFingerprintSHA1,
    hkaPublicKey,
    hkaAlgorithm,
    hkaWitnessedAt,
    hkaFingerprintSHA256,

    -- ** InputOrigin
    InputOrigin (..),
    mkInputOrigin,
    ioRegionName,
    ioName,
    ioProtocolPolicy,

    -- ** Instance
    Instance (..),
    mkInstance,
    iState,
    iIpv6Address,
    iResourceType,
    iArn,
    iCreatedAt,
    iLocation,
    iSshKeyName,
    iAddOns,
    iUsername,
    iNetworking,
    iBundleId,
    iName,
    iSupportCode,
    iBlueprintId,
    iPrivateIPAddress,
    iBlueprintName,
    iIsStaticIP,
    iPublicIPAddress,
    iHardware,
    iTags,

    -- ** InstanceAccessDetails
    InstanceAccessDetails (..),
    mkInstanceAccessDetails,
    iadHostKeys,
    iadCertKey,
    iadIpAddress,
    iadPrivateKey,
    iadExpiresAt,
    iadUsername,
    iadProtocol,
    iadPasswordData,
    iadPassword,
    iadInstanceName,

    -- ** InstanceEntry
    InstanceEntry (..),
    mkInstanceEntry,
    ieUserData,
    ieSourceName,
    ieInstanceType,
    iePortInfoSource,
    ieAvailabilityZone,

    -- ** InstanceHardware
    InstanceHardware (..),
    mkInstanceHardware,
    ihCpuCount,
    ihDisks,
    ihRamSizeInGb,

    -- ** InstanceHealthSummary
    InstanceHealthSummary (..),
    mkInstanceHealthSummary,
    ihsInstanceHealth,
    ihsInstanceName,
    ihsInstanceHealthReason,

    -- ** InstanceNetworking
    InstanceNetworking (..),
    mkInstanceNetworking,
    inMonthlyTransfer,
    inPorts,

    -- ** InstancePortInfo
    InstancePortInfo (..),
    mkInstancePortInfo,
    ipiFromPort,
    ipiCidrs,
    ipiCommonName,
    ipiProtocol,
    ipiCidrListAliases,
    ipiAccessDirection,
    ipiAccessType,
    ipiToPort,
    ipiAccessFrom,

    -- ** InstancePortState
    InstancePortState (..),
    mkInstancePortState,
    ipsFromPort,
    ipsCidrs,
    ipsState,
    ipsProtocol,
    ipsCidrListAliases,
    ipsToPort,

    -- ** InstanceSnapshot
    InstanceSnapshot (..),
    mkInstanceSnapshot,
    insFromBlueprintId,
    insIsFromAutoSnapshot,
    insState,
    insResourceType,
    insFromAttachedDisks,
    insArn,
    insCreatedAt,
    insLocation,
    insProgress,
    insName,
    insFromBundleId,
    insSizeInGb,
    insSupportCode,
    insFromInstanceARN,
    insFromInstanceName,
    insTags,

    -- ** InstanceSnapshotInfo
    InstanceSnapshotInfo (..),
    mkInstanceSnapshotInfo,
    isiFromBlueprintId,
    isiFromBundleId,
    isiFromDiskInfo,

    -- ** InstanceState
    InstanceState (..),
    mkInstanceState,
    isName,
    isCode,

    -- ** KeyPair
    KeyPair (..),
    mkKeyPair,
    kpResourceType,
    kpArn,
    kpCreatedAt,
    kpLocation,
    kpFingerprint,
    kpName,
    kpSupportCode,
    kpTags,

    -- ** LightsailDistribution
    LightsailDistribution (..),
    mkLightsailDistribution,
    ldStatus,
    ldOrigin,
    ldCertificateName,
    ldResourceType,
    ldArn,
    ldCreatedAt,
    ldLocation,
    ldCacheBehaviorSettings,
    ldAlternativeDomainNames,
    ldBundleId,
    ldAbleToUpdateBundle,
    ldOriginPublicDNS,
    ldDomainName,
    ldName,
    ldIsEnabled,
    ldSupportCode,
    ldDefaultCacheBehavior,
    ldCacheBehaviors,
    ldTags,

    -- ** LoadBalancer
    LoadBalancer (..),
    mkLoadBalancer,
    lbHealthCheckPath,
    lbState,
    lbResourceType,
    lbArn,
    lbCreatedAt,
    lbLocation,
    lbInstancePort,
    lbConfigurationOptions,
    lbProtocol,
    lbTlsCertificateSummaries,
    lbName,
    lbSupportCode,
    lbPublicPorts,
    lbDnsName,
    lbInstanceHealthSummary,
    lbTags,

    -- ** LoadBalancerTLSCertificate
    LoadBalancerTLSCertificate (..),
    mkLoadBalancerTLSCertificate,
    lbtcFailureReason,
    lbtcSubject,
    lbtcStatus,
    lbtcSubjectAlternativeNames,
    lbtcResourceType,
    lbtcArn,
    lbtcCreatedAt,
    lbtcLocation,
    lbtcLoadBalancerName,
    lbtcSerial,
    lbtcIsAttached,
    lbtcRevokedAt,
    lbtcNotBefore,
    lbtcRevocationReason,
    lbtcDomainName,
    lbtcName,
    lbtcRenewalSummary,
    lbtcSupportCode,
    lbtcDomainValidationRecords,
    lbtcIssuedAt,
    lbtcKeyAlgorithm,
    lbtcSignatureAlgorithm,
    lbtcIssuer,
    lbtcTags,
    lbtcNotAfter,

    -- ** LoadBalancerTLSCertificateDomainValidationOption
    LoadBalancerTLSCertificateDomainValidationOption (..),
    mkLoadBalancerTLSCertificateDomainValidationOption,
    lbtcdvoDomainName,
    lbtcdvoValidationStatus,

    -- ** LoadBalancerTLSCertificateDomainValidationRecord
    LoadBalancerTLSCertificateDomainValidationRecord (..),
    mkLoadBalancerTLSCertificateDomainValidationRecord,
    lbtcdvrValue,
    lbtcdvrDomainName,
    lbtcdvrName,
    lbtcdvrValidationStatus,
    lbtcdvrType,

    -- ** LoadBalancerTLSCertificateRenewalSummary
    LoadBalancerTLSCertificateRenewalSummary (..),
    mkLoadBalancerTLSCertificateRenewalSummary,
    lbtcrsRenewalStatus,
    lbtcrsDomainValidationOptions,

    -- ** LoadBalancerTLSCertificateSummary
    LoadBalancerTLSCertificateSummary (..),
    mkLoadBalancerTLSCertificateSummary,
    lbtcsIsAttached,
    lbtcsName,

    -- ** LogEvent
    LogEvent (..),
    mkLogEvent,
    leCreatedAt,
    leMessage,

    -- ** MetricDatapoint
    MetricDatapoint (..),
    mkMetricDatapoint,
    mdSampleCount,
    mdMaximum,
    mdAverage,
    mdMinimum,
    mdSum,
    mdTimestamp,
    mdUnit,

    -- ** MonitoredResourceInfo
    MonitoredResourceInfo (..),
    mkMonitoredResourceInfo,
    mriResourceType,
    mriArn,
    mriName,

    -- ** MonthlyTransfer
    MonthlyTransfer (..),
    mkMonthlyTransfer,
    mtGbPerMonthAllocated,

    -- ** Operation
    Operation (..),
    mkOperation,
    opeStatus,
    opeOperationDetails,
    opeResourceType,
    opeCreatedAt,
    opeResourceName,
    opeLocation,
    opeStatusChangedAt,
    opeErrorDetails,
    opeErrorCode,
    opeId,
    opeOperationType,
    opeIsTerminal,

    -- ** Origin
    Origin (..),
    mkOrigin,
    oRegionName,
    oResourceType,
    oName,
    oProtocolPolicy,

    -- ** PasswordData
    PasswordData (..),
    mkPasswordData,
    pdKeyPairName,
    pdCiphertext,

    -- ** PendingMaintenanceAction
    PendingMaintenanceAction (..),
    mkPendingMaintenanceAction,
    pmaAction,
    pmaDescription,
    pmaCurrentApplyDate,

    -- ** PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    mkPendingModifiedRelationalDatabaseValues,
    pmrdvEngineVersion,
    pmrdvMasterUserPassword,
    pmrdvBackupRetentionEnabled,

    -- ** PortInfo
    PortInfo (..),
    mkPortInfo,
    piFromPort,
    piCidrs,
    piProtocol,
    piCidrListAliases,
    piToPort,

    -- ** QueryStringObject
    QueryStringObject (..),
    mkQueryStringObject,
    qsoQueryStringsAllowList,
    qsoOption,

    -- ** RegionInfo
    RegionInfo (..),
    mkRegionInfo,
    riAvailabilityZones,
    riName,
    riRelationalDatabaseAvailabilityZones,
    riDisplayName,
    riContinentCode,
    riDescription,

    -- ** RelationalDatabase
    RelationalDatabase (..),
    mkRelationalDatabase,
    rdEngineVersion,
    rdRelationalDatabaseBundleId,
    rdMasterEndpoint,
    rdState,
    rdResourceType,
    rdPubliclyAccessible,
    rdMasterUsername,
    rdArn,
    rdCreatedAt,
    rdLocation,
    rdEngine,
    rdLatestRestorableTime,
    rdPreferredMaintenanceWindow,
    rdRelationalDatabaseBlueprintId,
    rdCaCertificateIdentifier,
    rdName,
    rdBackupRetentionEnabled,
    rdPreferredBackupWindow,
    rdPendingMaintenanceActions,
    rdSupportCode,
    rdSecondaryAvailabilityZone,
    rdPendingModifiedValues,
    rdMasterDatabaseName,
    rdHardware,
    rdParameterApplyStatus,
    rdTags,

    -- ** RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    mkRelationalDatabaseBlueprint,
    rdbEngineVersion,
    rdbIsEngineDefault,
    rdbEngineVersionDescription,
    rdbEngine,
    rdbBlueprintId,
    rdbEngineDescription,

    -- ** RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    mkRelationalDatabaseBundle,
    rdbIsEncrypted,
    rdbCpuCount,
    rdbTransferPerMonthInGb,
    rdbBundleId,
    rdbName,
    rdbDiskSizeInGb,
    rdbPrice,
    rdbIsActive,
    rdbRamSizeInGb,

    -- ** RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (..),
    mkRelationalDatabaseEndpoint,
    rdeAddress,
    rdePort,

    -- ** RelationalDatabaseEvent
    RelationalDatabaseEvent (..),
    mkRelationalDatabaseEvent,
    rdeCreatedAt,
    rdeEventCategories,
    rdeResource,
    rdeMessage,

    -- ** RelationalDatabaseHardware
    RelationalDatabaseHardware (..),
    mkRelationalDatabaseHardware,
    rdhCpuCount,
    rdhDiskSizeInGb,
    rdhRamSizeInGb,

    -- ** RelationalDatabaseParameter
    RelationalDatabaseParameter (..),
    mkRelationalDatabaseParameter,
    rdpApplyType,
    rdpParameterValue,
    rdpApplyMethod,
    rdpDataType,
    rdpIsModifiable,
    rdpAllowedValues,
    rdpParameterName,
    rdpDescription,

    -- ** RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    mkRelationalDatabaseSnapshot,
    rdsEngineVersion,
    rdsState,
    rdsFromRelationalDatabaseName,
    rdsResourceType,
    rdsFromRelationalDatabaseBlueprintId,
    rdsArn,
    rdsCreatedAt,
    rdsLocation,
    rdsEngine,
    rdsName,
    rdsSizeInGb,
    rdsSupportCode,
    rdsFromRelationalDatabaseARN,
    rdsFromRelationalDatabaseBundleId,
    rdsTags,

    -- ** RenewalSummary
    RenewalSummary (..),
    mkRenewalSummary,
    rsRenewalStatus,
    rsDomainValidationRecords,
    rsUpdatedAt,
    rsRenewalStatusReason,

    -- ** ResourceLocation
    ResourceLocation (..),
    mkResourceLocation,
    rlRegionName,
    rlAvailabilityZone,

    -- ** ResourceRecord
    ResourceRecord (..),
    mkResourceRecord,
    rrValue,
    rrName,
    rrType,

    -- ** StaticIP
    StaticIP (..),
    mkStaticIP,
    siIpAddress,
    siResourceType,
    siArn,
    siCreatedAt,
    siLocation,
    siIsAttached,
    siName,
    siSupportCode,
    siAttachedTo,

    -- ** Tag
    Tag (..),
    mkTag,
    tValue,
    tKey,

    -- * Serialization types
    Lude.Base64 (..),
    Lude._Base64,
    Lude.Sensitive (..),
    Lude._Sensitive,
    Lude.Time (..),
    Lude._Time,
    Lude.DateTime,
    Lude.Timestamp,
  )
where

import Network.AWS.Lightsail.AllocateStaticIP
import Network.AWS.Lightsail.AttachCertificateToDistribution
import Network.AWS.Lightsail.AttachDisk
import Network.AWS.Lightsail.AttachInstancesToLoadBalancer
import Network.AWS.Lightsail.AttachLoadBalancerTLSCertificate
import Network.AWS.Lightsail.AttachStaticIP
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
import Network.AWS.Lightsail.CreateLoadBalancerTLSCertificate
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
import Network.AWS.Lightsail.DeleteLoadBalancerTLSCertificate
import Network.AWS.Lightsail.DeleteRelationalDatabase
import Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
import Network.AWS.Lightsail.DetachCertificateFromDistribution
import Network.AWS.Lightsail.DetachDisk
import Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
import Network.AWS.Lightsail.DetachStaticIP
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
import Network.AWS.Lightsail.GetLoadBalancerTLSCertificates
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
import Network.AWS.Lightsail.GetStaticIP
import Network.AWS.Lightsail.GetStaticIPs
import Network.AWS.Lightsail.ImportKeyPair
import Network.AWS.Lightsail.IsVPCPeered
import Network.AWS.Lightsail.OpenInstancePublicPorts
import Network.AWS.Lightsail.PeerVPC
import Network.AWS.Lightsail.PutAlarm
import Network.AWS.Lightsail.PutInstancePublicPorts
import Network.AWS.Lightsail.RebootInstance
import Network.AWS.Lightsail.RebootRelationalDatabase
import Network.AWS.Lightsail.RegisterContainerImage
import Network.AWS.Lightsail.ReleaseStaticIP
import Network.AWS.Lightsail.ResetDistributionCache
import Network.AWS.Lightsail.SendContactMethodVerification
import Network.AWS.Lightsail.StartInstance
import Network.AWS.Lightsail.StartRelationalDatabase
import Network.AWS.Lightsail.StopInstance
import Network.AWS.Lightsail.StopRelationalDatabase
import Network.AWS.Lightsail.TagResource
import Network.AWS.Lightsail.TestAlarm
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.UnpeerVPC
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
