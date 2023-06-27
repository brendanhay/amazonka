{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AccountSetupInProgressException,
    _InvalidInputException,
    _NotFoundException,
    _OperationFailureException,
    _ServiceException,
    _UnauthenticatedException,

    -- * AccessDirection
    AccessDirection (..),

    -- * AccessType
    AccessType (..),

    -- * AccountLevelBpaSyncStatus
    AccountLevelBpaSyncStatus (..),

    -- * AddOnType
    AddOnType (..),

    -- * AlarmState
    AlarmState (..),

    -- * AppCategory
    AppCategory (..),

    -- * AutoMountStatus
    AutoMountStatus (..),

    -- * AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- * BPAStatusMessage
    BPAStatusMessage (..),

    -- * BehaviorEnum
    BehaviorEnum (..),

    -- * BlueprintType
    BlueprintType (..),

    -- * BucketMetricName
    BucketMetricName (..),

    -- * CertificateDomainValidationStatus
    CertificateDomainValidationStatus (..),

    -- * CertificateStatus
    CertificateStatus (..),

    -- * CloudFormationStackRecordSourceType
    CloudFormationStackRecordSourceType (..),

    -- * ComparisonOperator
    ComparisonOperator (..),

    -- * ContactMethodStatus
    ContactMethodStatus (..),

    -- * ContactMethodVerificationProtocol
    ContactMethodVerificationProtocol (..),

    -- * ContactProtocol
    ContactProtocol (..),

    -- * ContainerServiceDeploymentState
    ContainerServiceDeploymentState (..),

    -- * ContainerServiceMetricName
    ContainerServiceMetricName (..),

    -- * ContainerServicePowerName
    ContainerServicePowerName (..),

    -- * ContainerServiceProtocol
    ContainerServiceProtocol (..),

    -- * ContainerServiceState
    ContainerServiceState (..),

    -- * ContainerServiceStateDetailCode
    ContainerServiceStateDetailCode (..),

    -- * Currency
    Currency (..),

    -- * DiskSnapshotState
    DiskSnapshotState (..),

    -- * DiskState
    DiskState (..),

    -- * DistributionMetricName
    DistributionMetricName (..),

    -- * DnsRecordCreationStateCode
    DnsRecordCreationStateCode (..),

    -- * ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- * ForwardValues
    ForwardValues (..),

    -- * HeaderEnum
    HeaderEnum (..),

    -- * HttpEndpoint
    HttpEndpoint (..),

    -- * HttpProtocolIpv6
    HttpProtocolIpv6 (..),

    -- * HttpTokens
    HttpTokens (..),

    -- * InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- * InstanceHealthReason
    InstanceHealthReason (..),

    -- * InstanceHealthState
    InstanceHealthState (..),

    -- * InstanceMetadataState
    InstanceMetadataState (..),

    -- * InstanceMetricName
    InstanceMetricName (..),

    -- * InstancePlatform
    InstancePlatform (..),

    -- * InstanceSnapshotState
    InstanceSnapshotState (..),

    -- * IpAddressType
    IpAddressType (..),

    -- * LoadBalancerAttributeName
    LoadBalancerAttributeName (..),

    -- * LoadBalancerMetricName
    LoadBalancerMetricName (..),

    -- * LoadBalancerProtocol
    LoadBalancerProtocol (..),

    -- * LoadBalancerState
    LoadBalancerState (..),

    -- * LoadBalancerTlsCertificateDnsRecordCreationStateCode
    LoadBalancerTlsCertificateDnsRecordCreationStateCode (..),

    -- * LoadBalancerTlsCertificateDomainStatus
    LoadBalancerTlsCertificateDomainStatus (..),

    -- * LoadBalancerTlsCertificateFailureReason
    LoadBalancerTlsCertificateFailureReason (..),

    -- * LoadBalancerTlsCertificateRenewalStatus
    LoadBalancerTlsCertificateRenewalStatus (..),

    -- * LoadBalancerTlsCertificateRevocationReason
    LoadBalancerTlsCertificateRevocationReason (..),

    -- * LoadBalancerTlsCertificateStatus
    LoadBalancerTlsCertificateStatus (..),

    -- * MetricName
    MetricName (..),

    -- * MetricStatistic
    MetricStatistic (..),

    -- * MetricUnit
    MetricUnit (..),

    -- * NameServersUpdateStateCode
    NameServersUpdateStateCode (..),

    -- * NetworkProtocol
    NetworkProtocol (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * OperationType
    OperationType (..),

    -- * OriginProtocolPolicyEnum
    OriginProtocolPolicyEnum (..),

    -- * PortAccessType
    PortAccessType (..),

    -- * PortInfoSourceType
    PortInfoSourceType (..),

    -- * PortState
    PortState (..),

    -- * PricingUnit
    PricingUnit (..),

    -- * R53HostedZoneDeletionStateCode
    R53HostedZoneDeletionStateCode (..),

    -- * RecordState
    RecordState (..),

    -- * RegionName
    RegionName (..),

    -- * RelationalDatabaseEngine
    RelationalDatabaseEngine (..),

    -- * RelationalDatabaseMetricName
    RelationalDatabaseMetricName (..),

    -- * RelationalDatabasePasswordVersion
    RelationalDatabasePasswordVersion (..),

    -- * RenewalStatus
    RenewalStatus (..),

    -- * ResourceBucketAccess
    ResourceBucketAccess (..),

    -- * ResourceType
    ResourceType (..),

    -- * Status
    Status (..),

    -- * StatusType
    StatusType (..),

    -- * TreatMissingData
    TreatMissingData (..),

    -- * AccessKey
    AccessKey (..),
    newAccessKey,
    accessKey_accessKeyId,
    accessKey_createdAt,
    accessKey_lastUsed,
    accessKey_secretAccessKey,
    accessKey_status,

    -- * AccessKeyLastUsed
    AccessKeyLastUsed (..),
    newAccessKeyLastUsed,
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_region,
    accessKeyLastUsed_serviceName,

    -- * AccessRules
    AccessRules (..),
    newAccessRules,
    accessRules_allowPublicOverrides,
    accessRules_getObject,

    -- * AccountLevelBpaSync
    AccountLevelBpaSync (..),
    newAccountLevelBpaSync,
    accountLevelBpaSync_bpaImpactsLightsail,
    accountLevelBpaSync_lastSyncedAt,
    accountLevelBpaSync_message,
    accountLevelBpaSync_status,

    -- * AddOn
    AddOn (..),
    newAddOn,
    addOn_duration,
    addOn_name,
    addOn_nextSnapshotTimeOfDay,
    addOn_snapshotTimeOfDay,
    addOn_status,
    addOn_threshold,

    -- * AddOnRequest
    AddOnRequest (..),
    newAddOnRequest,
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_stopInstanceOnIdleRequest,
    addOnRequest_addOnType,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_arn,
    alarm_comparisonOperator,
    alarm_contactProtocols,
    alarm_createdAt,
    alarm_datapointsToAlarm,
    alarm_evaluationPeriods,
    alarm_location,
    alarm_metricName,
    alarm_monitoredResourceInfo,
    alarm_name,
    alarm_notificationEnabled,
    alarm_notificationTriggers,
    alarm_period,
    alarm_resourceType,
    alarm_state,
    alarm_statistic,
    alarm_supportCode,
    alarm_threshold,
    alarm_treatMissingData,
    alarm_unit,

    -- * AttachedDisk
    AttachedDisk (..),
    newAttachedDisk,
    attachedDisk_path,
    attachedDisk_sizeInGb,

    -- * AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest (..),
    newAutoSnapshotAddOnRequest,
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- * AutoSnapshotDetails
    AutoSnapshotDetails (..),
    newAutoSnapshotDetails,
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_status,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_state,
    availabilityZone_zoneName,

    -- * Blueprint
    Blueprint (..),
    newBlueprint,
    blueprint_appCategory,
    blueprint_blueprintId,
    blueprint_description,
    blueprint_group,
    blueprint_isActive,
    blueprint_licenseUrl,
    blueprint_minPower,
    blueprint_name,
    blueprint_platform,
    blueprint_productUrl,
    blueprint_type,
    blueprint_version,
    blueprint_versionCode,

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_ableToUpdateBundle,
    bucket_accessLogConfig,
    bucket_accessRules,
    bucket_arn,
    bucket_bundleId,
    bucket_createdAt,
    bucket_location,
    bucket_name,
    bucket_objectVersioning,
    bucket_readonlyAccessAccounts,
    bucket_resourceType,
    bucket_resourcesReceivingAccess,
    bucket_state,
    bucket_supportCode,
    bucket_tags,
    bucket_url,

    -- * BucketAccessLogConfig
    BucketAccessLogConfig (..),
    newBucketAccessLogConfig,
    bucketAccessLogConfig_destination,
    bucketAccessLogConfig_prefix,
    bucketAccessLogConfig_enabled,

    -- * BucketBundle
    BucketBundle (..),
    newBucketBundle,
    bucketBundle_bundleId,
    bucketBundle_isActive,
    bucketBundle_name,
    bucketBundle_price,
    bucketBundle_storagePerMonthInGb,
    bucketBundle_transferPerMonthInGb,

    -- * BucketState
    BucketState (..),
    newBucketState,
    bucketState_code,
    bucketState_message,

    -- * Bundle
    Bundle (..),
    newBundle,
    bundle_bundleId,
    bundle_cpuCount,
    bundle_diskSizeInGb,
    bundle_instanceType,
    bundle_isActive,
    bundle_name,
    bundle_power,
    bundle_price,
    bundle_ramSizeInGb,
    bundle_supportedAppCategories,
    bundle_supportedPlatforms,
    bundle_transferPerMonthInGb,

    -- * CacheBehavior
    CacheBehavior (..),
    newCacheBehavior,
    cacheBehavior_behavior,

    -- * CacheBehaviorPerPath
    CacheBehaviorPerPath (..),
    newCacheBehaviorPerPath,
    cacheBehaviorPerPath_behavior,
    cacheBehaviorPerPath_path,

    -- * CacheSettings
    CacheSettings (..),
    newCacheSettings,
    cacheSettings_allowedHTTPMethods,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_forwardedCookies,
    cacheSettings_forwardedHeaders,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_maximumTTL,
    cacheSettings_minimumTTL,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_arn,
    certificate_createdAt,
    certificate_domainName,
    certificate_domainValidationRecords,
    certificate_eligibleToRenew,
    certificate_inUseResourceCount,
    certificate_issuedAt,
    certificate_issuerCA,
    certificate_keyAlgorithm,
    certificate_name,
    certificate_notAfter,
    certificate_notBefore,
    certificate_renewalSummary,
    certificate_requestFailureReason,
    certificate_revocationReason,
    certificate_revokedAt,
    certificate_serialNumber,
    certificate_status,
    certificate_subjectAlternativeNames,
    certificate_supportCode,
    certificate_tags,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_certificateArn,
    certificateSummary_certificateDetail,
    certificateSummary_certificateName,
    certificateSummary_domainName,
    certificateSummary_tags,

    -- * CloudFormationStackRecord
    CloudFormationStackRecord (..),
    newCloudFormationStackRecord,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_destinationInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_state,

    -- * CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    newCloudFormationStackRecordSourceInfo,
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_name,
    cloudFormationStackRecordSourceInfo_resourceType,

    -- * ContactMethod
    ContactMethod (..),
    newContactMethod,
    contactMethod_arn,
    contactMethod_contactEndpoint,
    contactMethod_createdAt,
    contactMethod_location,
    contactMethod_name,
    contactMethod_protocol,
    contactMethod_resourceType,
    contactMethod_status,
    contactMethod_supportCode,

    -- * Container
    Container (..),
    newContainer,
    container_command,
    container_environment,
    container_image,
    container_ports,

    -- * ContainerImage
    ContainerImage (..),
    newContainerImage,
    containerImage_createdAt,
    containerImage_digest,
    containerImage_image,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_arn,
    containerService_containerServiceName,
    containerService_createdAt,
    containerService_currentDeployment,
    containerService_isDisabled,
    containerService_location,
    containerService_nextDeployment,
    containerService_power,
    containerService_powerId,
    containerService_principalArn,
    containerService_privateDomainName,
    containerService_privateRegistryAccess,
    containerService_publicDomainNames,
    containerService_resourceType,
    containerService_scale,
    containerService_state,
    containerService_stateDetail,
    containerService_tags,
    containerService_url,

    -- * ContainerServiceDeployment
    ContainerServiceDeployment (..),
    newContainerServiceDeployment,
    containerServiceDeployment_containers,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_state,
    containerServiceDeployment_version,

    -- * ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (..),
    newContainerServiceDeploymentRequest,
    containerServiceDeploymentRequest_containers,
    containerServiceDeploymentRequest_publicEndpoint,

    -- * ContainerServiceECRImagePullerRole
    ContainerServiceECRImagePullerRole (..),
    newContainerServiceECRImagePullerRole,
    containerServiceECRImagePullerRole_isActive,
    containerServiceECRImagePullerRole_principalArn,

    -- * ContainerServiceECRImagePullerRoleRequest
    ContainerServiceECRImagePullerRoleRequest (..),
    newContainerServiceECRImagePullerRoleRequest,
    containerServiceECRImagePullerRoleRequest_isActive,

    -- * ContainerServiceEndpoint
    ContainerServiceEndpoint (..),
    newContainerServiceEndpoint,
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_healthCheck,

    -- * ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    newContainerServiceHealthCheckConfig,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_unhealthyThreshold,

    -- * ContainerServiceLogEvent
    ContainerServiceLogEvent (..),
    newContainerServiceLogEvent,
    containerServiceLogEvent_createdAt,
    containerServiceLogEvent_message,

    -- * ContainerServicePower
    ContainerServicePower (..),
    newContainerServicePower,
    containerServicePower_cpuCount,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_powerId,
    containerServicePower_price,
    containerServicePower_ramSizeInGb,

    -- * ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    newContainerServiceRegistryLogin,
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_username,

    -- * ContainerServiceStateDetail
    ContainerServiceStateDetail (..),
    newContainerServiceStateDetail,
    containerServiceStateDetail_code,
    containerServiceStateDetail_message,

    -- * CookieObject
    CookieObject (..),
    newCookieObject,
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- * CostEstimate
    CostEstimate (..),
    newCostEstimate,
    costEstimate_resultsByTime,
    costEstimate_usageType,

    -- * DestinationInfo
    DestinationInfo (..),
    newDestinationInfo,
    destinationInfo_id,
    destinationInfo_service,

    -- * Disk
    Disk (..),
    newDisk,
    disk_addOns,
    disk_arn,
    disk_attachedTo,
    disk_attachmentState,
    disk_autoMountStatus,
    disk_createdAt,
    disk_gbInUse,
    disk_iops,
    disk_isAttached,
    disk_isSystemDisk,
    disk_location,
    disk_name,
    disk_path,
    disk_resourceType,
    disk_sizeInGb,
    disk_state,
    disk_supportCode,
    disk_tags,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_isSystemDisk,
    diskInfo_name,
    diskInfo_path,
    diskInfo_sizeInGb,

    -- * DiskMap
    DiskMap (..),
    newDiskMap,
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- * DiskSnapshot
    DiskSnapshot (..),
    newDiskSnapshot,
    diskSnapshot_arn,
    diskSnapshot_createdAt,
    diskSnapshot_fromDiskArn,
    diskSnapshot_fromDiskName,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_location,
    diskSnapshot_name,
    diskSnapshot_progress,
    diskSnapshot_resourceType,
    diskSnapshot_sizeInGb,
    diskSnapshot_state,
    diskSnapshot_supportCode,
    diskSnapshot_tags,

    -- * DiskSnapshotInfo
    DiskSnapshotInfo (..),
    newDiskSnapshotInfo,
    diskSnapshotInfo_sizeInGb,

    -- * DistributionBundle
    DistributionBundle (..),
    newDistributionBundle,
    distributionBundle_bundleId,
    distributionBundle_isActive,
    distributionBundle_name,
    distributionBundle_price,
    distributionBundle_transferPerMonthInGb,

    -- * DnsRecordCreationState
    DnsRecordCreationState (..),
    newDnsRecordCreationState,
    dnsRecordCreationState_code,
    dnsRecordCreationState_message,

    -- * Domain
    Domain (..),
    newDomain,
    domain_arn,
    domain_createdAt,
    domain_domainEntries,
    domain_location,
    domain_name,
    domain_registeredDomainDelegationInfo,
    domain_resourceType,
    domain_supportCode,
    domain_tags,

    -- * DomainEntry
    DomainEntry (..),
    newDomainEntry,
    domainEntry_id,
    domainEntry_isAlias,
    domainEntry_name,
    domainEntry_options,
    domainEntry_target,
    domainEntry_type,

    -- * DomainValidationRecord
    DomainValidationRecord (..),
    newDomainValidationRecord,
    domainValidationRecord_dnsRecordCreationState,
    domainValidationRecord_domainName,
    domainValidationRecord_resourceRecord,
    domainValidationRecord_validationStatus,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- * EstimateByTime
    EstimateByTime (..),
    newEstimateByTime,
    estimateByTime_currency,
    estimateByTime_pricingUnit,
    estimateByTime_timePeriod,
    estimateByTime_unit,
    estimateByTime_usageCost,

    -- * ExportSnapshotRecord
    ExportSnapshotRecord (..),
    newExportSnapshotRecord,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_destinationInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_name,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_state,

    -- * ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    newExportSnapshotRecordSourceInfo,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_fromResourceName,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_resourceType,

    -- * HeaderObject
    HeaderObject (..),
    newHeaderObject,
    headerObject_headersAllowList,
    headerObject_option,

    -- * HostKeyAttributes
    HostKeyAttributes (..),
    newHostKeyAttributes,
    hostKeyAttributes_algorithm,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_fingerprintSHA256,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_witnessedAt,

    -- * InputOrigin
    InputOrigin (..),
    newInputOrigin,
    inputOrigin_name,
    inputOrigin_protocolPolicy,
    inputOrigin_regionName,

    -- * Instance
    Instance (..),
    newInstance,
    instance_addOns,
    instance_arn,
    instance_blueprintId,
    instance_blueprintName,
    instance_bundleId,
    instance_createdAt,
    instance_hardware,
    instance_ipAddressType,
    instance_ipv6Addresses,
    instance_isStaticIp,
    instance_location,
    instance_metadataOptions,
    instance_name,
    instance_networking,
    instance_privateIpAddress,
    instance_publicIpAddress,
    instance_resourceType,
    instance_sshKeyName,
    instance_state,
    instance_supportCode,
    instance_tags,
    instance_username,

    -- * InstanceAccessDetails
    InstanceAccessDetails (..),
    newInstanceAccessDetails,
    instanceAccessDetails_certKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_password,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_protocol,
    instanceAccessDetails_username,

    -- * InstanceEntry
    InstanceEntry (..),
    newInstanceEntry,
    instanceEntry_userData,
    instanceEntry_sourceName,
    instanceEntry_instanceType,
    instanceEntry_portInfoSource,
    instanceEntry_availabilityZone,

    -- * InstanceHardware
    InstanceHardware (..),
    newInstanceHardware,
    instanceHardware_cpuCount,
    instanceHardware_disks,
    instanceHardware_ramSizeInGb,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    newInstanceHealthSummary,
    instanceHealthSummary_instanceHealth,
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceName,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    newInstanceMetadataOptions,
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpProtocolIpv6,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_httpTokens,
    instanceMetadataOptions_state,

    -- * InstanceNetworking
    InstanceNetworking (..),
    newInstanceNetworking,
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- * InstancePortInfo
    InstancePortInfo (..),
    newInstancePortInfo,
    instancePortInfo_accessDirection,
    instancePortInfo_accessFrom,
    instancePortInfo_accessType,
    instancePortInfo_cidrListAliases,
    instancePortInfo_cidrs,
    instancePortInfo_commonName,
    instancePortInfo_fromPort,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_protocol,
    instancePortInfo_toPort,

    -- * InstancePortState
    InstancePortState (..),
    newInstancePortState,
    instancePortState_cidrListAliases,
    instancePortState_cidrs,
    instancePortState_fromPort,
    instancePortState_ipv6Cidrs,
    instancePortState_protocol,
    instancePortState_state,
    instancePortState_toPort,

    -- * InstanceSnapshot
    InstanceSnapshot (..),
    newInstanceSnapshot,
    instanceSnapshot_arn,
    instanceSnapshot_createdAt,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_location,
    instanceSnapshot_name,
    instanceSnapshot_progress,
    instanceSnapshot_resourceType,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_state,
    instanceSnapshot_supportCode,
    instanceSnapshot_tags,

    -- * InstanceSnapshotInfo
    InstanceSnapshotInfo (..),
    newInstanceSnapshotInfo,
    instanceSnapshotInfo_fromBlueprintId,
    instanceSnapshotInfo_fromBundleId,
    instanceSnapshotInfo_fromDiskInfo,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_code,
    instanceState_name,

    -- * KeyPair
    KeyPair (..),
    newKeyPair,
    keyPair_arn,
    keyPair_createdAt,
    keyPair_fingerprint,
    keyPair_location,
    keyPair_name,
    keyPair_resourceType,
    keyPair_supportCode,
    keyPair_tags,

    -- * LightsailDistribution
    LightsailDistribution (..),
    newLightsailDistribution,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_arn,
    lightsailDistribution_bundleId,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_certificateName,
    lightsailDistribution_createdAt,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_domainName,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_isEnabled,
    lightsailDistribution_location,
    lightsailDistribution_name,
    lightsailDistribution_origin,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_resourceType,
    lightsailDistribution_status,
    lightsailDistribution_supportCode,
    lightsailDistribution_tags,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_arn,
    loadBalancer_configurationOptions,
    loadBalancer_createdAt,
    loadBalancer_dnsName,
    loadBalancer_healthCheckPath,
    loadBalancer_httpsRedirectionEnabled,
    loadBalancer_instanceHealthSummary,
    loadBalancer_instancePort,
    loadBalancer_ipAddressType,
    loadBalancer_location,
    loadBalancer_name,
    loadBalancer_protocol,
    loadBalancer_publicPorts,
    loadBalancer_resourceType,
    loadBalancer_state,
    loadBalancer_supportCode,
    loadBalancer_tags,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_tlsPolicyName,

    -- * LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (..),
    newLoadBalancerTlsCertificate,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_tags,

    -- * LoadBalancerTlsCertificateDnsRecordCreationState
    LoadBalancerTlsCertificateDnsRecordCreationState (..),
    newLoadBalancerTlsCertificateDnsRecordCreationState,
    loadBalancerTlsCertificateDnsRecordCreationState_code,
    loadBalancerTlsCertificateDnsRecordCreationState_message,

    -- * LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (..),
    newLoadBalancerTlsCertificateDomainValidationOption,
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- * LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (..),
    newLoadBalancerTlsCertificateDomainValidationRecord,
    loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_type,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_value,

    -- * LoadBalancerTlsCertificateRenewalSummary
    LoadBalancerTlsCertificateRenewalSummary (..),
    newLoadBalancerTlsCertificateRenewalSummary,
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,

    -- * LoadBalancerTlsCertificateSummary
    LoadBalancerTlsCertificateSummary (..),
    newLoadBalancerTlsCertificateSummary,
    loadBalancerTlsCertificateSummary_isAttached,
    loadBalancerTlsCertificateSummary_name,

    -- * LoadBalancerTlsPolicy
    LoadBalancerTlsPolicy (..),
    newLoadBalancerTlsPolicy,
    loadBalancerTlsPolicy_ciphers,
    loadBalancerTlsPolicy_description,
    loadBalancerTlsPolicy_isDefault,
    loadBalancerTlsPolicy_name,
    loadBalancerTlsPolicy_protocols,

    -- * LogEvent
    LogEvent (..),
    newLogEvent,
    logEvent_createdAt,
    logEvent_message,

    -- * MetricDatapoint
    MetricDatapoint (..),
    newMetricDatapoint,
    metricDatapoint_average,
    metricDatapoint_maximum,
    metricDatapoint_minimum,
    metricDatapoint_sampleCount,
    metricDatapoint_sum,
    metricDatapoint_timestamp,
    metricDatapoint_unit,

    -- * MonitoredResourceInfo
    MonitoredResourceInfo (..),
    newMonitoredResourceInfo,
    monitoredResourceInfo_arn,
    monitoredResourceInfo_name,
    monitoredResourceInfo_resourceType,

    -- * MonthlyTransfer
    MonthlyTransfer (..),
    newMonthlyTransfer,
    monthlyTransfer_gbPerMonthAllocated,

    -- * NameServersUpdateState
    NameServersUpdateState (..),
    newNameServersUpdateState,
    nameServersUpdateState_code,
    nameServersUpdateState_message,

    -- * Operation
    Operation (..),
    newOperation,
    operation_createdAt,
    operation_errorCode,
    operation_errorDetails,
    operation_id,
    operation_isTerminal,
    operation_location,
    operation_operationDetails,
    operation_operationType,
    operation_resourceName,
    operation_resourceType,
    operation_status,
    operation_statusChangedAt,

    -- * Origin
    Origin (..),
    newOrigin,
    origin_name,
    origin_protocolPolicy,
    origin_regionName,
    origin_resourceType,

    -- * PasswordData
    PasswordData (..),
    newPasswordData,
    passwordData_ciphertext,
    passwordData_keyPairName,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_description,

    -- * PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    newPendingModifiedRelationalDatabaseValues,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_engineVersion,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,

    -- * PortInfo
    PortInfo (..),
    newPortInfo,
    portInfo_cidrListAliases,
    portInfo_cidrs,
    portInfo_fromPort,
    portInfo_ipv6Cidrs,
    portInfo_protocol,
    portInfo_toPort,

    -- * PrivateRegistryAccess
    PrivateRegistryAccess (..),
    newPrivateRegistryAccess,
    privateRegistryAccess_ecrImagePullerRole,

    -- * PrivateRegistryAccessRequest
    PrivateRegistryAccessRequest (..),
    newPrivateRegistryAccessRequest,
    privateRegistryAccessRequest_ecrImagePullerRole,

    -- * QueryStringObject
    QueryStringObject (..),
    newQueryStringObject,
    queryStringObject_option,
    queryStringObject_queryStringsAllowList,

    -- * R53HostedZoneDeletionState
    R53HostedZoneDeletionState (..),
    newR53HostedZoneDeletionState,
    r53HostedZoneDeletionState_code,
    r53HostedZoneDeletionState_message,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_availabilityZones,
    regionInfo_continentCode,
    regionInfo_description,
    regionInfo_displayName,
    regionInfo_name,
    regionInfo_relationalDatabaseAvailabilityZones,

    -- * RegisteredDomainDelegationInfo
    RegisteredDomainDelegationInfo (..),
    newRegisteredDomainDelegationInfo,
    registeredDomainDelegationInfo_nameServersUpdateState,
    registeredDomainDelegationInfo_r53HostedZoneDeletionState,

    -- * RelationalDatabase
    RelationalDatabase (..),
    newRelationalDatabase,
    relationalDatabase_arn,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_createdAt,
    relationalDatabase_engine,
    relationalDatabase_engineVersion,
    relationalDatabase_hardware,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_location,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_masterEndpoint,
    relationalDatabase_masterUsername,
    relationalDatabase_name,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_resourceType,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_state,
    relationalDatabase_supportCode,
    relationalDatabase_tags,

    -- * RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    newRelationalDatabaseBlueprint,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_isEngineDefault,

    -- * RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    newRelationalDatabaseBundle,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_ramSizeInGb,
    relationalDatabaseBundle_transferPerMonthInGb,

    -- * RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (..),
    newRelationalDatabaseEndpoint,
    relationalDatabaseEndpoint_address,
    relationalDatabaseEndpoint_port,

    -- * RelationalDatabaseEvent
    RelationalDatabaseEvent (..),
    newRelationalDatabaseEvent,
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_resource,

    -- * RelationalDatabaseHardware
    RelationalDatabaseHardware (..),
    newRelationalDatabaseHardware,
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,
    relationalDatabaseHardware_ramSizeInGb,

    -- * RelationalDatabaseParameter
    RelationalDatabaseParameter (..),
    newRelationalDatabaseParameter,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_dataType,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_parameterValue,

    -- * RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    newRelationalDatabaseSnapshot,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_tags,

    -- * RenewalSummary
    RenewalSummary (..),
    newRenewalSummary,
    renewalSummary_domainValidationRecords,
    renewalSummary_renewalStatus,
    renewalSummary_renewalStatusReason,
    renewalSummary_updatedAt,

    -- * ResourceBudgetEstimate
    ResourceBudgetEstimate (..),
    newResourceBudgetEstimate,
    resourceBudgetEstimate_costEstimates,
    resourceBudgetEstimate_endTime,
    resourceBudgetEstimate_resourceName,
    resourceBudgetEstimate_resourceType,
    resourceBudgetEstimate_startTime,

    -- * ResourceLocation
    ResourceLocation (..),
    newResourceLocation,
    resourceLocation_availabilityZone,
    resourceLocation_regionName,

    -- * ResourceReceivingAccess
    ResourceReceivingAccess (..),
    newResourceReceivingAccess,
    resourceReceivingAccess_name,
    resourceReceivingAccess_resourceType,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- * Session
    Session (..),
    newSession,
    session_isPrimary,
    session_name,
    session_url,

    -- * StaticIp
    StaticIp (..),
    newStaticIp,
    staticIp_arn,
    staticIp_attachedTo,
    staticIp_createdAt,
    staticIp_ipAddress,
    staticIp_isAttached,
    staticIp_location,
    staticIp_name,
    staticIp_resourceType,
    staticIp_supportCode,

    -- * StopInstanceOnIdleRequest
    StopInstanceOnIdleRequest (..),
    newStopInstanceOnIdleRequest,
    stopInstanceOnIdleRequest_duration,
    stopInstanceOnIdleRequest_threshold,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TimePeriod
    TimePeriod (..),
    newTimePeriod,
    timePeriod_end,
    timePeriod_start,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types.AccessDirection
import Amazonka.Lightsail.Types.AccessKey
import Amazonka.Lightsail.Types.AccessKeyLastUsed
import Amazonka.Lightsail.Types.AccessRules
import Amazonka.Lightsail.Types.AccessType
import Amazonka.Lightsail.Types.AccountLevelBpaSync
import Amazonka.Lightsail.Types.AccountLevelBpaSyncStatus
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.AddOnRequest
import Amazonka.Lightsail.Types.AddOnType
import Amazonka.Lightsail.Types.Alarm
import Amazonka.Lightsail.Types.AlarmState
import Amazonka.Lightsail.Types.AppCategory
import Amazonka.Lightsail.Types.AttachedDisk
import Amazonka.Lightsail.Types.AutoMountStatus
import Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
import Amazonka.Lightsail.Types.AutoSnapshotDetails
import Amazonka.Lightsail.Types.AutoSnapshotStatus
import Amazonka.Lightsail.Types.AvailabilityZone
import Amazonka.Lightsail.Types.BPAStatusMessage
import Amazonka.Lightsail.Types.BehaviorEnum
import Amazonka.Lightsail.Types.Blueprint
import Amazonka.Lightsail.Types.BlueprintType
import Amazonka.Lightsail.Types.Bucket
import Amazonka.Lightsail.Types.BucketAccessLogConfig
import Amazonka.Lightsail.Types.BucketBundle
import Amazonka.Lightsail.Types.BucketMetricName
import Amazonka.Lightsail.Types.BucketState
import Amazonka.Lightsail.Types.Bundle
import Amazonka.Lightsail.Types.CacheBehavior
import Amazonka.Lightsail.Types.CacheBehaviorPerPath
import Amazonka.Lightsail.Types.CacheSettings
import Amazonka.Lightsail.Types.Certificate
import Amazonka.Lightsail.Types.CertificateDomainValidationStatus
import Amazonka.Lightsail.Types.CertificateStatus
import Amazonka.Lightsail.Types.CertificateSummary
import Amazonka.Lightsail.Types.CloudFormationStackRecord
import Amazonka.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Amazonka.Lightsail.Types.CloudFormationStackRecordSourceType
import Amazonka.Lightsail.Types.ComparisonOperator
import Amazonka.Lightsail.Types.ContactMethod
import Amazonka.Lightsail.Types.ContactMethodStatus
import Amazonka.Lightsail.Types.ContactMethodVerificationProtocol
import Amazonka.Lightsail.Types.ContactProtocol
import Amazonka.Lightsail.Types.Container
import Amazonka.Lightsail.Types.ContainerImage
import Amazonka.Lightsail.Types.ContainerService
import Amazonka.Lightsail.Types.ContainerServiceDeployment
import Amazonka.Lightsail.Types.ContainerServiceDeploymentRequest
import Amazonka.Lightsail.Types.ContainerServiceDeploymentState
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRole
import Amazonka.Lightsail.Types.ContainerServiceECRImagePullerRoleRequest
import Amazonka.Lightsail.Types.ContainerServiceEndpoint
import Amazonka.Lightsail.Types.ContainerServiceHealthCheckConfig
import Amazonka.Lightsail.Types.ContainerServiceLogEvent
import Amazonka.Lightsail.Types.ContainerServiceMetricName
import Amazonka.Lightsail.Types.ContainerServicePower
import Amazonka.Lightsail.Types.ContainerServicePowerName
import Amazonka.Lightsail.Types.ContainerServiceProtocol
import Amazonka.Lightsail.Types.ContainerServiceRegistryLogin
import Amazonka.Lightsail.Types.ContainerServiceState
import Amazonka.Lightsail.Types.ContainerServiceStateDetail
import Amazonka.Lightsail.Types.ContainerServiceStateDetailCode
import Amazonka.Lightsail.Types.CookieObject
import Amazonka.Lightsail.Types.CostEstimate
import Amazonka.Lightsail.Types.Currency
import Amazonka.Lightsail.Types.DestinationInfo
import Amazonka.Lightsail.Types.Disk
import Amazonka.Lightsail.Types.DiskInfo
import Amazonka.Lightsail.Types.DiskMap
import Amazonka.Lightsail.Types.DiskSnapshot
import Amazonka.Lightsail.Types.DiskSnapshotInfo
import Amazonka.Lightsail.Types.DiskSnapshotState
import Amazonka.Lightsail.Types.DiskState
import Amazonka.Lightsail.Types.DistributionBundle
import Amazonka.Lightsail.Types.DistributionMetricName
import Amazonka.Lightsail.Types.DnsRecordCreationState
import Amazonka.Lightsail.Types.DnsRecordCreationStateCode
import Amazonka.Lightsail.Types.Domain
import Amazonka.Lightsail.Types.DomainEntry
import Amazonka.Lightsail.Types.DomainValidationRecord
import Amazonka.Lightsail.Types.EndpointRequest
import Amazonka.Lightsail.Types.EstimateByTime
import Amazonka.Lightsail.Types.ExportSnapshotRecord
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceType
import Amazonka.Lightsail.Types.ForwardValues
import Amazonka.Lightsail.Types.HeaderEnum
import Amazonka.Lightsail.Types.HeaderObject
import Amazonka.Lightsail.Types.HostKeyAttributes
import Amazonka.Lightsail.Types.HttpEndpoint
import Amazonka.Lightsail.Types.HttpProtocolIpv6
import Amazonka.Lightsail.Types.HttpTokens
import Amazonka.Lightsail.Types.InputOrigin
import Amazonka.Lightsail.Types.Instance
import Amazonka.Lightsail.Types.InstanceAccessDetails
import Amazonka.Lightsail.Types.InstanceAccessProtocol
import Amazonka.Lightsail.Types.InstanceEntry
import Amazonka.Lightsail.Types.InstanceHardware
import Amazonka.Lightsail.Types.InstanceHealthReason
import Amazonka.Lightsail.Types.InstanceHealthState
import Amazonka.Lightsail.Types.InstanceHealthSummary
import Amazonka.Lightsail.Types.InstanceMetadataOptions
import Amazonka.Lightsail.Types.InstanceMetadataState
import Amazonka.Lightsail.Types.InstanceMetricName
import Amazonka.Lightsail.Types.InstanceNetworking
import Amazonka.Lightsail.Types.InstancePlatform
import Amazonka.Lightsail.Types.InstancePortInfo
import Amazonka.Lightsail.Types.InstancePortState
import Amazonka.Lightsail.Types.InstanceSnapshot
import Amazonka.Lightsail.Types.InstanceSnapshotInfo
import Amazonka.Lightsail.Types.InstanceSnapshotState
import Amazonka.Lightsail.Types.InstanceState
import Amazonka.Lightsail.Types.IpAddressType
import Amazonka.Lightsail.Types.KeyPair
import Amazonka.Lightsail.Types.LightsailDistribution
import Amazonka.Lightsail.Types.LoadBalancer
import Amazonka.Lightsail.Types.LoadBalancerAttributeName
import Amazonka.Lightsail.Types.LoadBalancerMetricName
import Amazonka.Lightsail.Types.LoadBalancerProtocol
import Amazonka.Lightsail.Types.LoadBalancerState
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificate
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationState
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDnsRecordCreationStateCode
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsPolicy
import Amazonka.Lightsail.Types.LogEvent
import Amazonka.Lightsail.Types.MetricDatapoint
import Amazonka.Lightsail.Types.MetricName
import Amazonka.Lightsail.Types.MetricStatistic
import Amazonka.Lightsail.Types.MetricUnit
import Amazonka.Lightsail.Types.MonitoredResourceInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
import Amazonka.Lightsail.Types.NameServersUpdateState
import Amazonka.Lightsail.Types.NameServersUpdateStateCode
import Amazonka.Lightsail.Types.NetworkProtocol
import Amazonka.Lightsail.Types.Operation
import Amazonka.Lightsail.Types.OperationStatus
import Amazonka.Lightsail.Types.OperationType
import Amazonka.Lightsail.Types.Origin
import Amazonka.Lightsail.Types.OriginProtocolPolicyEnum
import Amazonka.Lightsail.Types.PasswordData
import Amazonka.Lightsail.Types.PendingMaintenanceAction
import Amazonka.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Amazonka.Lightsail.Types.PortAccessType
import Amazonka.Lightsail.Types.PortInfo
import Amazonka.Lightsail.Types.PortInfoSourceType
import Amazonka.Lightsail.Types.PortState
import Amazonka.Lightsail.Types.PricingUnit
import Amazonka.Lightsail.Types.PrivateRegistryAccess
import Amazonka.Lightsail.Types.PrivateRegistryAccessRequest
import Amazonka.Lightsail.Types.QueryStringObject
import Amazonka.Lightsail.Types.R53HostedZoneDeletionState
import Amazonka.Lightsail.Types.R53HostedZoneDeletionStateCode
import Amazonka.Lightsail.Types.RecordState
import Amazonka.Lightsail.Types.RegionInfo
import Amazonka.Lightsail.Types.RegionName
import Amazonka.Lightsail.Types.RegisteredDomainDelegationInfo
import Amazonka.Lightsail.Types.RelationalDatabase
import Amazonka.Lightsail.Types.RelationalDatabaseBlueprint
import Amazonka.Lightsail.Types.RelationalDatabaseBundle
import Amazonka.Lightsail.Types.RelationalDatabaseEndpoint
import Amazonka.Lightsail.Types.RelationalDatabaseEngine
import Amazonka.Lightsail.Types.RelationalDatabaseEvent
import Amazonka.Lightsail.Types.RelationalDatabaseHardware
import Amazonka.Lightsail.Types.RelationalDatabaseMetricName
import Amazonka.Lightsail.Types.RelationalDatabaseParameter
import Amazonka.Lightsail.Types.RelationalDatabasePasswordVersion
import Amazonka.Lightsail.Types.RelationalDatabaseSnapshot
import Amazonka.Lightsail.Types.RenewalStatus
import Amazonka.Lightsail.Types.RenewalSummary
import Amazonka.Lightsail.Types.ResourceBucketAccess
import Amazonka.Lightsail.Types.ResourceBudgetEstimate
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceReceivingAccess
import Amazonka.Lightsail.Types.ResourceRecord
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Session
import Amazonka.Lightsail.Types.StaticIp
import Amazonka.Lightsail.Types.Status
import Amazonka.Lightsail.Types.StatusType
import Amazonka.Lightsail.Types.StopInstanceOnIdleRequest
import Amazonka.Lightsail.Types.Tag
import Amazonka.Lightsail.Types.TimePeriod
import Amazonka.Lightsail.Types.TreatMissingData
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Lightsail",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "lightsail",
      Core.signingName = "lightsail",
      Core.version = "2016-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Lightsail",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | Lightsail throws this exception when the user cannot be authenticated or
-- uses invalid credentials to access a resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Lightsail throws this exception when an account is still in the setup in
-- progress state.
_AccountSetupInProgressException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccountSetupInProgressException =
  Core._MatchServiceError
    defaultService
    "AccountSetupInProgressException"

-- | Lightsail throws this exception when user input does not conform to the
-- validation rules of an input field.
--
-- Domain and distribution APIs are only available in the N. Virginia
-- (@us-east-1@) Amazon Web Services Region. Please set your Amazon Web
-- Services Region configuration to @us-east-1@ to create, view, or edit
-- these resources.
_InvalidInputException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Lightsail throws this exception when it cannot find a resource.
_NotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Lightsail throws this exception when an operation fails to execute.
_OperationFailureException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_OperationFailureException =
  Core._MatchServiceError
    defaultService
    "OperationFailureException"

-- | A general service exception.
_ServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | Lightsail throws this exception when the user has not been
-- authenticated.
_UnauthenticatedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnauthenticatedException =
  Core._MatchServiceError
    defaultService
    "UnauthenticatedException"
