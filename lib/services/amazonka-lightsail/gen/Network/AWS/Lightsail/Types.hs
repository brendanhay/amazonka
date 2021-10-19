{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _AccountSetupInProgressException,
    _NotFoundException,
    _OperationFailureException,
    _ServiceException,
    _UnauthenticatedException,
    _InvalidInputException,

    -- * AccessDirection
    AccessDirection (..),

    -- * AccessType
    AccessType (..),

    -- * AddOnType
    AddOnType (..),

    -- * AlarmState
    AlarmState (..),

    -- * AutoSnapshotStatus
    AutoSnapshotStatus (..),

    -- * BehaviorEnum
    BehaviorEnum (..),

    -- * BlueprintType
    BlueprintType (..),

    -- * BucketMetricName
    BucketMetricName (..),

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

    -- * DiskSnapshotState
    DiskSnapshotState (..),

    -- * DiskState
    DiskState (..),

    -- * DistributionMetricName
    DistributionMetricName (..),

    -- * ExportSnapshotRecordSourceType
    ExportSnapshotRecordSourceType (..),

    -- * ForwardValues
    ForwardValues (..),

    -- * HeaderEnum
    HeaderEnum (..),

    -- * InstanceAccessProtocol
    InstanceAccessProtocol (..),

    -- * InstanceHealthReason
    InstanceHealthReason (..),

    -- * InstanceHealthState
    InstanceHealthState (..),

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

    -- * StatusType
    StatusType (..),

    -- * TreatMissingData
    TreatMissingData (..),

    -- * AccessKey
    AccessKey (..),
    newAccessKey,
    accessKey_status,
    accessKey_createdAt,
    accessKey_secretAccessKey,
    accessKey_lastUsed,
    accessKey_accessKeyId,

    -- * AccessKeyLastUsed
    AccessKeyLastUsed (..),
    newAccessKeyLastUsed,
    accessKeyLastUsed_lastUsedDate,
    accessKeyLastUsed_serviceName,
    accessKeyLastUsed_region,

    -- * AccessRules
    AccessRules (..),
    newAccessRules,
    accessRules_getObject,
    accessRules_allowPublicOverrides,

    -- * AddOn
    AddOn (..),
    newAddOn,
    addOn_status,
    addOn_nextSnapshotTimeOfDay,
    addOn_snapshotTimeOfDay,
    addOn_name,

    -- * AddOnRequest
    AddOnRequest (..),
    newAddOnRequest,
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_state,
    alarm_treatMissingData,
    alarm_resourceType,
    alarm_arn,
    alarm_createdAt,
    alarm_location,
    alarm_contactProtocols,
    alarm_period,
    alarm_evaluationPeriods,
    alarm_metricName,
    alarm_comparisonOperator,
    alarm_name,
    alarm_threshold,
    alarm_datapointsToAlarm,
    alarm_supportCode,
    alarm_notificationEnabled,
    alarm_notificationTriggers,
    alarm_statistic,
    alarm_unit,
    alarm_monitoredResourceInfo,

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
    autoSnapshotDetails_status,
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_state,
    availabilityZone_zoneName,

    -- * Blueprint
    Blueprint (..),
    newBlueprint,
    blueprint_versionCode,
    blueprint_platform,
    blueprint_group,
    blueprint_minPower,
    blueprint_productUrl,
    blueprint_licenseUrl,
    blueprint_name,
    blueprint_version,
    blueprint_blueprintId,
    blueprint_type,
    blueprint_isActive,
    blueprint_description,

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_state,
    bucket_objectVersioning,
    bucket_resourceType,
    bucket_arn,
    bucket_createdAt,
    bucket_location,
    bucket_readonlyAccessAccounts,
    bucket_url,
    bucket_resourcesReceivingAccess,
    bucket_bundleId,
    bucket_accessRules,
    bucket_ableToUpdateBundle,
    bucket_name,
    bucket_supportCode,
    bucket_tags,

    -- * BucketBundle
    BucketBundle (..),
    newBucketBundle,
    bucketBundle_storagePerMonthInGb,
    bucketBundle_transferPerMonthInGb,
    bucketBundle_bundleId,
    bucketBundle_name,
    bucketBundle_price,
    bucketBundle_isActive,

    -- * BucketState
    BucketState (..),
    newBucketState,
    bucketState_code,
    bucketState_message,

    -- * Bundle
    Bundle (..),
    newBundle,
    bundle_cpuCount,
    bundle_transferPerMonthInGb,
    bundle_bundleId,
    bundle_instanceType,
    bundle_name,
    bundle_power,
    bundle_diskSizeInGb,
    bundle_supportedPlatforms,
    bundle_price,
    bundle_isActive,
    bundle_ramSizeInGb,

    -- * CacheBehavior
    CacheBehavior (..),
    newCacheBehavior,
    cacheBehavior_behavior,

    -- * CacheBehaviorPerPath
    CacheBehaviorPerPath (..),
    newCacheBehaviorPerPath,
    cacheBehaviorPerPath_path,
    cacheBehaviorPerPath_behavior,

    -- * CacheSettings
    CacheSettings (..),
    newCacheSettings,
    cacheSettings_maximumTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_forwardedCookies,
    cacheSettings_allowedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_minimumTTL,
    cacheSettings_forwardedHeaders,
    cacheSettings_forwardedQueryStrings,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_status,
    certificate_subjectAlternativeNames,
    certificate_arn,
    certificate_createdAt,
    certificate_eligibleToRenew,
    certificate_requestFailureReason,
    certificate_revokedAt,
    certificate_notBefore,
    certificate_revocationReason,
    certificate_domainName,
    certificate_name,
    certificate_renewalSummary,
    certificate_supportCode,
    certificate_domainValidationRecords,
    certificate_inUseResourceCount,
    certificate_issuedAt,
    certificate_keyAlgorithm,
    certificate_serialNumber,
    certificate_issuerCA,
    certificate_tags,
    certificate_notAfter,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_certificateDetail,
    certificateSummary_certificateName,
    certificateSummary_certificateArn,
    certificateSummary_domainName,
    certificateSummary_tags,

    -- * CloudFormationStackRecord
    CloudFormationStackRecord (..),
    newCloudFormationStackRecord,
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_destinationInfo,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_sourceInfo,

    -- * CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    newCloudFormationStackRecordSourceInfo,
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_name,

    -- * ContactMethod
    ContactMethod (..),
    newContactMethod,
    contactMethod_status,
    contactMethod_resourceType,
    contactMethod_arn,
    contactMethod_createdAt,
    contactMethod_location,
    contactMethod_protocol,
    contactMethod_name,
    contactMethod_supportCode,
    contactMethod_contactEndpoint,

    -- * Container
    Container (..),
    newContainer,
    container_image,
    container_command,
    container_environment,
    container_ports,

    -- * ContainerImage
    ContainerImage (..),
    newContainerImage,
    containerImage_image,
    containerImage_createdAt,
    containerImage_digest,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_state,
    containerService_powerId,
    containerService_resourceType,
    containerService_arn,
    containerService_createdAt,
    containerService_location,
    containerService_scale,
    containerService_url,
    containerService_stateDetail,
    containerService_nextDeployment,
    containerService_principalArn,
    containerService_power,
    containerService_privateDomainName,
    containerService_isDisabled,
    containerService_publicDomainNames,
    containerService_containerServiceName,
    containerService_currentDeployment,
    containerService_tags,

    -- * ContainerServiceDeployment
    ContainerServiceDeployment (..),
    newContainerServiceDeployment,
    containerServiceDeployment_state,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_containers,
    containerServiceDeployment_version,

    -- * ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (..),
    newContainerServiceDeploymentRequest,
    containerServiceDeploymentRequest_publicEndpoint,
    containerServiceDeploymentRequest_containers,

    -- * ContainerServiceEndpoint
    ContainerServiceEndpoint (..),
    newContainerServiceEndpoint,
    containerServiceEndpoint_healthCheck,
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_containerPort,

    -- * ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    newContainerServiceHealthCheckConfig,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,
    containerServiceHealthCheckConfig_intervalSeconds,
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
    containerServicePower_powerId,
    containerServicePower_cpuCount,
    containerServicePower_name,
    containerServicePower_price,
    containerServicePower_isActive,
    containerServicePower_ramSizeInGb,

    -- * ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    newContainerServiceRegistryLogin,
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_username,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,

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

    -- * DestinationInfo
    DestinationInfo (..),
    newDestinationInfo,
    destinationInfo_service,
    destinationInfo_id,

    -- * Disk
    Disk (..),
    newDisk,
    disk_state,
    disk_resourceType,
    disk_arn,
    disk_path,
    disk_createdAt,
    disk_location,
    disk_iops,
    disk_isAttached,
    disk_addOns,
    disk_attachmentState,
    disk_name,
    disk_sizeInGb,
    disk_supportCode,
    disk_isSystemDisk,
    disk_attachedTo,
    disk_gbInUse,
    disk_tags,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_path,
    diskInfo_name,
    diskInfo_sizeInGb,
    diskInfo_isSystemDisk,

    -- * DiskMap
    DiskMap (..),
    newDiskMap,
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- * DiskSnapshot
    DiskSnapshot (..),
    newDiskSnapshot,
    diskSnapshot_fromDiskName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_state,
    diskSnapshot_resourceType,
    diskSnapshot_arn,
    diskSnapshot_createdAt,
    diskSnapshot_location,
    diskSnapshot_progress,
    diskSnapshot_name,
    diskSnapshot_sizeInGb,
    diskSnapshot_supportCode,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_fromDiskArn,
    diskSnapshot_tags,

    -- * DiskSnapshotInfo
    DiskSnapshotInfo (..),
    newDiskSnapshotInfo,
    diskSnapshotInfo_sizeInGb,

    -- * DistributionBundle
    DistributionBundle (..),
    newDistributionBundle,
    distributionBundle_transferPerMonthInGb,
    distributionBundle_bundleId,
    distributionBundle_name,
    distributionBundle_price,
    distributionBundle_isActive,

    -- * Domain
    Domain (..),
    newDomain,
    domain_resourceType,
    domain_domainEntries,
    domain_arn,
    domain_createdAt,
    domain_location,
    domain_name,
    domain_supportCode,
    domain_tags,

    -- * DomainEntry
    DomainEntry (..),
    newDomainEntry,
    domainEntry_isAlias,
    domainEntry_name,
    domainEntry_id,
    domainEntry_options,
    domainEntry_type,
    domainEntry_target,

    -- * DomainValidationRecord
    DomainValidationRecord (..),
    newDomainValidationRecord,
    domainValidationRecord_resourceRecord,
    domainValidationRecord_domainName,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- * ExportSnapshotRecord
    ExportSnapshotRecord (..),
    newExportSnapshotRecord,
    exportSnapshotRecord_state,
    exportSnapshotRecord_destinationInfo,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_location,
    exportSnapshotRecord_name,
    exportSnapshotRecord_sourceInfo,

    -- * ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    newExportSnapshotRecordSourceInfo,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,
    exportSnapshotRecordSourceInfo_fromResourceName,

    -- * HeaderObject
    HeaderObject (..),
    newHeaderObject,
    headerObject_headersAllowList,
    headerObject_option,

    -- * HostKeyAttributes
    HostKeyAttributes (..),
    newHostKeyAttributes,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_algorithm,
    hostKeyAttributes_witnessedAt,
    hostKeyAttributes_fingerprintSHA256,

    -- * InputOrigin
    InputOrigin (..),
    newInputOrigin,
    inputOrigin_regionName,
    inputOrigin_name,
    inputOrigin_protocolPolicy,

    -- * Instance
    Instance (..),
    newInstance,
    instance_state,
    instance_resourceType,
    instance_arn,
    instance_createdAt,
    instance_location,
    instance_sshKeyName,
    instance_addOns,
    instance_username,
    instance_networking,
    instance_bundleId,
    instance_name,
    instance_ipAddressType,
    instance_supportCode,
    instance_blueprintId,
    instance_privateIpAddress,
    instance_blueprintName,
    instance_isStaticIp,
    instance_publicIpAddress,
    instance_hardware,
    instance_ipv6Addresses,
    instance_tags,

    -- * InstanceAccessDetails
    InstanceAccessDetails (..),
    newInstanceAccessDetails,
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_certKey,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_username,
    instanceAccessDetails_protocol,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_password,
    instanceAccessDetails_instanceName,

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
    instanceHealthSummary_instanceName,
    instanceHealthSummary_instanceHealthReason,

    -- * InstanceNetworking
    InstanceNetworking (..),
    newInstanceNetworking,
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- * InstancePortInfo
    InstancePortInfo (..),
    newInstancePortInfo,
    instancePortInfo_fromPort,
    instancePortInfo_cidrs,
    instancePortInfo_commonName,
    instancePortInfo_protocol,
    instancePortInfo_cidrListAliases,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_accessDirection,
    instancePortInfo_accessType,
    instancePortInfo_toPort,
    instancePortInfo_accessFrom,

    -- * InstancePortState
    InstancePortState (..),
    newInstancePortState,
    instancePortState_fromPort,
    instancePortState_cidrs,
    instancePortState_state,
    instancePortState_protocol,
    instancePortState_cidrListAliases,
    instancePortState_ipv6Cidrs,
    instancePortState_toPort,

    -- * InstanceSnapshot
    InstanceSnapshot (..),
    newInstanceSnapshot,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_state,
    instanceSnapshot_resourceType,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_arn,
    instanceSnapshot_createdAt,
    instanceSnapshot_location,
    instanceSnapshot_progress,
    instanceSnapshot_name,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_supportCode,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_fromInstanceName,
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
    instanceState_name,
    instanceState_code,

    -- * KeyPair
    KeyPair (..),
    newKeyPair,
    keyPair_resourceType,
    keyPair_arn,
    keyPair_createdAt,
    keyPair_location,
    keyPair_fingerprint,
    keyPair_name,
    keyPair_supportCode,
    keyPair_tags,

    -- * LightsailDistribution
    LightsailDistribution (..),
    newLightsailDistribution,
    lightsailDistribution_status,
    lightsailDistribution_origin,
    lightsailDistribution_certificateName,
    lightsailDistribution_resourceType,
    lightsailDistribution_arn,
    lightsailDistribution_createdAt,
    lightsailDistribution_location,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_bundleId,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_domainName,
    lightsailDistribution_name,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_isEnabled,
    lightsailDistribution_supportCode,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_tags,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_healthCheckPath,
    loadBalancer_state,
    loadBalancer_resourceType,
    loadBalancer_arn,
    loadBalancer_createdAt,
    loadBalancer_location,
    loadBalancer_instancePort,
    loadBalancer_configurationOptions,
    loadBalancer_protocol,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_name,
    loadBalancer_ipAddressType,
    loadBalancer_supportCode,
    loadBalancer_publicPorts,
    loadBalancer_dnsName,
    loadBalancer_instanceHealthSummary,
    loadBalancer_tags,

    -- * LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (..),
    newLoadBalancerTlsCertificate,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_notAfter,

    -- * LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (..),
    newLoadBalancerTlsCertificateDomainValidationOption,
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- * LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (..),
    newLoadBalancerTlsCertificateDomainValidationRecord,
    loadBalancerTlsCertificateDomainValidationRecord_value,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_type,

    -- * LoadBalancerTlsCertificateRenewalSummary
    LoadBalancerTlsCertificateRenewalSummary (..),
    newLoadBalancerTlsCertificateRenewalSummary,
    loadBalancerTlsCertificateRenewalSummary_renewalStatus,
    loadBalancerTlsCertificateRenewalSummary_domainValidationOptions,

    -- * LoadBalancerTlsCertificateSummary
    LoadBalancerTlsCertificateSummary (..),
    newLoadBalancerTlsCertificateSummary,
    loadBalancerTlsCertificateSummary_isAttached,
    loadBalancerTlsCertificateSummary_name,

    -- * LogEvent
    LogEvent (..),
    newLogEvent,
    logEvent_createdAt,
    logEvent_message,

    -- * MetricDatapoint
    MetricDatapoint (..),
    newMetricDatapoint,
    metricDatapoint_sampleCount,
    metricDatapoint_maximum,
    metricDatapoint_average,
    metricDatapoint_minimum,
    metricDatapoint_sum,
    metricDatapoint_timestamp,
    metricDatapoint_unit,

    -- * MonitoredResourceInfo
    MonitoredResourceInfo (..),
    newMonitoredResourceInfo,
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_arn,
    monitoredResourceInfo_name,

    -- * MonthlyTransfer
    MonthlyTransfer (..),
    newMonthlyTransfer,
    monthlyTransfer_gbPerMonthAllocated,

    -- * Operation
    Operation (..),
    newOperation,
    operation_status,
    operation_operationDetails,
    operation_resourceType,
    operation_createdAt,
    operation_resourceName,
    operation_location,
    operation_statusChangedAt,
    operation_errorDetails,
    operation_errorCode,
    operation_id,
    operation_operationType,
    operation_isTerminal,

    -- * Origin
    Origin (..),
    newOrigin,
    origin_regionName,
    origin_resourceType,
    origin_name,
    origin_protocolPolicy,

    -- * PasswordData
    PasswordData (..),
    newPasswordData,
    passwordData_keyPairName,
    passwordData_ciphertext,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,

    -- * PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    newPendingModifiedRelationalDatabaseValues,
    pendingModifiedRelationalDatabaseValues_engineVersion,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,

    -- * PortInfo
    PortInfo (..),
    newPortInfo,
    portInfo_fromPort,
    portInfo_cidrs,
    portInfo_protocol,
    portInfo_cidrListAliases,
    portInfo_ipv6Cidrs,
    portInfo_toPort,

    -- * QueryStringObject
    QueryStringObject (..),
    newQueryStringObject,
    queryStringObject_queryStringsAllowList,
    queryStringObject_option,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_availabilityZones,
    regionInfo_name,
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_displayName,
    regionInfo_continentCode,
    regionInfo_description,

    -- * RelationalDatabase
    RelationalDatabase (..),
    newRelationalDatabase,
    relationalDatabase_engineVersion,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_masterEndpoint,
    relationalDatabase_state,
    relationalDatabase_resourceType,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_masterUsername,
    relationalDatabase_arn,
    relationalDatabase_createdAt,
    relationalDatabase_location,
    relationalDatabase_engine,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_name,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_supportCode,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_hardware,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_tags,

    -- * RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    newRelationalDatabaseBlueprint,
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineDescription,

    -- * RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    newRelationalDatabaseBundle,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_ramSizeInGb,

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
    relationalDatabaseEvent_resource,
    relationalDatabaseEvent_message,

    -- * RelationalDatabaseHardware
    RelationalDatabaseHardware (..),
    newRelationalDatabaseHardware,
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,
    relationalDatabaseHardware_ramSizeInGb,

    -- * RelationalDatabaseParameter
    RelationalDatabaseParameter (..),
    newRelationalDatabaseParameter,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_dataType,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_description,

    -- * RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    newRelationalDatabaseSnapshot,
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_tags,

    -- * RenewalSummary
    RenewalSummary (..),
    newRenewalSummary,
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationRecords,
    renewalSummary_updatedAt,
    renewalSummary_renewalStatusReason,

    -- * ResourceLocation
    ResourceLocation (..),
    newResourceLocation,
    resourceLocation_regionName,
    resourceLocation_availabilityZone,

    -- * ResourceReceivingAccess
    ResourceReceivingAccess (..),
    newResourceReceivingAccess,
    resourceReceivingAccess_resourceType,
    resourceReceivingAccess_name,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_value,
    resourceRecord_name,
    resourceRecord_type,

    -- * StaticIp
    StaticIp (..),
    newStaticIp,
    staticIp_ipAddress,
    staticIp_resourceType,
    staticIp_arn,
    staticIp_createdAt,
    staticIp_location,
    staticIp_isAttached,
    staticIp_name,
    staticIp_supportCode,
    staticIp_attachedTo,

    -- * Tag
    Tag (..),
    newTag,
    tag_value,
    tag_key,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AccessDirection
import Network.AWS.Lightsail.Types.AccessKey
import Network.AWS.Lightsail.Types.AccessKeyLastUsed
import Network.AWS.Lightsail.Types.AccessRules
import Network.AWS.Lightsail.Types.AccessType
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.AddOnRequest
import Network.AWS.Lightsail.Types.AddOnType
import Network.AWS.Lightsail.Types.Alarm
import Network.AWS.Lightsail.Types.AlarmState
import Network.AWS.Lightsail.Types.AttachedDisk
import Network.AWS.Lightsail.Types.AutoSnapshotAddOnRequest
import Network.AWS.Lightsail.Types.AutoSnapshotDetails
import Network.AWS.Lightsail.Types.AutoSnapshotStatus
import Network.AWS.Lightsail.Types.AvailabilityZone
import Network.AWS.Lightsail.Types.BehaviorEnum
import Network.AWS.Lightsail.Types.Blueprint
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.Bucket
import Network.AWS.Lightsail.Types.BucketBundle
import Network.AWS.Lightsail.Types.BucketMetricName
import Network.AWS.Lightsail.Types.BucketState
import Network.AWS.Lightsail.Types.Bundle
import Network.AWS.Lightsail.Types.CacheBehavior
import Network.AWS.Lightsail.Types.CacheBehaviorPerPath
import Network.AWS.Lightsail.Types.CacheSettings
import Network.AWS.Lightsail.Types.Certificate
import Network.AWS.Lightsail.Types.CertificateStatus
import Network.AWS.Lightsail.Types.CertificateSummary
import Network.AWS.Lightsail.Types.CloudFormationStackRecord
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceInfo
import Network.AWS.Lightsail.Types.CloudFormationStackRecordSourceType
import Network.AWS.Lightsail.Types.ComparisonOperator
import Network.AWS.Lightsail.Types.ContactMethod
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactMethodVerificationProtocol
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.Container
import Network.AWS.Lightsail.Types.ContainerImage
import Network.AWS.Lightsail.Types.ContainerService
import Network.AWS.Lightsail.Types.ContainerServiceDeployment
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentRequest
import Network.AWS.Lightsail.Types.ContainerServiceDeploymentState
import Network.AWS.Lightsail.Types.ContainerServiceEndpoint
import Network.AWS.Lightsail.Types.ContainerServiceHealthCheckConfig
import Network.AWS.Lightsail.Types.ContainerServiceLogEvent
import Network.AWS.Lightsail.Types.ContainerServiceMetricName
import Network.AWS.Lightsail.Types.ContainerServicePower
import Network.AWS.Lightsail.Types.ContainerServicePowerName
import Network.AWS.Lightsail.Types.ContainerServiceProtocol
import Network.AWS.Lightsail.Types.ContainerServiceRegistryLogin
import Network.AWS.Lightsail.Types.ContainerServiceState
import Network.AWS.Lightsail.Types.ContainerServiceStateDetail
import Network.AWS.Lightsail.Types.ContainerServiceStateDetailCode
import Network.AWS.Lightsail.Types.CookieObject
import Network.AWS.Lightsail.Types.DestinationInfo
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.DiskInfo
import Network.AWS.Lightsail.Types.DiskMap
import Network.AWS.Lightsail.Types.DiskSnapshot
import Network.AWS.Lightsail.Types.DiskSnapshotInfo
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.DistributionBundle
import Network.AWS.Lightsail.Types.DistributionMetricName
import Network.AWS.Lightsail.Types.Domain
import Network.AWS.Lightsail.Types.DomainEntry
import Network.AWS.Lightsail.Types.DomainValidationRecord
import Network.AWS.Lightsail.Types.EndpointRequest
import Network.AWS.Lightsail.Types.ExportSnapshotRecord
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Network.AWS.Lightsail.Types.ExportSnapshotRecordSourceType
import Network.AWS.Lightsail.Types.ForwardValues
import Network.AWS.Lightsail.Types.HeaderEnum
import Network.AWS.Lightsail.Types.HeaderObject
import Network.AWS.Lightsail.Types.HostKeyAttributes
import Network.AWS.Lightsail.Types.InputOrigin
import Network.AWS.Lightsail.Types.Instance
import Network.AWS.Lightsail.Types.InstanceAccessDetails
import Network.AWS.Lightsail.Types.InstanceAccessProtocol
import Network.AWS.Lightsail.Types.InstanceEntry
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceHealthReason
import Network.AWS.Lightsail.Types.InstanceHealthState
import Network.AWS.Lightsail.Types.InstanceHealthSummary
import Network.AWS.Lightsail.Types.InstanceMetricName
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstancePlatform
import Network.AWS.Lightsail.Types.InstancePortInfo
import Network.AWS.Lightsail.Types.InstancePortState
import Network.AWS.Lightsail.Types.InstanceSnapshot
import Network.AWS.Lightsail.Types.InstanceSnapshotInfo
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.IpAddressType
import Network.AWS.Lightsail.Types.KeyPair
import Network.AWS.Lightsail.Types.LightsailDistribution
import Network.AWS.Lightsail.Types.LoadBalancer
import Network.AWS.Lightsail.Types.LoadBalancerAttributeName
import Network.AWS.Lightsail.Types.LoadBalancerMetricName
import Network.AWS.Lightsail.Types.LoadBalancerProtocol
import Network.AWS.Lightsail.Types.LoadBalancerState
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificate
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateStatus
import Network.AWS.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Network.AWS.Lightsail.Types.LogEvent
import Network.AWS.Lightsail.Types.MetricDatapoint
import Network.AWS.Lightsail.Types.MetricName
import Network.AWS.Lightsail.Types.MetricStatistic
import Network.AWS.Lightsail.Types.MetricUnit
import Network.AWS.Lightsail.Types.MonitoredResourceInfo
import Network.AWS.Lightsail.Types.MonthlyTransfer
import Network.AWS.Lightsail.Types.NetworkProtocol
import Network.AWS.Lightsail.Types.Operation
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.Origin
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.PasswordData
import Network.AWS.Lightsail.Types.PendingMaintenanceAction
import Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
import Network.AWS.Lightsail.Types.PortAccessType
import Network.AWS.Lightsail.Types.PortInfo
import Network.AWS.Lightsail.Types.PortInfoSourceType
import Network.AWS.Lightsail.Types.PortState
import Network.AWS.Lightsail.Types.QueryStringObject
import Network.AWS.Lightsail.Types.RecordState
import Network.AWS.Lightsail.Types.RegionInfo
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.RelationalDatabase
import Network.AWS.Lightsail.Types.RelationalDatabaseBlueprint
import Network.AWS.Lightsail.Types.RelationalDatabaseBundle
import Network.AWS.Lightsail.Types.RelationalDatabaseEndpoint
import Network.AWS.Lightsail.Types.RelationalDatabaseEngine
import Network.AWS.Lightsail.Types.RelationalDatabaseEvent
import Network.AWS.Lightsail.Types.RelationalDatabaseHardware
import Network.AWS.Lightsail.Types.RelationalDatabaseMetricName
import Network.AWS.Lightsail.Types.RelationalDatabaseParameter
import Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
import Network.AWS.Lightsail.Types.RelationalDatabaseSnapshot
import Network.AWS.Lightsail.Types.RenewalStatus
import Network.AWS.Lightsail.Types.RenewalSummary
import Network.AWS.Lightsail.Types.ResourceBucketAccess
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceReceivingAccess
import Network.AWS.Lightsail.Types.ResourceRecord
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.StaticIp
import Network.AWS.Lightsail.Types.StatusType
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Lightsail.Types.TreatMissingData
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Lightsail",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "lightsail",
      Core._serviceSigningName = "lightsail",
      Core._serviceVersion = "2016-11-28",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Lightsail",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Lightsail throws this exception when the user cannot be authenticated or
-- uses invalid credentials to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Lightsail throws this exception when an account is still in the setup in
-- progress state.
_AccountSetupInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountSetupInProgressException =
  Core._MatchServiceError
    defaultService
    "AccountSetupInProgressException"

-- | Lightsail throws this exception when it cannot find a resource.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | Lightsail throws this exception when an operation fails to execute.
_OperationFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationFailureException =
  Core._MatchServiceError
    defaultService
    "OperationFailureException"

-- | A general service exception.
_ServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceException =
  Core._MatchServiceError
    defaultService
    "ServiceException"

-- | Lightsail throws this exception when the user has not been
-- authenticated.
_UnauthenticatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthenticatedException =
  Core._MatchServiceError
    defaultService
    "UnauthenticatedException"

-- | Lightsail throws this exception when user input does not conform to the
-- validation rules of an input field.
--
-- Domain and distribution APIs are only available in the N. Virginia
-- (@us-east-1@) AWS Region. Please set your AWS Region configuration to
-- @us-east-1@ to create, view, or edit these resources.
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"
