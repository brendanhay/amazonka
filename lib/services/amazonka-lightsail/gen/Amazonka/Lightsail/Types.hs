{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.AccessDirection
import Amazonka.Lightsail.Types.AccessKey
import Amazonka.Lightsail.Types.AccessKeyLastUsed
import Amazonka.Lightsail.Types.AccessRules
import Amazonka.Lightsail.Types.AccessType
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.AddOnRequest
import Amazonka.Lightsail.Types.AddOnType
import Amazonka.Lightsail.Types.Alarm
import Amazonka.Lightsail.Types.AlarmState
import Amazonka.Lightsail.Types.AttachedDisk
import Amazonka.Lightsail.Types.AutoSnapshotAddOnRequest
import Amazonka.Lightsail.Types.AutoSnapshotDetails
import Amazonka.Lightsail.Types.AutoSnapshotStatus
import Amazonka.Lightsail.Types.AvailabilityZone
import Amazonka.Lightsail.Types.BehaviorEnum
import Amazonka.Lightsail.Types.Blueprint
import Amazonka.Lightsail.Types.BlueprintType
import Amazonka.Lightsail.Types.Bucket
import Amazonka.Lightsail.Types.BucketBundle
import Amazonka.Lightsail.Types.BucketMetricName
import Amazonka.Lightsail.Types.BucketState
import Amazonka.Lightsail.Types.Bundle
import Amazonka.Lightsail.Types.CacheBehavior
import Amazonka.Lightsail.Types.CacheBehaviorPerPath
import Amazonka.Lightsail.Types.CacheSettings
import Amazonka.Lightsail.Types.Certificate
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
import Amazonka.Lightsail.Types.Domain
import Amazonka.Lightsail.Types.DomainEntry
import Amazonka.Lightsail.Types.DomainValidationRecord
import Amazonka.Lightsail.Types.EndpointRequest
import Amazonka.Lightsail.Types.ExportSnapshotRecord
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceInfo
import Amazonka.Lightsail.Types.ExportSnapshotRecordSourceType
import Amazonka.Lightsail.Types.ForwardValues
import Amazonka.Lightsail.Types.HeaderEnum
import Amazonka.Lightsail.Types.HeaderObject
import Amazonka.Lightsail.Types.HostKeyAttributes
import Amazonka.Lightsail.Types.InputOrigin
import Amazonka.Lightsail.Types.Instance
import Amazonka.Lightsail.Types.InstanceAccessDetails
import Amazonka.Lightsail.Types.InstanceAccessProtocol
import Amazonka.Lightsail.Types.InstanceEntry
import Amazonka.Lightsail.Types.InstanceHardware
import Amazonka.Lightsail.Types.InstanceHealthReason
import Amazonka.Lightsail.Types.InstanceHealthState
import Amazonka.Lightsail.Types.InstanceHealthSummary
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
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationOption
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateDomainValidationRecord
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateFailureReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRenewalSummary
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateRevocationReason
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateStatus
import Amazonka.Lightsail.Types.LoadBalancerTlsCertificateSummary
import Amazonka.Lightsail.Types.LogEvent
import Amazonka.Lightsail.Types.MetricDatapoint
import Amazonka.Lightsail.Types.MetricName
import Amazonka.Lightsail.Types.MetricStatistic
import Amazonka.Lightsail.Types.MetricUnit
import Amazonka.Lightsail.Types.MonitoredResourceInfo
import Amazonka.Lightsail.Types.MonthlyTransfer
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
import Amazonka.Lightsail.Types.QueryStringObject
import Amazonka.Lightsail.Types.RecordState
import Amazonka.Lightsail.Types.RegionInfo
import Amazonka.Lightsail.Types.RegionName
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
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceReceivingAccess
import Amazonka.Lightsail.Types.ResourceRecord
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.StaticIp
import Amazonka.Lightsail.Types.StatusType
import Amazonka.Lightsail.Types.Tag
import Amazonka.Lightsail.Types.TreatMissingData
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

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
