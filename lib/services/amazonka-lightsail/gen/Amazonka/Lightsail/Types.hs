{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OperationFailureException,
    _AccountSetupInProgressException,
    _InvalidInputException,
    _AccessDeniedException,
    _NotFoundException,
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

    -- * StatusType
    StatusType (..),

    -- * TreatMissingData
    TreatMissingData (..),

    -- * AccessKey
    AccessKey (..),
    newAccessKey,
    accessKey_status,
    accessKey_secretAccessKey,
    accessKey_lastUsed,
    accessKey_createdAt,
    accessKey_accessKeyId,

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
    accountLevelBpaSync_message,
    accountLevelBpaSync_status,
    accountLevelBpaSync_lastSyncedAt,
    accountLevelBpaSync_bpaImpactsLightsail,

    -- * AddOn
    AddOn (..),
    newAddOn,
    addOn_name,
    addOn_status,
    addOn_snapshotTimeOfDay,
    addOn_nextSnapshotTimeOfDay,

    -- * AddOnRequest
    AddOnRequest (..),
    newAddOnRequest,
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_resourceType,
    alarm_name,
    alarm_period,
    alarm_arn,
    alarm_state,
    alarm_treatMissingData,
    alarm_evaluationPeriods,
    alarm_datapointsToAlarm,
    alarm_location,
    alarm_notificationEnabled,
    alarm_metricName,
    alarm_monitoredResourceInfo,
    alarm_threshold,
    alarm_contactProtocols,
    alarm_comparisonOperator,
    alarm_supportCode,
    alarm_statistic,
    alarm_unit,
    alarm_notificationTriggers,
    alarm_createdAt,

    -- * AttachedDisk
    AttachedDisk (..),
    newAttachedDisk,
    attachedDisk_sizeInGb,
    attachedDisk_path,

    -- * AutoSnapshotAddOnRequest
    AutoSnapshotAddOnRequest (..),
    newAutoSnapshotAddOnRequest,
    autoSnapshotAddOnRequest_snapshotTimeOfDay,

    -- * AutoSnapshotDetails
    AutoSnapshotDetails (..),
    newAutoSnapshotDetails,
    autoSnapshotDetails_fromAttachedDisks,
    autoSnapshotDetails_date,
    autoSnapshotDetails_status,
    autoSnapshotDetails_createdAt,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_zoneName,
    availabilityZone_state,

    -- * Blueprint
    Blueprint (..),
    newBlueprint,
    blueprint_minPower,
    blueprint_isActive,
    blueprint_versionCode,
    blueprint_name,
    blueprint_type,
    blueprint_licenseUrl,
    blueprint_blueprintId,
    blueprint_description,
    blueprint_platform,
    blueprint_productUrl,
    blueprint_group,
    blueprint_version,

    -- * Bucket
    Bucket (..),
    newBucket,
    bucket_tags,
    bucket_resourceType,
    bucket_resourcesReceivingAccess,
    bucket_name,
    bucket_readonlyAccessAccounts,
    bucket_arn,
    bucket_state,
    bucket_url,
    bucket_location,
    bucket_ableToUpdateBundle,
    bucket_bundleId,
    bucket_objectVersioning,
    bucket_accessRules,
    bucket_supportCode,
    bucket_createdAt,
    bucket_accessLogConfig,

    -- * BucketAccessLogConfig
    BucketAccessLogConfig (..),
    newBucketAccessLogConfig,
    bucketAccessLogConfig_destination,
    bucketAccessLogConfig_prefix,
    bucketAccessLogConfig_enabled,

    -- * BucketBundle
    BucketBundle (..),
    newBucketBundle,
    bucketBundle_isActive,
    bucketBundle_name,
    bucketBundle_transferPerMonthInGb,
    bucketBundle_storagePerMonthInGb,
    bucketBundle_price,
    bucketBundle_bundleId,

    -- * BucketState
    BucketState (..),
    newBucketState,
    bucketState_message,
    bucketState_code,

    -- * Bundle
    Bundle (..),
    newBundle,
    bundle_cpuCount,
    bundle_isActive,
    bundle_name,
    bundle_power,
    bundle_transferPerMonthInGb,
    bundle_diskSizeInGb,
    bundle_instanceType,
    bundle_price,
    bundle_bundleId,
    bundle_supportedPlatforms,
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
    cacheSettings_allowedHTTPMethods,
    cacheSettings_defaultTTL,
    cacheSettings_forwardedCookies,
    cacheSettings_maximumTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_forwardedHeaders,
    cacheSettings_minimumTTL,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_tags,
    certificate_name,
    certificate_domainName,
    certificate_arn,
    certificate_renewalSummary,
    certificate_keyAlgorithm,
    certificate_requestFailureReason,
    certificate_status,
    certificate_inUseResourceCount,
    certificate_notBefore,
    certificate_revocationReason,
    certificate_serialNumber,
    certificate_domainValidationRecords,
    certificate_eligibleToRenew,
    certificate_revokedAt,
    certificate_supportCode,
    certificate_issuerCA,
    certificate_notAfter,
    certificate_subjectAlternativeNames,
    certificate_createdAt,
    certificate_issuedAt,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_tags,
    certificateSummary_domainName,
    certificateSummary_certificateName,
    certificateSummary_certificateArn,
    certificateSummary_certificateDetail,

    -- * CloudFormationStackRecord
    CloudFormationStackRecord (..),
    newCloudFormationStackRecord,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_destinationInfo,

    -- * CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    newCloudFormationStackRecordSourceInfo,
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_name,
    cloudFormationStackRecordSourceInfo_arn,

    -- * ContactMethod
    ContactMethod (..),
    newContactMethod,
    contactMethod_resourceType,
    contactMethod_name,
    contactMethod_contactEndpoint,
    contactMethod_arn,
    contactMethod_status,
    contactMethod_location,
    contactMethod_protocol,
    contactMethod_supportCode,
    contactMethod_createdAt,

    -- * Container
    Container (..),
    newContainer,
    container_environment,
    container_ports,
    container_command,
    container_image,

    -- * ContainerImage
    ContainerImage (..),
    newContainerImage,
    containerImage_digest,
    containerImage_image,
    containerImage_createdAt,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_tags,
    containerService_resourceType,
    containerService_power,
    containerService_stateDetail,
    containerService_currentDeployment,
    containerService_privateDomainName,
    containerService_principalArn,
    containerService_arn,
    containerService_state,
    containerService_url,
    containerService_location,
    containerService_containerServiceName,
    containerService_powerId,
    containerService_scale,
    containerService_createdAt,
    containerService_publicDomainNames,
    containerService_privateRegistryAccess,
    containerService_isDisabled,
    containerService_nextDeployment,

    -- * ContainerServiceDeployment
    ContainerServiceDeployment (..),
    newContainerServiceDeployment,
    containerServiceDeployment_containers,
    containerServiceDeployment_state,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_createdAt,
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
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_healthCheck,
    containerServiceEndpoint_containerName,

    -- * ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    newContainerServiceHealthCheckConfig,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_unhealthyThreshold,
    containerServiceHealthCheckConfig_successCodes,

    -- * ContainerServiceLogEvent
    ContainerServiceLogEvent (..),
    newContainerServiceLogEvent,
    containerServiceLogEvent_message,
    containerServiceLogEvent_createdAt,

    -- * ContainerServicePower
    ContainerServicePower (..),
    newContainerServicePower,
    containerServicePower_cpuCount,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_price,
    containerServicePower_powerId,
    containerServicePower_ramSizeInGb,

    -- * ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    newContainerServiceRegistryLogin,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_username,
    containerServiceRegistryLogin_expiresAt,

    -- * ContainerServiceStateDetail
    ContainerServiceStateDetail (..),
    newContainerServiceStateDetail,
    containerServiceStateDetail_message,
    containerServiceStateDetail_code,

    -- * CookieObject
    CookieObject (..),
    newCookieObject,
    cookieObject_cookiesAllowList,
    cookieObject_option,

    -- * DestinationInfo
    DestinationInfo (..),
    newDestinationInfo,
    destinationInfo_id,
    destinationInfo_service,

    -- * Disk
    Disk (..),
    newDisk,
    disk_tags,
    disk_resourceType,
    disk_name,
    disk_gbInUse,
    disk_sizeInGb,
    disk_attachedTo,
    disk_arn,
    disk_state,
    disk_path,
    disk_location,
    disk_isAttached,
    disk_addOns,
    disk_isSystemDisk,
    disk_attachmentState,
    disk_supportCode,
    disk_iops,
    disk_createdAt,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_name,
    diskInfo_sizeInGb,
    diskInfo_path,
    diskInfo_isSystemDisk,

    -- * DiskMap
    DiskMap (..),
    newDiskMap,
    diskMap_newDiskName,
    diskMap_originalDiskPath,

    -- * DiskSnapshot
    DiskSnapshot (..),
    newDiskSnapshot,
    diskSnapshot_tags,
    diskSnapshot_progress,
    diskSnapshot_resourceType,
    diskSnapshot_name,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_sizeInGb,
    diskSnapshot_fromDiskName,
    diskSnapshot_arn,
    diskSnapshot_state,
    diskSnapshot_fromDiskArn,
    diskSnapshot_location,
    diskSnapshot_fromInstanceName,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_supportCode,
    diskSnapshot_createdAt,

    -- * DiskSnapshotInfo
    DiskSnapshotInfo (..),
    newDiskSnapshotInfo,
    diskSnapshotInfo_sizeInGb,

    -- * DistributionBundle
    DistributionBundle (..),
    newDistributionBundle,
    distributionBundle_isActive,
    distributionBundle_name,
    distributionBundle_transferPerMonthInGb,
    distributionBundle_price,
    distributionBundle_bundleId,

    -- * DnsRecordCreationState
    DnsRecordCreationState (..),
    newDnsRecordCreationState,
    dnsRecordCreationState_message,
    dnsRecordCreationState_code,

    -- * Domain
    Domain (..),
    newDomain,
    domain_tags,
    domain_resourceType,
    domain_name,
    domain_registeredDomainDelegationInfo,
    domain_arn,
    domain_location,
    domain_domainEntries,
    domain_supportCode,
    domain_createdAt,

    -- * DomainEntry
    DomainEntry (..),
    newDomainEntry,
    domainEntry_name,
    domainEntry_type,
    domainEntry_isAlias,
    domainEntry_target,
    domainEntry_id,
    domainEntry_options,

    -- * DomainValidationRecord
    DomainValidationRecord (..),
    newDomainValidationRecord,
    domainValidationRecord_dnsRecordCreationState,
    domainValidationRecord_domainName,
    domainValidationRecord_validationStatus,
    domainValidationRecord_resourceRecord,

    -- * EndpointRequest
    EndpointRequest (..),
    newEndpointRequest,
    endpointRequest_healthCheck,
    endpointRequest_containerName,
    endpointRequest_containerPort,

    -- * ExportSnapshotRecord
    ExportSnapshotRecord (..),
    newExportSnapshotRecord,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_name,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_state,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_destinationInfo,

    -- * ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    newExportSnapshotRecordSourceInfo,
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_fromResourceName,
    exportSnapshotRecordSourceInfo_fromResourceArn,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_instanceSnapshotInfo,

    -- * HeaderObject
    HeaderObject (..),
    newHeaderObject,
    headerObject_option,
    headerObject_headersAllowList,

    -- * HostKeyAttributes
    HostKeyAttributes (..),
    newHostKeyAttributes,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_witnessedAt,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_algorithm,
    hostKeyAttributes_fingerprintSHA256,

    -- * InputOrigin
    InputOrigin (..),
    newInputOrigin,
    inputOrigin_name,
    inputOrigin_protocolPolicy,
    inputOrigin_regionName,

    -- * Instance
    Instance (..),
    newInstance,
    instance_tags,
    instance_resourceType,
    instance_name,
    instance_username,
    instance_blueprintId,
    instance_arn,
    instance_state,
    instance_isStaticIp,
    instance_location,
    instance_hardware,
    instance_publicIpAddress,
    instance_bundleId,
    instance_privateIpAddress,
    instance_sshKeyName,
    instance_addOns,
    instance_supportCode,
    instance_blueprintName,
    instance_ipAddressType,
    instance_networking,
    instance_createdAt,
    instance_ipv6Addresses,
    instance_metadataOptions,

    -- * InstanceAccessDetails
    InstanceAccessDetails (..),
    newInstanceAccessDetails,
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_password,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_username,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_certKey,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_protocol,
    instanceAccessDetails_ipAddress,

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
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceHealth,
    instanceHealthSummary_instanceName,

    -- * InstanceMetadataOptions
    InstanceMetadataOptions (..),
    newInstanceMetadataOptions,
    instanceMetadataOptions_httpPutResponseHopLimit,
    instanceMetadataOptions_state,
    instanceMetadataOptions_httpTokens,
    instanceMetadataOptions_httpEndpoint,
    instanceMetadataOptions_httpProtocolIpv6,

    -- * InstanceNetworking
    InstanceNetworking (..),
    newInstanceNetworking,
    instanceNetworking_ports,
    instanceNetworking_monthlyTransfer,

    -- * InstancePortInfo
    InstancePortInfo (..),
    newInstancePortInfo,
    instancePortInfo_accessDirection,
    instancePortInfo_toPort,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_cidrListAliases,
    instancePortInfo_cidrs,
    instancePortInfo_accessFrom,
    instancePortInfo_commonName,
    instancePortInfo_protocol,
    instancePortInfo_accessType,
    instancePortInfo_fromPort,

    -- * InstancePortState
    InstancePortState (..),
    newInstancePortState,
    instancePortState_toPort,
    instancePortState_ipv6Cidrs,
    instancePortState_cidrListAliases,
    instancePortState_cidrs,
    instancePortState_state,
    instancePortState_protocol,
    instancePortState_fromPort,

    -- * InstanceSnapshot
    InstanceSnapshot (..),
    newInstanceSnapshot,
    instanceSnapshot_tags,
    instanceSnapshot_progress,
    instanceSnapshot_fromAttachedDisks,
    instanceSnapshot_resourceType,
    instanceSnapshot_name,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_arn,
    instanceSnapshot_state,
    instanceSnapshot_location,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_supportCode,
    instanceSnapshot_createdAt,

    -- * InstanceSnapshotInfo
    InstanceSnapshotInfo (..),
    newInstanceSnapshotInfo,
    instanceSnapshotInfo_fromDiskInfo,
    instanceSnapshotInfo_fromBlueprintId,
    instanceSnapshotInfo_fromBundleId,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_name,
    instanceState_code,

    -- * KeyPair
    KeyPair (..),
    newKeyPair,
    keyPair_tags,
    keyPair_resourceType,
    keyPair_name,
    keyPair_arn,
    keyPair_location,
    keyPair_fingerprint,
    keyPair_supportCode,
    keyPair_createdAt,

    -- * LightsailDistribution
    LightsailDistribution (..),
    newLightsailDistribution,
    lightsailDistribution_tags,
    lightsailDistribution_resourceType,
    lightsailDistribution_name,
    lightsailDistribution_isEnabled,
    lightsailDistribution_domainName,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_arn,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_status,
    lightsailDistribution_certificateName,
    lightsailDistribution_location,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_bundleId,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_supportCode,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_origin,
    lightsailDistribution_createdAt,
    lightsailDistribution_defaultCacheBehavior,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_tags,
    loadBalancer_resourceType,
    loadBalancer_name,
    loadBalancer_httpsRedirectionEnabled,
    loadBalancer_healthCheckPath,
    loadBalancer_instanceHealthSummary,
    loadBalancer_configurationOptions,
    loadBalancer_arn,
    loadBalancer_state,
    loadBalancer_publicPorts,
    loadBalancer_location,
    loadBalancer_instancePort,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_protocol,
    loadBalancer_dnsName,
    loadBalancer_supportCode,
    loadBalancer_ipAddressType,
    loadBalancer_createdAt,
    loadBalancer_tlsPolicyName,

    -- * LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (..),
    newLoadBalancerTlsCertificate,
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_renewalSummary,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_issuedAt,

    -- * LoadBalancerTlsCertificateDnsRecordCreationState
    LoadBalancerTlsCertificateDnsRecordCreationState (..),
    newLoadBalancerTlsCertificateDnsRecordCreationState,
    loadBalancerTlsCertificateDnsRecordCreationState_message,
    loadBalancerTlsCertificateDnsRecordCreationState_code,

    -- * LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (..),
    newLoadBalancerTlsCertificateDomainValidationOption,
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- * LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (..),
    newLoadBalancerTlsCertificateDomainValidationRecord,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_type,
    loadBalancerTlsCertificateDomainValidationRecord_dnsRecordCreationState,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
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
    loadBalancerTlsCertificateSummary_name,
    loadBalancerTlsCertificateSummary_isAttached,

    -- * LoadBalancerTlsPolicy
    LoadBalancerTlsPolicy (..),
    newLoadBalancerTlsPolicy,
    loadBalancerTlsPolicy_name,
    loadBalancerTlsPolicy_protocols,
    loadBalancerTlsPolicy_description,
    loadBalancerTlsPolicy_isDefault,
    loadBalancerTlsPolicy_ciphers,

    -- * LogEvent
    LogEvent (..),
    newLogEvent,
    logEvent_message,
    logEvent_createdAt,

    -- * MetricDatapoint
    MetricDatapoint (..),
    newMetricDatapoint,
    metricDatapoint_minimum,
    metricDatapoint_average,
    metricDatapoint_timestamp,
    metricDatapoint_sampleCount,
    metricDatapoint_sum,
    metricDatapoint_maximum,
    metricDatapoint_unit,

    -- * MonitoredResourceInfo
    MonitoredResourceInfo (..),
    newMonitoredResourceInfo,
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_name,
    monitoredResourceInfo_arn,

    -- * MonthlyTransfer
    MonthlyTransfer (..),
    newMonthlyTransfer,
    monthlyTransfer_gbPerMonthAllocated,

    -- * NameServersUpdateState
    NameServersUpdateState (..),
    newNameServersUpdateState,
    nameServersUpdateState_message,
    nameServersUpdateState_code,

    -- * Operation
    Operation (..),
    newOperation,
    operation_resourceType,
    operation_statusChangedAt,
    operation_isTerminal,
    operation_resourceName,
    operation_operationType,
    operation_errorDetails,
    operation_status,
    operation_id,
    operation_location,
    operation_errorCode,
    operation_operationDetails,
    operation_createdAt,

    -- * Origin
    Origin (..),
    newOrigin,
    origin_resourceType,
    origin_name,
    origin_protocolPolicy,
    origin_regionName,

    -- * PasswordData
    PasswordData (..),
    newPasswordData,
    passwordData_ciphertext,
    passwordData_keyPairName,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_description,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,

    -- * PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    newPendingModifiedRelationalDatabaseValues,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_engineVersion,

    -- * PortInfo
    PortInfo (..),
    newPortInfo,
    portInfo_toPort,
    portInfo_ipv6Cidrs,
    portInfo_cidrListAliases,
    portInfo_cidrs,
    portInfo_protocol,
    portInfo_fromPort,

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
    queryStringObject_queryStringsAllowList,
    queryStringObject_option,

    -- * R53HostedZoneDeletionState
    R53HostedZoneDeletionState (..),
    newR53HostedZoneDeletionState,
    r53HostedZoneDeletionState_message,
    r53HostedZoneDeletionState_code,

    -- * RegionInfo
    RegionInfo (..),
    newRegionInfo,
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_name,
    regionInfo_availabilityZones,
    regionInfo_displayName,
    regionInfo_description,
    regionInfo_continentCode,

    -- * RegisteredDomainDelegationInfo
    RegisteredDomainDelegationInfo (..),
    newRegisteredDomainDelegationInfo,
    registeredDomainDelegationInfo_r53HostedZoneDeletionState,
    registeredDomainDelegationInfo_nameServersUpdateState,

    -- * RelationalDatabase
    RelationalDatabase (..),
    newRelationalDatabase,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_tags,
    relationalDatabase_resourceType,
    relationalDatabase_name,
    relationalDatabase_masterUsername,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_arn,
    relationalDatabase_state,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_location,
    relationalDatabase_hardware,
    relationalDatabase_masterEndpoint,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_engine,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_supportCode,
    relationalDatabase_createdAt,
    relationalDatabase_engineVersion,

    -- * RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    newRelationalDatabaseBlueprint,
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_engine,
    relationalDatabaseBlueprint_engineVersion,

    -- * RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    newRelationalDatabaseBundle,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_diskSizeInGb,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_ramSizeInGb,

    -- * RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (..),
    newRelationalDatabaseEndpoint,
    relationalDatabaseEndpoint_port,
    relationalDatabaseEndpoint_address,

    -- * RelationalDatabaseEvent
    RelationalDatabaseEvent (..),
    newRelationalDatabaseEvent,
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_createdAt,
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
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_dataType,

    -- * RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    newRelationalDatabaseSnapshot,
    relationalDatabaseSnapshot_tags,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_engineVersion,

    -- * RenewalSummary
    RenewalSummary (..),
    newRenewalSummary,
    renewalSummary_renewalStatusReason,
    renewalSummary_renewalStatus,
    renewalSummary_domainValidationRecords,
    renewalSummary_updatedAt,

    -- * ResourceLocation
    ResourceLocation (..),
    newResourceLocation,
    resourceLocation_availabilityZone,
    resourceLocation_regionName,

    -- * ResourceReceivingAccess
    ResourceReceivingAccess (..),
    newResourceReceivingAccess,
    resourceReceivingAccess_resourceType,
    resourceReceivingAccess_name,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_name,
    resourceRecord_type,
    resourceRecord_value,

    -- * StaticIp
    StaticIp (..),
    newStaticIp,
    staticIp_resourceType,
    staticIp_name,
    staticIp_attachedTo,
    staticIp_arn,
    staticIp_location,
    staticIp_isAttached,
    staticIp_supportCode,
    staticIp_createdAt,
    staticIp_ipAddress,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
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
import Amazonka.Lightsail.Types.AttachedDisk
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Lightsail throws this exception when an operation fails to execute.
_OperationFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OperationFailureException =
  Core._MatchServiceError
    defaultService
    "OperationFailureException"

-- | Lightsail throws this exception when an account is still in the setup in
-- progress state.
_AccountSetupInProgressException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
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
_InvalidInputException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidInputException =
  Core._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Lightsail throws this exception when the user cannot be authenticated or
-- uses invalid credentials to access a resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Lightsail throws this exception when it cannot find a resource.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

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
