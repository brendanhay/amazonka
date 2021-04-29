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
    _NotFoundException,
    _InvalidInputException,
    _UnauthenticatedException,
    _OperationFailureException,
    _AccessDeniedException,
    _AccountSetupInProgressException,
    _ServiceException,

    -- * AccessDirection
    AccessDirection (..),

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

    -- * ResourceType
    ResourceType (..),

    -- * TreatMissingData
    TreatMissingData (..),

    -- * AddOn
    AddOn (..),
    newAddOn,
    addOn_snapshotTimeOfDay,
    addOn_status,
    addOn_name,
    addOn_nextSnapshotTimeOfDay,

    -- * AddOnRequest
    AddOnRequest (..),
    newAddOnRequest,
    addOnRequest_autoSnapshotAddOnRequest,
    addOnRequest_addOnType,

    -- * Alarm
    Alarm (..),
    newAlarm,
    alarm_datapointsToAlarm,
    alarm_threshold,
    alarm_comparisonOperator,
    alarm_monitoredResourceInfo,
    alarm_notificationTriggers,
    alarm_unit,
    alarm_metricName,
    alarm_notificationEnabled,
    alarm_createdAt,
    alarm_arn,
    alarm_treatMissingData,
    alarm_resourceType,
    alarm_supportCode,
    alarm_state,
    alarm_name,
    alarm_statistic,
    alarm_evaluationPeriods,
    alarm_period,
    alarm_location,
    alarm_contactProtocols,

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
    autoSnapshotDetails_status,
    autoSnapshotDetails_createdAt,
    autoSnapshotDetails_date,
    autoSnapshotDetails_fromAttachedDisks,

    -- * AvailabilityZone
    AvailabilityZone (..),
    newAvailabilityZone,
    availabilityZone_zoneName,
    availabilityZone_state,

    -- * Blueprint
    Blueprint (..),
    newBlueprint,
    blueprint_platform,
    blueprint_isActive,
    blueprint_licenseUrl,
    blueprint_productUrl,
    blueprint_version,
    blueprint_blueprintId,
    blueprint_versionCode,
    blueprint_name,
    blueprint_group,
    blueprint_description,
    blueprint_type,
    blueprint_minPower,

    -- * Bundle
    Bundle (..),
    newBundle,
    bundle_power,
    bundle_instanceType,
    bundle_ramSizeInGb,
    bundle_bundleId,
    bundle_isActive,
    bundle_name,
    bundle_transferPerMonthInGb,
    bundle_cpuCount,
    bundle_price,
    bundle_supportedPlatforms,
    bundle_diskSizeInGb,

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
    cacheSettings_maximumTTL,
    cacheSettings_forwardedHeaders,
    cacheSettings_defaultTTL,
    cacheSettings_cachedHTTPMethods,
    cacheSettings_allowedHTTPMethods,
    cacheSettings_forwardedQueryStrings,
    cacheSettings_forwardedCookies,
    cacheSettings_minimumTTL,

    -- * Certificate
    Certificate (..),
    newCertificate,
    certificate_status,
    certificate_notBefore,
    certificate_inUseResourceCount,
    certificate_createdAt,
    certificate_arn,
    certificate_eligibleToRenew,
    certificate_supportCode,
    certificate_subjectAlternativeNames,
    certificate_name,
    certificate_domainName,
    certificate_revocationReason,
    certificate_revokedAt,
    certificate_issuerCA,
    certificate_notAfter,
    certificate_tags,
    certificate_serialNumber,
    certificate_issuedAt,
    certificate_keyAlgorithm,
    certificate_domainValidationRecords,
    certificate_requestFailureReason,
    certificate_renewalSummary,

    -- * CertificateSummary
    CertificateSummary (..),
    newCertificateSummary,
    certificateSummary_certificateArn,
    certificateSummary_domainName,
    certificateSummary_certificateDetail,
    certificateSummary_tags,
    certificateSummary_certificateName,

    -- * CloudFormationStackRecord
    CloudFormationStackRecord (..),
    newCloudFormationStackRecord,
    cloudFormationStackRecord_createdAt,
    cloudFormationStackRecord_arn,
    cloudFormationStackRecord_resourceType,
    cloudFormationStackRecord_state,
    cloudFormationStackRecord_name,
    cloudFormationStackRecord_sourceInfo,
    cloudFormationStackRecord_location,
    cloudFormationStackRecord_destinationInfo,

    -- * CloudFormationStackRecordSourceInfo
    CloudFormationStackRecordSourceInfo (..),
    newCloudFormationStackRecordSourceInfo,
    cloudFormationStackRecordSourceInfo_arn,
    cloudFormationStackRecordSourceInfo_resourceType,
    cloudFormationStackRecordSourceInfo_name,

    -- * ContactMethod
    ContactMethod (..),
    newContactMethod,
    contactMethod_status,
    contactMethod_contactEndpoint,
    contactMethod_createdAt,
    contactMethod_arn,
    contactMethod_resourceType,
    contactMethod_supportCode,
    contactMethod_name,
    contactMethod_protocol,
    contactMethod_location,

    -- * Container
    Container (..),
    newContainer,
    container_environment,
    container_ports,
    container_image,
    container_command,

    -- * ContainerImage
    ContainerImage (..),
    newContainerImage,
    containerImage_createdAt,
    containerImage_image,
    containerImage_digest,

    -- * ContainerService
    ContainerService (..),
    newContainerService,
    containerService_power,
    containerService_currentDeployment,
    containerService_createdAt,
    containerService_arn,
    containerService_containerServiceName,
    containerService_privateDomainName,
    containerService_resourceType,
    containerService_state,
    containerService_principalArn,
    containerService_tags,
    containerService_nextDeployment,
    containerService_url,
    containerService_scale,
    containerService_publicDomainNames,
    containerService_location,
    containerService_powerId,
    containerService_isDisabled,

    -- * ContainerServiceDeployment
    ContainerServiceDeployment (..),
    newContainerServiceDeployment,
    containerServiceDeployment_createdAt,
    containerServiceDeployment_version,
    containerServiceDeployment_publicEndpoint,
    containerServiceDeployment_state,
    containerServiceDeployment_containers,

    -- * ContainerServiceDeploymentRequest
    ContainerServiceDeploymentRequest (..),
    newContainerServiceDeploymentRequest,
    containerServiceDeploymentRequest_publicEndpoint,
    containerServiceDeploymentRequest_containers,

    -- * ContainerServiceEndpoint
    ContainerServiceEndpoint (..),
    newContainerServiceEndpoint,
    containerServiceEndpoint_containerPort,
    containerServiceEndpoint_containerName,
    containerServiceEndpoint_healthCheck,

    -- * ContainerServiceHealthCheckConfig
    ContainerServiceHealthCheckConfig (..),
    newContainerServiceHealthCheckConfig,
    containerServiceHealthCheckConfig_intervalSeconds,
    containerServiceHealthCheckConfig_healthyThreshold,
    containerServiceHealthCheckConfig_unhealthyThreshold,
    containerServiceHealthCheckConfig_timeoutSeconds,
    containerServiceHealthCheckConfig_path,
    containerServiceHealthCheckConfig_successCodes,

    -- * ContainerServiceLogEvent
    ContainerServiceLogEvent (..),
    newContainerServiceLogEvent,
    containerServiceLogEvent_message,
    containerServiceLogEvent_createdAt,

    -- * ContainerServicePower
    ContainerServicePower (..),
    newContainerServicePower,
    containerServicePower_ramSizeInGb,
    containerServicePower_isActive,
    containerServicePower_name,
    containerServicePower_cpuCount,
    containerServicePower_price,
    containerServicePower_powerId,

    -- * ContainerServiceRegistryLogin
    ContainerServiceRegistryLogin (..),
    newContainerServiceRegistryLogin,
    containerServiceRegistryLogin_expiresAt,
    containerServiceRegistryLogin_registry,
    containerServiceRegistryLogin_password,
    containerServiceRegistryLogin_username,

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
    disk_gbInUse,
    disk_attachmentState,
    disk_addOns,
    disk_isAttached,
    disk_createdAt,
    disk_arn,
    disk_resourceType,
    disk_supportCode,
    disk_sizeInGb,
    disk_state,
    disk_name,
    disk_attachedTo,
    disk_tags,
    disk_iops,
    disk_location,
    disk_path,
    disk_isSystemDisk,

    -- * DiskInfo
    DiskInfo (..),
    newDiskInfo,
    diskInfo_sizeInGb,
    diskInfo_name,
    diskInfo_path,
    diskInfo_isSystemDisk,

    -- * DiskMap
    DiskMap (..),
    newDiskMap,
    diskMap_originalDiskPath,
    diskMap_newDiskName,

    -- * DiskSnapshot
    DiskSnapshot (..),
    newDiskSnapshot,
    diskSnapshot_isFromAutoSnapshot,
    diskSnapshot_fromDiskName,
    diskSnapshot_createdAt,
    diskSnapshot_arn,
    diskSnapshot_fromInstanceArn,
    diskSnapshot_resourceType,
    diskSnapshot_supportCode,
    diskSnapshot_sizeInGb,
    diskSnapshot_state,
    diskSnapshot_name,
    diskSnapshot_tags,
    diskSnapshot_fromDiskArn,
    diskSnapshot_fromInstanceName,
    diskSnapshot_location,
    diskSnapshot_progress,

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
    distributionBundle_transferPerMonthInGb,
    distributionBundle_price,

    -- * Domain
    Domain (..),
    newDomain,
    domain_createdAt,
    domain_arn,
    domain_resourceType,
    domain_supportCode,
    domain_name,
    domain_tags,
    domain_location,
    domain_domainEntries,

    -- * DomainEntry
    DomainEntry (..),
    newDomainEntry,
    domainEntry_options,
    domainEntry_id,
    domainEntry_name,
    domainEntry_isAlias,
    domainEntry_target,
    domainEntry_type,

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
    exportSnapshotRecord_createdAt,
    exportSnapshotRecord_arn,
    exportSnapshotRecord_resourceType,
    exportSnapshotRecord_state,
    exportSnapshotRecord_name,
    exportSnapshotRecord_sourceInfo,
    exportSnapshotRecord_location,
    exportSnapshotRecord_destinationInfo,

    -- * ExportSnapshotRecordSourceInfo
    ExportSnapshotRecordSourceInfo (..),
    newExportSnapshotRecordSourceInfo,
    exportSnapshotRecordSourceInfo_diskSnapshotInfo,
    exportSnapshotRecordSourceInfo_createdAt,
    exportSnapshotRecordSourceInfo_arn,
    exportSnapshotRecordSourceInfo_resourceType,
    exportSnapshotRecordSourceInfo_name,
    exportSnapshotRecordSourceInfo_fromResourceArn,
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
    hostKeyAttributes_algorithm,
    hostKeyAttributes_publicKey,
    hostKeyAttributes_fingerprintSHA256,
    hostKeyAttributes_notValidBefore,
    hostKeyAttributes_notValidAfter,
    hostKeyAttributes_fingerprintSHA1,
    hostKeyAttributes_witnessedAt,

    -- * InputOrigin
    InputOrigin (..),
    newInputOrigin,
    inputOrigin_regionName,
    inputOrigin_protocolPolicy,
    inputOrigin_name,

    -- * Instance
    Instance (..),
    newInstance,
    instance_ipAddressType,
    instance_ipv6Addresses,
    instance_bundleId,
    instance_hardware,
    instance_addOns,
    instance_blueprintName,
    instance_sshKeyName,
    instance_createdAt,
    instance_arn,
    instance_blueprintId,
    instance_resourceType,
    instance_supportCode,
    instance_state,
    instance_name,
    instance_tags,
    instance_networking,
    instance_username,
    instance_publicIpAddress,
    instance_isStaticIp,
    instance_location,
    instance_privateIpAddress,

    -- * InstanceAccessDetails
    InstanceAccessDetails (..),
    newInstanceAccessDetails,
    instanceAccessDetails_hostKeys,
    instanceAccessDetails_instanceName,
    instanceAccessDetails_expiresAt,
    instanceAccessDetails_privateKey,
    instanceAccessDetails_certKey,
    instanceAccessDetails_passwordData,
    instanceAccessDetails_ipAddress,
    instanceAccessDetails_password,
    instanceAccessDetails_username,
    instanceAccessDetails_protocol,

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
    instanceHardware_ramSizeInGb,
    instanceHardware_disks,
    instanceHardware_cpuCount,

    -- * InstanceHealthSummary
    InstanceHealthSummary (..),
    newInstanceHealthSummary,
    instanceHealthSummary_instanceName,
    instanceHealthSummary_instanceHealthReason,
    instanceHealthSummary_instanceHealth,

    -- * InstanceNetworking
    InstanceNetworking (..),
    newInstanceNetworking,
    instanceNetworking_monthlyTransfer,
    instanceNetworking_ports,

    -- * InstancePortInfo
    InstancePortInfo (..),
    newInstancePortInfo,
    instancePortInfo_fromPort,
    instancePortInfo_cidrListAliases,
    instancePortInfo_ipv6Cidrs,
    instancePortInfo_commonName,
    instancePortInfo_cidrs,
    instancePortInfo_accessType,
    instancePortInfo_accessDirection,
    instancePortInfo_protocol,
    instancePortInfo_toPort,
    instancePortInfo_accessFrom,

    -- * InstancePortState
    InstancePortState (..),
    newInstancePortState,
    instancePortState_fromPort,
    instancePortState_cidrListAliases,
    instancePortState_ipv6Cidrs,
    instancePortState_state,
    instancePortState_cidrs,
    instancePortState_protocol,
    instancePortState_toPort,

    -- * InstanceSnapshot
    InstanceSnapshot (..),
    newInstanceSnapshot,
    instanceSnapshot_isFromAutoSnapshot,
    instanceSnapshot_createdAt,
    instanceSnapshot_arn,
    instanceSnapshot_fromInstanceArn,
    instanceSnapshot_resourceType,
    instanceSnapshot_supportCode,
    instanceSnapshot_sizeInGb,
    instanceSnapshot_fromBundleId,
    instanceSnapshot_state,
    instanceSnapshot_name,
    instanceSnapshot_fromBlueprintId,
    instanceSnapshot_tags,
    instanceSnapshot_fromInstanceName,
    instanceSnapshot_location,
    instanceSnapshot_progress,
    instanceSnapshot_fromAttachedDisks,

    -- * InstanceSnapshotInfo
    InstanceSnapshotInfo (..),
    newInstanceSnapshotInfo,
    instanceSnapshotInfo_fromDiskInfo,
    instanceSnapshotInfo_fromBundleId,
    instanceSnapshotInfo_fromBlueprintId,

    -- * InstanceState
    InstanceState (..),
    newInstanceState,
    instanceState_code,
    instanceState_name,

    -- * KeyPair
    KeyPair (..),
    newKeyPair,
    keyPair_createdAt,
    keyPair_arn,
    keyPair_resourceType,
    keyPair_supportCode,
    keyPair_name,
    keyPair_tags,
    keyPair_fingerprint,
    keyPair_location,

    -- * LightsailDistribution
    LightsailDistribution (..),
    newLightsailDistribution,
    lightsailDistribution_isEnabled,
    lightsailDistribution_ipAddressType,
    lightsailDistribution_origin,
    lightsailDistribution_status,
    lightsailDistribution_originPublicDNS,
    lightsailDistribution_bundleId,
    lightsailDistribution_alternativeDomainNames,
    lightsailDistribution_createdAt,
    lightsailDistribution_cacheBehaviorSettings,
    lightsailDistribution_arn,
    lightsailDistribution_resourceType,
    lightsailDistribution_supportCode,
    lightsailDistribution_name,
    lightsailDistribution_domainName,
    lightsailDistribution_ableToUpdateBundle,
    lightsailDistribution_tags,
    lightsailDistribution_cacheBehaviors,
    lightsailDistribution_defaultCacheBehavior,
    lightsailDistribution_location,
    lightsailDistribution_certificateName,

    -- * LoadBalancer
    LoadBalancer (..),
    newLoadBalancer,
    loadBalancer_ipAddressType,
    loadBalancer_tlsCertificateSummaries,
    loadBalancer_instanceHealthSummary,
    loadBalancer_publicPorts,
    loadBalancer_configurationOptions,
    loadBalancer_instancePort,
    loadBalancer_createdAt,
    loadBalancer_arn,
    loadBalancer_resourceType,
    loadBalancer_supportCode,
    loadBalancer_state,
    loadBalancer_name,
    loadBalancer_healthCheckPath,
    loadBalancer_tags,
    loadBalancer_dnsName,
    loadBalancer_protocol,
    loadBalancer_location,

    -- * LoadBalancerTlsCertificate
    LoadBalancerTlsCertificate (..),
    newLoadBalancerTlsCertificate,
    loadBalancerTlsCertificate_status,
    loadBalancerTlsCertificate_notBefore,
    loadBalancerTlsCertificate_serial,
    loadBalancerTlsCertificate_isAttached,
    loadBalancerTlsCertificate_createdAt,
    loadBalancerTlsCertificate_arn,
    loadBalancerTlsCertificate_resourceType,
    loadBalancerTlsCertificate_supportCode,
    loadBalancerTlsCertificate_subjectAlternativeNames,
    loadBalancerTlsCertificate_name,
    loadBalancerTlsCertificate_domainName,
    loadBalancerTlsCertificate_subject,
    loadBalancerTlsCertificate_failureReason,
    loadBalancerTlsCertificate_revocationReason,
    loadBalancerTlsCertificate_revokedAt,
    loadBalancerTlsCertificate_notAfter,
    loadBalancerTlsCertificate_tags,
    loadBalancerTlsCertificate_signatureAlgorithm,
    loadBalancerTlsCertificate_issuer,
    loadBalancerTlsCertificate_issuedAt,
    loadBalancerTlsCertificate_keyAlgorithm,
    loadBalancerTlsCertificate_domainValidationRecords,
    loadBalancerTlsCertificate_location,
    loadBalancerTlsCertificate_loadBalancerName,
    loadBalancerTlsCertificate_renewalSummary,

    -- * LoadBalancerTlsCertificateDomainValidationOption
    LoadBalancerTlsCertificateDomainValidationOption (..),
    newLoadBalancerTlsCertificateDomainValidationOption,
    loadBalancerTlsCertificateDomainValidationOption_domainName,
    loadBalancerTlsCertificateDomainValidationOption_validationStatus,

    -- * LoadBalancerTlsCertificateDomainValidationRecord
    LoadBalancerTlsCertificateDomainValidationRecord (..),
    newLoadBalancerTlsCertificateDomainValidationRecord,
    loadBalancerTlsCertificateDomainValidationRecord_name,
    loadBalancerTlsCertificateDomainValidationRecord_domainName,
    loadBalancerTlsCertificateDomainValidationRecord_validationStatus,
    loadBalancerTlsCertificateDomainValidationRecord_value,
    loadBalancerTlsCertificateDomainValidationRecord_type,

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

    -- * LogEvent
    LogEvent (..),
    newLogEvent,
    logEvent_message,
    logEvent_createdAt,

    -- * MetricDatapoint
    MetricDatapoint (..),
    newMetricDatapoint,
    metricDatapoint_minimum,
    metricDatapoint_unit,
    metricDatapoint_sum,
    metricDatapoint_sampleCount,
    metricDatapoint_timestamp,
    metricDatapoint_average,
    metricDatapoint_maximum,

    -- * MonitoredResourceInfo
    MonitoredResourceInfo (..),
    newMonitoredResourceInfo,
    monitoredResourceInfo_arn,
    monitoredResourceInfo_resourceType,
    monitoredResourceInfo_name,

    -- * MonthlyTransfer
    MonthlyTransfer (..),
    newMonthlyTransfer,
    monthlyTransfer_gbPerMonthAllocated,

    -- * Operation
    Operation (..),
    newOperation,
    operation_operationDetails,
    operation_status,
    operation_isTerminal,
    operation_createdAt,
    operation_id,
    operation_resourceType,
    operation_statusChangedAt,
    operation_location,
    operation_resourceName,
    operation_operationType,
    operation_errorCode,
    operation_errorDetails,

    -- * Origin
    Origin (..),
    newOrigin,
    origin_regionName,
    origin_protocolPolicy,
    origin_resourceType,
    origin_name,

    -- * PasswordData
    PasswordData (..),
    newPasswordData,
    passwordData_keyPairName,
    passwordData_ciphertext,

    -- * PendingMaintenanceAction
    PendingMaintenanceAction (..),
    newPendingMaintenanceAction,
    pendingMaintenanceAction_currentApplyDate,
    pendingMaintenanceAction_action,
    pendingMaintenanceAction_description,

    -- * PendingModifiedRelationalDatabaseValues
    PendingModifiedRelationalDatabaseValues (..),
    newPendingModifiedRelationalDatabaseValues,
    pendingModifiedRelationalDatabaseValues_masterUserPassword,
    pendingModifiedRelationalDatabaseValues_backupRetentionEnabled,
    pendingModifiedRelationalDatabaseValues_engineVersion,

    -- * PortInfo
    PortInfo (..),
    newPortInfo,
    portInfo_fromPort,
    portInfo_cidrListAliases,
    portInfo_ipv6Cidrs,
    portInfo_cidrs,
    portInfo_protocol,
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
    regionInfo_continentCode,
    regionInfo_relationalDatabaseAvailabilityZones,
    regionInfo_name,
    regionInfo_description,
    regionInfo_displayName,

    -- * RelationalDatabase
    RelationalDatabase (..),
    newRelationalDatabase,
    relationalDatabase_relationalDatabaseBundleId,
    relationalDatabase_masterEndpoint,
    relationalDatabase_pendingMaintenanceActions,
    relationalDatabase_preferredBackupWindow,
    relationalDatabase_caCertificateIdentifier,
    relationalDatabase_relationalDatabaseBlueprintId,
    relationalDatabase_latestRestorableTime,
    relationalDatabase_masterDatabaseName,
    relationalDatabase_hardware,
    relationalDatabase_createdAt,
    relationalDatabase_arn,
    relationalDatabase_masterUsername,
    relationalDatabase_resourceType,
    relationalDatabase_publiclyAccessible,
    relationalDatabase_supportCode,
    relationalDatabase_state,
    relationalDatabase_name,
    relationalDatabase_backupRetentionEnabled,
    relationalDatabase_engineVersion,
    relationalDatabase_preferredMaintenanceWindow,
    relationalDatabase_tags,
    relationalDatabase_parameterApplyStatus,
    relationalDatabase_pendingModifiedValues,
    relationalDatabase_engine,
    relationalDatabase_secondaryAvailabilityZone,
    relationalDatabase_location,

    -- * RelationalDatabaseBlueprint
    RelationalDatabaseBlueprint (..),
    newRelationalDatabaseBlueprint,
    relationalDatabaseBlueprint_engineDescription,
    relationalDatabaseBlueprint_blueprintId,
    relationalDatabaseBlueprint_engineVersionDescription,
    relationalDatabaseBlueprint_isEngineDefault,
    relationalDatabaseBlueprint_engineVersion,
    relationalDatabaseBlueprint_engine,

    -- * RelationalDatabaseBundle
    RelationalDatabaseBundle (..),
    newRelationalDatabaseBundle,
    relationalDatabaseBundle_ramSizeInGb,
    relationalDatabaseBundle_bundleId,
    relationalDatabaseBundle_isActive,
    relationalDatabaseBundle_name,
    relationalDatabaseBundle_isEncrypted,
    relationalDatabaseBundle_transferPerMonthInGb,
    relationalDatabaseBundle_cpuCount,
    relationalDatabaseBundle_price,
    relationalDatabaseBundle_diskSizeInGb,

    -- * RelationalDatabaseEndpoint
    RelationalDatabaseEndpoint (..),
    newRelationalDatabaseEndpoint,
    relationalDatabaseEndpoint_address,
    relationalDatabaseEndpoint_port,

    -- * RelationalDatabaseEvent
    RelationalDatabaseEvent (..),
    newRelationalDatabaseEvent,
    relationalDatabaseEvent_message,
    relationalDatabaseEvent_createdAt,
    relationalDatabaseEvent_eventCategories,
    relationalDatabaseEvent_resource,

    -- * RelationalDatabaseHardware
    RelationalDatabaseHardware (..),
    newRelationalDatabaseHardware,
    relationalDatabaseHardware_ramSizeInGb,
    relationalDatabaseHardware_cpuCount,
    relationalDatabaseHardware_diskSizeInGb,

    -- * RelationalDatabaseParameter
    RelationalDatabaseParameter (..),
    newRelationalDatabaseParameter,
    relationalDatabaseParameter_allowedValues,
    relationalDatabaseParameter_parameterValue,
    relationalDatabaseParameter_applyType,
    relationalDatabaseParameter_parameterName,
    relationalDatabaseParameter_description,
    relationalDatabaseParameter_applyMethod,
    relationalDatabaseParameter_isModifiable,
    relationalDatabaseParameter_dataType,

    -- * RelationalDatabaseSnapshot
    RelationalDatabaseSnapshot (..),
    newRelationalDatabaseSnapshot,
    relationalDatabaseSnapshot_fromRelationalDatabaseName,
    relationalDatabaseSnapshot_createdAt,
    relationalDatabaseSnapshot_arn,
    relationalDatabaseSnapshot_resourceType,
    relationalDatabaseSnapshot_supportCode,
    relationalDatabaseSnapshot_sizeInGb,
    relationalDatabaseSnapshot_state,
    relationalDatabaseSnapshot_name,
    relationalDatabaseSnapshot_engineVersion,
    relationalDatabaseSnapshot_fromRelationalDatabaseBundleId,
    relationalDatabaseSnapshot_tags,
    relationalDatabaseSnapshot_engine,
    relationalDatabaseSnapshot_fromRelationalDatabaseArn,
    relationalDatabaseSnapshot_location,
    relationalDatabaseSnapshot_fromRelationalDatabaseBlueprintId,

    -- * RenewalSummary
    RenewalSummary (..),
    newRenewalSummary,
    renewalSummary_updatedAt,
    renewalSummary_renewalStatus,
    renewalSummary_renewalStatusReason,
    renewalSummary_domainValidationRecords,

    -- * ResourceLocation
    ResourceLocation (..),
    newResourceLocation,
    resourceLocation_regionName,
    resourceLocation_availabilityZone,

    -- * ResourceRecord
    ResourceRecord (..),
    newResourceRecord,
    resourceRecord_name,
    resourceRecord_value,
    resourceRecord_type,

    -- * StaticIp
    StaticIp (..),
    newStaticIp,
    staticIp_isAttached,
    staticIp_createdAt,
    staticIp_arn,
    staticIp_resourceType,
    staticIp_supportCode,
    staticIp_name,
    staticIp_ipAddress,
    staticIp_attachedTo,
    staticIp_location,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AccessDirection
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
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceRecord
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.StaticIp
import Network.AWS.Lightsail.Types.Tag
import Network.AWS.Lightsail.Types.TreatMissingData
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2016-11-28@ of the Amazon Lightsail SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Lightsail",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "lightsail",
      Prelude._svcVersion = "2016-11-28",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "Lightsail",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Lightsail throws this exception when it cannot find a resource.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"

-- | Lightsail throws this exception when user input does not conform to the
-- validation rules of an input field.
--
-- Domain and distribution APIs are only available in the N. Virginia
-- (@us-east-1@) AWS Region. Please set your AWS Region configuration to
-- @us-east-1@ to create, view, or edit these resources.
_InvalidInputException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidInputException =
  Prelude._MatchServiceError
    defaultService
    "InvalidInputException"

-- | Lightsail throws this exception when the user has not been
-- authenticated.
_UnauthenticatedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthenticatedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthenticatedException"

-- | Lightsail throws this exception when an operation fails to execute.
_OperationFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OperationFailureException =
  Prelude._MatchServiceError
    defaultService
    "OperationFailureException"

-- | Lightsail throws this exception when the user cannot be authenticated or
-- uses invalid credentials to access a resource.
_AccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | Lightsail throws this exception when an account is still in the setup in
-- progress state.
_AccountSetupInProgressException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccountSetupInProgressException =
  Prelude._MatchServiceError
    defaultService
    "AccountSetupInProgressException"

-- | A general service exception.
_ServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceException =
  Prelude._MatchServiceError
    defaultService
    "ServiceException"
