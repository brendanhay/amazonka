{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _BadRequestException,
    _InternalServerErrorException,

    -- * AdminStatus
    AdminStatus (..),

    -- * CriterionKey
    CriterionKey (..),

    -- * DataSource
    DataSource (..),

    -- * DataSourceStatus
    DataSourceStatus (..),

    -- * DestinationType
    DestinationType (..),

    -- * DetectorStatus
    DetectorStatus (..),

    -- * EbsSnapshotPreservation
    EbsSnapshotPreservation (..),

    -- * Feedback
    Feedback (..),

    -- * FilterAction
    FilterAction (..),

    -- * FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- * FindingStatisticType
    FindingStatisticType (..),

    -- * IpSetFormat
    IpSetFormat (..),

    -- * IpSetStatus
    IpSetStatus (..),

    -- * OrderBy
    OrderBy (..),

    -- * PublishingStatus
    PublishingStatus (..),

    -- * ScanCriterionKey
    ScanCriterionKey (..),

    -- * ScanResult
    ScanResult (..),

    -- * ScanStatus
    ScanStatus (..),

    -- * ThreatIntelSetFormat
    ThreatIntelSetFormat (..),

    -- * ThreatIntelSetStatus
    ThreatIntelSetStatus (..),

    -- * UsageStatisticType
    UsageStatisticType (..),

    -- * AccessControlList
    AccessControlList (..),
    newAccessControlList,
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

    -- * AccessKeyDetails
    AccessKeyDetails (..),
    newAccessKeyDetails,
    accessKeyDetails_accessKeyId,
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_userType,

    -- * AccountDetail
    AccountDetail (..),
    newAccountDetail,
    accountDetail_accountId,
    accountDetail_email,

    -- * AccountFreeTrialInfo
    AccountFreeTrialInfo (..),
    newAccountFreeTrialInfo,
    accountFreeTrialInfo_accountId,
    accountFreeTrialInfo_dataSources,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    newAccountLevelPermissions,
    accountLevelPermissions_blockPublicAccess,

    -- * Action
    Action (..),
    newAction,
    action_actionType,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_kubernetesApiCallAction,
    action_networkConnectionAction,
    action_portProbeAction,

    -- * AdminAccount
    AdminAccount (..),
    newAdminAccount,
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- * Administrator
    Administrator (..),
    newAdministrator,
    administrator_accountId,
    administrator_invitationId,
    administrator_invitedAt,
    administrator_relationshipStatus,

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    newAwsApiCallAction,
    awsApiCallAction_affectedResources,
    awsApiCallAction_api,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_errorCode,
    awsApiCallAction_remoteAccountDetails,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_serviceName,
    awsApiCallAction_userAgent,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_restrictPublicBuckets,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    newBucketLevelPermissions,
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- * BucketPolicy
    BucketPolicy (..),
    newBucketPolicy,
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- * City
    City (..),
    newCity,
    city_cityName,

    -- * CloudTrailConfigurationResult
    CloudTrailConfigurationResult (..),
    newCloudTrailConfigurationResult,
    cloudTrailConfigurationResult_status,

    -- * Condition
    Condition (..),
    newCondition,
    condition_eq,
    condition_equals,
    condition_greaterThan,
    condition_greaterThanOrEqual,
    condition_gt,
    condition_gte,
    condition_lessThan,
    condition_lessThanOrEqual,
    condition_lt,
    condition_lte,
    condition_neq,
    condition_notEquals,

    -- * Container
    Container (..),
    newContainer,
    container_containerRuntime,
    container_id,
    container_image,
    container_imagePrefix,
    container_name,
    container_securityContext,
    container_volumeMounts,

    -- * Country
    Country (..),
    newCountry,
    country_countryCode,
    country_countryName,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    newDNSLogsConfigurationResult,
    dNSLogsConfigurationResult_status,

    -- * DataSourceConfigurations
    DataSourceConfigurations (..),
    newDataSourceConfigurations,
    dataSourceConfigurations_kubernetes,
    dataSourceConfigurations_malwareProtection,
    dataSourceConfigurations_s3Logs,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    newDataSourceConfigurationsResult,
    dataSourceConfigurationsResult_kubernetes,
    dataSourceConfigurationsResult_malwareProtection,
    dataSourceConfigurationsResult_cloudTrail,
    dataSourceConfigurationsResult_dNSLogs,
    dataSourceConfigurationsResult_flowLogs,
    dataSourceConfigurationsResult_s3Logs,

    -- * DataSourceFreeTrial
    DataSourceFreeTrial (..),
    newDataSourceFreeTrial,
    dataSourceFreeTrial_freeTrialDaysRemaining,

    -- * DataSourcesFreeTrial
    DataSourcesFreeTrial (..),
    newDataSourcesFreeTrial,
    dataSourcesFreeTrial_cloudTrail,
    dataSourcesFreeTrial_dnsLogs,
    dataSourcesFreeTrial_flowLogs,
    dataSourcesFreeTrial_kubernetes,
    dataSourcesFreeTrial_malwareProtection,
    dataSourcesFreeTrial_s3Logs,

    -- * DefaultServerSideEncryption
    DefaultServerSideEncryption (..),
    newDefaultServerSideEncryption,
    defaultServerSideEncryption_encryptionType,
    defaultServerSideEncryption_kmsMasterKeyArn,

    -- * Destination
    Destination (..),
    newDestination,
    destination_destinationId,
    destination_destinationType,
    destination_status,

    -- * DestinationProperties
    DestinationProperties (..),
    newDestinationProperties,
    destinationProperties_destinationArn,
    destinationProperties_kmsKeyArn,

    -- * DnsRequestAction
    DnsRequestAction (..),
    newDnsRequestAction,
    dnsRequestAction_blocked,
    dnsRequestAction_domain,
    dnsRequestAction_protocol,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domain,

    -- * EbsVolumeDetails
    EbsVolumeDetails (..),
    newEbsVolumeDetails,
    ebsVolumeDetails_scannedVolumeDetails,
    ebsVolumeDetails_skippedVolumeDetails,

    -- * EbsVolumeScanDetails
    EbsVolumeScanDetails (..),
    newEbsVolumeScanDetails,
    ebsVolumeScanDetails_scanCompletedAt,
    ebsVolumeScanDetails_scanDetections,
    ebsVolumeScanDetails_scanId,
    ebsVolumeScanDetails_scanStartedAt,
    ebsVolumeScanDetails_sources,
    ebsVolumeScanDetails_triggerFindingId,

    -- * EbsVolumesResult
    EbsVolumesResult (..),
    newEbsVolumesResult,
    ebsVolumesResult_reason,
    ebsVolumesResult_status,

    -- * EcsClusterDetails
    EcsClusterDetails (..),
    newEcsClusterDetails,
    ecsClusterDetails_activeServicesCount,
    ecsClusterDetails_arn,
    ecsClusterDetails_name,
    ecsClusterDetails_registeredContainerInstancesCount,
    ecsClusterDetails_runningTasksCount,
    ecsClusterDetails_status,
    ecsClusterDetails_tags,
    ecsClusterDetails_taskDetails,

    -- * EcsTaskDetails
    EcsTaskDetails (..),
    newEcsTaskDetails,
    ecsTaskDetails_arn,
    ecsTaskDetails_containers,
    ecsTaskDetails_definitionArn,
    ecsTaskDetails_group,
    ecsTaskDetails_startedAt,
    ecsTaskDetails_startedBy,
    ecsTaskDetails_tags,
    ecsTaskDetails_taskCreatedAt,
    ecsTaskDetails_version,
    ecsTaskDetails_volumes,

    -- * EksClusterDetails
    EksClusterDetails (..),
    newEksClusterDetails,
    eksClusterDetails_arn,
    eksClusterDetails_createdAt,
    eksClusterDetails_name,
    eksClusterDetails_status,
    eksClusterDetails_tags,
    eksClusterDetails_vpcId,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_threatIntelligenceDetails,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_equalsValue,
    filterCondition_greaterThan,
    filterCondition_lessThan,

    -- * FilterCriteria
    FilterCriteria (..),
    newFilterCriteria,
    filterCriteria_filterCriterion,

    -- * FilterCriterion
    FilterCriterion (..),
    newFilterCriterion,
    filterCriterion_criterionKey,
    filterCriterion_filterCondition,

    -- * Finding
    Finding (..),
    newFinding,
    finding_confidence,
    finding_description,
    finding_partition,
    finding_service,
    finding_title,
    finding_accountId,
    finding_arn,
    finding_createdAt,
    finding_id,
    finding_region,
    finding_resource,
    finding_schemaVersion,
    finding_severity,
    finding_type,
    finding_updatedAt,

    -- * FindingCriteria
    FindingCriteria (..),
    newFindingCriteria,
    findingCriteria_criterion,

    -- * FindingStatistics
    FindingStatistics (..),
    newFindingStatistics,
    findingStatistics_countBySeverity,

    -- * FlowLogsConfigurationResult
    FlowLogsConfigurationResult (..),
    newFlowLogsConfigurationResult,
    flowLogsConfigurationResult_status,

    -- * GeoLocation
    GeoLocation (..),
    newGeoLocation,
    geoLocation_lat,
    geoLocation_lon,

    -- * HighestSeverityThreatDetails
    HighestSeverityThreatDetails (..),
    newHighestSeverityThreatDetails,
    highestSeverityThreatDetails_count,
    highestSeverityThreatDetails_severity,
    highestSeverityThreatDetails_threatName,

    -- * HostPath
    HostPath (..),
    newHostPath,
    hostPath_path,

    -- * IamInstanceProfile
    IamInstanceProfile (..),
    newIamInstanceProfile,
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- * InstanceDetails
    InstanceDetails (..),
    newInstanceDetails,
    instanceDetails_availabilityZone,
    instanceDetails_iamInstanceProfile,
    instanceDetails_imageDescription,
    instanceDetails_imageId,
    instanceDetails_instanceId,
    instanceDetails_instanceState,
    instanceDetails_instanceType,
    instanceDetails_launchTime,
    instanceDetails_networkInterfaces,
    instanceDetails_outpostArn,
    instanceDetails_platform,
    instanceDetails_productCodes,
    instanceDetails_tags,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_relationshipStatus,

    -- * KubernetesApiCallAction
    KubernetesApiCallAction (..),
    newKubernetesApiCallAction,
    kubernetesApiCallAction_parameters,
    kubernetesApiCallAction_remoteIpDetails,
    kubernetesApiCallAction_requestUri,
    kubernetesApiCallAction_sourceIps,
    kubernetesApiCallAction_statusCode,
    kubernetesApiCallAction_userAgent,
    kubernetesApiCallAction_verb,

    -- * KubernetesAuditLogsConfiguration
    KubernetesAuditLogsConfiguration (..),
    newKubernetesAuditLogsConfiguration,
    kubernetesAuditLogsConfiguration_enable,

    -- * KubernetesAuditLogsConfigurationResult
    KubernetesAuditLogsConfigurationResult (..),
    newKubernetesAuditLogsConfigurationResult,
    kubernetesAuditLogsConfigurationResult_status,

    -- * KubernetesConfiguration
    KubernetesConfiguration (..),
    newKubernetesConfiguration,
    kubernetesConfiguration_auditLogs,

    -- * KubernetesConfigurationResult
    KubernetesConfigurationResult (..),
    newKubernetesConfigurationResult,
    kubernetesConfigurationResult_auditLogs,

    -- * KubernetesDataSourceFreeTrial
    KubernetesDataSourceFreeTrial (..),
    newKubernetesDataSourceFreeTrial,
    kubernetesDataSourceFreeTrial_auditLogs,

    -- * KubernetesDetails
    KubernetesDetails (..),
    newKubernetesDetails,
    kubernetesDetails_kubernetesUserDetails,
    kubernetesDetails_kubernetesWorkloadDetails,

    -- * KubernetesUserDetails
    KubernetesUserDetails (..),
    newKubernetesUserDetails,
    kubernetesUserDetails_groups,
    kubernetesUserDetails_uid,
    kubernetesUserDetails_username,

    -- * KubernetesWorkloadDetails
    KubernetesWorkloadDetails (..),
    newKubernetesWorkloadDetails,
    kubernetesWorkloadDetails_containers,
    kubernetesWorkloadDetails_hostNetwork,
    kubernetesWorkloadDetails_name,
    kubernetesWorkloadDetails_namespace,
    kubernetesWorkloadDetails_type,
    kubernetesWorkloadDetails_uid,
    kubernetesWorkloadDetails_volumes,

    -- * LocalIpDetails
    LocalIpDetails (..),
    newLocalIpDetails,
    localIpDetails_ipAddressV4,

    -- * LocalPortDetails
    LocalPortDetails (..),
    newLocalPortDetails,
    localPortDetails_port,
    localPortDetails_portName,

    -- * MalwareProtectionConfiguration
    MalwareProtectionConfiguration (..),
    newMalwareProtectionConfiguration,
    malwareProtectionConfiguration_scanEc2InstanceWithFindings,

    -- * MalwareProtectionConfigurationResult
    MalwareProtectionConfigurationResult (..),
    newMalwareProtectionConfigurationResult,
    malwareProtectionConfigurationResult_scanEc2InstanceWithFindings,
    malwareProtectionConfigurationResult_serviceRole,

    -- * MalwareProtectionDataSourceFreeTrial
    MalwareProtectionDataSourceFreeTrial (..),
    newMalwareProtectionDataSourceFreeTrial,
    malwareProtectionDataSourceFreeTrial_scanEc2InstanceWithFindings,

    -- * Member
    Member (..),
    newMember,
    member_administratorId,
    member_detectorId,
    member_invitedAt,
    member_accountId,
    member_masterId,
    member_email,
    member_relationshipStatus,
    member_updatedAt,

    -- * MemberDataSourceConfiguration
    MemberDataSourceConfiguration (..),
    newMemberDataSourceConfiguration,
    memberDataSourceConfiguration_accountId,
    memberDataSourceConfiguration_dataSources,

    -- * NetworkConnectionAction
    NetworkConnectionAction (..),
    newNetworkConnectionAction,
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_remotePortDetails,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_ipv6Addresses,
    networkInterface_networkInterfaceId,
    networkInterface_privateDnsName,
    networkInterface_privateIpAddress,
    networkInterface_privateIpAddresses,
    networkInterface_publicDnsName,
    networkInterface_publicIp,
    networkInterface_securityGroups,
    networkInterface_subnetId,
    networkInterface_vpcId,

    -- * Organization
    Organization (..),
    newOrganization,
    organization_asn,
    organization_asnOrg,
    organization_isp,
    organization_org,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    newOrganizationDataSourceConfigurations,
    organizationDataSourceConfigurations_kubernetes,
    organizationDataSourceConfigurations_malwareProtection,
    organizationDataSourceConfigurations_s3Logs,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    newOrganizationDataSourceConfigurationsResult,
    organizationDataSourceConfigurationsResult_kubernetes,
    organizationDataSourceConfigurationsResult_malwareProtection,
    organizationDataSourceConfigurationsResult_s3Logs,

    -- * OrganizationEbsVolumes
    OrganizationEbsVolumes (..),
    newOrganizationEbsVolumes,
    organizationEbsVolumes_autoEnable,

    -- * OrganizationEbsVolumesResult
    OrganizationEbsVolumesResult (..),
    newOrganizationEbsVolumesResult,
    organizationEbsVolumesResult_autoEnable,

    -- * OrganizationKubernetesAuditLogsConfiguration
    OrganizationKubernetesAuditLogsConfiguration (..),
    newOrganizationKubernetesAuditLogsConfiguration,
    organizationKubernetesAuditLogsConfiguration_autoEnable,

    -- * OrganizationKubernetesAuditLogsConfigurationResult
    OrganizationKubernetesAuditLogsConfigurationResult (..),
    newOrganizationKubernetesAuditLogsConfigurationResult,
    organizationKubernetesAuditLogsConfigurationResult_autoEnable,

    -- * OrganizationKubernetesConfiguration
    OrganizationKubernetesConfiguration (..),
    newOrganizationKubernetesConfiguration,
    organizationKubernetesConfiguration_auditLogs,

    -- * OrganizationKubernetesConfigurationResult
    OrganizationKubernetesConfigurationResult (..),
    newOrganizationKubernetesConfigurationResult,
    organizationKubernetesConfigurationResult_auditLogs,

    -- * OrganizationMalwareProtectionConfiguration
    OrganizationMalwareProtectionConfiguration (..),
    newOrganizationMalwareProtectionConfiguration,
    organizationMalwareProtectionConfiguration_scanEc2InstanceWithFindings,

    -- * OrganizationMalwareProtectionConfigurationResult
    OrganizationMalwareProtectionConfigurationResult (..),
    newOrganizationMalwareProtectionConfigurationResult,
    organizationMalwareProtectionConfigurationResult_scanEc2InstanceWithFindings,

    -- * OrganizationS3LogsConfiguration
    OrganizationS3LogsConfiguration (..),
    newOrganizationS3LogsConfiguration,
    organizationS3LogsConfiguration_autoEnable,

    -- * OrganizationS3LogsConfigurationResult
    OrganizationS3LogsConfigurationResult (..),
    newOrganizationS3LogsConfigurationResult,
    organizationS3LogsConfigurationResult_autoEnable,

    -- * OrganizationScanEc2InstanceWithFindings
    OrganizationScanEc2InstanceWithFindings (..),
    newOrganizationScanEc2InstanceWithFindings,
    organizationScanEc2InstanceWithFindings_ebsVolumes,

    -- * OrganizationScanEc2InstanceWithFindingsResult
    OrganizationScanEc2InstanceWithFindingsResult (..),
    newOrganizationScanEc2InstanceWithFindingsResult,
    organizationScanEc2InstanceWithFindingsResult_ebsVolumes,

    -- * Owner
    Owner (..),
    newOwner,
    owner_id,

    -- * PermissionConfiguration
    PermissionConfiguration (..),
    newPermissionConfiguration,
    permissionConfiguration_accountLevelPermissions,
    permissionConfiguration_bucketLevelPermissions,

    -- * PortProbeAction
    PortProbeAction (..),
    newPortProbeAction,
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- * PortProbeDetail
    PortProbeDetail (..),
    newPortProbeDetail,
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_remoteIpDetails,

    -- * PrivateIpAddressDetails
    PrivateIpAddressDetails (..),
    newPrivateIpAddressDetails,
    privateIpAddressDetails_privateDnsName,
    privateIpAddressDetails_privateIpAddress,

    -- * ProductCode
    ProductCode (..),
    newProductCode,
    productCode_code,
    productCode_productType,

    -- * PublicAccess
    PublicAccess (..),
    newPublicAccess,
    publicAccess_effectivePermission,
    publicAccess_permissionConfiguration,

    -- * RemoteAccountDetails
    RemoteAccountDetails (..),
    newRemoteAccountDetails,
    remoteAccountDetails_accountId,
    remoteAccountDetails_affiliated,

    -- * RemoteIpDetails
    RemoteIpDetails (..),
    newRemoteIpDetails,
    remoteIpDetails_city,
    remoteIpDetails_country,
    remoteIpDetails_geoLocation,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_organization,

    -- * RemotePortDetails
    RemotePortDetails (..),
    newRemotePortDetails,
    remotePortDetails_port,
    remotePortDetails_portName,

    -- * Resource
    Resource (..),
    newResource,
    resource_accessKeyDetails,
    resource_containerDetails,
    resource_ebsVolumeDetails,
    resource_ecsClusterDetails,
    resource_eksClusterDetails,
    resource_instanceDetails,
    resource_kubernetesDetails,
    resource_resourceType,
    resource_s3BucketDetails,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_instanceArn,

    -- * S3BucketDetail
    S3BucketDetail (..),
    newS3BucketDetail,
    s3BucketDetail_arn,
    s3BucketDetail_createdAt,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_name,
    s3BucketDetail_owner,
    s3BucketDetail_publicAccess,
    s3BucketDetail_tags,
    s3BucketDetail_type,

    -- * S3LogsConfiguration
    S3LogsConfiguration (..),
    newS3LogsConfiguration,
    s3LogsConfiguration_enable,

    -- * S3LogsConfigurationResult
    S3LogsConfigurationResult (..),
    newS3LogsConfigurationResult,
    s3LogsConfigurationResult_status,

    -- * Scan
    Scan (..),
    newScan,
    scan_accountId,
    scan_adminDetectorId,
    scan_attachedVolumes,
    scan_detectorId,
    scan_failureReason,
    scan_fileCount,
    scan_resourceDetails,
    scan_scanEndTime,
    scan_scanId,
    scan_scanResultDetails,
    scan_scanStartTime,
    scan_scanStatus,
    scan_totalBytes,
    scan_triggerDetails,

    -- * ScanCondition
    ScanCondition (..),
    newScanCondition,
    scanCondition_mapEquals,

    -- * ScanConditionPair
    ScanConditionPair (..),
    newScanConditionPair,
    scanConditionPair_value,
    scanConditionPair_key,

    -- * ScanDetections
    ScanDetections (..),
    newScanDetections,
    scanDetections_highestSeverityThreatDetails,
    scanDetections_scannedItemCount,
    scanDetections_threatDetectedByName,
    scanDetections_threatsDetectedItemCount,

    -- * ScanEc2InstanceWithFindings
    ScanEc2InstanceWithFindings (..),
    newScanEc2InstanceWithFindings,
    scanEc2InstanceWithFindings_ebsVolumes,

    -- * ScanEc2InstanceWithFindingsResult
    ScanEc2InstanceWithFindingsResult (..),
    newScanEc2InstanceWithFindingsResult,
    scanEc2InstanceWithFindingsResult_ebsVolumes,

    -- * ScanFilePath
    ScanFilePath (..),
    newScanFilePath,
    scanFilePath_fileName,
    scanFilePath_filePath,
    scanFilePath_hash,
    scanFilePath_volumeArn,

    -- * ScanResourceCriteria
    ScanResourceCriteria (..),
    newScanResourceCriteria,
    scanResourceCriteria_exclude,
    scanResourceCriteria_include,

    -- * ScanResultDetails
    ScanResultDetails (..),
    newScanResultDetails,
    scanResultDetails_scanResult,

    -- * ScanThreatName
    ScanThreatName (..),
    newScanThreatName,
    scanThreatName_filePaths,
    scanThreatName_itemCount,
    scanThreatName_name,
    scanThreatName_severity,

    -- * ScannedItemCount
    ScannedItemCount (..),
    newScannedItemCount,
    scannedItemCount_files,
    scannedItemCount_totalGb,
    scannedItemCount_volumes,

    -- * SecurityContext
    SecurityContext (..),
    newSecurityContext,
    securityContext_privileged,

    -- * SecurityGroup
    SecurityGroup (..),
    newSecurityGroup,
    securityGroup_groupId,
    securityGroup_groupName,

    -- * ServiceAdditionalInfo
    ServiceAdditionalInfo (..),
    newServiceAdditionalInfo,
    serviceAdditionalInfo_type,
    serviceAdditionalInfo_value,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
    serviceInfo_action,
    serviceInfo_additionalInfo,
    serviceInfo_archived,
    serviceInfo_count,
    serviceInfo_detectorId,
    serviceInfo_ebsVolumeScanDetails,
    serviceInfo_eventFirstSeen,
    serviceInfo_eventLastSeen,
    serviceInfo_evidence,
    serviceInfo_featureName,
    serviceInfo_resourceRole,
    serviceInfo_serviceName,
    serviceInfo_userFeedback,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * ThreatDetectedByName
    ThreatDetectedByName (..),
    newThreatDetectedByName,
    threatDetectedByName_itemCount,
    threatDetectedByName_shortened,
    threatDetectedByName_threatNames,
    threatDetectedByName_uniqueThreatNameCount,

    -- * ThreatIntelligenceDetail
    ThreatIntelligenceDetail (..),
    newThreatIntelligenceDetail,
    threatIntelligenceDetail_threatListName,
    threatIntelligenceDetail_threatNames,

    -- * ThreatsDetectedItemCount
    ThreatsDetectedItemCount (..),
    newThreatsDetectedItemCount,
    threatsDetectedItemCount_files,

    -- * Total
    Total (..),
    newTotal,
    total_amount,
    total_unit,

    -- * TriggerDetails
    TriggerDetails (..),
    newTriggerDetails,
    triggerDetails_description,
    triggerDetails_guardDutyFindingId,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_result,

    -- * UnprocessedDataSourcesResult
    UnprocessedDataSourcesResult (..),
    newUnprocessedDataSourcesResult,
    unprocessedDataSourcesResult_malwareProtection,

    -- * UsageAccountResult
    UsageAccountResult (..),
    newUsageAccountResult,
    usageAccountResult_accountId,
    usageAccountResult_total,

    -- * UsageCriteria
    UsageCriteria (..),
    newUsageCriteria,
    usageCriteria_accountIds,
    usageCriteria_resources,
    usageCriteria_dataSources,

    -- * UsageDataSourceResult
    UsageDataSourceResult (..),
    newUsageDataSourceResult,
    usageDataSourceResult_dataSource,
    usageDataSourceResult_total,

    -- * UsageResourceResult
    UsageResourceResult (..),
    newUsageResourceResult,
    usageResourceResult_resource,
    usageResourceResult_total,

    -- * UsageStatistics
    UsageStatistics (..),
    newUsageStatistics,
    usageStatistics_sumByAccount,
    usageStatistics_sumByDataSource,
    usageStatistics_sumByResource,
    usageStatistics_topResources,

    -- * Volume
    Volume (..),
    newVolume,
    volume_hostPath,
    volume_name,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_deviceName,
    volumeDetail_encryptionType,
    volumeDetail_kmsKeyArn,
    volumeDetail_snapshotArn,
    volumeDetail_volumeArn,
    volumeDetail_volumeSizeInGB,
    volumeDetail_volumeType,

    -- * VolumeMount
    VolumeMount (..),
    newVolumeMount,
    volumeMount_mountPath,
    volumeMount_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.AccessControlList
import Amazonka.GuardDuty.Types.AccessKeyDetails
import Amazonka.GuardDuty.Types.AccountDetail
import Amazonka.GuardDuty.Types.AccountFreeTrialInfo
import Amazonka.GuardDuty.Types.AccountLevelPermissions
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.AdminAccount
import Amazonka.GuardDuty.Types.AdminStatus
import Amazonka.GuardDuty.Types.Administrator
import Amazonka.GuardDuty.Types.AwsApiCallAction
import Amazonka.GuardDuty.Types.BlockPublicAccess
import Amazonka.GuardDuty.Types.BucketLevelPermissions
import Amazonka.GuardDuty.Types.BucketPolicy
import Amazonka.GuardDuty.Types.City
import Amazonka.GuardDuty.Types.CloudTrailConfigurationResult
import Amazonka.GuardDuty.Types.Condition
import Amazonka.GuardDuty.Types.Container
import Amazonka.GuardDuty.Types.Country
import Amazonka.GuardDuty.Types.CriterionKey
import Amazonka.GuardDuty.Types.DNSLogsConfigurationResult
import Amazonka.GuardDuty.Types.DataSource
import Amazonka.GuardDuty.Types.DataSourceConfigurations
import Amazonka.GuardDuty.Types.DataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.DataSourceFreeTrial
import Amazonka.GuardDuty.Types.DataSourceStatus
import Amazonka.GuardDuty.Types.DataSourcesFreeTrial
import Amazonka.GuardDuty.Types.DefaultServerSideEncryption
import Amazonka.GuardDuty.Types.Destination
import Amazonka.GuardDuty.Types.DestinationProperties
import Amazonka.GuardDuty.Types.DestinationType
import Amazonka.GuardDuty.Types.DetectorStatus
import Amazonka.GuardDuty.Types.DnsRequestAction
import Amazonka.GuardDuty.Types.DomainDetails
import Amazonka.GuardDuty.Types.EbsSnapshotPreservation
import Amazonka.GuardDuty.Types.EbsVolumeDetails
import Amazonka.GuardDuty.Types.EbsVolumeScanDetails
import Amazonka.GuardDuty.Types.EbsVolumesResult
import Amazonka.GuardDuty.Types.EcsClusterDetails
import Amazonka.GuardDuty.Types.EcsTaskDetails
import Amazonka.GuardDuty.Types.EksClusterDetails
import Amazonka.GuardDuty.Types.Evidence
import Amazonka.GuardDuty.Types.Feedback
import Amazonka.GuardDuty.Types.FilterAction
import Amazonka.GuardDuty.Types.FilterCondition
import Amazonka.GuardDuty.Types.FilterCriteria
import Amazonka.GuardDuty.Types.FilterCriterion
import Amazonka.GuardDuty.Types.Finding
import Amazonka.GuardDuty.Types.FindingCriteria
import Amazonka.GuardDuty.Types.FindingPublishingFrequency
import Amazonka.GuardDuty.Types.FindingStatisticType
import Amazonka.GuardDuty.Types.FindingStatistics
import Amazonka.GuardDuty.Types.FlowLogsConfigurationResult
import Amazonka.GuardDuty.Types.GeoLocation
import Amazonka.GuardDuty.Types.HighestSeverityThreatDetails
import Amazonka.GuardDuty.Types.HostPath
import Amazonka.GuardDuty.Types.IamInstanceProfile
import Amazonka.GuardDuty.Types.InstanceDetails
import Amazonka.GuardDuty.Types.Invitation
import Amazonka.GuardDuty.Types.IpSetFormat
import Amazonka.GuardDuty.Types.IpSetStatus
import Amazonka.GuardDuty.Types.KubernetesApiCallAction
import Amazonka.GuardDuty.Types.KubernetesAuditLogsConfiguration
import Amazonka.GuardDuty.Types.KubernetesAuditLogsConfigurationResult
import Amazonka.GuardDuty.Types.KubernetesConfiguration
import Amazonka.GuardDuty.Types.KubernetesConfigurationResult
import Amazonka.GuardDuty.Types.KubernetesDataSourceFreeTrial
import Amazonka.GuardDuty.Types.KubernetesDetails
import Amazonka.GuardDuty.Types.KubernetesUserDetails
import Amazonka.GuardDuty.Types.KubernetesWorkloadDetails
import Amazonka.GuardDuty.Types.LocalIpDetails
import Amazonka.GuardDuty.Types.LocalPortDetails
import Amazonka.GuardDuty.Types.MalwareProtectionConfiguration
import Amazonka.GuardDuty.Types.MalwareProtectionConfigurationResult
import Amazonka.GuardDuty.Types.MalwareProtectionDataSourceFreeTrial
import Amazonka.GuardDuty.Types.Member
import Amazonka.GuardDuty.Types.MemberDataSourceConfiguration
import Amazonka.GuardDuty.Types.NetworkConnectionAction
import Amazonka.GuardDuty.Types.NetworkInterface
import Amazonka.GuardDuty.Types.OrderBy
import Amazonka.GuardDuty.Types.Organization
import Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurations
import Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.OrganizationEbsVolumes
import Amazonka.GuardDuty.Types.OrganizationEbsVolumesResult
import Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfiguration
import Amazonka.GuardDuty.Types.OrganizationKubernetesAuditLogsConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationKubernetesConfiguration
import Amazonka.GuardDuty.Types.OrganizationKubernetesConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationMalwareProtectionConfiguration
import Amazonka.GuardDuty.Types.OrganizationMalwareProtectionConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfiguration
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindings
import Amazonka.GuardDuty.Types.OrganizationScanEc2InstanceWithFindingsResult
import Amazonka.GuardDuty.Types.Owner
import Amazonka.GuardDuty.Types.PermissionConfiguration
import Amazonka.GuardDuty.Types.PortProbeAction
import Amazonka.GuardDuty.Types.PortProbeDetail
import Amazonka.GuardDuty.Types.PrivateIpAddressDetails
import Amazonka.GuardDuty.Types.ProductCode
import Amazonka.GuardDuty.Types.PublicAccess
import Amazonka.GuardDuty.Types.PublishingStatus
import Amazonka.GuardDuty.Types.RemoteAccountDetails
import Amazonka.GuardDuty.Types.RemoteIpDetails
import Amazonka.GuardDuty.Types.RemotePortDetails
import Amazonka.GuardDuty.Types.Resource
import Amazonka.GuardDuty.Types.ResourceDetails
import Amazonka.GuardDuty.Types.S3BucketDetail
import Amazonka.GuardDuty.Types.S3LogsConfiguration
import Amazonka.GuardDuty.Types.S3LogsConfigurationResult
import Amazonka.GuardDuty.Types.Scan
import Amazonka.GuardDuty.Types.ScanCondition
import Amazonka.GuardDuty.Types.ScanConditionPair
import Amazonka.GuardDuty.Types.ScanCriterionKey
import Amazonka.GuardDuty.Types.ScanDetections
import Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindings
import Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindingsResult
import Amazonka.GuardDuty.Types.ScanFilePath
import Amazonka.GuardDuty.Types.ScanResourceCriteria
import Amazonka.GuardDuty.Types.ScanResult
import Amazonka.GuardDuty.Types.ScanResultDetails
import Amazonka.GuardDuty.Types.ScanStatus
import Amazonka.GuardDuty.Types.ScanThreatName
import Amazonka.GuardDuty.Types.ScannedItemCount
import Amazonka.GuardDuty.Types.SecurityContext
import Amazonka.GuardDuty.Types.SecurityGroup
import Amazonka.GuardDuty.Types.ServiceAdditionalInfo
import Amazonka.GuardDuty.Types.ServiceInfo
import Amazonka.GuardDuty.Types.SortCriteria
import Amazonka.GuardDuty.Types.Tag
import Amazonka.GuardDuty.Types.ThreatDetectedByName
import Amazonka.GuardDuty.Types.ThreatIntelSetFormat
import Amazonka.GuardDuty.Types.ThreatIntelSetStatus
import Amazonka.GuardDuty.Types.ThreatIntelligenceDetail
import Amazonka.GuardDuty.Types.ThreatsDetectedItemCount
import Amazonka.GuardDuty.Types.Total
import Amazonka.GuardDuty.Types.TriggerDetails
import Amazonka.GuardDuty.Types.UnprocessedAccount
import Amazonka.GuardDuty.Types.UnprocessedDataSourcesResult
import Amazonka.GuardDuty.Types.UsageAccountResult
import Amazonka.GuardDuty.Types.UsageCriteria
import Amazonka.GuardDuty.Types.UsageDataSourceResult
import Amazonka.GuardDuty.Types.UsageResourceResult
import Amazonka.GuardDuty.Types.UsageStatisticType
import Amazonka.GuardDuty.Types.UsageStatistics
import Amazonka.GuardDuty.Types.Volume
import Amazonka.GuardDuty.Types.VolumeDetail
import Amazonka.GuardDuty.Types.VolumeMount
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-11-28@ of the Amazon GuardDuty SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "GuardDuty",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "guardduty",
      Core.signingName = "guardduty",
      Core.version = "2017-11-28",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "GuardDuty",
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

-- | A bad request exception object.
_BadRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | An internal server error exception object.
_InternalServerErrorException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500
