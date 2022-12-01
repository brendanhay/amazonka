{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerErrorException,
    _BadRequestException,

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
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_userType,
    accessKeyDetails_accessKeyId,

    -- * AccountDetail
    AccountDetail (..),
    newAccountDetail,
    accountDetail_accountId,
    accountDetail_email,

    -- * AccountFreeTrialInfo
    AccountFreeTrialInfo (..),
    newAccountFreeTrialInfo,
    accountFreeTrialInfo_dataSources,
    accountFreeTrialInfo_accountId,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    newAccountLevelPermissions,
    accountLevelPermissions_blockPublicAccess,

    -- * Action
    Action (..),
    newAction,
    action_actionType,
    action_networkConnectionAction,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_kubernetesApiCallAction,
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
    administrator_invitedAt,
    administrator_relationshipStatus,
    administrator_invitationId,

    -- * AwsApiCallAction
    AwsApiCallAction (..),
    newAwsApiCallAction,
    awsApiCallAction_affectedResources,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_remoteAccountDetails,
    awsApiCallAction_domainDetails,
    awsApiCallAction_api,
    awsApiCallAction_errorCode,
    awsApiCallAction_serviceName,
    awsApiCallAction_userAgent,
    awsApiCallAction_callerType,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_blockPublicAcls,

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
    condition_notEquals,
    condition_lessThanOrEqual,
    condition_neq,
    condition_equals,
    condition_lessThan,
    condition_lte,
    condition_lt,
    condition_gte,
    condition_eq,
    condition_gt,
    condition_greaterThanOrEqual,
    condition_greaterThan,

    -- * Container
    Container (..),
    newContainer,
    container_name,
    container_containerRuntime,
    container_id,
    container_imagePrefix,
    container_securityContext,
    container_image,
    container_volumeMounts,

    -- * Country
    Country (..),
    newCountry,
    country_countryName,
    country_countryCode,

    -- * DNSLogsConfigurationResult
    DNSLogsConfigurationResult (..),
    newDNSLogsConfigurationResult,
    dNSLogsConfigurationResult_status,

    -- * DataSourceConfigurations
    DataSourceConfigurations (..),
    newDataSourceConfigurations,
    dataSourceConfigurations_malwareProtection,
    dataSourceConfigurations_s3Logs,
    dataSourceConfigurations_kubernetes,

    -- * DataSourceConfigurationsResult
    DataSourceConfigurationsResult (..),
    newDataSourceConfigurationsResult,
    dataSourceConfigurationsResult_malwareProtection,
    dataSourceConfigurationsResult_kubernetes,
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
    dataSourcesFreeTrial_malwareProtection,
    dataSourcesFreeTrial_cloudTrail,
    dataSourcesFreeTrial_dnsLogs,
    dataSourcesFreeTrial_s3Logs,
    dataSourcesFreeTrial_kubernetes,
    dataSourcesFreeTrial_flowLogs,

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
    destinationProperties_kmsKeyArn,
    destinationProperties_destinationArn,

    -- * DnsRequestAction
    DnsRequestAction (..),
    newDnsRequestAction,
    dnsRequestAction_domain,
    dnsRequestAction_blocked,
    dnsRequestAction_protocol,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domain,

    -- * EbsVolumeDetails
    EbsVolumeDetails (..),
    newEbsVolumeDetails,
    ebsVolumeDetails_skippedVolumeDetails,
    ebsVolumeDetails_scannedVolumeDetails,

    -- * EbsVolumeScanDetails
    EbsVolumeScanDetails (..),
    newEbsVolumeScanDetails,
    ebsVolumeScanDetails_sources,
    ebsVolumeScanDetails_triggerFindingId,
    ebsVolumeScanDetails_scanDetections,
    ebsVolumeScanDetails_scanId,
    ebsVolumeScanDetails_scanCompletedAt,
    ebsVolumeScanDetails_scanStartedAt,

    -- * EbsVolumesResult
    EbsVolumesResult (..),
    newEbsVolumesResult,
    ebsVolumesResult_status,
    ebsVolumesResult_reason,

    -- * EcsClusterDetails
    EcsClusterDetails (..),
    newEcsClusterDetails,
    ecsClusterDetails_tags,
    ecsClusterDetails_name,
    ecsClusterDetails_taskDetails,
    ecsClusterDetails_arn,
    ecsClusterDetails_registeredContainerInstancesCount,
    ecsClusterDetails_status,
    ecsClusterDetails_runningTasksCount,
    ecsClusterDetails_activeServicesCount,

    -- * EcsTaskDetails
    EcsTaskDetails (..),
    newEcsTaskDetails,
    ecsTaskDetails_tags,
    ecsTaskDetails_containers,
    ecsTaskDetails_arn,
    ecsTaskDetails_startedBy,
    ecsTaskDetails_volumes,
    ecsTaskDetails_taskCreatedAt,
    ecsTaskDetails_startedAt,
    ecsTaskDetails_definitionArn,
    ecsTaskDetails_group,
    ecsTaskDetails_version,

    -- * EksClusterDetails
    EksClusterDetails (..),
    newEksClusterDetails,
    eksClusterDetails_tags,
    eksClusterDetails_name,
    eksClusterDetails_arn,
    eksClusterDetails_status,
    eksClusterDetails_vpcId,
    eksClusterDetails_createdAt,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_threatIntelligenceDetails,

    -- * FilterCondition
    FilterCondition (..),
    newFilterCondition,
    filterCondition_equalsValue,
    filterCondition_lessThan,
    filterCondition_greaterThan,

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
    finding_service,
    finding_partition,
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
    highestSeverityThreatDetails_severity,
    highestSeverityThreatDetails_count,
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
    instanceDetails_tags,
    instanceDetails_instanceState,
    instanceDetails_iamInstanceProfile,
    instanceDetails_outpostArn,
    instanceDetails_imageDescription,
    instanceDetails_launchTime,
    instanceDetails_productCodes,
    instanceDetails_platform,
    instanceDetails_availabilityZone,
    instanceDetails_instanceType,
    instanceDetails_instanceId,
    instanceDetails_imageId,
    instanceDetails_networkInterfaces,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_accountId,
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,

    -- * KubernetesApiCallAction
    KubernetesApiCallAction (..),
    newKubernetesApiCallAction,
    kubernetesApiCallAction_remoteIpDetails,
    kubernetesApiCallAction_requestUri,
    kubernetesApiCallAction_statusCode,
    kubernetesApiCallAction_userAgent,
    kubernetesApiCallAction_verb,
    kubernetesApiCallAction_sourceIps,
    kubernetesApiCallAction_parameters,

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
    kubernetesUserDetails_username,
    kubernetesUserDetails_uid,
    kubernetesUserDetails_groups,

    -- * KubernetesWorkloadDetails
    KubernetesWorkloadDetails (..),
    newKubernetesWorkloadDetails,
    kubernetesWorkloadDetails_name,
    kubernetesWorkloadDetails_type,
    kubernetesWorkloadDetails_containers,
    kubernetesWorkloadDetails_uid,
    kubernetesWorkloadDetails_volumes,
    kubernetesWorkloadDetails_namespace,
    kubernetesWorkloadDetails_hostNetwork,

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
    malwareProtectionConfigurationResult_serviceRole,
    malwareProtectionConfigurationResult_scanEc2InstanceWithFindings,

    -- * MalwareProtectionDataSourceFreeTrial
    MalwareProtectionDataSourceFreeTrial (..),
    newMalwareProtectionDataSourceFreeTrial,
    malwareProtectionDataSourceFreeTrial_scanEc2InstanceWithFindings,

    -- * Member
    Member (..),
    newMember,
    member_administratorId,
    member_invitedAt,
    member_detectorId,
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
    networkConnectionAction_connectionDirection,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_protocol,
    networkConnectionAction_remotePortDetails,

    -- * NetworkInterface
    NetworkInterface (..),
    newNetworkInterface,
    networkInterface_privateIpAddresses,
    networkInterface_subnetId,
    networkInterface_publicIp,
    networkInterface_networkInterfaceId,
    networkInterface_publicDnsName,
    networkInterface_securityGroups,
    networkInterface_privateIpAddress,
    networkInterface_privateDnsName,
    networkInterface_vpcId,
    networkInterface_ipv6Addresses,

    -- * Organization
    Organization (..),
    newOrganization,
    organization_isp,
    organization_org,
    organization_asn,
    organization_asnOrg,

    -- * OrganizationDataSourceConfigurations
    OrganizationDataSourceConfigurations (..),
    newOrganizationDataSourceConfigurations,
    organizationDataSourceConfigurations_malwareProtection,
    organizationDataSourceConfigurations_s3Logs,
    organizationDataSourceConfigurations_kubernetes,

    -- * OrganizationDataSourceConfigurationsResult
    OrganizationDataSourceConfigurationsResult (..),
    newOrganizationDataSourceConfigurationsResult,
    organizationDataSourceConfigurationsResult_malwareProtection,
    organizationDataSourceConfigurationsResult_kubernetes,
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
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_localIpDetails,

    -- * PrivateIpAddressDetails
    PrivateIpAddressDetails (..),
    newPrivateIpAddressDetails,
    privateIpAddressDetails_privateIpAddress,
    privateIpAddressDetails_privateDnsName,

    -- * ProductCode
    ProductCode (..),
    newProductCode,
    productCode_code,
    productCode_productType,

    -- * PublicAccess
    PublicAccess (..),
    newPublicAccess,
    publicAccess_permissionConfiguration,
    publicAccess_effectivePermission,

    -- * RemoteAccountDetails
    RemoteAccountDetails (..),
    newRemoteAccountDetails,
    remoteAccountDetails_accountId,
    remoteAccountDetails_affiliated,

    -- * RemoteIpDetails
    RemoteIpDetails (..),
    newRemoteIpDetails,
    remoteIpDetails_country,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_city,
    remoteIpDetails_organization,
    remoteIpDetails_geoLocation,

    -- * RemotePortDetails
    RemotePortDetails (..),
    newRemotePortDetails,
    remotePortDetails_port,
    remotePortDetails_portName,

    -- * Resource
    Resource (..),
    newResource,
    resource_resourceType,
    resource_instanceDetails,
    resource_s3BucketDetails,
    resource_ecsClusterDetails,
    resource_containerDetails,
    resource_accessKeyDetails,
    resource_eksClusterDetails,
    resource_kubernetesDetails,
    resource_ebsVolumeDetails,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_instanceArn,

    -- * S3BucketDetail
    S3BucketDetail (..),
    newS3BucketDetail,
    s3BucketDetail_tags,
    s3BucketDetail_name,
    s3BucketDetail_type,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_arn,
    s3BucketDetail_publicAccess,
    s3BucketDetail_owner,
    s3BucketDetail_createdAt,

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
    scan_attachedVolumes,
    scan_scanStartTime,
    scan_adminDetectorId,
    scan_scanEndTime,
    scan_scanStatus,
    scan_resourceDetails,
    scan_accountId,
    scan_scanResultDetails,
    scan_triggerDetails,
    scan_totalBytes,
    scan_detectorId,
    scan_scanId,
    scan_fileCount,
    scan_failureReason,

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
    scanDetections_threatsDetectedItemCount,
    scanDetections_scannedItemCount,
    scanDetections_threatDetectedByName,
    scanDetections_highestSeverityThreatDetails,

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
    scanFilePath_filePath,
    scanFilePath_hash,
    scanFilePath_fileName,
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
    scanThreatName_severity,
    scanThreatName_name,
    scanThreatName_itemCount,
    scanThreatName_filePaths,

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
    securityGroup_groupName,
    securityGroup_groupId,

    -- * ServiceAdditionalInfo
    ServiceAdditionalInfo (..),
    newServiceAdditionalInfo,
    serviceAdditionalInfo_type,
    serviceAdditionalInfo_value,

    -- * ServiceInfo
    ServiceInfo (..),
    newServiceInfo,
    serviceInfo_additionalInfo,
    serviceInfo_resourceRole,
    serviceInfo_evidence,
    serviceInfo_userFeedback,
    serviceInfo_featureName,
    serviceInfo_count,
    serviceInfo_eventFirstSeen,
    serviceInfo_archived,
    serviceInfo_action,
    serviceInfo_ebsVolumeScanDetails,
    serviceInfo_detectorId,
    serviceInfo_serviceName,
    serviceInfo_eventLastSeen,

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
    threatDetectedByName_uniqueThreatNameCount,
    threatDetectedByName_threatNames,
    threatDetectedByName_shortened,

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
    total_unit,
    total_amount,

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
    usageAccountResult_total,
    usageAccountResult_accountId,

    -- * UsageCriteria
    UsageCriteria (..),
    newUsageCriteria,
    usageCriteria_accountIds,
    usageCriteria_resources,
    usageCriteria_dataSources,

    -- * UsageDataSourceResult
    UsageDataSourceResult (..),
    newUsageDataSourceResult,
    usageDataSourceResult_total,
    usageDataSourceResult_dataSource,

    -- * UsageResourceResult
    UsageResourceResult (..),
    newUsageResourceResult,
    usageResourceResult_total,
    usageResourceResult_resource,

    -- * UsageStatistics
    UsageStatistics (..),
    newUsageStatistics,
    usageStatistics_sumByDataSource,
    usageStatistics_topResources,
    usageStatistics_sumByResource,
    usageStatistics_sumByAccount,

    -- * Volume
    Volume (..),
    newVolume,
    volume_name,
    volume_hostPath,

    -- * VolumeDetail
    VolumeDetail (..),
    newVolumeDetail,
    volumeDetail_snapshotArn,
    volumeDetail_deviceName,
    volumeDetail_volumeArn,
    volumeDetail_volumeType,
    volumeDetail_kmsKeyArn,
    volumeDetail_encryptionType,
    volumeDetail_volumeSizeInGB,

    -- * VolumeMount
    VolumeMount (..),
    newVolumeMount,
    volumeMount_name,
    volumeMount_mountPath,
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

-- | An internal server error exception object.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Core.hasStatus 500

-- | A bad request exception object.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400
