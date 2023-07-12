{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Lens
  ( -- * Operations

    -- ** AcceptAdministratorInvitation
    acceptAdministratorInvitation_detectorId,
    acceptAdministratorInvitation_administratorId,
    acceptAdministratorInvitation_invitationId,
    acceptAdministratorInvitationResponse_httpStatus,

    -- ** ArchiveFindings
    archiveFindings_detectorId,
    archiveFindings_findingIds,
    archiveFindingsResponse_httpStatus,

    -- ** CreateDetector
    createDetector_clientToken,
    createDetector_dataSources,
    createDetector_findingPublishingFrequency,
    createDetector_tags,
    createDetector_enable,
    createDetectorResponse_detectorId,
    createDetectorResponse_unprocessedDataSources,
    createDetectorResponse_httpStatus,

    -- ** CreateFilter
    createFilter_action,
    createFilter_clientToken,
    createFilter_description,
    createFilter_rank,
    createFilter_tags,
    createFilter_detectorId,
    createFilter_name,
    createFilter_findingCriteria,
    createFilterResponse_httpStatus,
    createFilterResponse_name,

    -- ** CreateIPSet
    createIPSet_clientToken,
    createIPSet_tags,
    createIPSet_detectorId,
    createIPSet_name,
    createIPSet_format,
    createIPSet_location,
    createIPSet_activate,
    createIPSetResponse_httpStatus,
    createIPSetResponse_ipSetId,

    -- ** CreateMembers
    createMembers_detectorId,
    createMembers_accountDetails,
    createMembersResponse_httpStatus,
    createMembersResponse_unprocessedAccounts,

    -- ** CreatePublishingDestination
    createPublishingDestination_clientToken,
    createPublishingDestination_detectorId,
    createPublishingDestination_destinationType,
    createPublishingDestination_destinationProperties,
    createPublishingDestinationResponse_httpStatus,
    createPublishingDestinationResponse_destinationId,

    -- ** CreateSampleFindings
    createSampleFindings_findingTypes,
    createSampleFindings_detectorId,
    createSampleFindingsResponse_httpStatus,

    -- ** CreateThreatIntelSet
    createThreatIntelSet_clientToken,
    createThreatIntelSet_tags,
    createThreatIntelSet_detectorId,
    createThreatIntelSet_name,
    createThreatIntelSet_format,
    createThreatIntelSet_location,
    createThreatIntelSet_activate,
    createThreatIntelSetResponse_httpStatus,
    createThreatIntelSetResponse_threatIntelSetId,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_httpStatus,
    declineInvitationsResponse_unprocessedAccounts,

    -- ** DeleteDetector
    deleteDetector_detectorId,
    deleteDetectorResponse_httpStatus,

    -- ** DeleteFilter
    deleteFilter_detectorId,
    deleteFilter_filterName,
    deleteFilterResponse_httpStatus,

    -- ** DeleteIPSet
    deleteIPSet_detectorId,
    deleteIPSet_ipSetId,
    deleteIPSetResponse_httpStatus,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_httpStatus,
    deleteInvitationsResponse_unprocessedAccounts,

    -- ** DeleteMembers
    deleteMembers_detectorId,
    deleteMembers_accountIds,
    deleteMembersResponse_httpStatus,
    deleteMembersResponse_unprocessedAccounts,

    -- ** DeletePublishingDestination
    deletePublishingDestination_detectorId,
    deletePublishingDestination_destinationId,
    deletePublishingDestinationResponse_httpStatus,

    -- ** DeleteThreatIntelSet
    deleteThreatIntelSet_detectorId,
    deleteThreatIntelSet_threatIntelSetId,
    deleteThreatIntelSetResponse_httpStatus,

    -- ** DescribeMalwareScans
    describeMalwareScans_filterCriteria,
    describeMalwareScans_maxResults,
    describeMalwareScans_nextToken,
    describeMalwareScans_sortCriteria,
    describeMalwareScans_detectorId,
    describeMalwareScansResponse_nextToken,
    describeMalwareScansResponse_httpStatus,
    describeMalwareScansResponse_scans,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfiguration_detectorId,
    describeOrganizationConfigurationResponse_dataSources,
    describeOrganizationConfigurationResponse_httpStatus,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,

    -- ** DescribePublishingDestination
    describePublishingDestination_detectorId,
    describePublishingDestination_destinationId,
    describePublishingDestinationResponse_httpStatus,
    describePublishingDestinationResponse_destinationId,
    describePublishingDestinationResponse_destinationType,
    describePublishingDestinationResponse_status,
    describePublishingDestinationResponse_publishingFailureStartTimestamp,
    describePublishingDestinationResponse_destinationProperties,

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- ** DisassociateFromAdministratorAccount
    disassociateFromAdministratorAccount_detectorId,
    disassociateFromAdministratorAccountResponse_httpStatus,

    -- ** DisassociateMembers
    disassociateMembers_detectorId,
    disassociateMembers_accountIds,
    disassociateMembersResponse_httpStatus,
    disassociateMembersResponse_unprocessedAccounts,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** GetAdministratorAccount
    getAdministratorAccount_detectorId,
    getAdministratorAccountResponse_httpStatus,
    getAdministratorAccountResponse_administrator,

    -- ** GetDetector
    getDetector_detectorId,
    getDetectorResponse_createdAt,
    getDetectorResponse_dataSources,
    getDetectorResponse_findingPublishingFrequency,
    getDetectorResponse_tags,
    getDetectorResponse_updatedAt,
    getDetectorResponse_httpStatus,
    getDetectorResponse_serviceRole,
    getDetectorResponse_status,

    -- ** GetFilter
    getFilter_detectorId,
    getFilter_filterName,
    getFilterResponse_description,
    getFilterResponse_rank,
    getFilterResponse_tags,
    getFilterResponse_httpStatus,
    getFilterResponse_name,
    getFilterResponse_action,
    getFilterResponse_findingCriteria,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_detectorId,
    getFindings_findingIds,
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,

    -- ** GetFindingsStatistics
    getFindingsStatistics_findingCriteria,
    getFindingsStatistics_detectorId,
    getFindingsStatistics_findingStatisticTypes,
    getFindingsStatisticsResponse_httpStatus,
    getFindingsStatisticsResponse_findingStatistics,

    -- ** GetIPSet
    getIPSet_detectorId,
    getIPSet_ipSetId,
    getIPSetResponse_tags,
    getIPSetResponse_httpStatus,
    getIPSetResponse_name,
    getIPSetResponse_format,
    getIPSetResponse_location,
    getIPSetResponse_status,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** GetMalwareScanSettings
    getMalwareScanSettings_detectorId,
    getMalwareScanSettingsResponse_ebsSnapshotPreservation,
    getMalwareScanSettingsResponse_scanResourceCriteria,
    getMalwareScanSettingsResponse_httpStatus,

    -- ** GetMemberDetectors
    getMemberDetectors_detectorId,
    getMemberDetectors_accountIds,
    getMemberDetectorsResponse_httpStatus,
    getMemberDetectorsResponse_memberDataSourceConfigurations,
    getMemberDetectorsResponse_unprocessedAccounts,

    -- ** GetMembers
    getMembers_detectorId,
    getMembers_accountIds,
    getMembersResponse_httpStatus,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,

    -- ** GetRemainingFreeTrialDays
    getRemainingFreeTrialDays_accountIds,
    getRemainingFreeTrialDays_detectorId,
    getRemainingFreeTrialDaysResponse_accounts,
    getRemainingFreeTrialDaysResponse_unprocessedAccounts,
    getRemainingFreeTrialDaysResponse_httpStatus,

    -- ** GetThreatIntelSet
    getThreatIntelSet_detectorId,
    getThreatIntelSet_threatIntelSetId,
    getThreatIntelSetResponse_tags,
    getThreatIntelSetResponse_httpStatus,
    getThreatIntelSetResponse_name,
    getThreatIntelSetResponse_format,
    getThreatIntelSetResponse_location,
    getThreatIntelSetResponse_status,

    -- ** GetUsageStatistics
    getUsageStatistics_maxResults,
    getUsageStatistics_nextToken,
    getUsageStatistics_unit,
    getUsageStatistics_detectorId,
    getUsageStatistics_usageStatisticType,
    getUsageStatistics_usageCriteria,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_usageStatistics,
    getUsageStatisticsResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_disableEmailNotification,
    inviteMembers_message,
    inviteMembers_detectorId,
    inviteMembers_accountIds,
    inviteMembersResponse_httpStatus,
    inviteMembersResponse_unprocessedAccounts,

    -- ** ListDetectors
    listDetectors_maxResults,
    listDetectors_nextToken,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
    listDetectorsResponse_detectorIds,

    -- ** ListFilters
    listFilters_maxResults,
    listFilters_nextToken,
    listFilters_detectorId,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filterNames,

    -- ** ListFindings
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindings_sortCriteria,
    listFindings_detectorId,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingIds,

    -- ** ListIPSets
    listIPSets_maxResults,
    listIPSets_nextToken,
    listIPSets_detectorId,
    listIPSetsResponse_nextToken,
    listIPSetsResponse_httpStatus,
    listIPSetsResponse_ipSetIds,

    -- ** ListInvitations
    listInvitations_maxResults,
    listInvitations_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_detectorId,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListPublishingDestinations
    listPublishingDestinations_maxResults,
    listPublishingDestinations_nextToken,
    listPublishingDestinations_detectorId,
    listPublishingDestinationsResponse_nextToken,
    listPublishingDestinationsResponse_httpStatus,
    listPublishingDestinationsResponse_destinations,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThreatIntelSets
    listThreatIntelSets_maxResults,
    listThreatIntelSets_nextToken,
    listThreatIntelSets_detectorId,
    listThreatIntelSetsResponse_nextToken,
    listThreatIntelSetsResponse_httpStatus,
    listThreatIntelSetsResponse_threatIntelSetIds,

    -- ** StartMonitoringMembers
    startMonitoringMembers_detectorId,
    startMonitoringMembers_accountIds,
    startMonitoringMembersResponse_httpStatus,
    startMonitoringMembersResponse_unprocessedAccounts,

    -- ** StopMonitoringMembers
    stopMonitoringMembers_detectorId,
    stopMonitoringMembers_accountIds,
    stopMonitoringMembersResponse_httpStatus,
    stopMonitoringMembersResponse_unprocessedAccounts,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UnarchiveFindings
    unarchiveFindings_detectorId,
    unarchiveFindings_findingIds,
    unarchiveFindingsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDetector
    updateDetector_dataSources,
    updateDetector_enable,
    updateDetector_findingPublishingFrequency,
    updateDetector_detectorId,
    updateDetectorResponse_httpStatus,

    -- ** UpdateFilter
    updateFilter_action,
    updateFilter_description,
    updateFilter_findingCriteria,
    updateFilter_rank,
    updateFilter_detectorId,
    updateFilter_filterName,
    updateFilterResponse_httpStatus,
    updateFilterResponse_name,

    -- ** UpdateFindingsFeedback
    updateFindingsFeedback_comments,
    updateFindingsFeedback_detectorId,
    updateFindingsFeedback_findingIds,
    updateFindingsFeedback_feedback,
    updateFindingsFeedbackResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_activate,
    updateIPSet_location,
    updateIPSet_name,
    updateIPSet_detectorId,
    updateIPSet_ipSetId,
    updateIPSetResponse_httpStatus,

    -- ** UpdateMalwareScanSettings
    updateMalwareScanSettings_ebsSnapshotPreservation,
    updateMalwareScanSettings_scanResourceCriteria,
    updateMalwareScanSettings_detectorId,
    updateMalwareScanSettingsResponse_httpStatus,

    -- ** UpdateMemberDetectors
    updateMemberDetectors_dataSources,
    updateMemberDetectors_detectorId,
    updateMemberDetectors_accountIds,
    updateMemberDetectorsResponse_httpStatus,
    updateMemberDetectorsResponse_unprocessedAccounts,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_dataSources,
    updateOrganizationConfiguration_detectorId,
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** UpdatePublishingDestination
    updatePublishingDestination_destinationProperties,
    updatePublishingDestination_detectorId,
    updatePublishingDestination_destinationId,
    updatePublishingDestinationResponse_httpStatus,

    -- ** UpdateThreatIntelSet
    updateThreatIntelSet_activate,
    updateThreatIntelSet_location,
    updateThreatIntelSet_name,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,
    updateThreatIntelSetResponse_httpStatus,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

    -- ** AccessKeyDetails
    accessKeyDetails_accessKeyId,
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_userType,

    -- ** AccountDetail
    accountDetail_accountId,
    accountDetail_email,

    -- ** AccountFreeTrialInfo
    accountFreeTrialInfo_accountId,
    accountFreeTrialInfo_dataSources,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** Action
    action_actionType,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_kubernetesApiCallAction,
    action_networkConnectionAction,
    action_portProbeAction,

    -- ** AdminAccount
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- ** Administrator
    administrator_accountId,
    administrator_invitationId,
    administrator_invitedAt,
    administrator_relationshipStatus,

    -- ** AwsApiCallAction
    awsApiCallAction_affectedResources,
    awsApiCallAction_api,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_errorCode,
    awsApiCallAction_remoteAccountDetails,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_serviceName,
    awsApiCallAction_userAgent,

    -- ** BlockPublicAccess
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_restrictPublicBuckets,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- ** City
    city_cityName,

    -- ** CloudTrailConfigurationResult
    cloudTrailConfigurationResult_status,

    -- ** Condition
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

    -- ** Container
    container_containerRuntime,
    container_id,
    container_image,
    container_imagePrefix,
    container_name,
    container_securityContext,
    container_volumeMounts,

    -- ** Country
    country_countryCode,
    country_countryName,

    -- ** DNSLogsConfigurationResult
    dNSLogsConfigurationResult_status,

    -- ** DataSourceConfigurations
    dataSourceConfigurations_kubernetes,
    dataSourceConfigurations_malwareProtection,
    dataSourceConfigurations_s3Logs,

    -- ** DataSourceConfigurationsResult
    dataSourceConfigurationsResult_kubernetes,
    dataSourceConfigurationsResult_malwareProtection,
    dataSourceConfigurationsResult_cloudTrail,
    dataSourceConfigurationsResult_dNSLogs,
    dataSourceConfigurationsResult_flowLogs,
    dataSourceConfigurationsResult_s3Logs,

    -- ** DataSourceFreeTrial
    dataSourceFreeTrial_freeTrialDaysRemaining,

    -- ** DataSourcesFreeTrial
    dataSourcesFreeTrial_cloudTrail,
    dataSourcesFreeTrial_dnsLogs,
    dataSourcesFreeTrial_flowLogs,
    dataSourcesFreeTrial_kubernetes,
    dataSourcesFreeTrial_malwareProtection,
    dataSourcesFreeTrial_s3Logs,

    -- ** DefaultServerSideEncryption
    defaultServerSideEncryption_encryptionType,
    defaultServerSideEncryption_kmsMasterKeyArn,

    -- ** Destination
    destination_destinationId,
    destination_destinationType,
    destination_status,

    -- ** DestinationProperties
    destinationProperties_destinationArn,
    destinationProperties_kmsKeyArn,

    -- ** DnsRequestAction
    dnsRequestAction_blocked,
    dnsRequestAction_domain,
    dnsRequestAction_protocol,

    -- ** DomainDetails
    domainDetails_domain,

    -- ** EbsVolumeDetails
    ebsVolumeDetails_scannedVolumeDetails,
    ebsVolumeDetails_skippedVolumeDetails,

    -- ** EbsVolumeScanDetails
    ebsVolumeScanDetails_scanCompletedAt,
    ebsVolumeScanDetails_scanDetections,
    ebsVolumeScanDetails_scanId,
    ebsVolumeScanDetails_scanStartedAt,
    ebsVolumeScanDetails_sources,
    ebsVolumeScanDetails_triggerFindingId,

    -- ** EbsVolumesResult
    ebsVolumesResult_reason,
    ebsVolumesResult_status,

    -- ** EcsClusterDetails
    ecsClusterDetails_activeServicesCount,
    ecsClusterDetails_arn,
    ecsClusterDetails_name,
    ecsClusterDetails_registeredContainerInstancesCount,
    ecsClusterDetails_runningTasksCount,
    ecsClusterDetails_status,
    ecsClusterDetails_tags,
    ecsClusterDetails_taskDetails,

    -- ** EcsTaskDetails
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

    -- ** EksClusterDetails
    eksClusterDetails_arn,
    eksClusterDetails_createdAt,
    eksClusterDetails_name,
    eksClusterDetails_status,
    eksClusterDetails_tags,
    eksClusterDetails_vpcId,

    -- ** Evidence
    evidence_threatIntelligenceDetails,

    -- ** FilterCondition
    filterCondition_equalsValue,
    filterCondition_greaterThan,
    filterCondition_lessThan,

    -- ** FilterCriteria
    filterCriteria_filterCriterion,

    -- ** FilterCriterion
    filterCriterion_criterionKey,
    filterCriterion_filterCondition,

    -- ** Finding
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

    -- ** FindingCriteria
    findingCriteria_criterion,

    -- ** FindingStatistics
    findingStatistics_countBySeverity,

    -- ** FlowLogsConfigurationResult
    flowLogsConfigurationResult_status,

    -- ** GeoLocation
    geoLocation_lat,
    geoLocation_lon,

    -- ** HighestSeverityThreatDetails
    highestSeverityThreatDetails_count,
    highestSeverityThreatDetails_severity,
    highestSeverityThreatDetails_threatName,

    -- ** HostPath
    hostPath_path,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** InstanceDetails
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

    -- ** Invitation
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_relationshipStatus,

    -- ** KubernetesApiCallAction
    kubernetesApiCallAction_parameters,
    kubernetesApiCallAction_remoteIpDetails,
    kubernetesApiCallAction_requestUri,
    kubernetesApiCallAction_sourceIps,
    kubernetesApiCallAction_statusCode,
    kubernetesApiCallAction_userAgent,
    kubernetesApiCallAction_verb,

    -- ** KubernetesAuditLogsConfiguration
    kubernetesAuditLogsConfiguration_enable,

    -- ** KubernetesAuditLogsConfigurationResult
    kubernetesAuditLogsConfigurationResult_status,

    -- ** KubernetesConfiguration
    kubernetesConfiguration_auditLogs,

    -- ** KubernetesConfigurationResult
    kubernetesConfigurationResult_auditLogs,

    -- ** KubernetesDataSourceFreeTrial
    kubernetesDataSourceFreeTrial_auditLogs,

    -- ** KubernetesDetails
    kubernetesDetails_kubernetesUserDetails,
    kubernetesDetails_kubernetesWorkloadDetails,

    -- ** KubernetesUserDetails
    kubernetesUserDetails_groups,
    kubernetesUserDetails_uid,
    kubernetesUserDetails_username,

    -- ** KubernetesWorkloadDetails
    kubernetesWorkloadDetails_containers,
    kubernetesWorkloadDetails_hostNetwork,
    kubernetesWorkloadDetails_name,
    kubernetesWorkloadDetails_namespace,
    kubernetesWorkloadDetails_type,
    kubernetesWorkloadDetails_uid,
    kubernetesWorkloadDetails_volumes,

    -- ** LocalIpDetails
    localIpDetails_ipAddressV4,

    -- ** LocalPortDetails
    localPortDetails_port,
    localPortDetails_portName,

    -- ** MalwareProtectionConfiguration
    malwareProtectionConfiguration_scanEc2InstanceWithFindings,

    -- ** MalwareProtectionConfigurationResult
    malwareProtectionConfigurationResult_scanEc2InstanceWithFindings,
    malwareProtectionConfigurationResult_serviceRole,

    -- ** MalwareProtectionDataSourceFreeTrial
    malwareProtectionDataSourceFreeTrial_scanEc2InstanceWithFindings,

    -- ** Member
    member_administratorId,
    member_detectorId,
    member_invitedAt,
    member_accountId,
    member_masterId,
    member_email,
    member_relationshipStatus,
    member_updatedAt,

    -- ** MemberDataSourceConfiguration
    memberDataSourceConfiguration_accountId,
    memberDataSourceConfiguration_dataSources,

    -- ** NetworkConnectionAction
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_remotePortDetails,

    -- ** NetworkInterface
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

    -- ** Organization
    organization_asn,
    organization_asnOrg,
    organization_isp,
    organization_org,

    -- ** OrganizationDataSourceConfigurations
    organizationDataSourceConfigurations_kubernetes,
    organizationDataSourceConfigurations_malwareProtection,
    organizationDataSourceConfigurations_s3Logs,

    -- ** OrganizationDataSourceConfigurationsResult
    organizationDataSourceConfigurationsResult_kubernetes,
    organizationDataSourceConfigurationsResult_malwareProtection,
    organizationDataSourceConfigurationsResult_s3Logs,

    -- ** OrganizationEbsVolumes
    organizationEbsVolumes_autoEnable,

    -- ** OrganizationEbsVolumesResult
    organizationEbsVolumesResult_autoEnable,

    -- ** OrganizationKubernetesAuditLogsConfiguration
    organizationKubernetesAuditLogsConfiguration_autoEnable,

    -- ** OrganizationKubernetesAuditLogsConfigurationResult
    organizationKubernetesAuditLogsConfigurationResult_autoEnable,

    -- ** OrganizationKubernetesConfiguration
    organizationKubernetesConfiguration_auditLogs,

    -- ** OrganizationKubernetesConfigurationResult
    organizationKubernetesConfigurationResult_auditLogs,

    -- ** OrganizationMalwareProtectionConfiguration
    organizationMalwareProtectionConfiguration_scanEc2InstanceWithFindings,

    -- ** OrganizationMalwareProtectionConfigurationResult
    organizationMalwareProtectionConfigurationResult_scanEc2InstanceWithFindings,

    -- ** OrganizationS3LogsConfiguration
    organizationS3LogsConfiguration_autoEnable,

    -- ** OrganizationS3LogsConfigurationResult
    organizationS3LogsConfigurationResult_autoEnable,

    -- ** OrganizationScanEc2InstanceWithFindings
    organizationScanEc2InstanceWithFindings_ebsVolumes,

    -- ** OrganizationScanEc2InstanceWithFindingsResult
    organizationScanEc2InstanceWithFindingsResult_ebsVolumes,

    -- ** Owner
    owner_id,

    -- ** PermissionConfiguration
    permissionConfiguration_accountLevelPermissions,
    permissionConfiguration_bucketLevelPermissions,

    -- ** PortProbeAction
    portProbeAction_blocked,
    portProbeAction_portProbeDetails,

    -- ** PortProbeDetail
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_remoteIpDetails,

    -- ** PrivateIpAddressDetails
    privateIpAddressDetails_privateDnsName,
    privateIpAddressDetails_privateIpAddress,

    -- ** ProductCode
    productCode_code,
    productCode_productType,

    -- ** PublicAccess
    publicAccess_effectivePermission,
    publicAccess_permissionConfiguration,

    -- ** RemoteAccountDetails
    remoteAccountDetails_accountId,
    remoteAccountDetails_affiliated,

    -- ** RemoteIpDetails
    remoteIpDetails_city,
    remoteIpDetails_country,
    remoteIpDetails_geoLocation,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_organization,

    -- ** RemotePortDetails
    remotePortDetails_port,
    remotePortDetails_portName,

    -- ** Resource
    resource_accessKeyDetails,
    resource_containerDetails,
    resource_ebsVolumeDetails,
    resource_ecsClusterDetails,
    resource_eksClusterDetails,
    resource_instanceDetails,
    resource_kubernetesDetails,
    resource_resourceType,
    resource_s3BucketDetails,

    -- ** ResourceDetails
    resourceDetails_instanceArn,

    -- ** S3BucketDetail
    s3BucketDetail_arn,
    s3BucketDetail_createdAt,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_name,
    s3BucketDetail_owner,
    s3BucketDetail_publicAccess,
    s3BucketDetail_tags,
    s3BucketDetail_type,

    -- ** S3LogsConfiguration
    s3LogsConfiguration_enable,

    -- ** S3LogsConfigurationResult
    s3LogsConfigurationResult_status,

    -- ** Scan
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

    -- ** ScanCondition
    scanCondition_mapEquals,

    -- ** ScanConditionPair
    scanConditionPair_value,
    scanConditionPair_key,

    -- ** ScanDetections
    scanDetections_highestSeverityThreatDetails,
    scanDetections_scannedItemCount,
    scanDetections_threatDetectedByName,
    scanDetections_threatsDetectedItemCount,

    -- ** ScanEc2InstanceWithFindings
    scanEc2InstanceWithFindings_ebsVolumes,

    -- ** ScanEc2InstanceWithFindingsResult
    scanEc2InstanceWithFindingsResult_ebsVolumes,

    -- ** ScanFilePath
    scanFilePath_fileName,
    scanFilePath_filePath,
    scanFilePath_hash,
    scanFilePath_volumeArn,

    -- ** ScanResourceCriteria
    scanResourceCriteria_exclude,
    scanResourceCriteria_include,

    -- ** ScanResultDetails
    scanResultDetails_scanResult,

    -- ** ScanThreatName
    scanThreatName_filePaths,
    scanThreatName_itemCount,
    scanThreatName_name,
    scanThreatName_severity,

    -- ** ScannedItemCount
    scannedItemCount_files,
    scannedItemCount_totalGb,
    scannedItemCount_volumes,

    -- ** SecurityContext
    securityContext_privileged,

    -- ** SecurityGroup
    securityGroup_groupId,
    securityGroup_groupName,

    -- ** ServiceAdditionalInfo
    serviceAdditionalInfo_type,
    serviceAdditionalInfo_value,

    -- ** ServiceInfo
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

    -- ** SortCriteria
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThreatDetectedByName
    threatDetectedByName_itemCount,
    threatDetectedByName_shortened,
    threatDetectedByName_threatNames,
    threatDetectedByName_uniqueThreatNameCount,

    -- ** ThreatIntelligenceDetail
    threatIntelligenceDetail_threatListName,
    threatIntelligenceDetail_threatNames,

    -- ** ThreatsDetectedItemCount
    threatsDetectedItemCount_files,

    -- ** Total
    total_amount,
    total_unit,

    -- ** TriggerDetails
    triggerDetails_description,
    triggerDetails_guardDutyFindingId,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_result,

    -- ** UnprocessedDataSourcesResult
    unprocessedDataSourcesResult_malwareProtection,

    -- ** UsageAccountResult
    usageAccountResult_accountId,
    usageAccountResult_total,

    -- ** UsageCriteria
    usageCriteria_accountIds,
    usageCriteria_resources,
    usageCriteria_dataSources,

    -- ** UsageDataSourceResult
    usageDataSourceResult_dataSource,
    usageDataSourceResult_total,

    -- ** UsageResourceResult
    usageResourceResult_resource,
    usageResourceResult_total,

    -- ** UsageStatistics
    usageStatistics_sumByAccount,
    usageStatistics_sumByDataSource,
    usageStatistics_sumByResource,
    usageStatistics_topResources,

    -- ** Volume
    volume_hostPath,
    volume_name,

    -- ** VolumeDetail
    volumeDetail_deviceName,
    volumeDetail_encryptionType,
    volumeDetail_kmsKeyArn,
    volumeDetail_snapshotArn,
    volumeDetail_volumeArn,
    volumeDetail_volumeSizeInGB,
    volumeDetail_volumeType,

    -- ** VolumeMount
    volumeMount_mountPath,
    volumeMount_name,
  )
where

import Amazonka.GuardDuty.AcceptAdministratorInvitation
import Amazonka.GuardDuty.ArchiveFindings
import Amazonka.GuardDuty.CreateDetector
import Amazonka.GuardDuty.CreateFilter
import Amazonka.GuardDuty.CreateIPSet
import Amazonka.GuardDuty.CreateMembers
import Amazonka.GuardDuty.CreatePublishingDestination
import Amazonka.GuardDuty.CreateSampleFindings
import Amazonka.GuardDuty.CreateThreatIntelSet
import Amazonka.GuardDuty.DeclineInvitations
import Amazonka.GuardDuty.DeleteDetector
import Amazonka.GuardDuty.DeleteFilter
import Amazonka.GuardDuty.DeleteIPSet
import Amazonka.GuardDuty.DeleteInvitations
import Amazonka.GuardDuty.DeleteMembers
import Amazonka.GuardDuty.DeletePublishingDestination
import Amazonka.GuardDuty.DeleteThreatIntelSet
import Amazonka.GuardDuty.DescribeMalwareScans
import Amazonka.GuardDuty.DescribeOrganizationConfiguration
import Amazonka.GuardDuty.DescribePublishingDestination
import Amazonka.GuardDuty.DisableOrganizationAdminAccount
import Amazonka.GuardDuty.DisassociateFromAdministratorAccount
import Amazonka.GuardDuty.DisassociateMembers
import Amazonka.GuardDuty.EnableOrganizationAdminAccount
import Amazonka.GuardDuty.GetAdministratorAccount
import Amazonka.GuardDuty.GetDetector
import Amazonka.GuardDuty.GetFilter
import Amazonka.GuardDuty.GetFindings
import Amazonka.GuardDuty.GetFindingsStatistics
import Amazonka.GuardDuty.GetIPSet
import Amazonka.GuardDuty.GetInvitationsCount
import Amazonka.GuardDuty.GetMalwareScanSettings
import Amazonka.GuardDuty.GetMemberDetectors
import Amazonka.GuardDuty.GetMembers
import Amazonka.GuardDuty.GetRemainingFreeTrialDays
import Amazonka.GuardDuty.GetThreatIntelSet
import Amazonka.GuardDuty.GetUsageStatistics
import Amazonka.GuardDuty.InviteMembers
import Amazonka.GuardDuty.ListDetectors
import Amazonka.GuardDuty.ListFilters
import Amazonka.GuardDuty.ListFindings
import Amazonka.GuardDuty.ListIPSets
import Amazonka.GuardDuty.ListInvitations
import Amazonka.GuardDuty.ListMembers
import Amazonka.GuardDuty.ListOrganizationAdminAccounts
import Amazonka.GuardDuty.ListPublishingDestinations
import Amazonka.GuardDuty.ListTagsForResource
import Amazonka.GuardDuty.ListThreatIntelSets
import Amazonka.GuardDuty.StartMonitoringMembers
import Amazonka.GuardDuty.StopMonitoringMembers
import Amazonka.GuardDuty.TagResource
import Amazonka.GuardDuty.Types.AccessControlList
import Amazonka.GuardDuty.Types.AccessKeyDetails
import Amazonka.GuardDuty.Types.AccountDetail
import Amazonka.GuardDuty.Types.AccountFreeTrialInfo
import Amazonka.GuardDuty.Types.AccountLevelPermissions
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.AdminAccount
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
import Amazonka.GuardDuty.Types.DNSLogsConfigurationResult
import Amazonka.GuardDuty.Types.DataSourceConfigurations
import Amazonka.GuardDuty.Types.DataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.DataSourceFreeTrial
import Amazonka.GuardDuty.Types.DataSourcesFreeTrial
import Amazonka.GuardDuty.Types.DefaultServerSideEncryption
import Amazonka.GuardDuty.Types.Destination
import Amazonka.GuardDuty.Types.DestinationProperties
import Amazonka.GuardDuty.Types.DnsRequestAction
import Amazonka.GuardDuty.Types.DomainDetails
import Amazonka.GuardDuty.Types.EbsVolumeDetails
import Amazonka.GuardDuty.Types.EbsVolumeScanDetails
import Amazonka.GuardDuty.Types.EbsVolumesResult
import Amazonka.GuardDuty.Types.EcsClusterDetails
import Amazonka.GuardDuty.Types.EcsTaskDetails
import Amazonka.GuardDuty.Types.EksClusterDetails
import Amazonka.GuardDuty.Types.Evidence
import Amazonka.GuardDuty.Types.FilterCondition
import Amazonka.GuardDuty.Types.FilterCriteria
import Amazonka.GuardDuty.Types.FilterCriterion
import Amazonka.GuardDuty.Types.Finding
import Amazonka.GuardDuty.Types.FindingCriteria
import Amazonka.GuardDuty.Types.FindingStatistics
import Amazonka.GuardDuty.Types.FlowLogsConfigurationResult
import Amazonka.GuardDuty.Types.GeoLocation
import Amazonka.GuardDuty.Types.HighestSeverityThreatDetails
import Amazonka.GuardDuty.Types.HostPath
import Amazonka.GuardDuty.Types.IamInstanceProfile
import Amazonka.GuardDuty.Types.InstanceDetails
import Amazonka.GuardDuty.Types.Invitation
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
import Amazonka.GuardDuty.Types.ScanDetections
import Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindings
import Amazonka.GuardDuty.Types.ScanEc2InstanceWithFindingsResult
import Amazonka.GuardDuty.Types.ScanFilePath
import Amazonka.GuardDuty.Types.ScanResourceCriteria
import Amazonka.GuardDuty.Types.ScanResultDetails
import Amazonka.GuardDuty.Types.ScanThreatName
import Amazonka.GuardDuty.Types.ScannedItemCount
import Amazonka.GuardDuty.Types.SecurityContext
import Amazonka.GuardDuty.Types.SecurityGroup
import Amazonka.GuardDuty.Types.ServiceAdditionalInfo
import Amazonka.GuardDuty.Types.ServiceInfo
import Amazonka.GuardDuty.Types.SortCriteria
import Amazonka.GuardDuty.Types.Tag
import Amazonka.GuardDuty.Types.ThreatDetectedByName
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
import Amazonka.GuardDuty.Types.UsageStatistics
import Amazonka.GuardDuty.Types.Volume
import Amazonka.GuardDuty.Types.VolumeDetail
import Amazonka.GuardDuty.Types.VolumeMount
import Amazonka.GuardDuty.UnarchiveFindings
import Amazonka.GuardDuty.UntagResource
import Amazonka.GuardDuty.UpdateDetector
import Amazonka.GuardDuty.UpdateFilter
import Amazonka.GuardDuty.UpdateFindingsFeedback
import Amazonka.GuardDuty.UpdateIPSet
import Amazonka.GuardDuty.UpdateMalwareScanSettings
import Amazonka.GuardDuty.UpdateMemberDetectors
import Amazonka.GuardDuty.UpdateOrganizationConfiguration
import Amazonka.GuardDuty.UpdatePublishingDestination
import Amazonka.GuardDuty.UpdateThreatIntelSet
