{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    createDetector_tags,
    createDetector_clientToken,
    createDetector_dataSources,
    createDetector_findingPublishingFrequency,
    createDetector_enable,
    createDetectorResponse_detectorId,
    createDetectorResponse_httpStatus,

    -- ** CreateFilter
    createFilter_tags,
    createFilter_clientToken,
    createFilter_rank,
    createFilter_description,
    createFilter_action,
    createFilter_detectorId,
    createFilter_name,
    createFilter_findingCriteria,
    createFilterResponse_httpStatus,
    createFilterResponse_name,

    -- ** CreateIPSet
    createIPSet_tags,
    createIPSet_clientToken,
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
    createThreatIntelSet_tags,
    createThreatIntelSet_clientToken,
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
    describeMalwareScans_sortCriteria,
    describeMalwareScans_nextToken,
    describeMalwareScans_filterCriteria,
    describeMalwareScans_maxResults,
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
    getDetectorResponse_tags,
    getDetectorResponse_dataSources,
    getDetectorResponse_findingPublishingFrequency,
    getDetectorResponse_createdAt,
    getDetectorResponse_updatedAt,
    getDetectorResponse_httpStatus,
    getDetectorResponse_serviceRole,
    getDetectorResponse_status,

    -- ** GetFilter
    getFilter_detectorId,
    getFilter_filterName,
    getFilterResponse_tags,
    getFilterResponse_rank,
    getFilterResponse_description,
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
    getRemainingFreeTrialDaysResponse_unprocessedAccounts,
    getRemainingFreeTrialDaysResponse_accounts,
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
    getUsageStatistics_nextToken,
    getUsageStatistics_maxResults,
    getUsageStatistics_unit,
    getUsageStatistics_detectorId,
    getUsageStatistics_usageStatisticType,
    getUsageStatistics_usageCriteria,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_usageStatistics,
    getUsageStatisticsResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_message,
    inviteMembers_disableEmailNotification,
    inviteMembers_detectorId,
    inviteMembers_accountIds,
    inviteMembersResponse_httpStatus,
    inviteMembersResponse_unprocessedAccounts,

    -- ** ListDetectors
    listDetectors_nextToken,
    listDetectors_maxResults,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
    listDetectorsResponse_detectorIds,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_maxResults,
    listFilters_detectorId,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filterNames,

    -- ** ListFindings
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_detectorId,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingIds,

    -- ** ListIPSets
    listIPSets_nextToken,
    listIPSets_maxResults,
    listIPSets_detectorId,
    listIPSetsResponse_nextToken,
    listIPSetsResponse_httpStatus,
    listIPSetsResponse_ipSetIds,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_maxResults,
    listMembers_detectorId,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListPublishingDestinations
    listPublishingDestinations_nextToken,
    listPublishingDestinations_maxResults,
    listPublishingDestinations_detectorId,
    listPublishingDestinationsResponse_nextToken,
    listPublishingDestinationsResponse_httpStatus,
    listPublishingDestinationsResponse_destinations,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThreatIntelSets
    listThreatIntelSets_nextToken,
    listThreatIntelSets_maxResults,
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
    updateFilter_findingCriteria,
    updateFilter_rank,
    updateFilter_description,
    updateFilter_action,
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
    updateIPSet_name,
    updateIPSet_location,
    updateIPSet_activate,
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
    updateThreatIntelSet_name,
    updateThreatIntelSet_location,
    updateThreatIntelSet_activate,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,
    updateThreatIntelSetResponse_httpStatus,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

    -- ** AccessKeyDetails
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_userType,
    accessKeyDetails_accessKeyId,

    -- ** AccountDetail
    accountDetail_accountId,
    accountDetail_email,

    -- ** AccountFreeTrialInfo
    accountFreeTrialInfo_dataSources,
    accountFreeTrialInfo_accountId,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** Action
    action_actionType,
    action_networkConnectionAction,
    action_awsApiCallAction,
    action_dnsRequestAction,
    action_kubernetesApiCallAction,
    action_portProbeAction,

    -- ** AdminAccount
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- ** Administrator
    administrator_accountId,
    administrator_invitedAt,
    administrator_relationshipStatus,
    administrator_invitationId,

    -- ** AwsApiCallAction
    awsApiCallAction_affectedResources,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_remoteAccountDetails,
    awsApiCallAction_domainDetails,
    awsApiCallAction_api,
    awsApiCallAction_errorCode,
    awsApiCallAction_serviceName,
    awsApiCallAction_userAgent,
    awsApiCallAction_callerType,

    -- ** BlockPublicAccess
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_blockPublicAcls,

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

    -- ** Container
    container_name,
    container_containerRuntime,
    container_id,
    container_imagePrefix,
    container_securityContext,
    container_image,
    container_volumeMounts,

    -- ** Country
    country_countryName,
    country_countryCode,

    -- ** DNSLogsConfigurationResult
    dNSLogsConfigurationResult_status,

    -- ** DataSourceConfigurations
    dataSourceConfigurations_malwareProtection,
    dataSourceConfigurations_s3Logs,
    dataSourceConfigurations_kubernetes,

    -- ** DataSourceConfigurationsResult
    dataSourceConfigurationsResult_malwareProtection,
    dataSourceConfigurationsResult_kubernetes,
    dataSourceConfigurationsResult_cloudTrail,
    dataSourceConfigurationsResult_dNSLogs,
    dataSourceConfigurationsResult_flowLogs,
    dataSourceConfigurationsResult_s3Logs,

    -- ** DataSourceFreeTrial
    dataSourceFreeTrial_freeTrialDaysRemaining,

    -- ** DataSourcesFreeTrial
    dataSourcesFreeTrial_malwareProtection,
    dataSourcesFreeTrial_cloudTrail,
    dataSourcesFreeTrial_dnsLogs,
    dataSourcesFreeTrial_s3Logs,
    dataSourcesFreeTrial_kubernetes,
    dataSourcesFreeTrial_flowLogs,

    -- ** DefaultServerSideEncryption
    defaultServerSideEncryption_encryptionType,
    defaultServerSideEncryption_kmsMasterKeyArn,

    -- ** Destination
    destination_destinationId,
    destination_destinationType,
    destination_status,

    -- ** DestinationProperties
    destinationProperties_kmsKeyArn,
    destinationProperties_destinationArn,

    -- ** DnsRequestAction
    dnsRequestAction_domain,
    dnsRequestAction_blocked,
    dnsRequestAction_protocol,

    -- ** DomainDetails
    domainDetails_domain,

    -- ** EbsVolumeDetails
    ebsVolumeDetails_skippedVolumeDetails,
    ebsVolumeDetails_scannedVolumeDetails,

    -- ** EbsVolumeScanDetails
    ebsVolumeScanDetails_sources,
    ebsVolumeScanDetails_triggerFindingId,
    ebsVolumeScanDetails_scanDetections,
    ebsVolumeScanDetails_scanId,
    ebsVolumeScanDetails_scanCompletedAt,
    ebsVolumeScanDetails_scanStartedAt,

    -- ** EbsVolumesResult
    ebsVolumesResult_status,

    -- ** EcsClusterDetails
    ecsClusterDetails_tags,
    ecsClusterDetails_name,
    ecsClusterDetails_taskDetails,
    ecsClusterDetails_arn,
    ecsClusterDetails_registeredContainerInstancesCount,
    ecsClusterDetails_status,
    ecsClusterDetails_runningTasksCount,
    ecsClusterDetails_activeServicesCount,

    -- ** EcsTaskDetails
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

    -- ** EksClusterDetails
    eksClusterDetails_tags,
    eksClusterDetails_name,
    eksClusterDetails_arn,
    eksClusterDetails_status,
    eksClusterDetails_vpcId,
    eksClusterDetails_createdAt,

    -- ** Evidence
    evidence_threatIntelligenceDetails,

    -- ** FilterCondition
    filterCondition_equalsValue,
    filterCondition_lessThan,
    filterCondition_greaterThan,

    -- ** FilterCriteria
    filterCriteria_filterCriterion,

    -- ** FilterCriterion
    filterCriterion_criterionKey,
    filterCriterion_filterCondition,

    -- ** Finding
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
    highestSeverityThreatDetails_severity,
    highestSeverityThreatDetails_count,
    highestSeverityThreatDetails_threatName,

    -- ** HostPath
    hostPath_path,

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** InstanceDetails
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

    -- ** Invitation
    invitation_accountId,
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,

    -- ** KubernetesApiCallAction
    kubernetesApiCallAction_remoteIpDetails,
    kubernetesApiCallAction_requestUri,
    kubernetesApiCallAction_statusCode,
    kubernetesApiCallAction_userAgent,
    kubernetesApiCallAction_verb,
    kubernetesApiCallAction_sourceIps,
    kubernetesApiCallAction_parameters,

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
    kubernetesUserDetails_username,
    kubernetesUserDetails_uid,
    kubernetesUserDetails_groups,

    -- ** KubernetesWorkloadDetails
    kubernetesWorkloadDetails_name,
    kubernetesWorkloadDetails_type,
    kubernetesWorkloadDetails_containers,
    kubernetesWorkloadDetails_uid,
    kubernetesWorkloadDetails_volumes,
    kubernetesWorkloadDetails_namespace,
    kubernetesWorkloadDetails_hostNetwork,

    -- ** LocalIpDetails
    localIpDetails_ipAddressV4,

    -- ** LocalPortDetails
    localPortDetails_port,
    localPortDetails_portName,

    -- ** MalwareProtectionConfiguration
    malwareProtectionConfiguration_scanEc2InstanceWithFindings,

    -- ** MalwareProtectionConfigurationResult
    malwareProtectionConfigurationResult_serviceRole,
    malwareProtectionConfigurationResult_scanEc2InstanceWithFindings,

    -- ** MalwareProtectionDataSourceFreeTrial
    malwareProtectionDataSourceFreeTrial_scanEc2InstanceWithFindings,

    -- ** Member
    member_administratorId,
    member_invitedAt,
    member_detectorId,
    member_accountId,
    member_masterId,
    member_email,
    member_relationshipStatus,
    member_updatedAt,

    -- ** MemberDataSourceConfiguration
    memberDataSourceConfiguration_accountId,
    memberDataSourceConfiguration_dataSources,

    -- ** NetworkConnectionAction
    networkConnectionAction_connectionDirection,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_protocol,
    networkConnectionAction_remotePortDetails,

    -- ** NetworkInterface
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

    -- ** Organization
    organization_isp,
    organization_org,
    organization_asn,
    organization_asnOrg,

    -- ** OrganizationDataSourceConfigurations
    organizationDataSourceConfigurations_malwareProtection,
    organizationDataSourceConfigurations_s3Logs,
    organizationDataSourceConfigurations_kubernetes,

    -- ** OrganizationDataSourceConfigurationsResult
    organizationDataSourceConfigurationsResult_malwareProtection,
    organizationDataSourceConfigurationsResult_kubernetes,
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
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localPortDetails,
    portProbeDetail_localIpDetails,

    -- ** PrivateIpAddressDetails
    privateIpAddressDetails_privateIpAddress,
    privateIpAddressDetails_privateDnsName,

    -- ** ProductCode
    productCode_code,
    productCode_productType,

    -- ** PublicAccess
    publicAccess_permissionConfiguration,
    publicAccess_effectivePermission,

    -- ** RemoteAccountDetails
    remoteAccountDetails_accountId,
    remoteAccountDetails_affiliated,

    -- ** RemoteIpDetails
    remoteIpDetails_country,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_city,
    remoteIpDetails_organization,
    remoteIpDetails_geoLocation,

    -- ** RemotePortDetails
    remotePortDetails_port,
    remotePortDetails_portName,

    -- ** Resource
    resource_resourceType,
    resource_instanceDetails,
    resource_s3BucketDetails,
    resource_ecsClusterDetails,
    resource_containerDetails,
    resource_accessKeyDetails,
    resource_eksClusterDetails,
    resource_kubernetesDetails,
    resource_ebsVolumeDetails,

    -- ** ResourceDetails
    resourceDetails_instanceArn,

    -- ** S3BucketDetail
    s3BucketDetail_tags,
    s3BucketDetail_name,
    s3BucketDetail_type,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_arn,
    s3BucketDetail_publicAccess,
    s3BucketDetail_owner,
    s3BucketDetail_createdAt,

    -- ** S3LogsConfiguration
    s3LogsConfiguration_enable,

    -- ** S3LogsConfigurationResult
    s3LogsConfigurationResult_status,

    -- ** Scan
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

    -- ** ScanCondition
    scanCondition_mapEquals,

    -- ** ScanConditionPair
    scanConditionPair_value,
    scanConditionPair_key,

    -- ** ScanDetections
    scanDetections_threatsDetectedItemCount,
    scanDetections_scannedItemCount,
    scanDetections_threatDetectedByName,
    scanDetections_highestSeverityThreatDetails,

    -- ** ScanEc2InstanceWithFindings
    scanEc2InstanceWithFindings_ebsVolumes,

    -- ** ScanEc2InstanceWithFindingsResult
    scanEc2InstanceWithFindingsResult_ebsVolumes,

    -- ** ScanFilePath
    scanFilePath_filePath,
    scanFilePath_hash,
    scanFilePath_fileName,
    scanFilePath_volumeArn,

    -- ** ScanResourceCriteria
    scanResourceCriteria_exclude,
    scanResourceCriteria_include,

    -- ** ScanResultDetails
    scanResultDetails_scanResult,

    -- ** ScanThreatName
    scanThreatName_severity,
    scanThreatName_name,
    scanThreatName_itemCount,
    scanThreatName_filePaths,

    -- ** ScannedItemCount
    scannedItemCount_files,
    scannedItemCount_totalGb,
    scannedItemCount_volumes,

    -- ** SecurityContext
    securityContext_privileged,

    -- ** SecurityGroup
    securityGroup_groupName,
    securityGroup_groupId,

    -- ** ServiceAdditionalInfo
    serviceAdditionalInfo_type,
    serviceAdditionalInfo_value,

    -- ** ServiceInfo
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

    -- ** SortCriteria
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThreatDetectedByName
    threatDetectedByName_itemCount,
    threatDetectedByName_uniqueThreatNameCount,
    threatDetectedByName_threatNames,
    threatDetectedByName_shortened,

    -- ** ThreatIntelligenceDetail
    threatIntelligenceDetail_threatListName,
    threatIntelligenceDetail_threatNames,

    -- ** ThreatsDetectedItemCount
    threatsDetectedItemCount_files,

    -- ** Total
    total_unit,
    total_amount,

    -- ** TriggerDetails
    triggerDetails_description,
    triggerDetails_guardDutyFindingId,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_result,

    -- ** UsageAccountResult
    usageAccountResult_total,
    usageAccountResult_accountId,

    -- ** UsageCriteria
    usageCriteria_accountIds,
    usageCriteria_resources,
    usageCriteria_dataSources,

    -- ** UsageDataSourceResult
    usageDataSourceResult_total,
    usageDataSourceResult_dataSource,

    -- ** UsageResourceResult
    usageResourceResult_total,
    usageResourceResult_resource,

    -- ** UsageStatistics
    usageStatistics_sumByDataSource,
    usageStatistics_topResources,
    usageStatistics_sumByResource,
    usageStatistics_sumByAccount,

    -- ** Volume
    volume_name,
    volume_hostPath,

    -- ** VolumeDetail
    volumeDetail_snapshotArn,
    volumeDetail_deviceName,
    volumeDetail_volumeArn,
    volumeDetail_volumeType,
    volumeDetail_kmsKeyArn,
    volumeDetail_encryptionType,
    volumeDetail_volumeSizeInGB,

    -- ** VolumeMount
    volumeMount_name,
    volumeMount_mountPath,
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
