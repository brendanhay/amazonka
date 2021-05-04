{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Lens
  ( -- * Operations

    -- ** CreateMembers
    createMembers_detectorId,
    createMembers_accountDetails,
    createMembersResponse_httpStatus,
    createMembersResponse_unprocessedAccounts,

    -- ** UpdateThreatIntelSet
    updateThreatIntelSet_activate,
    updateThreatIntelSet_name,
    updateThreatIntelSet_location,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,
    updateThreatIntelSetResponse_httpStatus,

    -- ** DeleteThreatIntelSet
    deleteThreatIntelSet_detectorId,
    deleteThreatIntelSet_threatIntelSetId,
    deleteThreatIntelSetResponse_httpStatus,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** UnarchiveFindings
    unarchiveFindings_detectorId,
    unarchiveFindings_findingIds,
    unarchiveFindingsResponse_httpStatus,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** DeletePublishingDestination
    deletePublishingDestination_detectorId,
    deletePublishingDestination_destinationId,
    deletePublishingDestinationResponse_httpStatus,

    -- ** UpdatePublishingDestination
    updatePublishingDestination_destinationProperties,
    updatePublishingDestination_detectorId,
    updatePublishingDestination_destinationId,
    updatePublishingDestinationResponse_httpStatus,

    -- ** ArchiveFindings
    archiveFindings_detectorId,
    archiveFindings_findingIds,
    archiveFindingsResponse_httpStatus,

    -- ** CreateFilter
    createFilter_rank,
    createFilter_tags,
    createFilter_action,
    createFilter_description,
    createFilter_clientToken,
    createFilter_detectorId,
    createFilter_name,
    createFilter_findingCriteria,
    createFilterResponse_httpStatus,
    createFilterResponse_name,

    -- ** GetDetector
    getDetector_detectorId,
    getDetectorResponse_dataSources,
    getDetectorResponse_findingPublishingFrequency,
    getDetectorResponse_updatedAt,
    getDetectorResponse_createdAt,
    getDetectorResponse_tags,
    getDetectorResponse_httpStatus,
    getDetectorResponse_serviceRole,
    getDetectorResponse_status,

    -- ** ListFindings
    listFindings_nextToken,
    listFindings_sortCriteria,
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_detectorId,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingIds,

    -- ** UpdateFilter
    updateFilter_rank,
    updateFilter_findingCriteria,
    updateFilter_action,
    updateFilter_description,
    updateFilter_detectorId,
    updateFilter_filterName,
    updateFilterResponse_httpStatus,
    updateFilterResponse_name,

    -- ** DeleteFilter
    deleteFilter_detectorId,
    deleteFilter_filterName,
    deleteFilterResponse_httpStatus,

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- ** AcceptInvitation
    acceptInvitation_detectorId,
    acceptInvitation_masterId,
    acceptInvitation_invitationId,
    acceptInvitationResponse_httpStatus,

    -- ** UpdateFindingsFeedback
    updateFindingsFeedback_comments,
    updateFindingsFeedback_detectorId,
    updateFindingsFeedback_findingIds,
    updateFindingsFeedback_feedback,
    updateFindingsFeedbackResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfiguration_detectorId,
    describeOrganizationConfigurationResponse_dataSources,
    describeOrganizationConfigurationResponse_httpStatus,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,

    -- ** GetMasterAccount
    getMasterAccount_detectorId,
    getMasterAccountResponse_httpStatus,
    getMasterAccountResponse_master,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListDetectors
    listDetectors_nextToken,
    listDetectors_maxResults,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
    listDetectorsResponse_detectorIds,

    -- ** DescribePublishingDestination
    describePublishingDestination_detectorId,
    describePublishingDestination_destinationId,
    describePublishingDestinationResponse_httpStatus,
    describePublishingDestinationResponse_destinationId,
    describePublishingDestinationResponse_destinationType,
    describePublishingDestinationResponse_status,
    describePublishingDestinationResponse_publishingFailureStartTimestamp,
    describePublishingDestinationResponse_destinationProperties,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_detectorId,
    getFindings_findingIds,
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetFindingsStatistics
    getFindingsStatistics_findingCriteria,
    getFindingsStatistics_detectorId,
    getFindingsStatistics_findingStatisticTypes,
    getFindingsStatisticsResponse_httpStatus,
    getFindingsStatisticsResponse_findingStatistics,

    -- ** GetMembers
    getMembers_detectorId,
    getMembers_accountIds,
    getMembersResponse_httpStatus,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,

    -- ** DeleteIPSet
    deleteIPSet_detectorId,
    deleteIPSet_ipSetId,
    deleteIPSetResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_activate,
    updateIPSet_name,
    updateIPSet_location,
    updateIPSet_detectorId,
    updateIPSet_ipSetId,
    updateIPSetResponse_httpStatus,

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

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_dataSources,
    updateOrganizationConfiguration_detectorId,
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_message,
    inviteMembers_disableEmailNotification,
    inviteMembers_detectorId,
    inviteMembers_accountIds,
    inviteMembersResponse_httpStatus,
    inviteMembersResponse_unprocessedAccounts,

    -- ** StopMonitoringMembers
    stopMonitoringMembers_detectorId,
    stopMonitoringMembers_accountIds,
    stopMonitoringMembersResponse_httpStatus,
    stopMonitoringMembersResponse_unprocessedAccounts,

    -- ** ListThreatIntelSets
    listThreatIntelSets_nextToken,
    listThreatIntelSets_maxResults,
    listThreatIntelSets_detectorId,
    listThreatIntelSetsResponse_nextToken,
    listThreatIntelSetsResponse_httpStatus,
    listThreatIntelSetsResponse_threatIntelSetIds,

    -- ** GetMemberDetectors
    getMemberDetectors_detectorId,
    getMemberDetectors_accountIds,
    getMemberDetectorsResponse_httpStatus,
    getMemberDetectorsResponse_memberDataSourceConfigurations,
    getMemberDetectorsResponse_unprocessedAccounts,

    -- ** StartMonitoringMembers
    startMonitoringMembers_detectorId,
    startMonitoringMembers_accountIds,
    startMonitoringMembersResponse_httpStatus,
    startMonitoringMembersResponse_unprocessedAccounts,

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

    -- ** CreateSampleFindings
    createSampleFindings_findingTypes,
    createSampleFindings_detectorId,
    createSampleFindingsResponse_httpStatus,

    -- ** DisassociateMembers
    disassociateMembers_detectorId,
    disassociateMembers_accountIds,
    disassociateMembersResponse_httpStatus,
    disassociateMembersResponse_unprocessedAccounts,

    -- ** CreatePublishingDestination
    createPublishingDestination_clientToken,
    createPublishingDestination_detectorId,
    createPublishingDestination_destinationType,
    createPublishingDestination_destinationProperties,
    createPublishingDestinationResponse_httpStatus,
    createPublishingDestinationResponse_destinationId,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_maxResults,
    listFilters_detectorId,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filterNames,

    -- ** DisassociateFromMasterAccount
    disassociateFromMasterAccount_detectorId,
    disassociateFromMasterAccountResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_maxResults,
    listMembers_onlyAssociated,
    listMembers_detectorId,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_httpStatus,
    declineInvitationsResponse_unprocessedAccounts,

    -- ** CreateDetector
    createDetector_dataSources,
    createDetector_findingPublishingFrequency,
    createDetector_tags,
    createDetector_clientToken,
    createDetector_enable,
    createDetectorResponse_detectorId,
    createDetectorResponse_httpStatus,

    -- ** GetUsageStatistics
    getUsageStatistics_nextToken,
    getUsageStatistics_unit,
    getUsageStatistics_maxResults,
    getUsageStatistics_detectorId,
    getUsageStatistics_usageStatisticType,
    getUsageStatistics_usageCriteria,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_usageStatistics,
    getUsageStatisticsResponse_httpStatus,

    -- ** GetFilter
    getFilter_detectorId,
    getFilter_filterName,
    getFilterResponse_rank,
    getFilterResponse_tags,
    getFilterResponse_description,
    getFilterResponse_httpStatus,
    getFilterResponse_name,
    getFilterResponse_action,
    getFilterResponse_findingCriteria,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_httpStatus,
    deleteInvitationsResponse_unprocessedAccounts,

    -- ** UpdateDetector
    updateDetector_enable,
    updateDetector_dataSources,
    updateDetector_findingPublishingFrequency,
    updateDetector_detectorId,
    updateDetectorResponse_httpStatus,

    -- ** DeleteDetector
    deleteDetector_detectorId,
    deleteDetectorResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_httpStatus,

    -- ** GetThreatIntelSet
    getThreatIntelSet_detectorId,
    getThreatIntelSet_threatIntelSetId,
    getThreatIntelSetResponse_tags,
    getThreatIntelSetResponse_httpStatus,
    getThreatIntelSetResponse_name,
    getThreatIntelSetResponse_format,
    getThreatIntelSetResponse_location,
    getThreatIntelSetResponse_status,

    -- ** UpdateMemberDetectors
    updateMemberDetectors_dataSources,
    updateMemberDetectors_detectorId,
    updateMemberDetectors_accountIds,
    updateMemberDetectorsResponse_httpStatus,
    updateMemberDetectorsResponse_unprocessedAccounts,

    -- ** GetIPSet
    getIPSet_detectorId,
    getIPSet_ipSetId,
    getIPSetResponse_tags,
    getIPSetResponse_httpStatus,
    getIPSetResponse_name,
    getIPSetResponse_format,
    getIPSetResponse_location,
    getIPSetResponse_status,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteMembers
    deleteMembers_detectorId,
    deleteMembers_accountIds,
    deleteMembersResponse_httpStatus,
    deleteMembersResponse_unprocessedAccounts,

    -- ** ListIPSets
    listIPSets_nextToken,
    listIPSets_maxResults,
    listIPSets_detectorId,
    listIPSetsResponse_nextToken,
    listIPSetsResponse_httpStatus,
    listIPSetsResponse_ipSetIds,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

    -- ** AccessKeyDetails
    accessKeyDetails_principalId,
    accessKeyDetails_userType,
    accessKeyDetails_accessKeyId,
    accessKeyDetails_userName,

    -- ** AccountDetail
    accountDetail_accountId,
    accountDetail_email,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** Action
    action_actionType,
    action_dnsRequestAction,
    action_networkConnectionAction,
    action_awsApiCallAction,
    action_portProbeAction,

    -- ** AdminAccount
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- ** AwsApiCallAction
    awsApiCallAction_api,
    awsApiCallAction_serviceName,
    awsApiCallAction_domainDetails,
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_callerType,
    awsApiCallAction_errorCode,

    -- ** BlockPublicAccess
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_blockPublicPolicy,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,
    bucketLevelPermissions_accessControlList,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- ** City
    city_cityName,

    -- ** CloudTrailConfigurationResult
    cloudTrailConfigurationResult_status,

    -- ** Condition
    condition_eq,
    condition_greaterThan,
    condition_gt,
    condition_greaterThanOrEqual,
    condition_lte,
    condition_neq,
    condition_notEquals,
    condition_lessThan,
    condition_equals,
    condition_gte,
    condition_lessThanOrEqual,
    condition_lt,

    -- ** Country
    country_countryName,
    country_countryCode,

    -- ** DNSLogsConfigurationResult
    dNSLogsConfigurationResult_status,

    -- ** DataSourceConfigurations
    dataSourceConfigurations_s3Logs,

    -- ** DataSourceConfigurationsResult
    dataSourceConfigurationsResult_cloudTrail,
    dataSourceConfigurationsResult_dNSLogs,
    dataSourceConfigurationsResult_flowLogs,
    dataSourceConfigurationsResult_s3Logs,

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
    dnsRequestAction_domain,

    -- ** DomainDetails
    domainDetails_domain,

    -- ** Evidence
    evidence_threatIntelligenceDetails,

    -- ** Finding
    finding_title,
    finding_service,
    finding_partition,
    finding_confidence,
    finding_description,
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

    -- ** IamInstanceProfile
    iamInstanceProfile_arn,
    iamInstanceProfile_id,

    -- ** InstanceDetails
    instanceDetails_platform,
    instanceDetails_instanceId,
    instanceDetails_instanceType,
    instanceDetails_outpostArn,
    instanceDetails_launchTime,
    instanceDetails_productCodes,
    instanceDetails_imageId,
    instanceDetails_iamInstanceProfile,
    instanceDetails_availabilityZone,
    instanceDetails_tags,
    instanceDetails_imageDescription,
    instanceDetails_instanceState,
    instanceDetails_networkInterfaces,

    -- ** Invitation
    invitation_accountId,
    invitation_relationshipStatus,
    invitation_invitationId,
    invitation_invitedAt,

    -- ** LocalIpDetails
    localIpDetails_ipAddressV4,

    -- ** LocalPortDetails
    localPortDetails_portName,
    localPortDetails_port,

    -- ** Master
    master_accountId,
    master_relationshipStatus,
    master_invitationId,
    master_invitedAt,

    -- ** Member
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
    networkConnectionAction_remotePortDetails,
    networkConnectionAction_localPortDetails,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_blocked,
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_protocol,

    -- ** NetworkInterface
    networkInterface_privateIpAddresses,
    networkInterface_ipv6Addresses,
    networkInterface_securityGroups,
    networkInterface_publicDnsName,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_privateDnsName,
    networkInterface_publicIp,
    networkInterface_vpcId,
    networkInterface_privateIpAddress,

    -- ** Organization
    organization_asn,
    organization_isp,
    organization_asnOrg,
    organization_org,

    -- ** OrganizationDataSourceConfigurations
    organizationDataSourceConfigurations_s3Logs,

    -- ** OrganizationDataSourceConfigurationsResult
    organizationDataSourceConfigurationsResult_s3Logs,

    -- ** OrganizationS3LogsConfiguration
    organizationS3LogsConfiguration_autoEnable,

    -- ** OrganizationS3LogsConfigurationResult
    organizationS3LogsConfigurationResult_autoEnable,

    -- ** Owner
    owner_id,

    -- ** PermissionConfiguration
    permissionConfiguration_accountLevelPermissions,
    permissionConfiguration_bucketLevelPermissions,

    -- ** PortProbeAction
    portProbeAction_portProbeDetails,
    portProbeAction_blocked,

    -- ** PortProbeDetail
    portProbeDetail_localPortDetails,
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localIpDetails,

    -- ** PrivateIpAddressDetails
    privateIpAddressDetails_privateDnsName,
    privateIpAddressDetails_privateIpAddress,

    -- ** ProductCode
    productCode_code,
    productCode_productType,

    -- ** PublicAccess
    publicAccess_permissionConfiguration,
    publicAccess_effectivePermission,

    -- ** RemoteIpDetails
    remoteIpDetails_geoLocation,
    remoteIpDetails_city,
    remoteIpDetails_organization,
    remoteIpDetails_country,
    remoteIpDetails_ipAddressV4,

    -- ** RemotePortDetails
    remotePortDetails_portName,
    remotePortDetails_port,

    -- ** Resource
    resource_s3BucketDetails,
    resource_instanceDetails,
    resource_resourceType,
    resource_accessKeyDetails,

    -- ** S3BucketDetail
    s3BucketDetail_arn,
    s3BucketDetail_publicAccess,
    s3BucketDetail_createdAt,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_name,
    s3BucketDetail_tags,
    s3BucketDetail_owner,
    s3BucketDetail_type,

    -- ** S3LogsConfiguration
    s3LogsConfiguration_enable,

    -- ** S3LogsConfigurationResult
    s3LogsConfigurationResult_status,

    -- ** SecurityGroup
    securityGroup_groupName,
    securityGroup_groupId,

    -- ** ServiceInfo
    serviceInfo_resourceRole,
    serviceInfo_archived,
    serviceInfo_eventFirstSeen,
    serviceInfo_eventLastSeen,
    serviceInfo_serviceName,
    serviceInfo_detectorId,
    serviceInfo_action,
    serviceInfo_evidence,
    serviceInfo_count,
    serviceInfo_userFeedback,

    -- ** SortCriteria
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** ThreatIntelligenceDetail
    threatIntelligenceDetail_threatNames,
    threatIntelligenceDetail_threatListName,

    -- ** Total
    total_amount,
    total_unit,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_result,

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
    usageResourceResult_total,
    usageResourceResult_resource,

    -- ** UsageStatistics
    usageStatistics_sumByDataSource,
    usageStatistics_topResources,
    usageStatistics_sumByAccount,
    usageStatistics_sumByResource,
  )
where

import Network.AWS.GuardDuty.AcceptInvitation
import Network.AWS.GuardDuty.ArchiveFindings
import Network.AWS.GuardDuty.CreateDetector
import Network.AWS.GuardDuty.CreateFilter
import Network.AWS.GuardDuty.CreateIPSet
import Network.AWS.GuardDuty.CreateMembers
import Network.AWS.GuardDuty.CreatePublishingDestination
import Network.AWS.GuardDuty.CreateSampleFindings
import Network.AWS.GuardDuty.CreateThreatIntelSet
import Network.AWS.GuardDuty.DeclineInvitations
import Network.AWS.GuardDuty.DeleteDetector
import Network.AWS.GuardDuty.DeleteFilter
import Network.AWS.GuardDuty.DeleteIPSet
import Network.AWS.GuardDuty.DeleteInvitations
import Network.AWS.GuardDuty.DeleteMembers
import Network.AWS.GuardDuty.DeletePublishingDestination
import Network.AWS.GuardDuty.DeleteThreatIntelSet
import Network.AWS.GuardDuty.DescribeOrganizationConfiguration
import Network.AWS.GuardDuty.DescribePublishingDestination
import Network.AWS.GuardDuty.DisableOrganizationAdminAccount
import Network.AWS.GuardDuty.DisassociateFromMasterAccount
import Network.AWS.GuardDuty.DisassociateMembers
import Network.AWS.GuardDuty.EnableOrganizationAdminAccount
import Network.AWS.GuardDuty.GetDetector
import Network.AWS.GuardDuty.GetFilter
import Network.AWS.GuardDuty.GetFindings
import Network.AWS.GuardDuty.GetFindingsStatistics
import Network.AWS.GuardDuty.GetIPSet
import Network.AWS.GuardDuty.GetInvitationsCount
import Network.AWS.GuardDuty.GetMasterAccount
import Network.AWS.GuardDuty.GetMemberDetectors
import Network.AWS.GuardDuty.GetMembers
import Network.AWS.GuardDuty.GetThreatIntelSet
import Network.AWS.GuardDuty.GetUsageStatistics
import Network.AWS.GuardDuty.InviteMembers
import Network.AWS.GuardDuty.ListDetectors
import Network.AWS.GuardDuty.ListFilters
import Network.AWS.GuardDuty.ListFindings
import Network.AWS.GuardDuty.ListIPSets
import Network.AWS.GuardDuty.ListInvitations
import Network.AWS.GuardDuty.ListMembers
import Network.AWS.GuardDuty.ListOrganizationAdminAccounts
import Network.AWS.GuardDuty.ListPublishingDestinations
import Network.AWS.GuardDuty.ListTagsForResource
import Network.AWS.GuardDuty.ListThreatIntelSets
import Network.AWS.GuardDuty.StartMonitoringMembers
import Network.AWS.GuardDuty.StopMonitoringMembers
import Network.AWS.GuardDuty.TagResource
import Network.AWS.GuardDuty.Types.AccessControlList
import Network.AWS.GuardDuty.Types.AccessKeyDetails
import Network.AWS.GuardDuty.Types.AccountDetail
import Network.AWS.GuardDuty.Types.AccountLevelPermissions
import Network.AWS.GuardDuty.Types.Action
import Network.AWS.GuardDuty.Types.AdminAccount
import Network.AWS.GuardDuty.Types.AwsApiCallAction
import Network.AWS.GuardDuty.Types.BlockPublicAccess
import Network.AWS.GuardDuty.Types.BucketLevelPermissions
import Network.AWS.GuardDuty.Types.BucketPolicy
import Network.AWS.GuardDuty.Types.City
import Network.AWS.GuardDuty.Types.CloudTrailConfigurationResult
import Network.AWS.GuardDuty.Types.Condition
import Network.AWS.GuardDuty.Types.Country
import Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
import Network.AWS.GuardDuty.Types.DataSourceConfigurations
import Network.AWS.GuardDuty.Types.DataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Destination
import Network.AWS.GuardDuty.Types.DestinationProperties
import Network.AWS.GuardDuty.Types.DnsRequestAction
import Network.AWS.GuardDuty.Types.DomainDetails
import Network.AWS.GuardDuty.Types.Evidence
import Network.AWS.GuardDuty.Types.Finding
import Network.AWS.GuardDuty.Types.FindingCriteria
import Network.AWS.GuardDuty.Types.FindingStatistics
import Network.AWS.GuardDuty.Types.FlowLogsConfigurationResult
import Network.AWS.GuardDuty.Types.GeoLocation
import Network.AWS.GuardDuty.Types.IamInstanceProfile
import Network.AWS.GuardDuty.Types.InstanceDetails
import Network.AWS.GuardDuty.Types.Invitation
import Network.AWS.GuardDuty.Types.LocalIpDetails
import Network.AWS.GuardDuty.Types.LocalPortDetails
import Network.AWS.GuardDuty.Types.Master
import Network.AWS.GuardDuty.Types.Member
import Network.AWS.GuardDuty.Types.MemberDataSourceConfiguration
import Network.AWS.GuardDuty.Types.NetworkConnectionAction
import Network.AWS.GuardDuty.Types.NetworkInterface
import Network.AWS.GuardDuty.Types.Organization
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurations
import Network.AWS.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfiguration
import Network.AWS.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.Owner
import Network.AWS.GuardDuty.Types.PermissionConfiguration
import Network.AWS.GuardDuty.Types.PortProbeAction
import Network.AWS.GuardDuty.Types.PortProbeDetail
import Network.AWS.GuardDuty.Types.PrivateIpAddressDetails
import Network.AWS.GuardDuty.Types.ProductCode
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.RemoteIpDetails
import Network.AWS.GuardDuty.Types.RemotePortDetails
import Network.AWS.GuardDuty.Types.Resource
import Network.AWS.GuardDuty.Types.S3BucketDetail
import Network.AWS.GuardDuty.Types.S3LogsConfiguration
import Network.AWS.GuardDuty.Types.S3LogsConfigurationResult
import Network.AWS.GuardDuty.Types.SecurityGroup
import Network.AWS.GuardDuty.Types.ServiceInfo
import Network.AWS.GuardDuty.Types.SortCriteria
import Network.AWS.GuardDuty.Types.Tag
import Network.AWS.GuardDuty.Types.ThreatIntelligenceDetail
import Network.AWS.GuardDuty.Types.Total
import Network.AWS.GuardDuty.Types.UnprocessedAccount
import Network.AWS.GuardDuty.Types.UsageAccountResult
import Network.AWS.GuardDuty.Types.UsageCriteria
import Network.AWS.GuardDuty.Types.UsageDataSourceResult
import Network.AWS.GuardDuty.Types.UsageResourceResult
import Network.AWS.GuardDuty.Types.UsageStatistics
import Network.AWS.GuardDuty.UnarchiveFindings
import Network.AWS.GuardDuty.UntagResource
import Network.AWS.GuardDuty.UpdateDetector
import Network.AWS.GuardDuty.UpdateFilter
import Network.AWS.GuardDuty.UpdateFindingsFeedback
import Network.AWS.GuardDuty.UpdateIPSet
import Network.AWS.GuardDuty.UpdateMemberDetectors
import Network.AWS.GuardDuty.UpdateOrganizationConfiguration
import Network.AWS.GuardDuty.UpdatePublishingDestination
import Network.AWS.GuardDuty.UpdateThreatIntelSet
