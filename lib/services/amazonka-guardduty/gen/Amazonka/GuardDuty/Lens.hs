{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Lens
  ( -- * Operations

    -- ** CreateFilter
    createFilter_clientToken,
    createFilter_action,
    createFilter_description,
    createFilter_rank,
    createFilter_tags,
    createFilter_detectorId,
    createFilter_name,
    createFilter_findingCriteria,
    createFilterResponse_httpStatus,
    createFilterResponse_name,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** ListFindings
    listFindings_findingCriteria,
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_maxResults,
    listFindings_detectorId,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,
    listFindingsResponse_findingIds,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

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

    -- ** DeleteThreatIntelSet
    deleteThreatIntelSet_detectorId,
    deleteThreatIntelSet_threatIntelSetId,
    deleteThreatIntelSetResponse_httpStatus,

    -- ** UpdateThreatIntelSet
    updateThreatIntelSet_location,
    updateThreatIntelSet_activate,
    updateThreatIntelSet_name,
    updateThreatIntelSet_detectorId,
    updateThreatIntelSet_threatIntelSetId,
    updateThreatIntelSetResponse_httpStatus,

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

    -- ** DeleteMembers
    deleteMembers_detectorId,
    deleteMembers_accountIds,
    deleteMembersResponse_httpStatus,
    deleteMembersResponse_unprocessedAccounts,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

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

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** UpdateMemberDetectors
    updateMemberDetectors_dataSources,
    updateMemberDetectors_detectorId,
    updateMemberDetectors_accountIds,
    updateMemberDetectorsResponse_httpStatus,
    updateMemberDetectorsResponse_unprocessedAccounts,

    -- ** GetThreatIntelSet
    getThreatIntelSet_detectorId,
    getThreatIntelSet_threatIntelSetId,
    getThreatIntelSetResponse_tags,
    getThreatIntelSetResponse_httpStatus,
    getThreatIntelSetResponse_name,
    getThreatIntelSetResponse_format,
    getThreatIntelSetResponse_location,
    getThreatIntelSetResponse_status,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_httpStatus,
    deleteInvitationsResponse_unprocessedAccounts,

    -- ** GetMasterAccount
    getMasterAccount_detectorId,
    getMasterAccountResponse_httpStatus,
    getMasterAccountResponse_master,

    -- ** GetUsageStatistics
    getUsageStatistics_nextToken,
    getUsageStatistics_unit,
    getUsageStatistics_maxResults,
    getUsageStatistics_detectorId,
    getUsageStatistics_usageStatisticType,
    getUsageStatistics_usageCriteria,
    getUsageStatisticsResponse_usageStatistics,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_httpStatus,

    -- ** CreateDetector
    createDetector_clientToken,
    createDetector_findingPublishingFrequency,
    createDetector_dataSources,
    createDetector_tags,
    createDetector_enable,
    createDetectorResponse_detectorId,
    createDetectorResponse_httpStatus,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_httpStatus,
    declineInvitationsResponse_unprocessedAccounts,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfiguration_detectorId,
    describeOrganizationConfigurationResponse_dataSources,
    describeOrganizationConfigurationResponse_httpStatus,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_memberAccountLimitReached,

    -- ** CreatePublishingDestination
    createPublishingDestination_clientToken,
    createPublishingDestination_detectorId,
    createPublishingDestination_destinationType,
    createPublishingDestination_destinationProperties,
    createPublishingDestinationResponse_httpStatus,
    createPublishingDestinationResponse_destinationId,

    -- ** UpdateFilter
    updateFilter_findingCriteria,
    updateFilter_action,
    updateFilter_description,
    updateFilter_rank,
    updateFilter_detectorId,
    updateFilter_filterName,
    updateFilterResponse_httpStatus,
    updateFilterResponse_name,

    -- ** DeleteFilter
    deleteFilter_detectorId,
    deleteFilter_filterName,
    deleteFilterResponse_httpStatus,

    -- ** DisassociateMembers
    disassociateMembers_detectorId,
    disassociateMembers_accountIds,
    disassociateMembersResponse_httpStatus,
    disassociateMembersResponse_unprocessedAccounts,

    -- ** DisassociateFromMasterAccount
    disassociateFromMasterAccount_detectorId,
    disassociateFromMasterAccountResponse_httpStatus,

    -- ** AcceptInvitation
    acceptInvitation_detectorId,
    acceptInvitation_masterId,
    acceptInvitation_invitationId,
    acceptInvitationResponse_httpStatus,

    -- ** ListFilters
    listFilters_nextToken,
    listFilters_maxResults,
    listFilters_detectorId,
    listFiltersResponse_nextToken,
    listFiltersResponse_httpStatus,
    listFiltersResponse_filterNames,

    -- ** ListMembers
    listMembers_onlyAssociated,
    listMembers_nextToken,
    listMembers_maxResults,
    listMembers_detectorId,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListPublishingDestinations
    listPublishingDestinations_nextToken,
    listPublishingDestinations_maxResults,
    listPublishingDestinations_detectorId,
    listPublishingDestinationsResponse_nextToken,
    listPublishingDestinationsResponse_httpStatus,
    listPublishingDestinationsResponse_destinations,

    -- ** DeletePublishingDestination
    deletePublishingDestination_detectorId,
    deletePublishingDestination_destinationId,
    deletePublishingDestinationResponse_httpStatus,

    -- ** UpdatePublishingDestination
    updatePublishingDestination_destinationProperties,
    updatePublishingDestination_detectorId,
    updatePublishingDestination_destinationId,
    updatePublishingDestinationResponse_httpStatus,

    -- ** GetDetector
    getDetector_detectorId,
    getDetectorResponse_createdAt,
    getDetectorResponse_findingPublishingFrequency,
    getDetectorResponse_dataSources,
    getDetectorResponse_updatedAt,
    getDetectorResponse_tags,
    getDetectorResponse_httpStatus,
    getDetectorResponse_serviceRole,
    getDetectorResponse_status,

    -- ** CreateSampleFindings
    createSampleFindings_findingTypes,
    createSampleFindings_detectorId,
    createSampleFindingsResponse_httpStatus,

    -- ** ArchiveFindings
    archiveFindings_detectorId,
    archiveFindings_findingIds,
    archiveFindingsResponse_httpStatus,

    -- ** CreateMembers
    createMembers_detectorId,
    createMembers_accountDetails,
    createMembersResponse_httpStatus,
    createMembersResponse_unprocessedAccounts,

    -- ** UnarchiveFindings
    unarchiveFindings_detectorId,
    unarchiveFindings_findingIds,
    unarchiveFindingsResponse_httpStatus,

    -- ** GetMemberDetectors
    getMemberDetectors_detectorId,
    getMemberDetectors_accountIds,
    getMemberDetectorsResponse_httpStatus,
    getMemberDetectorsResponse_memberDataSourceConfigurations,
    getMemberDetectorsResponse_unprocessedAccounts,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** StartMonitoringMembers
    startMonitoringMembers_detectorId,
    startMonitoringMembers_accountIds,
    startMonitoringMembersResponse_httpStatus,
    startMonitoringMembersResponse_unprocessedAccounts,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_dataSources,
    updateOrganizationConfiguration_detectorId,
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** InviteMembers
    inviteMembers_disableEmailNotification,
    inviteMembers_message,
    inviteMembers_detectorId,
    inviteMembers_accountIds,
    inviteMembersResponse_httpStatus,
    inviteMembersResponse_unprocessedAccounts,

    -- ** DeleteIPSet
    deleteIPSet_detectorId,
    deleteIPSet_ipSetId,
    deleteIPSetResponse_httpStatus,

    -- ** UpdateIPSet
    updateIPSet_location,
    updateIPSet_activate,
    updateIPSet_name,
    updateIPSet_detectorId,
    updateIPSet_ipSetId,
    updateIPSetResponse_httpStatus,

    -- ** ListIPSets
    listIPSets_nextToken,
    listIPSets_maxResults,
    listIPSets_detectorId,
    listIPSetsResponse_nextToken,
    listIPSetsResponse_httpStatus,
    listIPSetsResponse_ipSetIds,

    -- ** GetMembers
    getMembers_detectorId,
    getMembers_accountIds,
    getMembersResponse_httpStatus,
    getMembersResponse_members,
    getMembersResponse_unprocessedAccounts,

    -- ** DescribePublishingDestination
    describePublishingDestination_detectorId,
    describePublishingDestination_destinationId,
    describePublishingDestinationResponse_httpStatus,
    describePublishingDestinationResponse_destinationId,
    describePublishingDestinationResponse_destinationType,
    describePublishingDestinationResponse_status,
    describePublishingDestinationResponse_publishingFailureStartTimestamp,
    describePublishingDestinationResponse_destinationProperties,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_detectorId,
    getFindings_findingIds,
    getFindingsResponse_httpStatus,
    getFindingsResponse_findings,

    -- ** ListDetectors
    listDetectors_nextToken,
    listDetectors_maxResults,
    listDetectorsResponse_nextToken,
    listDetectorsResponse_httpStatus,
    listDetectorsResponse_detectorIds,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDetector
    updateDetector_findingPublishingFrequency,
    updateDetector_dataSources,
    updateDetector_enable,
    updateDetector_detectorId,
    updateDetectorResponse_httpStatus,

    -- ** DeleteDetector
    deleteDetector_detectorId,
    deleteDetectorResponse_httpStatus,

    -- ** UpdateFindingsFeedback
    updateFindingsFeedback_comments,
    updateFindingsFeedback_detectorId,
    updateFindingsFeedback_findingIds,
    updateFindingsFeedback_feedback,
    updateFindingsFeedbackResponse_httpStatus,

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

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicWriteAccess,
    accessControlList_allowsPublicReadAccess,

    -- ** AccessKeyDetails
    accessKeyDetails_principalId,
    accessKeyDetails_userName,
    accessKeyDetails_accessKeyId,
    accessKeyDetails_userType,

    -- ** AccountDetail
    accountDetail_accountId,
    accountDetail_email,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** Action
    action_networkConnectionAction,
    action_portProbeAction,
    action_actionType,
    action_dnsRequestAction,
    action_awsApiCallAction,

    -- ** AdminAccount
    adminAccount_adminAccountId,
    adminAccount_adminStatus,

    -- ** AwsApiCallAction
    awsApiCallAction_remoteIpDetails,
    awsApiCallAction_callerType,
    awsApiCallAction_domainDetails,
    awsApiCallAction_serviceName,
    awsApiCallAction_errorCode,
    awsApiCallAction_api,

    -- ** BlockPublicAccess
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_blockPublicPolicy,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicWriteAccess,
    bucketPolicy_allowsPublicReadAccess,

    -- ** City
    city_cityName,

    -- ** CloudTrailConfigurationResult
    cloudTrailConfigurationResult_status,

    -- ** Condition
    condition_eq,
    condition_lessThan,
    condition_lte,
    condition_greaterThanOrEqual,
    condition_lessThanOrEqual,
    condition_gt,
    condition_equals,
    condition_neq,
    condition_notEquals,
    condition_lt,
    condition_gte,
    condition_greaterThan,

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
    destinationProperties_kmsKeyArn,
    destinationProperties_destinationArn,

    -- ** DnsRequestAction
    dnsRequestAction_domain,

    -- ** DomainDetails
    domainDetails_domain,

    -- ** Evidence
    evidence_threatIntelligenceDetails,

    -- ** Finding
    finding_service,
    finding_confidence,
    finding_partition,
    finding_title,
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
    instanceDetails_instanceId,
    instanceDetails_platform,
    instanceDetails_launchTime,
    instanceDetails_networkInterfaces,
    instanceDetails_outpostArn,
    instanceDetails_instanceType,
    instanceDetails_availabilityZone,
    instanceDetails_iamInstanceProfile,
    instanceDetails_imageId,
    instanceDetails_productCodes,
    instanceDetails_instanceState,
    instanceDetails_tags,
    instanceDetails_imageDescription,

    -- ** Invitation
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,
    invitation_accountId,

    -- ** LocalIpDetails
    localIpDetails_ipAddressV4,

    -- ** LocalPortDetails
    localPortDetails_portName,
    localPortDetails_port,

    -- ** Master
    master_invitedAt,
    master_relationshipStatus,
    master_invitationId,
    master_accountId,

    -- ** Member
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
    networkConnectionAction_remoteIpDetails,
    networkConnectionAction_protocol,
    networkConnectionAction_localIpDetails,
    networkConnectionAction_remotePortDetails,
    networkConnectionAction_blocked,
    networkConnectionAction_connectionDirection,
    networkConnectionAction_localPortDetails,

    -- ** NetworkInterface
    networkInterface_privateIpAddresses,
    networkInterface_publicDnsName,
    networkInterface_securityGroups,
    networkInterface_vpcId,
    networkInterface_networkInterfaceId,
    networkInterface_subnetId,
    networkInterface_privateIpAddress,
    networkInterface_publicIp,
    networkInterface_privateDnsName,
    networkInterface_ipv6Addresses,

    -- ** Organization
    organization_org,
    organization_asnOrg,
    organization_asn,
    organization_isp,

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
    permissionConfiguration_bucketLevelPermissions,
    permissionConfiguration_accountLevelPermissions,

    -- ** PortProbeAction
    portProbeAction_portProbeDetails,
    portProbeAction_blocked,

    -- ** PortProbeDetail
    portProbeDetail_remoteIpDetails,
    portProbeDetail_localIpDetails,
    portProbeDetail_localPortDetails,

    -- ** PrivateIpAddressDetails
    privateIpAddressDetails_privateIpAddress,
    privateIpAddressDetails_privateDnsName,

    -- ** ProductCode
    productCode_productType,
    productCode_code,

    -- ** PublicAccess
    publicAccess_permissionConfiguration,
    publicAccess_effectivePermission,

    -- ** RemoteIpDetails
    remoteIpDetails_country,
    remoteIpDetails_city,
    remoteIpDetails_ipAddressV4,
    remoteIpDetails_geoLocation,
    remoteIpDetails_organization,

    -- ** RemotePortDetails
    remotePortDetails_portName,
    remotePortDetails_port,

    -- ** Resource
    resource_resourceType,
    resource_s3BucketDetails,
    resource_instanceDetails,
    resource_accessKeyDetails,

    -- ** S3BucketDetail
    s3BucketDetail_arn,
    s3BucketDetail_createdAt,
    s3BucketDetail_owner,
    s3BucketDetail_name,
    s3BucketDetail_defaultServerSideEncryption,
    s3BucketDetail_publicAccess,
    s3BucketDetail_type,
    s3BucketDetail_tags,

    -- ** S3LogsConfiguration
    s3LogsConfiguration_enable,

    -- ** S3LogsConfigurationResult
    s3LogsConfigurationResult_status,

    -- ** SecurityGroup
    securityGroup_groupId,
    securityGroup_groupName,

    -- ** ServiceInfo
    serviceInfo_count,
    serviceInfo_eventFirstSeen,
    serviceInfo_action,
    serviceInfo_detectorId,
    serviceInfo_serviceName,
    serviceInfo_userFeedback,
    serviceInfo_evidence,
    serviceInfo_eventLastSeen,
    serviceInfo_resourceRole,
    serviceInfo_archived,

    -- ** SortCriteria
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- ** Tag
    tag_value,
    tag_key,

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
    usageDataSourceResult_total,
    usageDataSourceResult_dataSource,

    -- ** UsageResourceResult
    usageResourceResult_total,
    usageResourceResult_resource,

    -- ** UsageStatistics
    usageStatistics_topResources,
    usageStatistics_sumByResource,
    usageStatistics_sumByDataSource,
    usageStatistics_sumByAccount,
  )
where

import Amazonka.GuardDuty.AcceptInvitation
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
import Amazonka.GuardDuty.DescribeOrganizationConfiguration
import Amazonka.GuardDuty.DescribePublishingDestination
import Amazonka.GuardDuty.DisableOrganizationAdminAccount
import Amazonka.GuardDuty.DisassociateFromMasterAccount
import Amazonka.GuardDuty.DisassociateMembers
import Amazonka.GuardDuty.EnableOrganizationAdminAccount
import Amazonka.GuardDuty.GetDetector
import Amazonka.GuardDuty.GetFilter
import Amazonka.GuardDuty.GetFindings
import Amazonka.GuardDuty.GetFindingsStatistics
import Amazonka.GuardDuty.GetIPSet
import Amazonka.GuardDuty.GetInvitationsCount
import Amazonka.GuardDuty.GetMasterAccount
import Amazonka.GuardDuty.GetMemberDetectors
import Amazonka.GuardDuty.GetMembers
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
import Amazonka.GuardDuty.Types.AccountLevelPermissions
import Amazonka.GuardDuty.Types.Action
import Amazonka.GuardDuty.Types.AdminAccount
import Amazonka.GuardDuty.Types.AwsApiCallAction
import Amazonka.GuardDuty.Types.BlockPublicAccess
import Amazonka.GuardDuty.Types.BucketLevelPermissions
import Amazonka.GuardDuty.Types.BucketPolicy
import Amazonka.GuardDuty.Types.City
import Amazonka.GuardDuty.Types.CloudTrailConfigurationResult
import Amazonka.GuardDuty.Types.Condition
import Amazonka.GuardDuty.Types.Country
import Amazonka.GuardDuty.Types.DNSLogsConfigurationResult
import Amazonka.GuardDuty.Types.DataSourceConfigurations
import Amazonka.GuardDuty.Types.DataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.DefaultServerSideEncryption
import Amazonka.GuardDuty.Types.Destination
import Amazonka.GuardDuty.Types.DestinationProperties
import Amazonka.GuardDuty.Types.DnsRequestAction
import Amazonka.GuardDuty.Types.DomainDetails
import Amazonka.GuardDuty.Types.Evidence
import Amazonka.GuardDuty.Types.Finding
import Amazonka.GuardDuty.Types.FindingCriteria
import Amazonka.GuardDuty.Types.FindingStatistics
import Amazonka.GuardDuty.Types.FlowLogsConfigurationResult
import Amazonka.GuardDuty.Types.GeoLocation
import Amazonka.GuardDuty.Types.IamInstanceProfile
import Amazonka.GuardDuty.Types.InstanceDetails
import Amazonka.GuardDuty.Types.Invitation
import Amazonka.GuardDuty.Types.LocalIpDetails
import Amazonka.GuardDuty.Types.LocalPortDetails
import Amazonka.GuardDuty.Types.Master
import Amazonka.GuardDuty.Types.Member
import Amazonka.GuardDuty.Types.MemberDataSourceConfiguration
import Amazonka.GuardDuty.Types.NetworkConnectionAction
import Amazonka.GuardDuty.Types.NetworkInterface
import Amazonka.GuardDuty.Types.Organization
import Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurations
import Amazonka.GuardDuty.Types.OrganizationDataSourceConfigurationsResult
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfiguration
import Amazonka.GuardDuty.Types.OrganizationS3LogsConfigurationResult
import Amazonka.GuardDuty.Types.Owner
import Amazonka.GuardDuty.Types.PermissionConfiguration
import Amazonka.GuardDuty.Types.PortProbeAction
import Amazonka.GuardDuty.Types.PortProbeDetail
import Amazonka.GuardDuty.Types.PrivateIpAddressDetails
import Amazonka.GuardDuty.Types.ProductCode
import Amazonka.GuardDuty.Types.PublicAccess
import Amazonka.GuardDuty.Types.RemoteIpDetails
import Amazonka.GuardDuty.Types.RemotePortDetails
import Amazonka.GuardDuty.Types.Resource
import Amazonka.GuardDuty.Types.S3BucketDetail
import Amazonka.GuardDuty.Types.S3LogsConfiguration
import Amazonka.GuardDuty.Types.S3LogsConfigurationResult
import Amazonka.GuardDuty.Types.SecurityGroup
import Amazonka.GuardDuty.Types.ServiceInfo
import Amazonka.GuardDuty.Types.SortCriteria
import Amazonka.GuardDuty.Types.Tag
import Amazonka.GuardDuty.Types.ThreatIntelligenceDetail
import Amazonka.GuardDuty.Types.Total
import Amazonka.GuardDuty.Types.UnprocessedAccount
import Amazonka.GuardDuty.Types.UsageAccountResult
import Amazonka.GuardDuty.Types.UsageCriteria
import Amazonka.GuardDuty.Types.UsageDataSourceResult
import Amazonka.GuardDuty.Types.UsageResourceResult
import Amazonka.GuardDuty.Types.UsageStatistics
import Amazonka.GuardDuty.UnarchiveFindings
import Amazonka.GuardDuty.UntagResource
import Amazonka.GuardDuty.UpdateDetector
import Amazonka.GuardDuty.UpdateFilter
import Amazonka.GuardDuty.UpdateFindingsFeedback
import Amazonka.GuardDuty.UpdateIPSet
import Amazonka.GuardDuty.UpdateMemberDetectors
import Amazonka.GuardDuty.UpdateOrganizationConfiguration
import Amazonka.GuardDuty.UpdatePublishingDestination
import Amazonka.GuardDuty.UpdateThreatIntelSet
