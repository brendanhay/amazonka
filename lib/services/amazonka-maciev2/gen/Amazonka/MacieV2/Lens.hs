{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Lens
  ( -- * Operations

    -- ** AcceptInvitation
    acceptInvitation_masterAccount,
    acceptInvitation_administratorAccountId,
    acceptInvitation_invitationId,
    acceptInvitationResponse_httpStatus,

    -- ** BatchGetCustomDataIdentifiers
    batchGetCustomDataIdentifiers_ids,
    batchGetCustomDataIdentifiersResponse_notFoundIdentifierIds,
    batchGetCustomDataIdentifiersResponse_customDataIdentifiers,
    batchGetCustomDataIdentifiersResponse_httpStatus,

    -- ** CreateAllowList
    createAllowList_tags,
    createAllowList_description,
    createAllowList_criteria,
    createAllowList_clientToken,
    createAllowList_name,
    createAllowListResponse_arn,
    createAllowListResponse_id,
    createAllowListResponse_httpStatus,

    -- ** CreateClassificationJob
    createClassificationJob_tags,
    createClassificationJob_customDataIdentifierIds,
    createClassificationJob_managedDataIdentifierSelector,
    createClassificationJob_scheduleFrequency,
    createClassificationJob_description,
    createClassificationJob_initialRun,
    createClassificationJob_allowListIds,
    createClassificationJob_samplingPercentage,
    createClassificationJob_managedDataIdentifierIds,
    createClassificationJob_s3JobDefinition,
    createClassificationJob_jobType,
    createClassificationJob_clientToken,
    createClassificationJob_name,
    createClassificationJobResponse_jobId,
    createClassificationJobResponse_jobArn,
    createClassificationJobResponse_httpStatus,

    -- ** CreateCustomDataIdentifier
    createCustomDataIdentifier_tags,
    createCustomDataIdentifier_clientToken,
    createCustomDataIdentifier_ignoreWords,
    createCustomDataIdentifier_keywords,
    createCustomDataIdentifier_description,
    createCustomDataIdentifier_severityLevels,
    createCustomDataIdentifier_maximumMatchDistance,
    createCustomDataIdentifier_name,
    createCustomDataIdentifier_regex,
    createCustomDataIdentifierResponse_customDataIdentifierId,
    createCustomDataIdentifierResponse_httpStatus,

    -- ** CreateFindingsFilter
    createFindingsFilter_tags,
    createFindingsFilter_clientToken,
    createFindingsFilter_description,
    createFindingsFilter_position,
    createFindingsFilter_action,
    createFindingsFilter_findingCriteria,
    createFindingsFilter_name,
    createFindingsFilterResponse_arn,
    createFindingsFilterResponse_id,
    createFindingsFilterResponse_httpStatus,

    -- ** CreateInvitations
    createInvitations_message,
    createInvitations_disableEmailNotification,
    createInvitations_accountIds,
    createInvitationsResponse_unprocessedAccounts,
    createInvitationsResponse_httpStatus,

    -- ** CreateMember
    createMember_tags,
    createMember_account,
    createMemberResponse_arn,
    createMemberResponse_httpStatus,

    -- ** CreateSampleFindings
    createSampleFindings_findingTypes,
    createSampleFindingsResponse_httpStatus,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_unprocessedAccounts,
    declineInvitationsResponse_httpStatus,

    -- ** DeleteAllowList
    deleteAllowList_ignoreJobChecks,
    deleteAllowList_id,
    deleteAllowListResponse_httpStatus,

    -- ** DeleteCustomDataIdentifier
    deleteCustomDataIdentifier_id,
    deleteCustomDataIdentifierResponse_httpStatus,

    -- ** DeleteFindingsFilter
    deleteFindingsFilter_id,
    deleteFindingsFilterResponse_httpStatus,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_unprocessedAccounts,
    deleteInvitationsResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_id,
    deleteMemberResponse_httpStatus,

    -- ** DescribeBuckets
    describeBuckets_sortCriteria,
    describeBuckets_nextToken,
    describeBuckets_criteria,
    describeBuckets_maxResults,
    describeBucketsResponse_nextToken,
    describeBucketsResponse_buckets,
    describeBucketsResponse_httpStatus,

    -- ** DescribeClassificationJob
    describeClassificationJob_jobId,
    describeClassificationJobResponse_tags,
    describeClassificationJobResponse_name,
    describeClassificationJobResponse_clientToken,
    describeClassificationJobResponse_customDataIdentifierIds,
    describeClassificationJobResponse_jobStatus,
    describeClassificationJobResponse_userPausedDetails,
    describeClassificationJobResponse_statistics,
    describeClassificationJobResponse_managedDataIdentifierSelector,
    describeClassificationJobResponse_scheduleFrequency,
    describeClassificationJobResponse_jobId,
    describeClassificationJobResponse_description,
    describeClassificationJobResponse_s3JobDefinition,
    describeClassificationJobResponse_initialRun,
    describeClassificationJobResponse_allowListIds,
    describeClassificationJobResponse_lastRunTime,
    describeClassificationJobResponse_lastRunErrorStatus,
    describeClassificationJobResponse_jobArn,
    describeClassificationJobResponse_samplingPercentage,
    describeClassificationJobResponse_createdAt,
    describeClassificationJobResponse_jobType,
    describeClassificationJobResponse_managedDataIdentifierIds,
    describeClassificationJobResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_maxAccountLimitReached,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** DisableMacie
    disableMacieResponse_httpStatus,

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- ** DisassociateFromAdministratorAccount
    disassociateFromAdministratorAccountResponse_httpStatus,

    -- ** DisassociateFromMasterAccount
    disassociateFromMasterAccountResponse_httpStatus,

    -- ** DisassociateMember
    disassociateMember_id,
    disassociateMemberResponse_httpStatus,

    -- ** EnableMacie
    enableMacie_clientToken,
    enableMacie_status,
    enableMacie_findingPublishingFrequency,
    enableMacieResponse_httpStatus,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_clientToken,
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** GetAdministratorAccount
    getAdministratorAccountResponse_administrator,
    getAdministratorAccountResponse_httpStatus,

    -- ** GetAllowList
    getAllowList_id,
    getAllowListResponse_tags,
    getAllowListResponse_name,
    getAllowListResponse_criteria,
    getAllowListResponse_arn,
    getAllowListResponse_status,
    getAllowListResponse_description,
    getAllowListResponse_id,
    getAllowListResponse_createdAt,
    getAllowListResponse_updatedAt,
    getAllowListResponse_httpStatus,

    -- ** GetBucketStatistics
    getBucketStatistics_accountId,
    getBucketStatisticsResponse_classifiableSizeInBytes,
    getBucketStatisticsResponse_bucketCountByEffectivePermission,
    getBucketStatisticsResponse_unclassifiableObjectCount,
    getBucketStatisticsResponse_bucketCount,
    getBucketStatisticsResponse_objectCount,
    getBucketStatisticsResponse_lastUpdated,
    getBucketStatisticsResponse_classifiableObjectCount,
    getBucketStatisticsResponse_sizeInBytes,
    getBucketStatisticsResponse_sizeInBytesCompressed,
    getBucketStatisticsResponse_bucketCountBySharedAccessType,
    getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement,
    getBucketStatisticsResponse_unclassifiableObjectSizeInBytes,
    getBucketStatisticsResponse_bucketCountByEncryptionType,
    getBucketStatisticsResponse_httpStatus,

    -- ** GetClassificationExportConfiguration
    getClassificationExportConfigurationResponse_configuration,
    getClassificationExportConfigurationResponse_httpStatus,

    -- ** GetCustomDataIdentifier
    getCustomDataIdentifier_id,
    getCustomDataIdentifierResponse_tags,
    getCustomDataIdentifierResponse_name,
    getCustomDataIdentifierResponse_deleted,
    getCustomDataIdentifierResponse_regex,
    getCustomDataIdentifierResponse_arn,
    getCustomDataIdentifierResponse_ignoreWords,
    getCustomDataIdentifierResponse_keywords,
    getCustomDataIdentifierResponse_description,
    getCustomDataIdentifierResponse_id,
    getCustomDataIdentifierResponse_severityLevels,
    getCustomDataIdentifierResponse_maximumMatchDistance,
    getCustomDataIdentifierResponse_createdAt,
    getCustomDataIdentifierResponse_httpStatus,

    -- ** GetFindingStatistics
    getFindingStatistics_sortCriteria,
    getFindingStatistics_findingCriteria,
    getFindingStatistics_size,
    getFindingStatistics_groupBy,
    getFindingStatisticsResponse_countsByGroup,
    getFindingStatisticsResponse_httpStatus,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_findingIds,
    getFindingsResponse_findings,
    getFindingsResponse_httpStatus,

    -- ** GetFindingsFilter
    getFindingsFilter_id,
    getFindingsFilterResponse_tags,
    getFindingsFilterResponse_name,
    getFindingsFilterResponse_findingCriteria,
    getFindingsFilterResponse_arn,
    getFindingsFilterResponse_description,
    getFindingsFilterResponse_id,
    getFindingsFilterResponse_action,
    getFindingsFilterResponse_position,
    getFindingsFilterResponse_httpStatus,

    -- ** GetFindingsPublicationConfiguration
    getFindingsPublicationConfigurationResponse_securityHubConfiguration,
    getFindingsPublicationConfigurationResponse_httpStatus,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** GetMacieSession
    getMacieSessionResponse_status,
    getMacieSessionResponse_serviceRole,
    getMacieSessionResponse_findingPublishingFrequency,
    getMacieSessionResponse_createdAt,
    getMacieSessionResponse_updatedAt,
    getMacieSessionResponse_httpStatus,

    -- ** GetMasterAccount
    getMasterAccountResponse_master,
    getMasterAccountResponse_httpStatus,

    -- ** GetMember
    getMember_id,
    getMemberResponse_tags,
    getMemberResponse_email,
    getMemberResponse_arn,
    getMemberResponse_masterAccountId,
    getMemberResponse_accountId,
    getMemberResponse_invitedAt,
    getMemberResponse_administratorAccountId,
    getMemberResponse_relationshipStatus,
    getMemberResponse_updatedAt,
    getMemberResponse_httpStatus,

    -- ** GetRevealConfiguration
    getRevealConfigurationResponse_configuration,
    getRevealConfigurationResponse_httpStatus,

    -- ** GetSensitiveDataOccurrences
    getSensitiveDataOccurrences_findingId,
    getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences,
    getSensitiveDataOccurrencesResponse_status,
    getSensitiveDataOccurrencesResponse_error,
    getSensitiveDataOccurrencesResponse_httpStatus,

    -- ** GetSensitiveDataOccurrencesAvailability
    getSensitiveDataOccurrencesAvailability_findingId,
    getSensitiveDataOccurrencesAvailabilityResponse_code,
    getSensitiveDataOccurrencesAvailabilityResponse_reasons,
    getSensitiveDataOccurrencesAvailabilityResponse_httpStatus,

    -- ** GetUsageStatistics
    getUsageStatistics_nextToken,
    getUsageStatistics_timeRange,
    getUsageStatistics_filterBy,
    getUsageStatistics_sortBy,
    getUsageStatistics_maxResults,
    getUsageStatisticsResponse_records,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_timeRange,
    getUsageStatisticsResponse_httpStatus,

    -- ** GetUsageTotals
    getUsageTotals_timeRange,
    getUsageTotalsResponse_timeRange,
    getUsageTotalsResponse_usageTotals,
    getUsageTotalsResponse_httpStatus,

    -- ** ListAllowLists
    listAllowLists_nextToken,
    listAllowLists_maxResults,
    listAllowListsResponse_nextToken,
    listAllowListsResponse_allowLists,
    listAllowListsResponse_httpStatus,

    -- ** ListClassificationJobs
    listClassificationJobs_sortCriteria,
    listClassificationJobs_nextToken,
    listClassificationJobs_filterCriteria,
    listClassificationJobs_maxResults,
    listClassificationJobsResponse_items,
    listClassificationJobsResponse_nextToken,
    listClassificationJobsResponse_httpStatus,

    -- ** ListCustomDataIdentifiers
    listCustomDataIdentifiers_nextToken,
    listCustomDataIdentifiers_maxResults,
    listCustomDataIdentifiersResponse_items,
    listCustomDataIdentifiersResponse_nextToken,
    listCustomDataIdentifiersResponse_httpStatus,

    -- ** ListFindings
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindingsResponse_nextToken,
    listFindingsResponse_findingIds,
    listFindingsResponse_httpStatus,

    -- ** ListFindingsFilters
    listFindingsFilters_nextToken,
    listFindingsFilters_maxResults,
    listFindingsFiltersResponse_nextToken,
    listFindingsFiltersResponse_findingsFilterListItems,
    listFindingsFiltersResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListManagedDataIdentifiers
    listManagedDataIdentifiers_nextToken,
    listManagedDataIdentifiersResponse_items,
    listManagedDataIdentifiersResponse_nextToken,
    listManagedDataIdentifiersResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_maxResults,
    listMembersResponse_nextToken,
    listMembersResponse_members,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutClassificationExportConfiguration
    putClassificationExportConfiguration_configuration,
    putClassificationExportConfigurationResponse_configuration,
    putClassificationExportConfigurationResponse_httpStatus,

    -- ** PutFindingsPublicationConfiguration
    putFindingsPublicationConfiguration_clientToken,
    putFindingsPublicationConfiguration_securityHubConfiguration,
    putFindingsPublicationConfigurationResponse_httpStatus,

    -- ** SearchResources
    searchResources_sortCriteria,
    searchResources_bucketCriteria,
    searchResources_nextToken,
    searchResources_maxResults,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_matchingResources,
    searchResourcesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** TestCustomDataIdentifier
    testCustomDataIdentifier_ignoreWords,
    testCustomDataIdentifier_keywords,
    testCustomDataIdentifier_maximumMatchDistance,
    testCustomDataIdentifier_regex,
    testCustomDataIdentifier_sampleText,
    testCustomDataIdentifierResponse_matchCount,
    testCustomDataIdentifierResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,
    untagResourceResponse_httpStatus,

    -- ** UpdateAllowList
    updateAllowList_description,
    updateAllowList_id,
    updateAllowList_criteria,
    updateAllowList_name,
    updateAllowListResponse_arn,
    updateAllowListResponse_id,
    updateAllowListResponse_httpStatus,

    -- ** UpdateClassificationJob
    updateClassificationJob_jobId,
    updateClassificationJob_jobStatus,
    updateClassificationJobResponse_httpStatus,

    -- ** UpdateFindingsFilter
    updateFindingsFilter_name,
    updateFindingsFilter_clientToken,
    updateFindingsFilter_findingCriteria,
    updateFindingsFilter_description,
    updateFindingsFilter_action,
    updateFindingsFilter_position,
    updateFindingsFilter_id,
    updateFindingsFilterResponse_arn,
    updateFindingsFilterResponse_id,
    updateFindingsFilterResponse_httpStatus,

    -- ** UpdateMacieSession
    updateMacieSession_status,
    updateMacieSession_findingPublishingFrequency,
    updateMacieSessionResponse_httpStatus,

    -- ** UpdateMemberSession
    updateMemberSession_id,
    updateMemberSession_status,
    updateMemberSessionResponse_httpStatus,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** UpdateRevealConfiguration
    updateRevealConfiguration_configuration,
    updateRevealConfigurationResponse_configuration,
    updateRevealConfigurationResponse_httpStatus,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

    -- ** AccountDetail
    accountDetail_email,
    accountDetail_accountId,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** AdminAccount
    adminAccount_status,
    adminAccount_accountId,

    -- ** AllowListCriteria
    allowListCriteria_regex,
    allowListCriteria_s3WordsList,

    -- ** AllowListStatus
    allowListStatus_description,
    allowListStatus_code,

    -- ** AllowListSummary
    allowListSummary_name,
    allowListSummary_arn,
    allowListSummary_description,
    allowListSummary_id,
    allowListSummary_createdAt,
    allowListSummary_updatedAt,

    -- ** ApiCallDetails
    apiCallDetails_lastSeen,
    apiCallDetails_apiServiceName,
    apiCallDetails_api,
    apiCallDetails_firstSeen,

    -- ** AssumedRole
    assumedRole_principalId,
    assumedRole_arn,
    assumedRole_sessionContext,
    assumedRole_accountId,
    assumedRole_accessKeyId,

    -- ** AwsAccount
    awsAccount_principalId,
    awsAccount_accountId,

    -- ** AwsService
    awsService_invokedBy,

    -- ** BatchGetCustomDataIdentifierSummary
    batchGetCustomDataIdentifierSummary_name,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_description,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_createdAt,

    -- ** BlockPublicAccess
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_blockPublicAcls,

    -- ** BucketCountByEffectivePermission
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_unknown,
    bucketCountByEffectivePermission_publiclyWritable,

    -- ** BucketCountByEncryptionType
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_kmsManaged,
    bucketCountByEncryptionType_unknown,

    -- ** BucketCountBySharedAccessType
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_unknown,
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_internal,

    -- ** BucketCountPolicyAllowsUnencryptedObjectUploads
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,

    -- ** BucketCriteriaAdditionalProperties
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_gte,
    bucketCriteriaAdditionalProperties_prefix,
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_gt,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- ** BucketMetadata
    bucketMetadata_tags,
    bucketMetadata_serverSideEncryption,
    bucketMetadata_objectCountByEncryptionType,
    bucketMetadata_classifiableSizeInBytes,
    bucketMetadata_errorMessage,
    bucketMetadata_jobDetails,
    bucketMetadata_unclassifiableObjectCount,
    bucketMetadata_bucketCreatedAt,
    bucketMetadata_replicationDetails,
    bucketMetadata_allowsUnencryptedObjectUploads,
    bucketMetadata_publicAccess,
    bucketMetadata_objectCount,
    bucketMetadata_versioning,
    bucketMetadata_lastUpdated,
    bucketMetadata_region,
    bucketMetadata_bucketName,
    bucketMetadata_accountId,
    bucketMetadata_bucketArn,
    bucketMetadata_sharedAccess,
    bucketMetadata_classifiableObjectCount,
    bucketMetadata_sizeInBytes,
    bucketMetadata_errorCode,
    bucketMetadata_sizeInBytesCompressed,
    bucketMetadata_unclassifiableObjectSizeInBytes,

    -- ** BucketPermissionConfiguration
    bucketPermissionConfiguration_accountLevelPermissions,
    bucketPermissionConfiguration_bucketLevelPermissions,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- ** BucketPublicAccess
    bucketPublicAccess_permissionConfiguration,
    bucketPublicAccess_effectivePermission,

    -- ** BucketServerSideEncryption
    bucketServerSideEncryption_kmsMasterKeyId,
    bucketServerSideEncryption_type,

    -- ** BucketSortCriteria
    bucketSortCriteria_orderBy,
    bucketSortCriteria_attributeName,

    -- ** Cell
    cell_row,
    cell_cellReference,
    cell_columnName,
    cell_column,

    -- ** ClassificationDetails
    classificationDetails_originType,
    classificationDetails_jobId,
    classificationDetails_detailedResultsLocation,
    classificationDetails_result,
    classificationDetails_jobArn,

    -- ** ClassificationExportConfiguration
    classificationExportConfiguration_s3Destination,

    -- ** ClassificationResult
    classificationResult_status,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_sensitiveData,

    -- ** ClassificationResultStatus
    classificationResultStatus_code,
    classificationResultStatus_reason,

    -- ** CriteriaBlockForJob
    criteriaBlockForJob_and,

    -- ** CriteriaForJob
    criteriaForJob_tagCriterion,
    criteriaForJob_simpleCriterion,

    -- ** CriterionAdditionalProperties
    criterionAdditionalProperties_neq,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_gte,
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_eqExactMatch,

    -- ** CustomDataIdentifierSummary
    customDataIdentifierSummary_name,
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_description,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_createdAt,

    -- ** CustomDataIdentifiers
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- ** CustomDetection
    customDetection_occurrences,
    customDetection_name,
    customDetection_arn,
    customDetection_count,

    -- ** DailySchedule

    -- ** DefaultDetection
    defaultDetection_occurrences,
    defaultDetection_type,
    defaultDetection_count,

    -- ** DetectedDataDetails
    detectedDataDetails_value,

    -- ** DomainDetails
    domainDetails_domainName,

    -- ** FederatedUser
    federatedUser_principalId,
    federatedUser_arn,
    federatedUser_sessionContext,
    federatedUser_accountId,
    federatedUser_accessKeyId,

    -- ** Finding
    finding_severity,
    finding_type,
    finding_policyDetails,
    finding_classificationDetails,
    finding_description,
    finding_id,
    finding_count,
    finding_partition,
    finding_archived,
    finding_region,
    finding_accountId,
    finding_title,
    finding_resourcesAffected,
    finding_schemaVersion,
    finding_category,
    finding_createdAt,
    finding_updatedAt,
    finding_sample,

    -- ** FindingAction
    findingAction_actionType,
    findingAction_apiCallDetails,

    -- ** FindingActor
    findingActor_userIdentity,
    findingActor_ipAddressDetails,
    findingActor_domainDetails,

    -- ** FindingCriteria
    findingCriteria_criterion,

    -- ** FindingStatisticsSortCriteria
    findingStatisticsSortCriteria_orderBy,
    findingStatisticsSortCriteria_attributeName,

    -- ** FindingsFilterListItem
    findingsFilterListItem_tags,
    findingsFilterListItem_name,
    findingsFilterListItem_arn,
    findingsFilterListItem_id,
    findingsFilterListItem_action,

    -- ** GroupCount
    groupCount_groupKey,
    groupCount_count,

    -- ** IamUser
    iamUser_principalId,
    iamUser_userName,
    iamUser_arn,
    iamUser_accountId,

    -- ** Invitation
    invitation_accountId,
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,

    -- ** IpAddressDetails
    ipAddressDetails_ipCountry,
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipOwner,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipCity,

    -- ** IpCity
    ipCity_name,

    -- ** IpCountry
    ipCountry_name,
    ipCountry_code,

    -- ** IpGeoLocation
    ipGeoLocation_lat,
    ipGeoLocation_lon,

    -- ** IpOwner
    ipOwner_isp,
    ipOwner_org,
    ipOwner_asn,
    ipOwner_asnOrg,

    -- ** JobDetails
    jobDetails_lastJobId,
    jobDetails_isDefinedInJob,
    jobDetails_lastJobRunTime,
    jobDetails_isMonitoredByJob,

    -- ** JobScheduleFrequency
    jobScheduleFrequency_dailySchedule,
    jobScheduleFrequency_monthlySchedule,
    jobScheduleFrequency_weeklySchedule,

    -- ** JobScopeTerm
    jobScopeTerm_tagScopeTerm,
    jobScopeTerm_simpleScopeTerm,

    -- ** JobScopingBlock
    jobScopingBlock_and,

    -- ** JobSummary
    jobSummary_bucketCriteria,
    jobSummary_name,
    jobSummary_jobStatus,
    jobSummary_userPausedDetails,
    jobSummary_bucketDefinitions,
    jobSummary_jobId,
    jobSummary_lastRunErrorStatus,
    jobSummary_createdAt,
    jobSummary_jobType,

    -- ** KeyValuePair
    keyValuePair_key,
    keyValuePair_value,

    -- ** LastRunErrorStatus
    lastRunErrorStatus_code,

    -- ** ListJobsFilterCriteria
    listJobsFilterCriteria_excludes,
    listJobsFilterCriteria_includes,

    -- ** ListJobsFilterTerm
    listJobsFilterTerm_key,
    listJobsFilterTerm_comparator,
    listJobsFilterTerm_values,

    -- ** ListJobsSortCriteria
    listJobsSortCriteria_orderBy,
    listJobsSortCriteria_attributeName,

    -- ** ManagedDataIdentifierSummary
    managedDataIdentifierSummary_id,
    managedDataIdentifierSummary_category,

    -- ** MatchingBucket
    matchingBucket_objectCountByEncryptionType,
    matchingBucket_classifiableSizeInBytes,
    matchingBucket_errorMessage,
    matchingBucket_jobDetails,
    matchingBucket_unclassifiableObjectCount,
    matchingBucket_objectCount,
    matchingBucket_bucketName,
    matchingBucket_accountId,
    matchingBucket_classifiableObjectCount,
    matchingBucket_sizeInBytes,
    matchingBucket_errorCode,
    matchingBucket_sizeInBytesCompressed,
    matchingBucket_unclassifiableObjectSizeInBytes,

    -- ** MatchingResource
    matchingResource_matchingBucket,

    -- ** Member
    member_tags,
    member_email,
    member_arn,
    member_masterAccountId,
    member_accountId,
    member_invitedAt,
    member_administratorAccountId,
    member_relationshipStatus,
    member_updatedAt,

    -- ** MonthlySchedule
    monthlySchedule_dayOfMonth,

    -- ** ObjectCountByEncryptionType
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_customerManaged,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_unknown,

    -- ** ObjectLevelStatistics
    objectLevelStatistics_total,
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,

    -- ** Occurrences
    occurrences_records,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_cells,
    occurrences_pages,

    -- ** Page
    page_offsetRange,
    page_pageNumber,
    page_lineRange,

    -- ** PolicyDetails
    policyDetails_action,
    policyDetails_actor,

    -- ** Range
    range_start,
    range_end,
    range_startColumn,

    -- ** Record
    record_jsonPath,
    record_recordIndex,

    -- ** ReplicationDetails
    replicationDetails_replicatedExternally,
    replicationDetails_replicated,
    replicationDetails_replicationAccounts,

    -- ** ResourcesAffected
    resourcesAffected_s3Bucket,
    resourcesAffected_s3Object,

    -- ** RevealConfiguration
    revealConfiguration_kmsKeyId,
    revealConfiguration_status,

    -- ** S3Bucket
    s3Bucket_tags,
    s3Bucket_name,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_arn,
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_publicAccess,
    s3Bucket_owner,
    s3Bucket_createdAt,

    -- ** S3BucketCriteriaForJob
    s3BucketCriteriaForJob_excludes,
    s3BucketCriteriaForJob_includes,

    -- ** S3BucketDefinitionForJob
    s3BucketDefinitionForJob_accountId,
    s3BucketDefinitionForJob_buckets,

    -- ** S3BucketOwner
    s3BucketOwner_displayName,
    s3BucketOwner_id,

    -- ** S3Destination
    s3Destination_keyPrefix,
    s3Destination_bucketName,
    s3Destination_kmsKeyArn,

    -- ** S3JobDefinition
    s3JobDefinition_bucketCriteria,
    s3JobDefinition_bucketDefinitions,
    s3JobDefinition_scoping,

    -- ** S3Object
    s3Object_tags,
    s3Object_serverSideEncryption,
    s3Object_key,
    s3Object_extension,
    s3Object_path,
    s3Object_publicAccess,
    s3Object_size,
    s3Object_bucketArn,
    s3Object_lastModified,
    s3Object_eTag,
    s3Object_storageClass,
    s3Object_versionId,

    -- ** S3WordsList
    s3WordsList_bucketName,
    s3WordsList_objectKey,

    -- ** Scoping
    scoping_excludes,
    scoping_includes,

    -- ** SearchResourcesBucketCriteria
    searchResourcesBucketCriteria_excludes,
    searchResourcesBucketCriteria_includes,

    -- ** SearchResourcesCriteria
    searchResourcesCriteria_tagCriterion,
    searchResourcesCriteria_simpleCriterion,

    -- ** SearchResourcesCriteriaBlock
    searchResourcesCriteriaBlock_and,

    -- ** SearchResourcesSimpleCriterion
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_comparator,
    searchResourcesSimpleCriterion_values,

    -- ** SearchResourcesSortCriteria
    searchResourcesSortCriteria_orderBy,
    searchResourcesSortCriteria_attributeName,

    -- ** SearchResourcesTagCriterion
    searchResourcesTagCriterion_tagValues,
    searchResourcesTagCriterion_comparator,

    -- ** SearchResourcesTagCriterionPair
    searchResourcesTagCriterionPair_key,
    searchResourcesTagCriterionPair_value,

    -- ** SecurityHubConfiguration
    securityHubConfiguration_publishPolicyFindings,
    securityHubConfiguration_publishClassificationFindings,

    -- ** SensitiveDataItem
    sensitiveDataItem_detections,
    sensitiveDataItem_category,
    sensitiveDataItem_totalCount,

    -- ** ServerSideEncryption
    serverSideEncryption_kmsMasterKeyId,
    serverSideEncryption_encryptionType,

    -- ** ServiceLimit
    serviceLimit_isServiceLimited,
    serviceLimit_unit,
    serviceLimit_value,

    -- ** SessionContext
    sessionContext_sessionIssuer,
    sessionContext_attributes,

    -- ** SessionContextAttributes
    sessionContextAttributes_mfaAuthenticated,
    sessionContextAttributes_creationDate,

    -- ** SessionIssuer
    sessionIssuer_principalId,
    sessionIssuer_type,
    sessionIssuer_userName,
    sessionIssuer_arn,
    sessionIssuer_accountId,

    -- ** Severity
    severity_score,
    severity_description,

    -- ** SeverityLevel
    severityLevel_occurrencesThreshold,
    severityLevel_severity,

    -- ** SimpleCriterionForJob
    simpleCriterionForJob_key,
    simpleCriterionForJob_comparator,
    simpleCriterionForJob_values,

    -- ** SimpleScopeTerm
    simpleScopeTerm_key,
    simpleScopeTerm_comparator,
    simpleScopeTerm_values,

    -- ** SortCriteria
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- ** Statistics
    statistics_approximateNumberOfObjectsToProcess,
    statistics_numberOfRuns,

    -- ** TagCriterionForJob
    tagCriterionForJob_tagValues,
    tagCriterionForJob_comparator,

    -- ** TagCriterionPairForJob
    tagCriterionPairForJob_key,
    tagCriterionPairForJob_value,

    -- ** TagScopeTerm
    tagScopeTerm_key,
    tagScopeTerm_tagValues,
    tagScopeTerm_target,
    tagScopeTerm_comparator,

    -- ** TagValuePair
    tagValuePair_key,
    tagValuePair_value,

    -- ** UnprocessedAccount
    unprocessedAccount_errorMessage,
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,

    -- ** UsageByAccount
    usageByAccount_type,
    usageByAccount_serviceLimit,
    usageByAccount_currency,
    usageByAccount_estimatedCost,

    -- ** UsageRecord
    usageRecord_usage,
    usageRecord_accountId,
    usageRecord_freeTrialStartDate,

    -- ** UsageStatisticsFilter
    usageStatisticsFilter_key,
    usageStatisticsFilter_comparator,
    usageStatisticsFilter_values,

    -- ** UsageStatisticsSortBy
    usageStatisticsSortBy_key,
    usageStatisticsSortBy_orderBy,

    -- ** UsageTotal
    usageTotal_type,
    usageTotal_currency,
    usageTotal_estimatedCost,

    -- ** UserIdentity
    userIdentity_type,
    userIdentity_awsAccount,
    userIdentity_federatedUser,
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_assumedRole,
    userIdentity_awsService,

    -- ** UserIdentityRoot
    userIdentityRoot_principalId,
    userIdentityRoot_arn,
    userIdentityRoot_accountId,

    -- ** UserPausedDetails
    userPausedDetails_jobExpiresAt,
    userPausedDetails_jobPausedAt,
    userPausedDetails_jobImminentExpirationHealthEventArn,

    -- ** WeeklySchedule
    weeklySchedule_dayOfWeek,
  )
where

import Amazonka.MacieV2.AcceptInvitation
import Amazonka.MacieV2.BatchGetCustomDataIdentifiers
import Amazonka.MacieV2.CreateAllowList
import Amazonka.MacieV2.CreateClassificationJob
import Amazonka.MacieV2.CreateCustomDataIdentifier
import Amazonka.MacieV2.CreateFindingsFilter
import Amazonka.MacieV2.CreateInvitations
import Amazonka.MacieV2.CreateMember
import Amazonka.MacieV2.CreateSampleFindings
import Amazonka.MacieV2.DeclineInvitations
import Amazonka.MacieV2.DeleteAllowList
import Amazonka.MacieV2.DeleteCustomDataIdentifier
import Amazonka.MacieV2.DeleteFindingsFilter
import Amazonka.MacieV2.DeleteInvitations
import Amazonka.MacieV2.DeleteMember
import Amazonka.MacieV2.DescribeBuckets
import Amazonka.MacieV2.DescribeClassificationJob
import Amazonka.MacieV2.DescribeOrganizationConfiguration
import Amazonka.MacieV2.DisableMacie
import Amazonka.MacieV2.DisableOrganizationAdminAccount
import Amazonka.MacieV2.DisassociateFromAdministratorAccount
import Amazonka.MacieV2.DisassociateFromMasterAccount
import Amazonka.MacieV2.DisassociateMember
import Amazonka.MacieV2.EnableMacie
import Amazonka.MacieV2.EnableOrganizationAdminAccount
import Amazonka.MacieV2.GetAdministratorAccount
import Amazonka.MacieV2.GetAllowList
import Amazonka.MacieV2.GetBucketStatistics
import Amazonka.MacieV2.GetClassificationExportConfiguration
import Amazonka.MacieV2.GetCustomDataIdentifier
import Amazonka.MacieV2.GetFindingStatistics
import Amazonka.MacieV2.GetFindings
import Amazonka.MacieV2.GetFindingsFilter
import Amazonka.MacieV2.GetFindingsPublicationConfiguration
import Amazonka.MacieV2.GetInvitationsCount
import Amazonka.MacieV2.GetMacieSession
import Amazonka.MacieV2.GetMasterAccount
import Amazonka.MacieV2.GetMember
import Amazonka.MacieV2.GetRevealConfiguration
import Amazonka.MacieV2.GetSensitiveDataOccurrences
import Amazonka.MacieV2.GetSensitiveDataOccurrencesAvailability
import Amazonka.MacieV2.GetUsageStatistics
import Amazonka.MacieV2.GetUsageTotals
import Amazonka.MacieV2.ListAllowLists
import Amazonka.MacieV2.ListClassificationJobs
import Amazonka.MacieV2.ListCustomDataIdentifiers
import Amazonka.MacieV2.ListFindings
import Amazonka.MacieV2.ListFindingsFilters
import Amazonka.MacieV2.ListInvitations
import Amazonka.MacieV2.ListManagedDataIdentifiers
import Amazonka.MacieV2.ListMembers
import Amazonka.MacieV2.ListOrganizationAdminAccounts
import Amazonka.MacieV2.ListTagsForResource
import Amazonka.MacieV2.PutClassificationExportConfiguration
import Amazonka.MacieV2.PutFindingsPublicationConfiguration
import Amazonka.MacieV2.SearchResources
import Amazonka.MacieV2.TagResource
import Amazonka.MacieV2.TestCustomDataIdentifier
import Amazonka.MacieV2.Types.AccessControlList
import Amazonka.MacieV2.Types.AccountDetail
import Amazonka.MacieV2.Types.AccountLevelPermissions
import Amazonka.MacieV2.Types.AdminAccount
import Amazonka.MacieV2.Types.AllowListCriteria
import Amazonka.MacieV2.Types.AllowListStatus
import Amazonka.MacieV2.Types.AllowListSummary
import Amazonka.MacieV2.Types.ApiCallDetails
import Amazonka.MacieV2.Types.AssumedRole
import Amazonka.MacieV2.Types.AwsAccount
import Amazonka.MacieV2.Types.AwsService
import Amazonka.MacieV2.Types.BatchGetCustomDataIdentifierSummary
import Amazonka.MacieV2.Types.BlockPublicAccess
import Amazonka.MacieV2.Types.BucketCountByEffectivePermission
import Amazonka.MacieV2.Types.BucketCountByEncryptionType
import Amazonka.MacieV2.Types.BucketCountBySharedAccessType
import Amazonka.MacieV2.Types.BucketCountPolicyAllowsUnencryptedObjectUploads
import Amazonka.MacieV2.Types.BucketCriteriaAdditionalProperties
import Amazonka.MacieV2.Types.BucketLevelPermissions
import Amazonka.MacieV2.Types.BucketMetadata
import Amazonka.MacieV2.Types.BucketPermissionConfiguration
import Amazonka.MacieV2.Types.BucketPolicy
import Amazonka.MacieV2.Types.BucketPublicAccess
import Amazonka.MacieV2.Types.BucketServerSideEncryption
import Amazonka.MacieV2.Types.BucketSortCriteria
import Amazonka.MacieV2.Types.Cell
import Amazonka.MacieV2.Types.ClassificationDetails
import Amazonka.MacieV2.Types.ClassificationExportConfiguration
import Amazonka.MacieV2.Types.ClassificationResult
import Amazonka.MacieV2.Types.ClassificationResultStatus
import Amazonka.MacieV2.Types.CriteriaBlockForJob
import Amazonka.MacieV2.Types.CriteriaForJob
import Amazonka.MacieV2.Types.CriterionAdditionalProperties
import Amazonka.MacieV2.Types.CustomDataIdentifierSummary
import Amazonka.MacieV2.Types.CustomDataIdentifiers
import Amazonka.MacieV2.Types.CustomDetection
import Amazonka.MacieV2.Types.DailySchedule
import Amazonka.MacieV2.Types.DefaultDetection
import Amazonka.MacieV2.Types.DetectedDataDetails
import Amazonka.MacieV2.Types.DomainDetails
import Amazonka.MacieV2.Types.FederatedUser
import Amazonka.MacieV2.Types.Finding
import Amazonka.MacieV2.Types.FindingAction
import Amazonka.MacieV2.Types.FindingActor
import Amazonka.MacieV2.Types.FindingCriteria
import Amazonka.MacieV2.Types.FindingStatisticsSortCriteria
import Amazonka.MacieV2.Types.FindingsFilterListItem
import Amazonka.MacieV2.Types.GroupCount
import Amazonka.MacieV2.Types.IamUser
import Amazonka.MacieV2.Types.Invitation
import Amazonka.MacieV2.Types.IpAddressDetails
import Amazonka.MacieV2.Types.IpCity
import Amazonka.MacieV2.Types.IpCountry
import Amazonka.MacieV2.Types.IpGeoLocation
import Amazonka.MacieV2.Types.IpOwner
import Amazonka.MacieV2.Types.JobDetails
import Amazonka.MacieV2.Types.JobScheduleFrequency
import Amazonka.MacieV2.Types.JobScopeTerm
import Amazonka.MacieV2.Types.JobScopingBlock
import Amazonka.MacieV2.Types.JobSummary
import Amazonka.MacieV2.Types.KeyValuePair
import Amazonka.MacieV2.Types.LastRunErrorStatus
import Amazonka.MacieV2.Types.ListJobsFilterCriteria
import Amazonka.MacieV2.Types.ListJobsFilterTerm
import Amazonka.MacieV2.Types.ListJobsSortCriteria
import Amazonka.MacieV2.Types.ManagedDataIdentifierSummary
import Amazonka.MacieV2.Types.MatchingBucket
import Amazonka.MacieV2.Types.MatchingResource
import Amazonka.MacieV2.Types.Member
import Amazonka.MacieV2.Types.MonthlySchedule
import Amazonka.MacieV2.Types.ObjectCountByEncryptionType
import Amazonka.MacieV2.Types.ObjectLevelStatistics
import Amazonka.MacieV2.Types.Occurrences
import Amazonka.MacieV2.Types.Page
import Amazonka.MacieV2.Types.PolicyDetails
import Amazonka.MacieV2.Types.Range
import Amazonka.MacieV2.Types.Record
import Amazonka.MacieV2.Types.ReplicationDetails
import Amazonka.MacieV2.Types.ResourcesAffected
import Amazonka.MacieV2.Types.RevealConfiguration
import Amazonka.MacieV2.Types.S3Bucket
import Amazonka.MacieV2.Types.S3BucketCriteriaForJob
import Amazonka.MacieV2.Types.S3BucketDefinitionForJob
import Amazonka.MacieV2.Types.S3BucketOwner
import Amazonka.MacieV2.Types.S3Destination
import Amazonka.MacieV2.Types.S3JobDefinition
import Amazonka.MacieV2.Types.S3Object
import Amazonka.MacieV2.Types.S3WordsList
import Amazonka.MacieV2.Types.Scoping
import Amazonka.MacieV2.Types.SearchResourcesBucketCriteria
import Amazonka.MacieV2.Types.SearchResourcesCriteria
import Amazonka.MacieV2.Types.SearchResourcesCriteriaBlock
import Amazonka.MacieV2.Types.SearchResourcesSimpleCriterion
import Amazonka.MacieV2.Types.SearchResourcesSortCriteria
import Amazonka.MacieV2.Types.SearchResourcesTagCriterion
import Amazonka.MacieV2.Types.SearchResourcesTagCriterionPair
import Amazonka.MacieV2.Types.SecurityHubConfiguration
import Amazonka.MacieV2.Types.SensitiveDataItem
import Amazonka.MacieV2.Types.ServerSideEncryption
import Amazonka.MacieV2.Types.ServiceLimit
import Amazonka.MacieV2.Types.SessionContext
import Amazonka.MacieV2.Types.SessionContextAttributes
import Amazonka.MacieV2.Types.SessionIssuer
import Amazonka.MacieV2.Types.Severity
import Amazonka.MacieV2.Types.SeverityLevel
import Amazonka.MacieV2.Types.SimpleCriterionForJob
import Amazonka.MacieV2.Types.SimpleScopeTerm
import Amazonka.MacieV2.Types.SortCriteria
import Amazonka.MacieV2.Types.Statistics
import Amazonka.MacieV2.Types.TagCriterionForJob
import Amazonka.MacieV2.Types.TagCriterionPairForJob
import Amazonka.MacieV2.Types.TagScopeTerm
import Amazonka.MacieV2.Types.TagValuePair
import Amazonka.MacieV2.Types.UnprocessedAccount
import Amazonka.MacieV2.Types.UsageByAccount
import Amazonka.MacieV2.Types.UsageRecord
import Amazonka.MacieV2.Types.UsageStatisticsFilter
import Amazonka.MacieV2.Types.UsageStatisticsSortBy
import Amazonka.MacieV2.Types.UsageTotal
import Amazonka.MacieV2.Types.UserIdentity
import Amazonka.MacieV2.Types.UserIdentityRoot
import Amazonka.MacieV2.Types.UserPausedDetails
import Amazonka.MacieV2.Types.WeeklySchedule
import Amazonka.MacieV2.UntagResource
import Amazonka.MacieV2.UpdateAllowList
import Amazonka.MacieV2.UpdateClassificationJob
import Amazonka.MacieV2.UpdateFindingsFilter
import Amazonka.MacieV2.UpdateMacieSession
import Amazonka.MacieV2.UpdateMemberSession
import Amazonka.MacieV2.UpdateOrganizationConfiguration
import Amazonka.MacieV2.UpdateRevealConfiguration
