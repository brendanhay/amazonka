{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Lens
  ( -- * Operations

    -- ** CreateMember
    createMember_tags,
    createMember_account,
    createMemberResponse_arn,
    createMemberResponse_httpStatus,

    -- ** EnableOrganizationAdminAccount
    enableOrganizationAdminAccount_clientToken,
    enableOrganizationAdminAccount_adminAccountId,
    enableOrganizationAdminAccountResponse_httpStatus,

    -- ** DescribeClassificationJob
    describeClassificationJob_jobId,
    describeClassificationJobResponse_lastRunErrorStatus,
    describeClassificationJobResponse_jobType,
    describeClassificationJobResponse_initialRun,
    describeClassificationJobResponse_jobId,
    describeClassificationJobResponse_clientToken,
    describeClassificationJobResponse_jobArn,
    describeClassificationJobResponse_s3JobDefinition,
    describeClassificationJobResponse_createdAt,
    describeClassificationJobResponse_userPausedDetails,
    describeClassificationJobResponse_samplingPercentage,
    describeClassificationJobResponse_managedDataIdentifierSelector,
    describeClassificationJobResponse_lastRunTime,
    describeClassificationJobResponse_customDataIdentifierIds,
    describeClassificationJobResponse_name,
    describeClassificationJobResponse_statistics,
    describeClassificationJobResponse_managedDataIdentifierIds,
    describeClassificationJobResponse_jobStatus,
    describeClassificationJobResponse_description,
    describeClassificationJobResponse_tags,
    describeClassificationJobResponse_scheduleFrequency,
    describeClassificationJobResponse_httpStatus,

    -- ** ListFindings
    listFindings_findingCriteria,
    listFindings_sortCriteria,
    listFindings_nextToken,
    listFindings_maxResults,
    listFindingsResponse_findingIds,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,

    -- ** GetAdministratorAccount
    getAdministratorAccountResponse_administrator,
    getAdministratorAccountResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** SearchResources
    searchResources_bucketCriteria,
    searchResources_sortCriteria,
    searchResources_nextToken,
    searchResources_maxResults,
    searchResourcesResponse_nextToken,
    searchResourcesResponse_matchingResources,
    searchResourcesResponse_httpStatus,

    -- ** DisableMacie
    disableMacieResponse_httpStatus,

    -- ** UpdateFindingsFilter
    updateFindingsFilter_clientToken,
    updateFindingsFilter_findingCriteria,
    updateFindingsFilter_action,
    updateFindingsFilter_name,
    updateFindingsFilter_description,
    updateFindingsFilter_position,
    updateFindingsFilter_id,
    updateFindingsFilterResponse_arn,
    updateFindingsFilterResponse_id,
    updateFindingsFilterResponse_httpStatus,

    -- ** DeleteFindingsFilter
    deleteFindingsFilter_id,
    deleteFindingsFilterResponse_httpStatus,

    -- ** ListFindingsFilters
    listFindingsFilters_nextToken,
    listFindingsFilters_maxResults,
    listFindingsFiltersResponse_findingsFilterListItems,
    listFindingsFiltersResponse_nextToken,
    listFindingsFiltersResponse_httpStatus,

    -- ** EnableMacie
    enableMacie_status,
    enableMacie_clientToken,
    enableMacie_findingPublishingFrequency,
    enableMacieResponse_httpStatus,

    -- ** GetUsageTotals
    getUsageTotals_timeRange,
    getUsageTotalsResponse_timeRange,
    getUsageTotalsResponse_usageTotals,
    getUsageTotalsResponse_httpStatus,

    -- ** CreateFindingsFilter
    createFindingsFilter_clientToken,
    createFindingsFilter_description,
    createFindingsFilter_tags,
    createFindingsFilter_position,
    createFindingsFilter_action,
    createFindingsFilter_findingCriteria,
    createFindingsFilter_name,
    createFindingsFilterResponse_arn,
    createFindingsFilterResponse_id,
    createFindingsFilterResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_nextToken,
    listInvitations_maxResults,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** DescribeBuckets
    describeBuckets_sortCriteria,
    describeBuckets_nextToken,
    describeBuckets_criteria,
    describeBuckets_maxResults,
    describeBucketsResponse_buckets,
    describeBucketsResponse_nextToken,
    describeBucketsResponse_httpStatus,

    -- ** ListClassificationJobs
    listClassificationJobs_filterCriteria,
    listClassificationJobs_sortCriteria,
    listClassificationJobs_nextToken,
    listClassificationJobs_maxResults,
    listClassificationJobsResponse_items,
    listClassificationJobsResponse_nextToken,
    listClassificationJobsResponse_httpStatus,

    -- ** GetFindingsFilter
    getFindingsFilter_id,
    getFindingsFilterResponse_arn,
    getFindingsFilterResponse_findingCriteria,
    getFindingsFilterResponse_action,
    getFindingsFilterResponse_name,
    getFindingsFilterResponse_id,
    getFindingsFilterResponse_description,
    getFindingsFilterResponse_tags,
    getFindingsFilterResponse_position,
    getFindingsFilterResponse_httpStatus,

    -- ** UpdateClassificationJob
    updateClassificationJob_jobId,
    updateClassificationJob_jobStatus,
    updateClassificationJobResponse_httpStatus,

    -- ** DeleteInvitations
    deleteInvitations_accountIds,
    deleteInvitationsResponse_unprocessedAccounts,
    deleteInvitationsResponse_httpStatus,

    -- ** GetMasterAccount
    getMasterAccountResponse_master,
    getMasterAccountResponse_httpStatus,

    -- ** PutClassificationExportConfiguration
    putClassificationExportConfiguration_configuration,
    putClassificationExportConfigurationResponse_configuration,
    putClassificationExportConfigurationResponse_httpStatus,

    -- ** GetCustomDataIdentifier
    getCustomDataIdentifier_id,
    getCustomDataIdentifierResponse_arn,
    getCustomDataIdentifierResponse_createdAt,
    getCustomDataIdentifierResponse_regex,
    getCustomDataIdentifierResponse_name,
    getCustomDataIdentifierResponse_keywords,
    getCustomDataIdentifierResponse_ignoreWords,
    getCustomDataIdentifierResponse_id,
    getCustomDataIdentifierResponse_deleted,
    getCustomDataIdentifierResponse_maximumMatchDistance,
    getCustomDataIdentifierResponse_description,
    getCustomDataIdentifierResponse_tags,
    getCustomDataIdentifierResponse_httpStatus,

    -- ** GetUsageStatistics
    getUsageStatistics_timeRange,
    getUsageStatistics_nextToken,
    getUsageStatistics_filterBy,
    getUsageStatistics_maxResults,
    getUsageStatistics_sortBy,
    getUsageStatisticsResponse_timeRange,
    getUsageStatisticsResponse_records,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_httpStatus,

    -- ** DeclineInvitations
    declineInvitations_accountIds,
    declineInvitationsResponse_unprocessedAccounts,
    declineInvitationsResponse_httpStatus,

    -- ** TestCustomDataIdentifier
    testCustomDataIdentifier_keywords,
    testCustomDataIdentifier_ignoreWords,
    testCustomDataIdentifier_maximumMatchDistance,
    testCustomDataIdentifier_regex,
    testCustomDataIdentifier_sampleText,
    testCustomDataIdentifierResponse_matchCount,
    testCustomDataIdentifierResponse_httpStatus,

    -- ** CreateInvitations
    createInvitations_disableEmailNotification,
    createInvitations_message,
    createInvitations_accountIds,
    createInvitationsResponse_unprocessedAccounts,
    createInvitationsResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_maxAccountLimitReached,
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_httpStatus,

    -- ** BatchGetCustomDataIdentifiers
    batchGetCustomDataIdentifiers_ids,
    batchGetCustomDataIdentifiersResponse_notFoundIdentifierIds,
    batchGetCustomDataIdentifiersResponse_customDataIdentifiers,
    batchGetCustomDataIdentifiersResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_id,
    deleteMemberResponse_httpStatus,

    -- ** DisassociateFromMasterAccount
    disassociateFromMasterAccountResponse_httpStatus,

    -- ** AcceptInvitation
    acceptInvitation_administratorAccountId,
    acceptInvitation_masterAccount,
    acceptInvitation_invitationId,
    acceptInvitationResponse_httpStatus,

    -- ** ListMembers
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembers_maxResults,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** UpdateMacieSession
    updateMacieSession_status,
    updateMacieSession_findingPublishingFrequency,
    updateMacieSessionResponse_httpStatus,

    -- ** GetClassificationExportConfiguration
    getClassificationExportConfigurationResponse_configuration,
    getClassificationExportConfigurationResponse_httpStatus,

    -- ** GetFindingsPublicationConfiguration
    getFindingsPublicationConfigurationResponse_securityHubConfiguration,
    getFindingsPublicationConfigurationResponse_httpStatus,

    -- ** CreateCustomDataIdentifier
    createCustomDataIdentifier_clientToken,
    createCustomDataIdentifier_regex,
    createCustomDataIdentifier_name,
    createCustomDataIdentifier_keywords,
    createCustomDataIdentifier_ignoreWords,
    createCustomDataIdentifier_maximumMatchDistance,
    createCustomDataIdentifier_description,
    createCustomDataIdentifier_tags,
    createCustomDataIdentifierResponse_customDataIdentifierId,
    createCustomDataIdentifierResponse_httpStatus,

    -- ** CreateSampleFindings
    createSampleFindings_findingTypes,
    createSampleFindingsResponse_httpStatus,

    -- ** ListManagedDataIdentifiers
    listManagedDataIdentifiers_nextToken,
    listManagedDataIdentifiersResponse_items,
    listManagedDataIdentifiersResponse_nextToken,
    listManagedDataIdentifiersResponse_httpStatus,

    -- ** UpdateMemberSession
    updateMemberSession_id,
    updateMemberSession_status,
    updateMemberSessionResponse_httpStatus,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** DisassociateMember
    disassociateMember_id,
    disassociateMemberResponse_httpStatus,

    -- ** CreateClassificationJob
    createClassificationJob_initialRun,
    createClassificationJob_samplingPercentage,
    createClassificationJob_managedDataIdentifierSelector,
    createClassificationJob_customDataIdentifierIds,
    createClassificationJob_managedDataIdentifierIds,
    createClassificationJob_description,
    createClassificationJob_tags,
    createClassificationJob_scheduleFrequency,
    createClassificationJob_s3JobDefinition,
    createClassificationJob_jobType,
    createClassificationJob_clientToken,
    createClassificationJob_name,
    createClassificationJobResponse_jobId,
    createClassificationJobResponse_jobArn,
    createClassificationJobResponse_httpStatus,

    -- ** GetBucketStatistics
    getBucketStatistics_accountId,
    getBucketStatisticsResponse_sizeInBytesCompressed,
    getBucketStatisticsResponse_lastUpdated,
    getBucketStatisticsResponse_sizeInBytes,
    getBucketStatisticsResponse_bucketCountBySharedAccessType,
    getBucketStatisticsResponse_classifiableObjectCount,
    getBucketStatisticsResponse_unclassifiableObjectSizeInBytes,
    getBucketStatisticsResponse_unclassifiableObjectCount,
    getBucketStatisticsResponse_bucketCount,
    getBucketStatisticsResponse_bucketCountByEffectivePermission,
    getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement,
    getBucketStatisticsResponse_objectCount,
    getBucketStatisticsResponse_classifiableSizeInBytes,
    getBucketStatisticsResponse_bucketCountByEncryptionType,
    getBucketStatisticsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetFindings
    getFindings_sortCriteria,
    getFindings_findingIds,
    getFindingsResponse_findings,
    getFindingsResponse_httpStatus,

    -- ** PutFindingsPublicationConfiguration
    putFindingsPublicationConfiguration_clientToken,
    putFindingsPublicationConfiguration_securityHubConfiguration,
    putFindingsPublicationConfigurationResponse_httpStatus,

    -- ** UntagResource
    untagResource_tagKeys,
    untagResource_resourceArn,
    untagResourceResponse_httpStatus,

    -- ** GetMacieSession
    getMacieSessionResponse_status,
    getMacieSessionResponse_createdAt,
    getMacieSessionResponse_findingPublishingFrequency,
    getMacieSessionResponse_updatedAt,
    getMacieSessionResponse_serviceRole,
    getMacieSessionResponse_httpStatus,

    -- ** GetFindingStatistics
    getFindingStatistics_size,
    getFindingStatistics_findingCriteria,
    getFindingStatistics_sortCriteria,
    getFindingStatistics_groupBy,
    getFindingStatisticsResponse_countsByGroup,
    getFindingStatisticsResponse_httpStatus,

    -- ** GetMember
    getMember_id,
    getMemberResponse_email,
    getMemberResponse_administratorAccountId,
    getMemberResponse_arn,
    getMemberResponse_relationshipStatus,
    getMemberResponse_masterAccountId,
    getMemberResponse_invitedAt,
    getMemberResponse_accountId,
    getMemberResponse_updatedAt,
    getMemberResponse_tags,
    getMemberResponse_httpStatus,

    -- ** DisassociateFromAdministratorAccount
    disassociateFromAdministratorAccountResponse_httpStatus,

    -- ** DeleteCustomDataIdentifier
    deleteCustomDataIdentifier_id,
    deleteCustomDataIdentifierResponse_httpStatus,

    -- ** DisableOrganizationAdminAccount
    disableOrganizationAdminAccount_adminAccountId,
    disableOrganizationAdminAccountResponse_httpStatus,

    -- ** ListCustomDataIdentifiers
    listCustomDataIdentifiers_nextToken,
    listCustomDataIdentifiers_maxResults,
    listCustomDataIdentifiersResponse_items,
    listCustomDataIdentifiersResponse_nextToken,
    listCustomDataIdentifiersResponse_httpStatus,

    -- * Types

    -- ** AccessControlList
    accessControlList_allowsPublicWriteAccess,
    accessControlList_allowsPublicReadAccess,

    -- ** AccountDetail
    accountDetail_email,
    accountDetail_accountId,

    -- ** AccountLevelPermissions
    accountLevelPermissions_blockPublicAccess,

    -- ** AdminAccount
    adminAccount_status,
    adminAccount_accountId,

    -- ** ApiCallDetails
    apiCallDetails_firstSeen,
    apiCallDetails_apiServiceName,
    apiCallDetails_lastSeen,
    apiCallDetails_api,

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
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_createdAt,
    batchGetCustomDataIdentifierSummary_name,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_description,

    -- ** BlockPublicAccess
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_blockPublicPolicy,

    -- ** BucketCountByEffectivePermission
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_unknown,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_publiclyWritable,

    -- ** BucketCountByEncryptionType
    bucketCountByEncryptionType_unknown,
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_kmsManaged,

    -- ** BucketCountBySharedAccessType
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_internal,
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_unknown,

    -- ** BucketCountPolicyAllowsUnencryptedObjectUploads
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,

    -- ** BucketCriteriaAdditionalProperties
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_prefix,
    bucketCriteriaAdditionalProperties_gt,
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_gte,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- ** BucketMetadata
    bucketMetadata_sizeInBytesCompressed,
    bucketMetadata_lastUpdated,
    bucketMetadata_objectCountByEncryptionType,
    bucketMetadata_sharedAccess,
    bucketMetadata_sizeInBytes,
    bucketMetadata_bucketName,
    bucketMetadata_classifiableObjectCount,
    bucketMetadata_accountId,
    bucketMetadata_unclassifiableObjectSizeInBytes,
    bucketMetadata_unclassifiableObjectCount,
    bucketMetadata_versioning,
    bucketMetadata_allowsUnencryptedObjectUploads,
    bucketMetadata_publicAccess,
    bucketMetadata_errorCode,
    bucketMetadata_jobDetails,
    bucketMetadata_region,
    bucketMetadata_bucketCreatedAt,
    bucketMetadata_errorMessage,
    bucketMetadata_bucketArn,
    bucketMetadata_objectCount,
    bucketMetadata_replicationDetails,
    bucketMetadata_serverSideEncryption,
    bucketMetadata_tags,
    bucketMetadata_classifiableSizeInBytes,

    -- ** BucketPermissionConfiguration
    bucketPermissionConfiguration_bucketLevelPermissions,
    bucketPermissionConfiguration_accountLevelPermissions,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicWriteAccess,
    bucketPolicy_allowsPublicReadAccess,

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
    cell_column,
    cell_columnName,

    -- ** ClassificationDetails
    classificationDetails_detailedResultsLocation,
    classificationDetails_jobId,
    classificationDetails_jobArn,
    classificationDetails_result,

    -- ** ClassificationExportConfiguration
    classificationExportConfiguration_s3Destination,

    -- ** ClassificationResult
    classificationResult_sensitiveData,
    classificationResult_status,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,

    -- ** ClassificationResultStatus
    classificationResultStatus_reason,
    classificationResultStatus_code,

    -- ** CriteriaBlockForJob
    criteriaBlockForJob_and,

    -- ** CriteriaForJob
    criteriaForJob_tagCriterion,
    criteriaForJob_simpleCriterion,

    -- ** CriterionAdditionalProperties
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_eqExactMatch,
    criterionAdditionalProperties_neq,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_gte,

    -- ** CustomDataIdentifierSummary
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_createdAt,
    customDataIdentifierSummary_name,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_description,

    -- ** CustomDataIdentifiers
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- ** CustomDetection
    customDetection_occurrences,
    customDetection_arn,
    customDetection_count,
    customDetection_name,

    -- ** DailySchedule

    -- ** DefaultDetection
    defaultDetection_occurrences,
    defaultDetection_count,
    defaultDetection_type,

    -- ** DomainDetails
    domainDetails_domainName,

    -- ** FederatedUser
    federatedUser_principalId,
    federatedUser_arn,
    federatedUser_sessionContext,
    federatedUser_accountId,
    federatedUser_accessKeyId,

    -- ** Finding
    finding_classificationDetails,
    finding_policyDetails,
    finding_createdAt,
    finding_category,
    finding_severity,
    finding_count,
    finding_schemaVersion,
    finding_resourcesAffected,
    finding_accountId,
    finding_partition,
    finding_id,
    finding_region,
    finding_updatedAt,
    finding_title,
    finding_type,
    finding_archived,
    finding_description,
    finding_sample,

    -- ** FindingAction
    findingAction_apiCallDetails,
    findingAction_actionType,

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
    findingsFilterListItem_arn,
    findingsFilterListItem_action,
    findingsFilterListItem_name,
    findingsFilterListItem_id,
    findingsFilterListItem_tags,

    -- ** GroupCount
    groupCount_groupKey,
    groupCount_count,

    -- ** IamUser
    iamUser_principalId,
    iamUser_arn,
    iamUser_userName,
    iamUser_accountId,

    -- ** Invitation
    invitation_relationshipStatus,
    invitation_invitedAt,
    invitation_invitationId,
    invitation_accountId,

    -- ** IpAddressDetails
    ipAddressDetails_ipCity,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipOwner,
    ipAddressDetails_ipCountry,

    -- ** IpCity
    ipCity_name,

    -- ** IpCountry
    ipCountry_name,
    ipCountry_code,

    -- ** IpGeoLocation
    ipGeoLocation_lat,
    ipGeoLocation_lon,

    -- ** IpOwner
    ipOwner_org,
    ipOwner_asnOrg,
    ipOwner_asn,
    ipOwner_isp,

    -- ** JobDetails
    jobDetails_isMonitoredByJob,
    jobDetails_isDefinedInJob,
    jobDetails_lastJobId,
    jobDetails_lastJobRunTime,

    -- ** JobScheduleFrequency
    jobScheduleFrequency_dailySchedule,
    jobScheduleFrequency_monthlySchedule,
    jobScheduleFrequency_weeklySchedule,

    -- ** JobScopeTerm
    jobScopeTerm_simpleScopeTerm,
    jobScopeTerm_tagScopeTerm,

    -- ** JobScopingBlock
    jobScopingBlock_and,

    -- ** JobSummary
    jobSummary_lastRunErrorStatus,
    jobSummary_jobType,
    jobSummary_jobId,
    jobSummary_createdAt,
    jobSummary_userPausedDetails,
    jobSummary_bucketCriteria,
    jobSummary_name,
    jobSummary_bucketDefinitions,
    jobSummary_jobStatus,

    -- ** KeyValuePair
    keyValuePair_value,
    keyValuePair_key,

    -- ** LastRunErrorStatus
    lastRunErrorStatus_code,

    -- ** ListJobsFilterCriteria
    listJobsFilterCriteria_includes,
    listJobsFilterCriteria_excludes,

    -- ** ListJobsFilterTerm
    listJobsFilterTerm_values,
    listJobsFilterTerm_key,
    listJobsFilterTerm_comparator,

    -- ** ListJobsSortCriteria
    listJobsSortCriteria_orderBy,
    listJobsSortCriteria_attributeName,

    -- ** ManagedDataIdentifierSummary
    managedDataIdentifierSummary_category,
    managedDataIdentifierSummary_id,

    -- ** MatchingBucket
    matchingBucket_sizeInBytesCompressed,
    matchingBucket_objectCountByEncryptionType,
    matchingBucket_sizeInBytes,
    matchingBucket_bucketName,
    matchingBucket_classifiableObjectCount,
    matchingBucket_accountId,
    matchingBucket_unclassifiableObjectSizeInBytes,
    matchingBucket_unclassifiableObjectCount,
    matchingBucket_errorCode,
    matchingBucket_jobDetails,
    matchingBucket_errorMessage,
    matchingBucket_objectCount,
    matchingBucket_classifiableSizeInBytes,

    -- ** MatchingResource
    matchingResource_matchingBucket,

    -- ** Member
    member_email,
    member_administratorAccountId,
    member_arn,
    member_relationshipStatus,
    member_masterAccountId,
    member_invitedAt,
    member_accountId,
    member_updatedAt,
    member_tags,

    -- ** MonthlySchedule
    monthlySchedule_dayOfMonth,

    -- ** ObjectCountByEncryptionType
    objectCountByEncryptionType_unknown,
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_customerManaged,

    -- ** ObjectLevelStatistics
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,
    objectLevelStatistics_total,

    -- ** Occurrences
    occurrences_lineRanges,
    occurrences_cells,
    occurrences_pages,
    occurrences_records,
    occurrences_offsetRanges,

    -- ** Page
    page_offsetRange,
    page_lineRange,
    page_pageNumber,

    -- ** PolicyDetails
    policyDetails_actor,
    policyDetails_action,

    -- ** Range
    range_start,
    range_end,
    range_startColumn,

    -- ** Record
    record_jsonPath,
    record_recordIndex,

    -- ** ReplicationDetails
    replicationDetails_replicated,
    replicationDetails_replicationAccounts,
    replicationDetails_replicatedExternally,

    -- ** ResourcesAffected
    resourcesAffected_s3Object,
    resourcesAffected_s3Bucket,

    -- ** S3Bucket
    s3Bucket_arn,
    s3Bucket_createdAt,
    s3Bucket_owner,
    s3Bucket_name,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_publicAccess,
    s3Bucket_tags,

    -- ** S3BucketCriteriaForJob
    s3BucketCriteriaForJob_includes,
    s3BucketCriteriaForJob_excludes,

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
    s3JobDefinition_scoping,
    s3JobDefinition_bucketCriteria,
    s3JobDefinition_bucketDefinitions,

    -- ** S3Object
    s3Object_eTag,
    s3Object_versionId,
    s3Object_path,
    s3Object_size,
    s3Object_extension,
    s3Object_key,
    s3Object_storageClass,
    s3Object_publicAccess,
    s3Object_lastModified,
    s3Object_bucketArn,
    s3Object_serverSideEncryption,
    s3Object_tags,

    -- ** Scoping
    scoping_includes,
    scoping_excludes,

    -- ** SearchResourcesBucketCriteria
    searchResourcesBucketCriteria_includes,
    searchResourcesBucketCriteria_excludes,

    -- ** SearchResourcesCriteria
    searchResourcesCriteria_tagCriterion,
    searchResourcesCriteria_simpleCriterion,

    -- ** SearchResourcesCriteriaBlock
    searchResourcesCriteriaBlock_and,

    -- ** SearchResourcesSimpleCriterion
    searchResourcesSimpleCriterion_values,
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_comparator,

    -- ** SearchResourcesSortCriteria
    searchResourcesSortCriteria_orderBy,
    searchResourcesSortCriteria_attributeName,

    -- ** SearchResourcesTagCriterion
    searchResourcesTagCriterion_tagValues,
    searchResourcesTagCriterion_comparator,

    -- ** SearchResourcesTagCriterionPair
    searchResourcesTagCriterionPair_value,
    searchResourcesTagCriterionPair_key,

    -- ** SecurityHubConfiguration
    securityHubConfiguration_publishPolicyFindings,
    securityHubConfiguration_publishClassificationFindings,

    -- ** SensitiveDataItem
    sensitiveDataItem_detections,
    sensitiveDataItem_category,
    sensitiveDataItem_totalCount,

    -- ** ServerSideEncryption
    serverSideEncryption_encryptionType,
    serverSideEncryption_kmsMasterKeyId,

    -- ** ServiceLimit
    serviceLimit_isServiceLimited,
    serviceLimit_value,
    serviceLimit_unit,

    -- ** SessionContext
    sessionContext_attributes,
    sessionContext_sessionIssuer,

    -- ** SessionContextAttributes
    sessionContextAttributes_creationDate,
    sessionContextAttributes_mfaAuthenticated,

    -- ** SessionIssuer
    sessionIssuer_principalId,
    sessionIssuer_arn,
    sessionIssuer_userName,
    sessionIssuer_accountId,
    sessionIssuer_type,

    -- ** Severity
    severity_score,
    severity_description,

    -- ** SimpleCriterionForJob
    simpleCriterionForJob_values,
    simpleCriterionForJob_key,
    simpleCriterionForJob_comparator,

    -- ** SimpleScopeTerm
    simpleScopeTerm_values,
    simpleScopeTerm_key,
    simpleScopeTerm_comparator,

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
    tagCriterionPairForJob_value,
    tagCriterionPairForJob_key,

    -- ** TagScopeTerm
    tagScopeTerm_tagValues,
    tagScopeTerm_key,
    tagScopeTerm_comparator,
    tagScopeTerm_target,

    -- ** TagValuePair
    tagValuePair_value,
    tagValuePair_key,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,
    unprocessedAccount_errorMessage,

    -- ** UsageByAccount
    usageByAccount_serviceLimit,
    usageByAccount_currency,
    usageByAccount_type,
    usageByAccount_estimatedCost,

    -- ** UsageRecord
    usageRecord_accountId,
    usageRecord_freeTrialStartDate,
    usageRecord_usage,

    -- ** UsageStatisticsFilter
    usageStatisticsFilter_values,
    usageStatisticsFilter_key,
    usageStatisticsFilter_comparator,

    -- ** UsageStatisticsSortBy
    usageStatisticsSortBy_orderBy,
    usageStatisticsSortBy_key,

    -- ** UsageTotal
    usageTotal_currency,
    usageTotal_type,
    usageTotal_estimatedCost,

    -- ** UserIdentity
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_awsAccount,
    userIdentity_assumedRole,
    userIdentity_federatedUser,
    userIdentity_awsService,
    userIdentity_type,

    -- ** UserIdentityRoot
    userIdentityRoot_principalId,
    userIdentityRoot_arn,
    userIdentityRoot_accountId,

    -- ** UserPausedDetails
    userPausedDetails_jobExpiresAt,
    userPausedDetails_jobImminentExpirationHealthEventArn,
    userPausedDetails_jobPausedAt,

    -- ** WeeklySchedule
    weeklySchedule_dayOfWeek,
  )
where

import Amazonka.MacieV2.AcceptInvitation
import Amazonka.MacieV2.BatchGetCustomDataIdentifiers
import Amazonka.MacieV2.CreateClassificationJob
import Amazonka.MacieV2.CreateCustomDataIdentifier
import Amazonka.MacieV2.CreateFindingsFilter
import Amazonka.MacieV2.CreateInvitations
import Amazonka.MacieV2.CreateMember
import Amazonka.MacieV2.CreateSampleFindings
import Amazonka.MacieV2.DeclineInvitations
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
import Amazonka.MacieV2.GetUsageStatistics
import Amazonka.MacieV2.GetUsageTotals
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
import Amazonka.MacieV2.Types.S3Bucket
import Amazonka.MacieV2.Types.S3BucketCriteriaForJob
import Amazonka.MacieV2.Types.S3BucketDefinitionForJob
import Amazonka.MacieV2.Types.S3BucketOwner
import Amazonka.MacieV2.Types.S3Destination
import Amazonka.MacieV2.Types.S3JobDefinition
import Amazonka.MacieV2.Types.S3Object
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
import Amazonka.MacieV2.UpdateClassificationJob
import Amazonka.MacieV2.UpdateFindingsFilter
import Amazonka.MacieV2.UpdateMacieSession
import Amazonka.MacieV2.UpdateMemberSession
import Amazonka.MacieV2.UpdateOrganizationConfiguration
