{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Lens
  ( -- * Operations

    -- ** AcceptInvitation
    acceptInvitation_administratorAccountId,
    acceptInvitation_masterAccount,
    acceptInvitation_invitationId,
    acceptInvitationResponse_httpStatus,

    -- ** BatchGetCustomDataIdentifiers
    batchGetCustomDataIdentifiers_ids,
    batchGetCustomDataIdentifiersResponse_customDataIdentifiers,
    batchGetCustomDataIdentifiersResponse_notFoundIdentifierIds,
    batchGetCustomDataIdentifiersResponse_httpStatus,

    -- ** CreateAllowList
    createAllowList_description,
    createAllowList_tags,
    createAllowList_criteria,
    createAllowList_clientToken,
    createAllowList_name,
    createAllowListResponse_arn,
    createAllowListResponse_id,
    createAllowListResponse_httpStatus,

    -- ** CreateClassificationJob
    createClassificationJob_allowListIds,
    createClassificationJob_customDataIdentifierIds,
    createClassificationJob_description,
    createClassificationJob_initialRun,
    createClassificationJob_managedDataIdentifierIds,
    createClassificationJob_managedDataIdentifierSelector,
    createClassificationJob_samplingPercentage,
    createClassificationJob_scheduleFrequency,
    createClassificationJob_tags,
    createClassificationJob_s3JobDefinition,
    createClassificationJob_jobType,
    createClassificationJob_clientToken,
    createClassificationJob_name,
    createClassificationJobResponse_jobArn,
    createClassificationJobResponse_jobId,
    createClassificationJobResponse_httpStatus,

    -- ** CreateCustomDataIdentifier
    createCustomDataIdentifier_clientToken,
    createCustomDataIdentifier_description,
    createCustomDataIdentifier_ignoreWords,
    createCustomDataIdentifier_keywords,
    createCustomDataIdentifier_maximumMatchDistance,
    createCustomDataIdentifier_severityLevels,
    createCustomDataIdentifier_tags,
    createCustomDataIdentifier_regex,
    createCustomDataIdentifier_name,
    createCustomDataIdentifierResponse_customDataIdentifierId,
    createCustomDataIdentifierResponse_httpStatus,

    -- ** CreateFindingsFilter
    createFindingsFilter_clientToken,
    createFindingsFilter_description,
    createFindingsFilter_position,
    createFindingsFilter_tags,
    createFindingsFilter_action,
    createFindingsFilter_findingCriteria,
    createFindingsFilter_name,
    createFindingsFilterResponse_arn,
    createFindingsFilterResponse_id,
    createFindingsFilterResponse_httpStatus,

    -- ** CreateInvitations
    createInvitations_disableEmailNotification,
    createInvitations_message,
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
    describeBuckets_criteria,
    describeBuckets_maxResults,
    describeBuckets_nextToken,
    describeBuckets_sortCriteria,
    describeBucketsResponse_buckets,
    describeBucketsResponse_nextToken,
    describeBucketsResponse_httpStatus,

    -- ** DescribeClassificationJob
    describeClassificationJob_jobId,
    describeClassificationJobResponse_allowListIds,
    describeClassificationJobResponse_clientToken,
    describeClassificationJobResponse_createdAt,
    describeClassificationJobResponse_customDataIdentifierIds,
    describeClassificationJobResponse_description,
    describeClassificationJobResponse_initialRun,
    describeClassificationJobResponse_jobArn,
    describeClassificationJobResponse_jobId,
    describeClassificationJobResponse_jobStatus,
    describeClassificationJobResponse_jobType,
    describeClassificationJobResponse_lastRunErrorStatus,
    describeClassificationJobResponse_lastRunTime,
    describeClassificationJobResponse_managedDataIdentifierIds,
    describeClassificationJobResponse_managedDataIdentifierSelector,
    describeClassificationJobResponse_name,
    describeClassificationJobResponse_s3JobDefinition,
    describeClassificationJobResponse_samplingPercentage,
    describeClassificationJobResponse_scheduleFrequency,
    describeClassificationJobResponse_statistics,
    describeClassificationJobResponse_tags,
    describeClassificationJobResponse_userPausedDetails,
    describeClassificationJobResponse_httpStatus,

    -- ** DescribeOrganizationConfiguration
    describeOrganizationConfigurationResponse_autoEnable,
    describeOrganizationConfigurationResponse_maxAccountLimitReached,
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
    enableMacie_findingPublishingFrequency,
    enableMacie_status,
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
    getAllowListResponse_arn,
    getAllowListResponse_createdAt,
    getAllowListResponse_criteria,
    getAllowListResponse_description,
    getAllowListResponse_id,
    getAllowListResponse_name,
    getAllowListResponse_status,
    getAllowListResponse_tags,
    getAllowListResponse_updatedAt,
    getAllowListResponse_httpStatus,

    -- ** GetAutomatedDiscoveryConfiguration
    getAutomatedDiscoveryConfigurationResponse_classificationScopeId,
    getAutomatedDiscoveryConfigurationResponse_disabledAt,
    getAutomatedDiscoveryConfigurationResponse_firstEnabledAt,
    getAutomatedDiscoveryConfigurationResponse_lastUpdatedAt,
    getAutomatedDiscoveryConfigurationResponse_sensitivityInspectionTemplateId,
    getAutomatedDiscoveryConfigurationResponse_status,
    getAutomatedDiscoveryConfigurationResponse_httpStatus,

    -- ** GetBucketStatistics
    getBucketStatistics_accountId,
    getBucketStatisticsResponse_bucketCount,
    getBucketStatisticsResponse_bucketCountByEffectivePermission,
    getBucketStatisticsResponse_bucketCountByEncryptionType,
    getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement,
    getBucketStatisticsResponse_bucketCountBySharedAccessType,
    getBucketStatisticsResponse_bucketStatisticsBySensitivity,
    getBucketStatisticsResponse_classifiableObjectCount,
    getBucketStatisticsResponse_classifiableSizeInBytes,
    getBucketStatisticsResponse_lastUpdated,
    getBucketStatisticsResponse_objectCount,
    getBucketStatisticsResponse_sizeInBytes,
    getBucketStatisticsResponse_sizeInBytesCompressed,
    getBucketStatisticsResponse_unclassifiableObjectCount,
    getBucketStatisticsResponse_unclassifiableObjectSizeInBytes,
    getBucketStatisticsResponse_httpStatus,

    -- ** GetClassificationExportConfiguration
    getClassificationExportConfigurationResponse_configuration,
    getClassificationExportConfigurationResponse_httpStatus,

    -- ** GetClassificationScope
    getClassificationScope_id,
    getClassificationScopeResponse_id,
    getClassificationScopeResponse_name,
    getClassificationScopeResponse_s3,
    getClassificationScopeResponse_httpStatus,

    -- ** GetCustomDataIdentifier
    getCustomDataIdentifier_id,
    getCustomDataIdentifierResponse_arn,
    getCustomDataIdentifierResponse_createdAt,
    getCustomDataIdentifierResponse_deleted,
    getCustomDataIdentifierResponse_description,
    getCustomDataIdentifierResponse_id,
    getCustomDataIdentifierResponse_ignoreWords,
    getCustomDataIdentifierResponse_keywords,
    getCustomDataIdentifierResponse_maximumMatchDistance,
    getCustomDataIdentifierResponse_name,
    getCustomDataIdentifierResponse_regex,
    getCustomDataIdentifierResponse_severityLevels,
    getCustomDataIdentifierResponse_tags,
    getCustomDataIdentifierResponse_httpStatus,

    -- ** GetFindingStatistics
    getFindingStatistics_findingCriteria,
    getFindingStatistics_size,
    getFindingStatistics_sortCriteria,
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
    getFindingsFilterResponse_action,
    getFindingsFilterResponse_arn,
    getFindingsFilterResponse_description,
    getFindingsFilterResponse_findingCriteria,
    getFindingsFilterResponse_id,
    getFindingsFilterResponse_name,
    getFindingsFilterResponse_position,
    getFindingsFilterResponse_tags,
    getFindingsFilterResponse_httpStatus,

    -- ** GetFindingsPublicationConfiguration
    getFindingsPublicationConfigurationResponse_securityHubConfiguration,
    getFindingsPublicationConfigurationResponse_httpStatus,

    -- ** GetInvitationsCount
    getInvitationsCountResponse_invitationsCount,
    getInvitationsCountResponse_httpStatus,

    -- ** GetMacieSession
    getMacieSessionResponse_createdAt,
    getMacieSessionResponse_findingPublishingFrequency,
    getMacieSessionResponse_serviceRole,
    getMacieSessionResponse_status,
    getMacieSessionResponse_updatedAt,
    getMacieSessionResponse_httpStatus,

    -- ** GetMasterAccount
    getMasterAccountResponse_master,
    getMasterAccountResponse_httpStatus,

    -- ** GetMember
    getMember_id,
    getMemberResponse_accountId,
    getMemberResponse_administratorAccountId,
    getMemberResponse_arn,
    getMemberResponse_email,
    getMemberResponse_invitedAt,
    getMemberResponse_masterAccountId,
    getMemberResponse_relationshipStatus,
    getMemberResponse_tags,
    getMemberResponse_updatedAt,
    getMemberResponse_httpStatus,

    -- ** GetResourceProfile
    getResourceProfile_resourceArn,
    getResourceProfileResponse_profileUpdatedAt,
    getResourceProfileResponse_sensitivityScore,
    getResourceProfileResponse_sensitivityScoreOverridden,
    getResourceProfileResponse_statistics,
    getResourceProfileResponse_httpStatus,

    -- ** GetRevealConfiguration
    getRevealConfigurationResponse_configuration,
    getRevealConfigurationResponse_httpStatus,

    -- ** GetSensitiveDataOccurrences
    getSensitiveDataOccurrences_findingId,
    getSensitiveDataOccurrencesResponse_error,
    getSensitiveDataOccurrencesResponse_sensitiveDataOccurrences,
    getSensitiveDataOccurrencesResponse_status,
    getSensitiveDataOccurrencesResponse_httpStatus,

    -- ** GetSensitiveDataOccurrencesAvailability
    getSensitiveDataOccurrencesAvailability_findingId,
    getSensitiveDataOccurrencesAvailabilityResponse_code,
    getSensitiveDataOccurrencesAvailabilityResponse_reasons,
    getSensitiveDataOccurrencesAvailabilityResponse_httpStatus,

    -- ** GetSensitivityInspectionTemplate
    getSensitivityInspectionTemplate_id,
    getSensitivityInspectionTemplateResponse_description,
    getSensitivityInspectionTemplateResponse_excludes,
    getSensitivityInspectionTemplateResponse_includes,
    getSensitivityInspectionTemplateResponse_name,
    getSensitivityInspectionTemplateResponse_sensitivityInspectionTemplateId,
    getSensitivityInspectionTemplateResponse_httpStatus,

    -- ** GetUsageStatistics
    getUsageStatistics_filterBy,
    getUsageStatistics_maxResults,
    getUsageStatistics_nextToken,
    getUsageStatistics_sortBy,
    getUsageStatistics_timeRange,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_records,
    getUsageStatisticsResponse_timeRange,
    getUsageStatisticsResponse_httpStatus,

    -- ** GetUsageTotals
    getUsageTotals_timeRange,
    getUsageTotalsResponse_timeRange,
    getUsageTotalsResponse_usageTotals,
    getUsageTotalsResponse_httpStatus,

    -- ** ListAllowLists
    listAllowLists_maxResults,
    listAllowLists_nextToken,
    listAllowListsResponse_allowLists,
    listAllowListsResponse_nextToken,
    listAllowListsResponse_httpStatus,

    -- ** ListClassificationJobs
    listClassificationJobs_filterCriteria,
    listClassificationJobs_maxResults,
    listClassificationJobs_nextToken,
    listClassificationJobs_sortCriteria,
    listClassificationJobsResponse_items,
    listClassificationJobsResponse_nextToken,
    listClassificationJobsResponse_httpStatus,

    -- ** ListClassificationScopes
    listClassificationScopes_name,
    listClassificationScopes_nextToken,
    listClassificationScopesResponse_classificationScopes,
    listClassificationScopesResponse_nextToken,
    listClassificationScopesResponse_httpStatus,

    -- ** ListCustomDataIdentifiers
    listCustomDataIdentifiers_maxResults,
    listCustomDataIdentifiers_nextToken,
    listCustomDataIdentifiersResponse_items,
    listCustomDataIdentifiersResponse_nextToken,
    listCustomDataIdentifiersResponse_httpStatus,

    -- ** ListFindings
    listFindings_findingCriteria,
    listFindings_maxResults,
    listFindings_nextToken,
    listFindings_sortCriteria,
    listFindingsResponse_findingIds,
    listFindingsResponse_nextToken,
    listFindingsResponse_httpStatus,

    -- ** ListFindingsFilters
    listFindingsFilters_maxResults,
    listFindingsFilters_nextToken,
    listFindingsFiltersResponse_findingsFilterListItems,
    listFindingsFiltersResponse_nextToken,
    listFindingsFiltersResponse_httpStatus,

    -- ** ListInvitations
    listInvitations_maxResults,
    listInvitations_nextToken,
    listInvitationsResponse_invitations,
    listInvitationsResponse_nextToken,
    listInvitationsResponse_httpStatus,

    -- ** ListManagedDataIdentifiers
    listManagedDataIdentifiers_nextToken,
    listManagedDataIdentifiersResponse_items,
    listManagedDataIdentifiersResponse_nextToken,
    listManagedDataIdentifiersResponse_httpStatus,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_onlyAssociated,
    listMembersResponse_members,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,

    -- ** ListOrganizationAdminAccounts
    listOrganizationAdminAccounts_maxResults,
    listOrganizationAdminAccounts_nextToken,
    listOrganizationAdminAccountsResponse_adminAccounts,
    listOrganizationAdminAccountsResponse_nextToken,
    listOrganizationAdminAccountsResponse_httpStatus,

    -- ** ListResourceProfileArtifacts
    listResourceProfileArtifacts_nextToken,
    listResourceProfileArtifacts_resourceArn,
    listResourceProfileArtifactsResponse_artifacts,
    listResourceProfileArtifactsResponse_nextToken,
    listResourceProfileArtifactsResponse_httpStatus,

    -- ** ListResourceProfileDetections
    listResourceProfileDetections_maxResults,
    listResourceProfileDetections_nextToken,
    listResourceProfileDetections_resourceArn,
    listResourceProfileDetectionsResponse_detections,
    listResourceProfileDetectionsResponse_nextToken,
    listResourceProfileDetectionsResponse_httpStatus,

    -- ** ListSensitivityInspectionTemplates
    listSensitivityInspectionTemplates_maxResults,
    listSensitivityInspectionTemplates_nextToken,
    listSensitivityInspectionTemplatesResponse_nextToken,
    listSensitivityInspectionTemplatesResponse_sensitivityInspectionTemplates,
    listSensitivityInspectionTemplatesResponse_httpStatus,

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
    searchResources_bucketCriteria,
    searchResources_maxResults,
    searchResources_nextToken,
    searchResources_sortCriteria,
    searchResourcesResponse_matchingResources,
    searchResourcesResponse_nextToken,
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

    -- ** UpdateAutomatedDiscoveryConfiguration
    updateAutomatedDiscoveryConfiguration_status,
    updateAutomatedDiscoveryConfigurationResponse_httpStatus,

    -- ** UpdateClassificationJob
    updateClassificationJob_jobId,
    updateClassificationJob_jobStatus,
    updateClassificationJobResponse_httpStatus,

    -- ** UpdateClassificationScope
    updateClassificationScope_s3,
    updateClassificationScope_id,
    updateClassificationScopeResponse_httpStatus,

    -- ** UpdateFindingsFilter
    updateFindingsFilter_action,
    updateFindingsFilter_clientToken,
    updateFindingsFilter_description,
    updateFindingsFilter_findingCriteria,
    updateFindingsFilter_name,
    updateFindingsFilter_position,
    updateFindingsFilter_id,
    updateFindingsFilterResponse_arn,
    updateFindingsFilterResponse_id,
    updateFindingsFilterResponse_httpStatus,

    -- ** UpdateMacieSession
    updateMacieSession_findingPublishingFrequency,
    updateMacieSession_status,
    updateMacieSessionResponse_httpStatus,

    -- ** UpdateMemberSession
    updateMemberSession_id,
    updateMemberSession_status,
    updateMemberSessionResponse_httpStatus,

    -- ** UpdateOrganizationConfiguration
    updateOrganizationConfiguration_autoEnable,
    updateOrganizationConfigurationResponse_httpStatus,

    -- ** UpdateResourceProfile
    updateResourceProfile_sensitivityScoreOverride,
    updateResourceProfile_resourceArn,
    updateResourceProfileResponse_httpStatus,

    -- ** UpdateResourceProfileDetections
    updateResourceProfileDetections_suppressDataIdentifiers,
    updateResourceProfileDetections_resourceArn,
    updateResourceProfileDetectionsResponse_httpStatus,

    -- ** UpdateRevealConfiguration
    updateRevealConfiguration_configuration,
    updateRevealConfigurationResponse_configuration,
    updateRevealConfigurationResponse_httpStatus,

    -- ** UpdateSensitivityInspectionTemplate
    updateSensitivityInspectionTemplate_description,
    updateSensitivityInspectionTemplate_excludes,
    updateSensitivityInspectionTemplate_includes,
    updateSensitivityInspectionTemplate_id,
    updateSensitivityInspectionTemplateResponse_httpStatus,

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
    adminAccount_accountId,
    adminAccount_status,

    -- ** AllowListCriteria
    allowListCriteria_regex,
    allowListCriteria_s3WordsList,

    -- ** AllowListStatus
    allowListStatus_description,
    allowListStatus_code,

    -- ** AllowListSummary
    allowListSummary_arn,
    allowListSummary_createdAt,
    allowListSummary_description,
    allowListSummary_id,
    allowListSummary_name,
    allowListSummary_updatedAt,

    -- ** ApiCallDetails
    apiCallDetails_api,
    apiCallDetails_apiServiceName,
    apiCallDetails_firstSeen,
    apiCallDetails_lastSeen,

    -- ** AssumedRole
    assumedRole_accessKeyId,
    assumedRole_accountId,
    assumedRole_arn,
    assumedRole_principalId,
    assumedRole_sessionContext,

    -- ** AwsAccount
    awsAccount_accountId,
    awsAccount_principalId,

    -- ** AwsService
    awsService_invokedBy,

    -- ** BatchGetCustomDataIdentifierSummary
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_createdAt,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_description,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_name,

    -- ** BlockPublicAccess
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_restrictPublicBuckets,

    -- ** BucketCountByEffectivePermission
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_publiclyWritable,
    bucketCountByEffectivePermission_unknown,

    -- ** BucketCountByEncryptionType
    bucketCountByEncryptionType_kmsManaged,
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_unknown,

    -- ** BucketCountBySharedAccessType
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_internal,
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_unknown,

    -- ** BucketCountPolicyAllowsUnencryptedObjectUploads
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,

    -- ** BucketCriteriaAdditionalProperties
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_gt,
    bucketCriteriaAdditionalProperties_gte,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_prefix,

    -- ** BucketLevelPermissions
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- ** BucketMetadata
    bucketMetadata_accountId,
    bucketMetadata_allowsUnencryptedObjectUploads,
    bucketMetadata_bucketArn,
    bucketMetadata_bucketCreatedAt,
    bucketMetadata_bucketName,
    bucketMetadata_classifiableObjectCount,
    bucketMetadata_classifiableSizeInBytes,
    bucketMetadata_errorCode,
    bucketMetadata_errorMessage,
    bucketMetadata_jobDetails,
    bucketMetadata_lastAutomatedDiscoveryTime,
    bucketMetadata_lastUpdated,
    bucketMetadata_objectCount,
    bucketMetadata_objectCountByEncryptionType,
    bucketMetadata_publicAccess,
    bucketMetadata_region,
    bucketMetadata_replicationDetails,
    bucketMetadata_sensitivityScore,
    bucketMetadata_serverSideEncryption,
    bucketMetadata_sharedAccess,
    bucketMetadata_sizeInBytes,
    bucketMetadata_sizeInBytesCompressed,
    bucketMetadata_tags,
    bucketMetadata_unclassifiableObjectCount,
    bucketMetadata_unclassifiableObjectSizeInBytes,
    bucketMetadata_versioning,

    -- ** BucketPermissionConfiguration
    bucketPermissionConfiguration_accountLevelPermissions,
    bucketPermissionConfiguration_bucketLevelPermissions,

    -- ** BucketPolicy
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- ** BucketPublicAccess
    bucketPublicAccess_effectivePermission,
    bucketPublicAccess_permissionConfiguration,

    -- ** BucketServerSideEncryption
    bucketServerSideEncryption_kmsMasterKeyId,
    bucketServerSideEncryption_type,

    -- ** BucketSortCriteria
    bucketSortCriteria_attributeName,
    bucketSortCriteria_orderBy,

    -- ** BucketStatisticsBySensitivity
    bucketStatisticsBySensitivity_classificationError,
    bucketStatisticsBySensitivity_notClassified,
    bucketStatisticsBySensitivity_notSensitive,
    bucketStatisticsBySensitivity_sensitive,

    -- ** Cell
    cell_cellReference,
    cell_column,
    cell_columnName,
    cell_row,

    -- ** ClassificationDetails
    classificationDetails_detailedResultsLocation,
    classificationDetails_jobArn,
    classificationDetails_jobId,
    classificationDetails_originType,
    classificationDetails_result,

    -- ** ClassificationExportConfiguration
    classificationExportConfiguration_s3Destination,

    -- ** ClassificationResult
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sensitiveData,
    classificationResult_sizeClassified,
    classificationResult_status,

    -- ** ClassificationResultStatus
    classificationResultStatus_code,
    classificationResultStatus_reason,

    -- ** ClassificationScopeSummary
    classificationScopeSummary_id,
    classificationScopeSummary_name,

    -- ** CriteriaBlockForJob
    criteriaBlockForJob_and,

    -- ** CriteriaForJob
    criteriaForJob_simpleCriterion,
    criteriaForJob_tagCriterion,

    -- ** CriterionAdditionalProperties
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_eqExactMatch,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_gte,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_neq,

    -- ** CustomDataIdentifierSummary
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_createdAt,
    customDataIdentifierSummary_description,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_name,

    -- ** CustomDataIdentifiers
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- ** CustomDetection
    customDetection_arn,
    customDetection_count,
    customDetection_name,
    customDetection_occurrences,

    -- ** DailySchedule

    -- ** DefaultDetection
    defaultDetection_count,
    defaultDetection_occurrences,
    defaultDetection_type,

    -- ** DetectedDataDetails
    detectedDataDetails_value,

    -- ** Detection
    detection_arn,
    detection_count,
    detection_id,
    detection_name,
    detection_suppressed,
    detection_type,

    -- ** DomainDetails
    domainDetails_domainName,

    -- ** FederatedUser
    federatedUser_accessKeyId,
    federatedUser_accountId,
    federatedUser_arn,
    federatedUser_principalId,
    federatedUser_sessionContext,

    -- ** Finding
    finding_accountId,
    finding_archived,
    finding_category,
    finding_classificationDetails,
    finding_count,
    finding_createdAt,
    finding_description,
    finding_id,
    finding_partition,
    finding_policyDetails,
    finding_region,
    finding_resourcesAffected,
    finding_sample,
    finding_schemaVersion,
    finding_severity,
    finding_title,
    finding_type,
    finding_updatedAt,

    -- ** FindingAction
    findingAction_actionType,
    findingAction_apiCallDetails,

    -- ** FindingActor
    findingActor_domainDetails,
    findingActor_ipAddressDetails,
    findingActor_userIdentity,

    -- ** FindingCriteria
    findingCriteria_criterion,

    -- ** FindingStatisticsSortCriteria
    findingStatisticsSortCriteria_attributeName,
    findingStatisticsSortCriteria_orderBy,

    -- ** FindingsFilterListItem
    findingsFilterListItem_action,
    findingsFilterListItem_arn,
    findingsFilterListItem_id,
    findingsFilterListItem_name,
    findingsFilterListItem_tags,

    -- ** GroupCount
    groupCount_count,
    groupCount_groupKey,

    -- ** IamUser
    iamUser_accountId,
    iamUser_arn,
    iamUser_principalId,
    iamUser_userName,

    -- ** Invitation
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_relationshipStatus,

    -- ** IpAddressDetails
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipCity,
    ipAddressDetails_ipCountry,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipOwner,

    -- ** IpCity
    ipCity_name,

    -- ** IpCountry
    ipCountry_code,
    ipCountry_name,

    -- ** IpGeoLocation
    ipGeoLocation_lat,
    ipGeoLocation_lon,

    -- ** IpOwner
    ipOwner_asn,
    ipOwner_asnOrg,
    ipOwner_isp,
    ipOwner_org,

    -- ** JobDetails
    jobDetails_isDefinedInJob,
    jobDetails_isMonitoredByJob,
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
    jobSummary_bucketCriteria,
    jobSummary_bucketDefinitions,
    jobSummary_createdAt,
    jobSummary_jobId,
    jobSummary_jobStatus,
    jobSummary_jobType,
    jobSummary_lastRunErrorStatus,
    jobSummary_name,
    jobSummary_userPausedDetails,

    -- ** KeyValuePair
    keyValuePair_key,
    keyValuePair_value,

    -- ** LastRunErrorStatus
    lastRunErrorStatus_code,

    -- ** ListJobsFilterCriteria
    listJobsFilterCriteria_excludes,
    listJobsFilterCriteria_includes,

    -- ** ListJobsFilterTerm
    listJobsFilterTerm_comparator,
    listJobsFilterTerm_key,
    listJobsFilterTerm_values,

    -- ** ListJobsSortCriteria
    listJobsSortCriteria_attributeName,
    listJobsSortCriteria_orderBy,

    -- ** ManagedDataIdentifierSummary
    managedDataIdentifierSummary_category,
    managedDataIdentifierSummary_id,

    -- ** MatchingBucket
    matchingBucket_accountId,
    matchingBucket_bucketName,
    matchingBucket_classifiableObjectCount,
    matchingBucket_classifiableSizeInBytes,
    matchingBucket_errorCode,
    matchingBucket_errorMessage,
    matchingBucket_jobDetails,
    matchingBucket_lastAutomatedDiscoveryTime,
    matchingBucket_objectCount,
    matchingBucket_objectCountByEncryptionType,
    matchingBucket_sensitivityScore,
    matchingBucket_sizeInBytes,
    matchingBucket_sizeInBytesCompressed,
    matchingBucket_unclassifiableObjectCount,
    matchingBucket_unclassifiableObjectSizeInBytes,

    -- ** MatchingResource
    matchingResource_matchingBucket,

    -- ** Member
    member_accountId,
    member_administratorAccountId,
    member_arn,
    member_email,
    member_invitedAt,
    member_masterAccountId,
    member_relationshipStatus,
    member_tags,
    member_updatedAt,

    -- ** MonthlySchedule
    monthlySchedule_dayOfMonth,

    -- ** ObjectCountByEncryptionType
    objectCountByEncryptionType_customerManaged,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_unknown,

    -- ** ObjectLevelStatistics
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,
    objectLevelStatistics_total,

    -- ** Occurrences
    occurrences_cells,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_pages,
    occurrences_records,

    -- ** Page
    page_lineRange,
    page_offsetRange,
    page_pageNumber,

    -- ** PolicyDetails
    policyDetails_action,
    policyDetails_actor,

    -- ** Range
    range_end,
    range_start,
    range_startColumn,

    -- ** Record
    record_jsonPath,
    record_recordIndex,

    -- ** ReplicationDetails
    replicationDetails_replicated,
    replicationDetails_replicatedExternally,
    replicationDetails_replicationAccounts,

    -- ** ResourceProfileArtifact
    resourceProfileArtifact_sensitive,
    resourceProfileArtifact_classificationResultStatus,
    resourceProfileArtifact_arn,

    -- ** ResourceStatistics
    resourceStatistics_totalBytesClassified,
    resourceStatistics_totalDetections,
    resourceStatistics_totalDetectionsSuppressed,
    resourceStatistics_totalItemsClassified,
    resourceStatistics_totalItemsSensitive,
    resourceStatistics_totalItemsSkipped,
    resourceStatistics_totalItemsSkippedInvalidEncryption,
    resourceStatistics_totalItemsSkippedInvalidKms,
    resourceStatistics_totalItemsSkippedPermissionDenied,

    -- ** ResourcesAffected
    resourcesAffected_s3Bucket,
    resourcesAffected_s3Object,

    -- ** RevealConfiguration
    revealConfiguration_kmsKeyId,
    revealConfiguration_status,

    -- ** S3Bucket
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_arn,
    s3Bucket_createdAt,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_name,
    s3Bucket_owner,
    s3Bucket_publicAccess,
    s3Bucket_tags,

    -- ** S3BucketCriteriaForJob
    s3BucketCriteriaForJob_excludes,
    s3BucketCriteriaForJob_includes,

    -- ** S3BucketDefinitionForJob
    s3BucketDefinitionForJob_accountId,
    s3BucketDefinitionForJob_buckets,

    -- ** S3BucketOwner
    s3BucketOwner_displayName,
    s3BucketOwner_id,

    -- ** S3ClassificationScope
    s3ClassificationScope_excludes,

    -- ** S3ClassificationScopeExclusion
    s3ClassificationScopeExclusion_bucketNames,

    -- ** S3ClassificationScopeExclusionUpdate
    s3ClassificationScopeExclusionUpdate_bucketNames,
    s3ClassificationScopeExclusionUpdate_operation,

    -- ** S3ClassificationScopeUpdate
    s3ClassificationScopeUpdate_excludes,

    -- ** S3Destination
    s3Destination_keyPrefix,
    s3Destination_bucketName,
    s3Destination_kmsKeyArn,

    -- ** S3JobDefinition
    s3JobDefinition_bucketCriteria,
    s3JobDefinition_bucketDefinitions,
    s3JobDefinition_scoping,

    -- ** S3Object
    s3Object_bucketArn,
    s3Object_eTag,
    s3Object_extension,
    s3Object_key,
    s3Object_lastModified,
    s3Object_path,
    s3Object_publicAccess,
    s3Object_serverSideEncryption,
    s3Object_size,
    s3Object_storageClass,
    s3Object_tags,
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
    searchResourcesCriteria_simpleCriterion,
    searchResourcesCriteria_tagCriterion,

    -- ** SearchResourcesCriteriaBlock
    searchResourcesCriteriaBlock_and,

    -- ** SearchResourcesSimpleCriterion
    searchResourcesSimpleCriterion_comparator,
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_values,

    -- ** SearchResourcesSortCriteria
    searchResourcesSortCriteria_attributeName,
    searchResourcesSortCriteria_orderBy,

    -- ** SearchResourcesTagCriterion
    searchResourcesTagCriterion_comparator,
    searchResourcesTagCriterion_tagValues,

    -- ** SearchResourcesTagCriterionPair
    searchResourcesTagCriterionPair_key,
    searchResourcesTagCriterionPair_value,

    -- ** SecurityHubConfiguration
    securityHubConfiguration_publishPolicyFindings,
    securityHubConfiguration_publishClassificationFindings,

    -- ** SensitiveDataItem
    sensitiveDataItem_category,
    sensitiveDataItem_detections,
    sensitiveDataItem_totalCount,

    -- ** SensitivityAggregations
    sensitivityAggregations_classifiableSizeInBytes,
    sensitivityAggregations_publiclyAccessibleCount,
    sensitivityAggregations_totalCount,
    sensitivityAggregations_totalSizeInBytes,

    -- ** SensitivityInspectionTemplateExcludes
    sensitivityInspectionTemplateExcludes_managedDataIdentifierIds,

    -- ** SensitivityInspectionTemplateIncludes
    sensitivityInspectionTemplateIncludes_allowListIds,
    sensitivityInspectionTemplateIncludes_customDataIdentifierIds,
    sensitivityInspectionTemplateIncludes_managedDataIdentifierIds,

    -- ** SensitivityInspectionTemplatesEntry
    sensitivityInspectionTemplatesEntry_id,
    sensitivityInspectionTemplatesEntry_name,

    -- ** ServerSideEncryption
    serverSideEncryption_encryptionType,
    serverSideEncryption_kmsMasterKeyId,

    -- ** ServiceLimit
    serviceLimit_isServiceLimited,
    serviceLimit_unit,
    serviceLimit_value,

    -- ** SessionContext
    sessionContext_attributes,
    sessionContext_sessionIssuer,

    -- ** SessionContextAttributes
    sessionContextAttributes_creationDate,
    sessionContextAttributes_mfaAuthenticated,

    -- ** SessionIssuer
    sessionIssuer_accountId,
    sessionIssuer_arn,
    sessionIssuer_principalId,
    sessionIssuer_type,
    sessionIssuer_userName,

    -- ** Severity
    severity_description,
    severity_score,

    -- ** SeverityLevel
    severityLevel_occurrencesThreshold,
    severityLevel_severity,

    -- ** SimpleCriterionForJob
    simpleCriterionForJob_comparator,
    simpleCriterionForJob_key,
    simpleCriterionForJob_values,

    -- ** SimpleScopeTerm
    simpleScopeTerm_comparator,
    simpleScopeTerm_key,
    simpleScopeTerm_values,

    -- ** SortCriteria
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- ** Statistics
    statistics_approximateNumberOfObjectsToProcess,
    statistics_numberOfRuns,

    -- ** SuppressDataIdentifier
    suppressDataIdentifier_id,
    suppressDataIdentifier_type,

    -- ** TagCriterionForJob
    tagCriterionForJob_comparator,
    tagCriterionForJob_tagValues,

    -- ** TagCriterionPairForJob
    tagCriterionPairForJob_key,
    tagCriterionPairForJob_value,

    -- ** TagScopeTerm
    tagScopeTerm_comparator,
    tagScopeTerm_key,
    tagScopeTerm_tagValues,
    tagScopeTerm_target,

    -- ** TagValuePair
    tagValuePair_key,
    tagValuePair_value,

    -- ** UnprocessedAccount
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,
    unprocessedAccount_errorMessage,

    -- ** UsageByAccount
    usageByAccount_currency,
    usageByAccount_estimatedCost,
    usageByAccount_serviceLimit,
    usageByAccount_type,

    -- ** UsageRecord
    usageRecord_accountId,
    usageRecord_automatedDiscoveryFreeTrialStartDate,
    usageRecord_freeTrialStartDate,
    usageRecord_usage,

    -- ** UsageStatisticsFilter
    usageStatisticsFilter_comparator,
    usageStatisticsFilter_key,
    usageStatisticsFilter_values,

    -- ** UsageStatisticsSortBy
    usageStatisticsSortBy_key,
    usageStatisticsSortBy_orderBy,

    -- ** UsageTotal
    usageTotal_currency,
    usageTotal_estimatedCost,
    usageTotal_type,

    -- ** UserIdentity
    userIdentity_assumedRole,
    userIdentity_awsAccount,
    userIdentity_awsService,
    userIdentity_federatedUser,
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_type,

    -- ** UserIdentityRoot
    userIdentityRoot_accountId,
    userIdentityRoot_arn,
    userIdentityRoot_principalId,

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
import Amazonka.MacieV2.GetAutomatedDiscoveryConfiguration
import Amazonka.MacieV2.GetBucketStatistics
import Amazonka.MacieV2.GetClassificationExportConfiguration
import Amazonka.MacieV2.GetClassificationScope
import Amazonka.MacieV2.GetCustomDataIdentifier
import Amazonka.MacieV2.GetFindingStatistics
import Amazonka.MacieV2.GetFindings
import Amazonka.MacieV2.GetFindingsFilter
import Amazonka.MacieV2.GetFindingsPublicationConfiguration
import Amazonka.MacieV2.GetInvitationsCount
import Amazonka.MacieV2.GetMacieSession
import Amazonka.MacieV2.GetMasterAccount
import Amazonka.MacieV2.GetMember
import Amazonka.MacieV2.GetResourceProfile
import Amazonka.MacieV2.GetRevealConfiguration
import Amazonka.MacieV2.GetSensitiveDataOccurrences
import Amazonka.MacieV2.GetSensitiveDataOccurrencesAvailability
import Amazonka.MacieV2.GetSensitivityInspectionTemplate
import Amazonka.MacieV2.GetUsageStatistics
import Amazonka.MacieV2.GetUsageTotals
import Amazonka.MacieV2.ListAllowLists
import Amazonka.MacieV2.ListClassificationJobs
import Amazonka.MacieV2.ListClassificationScopes
import Amazonka.MacieV2.ListCustomDataIdentifiers
import Amazonka.MacieV2.ListFindings
import Amazonka.MacieV2.ListFindingsFilters
import Amazonka.MacieV2.ListInvitations
import Amazonka.MacieV2.ListManagedDataIdentifiers
import Amazonka.MacieV2.ListMembers
import Amazonka.MacieV2.ListOrganizationAdminAccounts
import Amazonka.MacieV2.ListResourceProfileArtifacts
import Amazonka.MacieV2.ListResourceProfileDetections
import Amazonka.MacieV2.ListSensitivityInspectionTemplates
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
import Amazonka.MacieV2.Types.BucketStatisticsBySensitivity
import Amazonka.MacieV2.Types.Cell
import Amazonka.MacieV2.Types.ClassificationDetails
import Amazonka.MacieV2.Types.ClassificationExportConfiguration
import Amazonka.MacieV2.Types.ClassificationResult
import Amazonka.MacieV2.Types.ClassificationResultStatus
import Amazonka.MacieV2.Types.ClassificationScopeSummary
import Amazonka.MacieV2.Types.CriteriaBlockForJob
import Amazonka.MacieV2.Types.CriteriaForJob
import Amazonka.MacieV2.Types.CriterionAdditionalProperties
import Amazonka.MacieV2.Types.CustomDataIdentifierSummary
import Amazonka.MacieV2.Types.CustomDataIdentifiers
import Amazonka.MacieV2.Types.CustomDetection
import Amazonka.MacieV2.Types.DailySchedule
import Amazonka.MacieV2.Types.DefaultDetection
import Amazonka.MacieV2.Types.DetectedDataDetails
import Amazonka.MacieV2.Types.Detection
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
import Amazonka.MacieV2.Types.ResourceProfileArtifact
import Amazonka.MacieV2.Types.ResourceStatistics
import Amazonka.MacieV2.Types.ResourcesAffected
import Amazonka.MacieV2.Types.RevealConfiguration
import Amazonka.MacieV2.Types.S3Bucket
import Amazonka.MacieV2.Types.S3BucketCriteriaForJob
import Amazonka.MacieV2.Types.S3BucketDefinitionForJob
import Amazonka.MacieV2.Types.S3BucketOwner
import Amazonka.MacieV2.Types.S3ClassificationScope
import Amazonka.MacieV2.Types.S3ClassificationScopeExclusion
import Amazonka.MacieV2.Types.S3ClassificationScopeExclusionUpdate
import Amazonka.MacieV2.Types.S3ClassificationScopeUpdate
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
import Amazonka.MacieV2.Types.SensitivityAggregations
import Amazonka.MacieV2.Types.SensitivityInspectionTemplateExcludes
import Amazonka.MacieV2.Types.SensitivityInspectionTemplateIncludes
import Amazonka.MacieV2.Types.SensitivityInspectionTemplatesEntry
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
import Amazonka.MacieV2.Types.SuppressDataIdentifier
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
import Amazonka.MacieV2.UpdateAutomatedDiscoveryConfiguration
import Amazonka.MacieV2.UpdateClassificationJob
import Amazonka.MacieV2.UpdateClassificationScope
import Amazonka.MacieV2.UpdateFindingsFilter
import Amazonka.MacieV2.UpdateMacieSession
import Amazonka.MacieV2.UpdateMemberSession
import Amazonka.MacieV2.UpdateOrganizationConfiguration
import Amazonka.MacieV2.UpdateResourceProfile
import Amazonka.MacieV2.UpdateResourceProfileDetections
import Amazonka.MacieV2.UpdateRevealConfiguration
import Amazonka.MacieV2.UpdateSensitivityInspectionTemplate
