{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MacieV2.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AdminStatus
    AdminStatus (..),

    -- * AllowsUnencryptedObjectUploads
    AllowsUnencryptedObjectUploads (..),

    -- * BucketMetadataErrorCode
    BucketMetadataErrorCode (..),

    -- * Currency
    Currency (..),

    -- * DayOfWeek
    DayOfWeek (..),

    -- * EffectivePermission
    EffectivePermission (..),

    -- * EncryptionType
    EncryptionType (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * FindingActionType
    FindingActionType (..),

    -- * FindingCategory
    FindingCategory (..),

    -- * FindingPublishingFrequency
    FindingPublishingFrequency (..),

    -- * FindingStatisticsSortAttributeName
    FindingStatisticsSortAttributeName (..),

    -- * FindingType
    FindingType (..),

    -- * FindingsFilterAction
    FindingsFilterAction (..),

    -- * GroupBy
    GroupBy (..),

    -- * IsDefinedInJob
    IsDefinedInJob (..),

    -- * IsMonitoredByJob
    IsMonitoredByJob (..),

    -- * JobComparator
    JobComparator (..),

    -- * JobStatus
    JobStatus (..),

    -- * JobType
    JobType (..),

    -- * LastRunErrorStatusCode
    LastRunErrorStatusCode (..),

    -- * ListJobsFilterKey
    ListJobsFilterKey (..),

    -- * ListJobsSortAttributeName
    ListJobsSortAttributeName (..),

    -- * MacieStatus
    MacieStatus (..),

    -- * ManagedDataIdentifierSelector
    ManagedDataIdentifierSelector (..),

    -- * OrderBy
    OrderBy (..),

    -- * RelationshipStatus
    RelationshipStatus (..),

    -- * ScopeFilterKey
    ScopeFilterKey (..),

    -- * SearchResourcesComparator
    SearchResourcesComparator (..),

    -- * SearchResourcesSimpleCriterionKey
    SearchResourcesSimpleCriterionKey (..),

    -- * SearchResourcesSortAttributeName
    SearchResourcesSortAttributeName (..),

    -- * SensitiveDataItemCategory
    SensitiveDataItemCategory (..),

    -- * SeverityDescription
    SeverityDescription (..),

    -- * SharedAccess
    SharedAccess (..),

    -- * SimpleCriterionKeyForJob
    SimpleCriterionKeyForJob (..),

    -- * StorageClass
    StorageClass (..),

    -- * TagTarget
    TagTarget (..),

    -- * TimeRange
    TimeRange (..),

    -- * Type
    Type (..),

    -- * Unit
    Unit (..),

    -- * UsageStatisticsFilterComparator
    UsageStatisticsFilterComparator (..),

    -- * UsageStatisticsFilterKey
    UsageStatisticsFilterKey (..),

    -- * UsageStatisticsSortKey
    UsageStatisticsSortKey (..),

    -- * UsageType
    UsageType (..),

    -- * UserIdentityType
    UserIdentityType (..),

    -- * AccessControlList
    AccessControlList (..),
    newAccessControlList,
    accessControlList_allowsPublicWriteAccess,
    accessControlList_allowsPublicReadAccess,

    -- * AccountDetail
    AccountDetail (..),
    newAccountDetail,
    accountDetail_email,
    accountDetail_accountId,

    -- * AccountLevelPermissions
    AccountLevelPermissions (..),
    newAccountLevelPermissions,
    accountLevelPermissions_blockPublicAccess,

    -- * AdminAccount
    AdminAccount (..),
    newAdminAccount,
    adminAccount_status,
    adminAccount_accountId,

    -- * ApiCallDetails
    ApiCallDetails (..),
    newApiCallDetails,
    apiCallDetails_firstSeen,
    apiCallDetails_apiServiceName,
    apiCallDetails_lastSeen,
    apiCallDetails_api,

    -- * AssumedRole
    AssumedRole (..),
    newAssumedRole,
    assumedRole_principalId,
    assumedRole_arn,
    assumedRole_sessionContext,
    assumedRole_accountId,
    assumedRole_accessKeyId,

    -- * AwsAccount
    AwsAccount (..),
    newAwsAccount,
    awsAccount_principalId,
    awsAccount_accountId,

    -- * AwsService
    AwsService (..),
    newAwsService,
    awsService_invokedBy,

    -- * BatchGetCustomDataIdentifierSummary
    BatchGetCustomDataIdentifierSummary (..),
    newBatchGetCustomDataIdentifierSummary,
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_createdAt,
    batchGetCustomDataIdentifierSummary_name,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_description,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_blockPublicPolicy,

    -- * BucketCountByEffectivePermission
    BucketCountByEffectivePermission (..),
    newBucketCountByEffectivePermission,
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_unknown,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_publiclyWritable,

    -- * BucketCountByEncryptionType
    BucketCountByEncryptionType (..),
    newBucketCountByEncryptionType,
    bucketCountByEncryptionType_unknown,
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_kmsManaged,

    -- * BucketCountBySharedAccessType
    BucketCountBySharedAccessType (..),
    newBucketCountBySharedAccessType,
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_internal,
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_unknown,

    -- * BucketCountPolicyAllowsUnencryptedObjectUploads
    BucketCountPolicyAllowsUnencryptedObjectUploads (..),
    newBucketCountPolicyAllowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,

    -- * BucketCriteriaAdditionalProperties
    BucketCriteriaAdditionalProperties (..),
    newBucketCriteriaAdditionalProperties,
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_prefix,
    bucketCriteriaAdditionalProperties_gt,
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_gte,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    newBucketLevelPermissions,
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- * BucketMetadata
    BucketMetadata (..),
    newBucketMetadata,
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

    -- * BucketPermissionConfiguration
    BucketPermissionConfiguration (..),
    newBucketPermissionConfiguration,
    bucketPermissionConfiguration_bucketLevelPermissions,
    bucketPermissionConfiguration_accountLevelPermissions,

    -- * BucketPolicy
    BucketPolicy (..),
    newBucketPolicy,
    bucketPolicy_allowsPublicWriteAccess,
    bucketPolicy_allowsPublicReadAccess,

    -- * BucketPublicAccess
    BucketPublicAccess (..),
    newBucketPublicAccess,
    bucketPublicAccess_permissionConfiguration,
    bucketPublicAccess_effectivePermission,

    -- * BucketServerSideEncryption
    BucketServerSideEncryption (..),
    newBucketServerSideEncryption,
    bucketServerSideEncryption_kmsMasterKeyId,
    bucketServerSideEncryption_type,

    -- * BucketSortCriteria
    BucketSortCriteria (..),
    newBucketSortCriteria,
    bucketSortCriteria_orderBy,
    bucketSortCriteria_attributeName,

    -- * Cell
    Cell (..),
    newCell,
    cell_row,
    cell_cellReference,
    cell_column,
    cell_columnName,

    -- * ClassificationDetails
    ClassificationDetails (..),
    newClassificationDetails,
    classificationDetails_detailedResultsLocation,
    classificationDetails_jobId,
    classificationDetails_jobArn,
    classificationDetails_result,

    -- * ClassificationExportConfiguration
    ClassificationExportConfiguration (..),
    newClassificationExportConfiguration,
    classificationExportConfiguration_s3Destination,

    -- * ClassificationResult
    ClassificationResult (..),
    newClassificationResult,
    classificationResult_sensitiveData,
    classificationResult_status,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,

    -- * ClassificationResultStatus
    ClassificationResultStatus (..),
    newClassificationResultStatus,
    classificationResultStatus_reason,
    classificationResultStatus_code,

    -- * CriteriaBlockForJob
    CriteriaBlockForJob (..),
    newCriteriaBlockForJob,
    criteriaBlockForJob_and,

    -- * CriteriaForJob
    CriteriaForJob (..),
    newCriteriaForJob,
    criteriaForJob_tagCriterion,
    criteriaForJob_simpleCriterion,

    -- * CriterionAdditionalProperties
    CriterionAdditionalProperties (..),
    newCriterionAdditionalProperties,
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_eqExactMatch,
    criterionAdditionalProperties_neq,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_gte,

    -- * CustomDataIdentifierSummary
    CustomDataIdentifierSummary (..),
    newCustomDataIdentifierSummary,
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_createdAt,
    customDataIdentifierSummary_name,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_description,

    -- * CustomDataIdentifiers
    CustomDataIdentifiers (..),
    newCustomDataIdentifiers,
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- * CustomDetection
    CustomDetection (..),
    newCustomDetection,
    customDetection_occurrences,
    customDetection_arn,
    customDetection_count,
    customDetection_name,

    -- * DailySchedule
    DailySchedule (..),
    newDailySchedule,

    -- * DefaultDetection
    DefaultDetection (..),
    newDefaultDetection,
    defaultDetection_occurrences,
    defaultDetection_count,
    defaultDetection_type,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domainName,

    -- * FederatedUser
    FederatedUser (..),
    newFederatedUser,
    federatedUser_principalId,
    federatedUser_arn,
    federatedUser_sessionContext,
    federatedUser_accountId,
    federatedUser_accessKeyId,

    -- * Finding
    Finding (..),
    newFinding,
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

    -- * FindingAction
    FindingAction (..),
    newFindingAction,
    findingAction_apiCallDetails,
    findingAction_actionType,

    -- * FindingActor
    FindingActor (..),
    newFindingActor,
    findingActor_userIdentity,
    findingActor_ipAddressDetails,
    findingActor_domainDetails,

    -- * FindingCriteria
    FindingCriteria (..),
    newFindingCriteria,
    findingCriteria_criterion,

    -- * FindingStatisticsSortCriteria
    FindingStatisticsSortCriteria (..),
    newFindingStatisticsSortCriteria,
    findingStatisticsSortCriteria_orderBy,
    findingStatisticsSortCriteria_attributeName,

    -- * FindingsFilterListItem
    FindingsFilterListItem (..),
    newFindingsFilterListItem,
    findingsFilterListItem_arn,
    findingsFilterListItem_action,
    findingsFilterListItem_name,
    findingsFilterListItem_id,
    findingsFilterListItem_tags,

    -- * GroupCount
    GroupCount (..),
    newGroupCount,
    groupCount_groupKey,
    groupCount_count,

    -- * IamUser
    IamUser (..),
    newIamUser,
    iamUser_principalId,
    iamUser_arn,
    iamUser_userName,
    iamUser_accountId,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_relationshipStatus,
    invitation_invitedAt,
    invitation_invitationId,
    invitation_accountId,

    -- * IpAddressDetails
    IpAddressDetails (..),
    newIpAddressDetails,
    ipAddressDetails_ipCity,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipOwner,
    ipAddressDetails_ipCountry,

    -- * IpCity
    IpCity (..),
    newIpCity,
    ipCity_name,

    -- * IpCountry
    IpCountry (..),
    newIpCountry,
    ipCountry_name,
    ipCountry_code,

    -- * IpGeoLocation
    IpGeoLocation (..),
    newIpGeoLocation,
    ipGeoLocation_lat,
    ipGeoLocation_lon,

    -- * IpOwner
    IpOwner (..),
    newIpOwner,
    ipOwner_org,
    ipOwner_asnOrg,
    ipOwner_asn,
    ipOwner_isp,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_isMonitoredByJob,
    jobDetails_isDefinedInJob,
    jobDetails_lastJobId,
    jobDetails_lastJobRunTime,

    -- * JobScheduleFrequency
    JobScheduleFrequency (..),
    newJobScheduleFrequency,
    jobScheduleFrequency_dailySchedule,
    jobScheduleFrequency_monthlySchedule,
    jobScheduleFrequency_weeklySchedule,

    -- * JobScopeTerm
    JobScopeTerm (..),
    newJobScopeTerm,
    jobScopeTerm_simpleScopeTerm,
    jobScopeTerm_tagScopeTerm,

    -- * JobScopingBlock
    JobScopingBlock (..),
    newJobScopingBlock,
    jobScopingBlock_and,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_lastRunErrorStatus,
    jobSummary_jobType,
    jobSummary_jobId,
    jobSummary_createdAt,
    jobSummary_userPausedDetails,
    jobSummary_bucketCriteria,
    jobSummary_name,
    jobSummary_bucketDefinitions,
    jobSummary_jobStatus,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_value,
    keyValuePair_key,

    -- * LastRunErrorStatus
    LastRunErrorStatus (..),
    newLastRunErrorStatus,
    lastRunErrorStatus_code,

    -- * ListJobsFilterCriteria
    ListJobsFilterCriteria (..),
    newListJobsFilterCriteria,
    listJobsFilterCriteria_includes,
    listJobsFilterCriteria_excludes,

    -- * ListJobsFilterTerm
    ListJobsFilterTerm (..),
    newListJobsFilterTerm,
    listJobsFilterTerm_values,
    listJobsFilterTerm_key,
    listJobsFilterTerm_comparator,

    -- * ListJobsSortCriteria
    ListJobsSortCriteria (..),
    newListJobsSortCriteria,
    listJobsSortCriteria_orderBy,
    listJobsSortCriteria_attributeName,

    -- * ManagedDataIdentifierSummary
    ManagedDataIdentifierSummary (..),
    newManagedDataIdentifierSummary,
    managedDataIdentifierSummary_category,
    managedDataIdentifierSummary_id,

    -- * MatchingBucket
    MatchingBucket (..),
    newMatchingBucket,
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

    -- * MatchingResource
    MatchingResource (..),
    newMatchingResource,
    matchingResource_matchingBucket,

    -- * Member
    Member (..),
    newMember,
    member_email,
    member_administratorAccountId,
    member_arn,
    member_relationshipStatus,
    member_masterAccountId,
    member_invitedAt,
    member_accountId,
    member_updatedAt,
    member_tags,

    -- * MonthlySchedule
    MonthlySchedule (..),
    newMonthlySchedule,
    monthlySchedule_dayOfMonth,

    -- * ObjectCountByEncryptionType
    ObjectCountByEncryptionType (..),
    newObjectCountByEncryptionType,
    objectCountByEncryptionType_unknown,
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_customerManaged,

    -- * ObjectLevelStatistics
    ObjectLevelStatistics (..),
    newObjectLevelStatistics,
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,
    objectLevelStatistics_total,

    -- * Occurrences
    Occurrences (..),
    newOccurrences,
    occurrences_lineRanges,
    occurrences_cells,
    occurrences_pages,
    occurrences_records,
    occurrences_offsetRanges,

    -- * Page
    Page (..),
    newPage,
    page_offsetRange,
    page_lineRange,
    page_pageNumber,

    -- * PolicyDetails
    PolicyDetails (..),
    newPolicyDetails,
    policyDetails_actor,
    policyDetails_action,

    -- * Range
    Range (..),
    newRange,
    range_start,
    range_end,
    range_startColumn,

    -- * Record
    Record (..),
    newRecord,
    record_jsonPath,
    record_recordIndex,

    -- * ReplicationDetails
    ReplicationDetails (..),
    newReplicationDetails,
    replicationDetails_replicated,
    replicationDetails_replicationAccounts,
    replicationDetails_replicatedExternally,

    -- * ResourcesAffected
    ResourcesAffected (..),
    newResourcesAffected,
    resourcesAffected_s3Object,
    resourcesAffected_s3Bucket,

    -- * S3Bucket
    S3Bucket (..),
    newS3Bucket,
    s3Bucket_arn,
    s3Bucket_createdAt,
    s3Bucket_owner,
    s3Bucket_name,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_publicAccess,
    s3Bucket_tags,

    -- * S3BucketCriteriaForJob
    S3BucketCriteriaForJob (..),
    newS3BucketCriteriaForJob,
    s3BucketCriteriaForJob_includes,
    s3BucketCriteriaForJob_excludes,

    -- * S3BucketDefinitionForJob
    S3BucketDefinitionForJob (..),
    newS3BucketDefinitionForJob,
    s3BucketDefinitionForJob_accountId,
    s3BucketDefinitionForJob_buckets,

    -- * S3BucketOwner
    S3BucketOwner (..),
    newS3BucketOwner,
    s3BucketOwner_displayName,
    s3BucketOwner_id,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_keyPrefix,
    s3Destination_bucketName,
    s3Destination_kmsKeyArn,

    -- * S3JobDefinition
    S3JobDefinition (..),
    newS3JobDefinition,
    s3JobDefinition_scoping,
    s3JobDefinition_bucketCriteria,
    s3JobDefinition_bucketDefinitions,

    -- * S3Object
    S3Object (..),
    newS3Object,
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

    -- * Scoping
    Scoping (..),
    newScoping,
    scoping_includes,
    scoping_excludes,

    -- * SearchResourcesBucketCriteria
    SearchResourcesBucketCriteria (..),
    newSearchResourcesBucketCriteria,
    searchResourcesBucketCriteria_includes,
    searchResourcesBucketCriteria_excludes,

    -- * SearchResourcesCriteria
    SearchResourcesCriteria (..),
    newSearchResourcesCriteria,
    searchResourcesCriteria_tagCriterion,
    searchResourcesCriteria_simpleCriterion,

    -- * SearchResourcesCriteriaBlock
    SearchResourcesCriteriaBlock (..),
    newSearchResourcesCriteriaBlock,
    searchResourcesCriteriaBlock_and,

    -- * SearchResourcesSimpleCriterion
    SearchResourcesSimpleCriterion (..),
    newSearchResourcesSimpleCriterion,
    searchResourcesSimpleCriterion_values,
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_comparator,

    -- * SearchResourcesSortCriteria
    SearchResourcesSortCriteria (..),
    newSearchResourcesSortCriteria,
    searchResourcesSortCriteria_orderBy,
    searchResourcesSortCriteria_attributeName,

    -- * SearchResourcesTagCriterion
    SearchResourcesTagCriterion (..),
    newSearchResourcesTagCriterion,
    searchResourcesTagCriterion_tagValues,
    searchResourcesTagCriterion_comparator,

    -- * SearchResourcesTagCriterionPair
    SearchResourcesTagCriterionPair (..),
    newSearchResourcesTagCriterionPair,
    searchResourcesTagCriterionPair_value,
    searchResourcesTagCriterionPair_key,

    -- * SecurityHubConfiguration
    SecurityHubConfiguration (..),
    newSecurityHubConfiguration,
    securityHubConfiguration_publishPolicyFindings,
    securityHubConfiguration_publishClassificationFindings,

    -- * SensitiveDataItem
    SensitiveDataItem (..),
    newSensitiveDataItem,
    sensitiveDataItem_detections,
    sensitiveDataItem_category,
    sensitiveDataItem_totalCount,

    -- * ServerSideEncryption
    ServerSideEncryption (..),
    newServerSideEncryption,
    serverSideEncryption_encryptionType,
    serverSideEncryption_kmsMasterKeyId,

    -- * ServiceLimit
    ServiceLimit (..),
    newServiceLimit,
    serviceLimit_isServiceLimited,
    serviceLimit_value,
    serviceLimit_unit,

    -- * SessionContext
    SessionContext (..),
    newSessionContext,
    sessionContext_attributes,
    sessionContext_sessionIssuer,

    -- * SessionContextAttributes
    SessionContextAttributes (..),
    newSessionContextAttributes,
    sessionContextAttributes_creationDate,
    sessionContextAttributes_mfaAuthenticated,

    -- * SessionIssuer
    SessionIssuer (..),
    newSessionIssuer,
    sessionIssuer_principalId,
    sessionIssuer_arn,
    sessionIssuer_userName,
    sessionIssuer_accountId,
    sessionIssuer_type,

    -- * Severity
    Severity (..),
    newSeverity,
    severity_score,
    severity_description,

    -- * SimpleCriterionForJob
    SimpleCriterionForJob (..),
    newSimpleCriterionForJob,
    simpleCriterionForJob_values,
    simpleCriterionForJob_key,
    simpleCriterionForJob_comparator,

    -- * SimpleScopeTerm
    SimpleScopeTerm (..),
    newSimpleScopeTerm,
    simpleScopeTerm_values,
    simpleScopeTerm_key,
    simpleScopeTerm_comparator,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_orderBy,
    sortCriteria_attributeName,

    -- * Statistics
    Statistics (..),
    newStatistics,
    statistics_approximateNumberOfObjectsToProcess,
    statistics_numberOfRuns,

    -- * TagCriterionForJob
    TagCriterionForJob (..),
    newTagCriterionForJob,
    tagCriterionForJob_tagValues,
    tagCriterionForJob_comparator,

    -- * TagCriterionPairForJob
    TagCriterionPairForJob (..),
    newTagCriterionPairForJob,
    tagCriterionPairForJob_value,
    tagCriterionPairForJob_key,

    -- * TagScopeTerm
    TagScopeTerm (..),
    newTagScopeTerm,
    tagScopeTerm_tagValues,
    tagScopeTerm_key,
    tagScopeTerm_comparator,
    tagScopeTerm_target,

    -- * TagValuePair
    TagValuePair (..),
    newTagValuePair,
    tagValuePair_value,
    tagValuePair_key,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,
    unprocessedAccount_errorMessage,

    -- * UsageByAccount
    UsageByAccount (..),
    newUsageByAccount,
    usageByAccount_serviceLimit,
    usageByAccount_currency,
    usageByAccount_type,
    usageByAccount_estimatedCost,

    -- * UsageRecord
    UsageRecord (..),
    newUsageRecord,
    usageRecord_accountId,
    usageRecord_freeTrialStartDate,
    usageRecord_usage,

    -- * UsageStatisticsFilter
    UsageStatisticsFilter (..),
    newUsageStatisticsFilter,
    usageStatisticsFilter_values,
    usageStatisticsFilter_key,
    usageStatisticsFilter_comparator,

    -- * UsageStatisticsSortBy
    UsageStatisticsSortBy (..),
    newUsageStatisticsSortBy,
    usageStatisticsSortBy_orderBy,
    usageStatisticsSortBy_key,

    -- * UsageTotal
    UsageTotal (..),
    newUsageTotal,
    usageTotal_currency,
    usageTotal_type,
    usageTotal_estimatedCost,

    -- * UserIdentity
    UserIdentity (..),
    newUserIdentity,
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_awsAccount,
    userIdentity_assumedRole,
    userIdentity_federatedUser,
    userIdentity_awsService,
    userIdentity_type,

    -- * UserIdentityRoot
    UserIdentityRoot (..),
    newUserIdentityRoot,
    userIdentityRoot_principalId,
    userIdentityRoot_arn,
    userIdentityRoot_accountId,

    -- * UserPausedDetails
    UserPausedDetails (..),
    newUserPausedDetails,
    userPausedDetails_jobExpiresAt,
    userPausedDetails_jobImminentExpirationHealthEventArn,
    userPausedDetails_jobPausedAt,

    -- * WeeklySchedule
    WeeklySchedule (..),
    newWeeklySchedule,
    weeklySchedule_dayOfWeek,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.AccessControlList
import Network.AWS.MacieV2.Types.AccountDetail
import Network.AWS.MacieV2.Types.AccountLevelPermissions
import Network.AWS.MacieV2.Types.AdminAccount
import Network.AWS.MacieV2.Types.AdminStatus
import Network.AWS.MacieV2.Types.AllowsUnencryptedObjectUploads
import Network.AWS.MacieV2.Types.ApiCallDetails
import Network.AWS.MacieV2.Types.AssumedRole
import Network.AWS.MacieV2.Types.AwsAccount
import Network.AWS.MacieV2.Types.AwsService
import Network.AWS.MacieV2.Types.BatchGetCustomDataIdentifierSummary
import Network.AWS.MacieV2.Types.BlockPublicAccess
import Network.AWS.MacieV2.Types.BucketCountByEffectivePermission
import Network.AWS.MacieV2.Types.BucketCountByEncryptionType
import Network.AWS.MacieV2.Types.BucketCountBySharedAccessType
import Network.AWS.MacieV2.Types.BucketCountPolicyAllowsUnencryptedObjectUploads
import Network.AWS.MacieV2.Types.BucketCriteriaAdditionalProperties
import Network.AWS.MacieV2.Types.BucketLevelPermissions
import Network.AWS.MacieV2.Types.BucketMetadata
import Network.AWS.MacieV2.Types.BucketMetadataErrorCode
import Network.AWS.MacieV2.Types.BucketPermissionConfiguration
import Network.AWS.MacieV2.Types.BucketPolicy
import Network.AWS.MacieV2.Types.BucketPublicAccess
import Network.AWS.MacieV2.Types.BucketServerSideEncryption
import Network.AWS.MacieV2.Types.BucketSortCriteria
import Network.AWS.MacieV2.Types.Cell
import Network.AWS.MacieV2.Types.ClassificationDetails
import Network.AWS.MacieV2.Types.ClassificationExportConfiguration
import Network.AWS.MacieV2.Types.ClassificationResult
import Network.AWS.MacieV2.Types.ClassificationResultStatus
import Network.AWS.MacieV2.Types.CriteriaBlockForJob
import Network.AWS.MacieV2.Types.CriteriaForJob
import Network.AWS.MacieV2.Types.CriterionAdditionalProperties
import Network.AWS.MacieV2.Types.Currency
import Network.AWS.MacieV2.Types.CustomDataIdentifierSummary
import Network.AWS.MacieV2.Types.CustomDataIdentifiers
import Network.AWS.MacieV2.Types.CustomDetection
import Network.AWS.MacieV2.Types.DailySchedule
import Network.AWS.MacieV2.Types.DayOfWeek
import Network.AWS.MacieV2.Types.DefaultDetection
import Network.AWS.MacieV2.Types.DomainDetails
import Network.AWS.MacieV2.Types.EffectivePermission
import Network.AWS.MacieV2.Types.EncryptionType
import Network.AWS.MacieV2.Types.ErrorCode
import Network.AWS.MacieV2.Types.FederatedUser
import Network.AWS.MacieV2.Types.Finding
import Network.AWS.MacieV2.Types.FindingAction
import Network.AWS.MacieV2.Types.FindingActionType
import Network.AWS.MacieV2.Types.FindingActor
import Network.AWS.MacieV2.Types.FindingCategory
import Network.AWS.MacieV2.Types.FindingCriteria
import Network.AWS.MacieV2.Types.FindingPublishingFrequency
import Network.AWS.MacieV2.Types.FindingStatisticsSortAttributeName
import Network.AWS.MacieV2.Types.FindingStatisticsSortCriteria
import Network.AWS.MacieV2.Types.FindingType
import Network.AWS.MacieV2.Types.FindingsFilterAction
import Network.AWS.MacieV2.Types.FindingsFilterListItem
import Network.AWS.MacieV2.Types.GroupBy
import Network.AWS.MacieV2.Types.GroupCount
import Network.AWS.MacieV2.Types.IamUser
import Network.AWS.MacieV2.Types.Invitation
import Network.AWS.MacieV2.Types.IpAddressDetails
import Network.AWS.MacieV2.Types.IpCity
import Network.AWS.MacieV2.Types.IpCountry
import Network.AWS.MacieV2.Types.IpGeoLocation
import Network.AWS.MacieV2.Types.IpOwner
import Network.AWS.MacieV2.Types.IsDefinedInJob
import Network.AWS.MacieV2.Types.IsMonitoredByJob
import Network.AWS.MacieV2.Types.JobComparator
import Network.AWS.MacieV2.Types.JobDetails
import Network.AWS.MacieV2.Types.JobScheduleFrequency
import Network.AWS.MacieV2.Types.JobScopeTerm
import Network.AWS.MacieV2.Types.JobScopingBlock
import Network.AWS.MacieV2.Types.JobStatus
import Network.AWS.MacieV2.Types.JobSummary
import Network.AWS.MacieV2.Types.JobType
import Network.AWS.MacieV2.Types.KeyValuePair
import Network.AWS.MacieV2.Types.LastRunErrorStatus
import Network.AWS.MacieV2.Types.LastRunErrorStatusCode
import Network.AWS.MacieV2.Types.ListJobsFilterCriteria
import Network.AWS.MacieV2.Types.ListJobsFilterKey
import Network.AWS.MacieV2.Types.ListJobsFilterTerm
import Network.AWS.MacieV2.Types.ListJobsSortAttributeName
import Network.AWS.MacieV2.Types.ListJobsSortCriteria
import Network.AWS.MacieV2.Types.MacieStatus
import Network.AWS.MacieV2.Types.ManagedDataIdentifierSelector
import Network.AWS.MacieV2.Types.ManagedDataIdentifierSummary
import Network.AWS.MacieV2.Types.MatchingBucket
import Network.AWS.MacieV2.Types.MatchingResource
import Network.AWS.MacieV2.Types.Member
import Network.AWS.MacieV2.Types.MonthlySchedule
import Network.AWS.MacieV2.Types.ObjectCountByEncryptionType
import Network.AWS.MacieV2.Types.ObjectLevelStatistics
import Network.AWS.MacieV2.Types.Occurrences
import Network.AWS.MacieV2.Types.OrderBy
import Network.AWS.MacieV2.Types.Page
import Network.AWS.MacieV2.Types.PolicyDetails
import Network.AWS.MacieV2.Types.Range
import Network.AWS.MacieV2.Types.Record
import Network.AWS.MacieV2.Types.RelationshipStatus
import Network.AWS.MacieV2.Types.ReplicationDetails
import Network.AWS.MacieV2.Types.ResourcesAffected
import Network.AWS.MacieV2.Types.S3Bucket
import Network.AWS.MacieV2.Types.S3BucketCriteriaForJob
import Network.AWS.MacieV2.Types.S3BucketDefinitionForJob
import Network.AWS.MacieV2.Types.S3BucketOwner
import Network.AWS.MacieV2.Types.S3Destination
import Network.AWS.MacieV2.Types.S3JobDefinition
import Network.AWS.MacieV2.Types.S3Object
import Network.AWS.MacieV2.Types.ScopeFilterKey
import Network.AWS.MacieV2.Types.Scoping
import Network.AWS.MacieV2.Types.SearchResourcesBucketCriteria
import Network.AWS.MacieV2.Types.SearchResourcesComparator
import Network.AWS.MacieV2.Types.SearchResourcesCriteria
import Network.AWS.MacieV2.Types.SearchResourcesCriteriaBlock
import Network.AWS.MacieV2.Types.SearchResourcesSimpleCriterion
import Network.AWS.MacieV2.Types.SearchResourcesSimpleCriterionKey
import Network.AWS.MacieV2.Types.SearchResourcesSortAttributeName
import Network.AWS.MacieV2.Types.SearchResourcesSortCriteria
import Network.AWS.MacieV2.Types.SearchResourcesTagCriterion
import Network.AWS.MacieV2.Types.SearchResourcesTagCriterionPair
import Network.AWS.MacieV2.Types.SecurityHubConfiguration
import Network.AWS.MacieV2.Types.SensitiveDataItem
import Network.AWS.MacieV2.Types.SensitiveDataItemCategory
import Network.AWS.MacieV2.Types.ServerSideEncryption
import Network.AWS.MacieV2.Types.ServiceLimit
import Network.AWS.MacieV2.Types.SessionContext
import Network.AWS.MacieV2.Types.SessionContextAttributes
import Network.AWS.MacieV2.Types.SessionIssuer
import Network.AWS.MacieV2.Types.Severity
import Network.AWS.MacieV2.Types.SeverityDescription
import Network.AWS.MacieV2.Types.SharedAccess
import Network.AWS.MacieV2.Types.SimpleCriterionForJob
import Network.AWS.MacieV2.Types.SimpleCriterionKeyForJob
import Network.AWS.MacieV2.Types.SimpleScopeTerm
import Network.AWS.MacieV2.Types.SortCriteria
import Network.AWS.MacieV2.Types.Statistics
import Network.AWS.MacieV2.Types.StorageClass
import Network.AWS.MacieV2.Types.TagCriterionForJob
import Network.AWS.MacieV2.Types.TagCriterionPairForJob
import Network.AWS.MacieV2.Types.TagScopeTerm
import Network.AWS.MacieV2.Types.TagTarget
import Network.AWS.MacieV2.Types.TagValuePair
import Network.AWS.MacieV2.Types.TimeRange
import Network.AWS.MacieV2.Types.Type
import Network.AWS.MacieV2.Types.Unit
import Network.AWS.MacieV2.Types.UnprocessedAccount
import Network.AWS.MacieV2.Types.UsageByAccount
import Network.AWS.MacieV2.Types.UsageRecord
import Network.AWS.MacieV2.Types.UsageStatisticsFilter
import Network.AWS.MacieV2.Types.UsageStatisticsFilterComparator
import Network.AWS.MacieV2.Types.UsageStatisticsFilterKey
import Network.AWS.MacieV2.Types.UsageStatisticsSortBy
import Network.AWS.MacieV2.Types.UsageStatisticsSortKey
import Network.AWS.MacieV2.Types.UsageTotal
import Network.AWS.MacieV2.Types.UsageType
import Network.AWS.MacieV2.Types.UserIdentity
import Network.AWS.MacieV2.Types.UserIdentityRoot
import Network.AWS.MacieV2.Types.UserIdentityType
import Network.AWS.MacieV2.Types.UserPausedDetails
import Network.AWS.MacieV2.Types.WeeklySchedule
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-01-01@ of the Amazon Macie 2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "MacieV2",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "macie2",
      Core._serviceSigningName = "macie2",
      Core._serviceVersion = "2020-01-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "MacieV2",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Provides information about an error that occurred due to a syntax error
-- in a request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Provides information about an error that occurred due to insufficient
-- access to a specified resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an error that occurred due to a versioning
-- conflict for a specified resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an error that occurred due to one or more
-- service quotas for an account.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Provides information about an error that occurred because too many
-- requests were sent during a certain amount of time.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Provides information about an error that occurred due to an unknown
-- internal server error, exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an error that occurred because a specified
-- resource wasn\'t found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
