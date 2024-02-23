{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _UnprocessableEntityException,
    _ValidationException,

    -- * AdminStatus
    AdminStatus (..),

    -- * AllowListStatusCode
    AllowListStatusCode (..),

    -- * AllowsUnencryptedObjectUploads
    AllowsUnencryptedObjectUploads (..),

    -- * AutomatedDiscoveryStatus
    AutomatedDiscoveryStatus (..),

    -- * AvailabilityCode
    AvailabilityCode (..),

    -- * BucketMetadataErrorCode
    BucketMetadataErrorCode (..),

    -- * ClassificationScopeUpdateOperation
    ClassificationScopeUpdateOperation (..),

    -- * Currency
    Currency (..),

    -- * DataIdentifierSeverity
    DataIdentifierSeverity (..),

    -- * DataIdentifierType
    DataIdentifierType (..),

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

    -- * OriginType
    OriginType (..),

    -- * RelationshipStatus
    RelationshipStatus (..),

    -- * RevealRequestStatus
    RevealRequestStatus (..),

    -- * RevealStatus
    RevealStatus (..),

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

    -- * UnavailabilityReasonCode
    UnavailabilityReasonCode (..),

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
    accessControlList_allowsPublicReadAccess,
    accessControlList_allowsPublicWriteAccess,

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
    adminAccount_accountId,
    adminAccount_status,

    -- * AllowListCriteria
    AllowListCriteria (..),
    newAllowListCriteria,
    allowListCriteria_regex,
    allowListCriteria_s3WordsList,

    -- * AllowListStatus
    AllowListStatus (..),
    newAllowListStatus,
    allowListStatus_description,
    allowListStatus_code,

    -- * AllowListSummary
    AllowListSummary (..),
    newAllowListSummary,
    allowListSummary_arn,
    allowListSummary_createdAt,
    allowListSummary_description,
    allowListSummary_id,
    allowListSummary_name,
    allowListSummary_updatedAt,

    -- * ApiCallDetails
    ApiCallDetails (..),
    newApiCallDetails,
    apiCallDetails_api,
    apiCallDetails_apiServiceName,
    apiCallDetails_firstSeen,
    apiCallDetails_lastSeen,

    -- * AssumedRole
    AssumedRole (..),
    newAssumedRole,
    assumedRole_accessKeyId,
    assumedRole_accountId,
    assumedRole_arn,
    assumedRole_principalId,
    assumedRole_sessionContext,

    -- * AwsAccount
    AwsAccount (..),
    newAwsAccount,
    awsAccount_accountId,
    awsAccount_principalId,

    -- * AwsService
    AwsService (..),
    newAwsService,
    awsService_invokedBy,

    -- * BatchGetCustomDataIdentifierSummary
    BatchGetCustomDataIdentifierSummary (..),
    newBatchGetCustomDataIdentifierSummary,
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_createdAt,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_description,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_name,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_blockPublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_restrictPublicBuckets,

    -- * BucketCountByEffectivePermission
    BucketCountByEffectivePermission (..),
    newBucketCountByEffectivePermission,
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_publiclyWritable,
    bucketCountByEffectivePermission_unknown,

    -- * BucketCountByEncryptionType
    BucketCountByEncryptionType (..),
    newBucketCountByEncryptionType,
    bucketCountByEncryptionType_kmsManaged,
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_unknown,

    -- * BucketCountBySharedAccessType
    BucketCountBySharedAccessType (..),
    newBucketCountBySharedAccessType,
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_internal,
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_unknown,

    -- * BucketCountPolicyAllowsUnencryptedObjectUploads
    BucketCountPolicyAllowsUnencryptedObjectUploads (..),
    newBucketCountPolicyAllowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,

    -- * BucketCriteriaAdditionalProperties
    BucketCriteriaAdditionalProperties (..),
    newBucketCriteriaAdditionalProperties,
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_gt,
    bucketCriteriaAdditionalProperties_gte,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_prefix,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    newBucketLevelPermissions,
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- * BucketMetadata
    BucketMetadata (..),
    newBucketMetadata,
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

    -- * BucketPermissionConfiguration
    BucketPermissionConfiguration (..),
    newBucketPermissionConfiguration,
    bucketPermissionConfiguration_accountLevelPermissions,
    bucketPermissionConfiguration_bucketLevelPermissions,

    -- * BucketPolicy
    BucketPolicy (..),
    newBucketPolicy,
    bucketPolicy_allowsPublicReadAccess,
    bucketPolicy_allowsPublicWriteAccess,

    -- * BucketPublicAccess
    BucketPublicAccess (..),
    newBucketPublicAccess,
    bucketPublicAccess_effectivePermission,
    bucketPublicAccess_permissionConfiguration,

    -- * BucketServerSideEncryption
    BucketServerSideEncryption (..),
    newBucketServerSideEncryption,
    bucketServerSideEncryption_kmsMasterKeyId,
    bucketServerSideEncryption_type,

    -- * BucketSortCriteria
    BucketSortCriteria (..),
    newBucketSortCriteria,
    bucketSortCriteria_attributeName,
    bucketSortCriteria_orderBy,

    -- * BucketStatisticsBySensitivity
    BucketStatisticsBySensitivity (..),
    newBucketStatisticsBySensitivity,
    bucketStatisticsBySensitivity_classificationError,
    bucketStatisticsBySensitivity_notClassified,
    bucketStatisticsBySensitivity_notSensitive,
    bucketStatisticsBySensitivity_sensitive,

    -- * Cell
    Cell (..),
    newCell,
    cell_cellReference,
    cell_column,
    cell_columnName,
    cell_row,

    -- * ClassificationDetails
    ClassificationDetails (..),
    newClassificationDetails,
    classificationDetails_detailedResultsLocation,
    classificationDetails_jobArn,
    classificationDetails_jobId,
    classificationDetails_originType,
    classificationDetails_result,

    -- * ClassificationExportConfiguration
    ClassificationExportConfiguration (..),
    newClassificationExportConfiguration,
    classificationExportConfiguration_s3Destination,

    -- * ClassificationResult
    ClassificationResult (..),
    newClassificationResult,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sensitiveData,
    classificationResult_sizeClassified,
    classificationResult_status,

    -- * ClassificationResultStatus
    ClassificationResultStatus (..),
    newClassificationResultStatus,
    classificationResultStatus_code,
    classificationResultStatus_reason,

    -- * ClassificationScopeSummary
    ClassificationScopeSummary (..),
    newClassificationScopeSummary,
    classificationScopeSummary_id,
    classificationScopeSummary_name,

    -- * CriteriaBlockForJob
    CriteriaBlockForJob (..),
    newCriteriaBlockForJob,
    criteriaBlockForJob_and,

    -- * CriteriaForJob
    CriteriaForJob (..),
    newCriteriaForJob,
    criteriaForJob_simpleCriterion,
    criteriaForJob_tagCriterion,

    -- * CriterionAdditionalProperties
    CriterionAdditionalProperties (..),
    newCriterionAdditionalProperties,
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_eqExactMatch,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_gte,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_neq,

    -- * CustomDataIdentifierSummary
    CustomDataIdentifierSummary (..),
    newCustomDataIdentifierSummary,
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_createdAt,
    customDataIdentifierSummary_description,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_name,

    -- * CustomDataIdentifiers
    CustomDataIdentifiers (..),
    newCustomDataIdentifiers,
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- * CustomDetection
    CustomDetection (..),
    newCustomDetection,
    customDetection_arn,
    customDetection_count,
    customDetection_name,
    customDetection_occurrences,

    -- * DailySchedule
    DailySchedule (..),
    newDailySchedule,

    -- * DefaultDetection
    DefaultDetection (..),
    newDefaultDetection,
    defaultDetection_count,
    defaultDetection_occurrences,
    defaultDetection_type,

    -- * DetectedDataDetails
    DetectedDataDetails (..),
    newDetectedDataDetails,
    detectedDataDetails_value,

    -- * Detection
    Detection (..),
    newDetection,
    detection_arn,
    detection_count,
    detection_id,
    detection_name,
    detection_suppressed,
    detection_type,

    -- * DomainDetails
    DomainDetails (..),
    newDomainDetails,
    domainDetails_domainName,

    -- * FederatedUser
    FederatedUser (..),
    newFederatedUser,
    federatedUser_accessKeyId,
    federatedUser_accountId,
    federatedUser_arn,
    federatedUser_principalId,
    federatedUser_sessionContext,

    -- * Finding
    Finding (..),
    newFinding,
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

    -- * FindingAction
    FindingAction (..),
    newFindingAction,
    findingAction_actionType,
    findingAction_apiCallDetails,

    -- * FindingActor
    FindingActor (..),
    newFindingActor,
    findingActor_domainDetails,
    findingActor_ipAddressDetails,
    findingActor_userIdentity,

    -- * FindingCriteria
    FindingCriteria (..),
    newFindingCriteria,
    findingCriteria_criterion,

    -- * FindingStatisticsSortCriteria
    FindingStatisticsSortCriteria (..),
    newFindingStatisticsSortCriteria,
    findingStatisticsSortCriteria_attributeName,
    findingStatisticsSortCriteria_orderBy,

    -- * FindingsFilterListItem
    FindingsFilterListItem (..),
    newFindingsFilterListItem,
    findingsFilterListItem_action,
    findingsFilterListItem_arn,
    findingsFilterListItem_id,
    findingsFilterListItem_name,
    findingsFilterListItem_tags,

    -- * GroupCount
    GroupCount (..),
    newGroupCount,
    groupCount_count,
    groupCount_groupKey,

    -- * IamUser
    IamUser (..),
    newIamUser,
    iamUser_accountId,
    iamUser_arn,
    iamUser_principalId,
    iamUser_userName,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_accountId,
    invitation_invitationId,
    invitation_invitedAt,
    invitation_relationshipStatus,

    -- * IpAddressDetails
    IpAddressDetails (..),
    newIpAddressDetails,
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipCity,
    ipAddressDetails_ipCountry,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipOwner,

    -- * IpCity
    IpCity (..),
    newIpCity,
    ipCity_name,

    -- * IpCountry
    IpCountry (..),
    newIpCountry,
    ipCountry_code,
    ipCountry_name,

    -- * IpGeoLocation
    IpGeoLocation (..),
    newIpGeoLocation,
    ipGeoLocation_lat,
    ipGeoLocation_lon,

    -- * IpOwner
    IpOwner (..),
    newIpOwner,
    ipOwner_asn,
    ipOwner_asnOrg,
    ipOwner_isp,
    ipOwner_org,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_isDefinedInJob,
    jobDetails_isMonitoredByJob,
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
    jobSummary_bucketCriteria,
    jobSummary_bucketDefinitions,
    jobSummary_createdAt,
    jobSummary_jobId,
    jobSummary_jobStatus,
    jobSummary_jobType,
    jobSummary_lastRunErrorStatus,
    jobSummary_name,
    jobSummary_userPausedDetails,

    -- * KeyValuePair
    KeyValuePair (..),
    newKeyValuePair,
    keyValuePair_key,
    keyValuePair_value,

    -- * LastRunErrorStatus
    LastRunErrorStatus (..),
    newLastRunErrorStatus,
    lastRunErrorStatus_code,

    -- * ListJobsFilterCriteria
    ListJobsFilterCriteria (..),
    newListJobsFilterCriteria,
    listJobsFilterCriteria_excludes,
    listJobsFilterCriteria_includes,

    -- * ListJobsFilterTerm
    ListJobsFilterTerm (..),
    newListJobsFilterTerm,
    listJobsFilterTerm_comparator,
    listJobsFilterTerm_key,
    listJobsFilterTerm_values,

    -- * ListJobsSortCriteria
    ListJobsSortCriteria (..),
    newListJobsSortCriteria,
    listJobsSortCriteria_attributeName,
    listJobsSortCriteria_orderBy,

    -- * ManagedDataIdentifierSummary
    ManagedDataIdentifierSummary (..),
    newManagedDataIdentifierSummary,
    managedDataIdentifierSummary_category,
    managedDataIdentifierSummary_id,

    -- * MatchingBucket
    MatchingBucket (..),
    newMatchingBucket,
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

    -- * MatchingResource
    MatchingResource (..),
    newMatchingResource,
    matchingResource_matchingBucket,

    -- * Member
    Member (..),
    newMember,
    member_accountId,
    member_administratorAccountId,
    member_arn,
    member_email,
    member_invitedAt,
    member_masterAccountId,
    member_relationshipStatus,
    member_tags,
    member_updatedAt,

    -- * MonthlySchedule
    MonthlySchedule (..),
    newMonthlySchedule,
    monthlySchedule_dayOfMonth,

    -- * ObjectCountByEncryptionType
    ObjectCountByEncryptionType (..),
    newObjectCountByEncryptionType,
    objectCountByEncryptionType_customerManaged,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_unknown,

    -- * ObjectLevelStatistics
    ObjectLevelStatistics (..),
    newObjectLevelStatistics,
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,
    objectLevelStatistics_total,

    -- * Occurrences
    Occurrences (..),
    newOccurrences,
    occurrences_cells,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_pages,
    occurrences_records,

    -- * Page
    Page (..),
    newPage,
    page_lineRange,
    page_offsetRange,
    page_pageNumber,

    -- * PolicyDetails
    PolicyDetails (..),
    newPolicyDetails,
    policyDetails_action,
    policyDetails_actor,

    -- * Range
    Range (..),
    newRange,
    range_end,
    range_start,
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
    replicationDetails_replicatedExternally,
    replicationDetails_replicationAccounts,

    -- * ResourceProfileArtifact
    ResourceProfileArtifact (..),
    newResourceProfileArtifact,
    resourceProfileArtifact_sensitive,
    resourceProfileArtifact_classificationResultStatus,
    resourceProfileArtifact_arn,

    -- * ResourceStatistics
    ResourceStatistics (..),
    newResourceStatistics,
    resourceStatistics_totalBytesClassified,
    resourceStatistics_totalDetections,
    resourceStatistics_totalDetectionsSuppressed,
    resourceStatistics_totalItemsClassified,
    resourceStatistics_totalItemsSensitive,
    resourceStatistics_totalItemsSkipped,
    resourceStatistics_totalItemsSkippedInvalidEncryption,
    resourceStatistics_totalItemsSkippedInvalidKms,
    resourceStatistics_totalItemsSkippedPermissionDenied,

    -- * ResourcesAffected
    ResourcesAffected (..),
    newResourcesAffected,
    resourcesAffected_s3Bucket,
    resourcesAffected_s3Object,

    -- * RevealConfiguration
    RevealConfiguration (..),
    newRevealConfiguration,
    revealConfiguration_kmsKeyId,
    revealConfiguration_status,

    -- * S3Bucket
    S3Bucket (..),
    newS3Bucket,
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_arn,
    s3Bucket_createdAt,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_name,
    s3Bucket_owner,
    s3Bucket_publicAccess,
    s3Bucket_tags,

    -- * S3BucketCriteriaForJob
    S3BucketCriteriaForJob (..),
    newS3BucketCriteriaForJob,
    s3BucketCriteriaForJob_excludes,
    s3BucketCriteriaForJob_includes,

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

    -- * S3ClassificationScope
    S3ClassificationScope (..),
    newS3ClassificationScope,
    s3ClassificationScope_excludes,

    -- * S3ClassificationScopeExclusion
    S3ClassificationScopeExclusion (..),
    newS3ClassificationScopeExclusion,
    s3ClassificationScopeExclusion_bucketNames,

    -- * S3ClassificationScopeExclusionUpdate
    S3ClassificationScopeExclusionUpdate (..),
    newS3ClassificationScopeExclusionUpdate,
    s3ClassificationScopeExclusionUpdate_bucketNames,
    s3ClassificationScopeExclusionUpdate_operation,

    -- * S3ClassificationScopeUpdate
    S3ClassificationScopeUpdate (..),
    newS3ClassificationScopeUpdate,
    s3ClassificationScopeUpdate_excludes,

    -- * S3Destination
    S3Destination (..),
    newS3Destination,
    s3Destination_keyPrefix,
    s3Destination_bucketName,
    s3Destination_kmsKeyArn,

    -- * S3JobDefinition
    S3JobDefinition (..),
    newS3JobDefinition,
    s3JobDefinition_bucketCriteria,
    s3JobDefinition_bucketDefinitions,
    s3JobDefinition_scoping,

    -- * S3Object
    S3Object (..),
    newS3Object,
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

    -- * S3WordsList
    S3WordsList (..),
    newS3WordsList,
    s3WordsList_bucketName,
    s3WordsList_objectKey,

    -- * Scoping
    Scoping (..),
    newScoping,
    scoping_excludes,
    scoping_includes,

    -- * SearchResourcesBucketCriteria
    SearchResourcesBucketCriteria (..),
    newSearchResourcesBucketCriteria,
    searchResourcesBucketCriteria_excludes,
    searchResourcesBucketCriteria_includes,

    -- * SearchResourcesCriteria
    SearchResourcesCriteria (..),
    newSearchResourcesCriteria,
    searchResourcesCriteria_simpleCriterion,
    searchResourcesCriteria_tagCriterion,

    -- * SearchResourcesCriteriaBlock
    SearchResourcesCriteriaBlock (..),
    newSearchResourcesCriteriaBlock,
    searchResourcesCriteriaBlock_and,

    -- * SearchResourcesSimpleCriterion
    SearchResourcesSimpleCriterion (..),
    newSearchResourcesSimpleCriterion,
    searchResourcesSimpleCriterion_comparator,
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_values,

    -- * SearchResourcesSortCriteria
    SearchResourcesSortCriteria (..),
    newSearchResourcesSortCriteria,
    searchResourcesSortCriteria_attributeName,
    searchResourcesSortCriteria_orderBy,

    -- * SearchResourcesTagCriterion
    SearchResourcesTagCriterion (..),
    newSearchResourcesTagCriterion,
    searchResourcesTagCriterion_comparator,
    searchResourcesTagCriterion_tagValues,

    -- * SearchResourcesTagCriterionPair
    SearchResourcesTagCriterionPair (..),
    newSearchResourcesTagCriterionPair,
    searchResourcesTagCriterionPair_key,
    searchResourcesTagCriterionPair_value,

    -- * SecurityHubConfiguration
    SecurityHubConfiguration (..),
    newSecurityHubConfiguration,
    securityHubConfiguration_publishPolicyFindings,
    securityHubConfiguration_publishClassificationFindings,

    -- * SensitiveDataItem
    SensitiveDataItem (..),
    newSensitiveDataItem,
    sensitiveDataItem_category,
    sensitiveDataItem_detections,
    sensitiveDataItem_totalCount,

    -- * SensitivityAggregations
    SensitivityAggregations (..),
    newSensitivityAggregations,
    sensitivityAggregations_classifiableSizeInBytes,
    sensitivityAggregations_publiclyAccessibleCount,
    sensitivityAggregations_totalCount,
    sensitivityAggregations_totalSizeInBytes,

    -- * SensitivityInspectionTemplateExcludes
    SensitivityInspectionTemplateExcludes (..),
    newSensitivityInspectionTemplateExcludes,
    sensitivityInspectionTemplateExcludes_managedDataIdentifierIds,

    -- * SensitivityInspectionTemplateIncludes
    SensitivityInspectionTemplateIncludes (..),
    newSensitivityInspectionTemplateIncludes,
    sensitivityInspectionTemplateIncludes_allowListIds,
    sensitivityInspectionTemplateIncludes_customDataIdentifierIds,
    sensitivityInspectionTemplateIncludes_managedDataIdentifierIds,

    -- * SensitivityInspectionTemplatesEntry
    SensitivityInspectionTemplatesEntry (..),
    newSensitivityInspectionTemplatesEntry,
    sensitivityInspectionTemplatesEntry_id,
    sensitivityInspectionTemplatesEntry_name,

    -- * ServerSideEncryption
    ServerSideEncryption (..),
    newServerSideEncryption,
    serverSideEncryption_encryptionType,
    serverSideEncryption_kmsMasterKeyId,

    -- * ServiceLimit
    ServiceLimit (..),
    newServiceLimit,
    serviceLimit_isServiceLimited,
    serviceLimit_unit,
    serviceLimit_value,

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
    sessionIssuer_accountId,
    sessionIssuer_arn,
    sessionIssuer_principalId,
    sessionIssuer_type,
    sessionIssuer_userName,

    -- * Severity
    Severity (..),
    newSeverity,
    severity_description,
    severity_score,

    -- * SeverityLevel
    SeverityLevel (..),
    newSeverityLevel,
    severityLevel_occurrencesThreshold,
    severityLevel_severity,

    -- * SimpleCriterionForJob
    SimpleCriterionForJob (..),
    newSimpleCriterionForJob,
    simpleCriterionForJob_comparator,
    simpleCriterionForJob_key,
    simpleCriterionForJob_values,

    -- * SimpleScopeTerm
    SimpleScopeTerm (..),
    newSimpleScopeTerm,
    simpleScopeTerm_comparator,
    simpleScopeTerm_key,
    simpleScopeTerm_values,

    -- * SortCriteria
    SortCriteria (..),
    newSortCriteria,
    sortCriteria_attributeName,
    sortCriteria_orderBy,

    -- * Statistics
    Statistics (..),
    newStatistics,
    statistics_approximateNumberOfObjectsToProcess,
    statistics_numberOfRuns,

    -- * SuppressDataIdentifier
    SuppressDataIdentifier (..),
    newSuppressDataIdentifier,
    suppressDataIdentifier_id,
    suppressDataIdentifier_type,

    -- * TagCriterionForJob
    TagCriterionForJob (..),
    newTagCriterionForJob,
    tagCriterionForJob_comparator,
    tagCriterionForJob_tagValues,

    -- * TagCriterionPairForJob
    TagCriterionPairForJob (..),
    newTagCriterionPairForJob,
    tagCriterionPairForJob_key,
    tagCriterionPairForJob_value,

    -- * TagScopeTerm
    TagScopeTerm (..),
    newTagScopeTerm,
    tagScopeTerm_comparator,
    tagScopeTerm_key,
    tagScopeTerm_tagValues,
    tagScopeTerm_target,

    -- * TagValuePair
    TagValuePair (..),
    newTagValuePair,
    tagValuePair_key,
    tagValuePair_value,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,
    unprocessedAccount_errorMessage,

    -- * UsageByAccount
    UsageByAccount (..),
    newUsageByAccount,
    usageByAccount_currency,
    usageByAccount_estimatedCost,
    usageByAccount_serviceLimit,
    usageByAccount_type,

    -- * UsageRecord
    UsageRecord (..),
    newUsageRecord,
    usageRecord_accountId,
    usageRecord_automatedDiscoveryFreeTrialStartDate,
    usageRecord_freeTrialStartDate,
    usageRecord_usage,

    -- * UsageStatisticsFilter
    UsageStatisticsFilter (..),
    newUsageStatisticsFilter,
    usageStatisticsFilter_comparator,
    usageStatisticsFilter_key,
    usageStatisticsFilter_values,

    -- * UsageStatisticsSortBy
    UsageStatisticsSortBy (..),
    newUsageStatisticsSortBy,
    usageStatisticsSortBy_key,
    usageStatisticsSortBy_orderBy,

    -- * UsageTotal
    UsageTotal (..),
    newUsageTotal,
    usageTotal_currency,
    usageTotal_estimatedCost,
    usageTotal_type,

    -- * UserIdentity
    UserIdentity (..),
    newUserIdentity,
    userIdentity_assumedRole,
    userIdentity_awsAccount,
    userIdentity_awsService,
    userIdentity_federatedUser,
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_type,

    -- * UserIdentityRoot
    UserIdentityRoot (..),
    newUserIdentityRoot,
    userIdentityRoot_accountId,
    userIdentityRoot_arn,
    userIdentityRoot_principalId,

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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.AccessControlList
import Amazonka.MacieV2.Types.AccountDetail
import Amazonka.MacieV2.Types.AccountLevelPermissions
import Amazonka.MacieV2.Types.AdminAccount
import Amazonka.MacieV2.Types.AdminStatus
import Amazonka.MacieV2.Types.AllowListCriteria
import Amazonka.MacieV2.Types.AllowListStatus
import Amazonka.MacieV2.Types.AllowListStatusCode
import Amazonka.MacieV2.Types.AllowListSummary
import Amazonka.MacieV2.Types.AllowsUnencryptedObjectUploads
import Amazonka.MacieV2.Types.ApiCallDetails
import Amazonka.MacieV2.Types.AssumedRole
import Amazonka.MacieV2.Types.AutomatedDiscoveryStatus
import Amazonka.MacieV2.Types.AvailabilityCode
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
import Amazonka.MacieV2.Types.BucketMetadataErrorCode
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
import Amazonka.MacieV2.Types.ClassificationScopeUpdateOperation
import Amazonka.MacieV2.Types.CriteriaBlockForJob
import Amazonka.MacieV2.Types.CriteriaForJob
import Amazonka.MacieV2.Types.CriterionAdditionalProperties
import Amazonka.MacieV2.Types.Currency
import Amazonka.MacieV2.Types.CustomDataIdentifierSummary
import Amazonka.MacieV2.Types.CustomDataIdentifiers
import Amazonka.MacieV2.Types.CustomDetection
import Amazonka.MacieV2.Types.DailySchedule
import Amazonka.MacieV2.Types.DataIdentifierSeverity
import Amazonka.MacieV2.Types.DataIdentifierType
import Amazonka.MacieV2.Types.DayOfWeek
import Amazonka.MacieV2.Types.DefaultDetection
import Amazonka.MacieV2.Types.DetectedDataDetails
import Amazonka.MacieV2.Types.Detection
import Amazonka.MacieV2.Types.DomainDetails
import Amazonka.MacieV2.Types.EffectivePermission
import Amazonka.MacieV2.Types.EncryptionType
import Amazonka.MacieV2.Types.ErrorCode
import Amazonka.MacieV2.Types.FederatedUser
import Amazonka.MacieV2.Types.Finding
import Amazonka.MacieV2.Types.FindingAction
import Amazonka.MacieV2.Types.FindingActionType
import Amazonka.MacieV2.Types.FindingActor
import Amazonka.MacieV2.Types.FindingCategory
import Amazonka.MacieV2.Types.FindingCriteria
import Amazonka.MacieV2.Types.FindingPublishingFrequency
import Amazonka.MacieV2.Types.FindingStatisticsSortAttributeName
import Amazonka.MacieV2.Types.FindingStatisticsSortCriteria
import Amazonka.MacieV2.Types.FindingType
import Amazonka.MacieV2.Types.FindingsFilterAction
import Amazonka.MacieV2.Types.FindingsFilterListItem
import Amazonka.MacieV2.Types.GroupBy
import Amazonka.MacieV2.Types.GroupCount
import Amazonka.MacieV2.Types.IamUser
import Amazonka.MacieV2.Types.Invitation
import Amazonka.MacieV2.Types.IpAddressDetails
import Amazonka.MacieV2.Types.IpCity
import Amazonka.MacieV2.Types.IpCountry
import Amazonka.MacieV2.Types.IpGeoLocation
import Amazonka.MacieV2.Types.IpOwner
import Amazonka.MacieV2.Types.IsDefinedInJob
import Amazonka.MacieV2.Types.IsMonitoredByJob
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.JobDetails
import Amazonka.MacieV2.Types.JobScheduleFrequency
import Amazonka.MacieV2.Types.JobScopeTerm
import Amazonka.MacieV2.Types.JobScopingBlock
import Amazonka.MacieV2.Types.JobStatus
import Amazonka.MacieV2.Types.JobSummary
import Amazonka.MacieV2.Types.JobType
import Amazonka.MacieV2.Types.KeyValuePair
import Amazonka.MacieV2.Types.LastRunErrorStatus
import Amazonka.MacieV2.Types.LastRunErrorStatusCode
import Amazonka.MacieV2.Types.ListJobsFilterCriteria
import Amazonka.MacieV2.Types.ListJobsFilterKey
import Amazonka.MacieV2.Types.ListJobsFilterTerm
import Amazonka.MacieV2.Types.ListJobsSortAttributeName
import Amazonka.MacieV2.Types.ListJobsSortCriteria
import Amazonka.MacieV2.Types.MacieStatus
import Amazonka.MacieV2.Types.ManagedDataIdentifierSelector
import Amazonka.MacieV2.Types.ManagedDataIdentifierSummary
import Amazonka.MacieV2.Types.MatchingBucket
import Amazonka.MacieV2.Types.MatchingResource
import Amazonka.MacieV2.Types.Member
import Amazonka.MacieV2.Types.MonthlySchedule
import Amazonka.MacieV2.Types.ObjectCountByEncryptionType
import Amazonka.MacieV2.Types.ObjectLevelStatistics
import Amazonka.MacieV2.Types.Occurrences
import Amazonka.MacieV2.Types.OrderBy
import Amazonka.MacieV2.Types.OriginType
import Amazonka.MacieV2.Types.Page
import Amazonka.MacieV2.Types.PolicyDetails
import Amazonka.MacieV2.Types.Range
import Amazonka.MacieV2.Types.Record
import Amazonka.MacieV2.Types.RelationshipStatus
import Amazonka.MacieV2.Types.ReplicationDetails
import Amazonka.MacieV2.Types.ResourceProfileArtifact
import Amazonka.MacieV2.Types.ResourceStatistics
import Amazonka.MacieV2.Types.ResourcesAffected
import Amazonka.MacieV2.Types.RevealConfiguration
import Amazonka.MacieV2.Types.RevealRequestStatus
import Amazonka.MacieV2.Types.RevealStatus
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
import Amazonka.MacieV2.Types.ScopeFilterKey
import Amazonka.MacieV2.Types.Scoping
import Amazonka.MacieV2.Types.SearchResourcesBucketCriteria
import Amazonka.MacieV2.Types.SearchResourcesComparator
import Amazonka.MacieV2.Types.SearchResourcesCriteria
import Amazonka.MacieV2.Types.SearchResourcesCriteriaBlock
import Amazonka.MacieV2.Types.SearchResourcesSimpleCriterion
import Amazonka.MacieV2.Types.SearchResourcesSimpleCriterionKey
import Amazonka.MacieV2.Types.SearchResourcesSortAttributeName
import Amazonka.MacieV2.Types.SearchResourcesSortCriteria
import Amazonka.MacieV2.Types.SearchResourcesTagCriterion
import Amazonka.MacieV2.Types.SearchResourcesTagCriterionPair
import Amazonka.MacieV2.Types.SecurityHubConfiguration
import Amazonka.MacieV2.Types.SensitiveDataItem
import Amazonka.MacieV2.Types.SensitiveDataItemCategory
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
import Amazonka.MacieV2.Types.SeverityDescription
import Amazonka.MacieV2.Types.SeverityLevel
import Amazonka.MacieV2.Types.SharedAccess
import Amazonka.MacieV2.Types.SimpleCriterionForJob
import Amazonka.MacieV2.Types.SimpleCriterionKeyForJob
import Amazonka.MacieV2.Types.SimpleScopeTerm
import Amazonka.MacieV2.Types.SortCriteria
import Amazonka.MacieV2.Types.Statistics
import Amazonka.MacieV2.Types.StorageClass
import Amazonka.MacieV2.Types.SuppressDataIdentifier
import Amazonka.MacieV2.Types.TagCriterionForJob
import Amazonka.MacieV2.Types.TagCriterionPairForJob
import Amazonka.MacieV2.Types.TagScopeTerm
import Amazonka.MacieV2.Types.TagTarget
import Amazonka.MacieV2.Types.TagValuePair
import Amazonka.MacieV2.Types.TimeRange
import Amazonka.MacieV2.Types.Type
import Amazonka.MacieV2.Types.UnavailabilityReasonCode
import Amazonka.MacieV2.Types.Unit
import Amazonka.MacieV2.Types.UnprocessedAccount
import Amazonka.MacieV2.Types.UsageByAccount
import Amazonka.MacieV2.Types.UsageRecord
import Amazonka.MacieV2.Types.UsageStatisticsFilter
import Amazonka.MacieV2.Types.UsageStatisticsFilterComparator
import Amazonka.MacieV2.Types.UsageStatisticsFilterKey
import Amazonka.MacieV2.Types.UsageStatisticsSortBy
import Amazonka.MacieV2.Types.UsageStatisticsSortKey
import Amazonka.MacieV2.Types.UsageTotal
import Amazonka.MacieV2.Types.UsageType
import Amazonka.MacieV2.Types.UserIdentity
import Amazonka.MacieV2.Types.UserIdentityRoot
import Amazonka.MacieV2.Types.UserIdentityType
import Amazonka.MacieV2.Types.UserPausedDetails
import Amazonka.MacieV2.Types.WeeklySchedule
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-01-01@ of the Amazon Macie 2 SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MacieV2",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "macie2",
      Core.signingName = "macie2",
      Core.version = "2020-01-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "MacieV2",
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

-- | Provides information about an error that occurred due to insufficient
-- access to a specified resource.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an error that occurred due to a versioning
-- conflict for a specified resource.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an error that occurred due to an unknown
-- internal server error, exception, or failure.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an error that occurred because a specified
-- resource wasn\'t found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides information about an error that occurred due to one or more
-- service quotas for an account.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Provides information about an error that occurred because too many
-- requests were sent during a certain amount of time.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Provides information about an error that occurred due to an
-- unprocessable entity.
_UnprocessableEntityException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | Provides information about an error that occurred due to a syntax error
-- in a request.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
