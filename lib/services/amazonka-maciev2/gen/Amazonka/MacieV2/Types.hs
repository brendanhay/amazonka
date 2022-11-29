{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _UnprocessableEntityException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,

    -- * AdminStatus
    AdminStatus (..),

    -- * AllowListStatusCode
    AllowListStatusCode (..),

    -- * AllowsUnencryptedObjectUploads
    AllowsUnencryptedObjectUploads (..),

    -- * AvailabilityCode
    AvailabilityCode (..),

    -- * BucketMetadataErrorCode
    BucketMetadataErrorCode (..),

    -- * Currency
    Currency (..),

    -- * DataIdentifierSeverity
    DataIdentifierSeverity (..),

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
    adminAccount_status,
    adminAccount_accountId,

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
    allowListSummary_name,
    allowListSummary_arn,
    allowListSummary_description,
    allowListSummary_id,
    allowListSummary_createdAt,
    allowListSummary_updatedAt,

    -- * ApiCallDetails
    ApiCallDetails (..),
    newApiCallDetails,
    apiCallDetails_lastSeen,
    apiCallDetails_apiServiceName,
    apiCallDetails_api,
    apiCallDetails_firstSeen,

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
    batchGetCustomDataIdentifierSummary_name,
    batchGetCustomDataIdentifierSummary_deleted,
    batchGetCustomDataIdentifierSummary_arn,
    batchGetCustomDataIdentifierSummary_description,
    batchGetCustomDataIdentifierSummary_id,
    batchGetCustomDataIdentifierSummary_createdAt,

    -- * BlockPublicAccess
    BlockPublicAccess (..),
    newBlockPublicAccess,
    blockPublicAccess_restrictPublicBuckets,
    blockPublicAccess_ignorePublicAcls,
    blockPublicAccess_blockPublicPolicy,
    blockPublicAccess_blockPublicAcls,

    -- * BucketCountByEffectivePermission
    BucketCountByEffectivePermission (..),
    newBucketCountByEffectivePermission,
    bucketCountByEffectivePermission_publiclyAccessible,
    bucketCountByEffectivePermission_publiclyReadable,
    bucketCountByEffectivePermission_unknown,
    bucketCountByEffectivePermission_publiclyWritable,

    -- * BucketCountByEncryptionType
    BucketCountByEncryptionType (..),
    newBucketCountByEncryptionType,
    bucketCountByEncryptionType_s3Managed,
    bucketCountByEncryptionType_unencrypted,
    bucketCountByEncryptionType_kmsManaged,
    bucketCountByEncryptionType_unknown,

    -- * BucketCountBySharedAccessType
    BucketCountBySharedAccessType (..),
    newBucketCountBySharedAccessType,
    bucketCountBySharedAccessType_external,
    bucketCountBySharedAccessType_unknown,
    bucketCountBySharedAccessType_notShared,
    bucketCountBySharedAccessType_internal,

    -- * BucketCountPolicyAllowsUnencryptedObjectUploads
    BucketCountPolicyAllowsUnencryptedObjectUploads (..),
    newBucketCountPolicyAllowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_deniesUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_allowsUnencryptedObjectUploads,
    bucketCountPolicyAllowsUnencryptedObjectUploads_unknown,

    -- * BucketCriteriaAdditionalProperties
    BucketCriteriaAdditionalProperties (..),
    newBucketCriteriaAdditionalProperties,
    bucketCriteriaAdditionalProperties_neq,
    bucketCriteriaAdditionalProperties_lte,
    bucketCriteriaAdditionalProperties_lt,
    bucketCriteriaAdditionalProperties_gte,
    bucketCriteriaAdditionalProperties_prefix,
    bucketCriteriaAdditionalProperties_eq,
    bucketCriteriaAdditionalProperties_gt,

    -- * BucketLevelPermissions
    BucketLevelPermissions (..),
    newBucketLevelPermissions,
    bucketLevelPermissions_accessControlList,
    bucketLevelPermissions_blockPublicAccess,
    bucketLevelPermissions_bucketPolicy,

    -- * BucketMetadata
    BucketMetadata (..),
    newBucketMetadata,
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
    cell_columnName,
    cell_column,

    -- * ClassificationDetails
    ClassificationDetails (..),
    newClassificationDetails,
    classificationDetails_originType,
    classificationDetails_jobId,
    classificationDetails_detailedResultsLocation,
    classificationDetails_result,
    classificationDetails_jobArn,

    -- * ClassificationExportConfiguration
    ClassificationExportConfiguration (..),
    newClassificationExportConfiguration,
    classificationExportConfiguration_s3Destination,

    -- * ClassificationResult
    ClassificationResult (..),
    newClassificationResult,
    classificationResult_status,
    classificationResult_additionalOccurrences,
    classificationResult_customDataIdentifiers,
    classificationResult_mimeType,
    classificationResult_sizeClassified,
    classificationResult_sensitiveData,

    -- * ClassificationResultStatus
    ClassificationResultStatus (..),
    newClassificationResultStatus,
    classificationResultStatus_code,
    classificationResultStatus_reason,

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
    criterionAdditionalProperties_neq,
    criterionAdditionalProperties_lte,
    criterionAdditionalProperties_lt,
    criterionAdditionalProperties_gte,
    criterionAdditionalProperties_eq,
    criterionAdditionalProperties_gt,
    criterionAdditionalProperties_eqExactMatch,

    -- * CustomDataIdentifierSummary
    CustomDataIdentifierSummary (..),
    newCustomDataIdentifierSummary,
    customDataIdentifierSummary_name,
    customDataIdentifierSummary_arn,
    customDataIdentifierSummary_description,
    customDataIdentifierSummary_id,
    customDataIdentifierSummary_createdAt,

    -- * CustomDataIdentifiers
    CustomDataIdentifiers (..),
    newCustomDataIdentifiers,
    customDataIdentifiers_detections,
    customDataIdentifiers_totalCount,

    -- * CustomDetection
    CustomDetection (..),
    newCustomDetection,
    customDetection_occurrences,
    customDetection_name,
    customDetection_arn,
    customDetection_count,

    -- * DailySchedule
    DailySchedule (..),
    newDailySchedule,

    -- * DefaultDetection
    DefaultDetection (..),
    newDefaultDetection,
    defaultDetection_occurrences,
    defaultDetection_type,
    defaultDetection_count,

    -- * DetectedDataDetails
    DetectedDataDetails (..),
    newDetectedDataDetails,
    detectedDataDetails_value,

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

    -- * FindingAction
    FindingAction (..),
    newFindingAction,
    findingAction_actionType,
    findingAction_apiCallDetails,

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
    findingsFilterListItem_tags,
    findingsFilterListItem_name,
    findingsFilterListItem_arn,
    findingsFilterListItem_id,
    findingsFilterListItem_action,

    -- * GroupCount
    GroupCount (..),
    newGroupCount,
    groupCount_groupKey,
    groupCount_count,

    -- * IamUser
    IamUser (..),
    newIamUser,
    iamUser_principalId,
    iamUser_userName,
    iamUser_arn,
    iamUser_accountId,

    -- * Invitation
    Invitation (..),
    newInvitation,
    invitation_accountId,
    invitation_invitedAt,
    invitation_relationshipStatus,
    invitation_invitationId,

    -- * IpAddressDetails
    IpAddressDetails (..),
    newIpAddressDetails,
    ipAddressDetails_ipCountry,
    ipAddressDetails_ipAddressV4,
    ipAddressDetails_ipOwner,
    ipAddressDetails_ipGeoLocation,
    ipAddressDetails_ipCity,

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
    ipOwner_isp,
    ipOwner_org,
    ipOwner_asn,
    ipOwner_asnOrg,

    -- * JobDetails
    JobDetails (..),
    newJobDetails,
    jobDetails_lastJobId,
    jobDetails_isDefinedInJob,
    jobDetails_lastJobRunTime,
    jobDetails_isMonitoredByJob,

    -- * JobScheduleFrequency
    JobScheduleFrequency (..),
    newJobScheduleFrequency,
    jobScheduleFrequency_dailySchedule,
    jobScheduleFrequency_monthlySchedule,
    jobScheduleFrequency_weeklySchedule,

    -- * JobScopeTerm
    JobScopeTerm (..),
    newJobScopeTerm,
    jobScopeTerm_tagScopeTerm,
    jobScopeTerm_simpleScopeTerm,

    -- * JobScopingBlock
    JobScopingBlock (..),
    newJobScopingBlock,
    jobScopingBlock_and,

    -- * JobSummary
    JobSummary (..),
    newJobSummary,
    jobSummary_bucketCriteria,
    jobSummary_name,
    jobSummary_jobStatus,
    jobSummary_userPausedDetails,
    jobSummary_bucketDefinitions,
    jobSummary_jobId,
    jobSummary_lastRunErrorStatus,
    jobSummary_createdAt,
    jobSummary_jobType,

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
    listJobsFilterTerm_key,
    listJobsFilterTerm_comparator,
    listJobsFilterTerm_values,

    -- * ListJobsSortCriteria
    ListJobsSortCriteria (..),
    newListJobsSortCriteria,
    listJobsSortCriteria_orderBy,
    listJobsSortCriteria_attributeName,

    -- * ManagedDataIdentifierSummary
    ManagedDataIdentifierSummary (..),
    newManagedDataIdentifierSummary,
    managedDataIdentifierSummary_id,
    managedDataIdentifierSummary_category,

    -- * MatchingBucket
    MatchingBucket (..),
    newMatchingBucket,
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

    -- * MatchingResource
    MatchingResource (..),
    newMatchingResource,
    matchingResource_matchingBucket,

    -- * Member
    Member (..),
    newMember,
    member_tags,
    member_email,
    member_arn,
    member_masterAccountId,
    member_accountId,
    member_invitedAt,
    member_administratorAccountId,
    member_relationshipStatus,
    member_updatedAt,

    -- * MonthlySchedule
    MonthlySchedule (..),
    newMonthlySchedule,
    monthlySchedule_dayOfMonth,

    -- * ObjectCountByEncryptionType
    ObjectCountByEncryptionType (..),
    newObjectCountByEncryptionType,
    objectCountByEncryptionType_s3Managed,
    objectCountByEncryptionType_customerManaged,
    objectCountByEncryptionType_unencrypted,
    objectCountByEncryptionType_kmsManaged,
    objectCountByEncryptionType_unknown,

    -- * ObjectLevelStatistics
    ObjectLevelStatistics (..),
    newObjectLevelStatistics,
    objectLevelStatistics_total,
    objectLevelStatistics_fileType,
    objectLevelStatistics_storageClass,

    -- * Occurrences
    Occurrences (..),
    newOccurrences,
    occurrences_records,
    occurrences_lineRanges,
    occurrences_offsetRanges,
    occurrences_cells,
    occurrences_pages,

    -- * Page
    Page (..),
    newPage,
    page_offsetRange,
    page_pageNumber,
    page_lineRange,

    -- * PolicyDetails
    PolicyDetails (..),
    newPolicyDetails,
    policyDetails_action,
    policyDetails_actor,

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
    replicationDetails_replicatedExternally,
    replicationDetails_replicated,
    replicationDetails_replicationAccounts,

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
    s3Bucket_tags,
    s3Bucket_name,
    s3Bucket_defaultServerSideEncryption,
    s3Bucket_arn,
    s3Bucket_allowsUnencryptedObjectUploads,
    s3Bucket_publicAccess,
    s3Bucket_owner,
    s3Bucket_createdAt,

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
    searchResourcesCriteria_tagCriterion,
    searchResourcesCriteria_simpleCriterion,

    -- * SearchResourcesCriteriaBlock
    SearchResourcesCriteriaBlock (..),
    newSearchResourcesCriteriaBlock,
    searchResourcesCriteriaBlock_and,

    -- * SearchResourcesSimpleCriterion
    SearchResourcesSimpleCriterion (..),
    newSearchResourcesSimpleCriterion,
    searchResourcesSimpleCriterion_key,
    searchResourcesSimpleCriterion_comparator,
    searchResourcesSimpleCriterion_values,

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
    sensitiveDataItem_detections,
    sensitiveDataItem_category,
    sensitiveDataItem_totalCount,

    -- * ServerSideEncryption
    ServerSideEncryption (..),
    newServerSideEncryption,
    serverSideEncryption_kmsMasterKeyId,
    serverSideEncryption_encryptionType,

    -- * ServiceLimit
    ServiceLimit (..),
    newServiceLimit,
    serviceLimit_isServiceLimited,
    serviceLimit_unit,
    serviceLimit_value,

    -- * SessionContext
    SessionContext (..),
    newSessionContext,
    sessionContext_sessionIssuer,
    sessionContext_attributes,

    -- * SessionContextAttributes
    SessionContextAttributes (..),
    newSessionContextAttributes,
    sessionContextAttributes_mfaAuthenticated,
    sessionContextAttributes_creationDate,

    -- * SessionIssuer
    SessionIssuer (..),
    newSessionIssuer,
    sessionIssuer_principalId,
    sessionIssuer_type,
    sessionIssuer_userName,
    sessionIssuer_arn,
    sessionIssuer_accountId,

    -- * Severity
    Severity (..),
    newSeverity,
    severity_score,
    severity_description,

    -- * SeverityLevel
    SeverityLevel (..),
    newSeverityLevel,
    severityLevel_occurrencesThreshold,
    severityLevel_severity,

    -- * SimpleCriterionForJob
    SimpleCriterionForJob (..),
    newSimpleCriterionForJob,
    simpleCriterionForJob_key,
    simpleCriterionForJob_comparator,
    simpleCriterionForJob_values,

    -- * SimpleScopeTerm
    SimpleScopeTerm (..),
    newSimpleScopeTerm,
    simpleScopeTerm_key,
    simpleScopeTerm_comparator,
    simpleScopeTerm_values,

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
    tagCriterionPairForJob_key,
    tagCriterionPairForJob_value,

    -- * TagScopeTerm
    TagScopeTerm (..),
    newTagScopeTerm,
    tagScopeTerm_key,
    tagScopeTerm_tagValues,
    tagScopeTerm_target,
    tagScopeTerm_comparator,

    -- * TagValuePair
    TagValuePair (..),
    newTagValuePair,
    tagValuePair_key,
    tagValuePair_value,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_errorMessage,
    unprocessedAccount_accountId,
    unprocessedAccount_errorCode,

    -- * UsageByAccount
    UsageByAccount (..),
    newUsageByAccount,
    usageByAccount_type,
    usageByAccount_serviceLimit,
    usageByAccount_currency,
    usageByAccount_estimatedCost,

    -- * UsageRecord
    UsageRecord (..),
    newUsageRecord,
    usageRecord_usage,
    usageRecord_accountId,
    usageRecord_freeTrialStartDate,

    -- * UsageStatisticsFilter
    UsageStatisticsFilter (..),
    newUsageStatisticsFilter,
    usageStatisticsFilter_key,
    usageStatisticsFilter_comparator,
    usageStatisticsFilter_values,

    -- * UsageStatisticsSortBy
    UsageStatisticsSortBy (..),
    newUsageStatisticsSortBy,
    usageStatisticsSortBy_key,
    usageStatisticsSortBy_orderBy,

    -- * UsageTotal
    UsageTotal (..),
    newUsageTotal,
    usageTotal_type,
    usageTotal_currency,
    usageTotal_estimatedCost,

    -- * UserIdentity
    UserIdentity (..),
    newUserIdentity,
    userIdentity_type,
    userIdentity_awsAccount,
    userIdentity_federatedUser,
    userIdentity_iamUser,
    userIdentity_root,
    userIdentity_assumedRole,
    userIdentity_awsService,

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
    userPausedDetails_jobPausedAt,
    userPausedDetails_jobImminentExpirationHealthEventArn,

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
import Amazonka.MacieV2.Types.Cell
import Amazonka.MacieV2.Types.ClassificationDetails
import Amazonka.MacieV2.Types.ClassificationExportConfiguration
import Amazonka.MacieV2.Types.ClassificationResult
import Amazonka.MacieV2.Types.ClassificationResultStatus
import Amazonka.MacieV2.Types.CriteriaBlockForJob
import Amazonka.MacieV2.Types.CriteriaForJob
import Amazonka.MacieV2.Types.CriterionAdditionalProperties
import Amazonka.MacieV2.Types.Currency
import Amazonka.MacieV2.Types.CustomDataIdentifierSummary
import Amazonka.MacieV2.Types.CustomDataIdentifiers
import Amazonka.MacieV2.Types.CustomDetection
import Amazonka.MacieV2.Types.DailySchedule
import Amazonka.MacieV2.Types.DataIdentifierSeverity
import Amazonka.MacieV2.Types.DayOfWeek
import Amazonka.MacieV2.Types.DefaultDetection
import Amazonka.MacieV2.Types.DetectedDataDetails
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
import Amazonka.MacieV2.Types.ResourcesAffected
import Amazonka.MacieV2.Types.RevealConfiguration
import Amazonka.MacieV2.Types.RevealRequestStatus
import Amazonka.MacieV2.Types.RevealStatus
import Amazonka.MacieV2.Types.S3Bucket
import Amazonka.MacieV2.Types.S3BucketCriteriaForJob
import Amazonka.MacieV2.Types.S3BucketDefinitionForJob
import Amazonka.MacieV2.Types.S3BucketOwner
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

-- | Provides information about an error that occurred due to insufficient
-- access to a specified resource.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Provides information about an error that occurred due to an unknown
-- internal server error, exception, or failure.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Provides information about an error that occurred due to one or more
-- service quotas for an account.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Provides information about an error that occurred because a specified
-- resource wasn\'t found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Provides information about an error that occurred due to an
-- unprocessable entity.
_UnprocessableEntityException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnprocessableEntityException =
  Core._MatchServiceError
    defaultService
    "UnprocessableEntityException"
    Prelude.. Core.hasStatus 422

-- | Provides information about an error that occurred due to a versioning
-- conflict for a specified resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Provides information about an error that occurred because too many
-- requests were sent during a certain amount of time.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Provides information about an error that occurred due to a syntax error
-- in a request.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
