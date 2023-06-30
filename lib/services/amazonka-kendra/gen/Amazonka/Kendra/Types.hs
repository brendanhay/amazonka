{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _InvalidRequestException,
    _ResourceAlreadyExistException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceUnavailableException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AdditionalResultAttributeValueType
    AdditionalResultAttributeValueType (..),

    -- * AlfrescoEntity
    AlfrescoEntity (..),

    -- * ConditionOperator
    ConditionOperator (..),

    -- * ConfluenceAttachmentFieldName
    ConfluenceAttachmentFieldName (..),

    -- * ConfluenceAuthenticationType
    ConfluenceAuthenticationType (..),

    -- * ConfluenceBlogFieldName
    ConfluenceBlogFieldName (..),

    -- * ConfluencePageFieldName
    ConfluencePageFieldName (..),

    -- * ConfluenceSpaceFieldName
    ConfluenceSpaceFieldName (..),

    -- * ConfluenceVersion
    ConfluenceVersion (..),

    -- * ContentType
    ContentType (..),

    -- * DataSourceStatus
    DataSourceStatus (..),

    -- * DataSourceSyncJobStatus
    DataSourceSyncJobStatus (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * DatabaseEngineType
    DatabaseEngineType (..),

    -- * DocumentAttributeValueType
    DocumentAttributeValueType (..),

    -- * DocumentStatus
    DocumentStatus (..),

    -- * EndpointType
    EndpointType (..),

    -- * EntityType
    EntityType (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ExperienceStatus
    ExperienceStatus (..),

    -- * FaqFileFormat
    FaqFileFormat (..),

    -- * FaqStatus
    FaqStatus (..),

    -- * FsxFileSystemType
    FsxFileSystemType (..),

    -- * HighlightType
    HighlightType (..),

    -- * IndexEdition
    IndexEdition (..),

    -- * IndexStatus
    IndexStatus (..),

    -- * Interval
    Interval (..),

    -- * IssueSubEntity
    IssueSubEntity (..),

    -- * KeyLocation
    KeyLocation (..),

    -- * MetricType
    MetricType (..),

    -- * Mode
    Mode (..),

    -- * Order
    Order (..),

    -- * Persona
    Persona (..),

    -- * PrincipalMappingStatus
    PrincipalMappingStatus (..),

    -- * PrincipalType
    PrincipalType (..),

    -- * QueryIdentifiersEnclosingOption
    QueryIdentifiersEnclosingOption (..),

    -- * QueryResultFormat
    QueryResultFormat (..),

    -- * QueryResultType
    QueryResultType (..),

    -- * QuerySuggestionsBlockListStatus
    QuerySuggestionsBlockListStatus (..),

    -- * QuerySuggestionsStatus
    QuerySuggestionsStatus (..),

    -- * ReadAccessType
    ReadAccessType (..),

    -- * RelevanceType
    RelevanceType (..),

    -- * SalesforceChatterFeedIncludeFilterType
    SalesforceChatterFeedIncludeFilterType (..),

    -- * SalesforceKnowledgeArticleState
    SalesforceKnowledgeArticleState (..),

    -- * SalesforceStandardObjectName
    SalesforceStandardObjectName (..),

    -- * ScoreConfidence
    ScoreConfidence (..),

    -- * ServiceNowAuthenticationType
    ServiceNowAuthenticationType (..),

    -- * ServiceNowBuildVersionType
    ServiceNowBuildVersionType (..),

    -- * SharePointOnlineAuthenticationType
    SharePointOnlineAuthenticationType (..),

    -- * SharePointVersion
    SharePointVersion (..),

    -- * SlackEntity
    SlackEntity (..),

    -- * SortOrder
    SortOrder (..),

    -- * ThesaurusStatus
    ThesaurusStatus (..),

    -- * Type
    Type (..),

    -- * UserContextPolicy
    UserContextPolicy (..),

    -- * UserGroupResolutionMode
    UserGroupResolutionMode (..),

    -- * WarningCode
    WarningCode (..),

    -- * WebCrawlerMode
    WebCrawlerMode (..),

    -- * AccessControlConfigurationSummary
    AccessControlConfigurationSummary (..),
    newAccessControlConfigurationSummary,
    accessControlConfigurationSummary_id,

    -- * AccessControlListConfiguration
    AccessControlListConfiguration (..),
    newAccessControlListConfiguration,
    accessControlListConfiguration_keyPath,

    -- * AclConfiguration
    AclConfiguration (..),
    newAclConfiguration,
    aclConfiguration_allowedGroupsColumnName,

    -- * AdditionalResultAttribute
    AdditionalResultAttribute (..),
    newAdditionalResultAttribute,
    additionalResultAttribute_key,
    additionalResultAttribute_valueType,
    additionalResultAttribute_value,

    -- * AdditionalResultAttributeValue
    AdditionalResultAttributeValue (..),
    newAdditionalResultAttributeValue,
    additionalResultAttributeValue_textWithHighlightsValue,

    -- * AlfrescoConfiguration
    AlfrescoConfiguration (..),
    newAlfrescoConfiguration,
    alfrescoConfiguration_blogFieldMappings,
    alfrescoConfiguration_crawlComments,
    alfrescoConfiguration_crawlSystemFolders,
    alfrescoConfiguration_documentLibraryFieldMappings,
    alfrescoConfiguration_entityFilter,
    alfrescoConfiguration_exclusionPatterns,
    alfrescoConfiguration_inclusionPatterns,
    alfrescoConfiguration_vpcConfiguration,
    alfrescoConfiguration_wikiFieldMappings,
    alfrescoConfiguration_siteUrl,
    alfrescoConfiguration_siteId,
    alfrescoConfiguration_secretArn,
    alfrescoConfiguration_sslCertificateS3Path,

    -- * AttributeFilter
    AttributeFilter (..),
    newAttributeFilter,
    attributeFilter_andAllFilters,
    attributeFilter_containsAll,
    attributeFilter_containsAny,
    attributeFilter_equalsTo,
    attributeFilter_greaterThan,
    attributeFilter_greaterThanOrEquals,
    attributeFilter_lessThan,
    attributeFilter_lessThanOrEquals,
    attributeFilter_notFilter,
    attributeFilter_orAllFilters,

    -- * AuthenticationConfiguration
    AuthenticationConfiguration (..),
    newAuthenticationConfiguration,
    authenticationConfiguration_basicAuthentication,

    -- * BasicAuthenticationConfiguration
    BasicAuthenticationConfiguration (..),
    newBasicAuthenticationConfiguration,
    basicAuthenticationConfiguration_host,
    basicAuthenticationConfiguration_port,
    basicAuthenticationConfiguration_credentials,

    -- * BatchDeleteDocumentResponseFailedDocument
    BatchDeleteDocumentResponseFailedDocument (..),
    newBatchDeleteDocumentResponseFailedDocument,
    batchDeleteDocumentResponseFailedDocument_errorCode,
    batchDeleteDocumentResponseFailedDocument_errorMessage,
    batchDeleteDocumentResponseFailedDocument_id,

    -- * BatchGetDocumentStatusResponseError
    BatchGetDocumentStatusResponseError (..),
    newBatchGetDocumentStatusResponseError,
    batchGetDocumentStatusResponseError_documentId,
    batchGetDocumentStatusResponseError_errorCode,
    batchGetDocumentStatusResponseError_errorMessage,

    -- * BatchPutDocumentResponseFailedDocument
    BatchPutDocumentResponseFailedDocument (..),
    newBatchPutDocumentResponseFailedDocument,
    batchPutDocumentResponseFailedDocument_errorCode,
    batchPutDocumentResponseFailedDocument_errorMessage,
    batchPutDocumentResponseFailedDocument_id,

    -- * BoxConfiguration
    BoxConfiguration (..),
    newBoxConfiguration,
    boxConfiguration_commentFieldMappings,
    boxConfiguration_crawlComments,
    boxConfiguration_crawlTasks,
    boxConfiguration_crawlWebLinks,
    boxConfiguration_exclusionPatterns,
    boxConfiguration_fileFieldMappings,
    boxConfiguration_inclusionPatterns,
    boxConfiguration_taskFieldMappings,
    boxConfiguration_useChangeLog,
    boxConfiguration_vpcConfiguration,
    boxConfiguration_webLinkFieldMappings,
    boxConfiguration_enterpriseId,
    boxConfiguration_secretArn,

    -- * CapacityUnitsConfiguration
    CapacityUnitsConfiguration (..),
    newCapacityUnitsConfiguration,
    capacityUnitsConfiguration_storageCapacityUnits,
    capacityUnitsConfiguration_queryCapacityUnits,

    -- * ClickFeedback
    ClickFeedback (..),
    newClickFeedback,
    clickFeedback_resultId,
    clickFeedback_clickTime,

    -- * ColumnConfiguration
    ColumnConfiguration (..),
    newColumnConfiguration,
    columnConfiguration_documentTitleColumnName,
    columnConfiguration_fieldMappings,
    columnConfiguration_documentIdColumnName,
    columnConfiguration_documentDataColumnName,
    columnConfiguration_changeDetectingColumns,

    -- * ConfluenceAttachmentConfiguration
    ConfluenceAttachmentConfiguration (..),
    newConfluenceAttachmentConfiguration,
    confluenceAttachmentConfiguration_attachmentFieldMappings,
    confluenceAttachmentConfiguration_crawlAttachments,

    -- * ConfluenceAttachmentToIndexFieldMapping
    ConfluenceAttachmentToIndexFieldMapping (..),
    newConfluenceAttachmentToIndexFieldMapping,
    confluenceAttachmentToIndexFieldMapping_dataSourceFieldName,
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,
    confluenceAttachmentToIndexFieldMapping_indexFieldName,

    -- * ConfluenceBlogConfiguration
    ConfluenceBlogConfiguration (..),
    newConfluenceBlogConfiguration,
    confluenceBlogConfiguration_blogFieldMappings,

    -- * ConfluenceBlogToIndexFieldMapping
    ConfluenceBlogToIndexFieldMapping (..),
    newConfluenceBlogToIndexFieldMapping,
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_dateFieldFormat,
    confluenceBlogToIndexFieldMapping_indexFieldName,

    -- * ConfluenceConfiguration
    ConfluenceConfiguration (..),
    newConfluenceConfiguration,
    confluenceConfiguration_attachmentConfiguration,
    confluenceConfiguration_authenticationType,
    confluenceConfiguration_blogConfiguration,
    confluenceConfiguration_exclusionPatterns,
    confluenceConfiguration_inclusionPatterns,
    confluenceConfiguration_pageConfiguration,
    confluenceConfiguration_proxyConfiguration,
    confluenceConfiguration_spaceConfiguration,
    confluenceConfiguration_vpcConfiguration,
    confluenceConfiguration_serverUrl,
    confluenceConfiguration_secretArn,
    confluenceConfiguration_version,

    -- * ConfluencePageConfiguration
    ConfluencePageConfiguration (..),
    newConfluencePageConfiguration,
    confluencePageConfiguration_pageFieldMappings,

    -- * ConfluencePageToIndexFieldMapping
    ConfluencePageToIndexFieldMapping (..),
    newConfluencePageToIndexFieldMapping,
    confluencePageToIndexFieldMapping_dataSourceFieldName,
    confluencePageToIndexFieldMapping_dateFieldFormat,
    confluencePageToIndexFieldMapping_indexFieldName,

    -- * ConfluenceSpaceConfiguration
    ConfluenceSpaceConfiguration (..),
    newConfluenceSpaceConfiguration,
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_excludeSpaces,
    confluenceSpaceConfiguration_includeSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,

    -- * ConfluenceSpaceToIndexFieldMapping
    ConfluenceSpaceToIndexFieldMapping (..),
    newConfluenceSpaceToIndexFieldMapping,
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,
    confluenceSpaceToIndexFieldMapping_indexFieldName,

    -- * ConnectionConfiguration
    ConnectionConfiguration (..),
    newConnectionConfiguration,
    connectionConfiguration_databaseHost,
    connectionConfiguration_databasePort,
    connectionConfiguration_databaseName,
    connectionConfiguration_tableName,
    connectionConfiguration_secretArn,

    -- * ContentSourceConfiguration
    ContentSourceConfiguration (..),
    newContentSourceConfiguration,
    contentSourceConfiguration_dataSourceIds,
    contentSourceConfiguration_directPutContent,
    contentSourceConfiguration_faqIds,

    -- * Correction
    Correction (..),
    newCorrection,
    correction_beginOffset,
    correction_correctedTerm,
    correction_endOffset,
    correction_term,

    -- * CustomDocumentEnrichmentConfiguration
    CustomDocumentEnrichmentConfiguration (..),
    newCustomDocumentEnrichmentConfiguration,
    customDocumentEnrichmentConfiguration_inlineConfigurations,
    customDocumentEnrichmentConfiguration_postExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_preExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_roleArn,

    -- * DataSourceConfiguration
    DataSourceConfiguration (..),
    newDataSourceConfiguration,
    dataSourceConfiguration_alfrescoConfiguration,
    dataSourceConfiguration_boxConfiguration,
    dataSourceConfiguration_confluenceConfiguration,
    dataSourceConfiguration_databaseConfiguration,
    dataSourceConfiguration_fsxConfiguration,
    dataSourceConfiguration_gitHubConfiguration,
    dataSourceConfiguration_googleDriveConfiguration,
    dataSourceConfiguration_jiraConfiguration,
    dataSourceConfiguration_oneDriveConfiguration,
    dataSourceConfiguration_quipConfiguration,
    dataSourceConfiguration_s3Configuration,
    dataSourceConfiguration_salesforceConfiguration,
    dataSourceConfiguration_serviceNowConfiguration,
    dataSourceConfiguration_sharePointConfiguration,
    dataSourceConfiguration_slackConfiguration,
    dataSourceConfiguration_templateConfiguration,
    dataSourceConfiguration_webCrawlerConfiguration,
    dataSourceConfiguration_workDocsConfiguration,

    -- * DataSourceGroup
    DataSourceGroup (..),
    newDataSourceGroup,
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- * DataSourceSummary
    DataSourceSummary (..),
    newDataSourceSummary,
    dataSourceSummary_createdAt,
    dataSourceSummary_id,
    dataSourceSummary_languageCode,
    dataSourceSummary_name,
    dataSourceSummary_status,
    dataSourceSummary_type,
    dataSourceSummary_updatedAt,

    -- * DataSourceSyncJob
    DataSourceSyncJob (..),
    newDataSourceSyncJob,
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_errorMessage,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_startTime,
    dataSourceSyncJob_status,

    -- * DataSourceSyncJobMetricTarget
    DataSourceSyncJobMetricTarget (..),
    newDataSourceSyncJobMetricTarget,
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- * DataSourceSyncJobMetrics
    DataSourceSyncJobMetrics (..),
    newDataSourceSyncJobMetrics,
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsDeleted,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsModified,
    dataSourceSyncJobMetrics_documentsScanned,

    -- * DataSourceToIndexFieldMapping
    DataSourceToIndexFieldMapping (..),
    newDataSourceToIndexFieldMapping,
    dataSourceToIndexFieldMapping_dateFieldFormat,
    dataSourceToIndexFieldMapping_dataSourceFieldName,
    dataSourceToIndexFieldMapping_indexFieldName,

    -- * DataSourceVpcConfiguration
    DataSourceVpcConfiguration (..),
    newDataSourceVpcConfiguration,
    dataSourceVpcConfiguration_subnetIds,
    dataSourceVpcConfiguration_securityGroupIds,

    -- * DatabaseConfiguration
    DatabaseConfiguration (..),
    newDatabaseConfiguration,
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- * Document
    Document (..),
    newDocument,
    document_accessControlConfigurationId,
    document_accessControlList,
    document_attributes,
    document_blob,
    document_contentType,
    document_hierarchicalAccessControlList,
    document_s3Path,
    document_title,
    document_id,

    -- * DocumentAttribute
    DocumentAttribute (..),
    newDocumentAttribute,
    documentAttribute_key,
    documentAttribute_value,

    -- * DocumentAttributeCondition
    DocumentAttributeCondition (..),
    newDocumentAttributeCondition,
    documentAttributeCondition_conditionOnValue,
    documentAttributeCondition_conditionDocumentAttributeKey,
    documentAttributeCondition_operator,

    -- * DocumentAttributeTarget
    DocumentAttributeTarget (..),
    newDocumentAttributeTarget,
    documentAttributeTarget_targetDocumentAttributeKey,
    documentAttributeTarget_targetDocumentAttributeValue,
    documentAttributeTarget_targetDocumentAttributeValueDeletion,

    -- * DocumentAttributeValue
    DocumentAttributeValue (..),
    newDocumentAttributeValue,
    documentAttributeValue_dateValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,
    documentAttributeValue_stringValue,

    -- * DocumentAttributeValueCountPair
    DocumentAttributeValueCountPair (..),
    newDocumentAttributeValueCountPair,
    documentAttributeValueCountPair_count,
    documentAttributeValueCountPair_documentAttributeValue,
    documentAttributeValueCountPair_facetResults,

    -- * DocumentInfo
    DocumentInfo (..),
    newDocumentInfo,
    documentInfo_attributes,
    documentInfo_documentId,

    -- * DocumentMetadataConfiguration
    DocumentMetadataConfiguration (..),
    newDocumentMetadataConfiguration,
    documentMetadataConfiguration_relevance,
    documentMetadataConfiguration_search,
    documentMetadataConfiguration_name,
    documentMetadataConfiguration_type,

    -- * DocumentRelevanceConfiguration
    DocumentRelevanceConfiguration (..),
    newDocumentRelevanceConfiguration,
    documentRelevanceConfiguration_name,
    documentRelevanceConfiguration_relevance,

    -- * DocumentsMetadataConfiguration
    DocumentsMetadataConfiguration (..),
    newDocumentsMetadataConfiguration,
    documentsMetadataConfiguration_s3Prefix,

    -- * EntityConfiguration
    EntityConfiguration (..),
    newEntityConfiguration,
    entityConfiguration_entityId,
    entityConfiguration_entityType,

    -- * EntityDisplayData
    EntityDisplayData (..),
    newEntityDisplayData,
    entityDisplayData_firstName,
    entityDisplayData_groupName,
    entityDisplayData_identifiedUserName,
    entityDisplayData_lastName,
    entityDisplayData_userName,

    -- * EntityPersonaConfiguration
    EntityPersonaConfiguration (..),
    newEntityPersonaConfiguration,
    entityPersonaConfiguration_entityId,
    entityPersonaConfiguration_persona,

    -- * ExperienceConfiguration
    ExperienceConfiguration (..),
    newExperienceConfiguration,
    experienceConfiguration_contentSourceConfiguration,
    experienceConfiguration_userIdentityConfiguration,

    -- * ExperienceEndpoint
    ExperienceEndpoint (..),
    newExperienceEndpoint,
    experienceEndpoint_endpoint,
    experienceEndpoint_endpointType,

    -- * ExperienceEntitiesSummary
    ExperienceEntitiesSummary (..),
    newExperienceEntitiesSummary,
    experienceEntitiesSummary_displayData,
    experienceEntitiesSummary_entityId,
    experienceEntitiesSummary_entityType,

    -- * ExperiencesSummary
    ExperiencesSummary (..),
    newExperiencesSummary,
    experiencesSummary_createdAt,
    experiencesSummary_endpoints,
    experiencesSummary_id,
    experiencesSummary_name,
    experiencesSummary_status,

    -- * Facet
    Facet (..),
    newFacet,
    facet_documentAttributeKey,
    facet_facets,
    facet_maxResults,

    -- * FacetResult
    FacetResult (..),
    newFacetResult,
    facetResult_documentAttributeKey,
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,

    -- * FailedEntity
    FailedEntity (..),
    newFailedEntity,
    failedEntity_entityId,
    failedEntity_errorMessage,

    -- * FaqStatistics
    FaqStatistics (..),
    newFaqStatistics,
    faqStatistics_indexedQuestionAnswersCount,

    -- * FaqSummary
    FaqSummary (..),
    newFaqSummary,
    faqSummary_createdAt,
    faqSummary_fileFormat,
    faqSummary_id,
    faqSummary_languageCode,
    faqSummary_name,
    faqSummary_status,
    faqSummary_updatedAt,

    -- * FsxConfiguration
    FsxConfiguration (..),
    newFsxConfiguration,
    fsxConfiguration_exclusionPatterns,
    fsxConfiguration_fieldMappings,
    fsxConfiguration_inclusionPatterns,
    fsxConfiguration_secretArn,
    fsxConfiguration_fileSystemId,
    fsxConfiguration_fileSystemType,
    fsxConfiguration_vpcConfiguration,

    -- * GitHubConfiguration
    GitHubConfiguration (..),
    newGitHubConfiguration,
    gitHubConfiguration_exclusionFileNamePatterns,
    gitHubConfiguration_exclusionFileTypePatterns,
    gitHubConfiguration_exclusionFolderNamePatterns,
    gitHubConfiguration_gitHubCommitConfigurationFieldMappings,
    gitHubConfiguration_gitHubDocumentCrawlProperties,
    gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings,
    gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings,
    gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings,
    gitHubConfiguration_gitHubPullRequestCommentConfigurationFieldMappings,
    gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings,
    gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings,
    gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings,
    gitHubConfiguration_inclusionFileNamePatterns,
    gitHubConfiguration_inclusionFileTypePatterns,
    gitHubConfiguration_inclusionFolderNamePatterns,
    gitHubConfiguration_onPremiseConfiguration,
    gitHubConfiguration_repositoryFilter,
    gitHubConfiguration_saaSConfiguration,
    gitHubConfiguration_type,
    gitHubConfiguration_useChangeLog,
    gitHubConfiguration_vpcConfiguration,
    gitHubConfiguration_secretArn,

    -- * GitHubDocumentCrawlProperties
    GitHubDocumentCrawlProperties (..),
    newGitHubDocumentCrawlProperties,
    gitHubDocumentCrawlProperties_crawlIssue,
    gitHubDocumentCrawlProperties_crawlIssueComment,
    gitHubDocumentCrawlProperties_crawlIssueCommentAttachment,
    gitHubDocumentCrawlProperties_crawlPullRequest,
    gitHubDocumentCrawlProperties_crawlPullRequestComment,
    gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment,
    gitHubDocumentCrawlProperties_crawlRepositoryDocuments,

    -- * GoogleDriveConfiguration
    GoogleDriveConfiguration (..),
    newGoogleDriveConfiguration,
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_secretArn,

    -- * GroupMembers
    GroupMembers (..),
    newGroupMembers,
    groupMembers_memberGroups,
    groupMembers_memberUsers,
    groupMembers_s3PathforGroupMembers,

    -- * GroupOrderingIdSummary
    GroupOrderingIdSummary (..),
    newGroupOrderingIdSummary,
    groupOrderingIdSummary_failureReason,
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_orderingId,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_status,

    -- * GroupSummary
    GroupSummary (..),
    newGroupSummary,
    groupSummary_groupId,
    groupSummary_orderingId,

    -- * HierarchicalPrincipal
    HierarchicalPrincipal (..),
    newHierarchicalPrincipal,
    hierarchicalPrincipal_principalList,

    -- * Highlight
    Highlight (..),
    newHighlight,
    highlight_topAnswer,
    highlight_type,
    highlight_beginOffset,
    highlight_endOffset,

    -- * HookConfiguration
    HookConfiguration (..),
    newHookConfiguration,
    hookConfiguration_invocationCondition,
    hookConfiguration_lambdaArn,
    hookConfiguration_s3Bucket,

    -- * IndexConfigurationSummary
    IndexConfigurationSummary (..),
    newIndexConfigurationSummary,
    indexConfigurationSummary_edition,
    indexConfigurationSummary_id,
    indexConfigurationSummary_name,
    indexConfigurationSummary_createdAt,
    indexConfigurationSummary_updatedAt,
    indexConfigurationSummary_status,

    -- * IndexStatistics
    IndexStatistics (..),
    newIndexStatistics,
    indexStatistics_faqStatistics,
    indexStatistics_textDocumentStatistics,

    -- * InlineCustomDocumentEnrichmentConfiguration
    InlineCustomDocumentEnrichmentConfiguration (..),
    newInlineCustomDocumentEnrichmentConfiguration,
    inlineCustomDocumentEnrichmentConfiguration_condition,
    inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion,
    inlineCustomDocumentEnrichmentConfiguration_target,

    -- * JiraConfiguration
    JiraConfiguration (..),
    newJiraConfiguration,
    jiraConfiguration_attachmentFieldMappings,
    jiraConfiguration_commentFieldMappings,
    jiraConfiguration_exclusionPatterns,
    jiraConfiguration_inclusionPatterns,
    jiraConfiguration_issueFieldMappings,
    jiraConfiguration_issueSubEntityFilter,
    jiraConfiguration_issueType,
    jiraConfiguration_project,
    jiraConfiguration_projectFieldMappings,
    jiraConfiguration_status,
    jiraConfiguration_useChangeLog,
    jiraConfiguration_vpcConfiguration,
    jiraConfiguration_workLogFieldMappings,
    jiraConfiguration_jiraAccountUrl,
    jiraConfiguration_secretArn,

    -- * JsonTokenTypeConfiguration
    JsonTokenTypeConfiguration (..),
    newJsonTokenTypeConfiguration,
    jsonTokenTypeConfiguration_userNameAttributeField,
    jsonTokenTypeConfiguration_groupAttributeField,

    -- * JwtTokenTypeConfiguration
    JwtTokenTypeConfiguration (..),
    newJwtTokenTypeConfiguration,
    jwtTokenTypeConfiguration_claimRegex,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_issuer,
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_keyLocation,

    -- * MemberGroup
    MemberGroup (..),
    newMemberGroup,
    memberGroup_dataSourceId,
    memberGroup_groupId,

    -- * MemberUser
    MemberUser (..),
    newMemberUser,
    memberUser_userId,

    -- * OnPremiseConfiguration
    OnPremiseConfiguration (..),
    newOnPremiseConfiguration,
    onPremiseConfiguration_hostUrl,
    onPremiseConfiguration_organizationName,
    onPremiseConfiguration_sslCertificateS3Path,

    -- * OneDriveConfiguration
    OneDriveConfiguration (..),
    newOneDriveConfiguration,
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_exclusionPatterns,
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_tenantDomain,
    oneDriveConfiguration_secretArn,
    oneDriveConfiguration_oneDriveUsers,

    -- * OneDriveUsers
    OneDriveUsers (..),
    newOneDriveUsers,
    oneDriveUsers_oneDriveUserList,
    oneDriveUsers_oneDriveUserS3Path,

    -- * PersonasSummary
    PersonasSummary (..),
    newPersonasSummary,
    personasSummary_createdAt,
    personasSummary_entityId,
    personasSummary_persona,
    personasSummary_updatedAt,

    -- * Principal
    Principal (..),
    newPrincipal,
    principal_dataSourceId,
    principal_name,
    principal_type,
    principal_access,

    -- * ProxyConfiguration
    ProxyConfiguration (..),
    newProxyConfiguration,
    proxyConfiguration_credentials,
    proxyConfiguration_host,
    proxyConfiguration_port,

    -- * QueryResultItem
    QueryResultItem (..),
    newQueryResultItem,
    queryResultItem_additionalAttributes,
    queryResultItem_documentAttributes,
    queryResultItem_documentExcerpt,
    queryResultItem_documentId,
    queryResultItem_documentTitle,
    queryResultItem_documentURI,
    queryResultItem_feedbackToken,
    queryResultItem_format,
    queryResultItem_id,
    queryResultItem_scoreAttributes,
    queryResultItem_tableExcerpt,
    queryResultItem_type,

    -- * QuerySuggestionsBlockListSummary
    QuerySuggestionsBlockListSummary (..),
    newQuerySuggestionsBlockListSummary,
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_itemCount,
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_updatedAt,

    -- * QuipConfiguration
    QuipConfiguration (..),
    newQuipConfiguration,
    quipConfiguration_attachmentFieldMappings,
    quipConfiguration_crawlAttachments,
    quipConfiguration_crawlChatRooms,
    quipConfiguration_crawlFileComments,
    quipConfiguration_exclusionPatterns,
    quipConfiguration_folderIds,
    quipConfiguration_inclusionPatterns,
    quipConfiguration_messageFieldMappings,
    quipConfiguration_threadFieldMappings,
    quipConfiguration_vpcConfiguration,
    quipConfiguration_domain,
    quipConfiguration_secretArn,

    -- * Relevance
    Relevance (..),
    newRelevance,
    relevance_duration,
    relevance_freshness,
    relevance_importance,
    relevance_rankOrder,
    relevance_valueImportanceMap,

    -- * RelevanceFeedback
    RelevanceFeedback (..),
    newRelevanceFeedback,
    relevanceFeedback_resultId,
    relevanceFeedback_relevanceValue,

    -- * S3DataSourceConfiguration
    S3DataSourceConfiguration (..),
    newS3DataSourceConfiguration,
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_exclusionPatterns,
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_bucketName,

    -- * S3Path
    S3Path (..),
    newS3Path,
    s3Path_bucket,
    s3Path_key,

    -- * SaaSConfiguration
    SaaSConfiguration (..),
    newSaaSConfiguration,
    saaSConfiguration_organizationName,
    saaSConfiguration_hostUrl,

    -- * SalesforceChatterFeedConfiguration
    SalesforceChatterFeedConfiguration (..),
    newSalesforceChatterFeedConfiguration,
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- * SalesforceConfiguration
    SalesforceConfiguration (..),
    newSalesforceConfiguration,
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_includeAttachmentFilePatterns,
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_serverUrl,
    salesforceConfiguration_secretArn,

    -- * SalesforceCustomKnowledgeArticleTypeConfiguration
    SalesforceCustomKnowledgeArticleTypeConfiguration (..),
    newSalesforceCustomKnowledgeArticleTypeConfiguration,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceCustomKnowledgeArticleTypeConfiguration_name,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- * SalesforceKnowledgeArticleConfiguration
    SalesforceKnowledgeArticleConfiguration (..),
    newSalesforceKnowledgeArticleConfiguration,
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
    salesforceKnowledgeArticleConfiguration_includedStates,

    -- * SalesforceStandardKnowledgeArticleTypeConfiguration
    SalesforceStandardKnowledgeArticleTypeConfiguration (..),
    newSalesforceStandardKnowledgeArticleTypeConfiguration,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- * SalesforceStandardObjectAttachmentConfiguration
    SalesforceStandardObjectAttachmentConfiguration (..),
    newSalesforceStandardObjectAttachmentConfiguration,
    salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName,
    salesforceStandardObjectAttachmentConfiguration_fieldMappings,

    -- * SalesforceStandardObjectConfiguration
    SalesforceStandardObjectConfiguration (..),
    newSalesforceStandardObjectConfiguration,
    salesforceStandardObjectConfiguration_documentTitleFieldName,
    salesforceStandardObjectConfiguration_fieldMappings,
    salesforceStandardObjectConfiguration_name,
    salesforceStandardObjectConfiguration_documentDataFieldName,

    -- * ScoreAttributes
    ScoreAttributes (..),
    newScoreAttributes,
    scoreAttributes_scoreConfidence,

    -- * Search
    Search (..),
    newSearch,
    search_displayable,
    search_facetable,
    search_searchable,
    search_sortable,

    -- * SeedUrlConfiguration
    SeedUrlConfiguration (..),
    newSeedUrlConfiguration,
    seedUrlConfiguration_webCrawlerMode,
    seedUrlConfiguration_seedUrls,

    -- * ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (..),
    newServerSideEncryptionConfiguration,
    serverSideEncryptionConfiguration_kmsKeyId,

    -- * ServiceNowConfiguration
    ServiceNowConfiguration (..),
    newServiceNowConfiguration,
    serviceNowConfiguration_authenticationType,
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- * ServiceNowKnowledgeArticleConfiguration
    ServiceNowKnowledgeArticleConfiguration (..),
    newServiceNowKnowledgeArticleConfiguration,
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- * ServiceNowServiceCatalogConfiguration
    ServiceNowServiceCatalogConfiguration (..),
    newServiceNowServiceCatalogConfiguration,
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- * SharePointConfiguration
    SharePointConfiguration (..),
    newSharePointConfiguration,
    sharePointConfiguration_authenticationType,
    sharePointConfiguration_crawlAttachments,
    sharePointConfiguration_disableLocalGroups,
    sharePointConfiguration_documentTitleFieldName,
    sharePointConfiguration_exclusionPatterns,
    sharePointConfiguration_fieldMappings,
    sharePointConfiguration_inclusionPatterns,
    sharePointConfiguration_proxyConfiguration,
    sharePointConfiguration_sslCertificateS3Path,
    sharePointConfiguration_useChangeLog,
    sharePointConfiguration_vpcConfiguration,
    sharePointConfiguration_sharePointVersion,
    sharePointConfiguration_urls,
    sharePointConfiguration_secretArn,

    -- * SiteMapsConfiguration
    SiteMapsConfiguration (..),
    newSiteMapsConfiguration,
    siteMapsConfiguration_siteMaps,

    -- * SlackConfiguration
    SlackConfiguration (..),
    newSlackConfiguration,
    slackConfiguration_crawlBotMessage,
    slackConfiguration_excludeArchived,
    slackConfiguration_exclusionPatterns,
    slackConfiguration_fieldMappings,
    slackConfiguration_inclusionPatterns,
    slackConfiguration_lookBackPeriod,
    slackConfiguration_privateChannelFilter,
    slackConfiguration_publicChannelFilter,
    slackConfiguration_useChangeLog,
    slackConfiguration_vpcConfiguration,
    slackConfiguration_teamId,
    slackConfiguration_secretArn,
    slackConfiguration_slackEntityList,
    slackConfiguration_sinceCrawlDate,

    -- * SortingConfiguration
    SortingConfiguration (..),
    newSortingConfiguration,
    sortingConfiguration_documentAttributeKey,
    sortingConfiguration_sortOrder,

    -- * SpellCorrectedQuery
    SpellCorrectedQuery (..),
    newSpellCorrectedQuery,
    spellCorrectedQuery_corrections,
    spellCorrectedQuery_suggestedQueryText,

    -- * SpellCorrectionConfiguration
    SpellCorrectionConfiguration (..),
    newSpellCorrectionConfiguration,
    spellCorrectionConfiguration_includeQuerySpellCheckSuggestions,

    -- * SqlConfiguration
    SqlConfiguration (..),
    newSqlConfiguration,
    sqlConfiguration_queryIdentifiersEnclosingOption,

    -- * Status
    Status (..),
    newStatus,
    status_documentId,
    status_documentStatus,
    status_failureCode,
    status_failureReason,

    -- * Suggestion
    Suggestion (..),
    newSuggestion,
    suggestion_id,
    suggestion_value,

    -- * SuggestionHighlight
    SuggestionHighlight (..),
    newSuggestionHighlight,
    suggestionHighlight_beginOffset,
    suggestionHighlight_endOffset,

    -- * SuggestionTextWithHighlights
    SuggestionTextWithHighlights (..),
    newSuggestionTextWithHighlights,
    suggestionTextWithHighlights_highlights,
    suggestionTextWithHighlights_text,

    -- * SuggestionValue
    SuggestionValue (..),
    newSuggestionValue,
    suggestionValue_text,

    -- * TableCell
    TableCell (..),
    newTableCell,
    tableCell_header,
    tableCell_highlighted,
    tableCell_topAnswer,
    tableCell_value,

    -- * TableExcerpt
    TableExcerpt (..),
    newTableExcerpt,
    tableExcerpt_rows,
    tableExcerpt_totalNumberOfRows,

    -- * TableRow
    TableRow (..),
    newTableRow,
    tableRow_cells,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Template
    Template (..),
    newTemplate,

    -- * TemplateConfiguration
    TemplateConfiguration (..),
    newTemplateConfiguration,
    templateConfiguration_template,

    -- * TextDocumentStatistics
    TextDocumentStatistics (..),
    newTextDocumentStatistics,
    textDocumentStatistics_indexedTextDocumentsCount,
    textDocumentStatistics_indexedTextBytes,

    -- * TextWithHighlights
    TextWithHighlights (..),
    newTextWithHighlights,
    textWithHighlights_highlights,
    textWithHighlights_text,

    -- * ThesaurusSummary
    ThesaurusSummary (..),
    newThesaurusSummary,
    thesaurusSummary_createdAt,
    thesaurusSummary_id,
    thesaurusSummary_name,
    thesaurusSummary_status,
    thesaurusSummary_updatedAt,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_endTime,
    timeRange_startTime,

    -- * Urls
    Urls (..),
    newUrls,
    urls_seedUrlConfiguration,
    urls_siteMapsConfiguration,

    -- * UserContext
    UserContext (..),
    newUserContext,
    userContext_dataSourceGroups,
    userContext_groups,
    userContext_token,
    userContext_userId,

    -- * UserGroupResolutionConfiguration
    UserGroupResolutionConfiguration (..),
    newUserGroupResolutionConfiguration,
    userGroupResolutionConfiguration_userGroupResolutionMode,

    -- * UserIdentityConfiguration
    UserIdentityConfiguration (..),
    newUserIdentityConfiguration,
    userIdentityConfiguration_identityAttributeName,

    -- * UserTokenConfiguration
    UserTokenConfiguration (..),
    newUserTokenConfiguration,
    userTokenConfiguration_jsonTokenTypeConfiguration,
    userTokenConfiguration_jwtTokenTypeConfiguration,

    -- * Warning
    Warning (..),
    newWarning,
    warning_code,
    warning_message,

    -- * WebCrawlerConfiguration
    WebCrawlerConfiguration (..),
    newWebCrawlerConfiguration,
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- * WorkDocsConfiguration
    WorkDocsConfiguration (..),
    newWorkDocsConfiguration,
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_exclusionPatterns,
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_organizationId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Kendra.Types.AccessControlConfigurationSummary
import Amazonka.Kendra.Types.AccessControlListConfiguration
import Amazonka.Kendra.Types.AclConfiguration
import Amazonka.Kendra.Types.AdditionalResultAttribute
import Amazonka.Kendra.Types.AdditionalResultAttributeValue
import Amazonka.Kendra.Types.AdditionalResultAttributeValueType
import Amazonka.Kendra.Types.AlfrescoConfiguration
import Amazonka.Kendra.Types.AlfrescoEntity
import Amazonka.Kendra.Types.AttributeFilter
import Amazonka.Kendra.Types.AuthenticationConfiguration
import Amazonka.Kendra.Types.BasicAuthenticationConfiguration
import Amazonka.Kendra.Types.BatchDeleteDocumentResponseFailedDocument
import Amazonka.Kendra.Types.BatchGetDocumentStatusResponseError
import Amazonka.Kendra.Types.BatchPutDocumentResponseFailedDocument
import Amazonka.Kendra.Types.BoxConfiguration
import Amazonka.Kendra.Types.CapacityUnitsConfiguration
import Amazonka.Kendra.Types.ClickFeedback
import Amazonka.Kendra.Types.ColumnConfiguration
import Amazonka.Kendra.Types.ConditionOperator
import Amazonka.Kendra.Types.ConfluenceAttachmentConfiguration
import Amazonka.Kendra.Types.ConfluenceAttachmentFieldName
import Amazonka.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceAuthenticationType
import Amazonka.Kendra.Types.ConfluenceBlogConfiguration
import Amazonka.Kendra.Types.ConfluenceBlogFieldName
import Amazonka.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceConfiguration
import Amazonka.Kendra.Types.ConfluencePageConfiguration
import Amazonka.Kendra.Types.ConfluencePageFieldName
import Amazonka.Kendra.Types.ConfluencePageToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceSpaceConfiguration
import Amazonka.Kendra.Types.ConfluenceSpaceFieldName
import Amazonka.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceVersion
import Amazonka.Kendra.Types.ConnectionConfiguration
import Amazonka.Kendra.Types.ContentSourceConfiguration
import Amazonka.Kendra.Types.ContentType
import Amazonka.Kendra.Types.Correction
import Amazonka.Kendra.Types.CustomDocumentEnrichmentConfiguration
import Amazonka.Kendra.Types.DataSourceConfiguration
import Amazonka.Kendra.Types.DataSourceGroup
import Amazonka.Kendra.Types.DataSourceStatus
import Amazonka.Kendra.Types.DataSourceSummary
import Amazonka.Kendra.Types.DataSourceSyncJob
import Amazonka.Kendra.Types.DataSourceSyncJobMetricTarget
import Amazonka.Kendra.Types.DataSourceSyncJobMetrics
import Amazonka.Kendra.Types.DataSourceSyncJobStatus
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceType
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.DatabaseConfiguration
import Amazonka.Kendra.Types.DatabaseEngineType
import Amazonka.Kendra.Types.Document
import Amazonka.Kendra.Types.DocumentAttribute
import Amazonka.Kendra.Types.DocumentAttributeCondition
import Amazonka.Kendra.Types.DocumentAttributeTarget
import Amazonka.Kendra.Types.DocumentAttributeValue
import Amazonka.Kendra.Types.DocumentAttributeValueCountPair
import Amazonka.Kendra.Types.DocumentAttributeValueType
import Amazonka.Kendra.Types.DocumentInfo
import Amazonka.Kendra.Types.DocumentMetadataConfiguration
import Amazonka.Kendra.Types.DocumentRelevanceConfiguration
import Amazonka.Kendra.Types.DocumentStatus
import Amazonka.Kendra.Types.DocumentsMetadataConfiguration
import Amazonka.Kendra.Types.EndpointType
import Amazonka.Kendra.Types.EntityConfiguration
import Amazonka.Kendra.Types.EntityDisplayData
import Amazonka.Kendra.Types.EntityPersonaConfiguration
import Amazonka.Kendra.Types.EntityType
import Amazonka.Kendra.Types.ErrorCode
import Amazonka.Kendra.Types.ExperienceConfiguration
import Amazonka.Kendra.Types.ExperienceEndpoint
import Amazonka.Kendra.Types.ExperienceEntitiesSummary
import Amazonka.Kendra.Types.ExperienceStatus
import Amazonka.Kendra.Types.ExperiencesSummary
import Amazonka.Kendra.Types.Facet
import Amazonka.Kendra.Types.FacetResult
import Amazonka.Kendra.Types.FailedEntity
import Amazonka.Kendra.Types.FaqFileFormat
import Amazonka.Kendra.Types.FaqStatistics
import Amazonka.Kendra.Types.FaqStatus
import Amazonka.Kendra.Types.FaqSummary
import Amazonka.Kendra.Types.FsxConfiguration
import Amazonka.Kendra.Types.FsxFileSystemType
import Amazonka.Kendra.Types.GitHubConfiguration
import Amazonka.Kendra.Types.GitHubDocumentCrawlProperties
import Amazonka.Kendra.Types.GoogleDriveConfiguration
import Amazonka.Kendra.Types.GroupMembers
import Amazonka.Kendra.Types.GroupOrderingIdSummary
import Amazonka.Kendra.Types.GroupSummary
import Amazonka.Kendra.Types.HierarchicalPrincipal
import Amazonka.Kendra.Types.Highlight
import Amazonka.Kendra.Types.HighlightType
import Amazonka.Kendra.Types.HookConfiguration
import Amazonka.Kendra.Types.IndexConfigurationSummary
import Amazonka.Kendra.Types.IndexEdition
import Amazonka.Kendra.Types.IndexStatistics
import Amazonka.Kendra.Types.IndexStatus
import Amazonka.Kendra.Types.InlineCustomDocumentEnrichmentConfiguration
import Amazonka.Kendra.Types.Interval
import Amazonka.Kendra.Types.IssueSubEntity
import Amazonka.Kendra.Types.JiraConfiguration
import Amazonka.Kendra.Types.JsonTokenTypeConfiguration
import Amazonka.Kendra.Types.JwtTokenTypeConfiguration
import Amazonka.Kendra.Types.KeyLocation
import Amazonka.Kendra.Types.MemberGroup
import Amazonka.Kendra.Types.MemberUser
import Amazonka.Kendra.Types.MetricType
import Amazonka.Kendra.Types.Mode
import Amazonka.Kendra.Types.OnPremiseConfiguration
import Amazonka.Kendra.Types.OneDriveConfiguration
import Amazonka.Kendra.Types.OneDriveUsers
import Amazonka.Kendra.Types.Order
import Amazonka.Kendra.Types.Persona
import Amazonka.Kendra.Types.PersonasSummary
import Amazonka.Kendra.Types.Principal
import Amazonka.Kendra.Types.PrincipalMappingStatus
import Amazonka.Kendra.Types.PrincipalType
import Amazonka.Kendra.Types.ProxyConfiguration
import Amazonka.Kendra.Types.QueryIdentifiersEnclosingOption
import Amazonka.Kendra.Types.QueryResultFormat
import Amazonka.Kendra.Types.QueryResultItem
import Amazonka.Kendra.Types.QueryResultType
import Amazonka.Kendra.Types.QuerySuggestionsBlockListStatus
import Amazonka.Kendra.Types.QuerySuggestionsBlockListSummary
import Amazonka.Kendra.Types.QuerySuggestionsStatus
import Amazonka.Kendra.Types.QuipConfiguration
import Amazonka.Kendra.Types.ReadAccessType
import Amazonka.Kendra.Types.Relevance
import Amazonka.Kendra.Types.RelevanceFeedback
import Amazonka.Kendra.Types.RelevanceType
import Amazonka.Kendra.Types.S3DataSourceConfiguration
import Amazonka.Kendra.Types.S3Path
import Amazonka.Kendra.Types.SaaSConfiguration
import Amazonka.Kendra.Types.SalesforceChatterFeedConfiguration
import Amazonka.Kendra.Types.SalesforceChatterFeedIncludeFilterType
import Amazonka.Kendra.Types.SalesforceConfiguration
import Amazonka.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Amazonka.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.SalesforceKnowledgeArticleState
import Amazonka.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectName
import Amazonka.Kendra.Types.ScoreAttributes
import Amazonka.Kendra.Types.ScoreConfidence
import Amazonka.Kendra.Types.Search
import Amazonka.Kendra.Types.SeedUrlConfiguration
import Amazonka.Kendra.Types.ServerSideEncryptionConfiguration
import Amazonka.Kendra.Types.ServiceNowAuthenticationType
import Amazonka.Kendra.Types.ServiceNowBuildVersionType
import Amazonka.Kendra.Types.ServiceNowConfiguration
import Amazonka.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.ServiceNowServiceCatalogConfiguration
import Amazonka.Kendra.Types.SharePointConfiguration
import Amazonka.Kendra.Types.SharePointOnlineAuthenticationType
import Amazonka.Kendra.Types.SharePointVersion
import Amazonka.Kendra.Types.SiteMapsConfiguration
import Amazonka.Kendra.Types.SlackConfiguration
import Amazonka.Kendra.Types.SlackEntity
import Amazonka.Kendra.Types.SortOrder
import Amazonka.Kendra.Types.SortingConfiguration
import Amazonka.Kendra.Types.SpellCorrectedQuery
import Amazonka.Kendra.Types.SpellCorrectionConfiguration
import Amazonka.Kendra.Types.SqlConfiguration
import Amazonka.Kendra.Types.Status
import Amazonka.Kendra.Types.Suggestion
import Amazonka.Kendra.Types.SuggestionHighlight
import Amazonka.Kendra.Types.SuggestionTextWithHighlights
import Amazonka.Kendra.Types.SuggestionValue
import Amazonka.Kendra.Types.TableCell
import Amazonka.Kendra.Types.TableExcerpt
import Amazonka.Kendra.Types.TableRow
import Amazonka.Kendra.Types.Tag
import Amazonka.Kendra.Types.Template
import Amazonka.Kendra.Types.TemplateConfiguration
import Amazonka.Kendra.Types.TextDocumentStatistics
import Amazonka.Kendra.Types.TextWithHighlights
import Amazonka.Kendra.Types.ThesaurusStatus
import Amazonka.Kendra.Types.ThesaurusSummary
import Amazonka.Kendra.Types.TimeRange
import Amazonka.Kendra.Types.Type
import Amazonka.Kendra.Types.Urls
import Amazonka.Kendra.Types.UserContext
import Amazonka.Kendra.Types.UserContextPolicy
import Amazonka.Kendra.Types.UserGroupResolutionConfiguration
import Amazonka.Kendra.Types.UserGroupResolutionMode
import Amazonka.Kendra.Types.UserIdentityConfiguration
import Amazonka.Kendra.Types.UserTokenConfiguration
import Amazonka.Kendra.Types.Warning
import Amazonka.Kendra.Types.WarningCode
import Amazonka.Kendra.Types.WebCrawlerConfiguration
import Amazonka.Kendra.Types.WebCrawlerMode
import Amazonka.Kendra.Types.WorkDocsConfiguration
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-02-03@ of the Amazon KendraFrontendService SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Kendra",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "kendra",
      Core.signingName = "kendra",
      Core.version = "2019-02-03",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Kendra",
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

-- | You don\'t have sufficient access to perform this action. Please ensure
-- you have the required permission policies and user accounts and try
-- again.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | A conflict occurred with the request. Please fix any inconsistences with
-- your resources and try again.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | An issue occurred with the internal server used for your Amazon Kendra
-- service. Please wait a few minutes and try again, or contact
-- <http://aws.amazon.com/aws.amazon.com/contact-us Support> for help.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The input to the request is not valid. Please provide the correct input
-- and try again.
_InvalidRequestException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The resource you want to use already exists. Please check you have
-- provided the correct resource and try again.
_ResourceAlreadyExistException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceAlreadyExistException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistException"

-- | The resource you want to use is currently in use. Please check you have
-- provided the correct resource and try again.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- | The resource you want to use doesn’t exist. Please check you have
-- provided the correct resource and try again.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The resource you want to use isn\'t available. Please check you have
-- provided the correct resource and try again.
_ResourceUnavailableException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- | You have exceeded the set limits for your Amazon Kendra service. Please
-- see Quotas[hyperlink Kendra Quotas pg] for more information, or contact
-- <http://aws.amazon.com/aws.amazon.com/contact-us Support> to inquire
-- about an increase of limits.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling. Please reduce the
-- number of requests and try again.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The input fails to satisfy the constraints set by the Amazon Kendra
-- service. Please provide the correct input and try again.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
