{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ResourceUnavailableException,
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ResourceInUseException,
    _ConflictException,
    _ThrottlingException,
    _ValidationException,
    _ResourceAlreadyExistException,
    _InvalidRequestException,

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
    alfrescoConfiguration_vpcConfiguration,
    alfrescoConfiguration_wikiFieldMappings,
    alfrescoConfiguration_documentLibraryFieldMappings,
    alfrescoConfiguration_crawlSystemFolders,
    alfrescoConfiguration_inclusionPatterns,
    alfrescoConfiguration_entityFilter,
    alfrescoConfiguration_crawlComments,
    alfrescoConfiguration_blogFieldMappings,
    alfrescoConfiguration_exclusionPatterns,
    alfrescoConfiguration_siteUrl,
    alfrescoConfiguration_siteId,
    alfrescoConfiguration_secretArn,
    alfrescoConfiguration_sslCertificateS3Path,

    -- * AttributeFilter
    AttributeFilter (..),
    newAttributeFilter,
    attributeFilter_containsAll,
    attributeFilter_containsAny,
    attributeFilter_lessThan,
    attributeFilter_andAllFilters,
    attributeFilter_orAllFilters,
    attributeFilter_notFilter,
    attributeFilter_greaterThanOrEquals,
    attributeFilter_equalsTo,
    attributeFilter_greaterThan,
    attributeFilter_lessThanOrEquals,

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
    batchDeleteDocumentResponseFailedDocument_errorMessage,
    batchDeleteDocumentResponseFailedDocument_id,
    batchDeleteDocumentResponseFailedDocument_errorCode,

    -- * BatchGetDocumentStatusResponseError
    BatchGetDocumentStatusResponseError (..),
    newBatchGetDocumentStatusResponseError,
    batchGetDocumentStatusResponseError_errorMessage,
    batchGetDocumentStatusResponseError_errorCode,
    batchGetDocumentStatusResponseError_documentId,

    -- * BatchPutDocumentResponseFailedDocument
    BatchPutDocumentResponseFailedDocument (..),
    newBatchPutDocumentResponseFailedDocument,
    batchPutDocumentResponseFailedDocument_errorMessage,
    batchPutDocumentResponseFailedDocument_id,
    batchPutDocumentResponseFailedDocument_errorCode,

    -- * BoxConfiguration
    BoxConfiguration (..),
    newBoxConfiguration,
    boxConfiguration_useChangeLog,
    boxConfiguration_vpcConfiguration,
    boxConfiguration_fileFieldMappings,
    boxConfiguration_crawlWebLinks,
    boxConfiguration_commentFieldMappings,
    boxConfiguration_inclusionPatterns,
    boxConfiguration_webLinkFieldMappings,
    boxConfiguration_crawlTasks,
    boxConfiguration_crawlComments,
    boxConfiguration_taskFieldMappings,
    boxConfiguration_exclusionPatterns,
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
    columnConfiguration_fieldMappings,
    columnConfiguration_documentTitleColumnName,
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
    confluenceAttachmentToIndexFieldMapping_indexFieldName,
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,

    -- * ConfluenceBlogConfiguration
    ConfluenceBlogConfiguration (..),
    newConfluenceBlogConfiguration,
    confluenceBlogConfiguration_blogFieldMappings,

    -- * ConfluenceBlogToIndexFieldMapping
    ConfluenceBlogToIndexFieldMapping (..),
    newConfluenceBlogToIndexFieldMapping,
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_indexFieldName,
    confluenceBlogToIndexFieldMapping_dateFieldFormat,

    -- * ConfluenceConfiguration
    ConfluenceConfiguration (..),
    newConfluenceConfiguration,
    confluenceConfiguration_vpcConfiguration,
    confluenceConfiguration_proxyConfiguration,
    confluenceConfiguration_pageConfiguration,
    confluenceConfiguration_authenticationType,
    confluenceConfiguration_inclusionPatterns,
    confluenceConfiguration_spaceConfiguration,
    confluenceConfiguration_blogConfiguration,
    confluenceConfiguration_attachmentConfiguration,
    confluenceConfiguration_exclusionPatterns,
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
    confluencePageToIndexFieldMapping_indexFieldName,
    confluencePageToIndexFieldMapping_dateFieldFormat,

    -- * ConfluenceSpaceConfiguration
    ConfluenceSpaceConfiguration (..),
    newConfluenceSpaceConfiguration,
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_excludeSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,
    confluenceSpaceConfiguration_includeSpaces,

    -- * ConfluenceSpaceToIndexFieldMapping
    ConfluenceSpaceToIndexFieldMapping (..),
    newConfluenceSpaceToIndexFieldMapping,
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_indexFieldName,
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,

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
    contentSourceConfiguration_directPutContent,
    contentSourceConfiguration_faqIds,
    contentSourceConfiguration_dataSourceIds,

    -- * Correction
    Correction (..),
    newCorrection,
    correction_beginOffset,
    correction_endOffset,
    correction_term,
    correction_correctedTerm,

    -- * CustomDocumentEnrichmentConfiguration
    CustomDocumentEnrichmentConfiguration (..),
    newCustomDocumentEnrichmentConfiguration,
    customDocumentEnrichmentConfiguration_roleArn,
    customDocumentEnrichmentConfiguration_inlineConfigurations,
    customDocumentEnrichmentConfiguration_postExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_preExtractionHookConfiguration,

    -- * DataSourceConfiguration
    DataSourceConfiguration (..),
    newDataSourceConfiguration,
    dataSourceConfiguration_googleDriveConfiguration,
    dataSourceConfiguration_s3Configuration,
    dataSourceConfiguration_gitHubConfiguration,
    dataSourceConfiguration_workDocsConfiguration,
    dataSourceConfiguration_quipConfiguration,
    dataSourceConfiguration_jiraConfiguration,
    dataSourceConfiguration_confluenceConfiguration,
    dataSourceConfiguration_boxConfiguration,
    dataSourceConfiguration_oneDriveConfiguration,
    dataSourceConfiguration_sharePointConfiguration,
    dataSourceConfiguration_fsxConfiguration,
    dataSourceConfiguration_salesforceConfiguration,
    dataSourceConfiguration_databaseConfiguration,
    dataSourceConfiguration_serviceNowConfiguration,
    dataSourceConfiguration_templateConfiguration,
    dataSourceConfiguration_slackConfiguration,
    dataSourceConfiguration_webCrawlerConfiguration,
    dataSourceConfiguration_alfrescoConfiguration,

    -- * DataSourceGroup
    DataSourceGroup (..),
    newDataSourceGroup,
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- * DataSourceSummary
    DataSourceSummary (..),
    newDataSourceSummary,
    dataSourceSummary_name,
    dataSourceSummary_type,
    dataSourceSummary_status,
    dataSourceSummary_id,
    dataSourceSummary_languageCode,
    dataSourceSummary_createdAt,
    dataSourceSummary_updatedAt,

    -- * DataSourceSyncJob
    DataSourceSyncJob (..),
    newDataSourceSyncJob,
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_errorMessage,
    dataSourceSyncJob_status,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_startTime,

    -- * DataSourceSyncJobMetricTarget
    DataSourceSyncJobMetricTarget (..),
    newDataSourceSyncJobMetricTarget,
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- * DataSourceSyncJobMetrics
    DataSourceSyncJobMetrics (..),
    newDataSourceSyncJobMetrics,
    dataSourceSyncJobMetrics_documentsScanned,
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsModified,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsDeleted,

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
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- * Document
    Document (..),
    newDocument,
    document_accessControlList,
    document_blob,
    document_title,
    document_s3Path,
    document_attributes,
    document_hierarchicalAccessControlList,
    document_accessControlConfigurationId,
    document_contentType,
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
    documentAttributeTarget_targetDocumentAttributeValue,
    documentAttributeTarget_targetDocumentAttributeKey,
    documentAttributeTarget_targetDocumentAttributeValueDeletion,

    -- * DocumentAttributeValue
    DocumentAttributeValue (..),
    newDocumentAttributeValue,
    documentAttributeValue_stringValue,
    documentAttributeValue_dateValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,

    -- * DocumentAttributeValueCountPair
    DocumentAttributeValueCountPair (..),
    newDocumentAttributeValueCountPair,
    documentAttributeValueCountPair_facetResults,
    documentAttributeValueCountPair_count,
    documentAttributeValueCountPair_documentAttributeValue,

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
    entityDisplayData_identifiedUserName,
    entityDisplayData_firstName,
    entityDisplayData_userName,
    entityDisplayData_groupName,
    entityDisplayData_lastName,

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
    experienceEndpoint_endpointType,
    experienceEndpoint_endpoint,

    -- * ExperienceEntitiesSummary
    ExperienceEntitiesSummary (..),
    newExperienceEntitiesSummary,
    experienceEntitiesSummary_entityId,
    experienceEntitiesSummary_displayData,
    experienceEntitiesSummary_entityType,

    -- * ExperiencesSummary
    ExperiencesSummary (..),
    newExperiencesSummary,
    experiencesSummary_name,
    experiencesSummary_status,
    experiencesSummary_endpoints,
    experiencesSummary_id,
    experiencesSummary_createdAt,

    -- * Facet
    Facet (..),
    newFacet,
    facet_facets,
    facet_maxResults,
    facet_documentAttributeKey,

    -- * FacetResult
    FacetResult (..),
    newFacetResult,
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,
    facetResult_documentAttributeKey,

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
    faqSummary_name,
    faqSummary_status,
    faqSummary_id,
    faqSummary_languageCode,
    faqSummary_createdAt,
    faqSummary_updatedAt,
    faqSummary_fileFormat,

    -- * FsxConfiguration
    FsxConfiguration (..),
    newFsxConfiguration,
    fsxConfiguration_inclusionPatterns,
    fsxConfiguration_fieldMappings,
    fsxConfiguration_secretArn,
    fsxConfiguration_exclusionPatterns,
    fsxConfiguration_fileSystemId,
    fsxConfiguration_fileSystemType,
    fsxConfiguration_vpcConfiguration,

    -- * GitHubConfiguration
    GitHubConfiguration (..),
    newGitHubConfiguration,
    gitHubConfiguration_gitHubIssueAttachmentConfigurationFieldMappings,
    gitHubConfiguration_useChangeLog,
    gitHubConfiguration_vpcConfiguration,
    gitHubConfiguration_type,
    gitHubConfiguration_repositoryFilter,
    gitHubConfiguration_saaSConfiguration,
    gitHubConfiguration_gitHubRepositoryConfigurationFieldMappings,
    gitHubConfiguration_gitHubPullRequestCommentConfigurationFieldMappings,
    gitHubConfiguration_gitHubIssueCommentConfigurationFieldMappings,
    gitHubConfiguration_onPremiseConfiguration,
    gitHubConfiguration_gitHubCommitConfigurationFieldMappings,
    gitHubConfiguration_gitHubDocumentCrawlProperties,
    gitHubConfiguration_gitHubPullRequestDocumentConfigurationFieldMappings,
    gitHubConfiguration_exclusionFolderNamePatterns,
    gitHubConfiguration_gitHubIssueDocumentConfigurationFieldMappings,
    gitHubConfiguration_inclusionFolderNamePatterns,
    gitHubConfiguration_exclusionFileTypePatterns,
    gitHubConfiguration_inclusionFileNamePatterns,
    gitHubConfiguration_inclusionFileTypePatterns,
    gitHubConfiguration_exclusionFileNamePatterns,
    gitHubConfiguration_gitHubPullRequestDocumentAttachmentConfigurationFieldMappings,
    gitHubConfiguration_secretArn,

    -- * GitHubDocumentCrawlProperties
    GitHubDocumentCrawlProperties (..),
    newGitHubDocumentCrawlProperties,
    gitHubDocumentCrawlProperties_crawlPullRequestComment,
    gitHubDocumentCrawlProperties_crawlRepositoryDocuments,
    gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment,
    gitHubDocumentCrawlProperties_crawlPullRequest,
    gitHubDocumentCrawlProperties_crawlIssueComment,
    gitHubDocumentCrawlProperties_crawlIssueCommentAttachment,
    gitHubDocumentCrawlProperties_crawlIssue,

    -- * GoogleDriveConfiguration
    GoogleDriveConfiguration (..),
    newGoogleDriveConfiguration,
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_secretArn,

    -- * GroupMembers
    GroupMembers (..),
    newGroupMembers,
    groupMembers_memberUsers,
    groupMembers_memberGroups,
    groupMembers_s3PathforGroupMembers,

    -- * GroupOrderingIdSummary
    GroupOrderingIdSummary (..),
    newGroupOrderingIdSummary,
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_status,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_orderingId,
    groupOrderingIdSummary_failureReason,

    -- * GroupSummary
    GroupSummary (..),
    newGroupSummary,
    groupSummary_orderingId,
    groupSummary_groupId,

    -- * HierarchicalPrincipal
    HierarchicalPrincipal (..),
    newHierarchicalPrincipal,
    hierarchicalPrincipal_principalList,

    -- * Highlight
    Highlight (..),
    newHighlight,
    highlight_type,
    highlight_topAnswer,
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
    indexConfigurationSummary_name,
    indexConfigurationSummary_edition,
    indexConfigurationSummary_id,
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
    inlineCustomDocumentEnrichmentConfiguration_target,
    inlineCustomDocumentEnrichmentConfiguration_condition,
    inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion,

    -- * JiraConfiguration
    JiraConfiguration (..),
    newJiraConfiguration,
    jiraConfiguration_useChangeLog,
    jiraConfiguration_vpcConfiguration,
    jiraConfiguration_projectFieldMappings,
    jiraConfiguration_attachmentFieldMappings,
    jiraConfiguration_issueType,
    jiraConfiguration_commentFieldMappings,
    jiraConfiguration_issueSubEntityFilter,
    jiraConfiguration_inclusionPatterns,
    jiraConfiguration_status,
    jiraConfiguration_project,
    jiraConfiguration_workLogFieldMappings,
    jiraConfiguration_issueFieldMappings,
    jiraConfiguration_exclusionPatterns,
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
    jwtTokenTypeConfiguration_issuer,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_claimRegex,
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
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_exclusionPatterns,
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
    personasSummary_entityId,
    personasSummary_persona,
    personasSummary_createdAt,
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
    queryResultItem_type,
    queryResultItem_documentTitle,
    queryResultItem_documentAttributes,
    queryResultItem_scoreAttributes,
    queryResultItem_id,
    queryResultItem_feedbackToken,
    queryResultItem_additionalAttributes,
    queryResultItem_documentExcerpt,
    queryResultItem_documentId,
    queryResultItem_documentURI,

    -- * QuerySuggestionsBlockListSummary
    QuerySuggestionsBlockListSummary (..),
    newQuerySuggestionsBlockListSummary,
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_itemCount,
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_updatedAt,

    -- * QuipConfiguration
    QuipConfiguration (..),
    newQuipConfiguration,
    quipConfiguration_vpcConfiguration,
    quipConfiguration_attachmentFieldMappings,
    quipConfiguration_crawlAttachments,
    quipConfiguration_messageFieldMappings,
    quipConfiguration_inclusionPatterns,
    quipConfiguration_crawlFileComments,
    quipConfiguration_folderIds,
    quipConfiguration_crawlChatRooms,
    quipConfiguration_threadFieldMappings,
    quipConfiguration_exclusionPatterns,
    quipConfiguration_domain,
    quipConfiguration_secretArn,

    -- * Relevance
    Relevance (..),
    newRelevance,
    relevance_importance,
    relevance_freshness,
    relevance_duration,
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
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_exclusionPatterns,
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
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- * SalesforceConfiguration
    SalesforceConfiguration (..),
    newSalesforceConfiguration,
    salesforceConfiguration_includeAttachmentFilePatterns,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_serverUrl,
    salesforceConfiguration_secretArn,

    -- * SalesforceCustomKnowledgeArticleTypeConfiguration
    SalesforceCustomKnowledgeArticleTypeConfiguration (..),
    newSalesforceCustomKnowledgeArticleTypeConfiguration,
    salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceCustomKnowledgeArticleTypeConfiguration_name,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- * SalesforceKnowledgeArticleConfiguration
    SalesforceKnowledgeArticleConfiguration (..),
    newSalesforceKnowledgeArticleConfiguration,
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
    salesforceKnowledgeArticleConfiguration_includedStates,

    -- * SalesforceStandardKnowledgeArticleTypeConfiguration
    SalesforceStandardKnowledgeArticleTypeConfiguration (..),
    newSalesforceStandardKnowledgeArticleTypeConfiguration,
    salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- * SalesforceStandardObjectAttachmentConfiguration
    SalesforceStandardObjectAttachmentConfiguration (..),
    newSalesforceStandardObjectAttachmentConfiguration,
    salesforceStandardObjectAttachmentConfiguration_fieldMappings,
    salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName,

    -- * SalesforceStandardObjectConfiguration
    SalesforceStandardObjectConfiguration (..),
    newSalesforceStandardObjectConfiguration,
    salesforceStandardObjectConfiguration_fieldMappings,
    salesforceStandardObjectConfiguration_documentTitleFieldName,
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
    search_sortable,
    search_searchable,
    search_facetable,

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
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- * ServiceNowKnowledgeArticleConfiguration
    ServiceNowKnowledgeArticleConfiguration (..),
    newServiceNowKnowledgeArticleConfiguration,
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- * ServiceNowServiceCatalogConfiguration
    ServiceNowServiceCatalogConfiguration (..),
    newServiceNowServiceCatalogConfiguration,
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- * SharePointConfiguration
    SharePointConfiguration (..),
    newSharePointConfiguration,
    sharePointConfiguration_useChangeLog,
    sharePointConfiguration_vpcConfiguration,
    sharePointConfiguration_proxyConfiguration,
    sharePointConfiguration_authenticationType,
    sharePointConfiguration_sslCertificateS3Path,
    sharePointConfiguration_crawlAttachments,
    sharePointConfiguration_inclusionPatterns,
    sharePointConfiguration_fieldMappings,
    sharePointConfiguration_documentTitleFieldName,
    sharePointConfiguration_disableLocalGroups,
    sharePointConfiguration_exclusionPatterns,
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
    slackConfiguration_useChangeLog,
    slackConfiguration_vpcConfiguration,
    slackConfiguration_crawlBotMessage,
    slackConfiguration_inclusionPatterns,
    slackConfiguration_lookBackPeriod,
    slackConfiguration_excludeArchived,
    slackConfiguration_fieldMappings,
    slackConfiguration_publicChannelFilter,
    slackConfiguration_privateChannelFilter,
    slackConfiguration_exclusionPatterns,
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
    spellCorrectedQuery_suggestedQueryText,
    spellCorrectedQuery_corrections,

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
    status_failureCode,
    status_documentId,
    status_documentStatus,
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
    thesaurusSummary_name,
    thesaurusSummary_status,
    thesaurusSummary_id,
    thesaurusSummary_createdAt,
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
    userContext_userId,
    userContext_groups,
    userContext_token,

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
    userTokenConfiguration_jwtTokenTypeConfiguration,
    userTokenConfiguration_jsonTokenTypeConfiguration,

    -- * Warning
    Warning (..),
    newWarning,
    warning_message,
    warning_code,

    -- * WebCrawlerConfiguration
    WebCrawlerConfiguration (..),
    newWebCrawlerConfiguration,
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- * WorkDocsConfiguration
    WorkDocsConfiguration (..),
    newWorkDocsConfiguration,
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_exclusionPatterns,
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

-- |
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- |
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- |
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- |
_ResourceInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- |
_ResourceAlreadyExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistException"

-- | The input to the request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
