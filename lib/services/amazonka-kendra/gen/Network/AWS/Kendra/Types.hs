{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kendra.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ResourceUnavailableException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceAlreadyExistException,
    _ResourceNotFoundException,
    _ResourceInUseException,

    -- * AdditionalResultAttributeValueType
    AdditionalResultAttributeValueType (..),

    -- * ConfluenceAttachmentFieldName
    ConfluenceAttachmentFieldName (..),

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

    -- * ErrorCode
    ErrorCode (..),

    -- * FaqFileFormat
    FaqFileFormat (..),

    -- * FaqStatus
    FaqStatus (..),

    -- * HighlightType
    HighlightType (..),

    -- * IndexEdition
    IndexEdition (..),

    -- * IndexStatus
    IndexStatus (..),

    -- * KeyLocation
    KeyLocation (..),

    -- * Mode
    Mode (..),

    -- * Order
    Order (..),

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

    -- * SharePointVersion
    SharePointVersion (..),

    -- * SortOrder
    SortOrder (..),

    -- * ThesaurusStatus
    ThesaurusStatus (..),

    -- * UserContextPolicy
    UserContextPolicy (..),

    -- * UserGroupResolutionMode
    UserGroupResolutionMode (..),

    -- * WebCrawlerMode
    WebCrawlerMode (..),

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

    -- * AttributeFilter
    AttributeFilter (..),
    newAttributeFilter,
    orAllFilters,
    containsAll,
    lessThan,
    containsAny,
    lessThanOrEquals,
    equalsTo,
    notFilter,
    greaterThanOrEquals,
    greaterThan,
    andAllFilters,

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
    batchDeleteDocumentResponseFailedDocument_id,
    batchDeleteDocumentResponseFailedDocument_errorMessage,

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
    batchPutDocumentResponseFailedDocument_id,
    batchPutDocumentResponseFailedDocument_errorMessage,

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
    confluenceAttachmentConfiguration_crawlAttachments,
    confluenceAttachmentConfiguration_attachmentFieldMappings,

    -- * ConfluenceAttachmentToIndexFieldMapping
    ConfluenceAttachmentToIndexFieldMapping (..),
    newConfluenceAttachmentToIndexFieldMapping,
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,
    confluenceAttachmentToIndexFieldMapping_dataSourceFieldName,
    confluenceAttachmentToIndexFieldMapping_indexFieldName,

    -- * ConfluenceBlogConfiguration
    ConfluenceBlogConfiguration (..),
    newConfluenceBlogConfiguration,
    confluenceBlogConfiguration_blogFieldMappings,

    -- * ConfluenceBlogToIndexFieldMapping
    ConfluenceBlogToIndexFieldMapping (..),
    newConfluenceBlogToIndexFieldMapping,
    confluenceBlogToIndexFieldMapping_dateFieldFormat,
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_indexFieldName,

    -- * ConfluenceConfiguration
    ConfluenceConfiguration (..),
    newConfluenceConfiguration,
    confluenceConfiguration_pageConfiguration,
    confluenceConfiguration_attachmentConfiguration,
    confluenceConfiguration_spaceConfiguration,
    confluenceConfiguration_exclusionPatterns,
    confluenceConfiguration_vpcConfiguration,
    confluenceConfiguration_inclusionPatterns,
    confluenceConfiguration_blogConfiguration,
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
    confluencePageToIndexFieldMapping_dateFieldFormat,
    confluencePageToIndexFieldMapping_dataSourceFieldName,
    confluencePageToIndexFieldMapping_indexFieldName,

    -- * ConfluenceSpaceConfiguration
    ConfluenceSpaceConfiguration (..),
    newConfluenceSpaceConfiguration,
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_includeSpaces,
    confluenceSpaceConfiguration_excludeSpaces,

    -- * ConfluenceSpaceToIndexFieldMapping
    ConfluenceSpaceToIndexFieldMapping (..),
    newConfluenceSpaceToIndexFieldMapping,
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_indexFieldName,

    -- * ConnectionConfiguration
    ConnectionConfiguration (..),
    newConnectionConfiguration,
    connectionConfiguration_databaseHost,
    connectionConfiguration_databasePort,
    connectionConfiguration_databaseName,
    connectionConfiguration_tableName,
    connectionConfiguration_secretArn,

    -- * DataSourceConfiguration
    DataSourceConfiguration (..),
    newDataSourceConfiguration,
    dataSourceConfiguration_webCrawlerConfiguration,
    dataSourceConfiguration_databaseConfiguration,
    dataSourceConfiguration_googleDriveConfiguration,
    dataSourceConfiguration_oneDriveConfiguration,
    dataSourceConfiguration_confluenceConfiguration,
    dataSourceConfiguration_s3Configuration,
    dataSourceConfiguration_serviceNowConfiguration,
    dataSourceConfiguration_sharePointConfiguration,
    dataSourceConfiguration_workDocsConfiguration,
    dataSourceConfiguration_salesforceConfiguration,

    -- * DataSourceGroup
    DataSourceGroup (..),
    newDataSourceGroup,
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- * DataSourceSummary
    DataSourceSummary (..),
    newDataSourceSummary,
    dataSourceSummary_status,
    dataSourceSummary_languageCode,
    dataSourceSummary_createdAt,
    dataSourceSummary_name,
    dataSourceSummary_id,
    dataSourceSummary_type,
    dataSourceSummary_updatedAt,

    -- * DataSourceSyncJob
    DataSourceSyncJob (..),
    newDataSourceSyncJob,
    dataSourceSyncJob_status,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_startTime,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_errorMessage,

    -- * DataSourceSyncJobMetricTarget
    DataSourceSyncJobMetricTarget (..),
    newDataSourceSyncJobMetricTarget,
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- * DataSourceSyncJobMetrics
    DataSourceSyncJobMetrics (..),
    newDataSourceSyncJobMetrics,
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsDeleted,
    dataSourceSyncJobMetrics_documentsScanned,
    dataSourceSyncJobMetrics_documentsModified,

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
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- * Document
    Document (..),
    newDocument,
    document_blob,
    document_accessControlList,
    document_attributes,
    document_s3Path,
    document_title,
    document_hierarchicalAccessControlList,
    document_contentType,
    document_id,

    -- * DocumentAttribute
    DocumentAttribute (..),
    newDocumentAttribute,
    documentAttribute_key,
    documentAttribute_value,

    -- * DocumentAttributeValue
    DocumentAttributeValue (..),
    newDocumentAttributeValue,
    documentAttributeValue_stringValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,
    documentAttributeValue_dateValue,

    -- * DocumentAttributeValueCountPair
    DocumentAttributeValueCountPair (..),
    newDocumentAttributeValueCountPair,
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

    -- * Facet
    Facet (..),
    newFacet,
    facet_documentAttributeKey,

    -- * FacetResult
    FacetResult (..),
    newFacetResult,
    facetResult_documentAttributeKey,
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,

    -- * FaqStatistics
    FaqStatistics (..),
    newFaqStatistics,
    faqStatistics_indexedQuestionAnswersCount,

    -- * FaqSummary
    FaqSummary (..),
    newFaqSummary,
    faqSummary_status,
    faqSummary_languageCode,
    faqSummary_createdAt,
    faqSummary_fileFormat,
    faqSummary_name,
    faqSummary_id,
    faqSummary_updatedAt,

    -- * GoogleDriveConfiguration
    GoogleDriveConfiguration (..),
    newGoogleDriveConfiguration,
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_secretArn,

    -- * GroupMembers
    GroupMembers (..),
    newGroupMembers,
    groupMembers_s3PathforGroupMembers,
    groupMembers_memberGroups,
    groupMembers_memberUsers,

    -- * GroupOrderingIdSummary
    GroupOrderingIdSummary (..),
    newGroupOrderingIdSummary,
    groupOrderingIdSummary_status,
    groupOrderingIdSummary_failureReason,
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_orderingId,

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

    -- * IndexConfigurationSummary
    IndexConfigurationSummary (..),
    newIndexConfigurationSummary,
    indexConfigurationSummary_edition,
    indexConfigurationSummary_name,
    indexConfigurationSummary_id,
    indexConfigurationSummary_createdAt,
    indexConfigurationSummary_updatedAt,
    indexConfigurationSummary_status,

    -- * IndexStatistics
    IndexStatistics (..),
    newIndexStatistics,
    indexStatistics_faqStatistics,
    indexStatistics_textDocumentStatistics,

    -- * JsonTokenTypeConfiguration
    JsonTokenTypeConfiguration (..),
    newJsonTokenTypeConfiguration,
    jsonTokenTypeConfiguration_userNameAttributeField,
    jsonTokenTypeConfiguration_groupAttributeField,

    -- * JwtTokenTypeConfiguration
    JwtTokenTypeConfiguration (..),
    newJwtTokenTypeConfiguration,
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_claimRegex,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_issuer,
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

    -- * OneDriveConfiguration
    OneDriveConfiguration (..),
    newOneDriveConfiguration,
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_exclusionPatterns,
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_tenantDomain,
    oneDriveConfiguration_secretArn,
    oneDriveConfiguration_oneDriveUsers,

    -- * OneDriveUsers
    OneDriveUsers (..),
    newOneDriveUsers,
    oneDriveUsers_oneDriveUserS3Path,
    oneDriveUsers_oneDriveUserList,

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
    queryResultItem_feedbackToken,
    queryResultItem_documentId,
    queryResultItem_documentAttributes,
    queryResultItem_additionalAttributes,
    queryResultItem_scoreAttributes,
    queryResultItem_documentTitle,
    queryResultItem_documentExcerpt,
    queryResultItem_id,
    queryResultItem_type,
    queryResultItem_documentURI,

    -- * QuerySuggestionsBlockListSummary
    QuerySuggestionsBlockListSummary (..),
    newQuerySuggestionsBlockListSummary,
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_updatedAt,
    querySuggestionsBlockListSummary_itemCount,

    -- * Relevance
    Relevance (..),
    newRelevance,
    relevance_importance,
    relevance_rankOrder,
    relevance_valueImportanceMap,
    relevance_freshness,
    relevance_duration,

    -- * RelevanceFeedback
    RelevanceFeedback (..),
    newRelevanceFeedback,
    relevanceFeedback_resultId,
    relevanceFeedback_relevanceValue,

    -- * S3DataSourceConfiguration
    S3DataSourceConfiguration (..),
    newS3DataSourceConfiguration,
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_exclusionPatterns,
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_bucketName,

    -- * S3Path
    S3Path (..),
    newS3Path,
    s3Path_bucket,
    s3Path_key,

    -- * SalesforceChatterFeedConfiguration
    SalesforceChatterFeedConfiguration (..),
    newSalesforceChatterFeedConfiguration,
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- * SalesforceConfiguration
    SalesforceConfiguration (..),
    newSalesforceConfiguration,
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_includeAttachmentFilePatterns,
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
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
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
    search_facetable,
    search_sortable,
    search_searchable,
    search_displayable,

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
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_authenticationType,
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- * ServiceNowKnowledgeArticleConfiguration
    ServiceNowKnowledgeArticleConfiguration (..),
    newServiceNowKnowledgeArticleConfiguration,
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- * ServiceNowServiceCatalogConfiguration
    ServiceNowServiceCatalogConfiguration (..),
    newServiceNowServiceCatalogConfiguration,
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- * SharePointConfiguration
    SharePointConfiguration (..),
    newSharePointConfiguration,
    sharePointConfiguration_fieldMappings,
    sharePointConfiguration_useChangeLog,
    sharePointConfiguration_crawlAttachments,
    sharePointConfiguration_sslCertificateS3Path,
    sharePointConfiguration_exclusionPatterns,
    sharePointConfiguration_documentTitleFieldName,
    sharePointConfiguration_disableLocalGroups,
    sharePointConfiguration_vpcConfiguration,
    sharePointConfiguration_inclusionPatterns,
    sharePointConfiguration_sharePointVersion,
    sharePointConfiguration_urls,
    sharePointConfiguration_secretArn,

    -- * SiteMapsConfiguration
    SiteMapsConfiguration (..),
    newSiteMapsConfiguration,
    siteMapsConfiguration_siteMaps,

    -- * SortingConfiguration
    SortingConfiguration (..),
    newSortingConfiguration,
    sortingConfiguration_documentAttributeKey,
    sortingConfiguration_sortOrder,

    -- * SqlConfiguration
    SqlConfiguration (..),
    newSqlConfiguration,
    sqlConfiguration_queryIdentifiersEnclosingOption,

    -- * Status
    Status (..),
    newStatus,
    status_failureReason,
    status_documentId,
    status_failureCode,
    status_documentStatus,

    -- * Suggestion
    Suggestion (..),
    newSuggestion,
    suggestion_value,
    suggestion_id,

    -- * SuggestionHighlight
    SuggestionHighlight (..),
    newSuggestionHighlight,
    suggestionHighlight_beginOffset,
    suggestionHighlight_endOffset,

    -- * SuggestionTextWithHighlights
    SuggestionTextWithHighlights (..),
    newSuggestionTextWithHighlights,
    suggestionTextWithHighlights_text,
    suggestionTextWithHighlights_highlights,

    -- * SuggestionValue
    SuggestionValue (..),
    newSuggestionValue,
    suggestionValue_text,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TextDocumentStatistics
    TextDocumentStatistics (..),
    newTextDocumentStatistics,
    textDocumentStatistics_indexedTextDocumentsCount,
    textDocumentStatistics_indexedTextBytes,

    -- * TextWithHighlights
    TextWithHighlights (..),
    newTextWithHighlights,
    textWithHighlights_text,
    textWithHighlights_highlights,

    -- * ThesaurusSummary
    ThesaurusSummary (..),
    newThesaurusSummary,
    thesaurusSummary_status,
    thesaurusSummary_createdAt,
    thesaurusSummary_name,
    thesaurusSummary_id,
    thesaurusSummary_updatedAt,

    -- * TimeRange
    TimeRange (..),
    newTimeRange,
    timeRange_startTime,
    timeRange_endTime,

    -- * Urls
    Urls (..),
    newUrls,
    urls_seedUrlConfiguration,
    urls_siteMapsConfiguration,

    -- * UserContext
    UserContext (..),
    newUserContext,
    userContext_groups,
    userContext_token,
    userContext_userId,
    userContext_dataSourceGroups,

    -- * UserGroupResolutionConfiguration
    UserGroupResolutionConfiguration (..),
    newUserGroupResolutionConfiguration,
    userGroupResolutionConfiguration_userGroupResolutionMode,

    -- * UserTokenConfiguration
    UserTokenConfiguration (..),
    newUserTokenConfiguration,
    userTokenConfiguration_jwtTokenTypeConfiguration,
    userTokenConfiguration_jsonTokenTypeConfiguration,

    -- * WebCrawlerConfiguration
    WebCrawlerConfiguration (..),
    newWebCrawlerConfiguration,
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- * WorkDocsConfiguration
    WorkDocsConfiguration (..),
    newWorkDocsConfiguration,
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_exclusionPatterns,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_organizationId,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.AccessControlListConfiguration
import Network.AWS.Kendra.Types.AclConfiguration
import Network.AWS.Kendra.Types.AdditionalResultAttribute
import Network.AWS.Kendra.Types.AdditionalResultAttributeValue
import Network.AWS.Kendra.Types.AdditionalResultAttributeValueType
import Network.AWS.Kendra.Types.AttributeFilter
import Network.AWS.Kendra.Types.AuthenticationConfiguration
import Network.AWS.Kendra.Types.BasicAuthenticationConfiguration
import Network.AWS.Kendra.Types.BatchDeleteDocumentResponseFailedDocument
import Network.AWS.Kendra.Types.BatchGetDocumentStatusResponseError
import Network.AWS.Kendra.Types.BatchPutDocumentResponseFailedDocument
import Network.AWS.Kendra.Types.CapacityUnitsConfiguration
import Network.AWS.Kendra.Types.ClickFeedback
import Network.AWS.Kendra.Types.ColumnConfiguration
import Network.AWS.Kendra.Types.ConfluenceAttachmentConfiguration
import Network.AWS.Kendra.Types.ConfluenceAttachmentFieldName
import Network.AWS.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceBlogConfiguration
import Network.AWS.Kendra.Types.ConfluenceBlogFieldName
import Network.AWS.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceConfiguration
import Network.AWS.Kendra.Types.ConfluencePageConfiguration
import Network.AWS.Kendra.Types.ConfluencePageFieldName
import Network.AWS.Kendra.Types.ConfluencePageToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceSpaceConfiguration
import Network.AWS.Kendra.Types.ConfluenceSpaceFieldName
import Network.AWS.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceVersion
import Network.AWS.Kendra.Types.ConnectionConfiguration
import Network.AWS.Kendra.Types.ContentType
import Network.AWS.Kendra.Types.DataSourceConfiguration
import Network.AWS.Kendra.Types.DataSourceGroup
import Network.AWS.Kendra.Types.DataSourceStatus
import Network.AWS.Kendra.Types.DataSourceSummary
import Network.AWS.Kendra.Types.DataSourceSyncJob
import Network.AWS.Kendra.Types.DataSourceSyncJobMetricTarget
import Network.AWS.Kendra.Types.DataSourceSyncJobMetrics
import Network.AWS.Kendra.Types.DataSourceSyncJobStatus
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.DataSourceType
import Network.AWS.Kendra.Types.DataSourceVpcConfiguration
import Network.AWS.Kendra.Types.DatabaseConfiguration
import Network.AWS.Kendra.Types.DatabaseEngineType
import Network.AWS.Kendra.Types.Document
import Network.AWS.Kendra.Types.DocumentAttribute
import Network.AWS.Kendra.Types.DocumentAttributeValue
import Network.AWS.Kendra.Types.DocumentAttributeValueCountPair
import Network.AWS.Kendra.Types.DocumentAttributeValueType
import Network.AWS.Kendra.Types.DocumentInfo
import Network.AWS.Kendra.Types.DocumentMetadataConfiguration
import Network.AWS.Kendra.Types.DocumentRelevanceConfiguration
import Network.AWS.Kendra.Types.DocumentStatus
import Network.AWS.Kendra.Types.DocumentsMetadataConfiguration
import Network.AWS.Kendra.Types.ErrorCode
import Network.AWS.Kendra.Types.Facet
import Network.AWS.Kendra.Types.FacetResult
import Network.AWS.Kendra.Types.FaqFileFormat
import Network.AWS.Kendra.Types.FaqStatistics
import Network.AWS.Kendra.Types.FaqStatus
import Network.AWS.Kendra.Types.FaqSummary
import Network.AWS.Kendra.Types.GoogleDriveConfiguration
import Network.AWS.Kendra.Types.GroupMembers
import Network.AWS.Kendra.Types.GroupOrderingIdSummary
import Network.AWS.Kendra.Types.GroupSummary
import Network.AWS.Kendra.Types.HierarchicalPrincipal
import Network.AWS.Kendra.Types.Highlight
import Network.AWS.Kendra.Types.HighlightType
import Network.AWS.Kendra.Types.IndexConfigurationSummary
import Network.AWS.Kendra.Types.IndexEdition
import Network.AWS.Kendra.Types.IndexStatistics
import Network.AWS.Kendra.Types.IndexStatus
import Network.AWS.Kendra.Types.JsonTokenTypeConfiguration
import Network.AWS.Kendra.Types.JwtTokenTypeConfiguration
import Network.AWS.Kendra.Types.KeyLocation
import Network.AWS.Kendra.Types.MemberGroup
import Network.AWS.Kendra.Types.MemberUser
import Network.AWS.Kendra.Types.Mode
import Network.AWS.Kendra.Types.OneDriveConfiguration
import Network.AWS.Kendra.Types.OneDriveUsers
import Network.AWS.Kendra.Types.Order
import Network.AWS.Kendra.Types.Principal
import Network.AWS.Kendra.Types.PrincipalMappingStatus
import Network.AWS.Kendra.Types.PrincipalType
import Network.AWS.Kendra.Types.ProxyConfiguration
import Network.AWS.Kendra.Types.QueryIdentifiersEnclosingOption
import Network.AWS.Kendra.Types.QueryResultItem
import Network.AWS.Kendra.Types.QueryResultType
import Network.AWS.Kendra.Types.QuerySuggestionsBlockListStatus
import Network.AWS.Kendra.Types.QuerySuggestionsBlockListSummary
import Network.AWS.Kendra.Types.QuerySuggestionsStatus
import Network.AWS.Kendra.Types.ReadAccessType
import Network.AWS.Kendra.Types.Relevance
import Network.AWS.Kendra.Types.RelevanceFeedback
import Network.AWS.Kendra.Types.RelevanceType
import Network.AWS.Kendra.Types.S3DataSourceConfiguration
import Network.AWS.Kendra.Types.S3Path
import Network.AWS.Kendra.Types.SalesforceChatterFeedConfiguration
import Network.AWS.Kendra.Types.SalesforceChatterFeedIncludeFilterType
import Network.AWS.Kendra.Types.SalesforceConfiguration
import Network.AWS.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Network.AWS.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.SalesforceKnowledgeArticleState
import Network.AWS.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectName
import Network.AWS.Kendra.Types.ScoreAttributes
import Network.AWS.Kendra.Types.ScoreConfidence
import Network.AWS.Kendra.Types.Search
import Network.AWS.Kendra.Types.SeedUrlConfiguration
import Network.AWS.Kendra.Types.ServerSideEncryptionConfiguration
import Network.AWS.Kendra.Types.ServiceNowAuthenticationType
import Network.AWS.Kendra.Types.ServiceNowBuildVersionType
import Network.AWS.Kendra.Types.ServiceNowConfiguration
import Network.AWS.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.ServiceNowServiceCatalogConfiguration
import Network.AWS.Kendra.Types.SharePointConfiguration
import Network.AWS.Kendra.Types.SharePointVersion
import Network.AWS.Kendra.Types.SiteMapsConfiguration
import Network.AWS.Kendra.Types.SortOrder
import Network.AWS.Kendra.Types.SortingConfiguration
import Network.AWS.Kendra.Types.SqlConfiguration
import Network.AWS.Kendra.Types.Status
import Network.AWS.Kendra.Types.Suggestion
import Network.AWS.Kendra.Types.SuggestionHighlight
import Network.AWS.Kendra.Types.SuggestionTextWithHighlights
import Network.AWS.Kendra.Types.SuggestionValue
import Network.AWS.Kendra.Types.Tag
import Network.AWS.Kendra.Types.TextDocumentStatistics
import Network.AWS.Kendra.Types.TextWithHighlights
import Network.AWS.Kendra.Types.ThesaurusStatus
import Network.AWS.Kendra.Types.ThesaurusSummary
import Network.AWS.Kendra.Types.TimeRange
import Network.AWS.Kendra.Types.Urls
import Network.AWS.Kendra.Types.UserContext
import Network.AWS.Kendra.Types.UserContextPolicy
import Network.AWS.Kendra.Types.UserGroupResolutionConfiguration
import Network.AWS.Kendra.Types.UserGroupResolutionMode
import Network.AWS.Kendra.Types.UserTokenConfiguration
import Network.AWS.Kendra.Types.WebCrawlerConfiguration
import Network.AWS.Kendra.Types.WebCrawlerMode
import Network.AWS.Kendra.Types.WorkDocsConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-02-03@ of the Amazon KendraFrontendService SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Kendra",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "kendra",
      Core._serviceSigningName = "kendra",
      Core._serviceVersion = "2019-02-03",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Kendra",
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

-- |
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- |
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- |
_ResourceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ResourceUnavailableException"

-- |
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- |
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- |
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- |
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- |
_ResourceAlreadyExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceAlreadyExistException =
  Core._MatchServiceError
    defaultService
    "ResourceAlreadyExistException"

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
