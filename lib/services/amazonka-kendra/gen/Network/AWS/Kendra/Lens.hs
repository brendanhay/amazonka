{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kendra.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Lens
  ( -- * Operations

    -- ** UpdateDataSource
    updateDataSource_languageCode,
    updateDataSource_schedule,
    updateDataSource_name,
    updateDataSource_configuration,
    updateDataSource_description,
    updateDataSource_roleArn,
    updateDataSource_id,
    updateDataSource_indexId,

    -- ** DeleteDataSource
    deleteDataSource_id,
    deleteDataSource_indexId,

    -- ** ClearQuerySuggestions
    clearQuerySuggestions_indexId,

    -- ** ListFaqs
    listFaqs_nextToken,
    listFaqs_maxResults,
    listFaqs_indexId,
    listFaqsResponse_faqSummaryItems,
    listFaqsResponse_nextToken,
    listFaqsResponse_httpStatus,

    -- ** DeleteIndex
    deleteIndex_id,

    -- ** UpdateIndex
    updateIndex_documentMetadataConfigurationUpdates,
    updateIndex_capacityUnits,
    updateIndex_name,
    updateIndex_userGroupResolutionConfiguration,
    updateIndex_description,
    updateIndex_userContextPolicy,
    updateIndex_userTokenConfigurations,
    updateIndex_roleArn,
    updateIndex_id,

    -- ** ListQuerySuggestionsBlockLists
    listQuerySuggestionsBlockLists_nextToken,
    listQuerySuggestionsBlockLists_maxResults,
    listQuerySuggestionsBlockLists_indexId,
    listQuerySuggestionsBlockListsResponse_nextToken,
    listQuerySuggestionsBlockListsResponse_blockListSummaryItems,
    listQuerySuggestionsBlockListsResponse_httpStatus,

    -- ** CreateFaq
    createFaq_languageCode,
    createFaq_clientToken,
    createFaq_fileFormat,
    createFaq_description,
    createFaq_tags,
    createFaq_indexId,
    createFaq_name,
    createFaq_s3Path,
    createFaq_roleArn,
    createFaqResponse_id,
    createFaqResponse_httpStatus,

    -- ** CreateQuerySuggestionsBlockList
    createQuerySuggestionsBlockList_clientToken,
    createQuerySuggestionsBlockList_description,
    createQuerySuggestionsBlockList_tags,
    createQuerySuggestionsBlockList_indexId,
    createQuerySuggestionsBlockList_name,
    createQuerySuggestionsBlockList_sourceS3Path,
    createQuerySuggestionsBlockList_roleArn,
    createQuerySuggestionsBlockListResponse_id,
    createQuerySuggestionsBlockListResponse_httpStatus,

    -- ** BatchPutDocument
    batchPutDocument_roleArn,
    batchPutDocument_indexId,
    batchPutDocument_documents,
    batchPutDocumentResponse_failedDocuments,
    batchPutDocumentResponse_httpStatus,

    -- ** BatchDeleteDocument
    batchDeleteDocument_dataSourceSyncJobMetricTarget,
    batchDeleteDocument_indexId,
    batchDeleteDocument_documentIdList,
    batchDeleteDocumentResponse_failedDocuments,
    batchDeleteDocumentResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SubmitFeedback
    submitFeedback_relevanceFeedbackItems,
    submitFeedback_clickFeedbackItems,
    submitFeedback_indexId,
    submitFeedback_queryId,

    -- ** StopDataSourceSyncJob
    stopDataSourceSyncJob_id,
    stopDataSourceSyncJob_indexId,

    -- ** DescribeDataSource
    describeDataSource_id,
    describeDataSource_indexId,
    describeDataSourceResponse_status,
    describeDataSourceResponse_languageCode,
    describeDataSourceResponse_createdAt,
    describeDataSourceResponse_schedule,
    describeDataSourceResponse_name,
    describeDataSourceResponse_id,
    describeDataSourceResponse_configuration,
    describeDataSourceResponse_type,
    describeDataSourceResponse_updatedAt,
    describeDataSourceResponse_errorMessage,
    describeDataSourceResponse_indexId,
    describeDataSourceResponse_description,
    describeDataSourceResponse_roleArn,
    describeDataSourceResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_id,
    describeIndexResponse_edition,
    describeIndexResponse_status,
    describeIndexResponse_createdAt,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_capacityUnits,
    describeIndexResponse_name,
    describeIndexResponse_id,
    describeIndexResponse_updatedAt,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_errorMessage,
    describeIndexResponse_description,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_roleArn,
    describeIndexResponse_httpStatus,

    -- ** UpdateQuerySuggestionsConfig
    updateQuerySuggestionsConfig_minimumQueryCount,
    updateQuerySuggestionsConfig_mode,
    updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation,
    updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers,
    updateQuerySuggestionsConfig_queryLogLookBackWindowInDays,
    updateQuerySuggestionsConfig_indexId,

    -- ** CreateDataSource
    createDataSource_languageCode,
    createDataSource_clientToken,
    createDataSource_schedule,
    createDataSource_configuration,
    createDataSource_description,
    createDataSource_tags,
    createDataSource_roleArn,
    createDataSource_name,
    createDataSource_indexId,
    createDataSource_type,
    createDataSourceResponse_httpStatus,
    createDataSourceResponse_id,

    -- ** BatchGetDocumentStatus
    batchGetDocumentStatus_indexId,
    batchGetDocumentStatus_documentInfoList,
    batchGetDocumentStatusResponse_errors,
    batchGetDocumentStatusResponse_documentStatusList,
    batchGetDocumentStatusResponse_httpStatus,

    -- ** ListDataSourceSyncJobs
    listDataSourceSyncJobs_nextToken,
    listDataSourceSyncJobs_statusFilter,
    listDataSourceSyncJobs_startTimeFilter,
    listDataSourceSyncJobs_maxResults,
    listDataSourceSyncJobs_id,
    listDataSourceSyncJobs_indexId,
    listDataSourceSyncJobsResponse_history,
    listDataSourceSyncJobsResponse_nextToken,
    listDataSourceSyncJobsResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_indexId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_summaryItems,
    listDataSourcesResponse_httpStatus,

    -- ** DeleteQuerySuggestionsBlockList
    deleteQuerySuggestionsBlockList_indexId,
    deleteQuerySuggestionsBlockList_id,

    -- ** UpdateQuerySuggestionsBlockList
    updateQuerySuggestionsBlockList_sourceS3Path,
    updateQuerySuggestionsBlockList_name,
    updateQuerySuggestionsBlockList_description,
    updateQuerySuggestionsBlockList_roleArn,
    updateQuerySuggestionsBlockList_indexId,
    updateQuerySuggestionsBlockList_id,

    -- ** DeleteFaq
    deleteFaq_id,
    deleteFaq_indexId,

    -- ** PutPrincipalMapping
    putPrincipalMapping_dataSourceId,
    putPrincipalMapping_orderingId,
    putPrincipalMapping_roleArn,
    putPrincipalMapping_indexId,
    putPrincipalMapping_groupId,
    putPrincipalMapping_groupMembers,

    -- ** DeletePrincipalMapping
    deletePrincipalMapping_dataSourceId,
    deletePrincipalMapping_orderingId,
    deletePrincipalMapping_indexId,
    deletePrincipalMapping_groupId,

    -- ** DescribeThesaurus
    describeThesaurus_id,
    describeThesaurus_indexId,
    describeThesaurusResponse_status,
    describeThesaurusResponse_fileSizeBytes,
    describeThesaurusResponse_createdAt,
    describeThesaurusResponse_sourceS3Path,
    describeThesaurusResponse_synonymRuleCount,
    describeThesaurusResponse_name,
    describeThesaurusResponse_id,
    describeThesaurusResponse_termCount,
    describeThesaurusResponse_updatedAt,
    describeThesaurusResponse_errorMessage,
    describeThesaurusResponse_indexId,
    describeThesaurusResponse_description,
    describeThesaurusResponse_roleArn,
    describeThesaurusResponse_httpStatus,

    -- ** ListThesauri
    listThesauri_nextToken,
    listThesauri_maxResults,
    listThesauri_indexId,
    listThesauriResponse_thesaurusSummaryItems,
    listThesauriResponse_nextToken,
    listThesauriResponse_httpStatus,

    -- ** CreateIndex
    createIndex_edition,
    createIndex_clientToken,
    createIndex_userGroupResolutionConfiguration,
    createIndex_description,
    createIndex_userContextPolicy,
    createIndex_tags,
    createIndex_userTokenConfigurations,
    createIndex_serverSideEncryptionConfiguration,
    createIndex_name,
    createIndex_roleArn,
    createIndexResponse_id,
    createIndexResponse_httpStatus,

    -- ** Query
    query_sortingConfiguration,
    query_requestedDocumentAttributes,
    query_facets,
    query_userContext,
    query_visitorId,
    query_attributeFilter,
    query_pageNumber,
    query_queryResultTypeFilter,
    query_pageSize,
    query_documentRelevanceOverrideConfigurations,
    query_indexId,
    query_queryText,
    queryResponse_queryId,
    queryResponse_facetResults,
    queryResponse_resultItems,
    queryResponse_totalNumberOfResults,
    queryResponse_httpStatus,

    -- ** DescribeQuerySuggestionsConfig
    describeQuerySuggestionsConfig_indexId,
    describeQuerySuggestionsConfigResponse_status,
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_httpStatus,

    -- ** StartDataSourceSyncJob
    startDataSourceSyncJob_id,
    startDataSourceSyncJob_indexId,
    startDataSourceSyncJobResponse_executionId,
    startDataSourceSyncJobResponse_httpStatus,

    -- ** ListIndices
    listIndices_nextToken,
    listIndices_maxResults,
    listIndicesResponse_indexConfigurationSummaryItems,
    listIndicesResponse_nextToken,
    listIndicesResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetQuerySuggestions
    getQuerySuggestions_maxSuggestionsCount,
    getQuerySuggestions_indexId,
    getQuerySuggestions_queryText,
    getQuerySuggestionsResponse_suggestions,
    getQuerySuggestionsResponse_querySuggestionsId,
    getQuerySuggestionsResponse_httpStatus,

    -- ** DeleteThesaurus
    deleteThesaurus_id,
    deleteThesaurus_indexId,

    -- ** UpdateThesaurus
    updateThesaurus_sourceS3Path,
    updateThesaurus_name,
    updateThesaurus_description,
    updateThesaurus_roleArn,
    updateThesaurus_id,
    updateThesaurus_indexId,

    -- ** DescribeFaq
    describeFaq_id,
    describeFaq_indexId,
    describeFaqResponse_status,
    describeFaqResponse_languageCode,
    describeFaqResponse_createdAt,
    describeFaqResponse_fileFormat,
    describeFaqResponse_name,
    describeFaqResponse_id,
    describeFaqResponse_s3Path,
    describeFaqResponse_updatedAt,
    describeFaqResponse_errorMessage,
    describeFaqResponse_indexId,
    describeFaqResponse_description,
    describeFaqResponse_roleArn,
    describeFaqResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** DescribeQuerySuggestionsBlockList
    describeQuerySuggestionsBlockList_indexId,
    describeQuerySuggestionsBlockList_id,
    describeQuerySuggestionsBlockListResponse_status,
    describeQuerySuggestionsBlockListResponse_fileSizeBytes,
    describeQuerySuggestionsBlockListResponse_createdAt,
    describeQuerySuggestionsBlockListResponse_sourceS3Path,
    describeQuerySuggestionsBlockListResponse_name,
    describeQuerySuggestionsBlockListResponse_id,
    describeQuerySuggestionsBlockListResponse_updatedAt,
    describeQuerySuggestionsBlockListResponse_errorMessage,
    describeQuerySuggestionsBlockListResponse_indexId,
    describeQuerySuggestionsBlockListResponse_itemCount,
    describeQuerySuggestionsBlockListResponse_description,
    describeQuerySuggestionsBlockListResponse_roleArn,
    describeQuerySuggestionsBlockListResponse_httpStatus,

    -- ** DescribePrincipalMapping
    describePrincipalMapping_dataSourceId,
    describePrincipalMapping_indexId,
    describePrincipalMapping_groupId,
    describePrincipalMappingResponse_dataSourceId,
    describePrincipalMappingResponse_groupId,
    describePrincipalMappingResponse_indexId,
    describePrincipalMappingResponse_groupOrderingIdSummaries,
    describePrincipalMappingResponse_httpStatus,

    -- ** ListGroupsOlderThanOrderingId
    listGroupsOlderThanOrderingId_dataSourceId,
    listGroupsOlderThanOrderingId_nextToken,
    listGroupsOlderThanOrderingId_maxResults,
    listGroupsOlderThanOrderingId_indexId,
    listGroupsOlderThanOrderingId_orderingId,
    listGroupsOlderThanOrderingIdResponse_groupsSummaries,
    listGroupsOlderThanOrderingIdResponse_nextToken,
    listGroupsOlderThanOrderingIdResponse_httpStatus,

    -- ** CreateThesaurus
    createThesaurus_clientToken,
    createThesaurus_description,
    createThesaurus_tags,
    createThesaurus_indexId,
    createThesaurus_name,
    createThesaurus_roleArn,
    createThesaurus_sourceS3Path,
    createThesaurusResponse_id,
    createThesaurusResponse_httpStatus,

    -- * Types

    -- ** AccessControlListConfiguration
    accessControlListConfiguration_keyPath,

    -- ** AclConfiguration
    aclConfiguration_allowedGroupsColumnName,

    -- ** AdditionalResultAttribute
    additionalResultAttribute_key,
    additionalResultAttribute_valueType,
    additionalResultAttribute_value,

    -- ** AdditionalResultAttributeValue
    additionalResultAttributeValue_textWithHighlightsValue,

    -- ** AttributeFilter
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

    -- ** AuthenticationConfiguration
    authenticationConfiguration_basicAuthentication,

    -- ** BasicAuthenticationConfiguration
    basicAuthenticationConfiguration_host,
    basicAuthenticationConfiguration_port,
    basicAuthenticationConfiguration_credentials,

    -- ** BatchDeleteDocumentResponseFailedDocument
    batchDeleteDocumentResponseFailedDocument_errorCode,
    batchDeleteDocumentResponseFailedDocument_id,
    batchDeleteDocumentResponseFailedDocument_errorMessage,

    -- ** BatchGetDocumentStatusResponseError
    batchGetDocumentStatusResponseError_documentId,
    batchGetDocumentStatusResponseError_errorCode,
    batchGetDocumentStatusResponseError_errorMessage,

    -- ** BatchPutDocumentResponseFailedDocument
    batchPutDocumentResponseFailedDocument_errorCode,
    batchPutDocumentResponseFailedDocument_id,
    batchPutDocumentResponseFailedDocument_errorMessage,

    -- ** CapacityUnitsConfiguration
    capacityUnitsConfiguration_storageCapacityUnits,
    capacityUnitsConfiguration_queryCapacityUnits,

    -- ** ClickFeedback
    clickFeedback_resultId,
    clickFeedback_clickTime,

    -- ** ColumnConfiguration
    columnConfiguration_fieldMappings,
    columnConfiguration_documentTitleColumnName,
    columnConfiguration_documentIdColumnName,
    columnConfiguration_documentDataColumnName,
    columnConfiguration_changeDetectingColumns,

    -- ** ConfluenceAttachmentConfiguration
    confluenceAttachmentConfiguration_crawlAttachments,
    confluenceAttachmentConfiguration_attachmentFieldMappings,

    -- ** ConfluenceAttachmentToIndexFieldMapping
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,
    confluenceAttachmentToIndexFieldMapping_dataSourceFieldName,
    confluenceAttachmentToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceBlogConfiguration
    confluenceBlogConfiguration_blogFieldMappings,

    -- ** ConfluenceBlogToIndexFieldMapping
    confluenceBlogToIndexFieldMapping_dateFieldFormat,
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceConfiguration
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

    -- ** ConfluencePageConfiguration
    confluencePageConfiguration_pageFieldMappings,

    -- ** ConfluencePageToIndexFieldMapping
    confluencePageToIndexFieldMapping_dateFieldFormat,
    confluencePageToIndexFieldMapping_dataSourceFieldName,
    confluencePageToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceSpaceConfiguration
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_includeSpaces,
    confluenceSpaceConfiguration_excludeSpaces,

    -- ** ConfluenceSpaceToIndexFieldMapping
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_indexFieldName,

    -- ** ConnectionConfiguration
    connectionConfiguration_databaseHost,
    connectionConfiguration_databasePort,
    connectionConfiguration_databaseName,
    connectionConfiguration_tableName,
    connectionConfiguration_secretArn,

    -- ** DataSourceConfiguration
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

    -- ** DataSourceGroup
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- ** DataSourceSummary
    dataSourceSummary_status,
    dataSourceSummary_languageCode,
    dataSourceSummary_createdAt,
    dataSourceSummary_name,
    dataSourceSummary_id,
    dataSourceSummary_type,
    dataSourceSummary_updatedAt,

    -- ** DataSourceSyncJob
    dataSourceSyncJob_status,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_startTime,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_errorMessage,

    -- ** DataSourceSyncJobMetricTarget
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- ** DataSourceSyncJobMetrics
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsDeleted,
    dataSourceSyncJobMetrics_documentsScanned,
    dataSourceSyncJobMetrics_documentsModified,

    -- ** DataSourceToIndexFieldMapping
    dataSourceToIndexFieldMapping_dateFieldFormat,
    dataSourceToIndexFieldMapping_dataSourceFieldName,
    dataSourceToIndexFieldMapping_indexFieldName,

    -- ** DataSourceVpcConfiguration
    dataSourceVpcConfiguration_subnetIds,
    dataSourceVpcConfiguration_securityGroupIds,

    -- ** DatabaseConfiguration
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- ** Document
    document_blob,
    document_accessControlList,
    document_attributes,
    document_s3Path,
    document_title,
    document_hierarchicalAccessControlList,
    document_contentType,
    document_id,

    -- ** DocumentAttribute
    documentAttribute_key,
    documentAttribute_value,

    -- ** DocumentAttributeValue
    documentAttributeValue_stringValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,
    documentAttributeValue_dateValue,

    -- ** DocumentAttributeValueCountPair
    documentAttributeValueCountPair_count,
    documentAttributeValueCountPair_documentAttributeValue,

    -- ** DocumentInfo
    documentInfo_attributes,
    documentInfo_documentId,

    -- ** DocumentMetadataConfiguration
    documentMetadataConfiguration_relevance,
    documentMetadataConfiguration_search,
    documentMetadataConfiguration_name,
    documentMetadataConfiguration_type,

    -- ** DocumentRelevanceConfiguration
    documentRelevanceConfiguration_name,
    documentRelevanceConfiguration_relevance,

    -- ** DocumentsMetadataConfiguration
    documentsMetadataConfiguration_s3Prefix,

    -- ** Facet
    facet_documentAttributeKey,

    -- ** FacetResult
    facetResult_documentAttributeKey,
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,

    -- ** FaqStatistics
    faqStatistics_indexedQuestionAnswersCount,

    -- ** FaqSummary
    faqSummary_status,
    faqSummary_languageCode,
    faqSummary_createdAt,
    faqSummary_fileFormat,
    faqSummary_name,
    faqSummary_id,
    faqSummary_updatedAt,

    -- ** GoogleDriveConfiguration
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_secretArn,

    -- ** GroupMembers
    groupMembers_s3PathforGroupMembers,
    groupMembers_memberGroups,
    groupMembers_memberUsers,

    -- ** GroupOrderingIdSummary
    groupOrderingIdSummary_status,
    groupOrderingIdSummary_failureReason,
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_orderingId,

    -- ** GroupSummary
    groupSummary_groupId,
    groupSummary_orderingId,

    -- ** HierarchicalPrincipal
    hierarchicalPrincipal_principalList,

    -- ** Highlight
    highlight_topAnswer,
    highlight_type,
    highlight_beginOffset,
    highlight_endOffset,

    -- ** IndexConfigurationSummary
    indexConfigurationSummary_edition,
    indexConfigurationSummary_name,
    indexConfigurationSummary_id,
    indexConfigurationSummary_createdAt,
    indexConfigurationSummary_updatedAt,
    indexConfigurationSummary_status,

    -- ** IndexStatistics
    indexStatistics_faqStatistics,
    indexStatistics_textDocumentStatistics,

    -- ** JsonTokenTypeConfiguration
    jsonTokenTypeConfiguration_userNameAttributeField,
    jsonTokenTypeConfiguration_groupAttributeField,

    -- ** JwtTokenTypeConfiguration
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_claimRegex,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_issuer,
    jwtTokenTypeConfiguration_keyLocation,

    -- ** MemberGroup
    memberGroup_dataSourceId,
    memberGroup_groupId,

    -- ** MemberUser
    memberUser_userId,

    -- ** OneDriveConfiguration
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_exclusionPatterns,
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_tenantDomain,
    oneDriveConfiguration_secretArn,
    oneDriveConfiguration_oneDriveUsers,

    -- ** OneDriveUsers
    oneDriveUsers_oneDriveUserS3Path,
    oneDriveUsers_oneDriveUserList,

    -- ** Principal
    principal_dataSourceId,
    principal_name,
    principal_type,
    principal_access,

    -- ** ProxyConfiguration
    proxyConfiguration_credentials,
    proxyConfiguration_host,
    proxyConfiguration_port,

    -- ** QueryResultItem
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

    -- ** QuerySuggestionsBlockListSummary
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_updatedAt,
    querySuggestionsBlockListSummary_itemCount,

    -- ** Relevance
    relevance_importance,
    relevance_rankOrder,
    relevance_valueImportanceMap,
    relevance_freshness,
    relevance_duration,

    -- ** RelevanceFeedback
    relevanceFeedback_resultId,
    relevanceFeedback_relevanceValue,

    -- ** S3DataSourceConfiguration
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_exclusionPatterns,
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_bucketName,

    -- ** S3Path
    s3Path_bucket,
    s3Path_key,

    -- ** SalesforceChatterFeedConfiguration
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- ** SalesforceConfiguration
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_includeAttachmentFilePatterns,
    salesforceConfiguration_serverUrl,
    salesforceConfiguration_secretArn,

    -- ** SalesforceCustomKnowledgeArticleTypeConfiguration
    salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceCustomKnowledgeArticleTypeConfiguration_name,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- ** SalesforceKnowledgeArticleConfiguration
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
    salesforceKnowledgeArticleConfiguration_includedStates,

    -- ** SalesforceStandardKnowledgeArticleTypeConfiguration
    salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- ** SalesforceStandardObjectAttachmentConfiguration
    salesforceStandardObjectAttachmentConfiguration_fieldMappings,
    salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName,

    -- ** SalesforceStandardObjectConfiguration
    salesforceStandardObjectConfiguration_fieldMappings,
    salesforceStandardObjectConfiguration_documentTitleFieldName,
    salesforceStandardObjectConfiguration_name,
    salesforceStandardObjectConfiguration_documentDataFieldName,

    -- ** ScoreAttributes
    scoreAttributes_scoreConfidence,

    -- ** Search
    search_facetable,
    search_sortable,
    search_searchable,
    search_displayable,

    -- ** SeedUrlConfiguration
    seedUrlConfiguration_webCrawlerMode,
    seedUrlConfiguration_seedUrls,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_kmsKeyId,

    -- ** ServiceNowConfiguration
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_authenticationType,
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- ** ServiceNowKnowledgeArticleConfiguration
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- ** ServiceNowServiceCatalogConfiguration
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- ** SharePointConfiguration
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

    -- ** SiteMapsConfiguration
    siteMapsConfiguration_siteMaps,

    -- ** SortingConfiguration
    sortingConfiguration_documentAttributeKey,
    sortingConfiguration_sortOrder,

    -- ** SqlConfiguration
    sqlConfiguration_queryIdentifiersEnclosingOption,

    -- ** Status
    status_failureReason,
    status_documentId,
    status_failureCode,
    status_documentStatus,

    -- ** Suggestion
    suggestion_value,
    suggestion_id,

    -- ** SuggestionHighlight
    suggestionHighlight_beginOffset,
    suggestionHighlight_endOffset,

    -- ** SuggestionTextWithHighlights
    suggestionTextWithHighlights_text,
    suggestionTextWithHighlights_highlights,

    -- ** SuggestionValue
    suggestionValue_text,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TextDocumentStatistics
    textDocumentStatistics_indexedTextDocumentsCount,
    textDocumentStatistics_indexedTextBytes,

    -- ** TextWithHighlights
    textWithHighlights_text,
    textWithHighlights_highlights,

    -- ** ThesaurusSummary
    thesaurusSummary_status,
    thesaurusSummary_createdAt,
    thesaurusSummary_name,
    thesaurusSummary_id,
    thesaurusSummary_updatedAt,

    -- ** TimeRange
    timeRange_startTime,
    timeRange_endTime,

    -- ** Urls
    urls_seedUrlConfiguration,
    urls_siteMapsConfiguration,

    -- ** UserContext
    userContext_groups,
    userContext_token,
    userContext_userId,
    userContext_dataSourceGroups,

    -- ** UserGroupResolutionConfiguration
    userGroupResolutionConfiguration_userGroupResolutionMode,

    -- ** UserTokenConfiguration
    userTokenConfiguration_jwtTokenTypeConfiguration,
    userTokenConfiguration_jsonTokenTypeConfiguration,

    -- ** WebCrawlerConfiguration
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- ** WorkDocsConfiguration
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_exclusionPatterns,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_organizationId,
  )
where

import Network.AWS.Kendra.BatchDeleteDocument
import Network.AWS.Kendra.BatchGetDocumentStatus
import Network.AWS.Kendra.BatchPutDocument
import Network.AWS.Kendra.ClearQuerySuggestions
import Network.AWS.Kendra.CreateDataSource
import Network.AWS.Kendra.CreateFaq
import Network.AWS.Kendra.CreateIndex
import Network.AWS.Kendra.CreateQuerySuggestionsBlockList
import Network.AWS.Kendra.CreateThesaurus
import Network.AWS.Kendra.DeleteDataSource
import Network.AWS.Kendra.DeleteFaq
import Network.AWS.Kendra.DeleteIndex
import Network.AWS.Kendra.DeletePrincipalMapping
import Network.AWS.Kendra.DeleteQuerySuggestionsBlockList
import Network.AWS.Kendra.DeleteThesaurus
import Network.AWS.Kendra.DescribeDataSource
import Network.AWS.Kendra.DescribeFaq
import Network.AWS.Kendra.DescribeIndex
import Network.AWS.Kendra.DescribePrincipalMapping
import Network.AWS.Kendra.DescribeQuerySuggestionsBlockList
import Network.AWS.Kendra.DescribeQuerySuggestionsConfig
import Network.AWS.Kendra.DescribeThesaurus
import Network.AWS.Kendra.GetQuerySuggestions
import Network.AWS.Kendra.ListDataSourceSyncJobs
import Network.AWS.Kendra.ListDataSources
import Network.AWS.Kendra.ListFaqs
import Network.AWS.Kendra.ListGroupsOlderThanOrderingId
import Network.AWS.Kendra.ListIndices
import Network.AWS.Kendra.ListQuerySuggestionsBlockLists
import Network.AWS.Kendra.ListTagsForResource
import Network.AWS.Kendra.ListThesauri
import Network.AWS.Kendra.PutPrincipalMapping
import Network.AWS.Kendra.Query
import Network.AWS.Kendra.StartDataSourceSyncJob
import Network.AWS.Kendra.StopDataSourceSyncJob
import Network.AWS.Kendra.SubmitFeedback
import Network.AWS.Kendra.TagResource
import Network.AWS.Kendra.Types.AccessControlListConfiguration
import Network.AWS.Kendra.Types.AclConfiguration
import Network.AWS.Kendra.Types.AdditionalResultAttribute
import Network.AWS.Kendra.Types.AdditionalResultAttributeValue
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
import Network.AWS.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceBlogConfiguration
import Network.AWS.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceConfiguration
import Network.AWS.Kendra.Types.ConfluencePageConfiguration
import Network.AWS.Kendra.Types.ConfluencePageToIndexFieldMapping
import Network.AWS.Kendra.Types.ConfluenceSpaceConfiguration
import Network.AWS.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import Network.AWS.Kendra.Types.ConnectionConfiguration
import Network.AWS.Kendra.Types.DataSourceConfiguration
import Network.AWS.Kendra.Types.DataSourceGroup
import Network.AWS.Kendra.Types.DataSourceSummary
import Network.AWS.Kendra.Types.DataSourceSyncJob
import Network.AWS.Kendra.Types.DataSourceSyncJobMetricTarget
import Network.AWS.Kendra.Types.DataSourceSyncJobMetrics
import Network.AWS.Kendra.Types.DataSourceToIndexFieldMapping
import Network.AWS.Kendra.Types.DataSourceVpcConfiguration
import Network.AWS.Kendra.Types.DatabaseConfiguration
import Network.AWS.Kendra.Types.Document
import Network.AWS.Kendra.Types.DocumentAttribute
import Network.AWS.Kendra.Types.DocumentAttributeValue
import Network.AWS.Kendra.Types.DocumentAttributeValueCountPair
import Network.AWS.Kendra.Types.DocumentInfo
import Network.AWS.Kendra.Types.DocumentMetadataConfiguration
import Network.AWS.Kendra.Types.DocumentRelevanceConfiguration
import Network.AWS.Kendra.Types.DocumentsMetadataConfiguration
import Network.AWS.Kendra.Types.Facet
import Network.AWS.Kendra.Types.FacetResult
import Network.AWS.Kendra.Types.FaqStatistics
import Network.AWS.Kendra.Types.FaqSummary
import Network.AWS.Kendra.Types.GoogleDriveConfiguration
import Network.AWS.Kendra.Types.GroupMembers
import Network.AWS.Kendra.Types.GroupOrderingIdSummary
import Network.AWS.Kendra.Types.GroupSummary
import Network.AWS.Kendra.Types.HierarchicalPrincipal
import Network.AWS.Kendra.Types.Highlight
import Network.AWS.Kendra.Types.IndexConfigurationSummary
import Network.AWS.Kendra.Types.IndexStatistics
import Network.AWS.Kendra.Types.JsonTokenTypeConfiguration
import Network.AWS.Kendra.Types.JwtTokenTypeConfiguration
import Network.AWS.Kendra.Types.MemberGroup
import Network.AWS.Kendra.Types.MemberUser
import Network.AWS.Kendra.Types.OneDriveConfiguration
import Network.AWS.Kendra.Types.OneDriveUsers
import Network.AWS.Kendra.Types.Principal
import Network.AWS.Kendra.Types.ProxyConfiguration
import Network.AWS.Kendra.Types.QueryResultItem
import Network.AWS.Kendra.Types.QuerySuggestionsBlockListSummary
import Network.AWS.Kendra.Types.Relevance
import Network.AWS.Kendra.Types.RelevanceFeedback
import Network.AWS.Kendra.Types.S3DataSourceConfiguration
import Network.AWS.Kendra.Types.S3Path
import Network.AWS.Kendra.Types.SalesforceChatterFeedConfiguration
import Network.AWS.Kendra.Types.SalesforceConfiguration
import Network.AWS.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Network.AWS.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Network.AWS.Kendra.Types.SalesforceStandardObjectConfiguration
import Network.AWS.Kendra.Types.ScoreAttributes
import Network.AWS.Kendra.Types.Search
import Network.AWS.Kendra.Types.SeedUrlConfiguration
import Network.AWS.Kendra.Types.ServerSideEncryptionConfiguration
import Network.AWS.Kendra.Types.ServiceNowConfiguration
import Network.AWS.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Network.AWS.Kendra.Types.ServiceNowServiceCatalogConfiguration
import Network.AWS.Kendra.Types.SharePointConfiguration
import Network.AWS.Kendra.Types.SiteMapsConfiguration
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
import Network.AWS.Kendra.Types.ThesaurusSummary
import Network.AWS.Kendra.Types.TimeRange
import Network.AWS.Kendra.Types.Urls
import Network.AWS.Kendra.Types.UserContext
import Network.AWS.Kendra.Types.UserGroupResolutionConfiguration
import Network.AWS.Kendra.Types.UserTokenConfiguration
import Network.AWS.Kendra.Types.WebCrawlerConfiguration
import Network.AWS.Kendra.Types.WorkDocsConfiguration
import Network.AWS.Kendra.UntagResource
import Network.AWS.Kendra.UpdateDataSource
import Network.AWS.Kendra.UpdateIndex
import Network.AWS.Kendra.UpdateQuerySuggestionsBlockList
import Network.AWS.Kendra.UpdateQuerySuggestionsConfig
import Network.AWS.Kendra.UpdateThesaurus
