{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Kendra.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Lens
  ( -- * Operations

    -- ** AssociateEntitiesToExperience
    associateEntitiesToExperience_id,
    associateEntitiesToExperience_indexId,
    associateEntitiesToExperience_entityList,
    associateEntitiesToExperienceResponse_failedEntityList,
    associateEntitiesToExperienceResponse_httpStatus,

    -- ** AssociatePersonasToEntities
    associatePersonasToEntities_id,
    associatePersonasToEntities_indexId,
    associatePersonasToEntities_personas,
    associatePersonasToEntitiesResponse_failedEntityList,
    associatePersonasToEntitiesResponse_httpStatus,

    -- ** BatchDeleteDocument
    batchDeleteDocument_dataSourceSyncJobMetricTarget,
    batchDeleteDocument_indexId,
    batchDeleteDocument_documentIdList,
    batchDeleteDocumentResponse_failedDocuments,
    batchDeleteDocumentResponse_httpStatus,

    -- ** BatchGetDocumentStatus
    batchGetDocumentStatus_indexId,
    batchGetDocumentStatus_documentInfoList,
    batchGetDocumentStatusResponse_documentStatusList,
    batchGetDocumentStatusResponse_errors,
    batchGetDocumentStatusResponse_httpStatus,

    -- ** BatchPutDocument
    batchPutDocument_customDocumentEnrichmentConfiguration,
    batchPutDocument_roleArn,
    batchPutDocument_indexId,
    batchPutDocument_documents,
    batchPutDocumentResponse_failedDocuments,
    batchPutDocumentResponse_httpStatus,

    -- ** ClearQuerySuggestions
    clearQuerySuggestions_indexId,

    -- ** CreateAccessControlConfiguration
    createAccessControlConfiguration_accessControlList,
    createAccessControlConfiguration_clientToken,
    createAccessControlConfiguration_description,
    createAccessControlConfiguration_hierarchicalAccessControlList,
    createAccessControlConfiguration_indexId,
    createAccessControlConfiguration_name,
    createAccessControlConfigurationResponse_httpStatus,
    createAccessControlConfigurationResponse_id,

    -- ** CreateDataSource
    createDataSource_clientToken,
    createDataSource_configuration,
    createDataSource_customDocumentEnrichmentConfiguration,
    createDataSource_description,
    createDataSource_languageCode,
    createDataSource_roleArn,
    createDataSource_schedule,
    createDataSource_tags,
    createDataSource_vpcConfiguration,
    createDataSource_name,
    createDataSource_indexId,
    createDataSource_type,
    createDataSourceResponse_httpStatus,
    createDataSourceResponse_id,

    -- ** CreateExperience
    createExperience_clientToken,
    createExperience_configuration,
    createExperience_description,
    createExperience_roleArn,
    createExperience_name,
    createExperience_indexId,
    createExperienceResponse_httpStatus,
    createExperienceResponse_id,

    -- ** CreateFaq
    createFaq_clientToken,
    createFaq_description,
    createFaq_fileFormat,
    createFaq_languageCode,
    createFaq_tags,
    createFaq_indexId,
    createFaq_name,
    createFaq_s3Path,
    createFaq_roleArn,
    createFaqResponse_id,
    createFaqResponse_httpStatus,

    -- ** CreateIndex
    createIndex_clientToken,
    createIndex_description,
    createIndex_edition,
    createIndex_serverSideEncryptionConfiguration,
    createIndex_tags,
    createIndex_userContextPolicy,
    createIndex_userGroupResolutionConfiguration,
    createIndex_userTokenConfigurations,
    createIndex_name,
    createIndex_roleArn,
    createIndexResponse_id,
    createIndexResponse_httpStatus,

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

    -- ** DeleteAccessControlConfiguration
    deleteAccessControlConfiguration_indexId,
    deleteAccessControlConfiguration_id,
    deleteAccessControlConfigurationResponse_httpStatus,

    -- ** DeleteDataSource
    deleteDataSource_id,
    deleteDataSource_indexId,

    -- ** DeleteExperience
    deleteExperience_id,
    deleteExperience_indexId,
    deleteExperienceResponse_httpStatus,

    -- ** DeleteFaq
    deleteFaq_id,
    deleteFaq_indexId,

    -- ** DeleteIndex
    deleteIndex_id,

    -- ** DeletePrincipalMapping
    deletePrincipalMapping_dataSourceId,
    deletePrincipalMapping_orderingId,
    deletePrincipalMapping_indexId,
    deletePrincipalMapping_groupId,

    -- ** DeleteQuerySuggestionsBlockList
    deleteQuerySuggestionsBlockList_indexId,
    deleteQuerySuggestionsBlockList_id,

    -- ** DeleteThesaurus
    deleteThesaurus_id,
    deleteThesaurus_indexId,

    -- ** DescribeAccessControlConfiguration
    describeAccessControlConfiguration_indexId,
    describeAccessControlConfiguration_id,
    describeAccessControlConfigurationResponse_accessControlList,
    describeAccessControlConfigurationResponse_description,
    describeAccessControlConfigurationResponse_errorMessage,
    describeAccessControlConfigurationResponse_hierarchicalAccessControlList,
    describeAccessControlConfigurationResponse_httpStatus,
    describeAccessControlConfigurationResponse_name,

    -- ** DescribeDataSource
    describeDataSource_id,
    describeDataSource_indexId,
    describeDataSourceResponse_configuration,
    describeDataSourceResponse_createdAt,
    describeDataSourceResponse_customDocumentEnrichmentConfiguration,
    describeDataSourceResponse_description,
    describeDataSourceResponse_errorMessage,
    describeDataSourceResponse_id,
    describeDataSourceResponse_indexId,
    describeDataSourceResponse_languageCode,
    describeDataSourceResponse_name,
    describeDataSourceResponse_roleArn,
    describeDataSourceResponse_schedule,
    describeDataSourceResponse_status,
    describeDataSourceResponse_type,
    describeDataSourceResponse_updatedAt,
    describeDataSourceResponse_vpcConfiguration,
    describeDataSourceResponse_httpStatus,

    -- ** DescribeExperience
    describeExperience_id,
    describeExperience_indexId,
    describeExperienceResponse_configuration,
    describeExperienceResponse_createdAt,
    describeExperienceResponse_description,
    describeExperienceResponse_endpoints,
    describeExperienceResponse_errorMessage,
    describeExperienceResponse_id,
    describeExperienceResponse_indexId,
    describeExperienceResponse_name,
    describeExperienceResponse_roleArn,
    describeExperienceResponse_status,
    describeExperienceResponse_updatedAt,
    describeExperienceResponse_httpStatus,

    -- ** DescribeFaq
    describeFaq_id,
    describeFaq_indexId,
    describeFaqResponse_createdAt,
    describeFaqResponse_description,
    describeFaqResponse_errorMessage,
    describeFaqResponse_fileFormat,
    describeFaqResponse_id,
    describeFaqResponse_indexId,
    describeFaqResponse_languageCode,
    describeFaqResponse_name,
    describeFaqResponse_roleArn,
    describeFaqResponse_s3Path,
    describeFaqResponse_status,
    describeFaqResponse_updatedAt,
    describeFaqResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_id,
    describeIndexResponse_capacityUnits,
    describeIndexResponse_createdAt,
    describeIndexResponse_description,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_edition,
    describeIndexResponse_errorMessage,
    describeIndexResponse_id,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_name,
    describeIndexResponse_roleArn,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_status,
    describeIndexResponse_updatedAt,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_httpStatus,

    -- ** DescribePrincipalMapping
    describePrincipalMapping_dataSourceId,
    describePrincipalMapping_indexId,
    describePrincipalMapping_groupId,
    describePrincipalMappingResponse_dataSourceId,
    describePrincipalMappingResponse_groupId,
    describePrincipalMappingResponse_groupOrderingIdSummaries,
    describePrincipalMappingResponse_indexId,
    describePrincipalMappingResponse_httpStatus,

    -- ** DescribeQuerySuggestionsBlockList
    describeQuerySuggestionsBlockList_indexId,
    describeQuerySuggestionsBlockList_id,
    describeQuerySuggestionsBlockListResponse_createdAt,
    describeQuerySuggestionsBlockListResponse_description,
    describeQuerySuggestionsBlockListResponse_errorMessage,
    describeQuerySuggestionsBlockListResponse_fileSizeBytes,
    describeQuerySuggestionsBlockListResponse_id,
    describeQuerySuggestionsBlockListResponse_indexId,
    describeQuerySuggestionsBlockListResponse_itemCount,
    describeQuerySuggestionsBlockListResponse_name,
    describeQuerySuggestionsBlockListResponse_roleArn,
    describeQuerySuggestionsBlockListResponse_sourceS3Path,
    describeQuerySuggestionsBlockListResponse_status,
    describeQuerySuggestionsBlockListResponse_updatedAt,
    describeQuerySuggestionsBlockListResponse_httpStatus,

    -- ** DescribeQuerySuggestionsConfig
    describeQuerySuggestionsConfig_indexId,
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_status,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_httpStatus,

    -- ** DescribeThesaurus
    describeThesaurus_id,
    describeThesaurus_indexId,
    describeThesaurusResponse_createdAt,
    describeThesaurusResponse_description,
    describeThesaurusResponse_errorMessage,
    describeThesaurusResponse_fileSizeBytes,
    describeThesaurusResponse_id,
    describeThesaurusResponse_indexId,
    describeThesaurusResponse_name,
    describeThesaurusResponse_roleArn,
    describeThesaurusResponse_sourceS3Path,
    describeThesaurusResponse_status,
    describeThesaurusResponse_synonymRuleCount,
    describeThesaurusResponse_termCount,
    describeThesaurusResponse_updatedAt,
    describeThesaurusResponse_httpStatus,

    -- ** DisassociateEntitiesFromExperience
    disassociateEntitiesFromExperience_id,
    disassociateEntitiesFromExperience_indexId,
    disassociateEntitiesFromExperience_entityList,
    disassociateEntitiesFromExperienceResponse_failedEntityList,
    disassociateEntitiesFromExperienceResponse_httpStatus,

    -- ** DisassociatePersonasFromEntities
    disassociatePersonasFromEntities_id,
    disassociatePersonasFromEntities_indexId,
    disassociatePersonasFromEntities_entityIds,
    disassociatePersonasFromEntitiesResponse_failedEntityList,
    disassociatePersonasFromEntitiesResponse_httpStatus,

    -- ** GetQuerySuggestions
    getQuerySuggestions_maxSuggestionsCount,
    getQuerySuggestions_indexId,
    getQuerySuggestions_queryText,
    getQuerySuggestionsResponse_querySuggestionsId,
    getQuerySuggestionsResponse_suggestions,
    getQuerySuggestionsResponse_httpStatus,

    -- ** GetSnapshots
    getSnapshots_maxResults,
    getSnapshots_nextToken,
    getSnapshots_indexId,
    getSnapshots_interval,
    getSnapshots_metricType,
    getSnapshotsResponse_nextToken,
    getSnapshotsResponse_snapShotTimeFilter,
    getSnapshotsResponse_snapshotsData,
    getSnapshotsResponse_snapshotsDataHeader,
    getSnapshotsResponse_httpStatus,

    -- ** ListAccessControlConfigurations
    listAccessControlConfigurations_maxResults,
    listAccessControlConfigurations_nextToken,
    listAccessControlConfigurations_indexId,
    listAccessControlConfigurationsResponse_nextToken,
    listAccessControlConfigurationsResponse_httpStatus,
    listAccessControlConfigurationsResponse_accessControlConfigurations,

    -- ** ListDataSourceSyncJobs
    listDataSourceSyncJobs_maxResults,
    listDataSourceSyncJobs_nextToken,
    listDataSourceSyncJobs_startTimeFilter,
    listDataSourceSyncJobs_statusFilter,
    listDataSourceSyncJobs_id,
    listDataSourceSyncJobs_indexId,
    listDataSourceSyncJobsResponse_history,
    listDataSourceSyncJobsResponse_nextToken,
    listDataSourceSyncJobsResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_maxResults,
    listDataSources_nextToken,
    listDataSources_indexId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_summaryItems,
    listDataSourcesResponse_httpStatus,

    -- ** ListEntityPersonas
    listEntityPersonas_maxResults,
    listEntityPersonas_nextToken,
    listEntityPersonas_id,
    listEntityPersonas_indexId,
    listEntityPersonasResponse_nextToken,
    listEntityPersonasResponse_summaryItems,
    listEntityPersonasResponse_httpStatus,

    -- ** ListExperienceEntities
    listExperienceEntities_nextToken,
    listExperienceEntities_id,
    listExperienceEntities_indexId,
    listExperienceEntitiesResponse_nextToken,
    listExperienceEntitiesResponse_summaryItems,
    listExperienceEntitiesResponse_httpStatus,

    -- ** ListExperiences
    listExperiences_maxResults,
    listExperiences_nextToken,
    listExperiences_indexId,
    listExperiencesResponse_nextToken,
    listExperiencesResponse_summaryItems,
    listExperiencesResponse_httpStatus,

    -- ** ListFaqs
    listFaqs_maxResults,
    listFaqs_nextToken,
    listFaqs_indexId,
    listFaqsResponse_faqSummaryItems,
    listFaqsResponse_nextToken,
    listFaqsResponse_httpStatus,

    -- ** ListGroupsOlderThanOrderingId
    listGroupsOlderThanOrderingId_dataSourceId,
    listGroupsOlderThanOrderingId_maxResults,
    listGroupsOlderThanOrderingId_nextToken,
    listGroupsOlderThanOrderingId_indexId,
    listGroupsOlderThanOrderingId_orderingId,
    listGroupsOlderThanOrderingIdResponse_groupsSummaries,
    listGroupsOlderThanOrderingIdResponse_nextToken,
    listGroupsOlderThanOrderingIdResponse_httpStatus,

    -- ** ListIndices
    listIndices_maxResults,
    listIndices_nextToken,
    listIndicesResponse_indexConfigurationSummaryItems,
    listIndicesResponse_nextToken,
    listIndicesResponse_httpStatus,

    -- ** ListQuerySuggestionsBlockLists
    listQuerySuggestionsBlockLists_maxResults,
    listQuerySuggestionsBlockLists_nextToken,
    listQuerySuggestionsBlockLists_indexId,
    listQuerySuggestionsBlockListsResponse_blockListSummaryItems,
    listQuerySuggestionsBlockListsResponse_nextToken,
    listQuerySuggestionsBlockListsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThesauri
    listThesauri_maxResults,
    listThesauri_nextToken,
    listThesauri_indexId,
    listThesauriResponse_nextToken,
    listThesauriResponse_thesaurusSummaryItems,
    listThesauriResponse_httpStatus,

    -- ** PutPrincipalMapping
    putPrincipalMapping_dataSourceId,
    putPrincipalMapping_orderingId,
    putPrincipalMapping_roleArn,
    putPrincipalMapping_indexId,
    putPrincipalMapping_groupId,
    putPrincipalMapping_groupMembers,

    -- ** Query
    query_attributeFilter,
    query_documentRelevanceOverrideConfigurations,
    query_facets,
    query_pageNumber,
    query_pageSize,
    query_queryResultTypeFilter,
    query_queryText,
    query_requestedDocumentAttributes,
    query_sortingConfiguration,
    query_spellCorrectionConfiguration,
    query_userContext,
    query_visitorId,
    query_indexId,
    queryResponse_facetResults,
    queryResponse_queryId,
    queryResponse_resultItems,
    queryResponse_spellCorrectedQueries,
    queryResponse_totalNumberOfResults,
    queryResponse_warnings,
    queryResponse_httpStatus,

    -- ** StartDataSourceSyncJob
    startDataSourceSyncJob_id,
    startDataSourceSyncJob_indexId,
    startDataSourceSyncJobResponse_executionId,
    startDataSourceSyncJobResponse_httpStatus,

    -- ** StopDataSourceSyncJob
    stopDataSourceSyncJob_id,
    stopDataSourceSyncJob_indexId,

    -- ** SubmitFeedback
    submitFeedback_clickFeedbackItems,
    submitFeedback_relevanceFeedbackItems,
    submitFeedback_indexId,
    submitFeedback_queryId,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAccessControlConfiguration
    updateAccessControlConfiguration_accessControlList,
    updateAccessControlConfiguration_description,
    updateAccessControlConfiguration_hierarchicalAccessControlList,
    updateAccessControlConfiguration_name,
    updateAccessControlConfiguration_indexId,
    updateAccessControlConfiguration_id,
    updateAccessControlConfigurationResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_configuration,
    updateDataSource_customDocumentEnrichmentConfiguration,
    updateDataSource_description,
    updateDataSource_languageCode,
    updateDataSource_name,
    updateDataSource_roleArn,
    updateDataSource_schedule,
    updateDataSource_vpcConfiguration,
    updateDataSource_id,
    updateDataSource_indexId,

    -- ** UpdateExperience
    updateExperience_configuration,
    updateExperience_description,
    updateExperience_name,
    updateExperience_roleArn,
    updateExperience_id,
    updateExperience_indexId,

    -- ** UpdateIndex
    updateIndex_capacityUnits,
    updateIndex_description,
    updateIndex_documentMetadataConfigurationUpdates,
    updateIndex_name,
    updateIndex_roleArn,
    updateIndex_userContextPolicy,
    updateIndex_userGroupResolutionConfiguration,
    updateIndex_userTokenConfigurations,
    updateIndex_id,

    -- ** UpdateQuerySuggestionsBlockList
    updateQuerySuggestionsBlockList_description,
    updateQuerySuggestionsBlockList_name,
    updateQuerySuggestionsBlockList_roleArn,
    updateQuerySuggestionsBlockList_sourceS3Path,
    updateQuerySuggestionsBlockList_indexId,
    updateQuerySuggestionsBlockList_id,

    -- ** UpdateQuerySuggestionsConfig
    updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation,
    updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers,
    updateQuerySuggestionsConfig_minimumQueryCount,
    updateQuerySuggestionsConfig_mode,
    updateQuerySuggestionsConfig_queryLogLookBackWindowInDays,
    updateQuerySuggestionsConfig_indexId,

    -- ** UpdateThesaurus
    updateThesaurus_description,
    updateThesaurus_name,
    updateThesaurus_roleArn,
    updateThesaurus_sourceS3Path,
    updateThesaurus_id,
    updateThesaurus_indexId,

    -- * Types

    -- ** AccessControlConfigurationSummary
    accessControlConfigurationSummary_id,

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

    -- ** AlfrescoConfiguration
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

    -- ** AttributeFilter
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

    -- ** AuthenticationConfiguration
    authenticationConfiguration_basicAuthentication,

    -- ** BasicAuthenticationConfiguration
    basicAuthenticationConfiguration_host,
    basicAuthenticationConfiguration_port,
    basicAuthenticationConfiguration_credentials,

    -- ** BatchDeleteDocumentResponseFailedDocument
    batchDeleteDocumentResponseFailedDocument_errorCode,
    batchDeleteDocumentResponseFailedDocument_errorMessage,
    batchDeleteDocumentResponseFailedDocument_id,

    -- ** BatchGetDocumentStatusResponseError
    batchGetDocumentStatusResponseError_documentId,
    batchGetDocumentStatusResponseError_errorCode,
    batchGetDocumentStatusResponseError_errorMessage,

    -- ** BatchPutDocumentResponseFailedDocument
    batchPutDocumentResponseFailedDocument_errorCode,
    batchPutDocumentResponseFailedDocument_errorMessage,
    batchPutDocumentResponseFailedDocument_id,

    -- ** BoxConfiguration
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

    -- ** CapacityUnitsConfiguration
    capacityUnitsConfiguration_storageCapacityUnits,
    capacityUnitsConfiguration_queryCapacityUnits,

    -- ** ClickFeedback
    clickFeedback_resultId,
    clickFeedback_clickTime,

    -- ** ColumnConfiguration
    columnConfiguration_documentTitleColumnName,
    columnConfiguration_fieldMappings,
    columnConfiguration_documentIdColumnName,
    columnConfiguration_documentDataColumnName,
    columnConfiguration_changeDetectingColumns,

    -- ** ConfluenceAttachmentConfiguration
    confluenceAttachmentConfiguration_attachmentFieldMappings,
    confluenceAttachmentConfiguration_crawlAttachments,

    -- ** ConfluenceAttachmentToIndexFieldMapping
    confluenceAttachmentToIndexFieldMapping_dataSourceFieldName,
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,
    confluenceAttachmentToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceBlogConfiguration
    confluenceBlogConfiguration_blogFieldMappings,

    -- ** ConfluenceBlogToIndexFieldMapping
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_dateFieldFormat,
    confluenceBlogToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceConfiguration
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

    -- ** ConfluencePageConfiguration
    confluencePageConfiguration_pageFieldMappings,

    -- ** ConfluencePageToIndexFieldMapping
    confluencePageToIndexFieldMapping_dataSourceFieldName,
    confluencePageToIndexFieldMapping_dateFieldFormat,
    confluencePageToIndexFieldMapping_indexFieldName,

    -- ** ConfluenceSpaceConfiguration
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_excludeSpaces,
    confluenceSpaceConfiguration_includeSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,

    -- ** ConfluenceSpaceToIndexFieldMapping
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,
    confluenceSpaceToIndexFieldMapping_indexFieldName,

    -- ** ConnectionConfiguration
    connectionConfiguration_databaseHost,
    connectionConfiguration_databasePort,
    connectionConfiguration_databaseName,
    connectionConfiguration_tableName,
    connectionConfiguration_secretArn,

    -- ** ContentSourceConfiguration
    contentSourceConfiguration_dataSourceIds,
    contentSourceConfiguration_directPutContent,
    contentSourceConfiguration_faqIds,

    -- ** Correction
    correction_beginOffset,
    correction_correctedTerm,
    correction_endOffset,
    correction_term,

    -- ** CustomDocumentEnrichmentConfiguration
    customDocumentEnrichmentConfiguration_inlineConfigurations,
    customDocumentEnrichmentConfiguration_postExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_preExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_roleArn,

    -- ** DataSourceConfiguration
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

    -- ** DataSourceGroup
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- ** DataSourceSummary
    dataSourceSummary_createdAt,
    dataSourceSummary_id,
    dataSourceSummary_languageCode,
    dataSourceSummary_name,
    dataSourceSummary_status,
    dataSourceSummary_type,
    dataSourceSummary_updatedAt,

    -- ** DataSourceSyncJob
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_errorMessage,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_startTime,
    dataSourceSyncJob_status,

    -- ** DataSourceSyncJobMetricTarget
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- ** DataSourceSyncJobMetrics
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsDeleted,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsModified,
    dataSourceSyncJobMetrics_documentsScanned,

    -- ** DataSourceToIndexFieldMapping
    dataSourceToIndexFieldMapping_dateFieldFormat,
    dataSourceToIndexFieldMapping_dataSourceFieldName,
    dataSourceToIndexFieldMapping_indexFieldName,

    -- ** DataSourceVpcConfiguration
    dataSourceVpcConfiguration_subnetIds,
    dataSourceVpcConfiguration_securityGroupIds,

    -- ** DatabaseConfiguration
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- ** Document
    document_accessControlConfigurationId,
    document_accessControlList,
    document_attributes,
    document_blob,
    document_contentType,
    document_hierarchicalAccessControlList,
    document_s3Path,
    document_title,
    document_id,

    -- ** DocumentAttribute
    documentAttribute_key,
    documentAttribute_value,

    -- ** DocumentAttributeCondition
    documentAttributeCondition_conditionOnValue,
    documentAttributeCondition_conditionDocumentAttributeKey,
    documentAttributeCondition_operator,

    -- ** DocumentAttributeTarget
    documentAttributeTarget_targetDocumentAttributeKey,
    documentAttributeTarget_targetDocumentAttributeValue,
    documentAttributeTarget_targetDocumentAttributeValueDeletion,

    -- ** DocumentAttributeValue
    documentAttributeValue_dateValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,
    documentAttributeValue_stringValue,

    -- ** DocumentAttributeValueCountPair
    documentAttributeValueCountPair_count,
    documentAttributeValueCountPair_documentAttributeValue,
    documentAttributeValueCountPair_facetResults,

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

    -- ** EntityConfiguration
    entityConfiguration_entityId,
    entityConfiguration_entityType,

    -- ** EntityDisplayData
    entityDisplayData_firstName,
    entityDisplayData_groupName,
    entityDisplayData_identifiedUserName,
    entityDisplayData_lastName,
    entityDisplayData_userName,

    -- ** EntityPersonaConfiguration
    entityPersonaConfiguration_entityId,
    entityPersonaConfiguration_persona,

    -- ** ExperienceConfiguration
    experienceConfiguration_contentSourceConfiguration,
    experienceConfiguration_userIdentityConfiguration,

    -- ** ExperienceEndpoint
    experienceEndpoint_endpoint,
    experienceEndpoint_endpointType,

    -- ** ExperienceEntitiesSummary
    experienceEntitiesSummary_displayData,
    experienceEntitiesSummary_entityId,
    experienceEntitiesSummary_entityType,

    -- ** ExperiencesSummary
    experiencesSummary_createdAt,
    experiencesSummary_endpoints,
    experiencesSummary_id,
    experiencesSummary_name,
    experiencesSummary_status,

    -- ** Facet
    facet_documentAttributeKey,
    facet_facets,
    facet_maxResults,

    -- ** FacetResult
    facetResult_documentAttributeKey,
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,

    -- ** FailedEntity
    failedEntity_entityId,
    failedEntity_errorMessage,

    -- ** FaqStatistics
    faqStatistics_indexedQuestionAnswersCount,

    -- ** FaqSummary
    faqSummary_createdAt,
    faqSummary_fileFormat,
    faqSummary_id,
    faqSummary_languageCode,
    faqSummary_name,
    faqSummary_status,
    faqSummary_updatedAt,

    -- ** FsxConfiguration
    fsxConfiguration_exclusionPatterns,
    fsxConfiguration_fieldMappings,
    fsxConfiguration_inclusionPatterns,
    fsxConfiguration_secretArn,
    fsxConfiguration_fileSystemId,
    fsxConfiguration_fileSystemType,
    fsxConfiguration_vpcConfiguration,

    -- ** GitHubConfiguration
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

    -- ** GitHubDocumentCrawlProperties
    gitHubDocumentCrawlProperties_crawlIssue,
    gitHubDocumentCrawlProperties_crawlIssueComment,
    gitHubDocumentCrawlProperties_crawlIssueCommentAttachment,
    gitHubDocumentCrawlProperties_crawlPullRequest,
    gitHubDocumentCrawlProperties_crawlPullRequestComment,
    gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment,
    gitHubDocumentCrawlProperties_crawlRepositoryDocuments,

    -- ** GoogleDriveConfiguration
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_secretArn,

    -- ** GroupMembers
    groupMembers_memberGroups,
    groupMembers_memberUsers,
    groupMembers_s3PathforGroupMembers,

    -- ** GroupOrderingIdSummary
    groupOrderingIdSummary_failureReason,
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_orderingId,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_status,

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

    -- ** HookConfiguration
    hookConfiguration_invocationCondition,
    hookConfiguration_lambdaArn,
    hookConfiguration_s3Bucket,

    -- ** IndexConfigurationSummary
    indexConfigurationSummary_edition,
    indexConfigurationSummary_id,
    indexConfigurationSummary_name,
    indexConfigurationSummary_createdAt,
    indexConfigurationSummary_updatedAt,
    indexConfigurationSummary_status,

    -- ** IndexStatistics
    indexStatistics_faqStatistics,
    indexStatistics_textDocumentStatistics,

    -- ** InlineCustomDocumentEnrichmentConfiguration
    inlineCustomDocumentEnrichmentConfiguration_condition,
    inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion,
    inlineCustomDocumentEnrichmentConfiguration_target,

    -- ** JiraConfiguration
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

    -- ** JsonTokenTypeConfiguration
    jsonTokenTypeConfiguration_userNameAttributeField,
    jsonTokenTypeConfiguration_groupAttributeField,

    -- ** JwtTokenTypeConfiguration
    jwtTokenTypeConfiguration_claimRegex,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_issuer,
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_keyLocation,

    -- ** MemberGroup
    memberGroup_dataSourceId,
    memberGroup_groupId,

    -- ** MemberUser
    memberUser_userId,

    -- ** OnPremiseConfiguration
    onPremiseConfiguration_hostUrl,
    onPremiseConfiguration_organizationName,
    onPremiseConfiguration_sslCertificateS3Path,

    -- ** OneDriveConfiguration
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_exclusionPatterns,
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_tenantDomain,
    oneDriveConfiguration_secretArn,
    oneDriveConfiguration_oneDriveUsers,

    -- ** OneDriveUsers
    oneDriveUsers_oneDriveUserList,
    oneDriveUsers_oneDriveUserS3Path,

    -- ** PersonasSummary
    personasSummary_createdAt,
    personasSummary_entityId,
    personasSummary_persona,
    personasSummary_updatedAt,

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

    -- ** QuerySuggestionsBlockListSummary
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_itemCount,
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_updatedAt,

    -- ** QuipConfiguration
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

    -- ** Relevance
    relevance_duration,
    relevance_freshness,
    relevance_importance,
    relevance_rankOrder,
    relevance_valueImportanceMap,

    -- ** RelevanceFeedback
    relevanceFeedback_resultId,
    relevanceFeedback_relevanceValue,

    -- ** S3DataSourceConfiguration
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_exclusionPatterns,
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_bucketName,

    -- ** S3Path
    s3Path_bucket,
    s3Path_key,

    -- ** SaaSConfiguration
    saaSConfiguration_organizationName,
    saaSConfiguration_hostUrl,

    -- ** SalesforceChatterFeedConfiguration
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- ** SalesforceConfiguration
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_includeAttachmentFilePatterns,
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_serverUrl,
    salesforceConfiguration_secretArn,

    -- ** SalesforceCustomKnowledgeArticleTypeConfiguration
    salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceCustomKnowledgeArticleTypeConfiguration_name,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- ** SalesforceKnowledgeArticleConfiguration
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
    salesforceKnowledgeArticleConfiguration_includedStates,

    -- ** SalesforceStandardKnowledgeArticleTypeConfiguration
    salesforceStandardKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceStandardKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceStandardKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- ** SalesforceStandardObjectAttachmentConfiguration
    salesforceStandardObjectAttachmentConfiguration_documentTitleFieldName,
    salesforceStandardObjectAttachmentConfiguration_fieldMappings,

    -- ** SalesforceStandardObjectConfiguration
    salesforceStandardObjectConfiguration_documentTitleFieldName,
    salesforceStandardObjectConfiguration_fieldMappings,
    salesforceStandardObjectConfiguration_name,
    salesforceStandardObjectConfiguration_documentDataFieldName,

    -- ** ScoreAttributes
    scoreAttributes_scoreConfidence,

    -- ** Search
    search_displayable,
    search_facetable,
    search_searchable,
    search_sortable,

    -- ** SeedUrlConfiguration
    seedUrlConfiguration_webCrawlerMode,
    seedUrlConfiguration_seedUrls,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_kmsKeyId,

    -- ** ServiceNowConfiguration
    serviceNowConfiguration_authenticationType,
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- ** ServiceNowKnowledgeArticleConfiguration
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- ** ServiceNowServiceCatalogConfiguration
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- ** SharePointConfiguration
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

    -- ** SiteMapsConfiguration
    siteMapsConfiguration_siteMaps,

    -- ** SlackConfiguration
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

    -- ** SortingConfiguration
    sortingConfiguration_documentAttributeKey,
    sortingConfiguration_sortOrder,

    -- ** SpellCorrectedQuery
    spellCorrectedQuery_corrections,
    spellCorrectedQuery_suggestedQueryText,

    -- ** SpellCorrectionConfiguration
    spellCorrectionConfiguration_includeQuerySpellCheckSuggestions,

    -- ** SqlConfiguration
    sqlConfiguration_queryIdentifiersEnclosingOption,

    -- ** Status
    status_documentId,
    status_documentStatus,
    status_failureCode,
    status_failureReason,

    -- ** Suggestion
    suggestion_id,
    suggestion_value,

    -- ** SuggestionHighlight
    suggestionHighlight_beginOffset,
    suggestionHighlight_endOffset,

    -- ** SuggestionTextWithHighlights
    suggestionTextWithHighlights_highlights,
    suggestionTextWithHighlights_text,

    -- ** SuggestionValue
    suggestionValue_text,

    -- ** TableCell
    tableCell_header,
    tableCell_highlighted,
    tableCell_topAnswer,
    tableCell_value,

    -- ** TableExcerpt
    tableExcerpt_rows,
    tableExcerpt_totalNumberOfRows,

    -- ** TableRow
    tableRow_cells,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Template

    -- ** TemplateConfiguration
    templateConfiguration_template,

    -- ** TextDocumentStatistics
    textDocumentStatistics_indexedTextDocumentsCount,
    textDocumentStatistics_indexedTextBytes,

    -- ** TextWithHighlights
    textWithHighlights_highlights,
    textWithHighlights_text,

    -- ** ThesaurusSummary
    thesaurusSummary_createdAt,
    thesaurusSummary_id,
    thesaurusSummary_name,
    thesaurusSummary_status,
    thesaurusSummary_updatedAt,

    -- ** TimeRange
    timeRange_endTime,
    timeRange_startTime,

    -- ** Urls
    urls_seedUrlConfiguration,
    urls_siteMapsConfiguration,

    -- ** UserContext
    userContext_dataSourceGroups,
    userContext_groups,
    userContext_token,
    userContext_userId,

    -- ** UserGroupResolutionConfiguration
    userGroupResolutionConfiguration_userGroupResolutionMode,

    -- ** UserIdentityConfiguration
    userIdentityConfiguration_identityAttributeName,

    -- ** UserTokenConfiguration
    userTokenConfiguration_jsonTokenTypeConfiguration,
    userTokenConfiguration_jwtTokenTypeConfiguration,

    -- ** Warning
    warning_code,
    warning_message,

    -- ** WebCrawlerConfiguration
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- ** WorkDocsConfiguration
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_exclusionPatterns,
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_organizationId,
  )
where

import Amazonka.Kendra.AssociateEntitiesToExperience
import Amazonka.Kendra.AssociatePersonasToEntities
import Amazonka.Kendra.BatchDeleteDocument
import Amazonka.Kendra.BatchGetDocumentStatus
import Amazonka.Kendra.BatchPutDocument
import Amazonka.Kendra.ClearQuerySuggestions
import Amazonka.Kendra.CreateAccessControlConfiguration
import Amazonka.Kendra.CreateDataSource
import Amazonka.Kendra.CreateExperience
import Amazonka.Kendra.CreateFaq
import Amazonka.Kendra.CreateIndex
import Amazonka.Kendra.CreateQuerySuggestionsBlockList
import Amazonka.Kendra.CreateThesaurus
import Amazonka.Kendra.DeleteAccessControlConfiguration
import Amazonka.Kendra.DeleteDataSource
import Amazonka.Kendra.DeleteExperience
import Amazonka.Kendra.DeleteFaq
import Amazonka.Kendra.DeleteIndex
import Amazonka.Kendra.DeletePrincipalMapping
import Amazonka.Kendra.DeleteQuerySuggestionsBlockList
import Amazonka.Kendra.DeleteThesaurus
import Amazonka.Kendra.DescribeAccessControlConfiguration
import Amazonka.Kendra.DescribeDataSource
import Amazonka.Kendra.DescribeExperience
import Amazonka.Kendra.DescribeFaq
import Amazonka.Kendra.DescribeIndex
import Amazonka.Kendra.DescribePrincipalMapping
import Amazonka.Kendra.DescribeQuerySuggestionsBlockList
import Amazonka.Kendra.DescribeQuerySuggestionsConfig
import Amazonka.Kendra.DescribeThesaurus
import Amazonka.Kendra.DisassociateEntitiesFromExperience
import Amazonka.Kendra.DisassociatePersonasFromEntities
import Amazonka.Kendra.GetQuerySuggestions
import Amazonka.Kendra.GetSnapshots
import Amazonka.Kendra.ListAccessControlConfigurations
import Amazonka.Kendra.ListDataSourceSyncJobs
import Amazonka.Kendra.ListDataSources
import Amazonka.Kendra.ListEntityPersonas
import Amazonka.Kendra.ListExperienceEntities
import Amazonka.Kendra.ListExperiences
import Amazonka.Kendra.ListFaqs
import Amazonka.Kendra.ListGroupsOlderThanOrderingId
import Amazonka.Kendra.ListIndices
import Amazonka.Kendra.ListQuerySuggestionsBlockLists
import Amazonka.Kendra.ListTagsForResource
import Amazonka.Kendra.ListThesauri
import Amazonka.Kendra.PutPrincipalMapping
import Amazonka.Kendra.Query
import Amazonka.Kendra.StartDataSourceSyncJob
import Amazonka.Kendra.StopDataSourceSyncJob
import Amazonka.Kendra.SubmitFeedback
import Amazonka.Kendra.TagResource
import Amazonka.Kendra.Types.AccessControlConfigurationSummary
import Amazonka.Kendra.Types.AccessControlListConfiguration
import Amazonka.Kendra.Types.AclConfiguration
import Amazonka.Kendra.Types.AdditionalResultAttribute
import Amazonka.Kendra.Types.AdditionalResultAttributeValue
import Amazonka.Kendra.Types.AlfrescoConfiguration
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
import Amazonka.Kendra.Types.ConfluenceAttachmentConfiguration
import Amazonka.Kendra.Types.ConfluenceAttachmentToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceBlogConfiguration
import Amazonka.Kendra.Types.ConfluenceBlogToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceConfiguration
import Amazonka.Kendra.Types.ConfluencePageConfiguration
import Amazonka.Kendra.Types.ConfluencePageToIndexFieldMapping
import Amazonka.Kendra.Types.ConfluenceSpaceConfiguration
import Amazonka.Kendra.Types.ConfluenceSpaceToIndexFieldMapping
import Amazonka.Kendra.Types.ConnectionConfiguration
import Amazonka.Kendra.Types.ContentSourceConfiguration
import Amazonka.Kendra.Types.Correction
import Amazonka.Kendra.Types.CustomDocumentEnrichmentConfiguration
import Amazonka.Kendra.Types.DataSourceConfiguration
import Amazonka.Kendra.Types.DataSourceGroup
import Amazonka.Kendra.Types.DataSourceSummary
import Amazonka.Kendra.Types.DataSourceSyncJob
import Amazonka.Kendra.Types.DataSourceSyncJobMetricTarget
import Amazonka.Kendra.Types.DataSourceSyncJobMetrics
import Amazonka.Kendra.Types.DataSourceToIndexFieldMapping
import Amazonka.Kendra.Types.DataSourceVpcConfiguration
import Amazonka.Kendra.Types.DatabaseConfiguration
import Amazonka.Kendra.Types.Document
import Amazonka.Kendra.Types.DocumentAttribute
import Amazonka.Kendra.Types.DocumentAttributeCondition
import Amazonka.Kendra.Types.DocumentAttributeTarget
import Amazonka.Kendra.Types.DocumentAttributeValue
import Amazonka.Kendra.Types.DocumentAttributeValueCountPair
import Amazonka.Kendra.Types.DocumentInfo
import Amazonka.Kendra.Types.DocumentMetadataConfiguration
import Amazonka.Kendra.Types.DocumentRelevanceConfiguration
import Amazonka.Kendra.Types.DocumentsMetadataConfiguration
import Amazonka.Kendra.Types.EntityConfiguration
import Amazonka.Kendra.Types.EntityDisplayData
import Amazonka.Kendra.Types.EntityPersonaConfiguration
import Amazonka.Kendra.Types.ExperienceConfiguration
import Amazonka.Kendra.Types.ExperienceEndpoint
import Amazonka.Kendra.Types.ExperienceEntitiesSummary
import Amazonka.Kendra.Types.ExperiencesSummary
import Amazonka.Kendra.Types.Facet
import Amazonka.Kendra.Types.FacetResult
import Amazonka.Kendra.Types.FailedEntity
import Amazonka.Kendra.Types.FaqStatistics
import Amazonka.Kendra.Types.FaqSummary
import Amazonka.Kendra.Types.FsxConfiguration
import Amazonka.Kendra.Types.GitHubConfiguration
import Amazonka.Kendra.Types.GitHubDocumentCrawlProperties
import Amazonka.Kendra.Types.GoogleDriveConfiguration
import Amazonka.Kendra.Types.GroupMembers
import Amazonka.Kendra.Types.GroupOrderingIdSummary
import Amazonka.Kendra.Types.GroupSummary
import Amazonka.Kendra.Types.HierarchicalPrincipal
import Amazonka.Kendra.Types.Highlight
import Amazonka.Kendra.Types.HookConfiguration
import Amazonka.Kendra.Types.IndexConfigurationSummary
import Amazonka.Kendra.Types.IndexStatistics
import Amazonka.Kendra.Types.InlineCustomDocumentEnrichmentConfiguration
import Amazonka.Kendra.Types.JiraConfiguration
import Amazonka.Kendra.Types.JsonTokenTypeConfiguration
import Amazonka.Kendra.Types.JwtTokenTypeConfiguration
import Amazonka.Kendra.Types.MemberGroup
import Amazonka.Kendra.Types.MemberUser
import Amazonka.Kendra.Types.OnPremiseConfiguration
import Amazonka.Kendra.Types.OneDriveConfiguration
import Amazonka.Kendra.Types.OneDriveUsers
import Amazonka.Kendra.Types.PersonasSummary
import Amazonka.Kendra.Types.Principal
import Amazonka.Kendra.Types.ProxyConfiguration
import Amazonka.Kendra.Types.QueryResultItem
import Amazonka.Kendra.Types.QuerySuggestionsBlockListSummary
import Amazonka.Kendra.Types.QuipConfiguration
import Amazonka.Kendra.Types.Relevance
import Amazonka.Kendra.Types.RelevanceFeedback
import Amazonka.Kendra.Types.S3DataSourceConfiguration
import Amazonka.Kendra.Types.S3Path
import Amazonka.Kendra.Types.SaaSConfiguration
import Amazonka.Kendra.Types.SalesforceChatterFeedConfiguration
import Amazonka.Kendra.Types.SalesforceConfiguration
import Amazonka.Kendra.Types.SalesforceCustomKnowledgeArticleTypeConfiguration
import Amazonka.Kendra.Types.SalesforceKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.SalesforceStandardKnowledgeArticleTypeConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectAttachmentConfiguration
import Amazonka.Kendra.Types.SalesforceStandardObjectConfiguration
import Amazonka.Kendra.Types.ScoreAttributes
import Amazonka.Kendra.Types.Search
import Amazonka.Kendra.Types.SeedUrlConfiguration
import Amazonka.Kendra.Types.ServerSideEncryptionConfiguration
import Amazonka.Kendra.Types.ServiceNowConfiguration
import Amazonka.Kendra.Types.ServiceNowKnowledgeArticleConfiguration
import Amazonka.Kendra.Types.ServiceNowServiceCatalogConfiguration
import Amazonka.Kendra.Types.SharePointConfiguration
import Amazonka.Kendra.Types.SiteMapsConfiguration
import Amazonka.Kendra.Types.SlackConfiguration
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
import Amazonka.Kendra.Types.ThesaurusSummary
import Amazonka.Kendra.Types.TimeRange
import Amazonka.Kendra.Types.Urls
import Amazonka.Kendra.Types.UserContext
import Amazonka.Kendra.Types.UserGroupResolutionConfiguration
import Amazonka.Kendra.Types.UserIdentityConfiguration
import Amazonka.Kendra.Types.UserTokenConfiguration
import Amazonka.Kendra.Types.Warning
import Amazonka.Kendra.Types.WebCrawlerConfiguration
import Amazonka.Kendra.Types.WorkDocsConfiguration
import Amazonka.Kendra.UntagResource
import Amazonka.Kendra.UpdateAccessControlConfiguration
import Amazonka.Kendra.UpdateDataSource
import Amazonka.Kendra.UpdateExperience
import Amazonka.Kendra.UpdateIndex
import Amazonka.Kendra.UpdateQuerySuggestionsBlockList
import Amazonka.Kendra.UpdateQuerySuggestionsConfig
import Amazonka.Kendra.UpdateThesaurus
