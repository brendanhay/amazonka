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
    createDataSource_tags,
    createDataSource_schedule,
    createDataSource_vpcConfiguration,
    createDataSource_customDocumentEnrichmentConfiguration,
    createDataSource_clientToken,
    createDataSource_roleArn,
    createDataSource_configuration,
    createDataSource_description,
    createDataSource_languageCode,
    createDataSource_name,
    createDataSource_indexId,
    createDataSource_type,
    createDataSourceResponse_httpStatus,
    createDataSourceResponse_id,

    -- ** CreateExperience
    createExperience_clientToken,
    createExperience_roleArn,
    createExperience_configuration,
    createExperience_description,
    createExperience_name,
    createExperience_indexId,
    createExperienceResponse_httpStatus,
    createExperienceResponse_id,

    -- ** CreateFaq
    createFaq_tags,
    createFaq_clientToken,
    createFaq_description,
    createFaq_languageCode,
    createFaq_fileFormat,
    createFaq_indexId,
    createFaq_name,
    createFaq_s3Path,
    createFaq_roleArn,
    createFaqResponse_id,
    createFaqResponse_httpStatus,

    -- ** CreateIndex
    createIndex_tags,
    createIndex_clientToken,
    createIndex_userGroupResolutionConfiguration,
    createIndex_serverSideEncryptionConfiguration,
    createIndex_edition,
    createIndex_description,
    createIndex_userTokenConfigurations,
    createIndex_userContextPolicy,
    createIndex_name,
    createIndex_roleArn,
    createIndexResponse_id,
    createIndexResponse_httpStatus,

    -- ** CreateQuerySuggestionsBlockList
    createQuerySuggestionsBlockList_tags,
    createQuerySuggestionsBlockList_clientToken,
    createQuerySuggestionsBlockList_description,
    createQuerySuggestionsBlockList_indexId,
    createQuerySuggestionsBlockList_name,
    createQuerySuggestionsBlockList_sourceS3Path,
    createQuerySuggestionsBlockList_roleArn,
    createQuerySuggestionsBlockListResponse_id,
    createQuerySuggestionsBlockListResponse_httpStatus,

    -- ** CreateThesaurus
    createThesaurus_tags,
    createThesaurus_clientToken,
    createThesaurus_description,
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
    describeAccessControlConfigurationResponse_errorMessage,
    describeAccessControlConfigurationResponse_description,
    describeAccessControlConfigurationResponse_hierarchicalAccessControlList,
    describeAccessControlConfigurationResponse_httpStatus,
    describeAccessControlConfigurationResponse_name,

    -- ** DescribeDataSource
    describeDataSource_id,
    describeDataSource_indexId,
    describeDataSourceResponse_schedule,
    describeDataSourceResponse_vpcConfiguration,
    describeDataSourceResponse_customDocumentEnrichmentConfiguration,
    describeDataSourceResponse_indexId,
    describeDataSourceResponse_name,
    describeDataSourceResponse_type,
    describeDataSourceResponse_roleArn,
    describeDataSourceResponse_errorMessage,
    describeDataSourceResponse_configuration,
    describeDataSourceResponse_status,
    describeDataSourceResponse_id,
    describeDataSourceResponse_description,
    describeDataSourceResponse_languageCode,
    describeDataSourceResponse_createdAt,
    describeDataSourceResponse_updatedAt,
    describeDataSourceResponse_httpStatus,

    -- ** DescribeExperience
    describeExperience_id,
    describeExperience_indexId,
    describeExperienceResponse_indexId,
    describeExperienceResponse_name,
    describeExperienceResponse_roleArn,
    describeExperienceResponse_errorMessage,
    describeExperienceResponse_configuration,
    describeExperienceResponse_status,
    describeExperienceResponse_endpoints,
    describeExperienceResponse_id,
    describeExperienceResponse_description,
    describeExperienceResponse_createdAt,
    describeExperienceResponse_updatedAt,
    describeExperienceResponse_httpStatus,

    -- ** DescribeFaq
    describeFaq_id,
    describeFaq_indexId,
    describeFaqResponse_indexId,
    describeFaqResponse_name,
    describeFaqResponse_roleArn,
    describeFaqResponse_errorMessage,
    describeFaqResponse_status,
    describeFaqResponse_id,
    describeFaqResponse_description,
    describeFaqResponse_languageCode,
    describeFaqResponse_s3Path,
    describeFaqResponse_createdAt,
    describeFaqResponse_updatedAt,
    describeFaqResponse_fileFormat,
    describeFaqResponse_httpStatus,

    -- ** DescribeIndex
    describeIndex_id,
    describeIndexResponse_name,
    describeIndexResponse_roleArn,
    describeIndexResponse_capacityUnits,
    describeIndexResponse_errorMessage,
    describeIndexResponse_userGroupResolutionConfiguration,
    describeIndexResponse_serverSideEncryptionConfiguration,
    describeIndexResponse_edition,
    describeIndexResponse_status,
    describeIndexResponse_id,
    describeIndexResponse_description,
    describeIndexResponse_userTokenConfigurations,
    describeIndexResponse_userContextPolicy,
    describeIndexResponse_indexStatistics,
    describeIndexResponse_createdAt,
    describeIndexResponse_updatedAt,
    describeIndexResponse_documentMetadataConfigurations,
    describeIndexResponse_httpStatus,

    -- ** DescribePrincipalMapping
    describePrincipalMapping_dataSourceId,
    describePrincipalMapping_indexId,
    describePrincipalMapping_groupId,
    describePrincipalMappingResponse_groupOrderingIdSummaries,
    describePrincipalMappingResponse_indexId,
    describePrincipalMappingResponse_dataSourceId,
    describePrincipalMappingResponse_groupId,
    describePrincipalMappingResponse_httpStatus,

    -- ** DescribeQuerySuggestionsBlockList
    describeQuerySuggestionsBlockList_indexId,
    describeQuerySuggestionsBlockList_id,
    describeQuerySuggestionsBlockListResponse_indexId,
    describeQuerySuggestionsBlockListResponse_name,
    describeQuerySuggestionsBlockListResponse_roleArn,
    describeQuerySuggestionsBlockListResponse_errorMessage,
    describeQuerySuggestionsBlockListResponse_itemCount,
    describeQuerySuggestionsBlockListResponse_status,
    describeQuerySuggestionsBlockListResponse_id,
    describeQuerySuggestionsBlockListResponse_description,
    describeQuerySuggestionsBlockListResponse_sourceS3Path,
    describeQuerySuggestionsBlockListResponse_fileSizeBytes,
    describeQuerySuggestionsBlockListResponse_createdAt,
    describeQuerySuggestionsBlockListResponse_updatedAt,
    describeQuerySuggestionsBlockListResponse_httpStatus,

    -- ** DescribeQuerySuggestionsConfig
    describeQuerySuggestionsConfig_indexId,
    describeQuerySuggestionsConfigResponse_status,
    describeQuerySuggestionsConfigResponse_minimumNumberOfQueryingUsers,
    describeQuerySuggestionsConfigResponse_totalSuggestionsCount,
    describeQuerySuggestionsConfigResponse_lastSuggestionsBuildTime,
    describeQuerySuggestionsConfigResponse_queryLogLookBackWindowInDays,
    describeQuerySuggestionsConfigResponse_mode,
    describeQuerySuggestionsConfigResponse_minimumQueryCount,
    describeQuerySuggestionsConfigResponse_includeQueriesWithoutUserInformation,
    describeQuerySuggestionsConfigResponse_lastClearTime,
    describeQuerySuggestionsConfigResponse_httpStatus,

    -- ** DescribeThesaurus
    describeThesaurus_id,
    describeThesaurus_indexId,
    describeThesaurusResponse_indexId,
    describeThesaurusResponse_name,
    describeThesaurusResponse_roleArn,
    describeThesaurusResponse_termCount,
    describeThesaurusResponse_errorMessage,
    describeThesaurusResponse_status,
    describeThesaurusResponse_id,
    describeThesaurusResponse_description,
    describeThesaurusResponse_synonymRuleCount,
    describeThesaurusResponse_sourceS3Path,
    describeThesaurusResponse_fileSizeBytes,
    describeThesaurusResponse_createdAt,
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
    getSnapshots_nextToken,
    getSnapshots_maxResults,
    getSnapshots_indexId,
    getSnapshots_interval,
    getSnapshots_metricType,
    getSnapshotsResponse_snapShotTimeFilter,
    getSnapshotsResponse_nextToken,
    getSnapshotsResponse_snapshotsDataHeader,
    getSnapshotsResponse_snapshotsData,
    getSnapshotsResponse_httpStatus,

    -- ** ListAccessControlConfigurations
    listAccessControlConfigurations_nextToken,
    listAccessControlConfigurations_maxResults,
    listAccessControlConfigurations_indexId,
    listAccessControlConfigurationsResponse_nextToken,
    listAccessControlConfigurationsResponse_httpStatus,
    listAccessControlConfigurationsResponse_accessControlConfigurations,

    -- ** ListDataSourceSyncJobs
    listDataSourceSyncJobs_nextToken,
    listDataSourceSyncJobs_maxResults,
    listDataSourceSyncJobs_startTimeFilter,
    listDataSourceSyncJobs_statusFilter,
    listDataSourceSyncJobs_id,
    listDataSourceSyncJobs_indexId,
    listDataSourceSyncJobsResponse_nextToken,
    listDataSourceSyncJobsResponse_history,
    listDataSourceSyncJobsResponse_httpStatus,

    -- ** ListDataSources
    listDataSources_nextToken,
    listDataSources_maxResults,
    listDataSources_indexId,
    listDataSourcesResponse_nextToken,
    listDataSourcesResponse_summaryItems,
    listDataSourcesResponse_httpStatus,

    -- ** ListEntityPersonas
    listEntityPersonas_nextToken,
    listEntityPersonas_maxResults,
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
    listExperiences_nextToken,
    listExperiences_maxResults,
    listExperiences_indexId,
    listExperiencesResponse_nextToken,
    listExperiencesResponse_summaryItems,
    listExperiencesResponse_httpStatus,

    -- ** ListFaqs
    listFaqs_nextToken,
    listFaqs_maxResults,
    listFaqs_indexId,
    listFaqsResponse_nextToken,
    listFaqsResponse_faqSummaryItems,
    listFaqsResponse_httpStatus,

    -- ** ListGroupsOlderThanOrderingId
    listGroupsOlderThanOrderingId_nextToken,
    listGroupsOlderThanOrderingId_dataSourceId,
    listGroupsOlderThanOrderingId_maxResults,
    listGroupsOlderThanOrderingId_indexId,
    listGroupsOlderThanOrderingId_orderingId,
    listGroupsOlderThanOrderingIdResponse_nextToken,
    listGroupsOlderThanOrderingIdResponse_groupsSummaries,
    listGroupsOlderThanOrderingIdResponse_httpStatus,

    -- ** ListIndices
    listIndices_nextToken,
    listIndices_maxResults,
    listIndicesResponse_nextToken,
    listIndicesResponse_indexConfigurationSummaryItems,
    listIndicesResponse_httpStatus,

    -- ** ListQuerySuggestionsBlockLists
    listQuerySuggestionsBlockLists_nextToken,
    listQuerySuggestionsBlockLists_maxResults,
    listQuerySuggestionsBlockLists_indexId,
    listQuerySuggestionsBlockListsResponse_nextToken,
    listQuerySuggestionsBlockListsResponse_blockListSummaryItems,
    listQuerySuggestionsBlockListsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListThesauri
    listThesauri_nextToken,
    listThesauri_maxResults,
    listThesauri_indexId,
    listThesauriResponse_nextToken,
    listThesauriResponse_thesaurusSummaryItems,
    listThesauriResponse_httpStatus,

    -- ** PutPrincipalMapping
    putPrincipalMapping_dataSourceId,
    putPrincipalMapping_roleArn,
    putPrincipalMapping_orderingId,
    putPrincipalMapping_indexId,
    putPrincipalMapping_groupId,
    putPrincipalMapping_groupMembers,

    -- ** Query
    query_documentRelevanceOverrideConfigurations,
    query_requestedDocumentAttributes,
    query_userContext,
    query_queryText,
    query_queryResultTypeFilter,
    query_facets,
    query_pageNumber,
    query_pageSize,
    query_spellCorrectionConfiguration,
    query_sortingConfiguration,
    query_visitorId,
    query_attributeFilter,
    query_indexId,
    queryResponse_spellCorrectedQueries,
    queryResponse_facetResults,
    queryResponse_queryId,
    queryResponse_resultItems,
    queryResponse_warnings,
    queryResponse_totalNumberOfResults,
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
    updateAccessControlConfiguration_name,
    updateAccessControlConfiguration_description,
    updateAccessControlConfiguration_hierarchicalAccessControlList,
    updateAccessControlConfiguration_indexId,
    updateAccessControlConfiguration_id,
    updateAccessControlConfigurationResponse_httpStatus,

    -- ** UpdateDataSource
    updateDataSource_schedule,
    updateDataSource_vpcConfiguration,
    updateDataSource_customDocumentEnrichmentConfiguration,
    updateDataSource_name,
    updateDataSource_roleArn,
    updateDataSource_configuration,
    updateDataSource_description,
    updateDataSource_languageCode,
    updateDataSource_id,
    updateDataSource_indexId,

    -- ** UpdateExperience
    updateExperience_name,
    updateExperience_roleArn,
    updateExperience_configuration,
    updateExperience_description,
    updateExperience_id,
    updateExperience_indexId,

    -- ** UpdateIndex
    updateIndex_name,
    updateIndex_roleArn,
    updateIndex_capacityUnits,
    updateIndex_userGroupResolutionConfiguration,
    updateIndex_documentMetadataConfigurationUpdates,
    updateIndex_description,
    updateIndex_userTokenConfigurations,
    updateIndex_userContextPolicy,
    updateIndex_id,

    -- ** UpdateQuerySuggestionsBlockList
    updateQuerySuggestionsBlockList_name,
    updateQuerySuggestionsBlockList_roleArn,
    updateQuerySuggestionsBlockList_description,
    updateQuerySuggestionsBlockList_sourceS3Path,
    updateQuerySuggestionsBlockList_indexId,
    updateQuerySuggestionsBlockList_id,

    -- ** UpdateQuerySuggestionsConfig
    updateQuerySuggestionsConfig_minimumNumberOfQueryingUsers,
    updateQuerySuggestionsConfig_queryLogLookBackWindowInDays,
    updateQuerySuggestionsConfig_mode,
    updateQuerySuggestionsConfig_minimumQueryCount,
    updateQuerySuggestionsConfig_includeQueriesWithoutUserInformation,
    updateQuerySuggestionsConfig_indexId,

    -- ** UpdateThesaurus
    updateThesaurus_name,
    updateThesaurus_roleArn,
    updateThesaurus_description,
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

    -- ** AttributeFilter
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

    -- ** AuthenticationConfiguration
    authenticationConfiguration_basicAuthentication,

    -- ** BasicAuthenticationConfiguration
    basicAuthenticationConfiguration_host,
    basicAuthenticationConfiguration_port,
    basicAuthenticationConfiguration_credentials,

    -- ** BatchDeleteDocumentResponseFailedDocument
    batchDeleteDocumentResponseFailedDocument_errorMessage,
    batchDeleteDocumentResponseFailedDocument_id,
    batchDeleteDocumentResponseFailedDocument_errorCode,

    -- ** BatchGetDocumentStatusResponseError
    batchGetDocumentStatusResponseError_errorMessage,
    batchGetDocumentStatusResponseError_errorCode,
    batchGetDocumentStatusResponseError_documentId,

    -- ** BatchPutDocumentResponseFailedDocument
    batchPutDocumentResponseFailedDocument_errorMessage,
    batchPutDocumentResponseFailedDocument_id,
    batchPutDocumentResponseFailedDocument_errorCode,

    -- ** BoxConfiguration
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
    confluenceAttachmentConfiguration_attachmentFieldMappings,
    confluenceAttachmentConfiguration_crawlAttachments,

    -- ** ConfluenceAttachmentToIndexFieldMapping
    confluenceAttachmentToIndexFieldMapping_dataSourceFieldName,
    confluenceAttachmentToIndexFieldMapping_indexFieldName,
    confluenceAttachmentToIndexFieldMapping_dateFieldFormat,

    -- ** ConfluenceBlogConfiguration
    confluenceBlogConfiguration_blogFieldMappings,

    -- ** ConfluenceBlogToIndexFieldMapping
    confluenceBlogToIndexFieldMapping_dataSourceFieldName,
    confluenceBlogToIndexFieldMapping_indexFieldName,
    confluenceBlogToIndexFieldMapping_dateFieldFormat,

    -- ** ConfluenceConfiguration
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

    -- ** ConfluencePageConfiguration
    confluencePageConfiguration_pageFieldMappings,

    -- ** ConfluencePageToIndexFieldMapping
    confluencePageToIndexFieldMapping_dataSourceFieldName,
    confluencePageToIndexFieldMapping_indexFieldName,
    confluencePageToIndexFieldMapping_dateFieldFormat,

    -- ** ConfluenceSpaceConfiguration
    confluenceSpaceConfiguration_crawlPersonalSpaces,
    confluenceSpaceConfiguration_crawlArchivedSpaces,
    confluenceSpaceConfiguration_excludeSpaces,
    confluenceSpaceConfiguration_spaceFieldMappings,
    confluenceSpaceConfiguration_includeSpaces,

    -- ** ConfluenceSpaceToIndexFieldMapping
    confluenceSpaceToIndexFieldMapping_dataSourceFieldName,
    confluenceSpaceToIndexFieldMapping_indexFieldName,
    confluenceSpaceToIndexFieldMapping_dateFieldFormat,

    -- ** ConnectionConfiguration
    connectionConfiguration_databaseHost,
    connectionConfiguration_databasePort,
    connectionConfiguration_databaseName,
    connectionConfiguration_tableName,
    connectionConfiguration_secretArn,

    -- ** ContentSourceConfiguration
    contentSourceConfiguration_directPutContent,
    contentSourceConfiguration_faqIds,
    contentSourceConfiguration_dataSourceIds,

    -- ** Correction
    correction_beginOffset,
    correction_endOffset,
    correction_term,
    correction_correctedTerm,

    -- ** CustomDocumentEnrichmentConfiguration
    customDocumentEnrichmentConfiguration_roleArn,
    customDocumentEnrichmentConfiguration_inlineConfigurations,
    customDocumentEnrichmentConfiguration_postExtractionHookConfiguration,
    customDocumentEnrichmentConfiguration_preExtractionHookConfiguration,

    -- ** DataSourceConfiguration
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

    -- ** DataSourceGroup
    dataSourceGroup_groupId,
    dataSourceGroup_dataSourceId,

    -- ** DataSourceSummary
    dataSourceSummary_name,
    dataSourceSummary_type,
    dataSourceSummary_status,
    dataSourceSummary_id,
    dataSourceSummary_languageCode,
    dataSourceSummary_createdAt,
    dataSourceSummary_updatedAt,

    -- ** DataSourceSyncJob
    dataSourceSyncJob_dataSourceErrorCode,
    dataSourceSyncJob_errorMessage,
    dataSourceSyncJob_status,
    dataSourceSyncJob_metrics,
    dataSourceSyncJob_endTime,
    dataSourceSyncJob_executionId,
    dataSourceSyncJob_errorCode,
    dataSourceSyncJob_startTime,

    -- ** DataSourceSyncJobMetricTarget
    dataSourceSyncJobMetricTarget_dataSourceSyncJobId,
    dataSourceSyncJobMetricTarget_dataSourceId,

    -- ** DataSourceSyncJobMetrics
    dataSourceSyncJobMetrics_documentsScanned,
    dataSourceSyncJobMetrics_documentsAdded,
    dataSourceSyncJobMetrics_documentsModified,
    dataSourceSyncJobMetrics_documentsFailed,
    dataSourceSyncJobMetrics_documentsDeleted,

    -- ** DataSourceToIndexFieldMapping
    dataSourceToIndexFieldMapping_dateFieldFormat,
    dataSourceToIndexFieldMapping_dataSourceFieldName,
    dataSourceToIndexFieldMapping_indexFieldName,

    -- ** DataSourceVpcConfiguration
    dataSourceVpcConfiguration_subnetIds,
    dataSourceVpcConfiguration_securityGroupIds,

    -- ** DatabaseConfiguration
    databaseConfiguration_vpcConfiguration,
    databaseConfiguration_aclConfiguration,
    databaseConfiguration_sqlConfiguration,
    databaseConfiguration_databaseEngineType,
    databaseConfiguration_connectionConfiguration,
    databaseConfiguration_columnConfiguration,

    -- ** Document
    document_accessControlList,
    document_blob,
    document_title,
    document_s3Path,
    document_attributes,
    document_hierarchicalAccessControlList,
    document_accessControlConfigurationId,
    document_contentType,
    document_id,

    -- ** DocumentAttribute
    documentAttribute_key,
    documentAttribute_value,

    -- ** DocumentAttributeCondition
    documentAttributeCondition_conditionOnValue,
    documentAttributeCondition_conditionDocumentAttributeKey,
    documentAttributeCondition_operator,

    -- ** DocumentAttributeTarget
    documentAttributeTarget_targetDocumentAttributeValue,
    documentAttributeTarget_targetDocumentAttributeKey,
    documentAttributeTarget_targetDocumentAttributeValueDeletion,

    -- ** DocumentAttributeValue
    documentAttributeValue_stringValue,
    documentAttributeValue_dateValue,
    documentAttributeValue_longValue,
    documentAttributeValue_stringListValue,

    -- ** DocumentAttributeValueCountPair
    documentAttributeValueCountPair_facetResults,
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

    -- ** EntityConfiguration
    entityConfiguration_entityId,
    entityConfiguration_entityType,

    -- ** EntityDisplayData
    entityDisplayData_identifiedUserName,
    entityDisplayData_firstName,
    entityDisplayData_userName,
    entityDisplayData_groupName,
    entityDisplayData_lastName,

    -- ** EntityPersonaConfiguration
    entityPersonaConfiguration_entityId,
    entityPersonaConfiguration_persona,

    -- ** ExperienceConfiguration
    experienceConfiguration_contentSourceConfiguration,
    experienceConfiguration_userIdentityConfiguration,

    -- ** ExperienceEndpoint
    experienceEndpoint_endpointType,
    experienceEndpoint_endpoint,

    -- ** ExperienceEntitiesSummary
    experienceEntitiesSummary_entityId,
    experienceEntitiesSummary_displayData,
    experienceEntitiesSummary_entityType,

    -- ** ExperiencesSummary
    experiencesSummary_name,
    experiencesSummary_status,
    experiencesSummary_endpoints,
    experiencesSummary_id,
    experiencesSummary_createdAt,

    -- ** Facet
    facet_facets,
    facet_maxResults,
    facet_documentAttributeKey,

    -- ** FacetResult
    facetResult_documentAttributeValueCountPairs,
    facetResult_documentAttributeValueType,
    facetResult_documentAttributeKey,

    -- ** FailedEntity
    failedEntity_entityId,
    failedEntity_errorMessage,

    -- ** FaqStatistics
    faqStatistics_indexedQuestionAnswersCount,

    -- ** FaqSummary
    faqSummary_name,
    faqSummary_status,
    faqSummary_id,
    faqSummary_languageCode,
    faqSummary_createdAt,
    faqSummary_updatedAt,
    faqSummary_fileFormat,

    -- ** FsxConfiguration
    fsxConfiguration_inclusionPatterns,
    fsxConfiguration_fieldMappings,
    fsxConfiguration_secretArn,
    fsxConfiguration_exclusionPatterns,
    fsxConfiguration_fileSystemId,
    fsxConfiguration_fileSystemType,
    fsxConfiguration_vpcConfiguration,

    -- ** GitHubConfiguration
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

    -- ** GitHubDocumentCrawlProperties
    gitHubDocumentCrawlProperties_crawlPullRequestComment,
    gitHubDocumentCrawlProperties_crawlRepositoryDocuments,
    gitHubDocumentCrawlProperties_crawlPullRequestCommentAttachment,
    gitHubDocumentCrawlProperties_crawlPullRequest,
    gitHubDocumentCrawlProperties_crawlIssueComment,
    gitHubDocumentCrawlProperties_crawlIssueCommentAttachment,
    gitHubDocumentCrawlProperties_crawlIssue,

    -- ** GoogleDriveConfiguration
    googleDriveConfiguration_inclusionPatterns,
    googleDriveConfiguration_fieldMappings,
    googleDriveConfiguration_excludeSharedDrives,
    googleDriveConfiguration_excludeUserAccounts,
    googleDriveConfiguration_excludeMimeTypes,
    googleDriveConfiguration_exclusionPatterns,
    googleDriveConfiguration_secretArn,

    -- ** GroupMembers
    groupMembers_memberUsers,
    groupMembers_memberGroups,
    groupMembers_s3PathforGroupMembers,

    -- ** GroupOrderingIdSummary
    groupOrderingIdSummary_lastUpdatedAt,
    groupOrderingIdSummary_status,
    groupOrderingIdSummary_receivedAt,
    groupOrderingIdSummary_orderingId,
    groupOrderingIdSummary_failureReason,

    -- ** GroupSummary
    groupSummary_orderingId,
    groupSummary_groupId,

    -- ** HierarchicalPrincipal
    hierarchicalPrincipal_principalList,

    -- ** Highlight
    highlight_type,
    highlight_topAnswer,
    highlight_beginOffset,
    highlight_endOffset,

    -- ** HookConfiguration
    hookConfiguration_invocationCondition,
    hookConfiguration_lambdaArn,
    hookConfiguration_s3Bucket,

    -- ** IndexConfigurationSummary
    indexConfigurationSummary_name,
    indexConfigurationSummary_edition,
    indexConfigurationSummary_id,
    indexConfigurationSummary_createdAt,
    indexConfigurationSummary_updatedAt,
    indexConfigurationSummary_status,

    -- ** IndexStatistics
    indexStatistics_faqStatistics,
    indexStatistics_textDocumentStatistics,

    -- ** InlineCustomDocumentEnrichmentConfiguration
    inlineCustomDocumentEnrichmentConfiguration_target,
    inlineCustomDocumentEnrichmentConfiguration_condition,
    inlineCustomDocumentEnrichmentConfiguration_documentContentDeletion,

    -- ** JiraConfiguration
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

    -- ** JsonTokenTypeConfiguration
    jsonTokenTypeConfiguration_userNameAttributeField,
    jsonTokenTypeConfiguration_groupAttributeField,

    -- ** JwtTokenTypeConfiguration
    jwtTokenTypeConfiguration_issuer,
    jwtTokenTypeConfiguration_userNameAttributeField,
    jwtTokenTypeConfiguration_url,
    jwtTokenTypeConfiguration_groupAttributeField,
    jwtTokenTypeConfiguration_secretManagerArn,
    jwtTokenTypeConfiguration_claimRegex,
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
    oneDriveConfiguration_inclusionPatterns,
    oneDriveConfiguration_fieldMappings,
    oneDriveConfiguration_disableLocalGroups,
    oneDriveConfiguration_exclusionPatterns,
    oneDriveConfiguration_tenantDomain,
    oneDriveConfiguration_secretArn,
    oneDriveConfiguration_oneDriveUsers,

    -- ** OneDriveUsers
    oneDriveUsers_oneDriveUserList,
    oneDriveUsers_oneDriveUserS3Path,

    -- ** PersonasSummary
    personasSummary_entityId,
    personasSummary_persona,
    personasSummary_createdAt,
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

    -- ** QuerySuggestionsBlockListSummary
    querySuggestionsBlockListSummary_name,
    querySuggestionsBlockListSummary_itemCount,
    querySuggestionsBlockListSummary_status,
    querySuggestionsBlockListSummary_id,
    querySuggestionsBlockListSummary_createdAt,
    querySuggestionsBlockListSummary_updatedAt,

    -- ** QuipConfiguration
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

    -- ** Relevance
    relevance_importance,
    relevance_freshness,
    relevance_duration,
    relevance_rankOrder,
    relevance_valueImportanceMap,

    -- ** RelevanceFeedback
    relevanceFeedback_resultId,
    relevanceFeedback_relevanceValue,

    -- ** S3DataSourceConfiguration
    s3DataSourceConfiguration_inclusionPatterns,
    s3DataSourceConfiguration_documentsMetadataConfiguration,
    s3DataSourceConfiguration_accessControlListConfiguration,
    s3DataSourceConfiguration_inclusionPrefixes,
    s3DataSourceConfiguration_exclusionPatterns,
    s3DataSourceConfiguration_bucketName,

    -- ** S3Path
    s3Path_bucket,
    s3Path_key,

    -- ** SaaSConfiguration
    saaSConfiguration_organizationName,
    saaSConfiguration_hostUrl,

    -- ** SalesforceChatterFeedConfiguration
    salesforceChatterFeedConfiguration_includeFilterTypes,
    salesforceChatterFeedConfiguration_fieldMappings,
    salesforceChatterFeedConfiguration_documentTitleFieldName,
    salesforceChatterFeedConfiguration_documentDataFieldName,

    -- ** SalesforceConfiguration
    salesforceConfiguration_includeAttachmentFilePatterns,
    salesforceConfiguration_crawlAttachments,
    salesforceConfiguration_excludeAttachmentFilePatterns,
    salesforceConfiguration_standardObjectAttachmentConfiguration,
    salesforceConfiguration_chatterFeedConfiguration,
    salesforceConfiguration_knowledgeArticleConfiguration,
    salesforceConfiguration_standardObjectConfigurations,
    salesforceConfiguration_serverUrl,
    salesforceConfiguration_secretArn,

    -- ** SalesforceCustomKnowledgeArticleTypeConfiguration
    salesforceCustomKnowledgeArticleTypeConfiguration_fieldMappings,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentTitleFieldName,
    salesforceCustomKnowledgeArticleTypeConfiguration_name,
    salesforceCustomKnowledgeArticleTypeConfiguration_documentDataFieldName,

    -- ** SalesforceKnowledgeArticleConfiguration
    salesforceKnowledgeArticleConfiguration_standardKnowledgeArticleTypeConfiguration,
    salesforceKnowledgeArticleConfiguration_customKnowledgeArticleTypeConfigurations,
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
    search_displayable,
    search_sortable,
    search_searchable,
    search_facetable,

    -- ** SeedUrlConfiguration
    seedUrlConfiguration_webCrawlerMode,
    seedUrlConfiguration_seedUrls,

    -- ** ServerSideEncryptionConfiguration
    serverSideEncryptionConfiguration_kmsKeyId,

    -- ** ServiceNowConfiguration
    serviceNowConfiguration_authenticationType,
    serviceNowConfiguration_serviceCatalogConfiguration,
    serviceNowConfiguration_knowledgeArticleConfiguration,
    serviceNowConfiguration_hostUrl,
    serviceNowConfiguration_secretArn,
    serviceNowConfiguration_serviceNowBuildVersion,

    -- ** ServiceNowKnowledgeArticleConfiguration
    serviceNowKnowledgeArticleConfiguration_filterQuery,
    serviceNowKnowledgeArticleConfiguration_includeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_crawlAttachments,
    serviceNowKnowledgeArticleConfiguration_excludeAttachmentFilePatterns,
    serviceNowKnowledgeArticleConfiguration_fieldMappings,
    serviceNowKnowledgeArticleConfiguration_documentTitleFieldName,
    serviceNowKnowledgeArticleConfiguration_documentDataFieldName,

    -- ** ServiceNowServiceCatalogConfiguration
    serviceNowServiceCatalogConfiguration_includeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_crawlAttachments,
    serviceNowServiceCatalogConfiguration_excludeAttachmentFilePatterns,
    serviceNowServiceCatalogConfiguration_fieldMappings,
    serviceNowServiceCatalogConfiguration_documentTitleFieldName,
    serviceNowServiceCatalogConfiguration_documentDataFieldName,

    -- ** SharePointConfiguration
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

    -- ** SiteMapsConfiguration
    siteMapsConfiguration_siteMaps,

    -- ** SlackConfiguration
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

    -- ** SortingConfiguration
    sortingConfiguration_documentAttributeKey,
    sortingConfiguration_sortOrder,

    -- ** SpellCorrectedQuery
    spellCorrectedQuery_suggestedQueryText,
    spellCorrectedQuery_corrections,

    -- ** SpellCorrectionConfiguration
    spellCorrectionConfiguration_includeQuerySpellCheckSuggestions,

    -- ** SqlConfiguration
    sqlConfiguration_queryIdentifiersEnclosingOption,

    -- ** Status
    status_failureCode,
    status_documentId,
    status_documentStatus,
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
    thesaurusSummary_name,
    thesaurusSummary_status,
    thesaurusSummary_id,
    thesaurusSummary_createdAt,
    thesaurusSummary_updatedAt,

    -- ** TimeRange
    timeRange_endTime,
    timeRange_startTime,

    -- ** Urls
    urls_seedUrlConfiguration,
    urls_siteMapsConfiguration,

    -- ** UserContext
    userContext_dataSourceGroups,
    userContext_userId,
    userContext_groups,
    userContext_token,

    -- ** UserGroupResolutionConfiguration
    userGroupResolutionConfiguration_userGroupResolutionMode,

    -- ** UserIdentityConfiguration
    userIdentityConfiguration_identityAttributeName,

    -- ** UserTokenConfiguration
    userTokenConfiguration_jwtTokenTypeConfiguration,
    userTokenConfiguration_jsonTokenTypeConfiguration,

    -- ** Warning
    warning_message,
    warning_code,

    -- ** WebCrawlerConfiguration
    webCrawlerConfiguration_proxyConfiguration,
    webCrawlerConfiguration_maxContentSizePerPageInMegaBytes,
    webCrawlerConfiguration_maxUrlsPerMinuteCrawlRate,
    webCrawlerConfiguration_urlExclusionPatterns,
    webCrawlerConfiguration_maxLinksPerPage,
    webCrawlerConfiguration_crawlDepth,
    webCrawlerConfiguration_authenticationConfiguration,
    webCrawlerConfiguration_urlInclusionPatterns,
    webCrawlerConfiguration_urls,

    -- ** WorkDocsConfiguration
    workDocsConfiguration_useChangeLog,
    workDocsConfiguration_inclusionPatterns,
    workDocsConfiguration_fieldMappings,
    workDocsConfiguration_crawlComments,
    workDocsConfiguration_exclusionPatterns,
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
