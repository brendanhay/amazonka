{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Kendra
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-02-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Kendra is a service for indexing large document sets.
module Amazonka.Kendra
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** ResourceAlreadyExistException
    _ResourceAlreadyExistException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateEntitiesToExperience
    AssociateEntitiesToExperience (AssociateEntitiesToExperience'),
    newAssociateEntitiesToExperience,
    AssociateEntitiesToExperienceResponse (AssociateEntitiesToExperienceResponse'),
    newAssociateEntitiesToExperienceResponse,

    -- ** AssociatePersonasToEntities
    AssociatePersonasToEntities (AssociatePersonasToEntities'),
    newAssociatePersonasToEntities,
    AssociatePersonasToEntitiesResponse (AssociatePersonasToEntitiesResponse'),
    newAssociatePersonasToEntitiesResponse,

    -- ** BatchDeleteDocument
    BatchDeleteDocument (BatchDeleteDocument'),
    newBatchDeleteDocument,
    BatchDeleteDocumentResponse (BatchDeleteDocumentResponse'),
    newBatchDeleteDocumentResponse,

    -- ** BatchGetDocumentStatus
    BatchGetDocumentStatus (BatchGetDocumentStatus'),
    newBatchGetDocumentStatus,
    BatchGetDocumentStatusResponse (BatchGetDocumentStatusResponse'),
    newBatchGetDocumentStatusResponse,

    -- ** BatchPutDocument
    BatchPutDocument (BatchPutDocument'),
    newBatchPutDocument,
    BatchPutDocumentResponse (BatchPutDocumentResponse'),
    newBatchPutDocumentResponse,

    -- ** ClearQuerySuggestions
    ClearQuerySuggestions (ClearQuerySuggestions'),
    newClearQuerySuggestions,
    ClearQuerySuggestionsResponse (ClearQuerySuggestionsResponse'),
    newClearQuerySuggestionsResponse,

    -- ** CreateAccessControlConfiguration
    CreateAccessControlConfiguration (CreateAccessControlConfiguration'),
    newCreateAccessControlConfiguration,
    CreateAccessControlConfigurationResponse (CreateAccessControlConfigurationResponse'),
    newCreateAccessControlConfigurationResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** CreateExperience
    CreateExperience (CreateExperience'),
    newCreateExperience,
    CreateExperienceResponse (CreateExperienceResponse'),
    newCreateExperienceResponse,

    -- ** CreateFaq
    CreateFaq (CreateFaq'),
    newCreateFaq,
    CreateFaqResponse (CreateFaqResponse'),
    newCreateFaqResponse,

    -- ** CreateIndex
    CreateIndex (CreateIndex'),
    newCreateIndex,
    CreateIndexResponse (CreateIndexResponse'),
    newCreateIndexResponse,

    -- ** CreateQuerySuggestionsBlockList
    CreateQuerySuggestionsBlockList (CreateQuerySuggestionsBlockList'),
    newCreateQuerySuggestionsBlockList,
    CreateQuerySuggestionsBlockListResponse (CreateQuerySuggestionsBlockListResponse'),
    newCreateQuerySuggestionsBlockListResponse,

    -- ** CreateThesaurus
    CreateThesaurus (CreateThesaurus'),
    newCreateThesaurus,
    CreateThesaurusResponse (CreateThesaurusResponse'),
    newCreateThesaurusResponse,

    -- ** DeleteAccessControlConfiguration
    DeleteAccessControlConfiguration (DeleteAccessControlConfiguration'),
    newDeleteAccessControlConfiguration,
    DeleteAccessControlConfigurationResponse (DeleteAccessControlConfigurationResponse'),
    newDeleteAccessControlConfigurationResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** DeleteExperience
    DeleteExperience (DeleteExperience'),
    newDeleteExperience,
    DeleteExperienceResponse (DeleteExperienceResponse'),
    newDeleteExperienceResponse,

    -- ** DeleteFaq
    DeleteFaq (DeleteFaq'),
    newDeleteFaq,
    DeleteFaqResponse (DeleteFaqResponse'),
    newDeleteFaqResponse,

    -- ** DeleteIndex
    DeleteIndex (DeleteIndex'),
    newDeleteIndex,
    DeleteIndexResponse (DeleteIndexResponse'),
    newDeleteIndexResponse,

    -- ** DeletePrincipalMapping
    DeletePrincipalMapping (DeletePrincipalMapping'),
    newDeletePrincipalMapping,
    DeletePrincipalMappingResponse (DeletePrincipalMappingResponse'),
    newDeletePrincipalMappingResponse,

    -- ** DeleteQuerySuggestionsBlockList
    DeleteQuerySuggestionsBlockList (DeleteQuerySuggestionsBlockList'),
    newDeleteQuerySuggestionsBlockList,
    DeleteQuerySuggestionsBlockListResponse (DeleteQuerySuggestionsBlockListResponse'),
    newDeleteQuerySuggestionsBlockListResponse,

    -- ** DeleteThesaurus
    DeleteThesaurus (DeleteThesaurus'),
    newDeleteThesaurus,
    DeleteThesaurusResponse (DeleteThesaurusResponse'),
    newDeleteThesaurusResponse,

    -- ** DescribeAccessControlConfiguration
    DescribeAccessControlConfiguration (DescribeAccessControlConfiguration'),
    newDescribeAccessControlConfiguration,
    DescribeAccessControlConfigurationResponse (DescribeAccessControlConfigurationResponse'),
    newDescribeAccessControlConfigurationResponse,

    -- ** DescribeDataSource
    DescribeDataSource (DescribeDataSource'),
    newDescribeDataSource,
    DescribeDataSourceResponse (DescribeDataSourceResponse'),
    newDescribeDataSourceResponse,

    -- ** DescribeExperience
    DescribeExperience (DescribeExperience'),
    newDescribeExperience,
    DescribeExperienceResponse (DescribeExperienceResponse'),
    newDescribeExperienceResponse,

    -- ** DescribeFaq
    DescribeFaq (DescribeFaq'),
    newDescribeFaq,
    DescribeFaqResponse (DescribeFaqResponse'),
    newDescribeFaqResponse,

    -- ** DescribeIndex
    DescribeIndex (DescribeIndex'),
    newDescribeIndex,
    DescribeIndexResponse (DescribeIndexResponse'),
    newDescribeIndexResponse,

    -- ** DescribePrincipalMapping
    DescribePrincipalMapping (DescribePrincipalMapping'),
    newDescribePrincipalMapping,
    DescribePrincipalMappingResponse (DescribePrincipalMappingResponse'),
    newDescribePrincipalMappingResponse,

    -- ** DescribeQuerySuggestionsBlockList
    DescribeQuerySuggestionsBlockList (DescribeQuerySuggestionsBlockList'),
    newDescribeQuerySuggestionsBlockList,
    DescribeQuerySuggestionsBlockListResponse (DescribeQuerySuggestionsBlockListResponse'),
    newDescribeQuerySuggestionsBlockListResponse,

    -- ** DescribeQuerySuggestionsConfig
    DescribeQuerySuggestionsConfig (DescribeQuerySuggestionsConfig'),
    newDescribeQuerySuggestionsConfig,
    DescribeQuerySuggestionsConfigResponse (DescribeQuerySuggestionsConfigResponse'),
    newDescribeQuerySuggestionsConfigResponse,

    -- ** DescribeThesaurus
    DescribeThesaurus (DescribeThesaurus'),
    newDescribeThesaurus,
    DescribeThesaurusResponse (DescribeThesaurusResponse'),
    newDescribeThesaurusResponse,

    -- ** DisassociateEntitiesFromExperience
    DisassociateEntitiesFromExperience (DisassociateEntitiesFromExperience'),
    newDisassociateEntitiesFromExperience,
    DisassociateEntitiesFromExperienceResponse (DisassociateEntitiesFromExperienceResponse'),
    newDisassociateEntitiesFromExperienceResponse,

    -- ** DisassociatePersonasFromEntities
    DisassociatePersonasFromEntities (DisassociatePersonasFromEntities'),
    newDisassociatePersonasFromEntities,
    DisassociatePersonasFromEntitiesResponse (DisassociatePersonasFromEntitiesResponse'),
    newDisassociatePersonasFromEntitiesResponse,

    -- ** GetQuerySuggestions
    GetQuerySuggestions (GetQuerySuggestions'),
    newGetQuerySuggestions,
    GetQuerySuggestionsResponse (GetQuerySuggestionsResponse'),
    newGetQuerySuggestionsResponse,

    -- ** GetSnapshots
    GetSnapshots (GetSnapshots'),
    newGetSnapshots,
    GetSnapshotsResponse (GetSnapshotsResponse'),
    newGetSnapshotsResponse,

    -- ** ListAccessControlConfigurations
    ListAccessControlConfigurations (ListAccessControlConfigurations'),
    newListAccessControlConfigurations,
    ListAccessControlConfigurationsResponse (ListAccessControlConfigurationsResponse'),
    newListAccessControlConfigurationsResponse,

    -- ** ListDataSourceSyncJobs
    ListDataSourceSyncJobs (ListDataSourceSyncJobs'),
    newListDataSourceSyncJobs,
    ListDataSourceSyncJobsResponse (ListDataSourceSyncJobsResponse'),
    newListDataSourceSyncJobsResponse,

    -- ** ListDataSources
    ListDataSources (ListDataSources'),
    newListDataSources,
    ListDataSourcesResponse (ListDataSourcesResponse'),
    newListDataSourcesResponse,

    -- ** ListEntityPersonas
    ListEntityPersonas (ListEntityPersonas'),
    newListEntityPersonas,
    ListEntityPersonasResponse (ListEntityPersonasResponse'),
    newListEntityPersonasResponse,

    -- ** ListExperienceEntities
    ListExperienceEntities (ListExperienceEntities'),
    newListExperienceEntities,
    ListExperienceEntitiesResponse (ListExperienceEntitiesResponse'),
    newListExperienceEntitiesResponse,

    -- ** ListExperiences
    ListExperiences (ListExperiences'),
    newListExperiences,
    ListExperiencesResponse (ListExperiencesResponse'),
    newListExperiencesResponse,

    -- ** ListFaqs
    ListFaqs (ListFaqs'),
    newListFaqs,
    ListFaqsResponse (ListFaqsResponse'),
    newListFaqsResponse,

    -- ** ListGroupsOlderThanOrderingId
    ListGroupsOlderThanOrderingId (ListGroupsOlderThanOrderingId'),
    newListGroupsOlderThanOrderingId,
    ListGroupsOlderThanOrderingIdResponse (ListGroupsOlderThanOrderingIdResponse'),
    newListGroupsOlderThanOrderingIdResponse,

    -- ** ListIndices
    ListIndices (ListIndices'),
    newListIndices,
    ListIndicesResponse (ListIndicesResponse'),
    newListIndicesResponse,

    -- ** ListQuerySuggestionsBlockLists
    ListQuerySuggestionsBlockLists (ListQuerySuggestionsBlockLists'),
    newListQuerySuggestionsBlockLists,
    ListQuerySuggestionsBlockListsResponse (ListQuerySuggestionsBlockListsResponse'),
    newListQuerySuggestionsBlockListsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListThesauri
    ListThesauri (ListThesauri'),
    newListThesauri,
    ListThesauriResponse (ListThesauriResponse'),
    newListThesauriResponse,

    -- ** PutPrincipalMapping
    PutPrincipalMapping (PutPrincipalMapping'),
    newPutPrincipalMapping,
    PutPrincipalMappingResponse (PutPrincipalMappingResponse'),
    newPutPrincipalMappingResponse,

    -- ** Query
    Query (Query'),
    newQuery,
    QueryResponse (QueryResponse'),
    newQueryResponse,

    -- ** StartDataSourceSyncJob
    StartDataSourceSyncJob (StartDataSourceSyncJob'),
    newStartDataSourceSyncJob,
    StartDataSourceSyncJobResponse (StartDataSourceSyncJobResponse'),
    newStartDataSourceSyncJobResponse,

    -- ** StopDataSourceSyncJob
    StopDataSourceSyncJob (StopDataSourceSyncJob'),
    newStopDataSourceSyncJob,
    StopDataSourceSyncJobResponse (StopDataSourceSyncJobResponse'),
    newStopDataSourceSyncJobResponse,

    -- ** SubmitFeedback
    SubmitFeedback (SubmitFeedback'),
    newSubmitFeedback,
    SubmitFeedbackResponse (SubmitFeedbackResponse'),
    newSubmitFeedbackResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateAccessControlConfiguration
    UpdateAccessControlConfiguration (UpdateAccessControlConfiguration'),
    newUpdateAccessControlConfiguration,
    UpdateAccessControlConfigurationResponse (UpdateAccessControlConfigurationResponse'),
    newUpdateAccessControlConfigurationResponse,

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** UpdateExperience
    UpdateExperience (UpdateExperience'),
    newUpdateExperience,
    UpdateExperienceResponse (UpdateExperienceResponse'),
    newUpdateExperienceResponse,

    -- ** UpdateIndex
    UpdateIndex (UpdateIndex'),
    newUpdateIndex,
    UpdateIndexResponse (UpdateIndexResponse'),
    newUpdateIndexResponse,

    -- ** UpdateQuerySuggestionsBlockList
    UpdateQuerySuggestionsBlockList (UpdateQuerySuggestionsBlockList'),
    newUpdateQuerySuggestionsBlockList,
    UpdateQuerySuggestionsBlockListResponse (UpdateQuerySuggestionsBlockListResponse'),
    newUpdateQuerySuggestionsBlockListResponse,

    -- ** UpdateQuerySuggestionsConfig
    UpdateQuerySuggestionsConfig (UpdateQuerySuggestionsConfig'),
    newUpdateQuerySuggestionsConfig,
    UpdateQuerySuggestionsConfigResponse (UpdateQuerySuggestionsConfigResponse'),
    newUpdateQuerySuggestionsConfigResponse,

    -- ** UpdateThesaurus
    UpdateThesaurus (UpdateThesaurus'),
    newUpdateThesaurus,
    UpdateThesaurusResponse (UpdateThesaurusResponse'),
    newUpdateThesaurusResponse,

    -- * Types

    -- ** AdditionalResultAttributeValueType
    AdditionalResultAttributeValueType (..),

    -- ** AlfrescoEntity
    AlfrescoEntity (..),

    -- ** ConditionOperator
    ConditionOperator (..),

    -- ** ConfluenceAttachmentFieldName
    ConfluenceAttachmentFieldName (..),

    -- ** ConfluenceAuthenticationType
    ConfluenceAuthenticationType (..),

    -- ** ConfluenceBlogFieldName
    ConfluenceBlogFieldName (..),

    -- ** ConfluencePageFieldName
    ConfluencePageFieldName (..),

    -- ** ConfluenceSpaceFieldName
    ConfluenceSpaceFieldName (..),

    -- ** ConfluenceVersion
    ConfluenceVersion (..),

    -- ** ContentType
    ContentType (..),

    -- ** DataSourceStatus
    DataSourceStatus (..),

    -- ** DataSourceSyncJobStatus
    DataSourceSyncJobStatus (..),

    -- ** DataSourceType
    DataSourceType (..),

    -- ** DatabaseEngineType
    DatabaseEngineType (..),

    -- ** DocumentAttributeValueType
    DocumentAttributeValueType (..),

    -- ** DocumentStatus
    DocumentStatus (..),

    -- ** EndpointType
    EndpointType (..),

    -- ** EntityType
    EntityType (..),

    -- ** ErrorCode
    ErrorCode (..),

    -- ** ExperienceStatus
    ExperienceStatus (..),

    -- ** FaqFileFormat
    FaqFileFormat (..),

    -- ** FaqStatus
    FaqStatus (..),

    -- ** FsxFileSystemType
    FsxFileSystemType (..),

    -- ** HighlightType
    HighlightType (..),

    -- ** IndexEdition
    IndexEdition (..),

    -- ** IndexStatus
    IndexStatus (..),

    -- ** Interval
    Interval (..),

    -- ** IssueSubEntity
    IssueSubEntity (..),

    -- ** KeyLocation
    KeyLocation (..),

    -- ** MetricType
    MetricType (..),

    -- ** Mode
    Mode (..),

    -- ** Order
    Order (..),

    -- ** Persona
    Persona (..),

    -- ** PrincipalMappingStatus
    PrincipalMappingStatus (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** QueryIdentifiersEnclosingOption
    QueryIdentifiersEnclosingOption (..),

    -- ** QueryResultFormat
    QueryResultFormat (..),

    -- ** QueryResultType
    QueryResultType (..),

    -- ** QuerySuggestionsBlockListStatus
    QuerySuggestionsBlockListStatus (..),

    -- ** QuerySuggestionsStatus
    QuerySuggestionsStatus (..),

    -- ** ReadAccessType
    ReadAccessType (..),

    -- ** RelevanceType
    RelevanceType (..),

    -- ** SalesforceChatterFeedIncludeFilterType
    SalesforceChatterFeedIncludeFilterType (..),

    -- ** SalesforceKnowledgeArticleState
    SalesforceKnowledgeArticleState (..),

    -- ** SalesforceStandardObjectName
    SalesforceStandardObjectName (..),

    -- ** ScoreConfidence
    ScoreConfidence (..),

    -- ** ServiceNowAuthenticationType
    ServiceNowAuthenticationType (..),

    -- ** ServiceNowBuildVersionType
    ServiceNowBuildVersionType (..),

    -- ** SharePointOnlineAuthenticationType
    SharePointOnlineAuthenticationType (..),

    -- ** SharePointVersion
    SharePointVersion (..),

    -- ** SlackEntity
    SlackEntity (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** ThesaurusStatus
    ThesaurusStatus (..),

    -- ** Type
    Type (..),

    -- ** UserContextPolicy
    UserContextPolicy (..),

    -- ** UserGroupResolutionMode
    UserGroupResolutionMode (..),

    -- ** WarningCode
    WarningCode (..),

    -- ** WebCrawlerMode
    WebCrawlerMode (..),

    -- ** AccessControlConfigurationSummary
    AccessControlConfigurationSummary (AccessControlConfigurationSummary'),
    newAccessControlConfigurationSummary,

    -- ** AccessControlListConfiguration
    AccessControlListConfiguration (AccessControlListConfiguration'),
    newAccessControlListConfiguration,

    -- ** AclConfiguration
    AclConfiguration (AclConfiguration'),
    newAclConfiguration,

    -- ** AdditionalResultAttribute
    AdditionalResultAttribute (AdditionalResultAttribute'),
    newAdditionalResultAttribute,

    -- ** AdditionalResultAttributeValue
    AdditionalResultAttributeValue (AdditionalResultAttributeValue'),
    newAdditionalResultAttributeValue,

    -- ** AlfrescoConfiguration
    AlfrescoConfiguration (AlfrescoConfiguration'),
    newAlfrescoConfiguration,

    -- ** AttributeFilter
    AttributeFilter (AttributeFilter'),
    newAttributeFilter,

    -- ** AuthenticationConfiguration
    AuthenticationConfiguration (AuthenticationConfiguration'),
    newAuthenticationConfiguration,

    -- ** BasicAuthenticationConfiguration
    BasicAuthenticationConfiguration (BasicAuthenticationConfiguration'),
    newBasicAuthenticationConfiguration,

    -- ** BatchDeleteDocumentResponseFailedDocument
    BatchDeleteDocumentResponseFailedDocument (BatchDeleteDocumentResponseFailedDocument'),
    newBatchDeleteDocumentResponseFailedDocument,

    -- ** BatchGetDocumentStatusResponseError
    BatchGetDocumentStatusResponseError (BatchGetDocumentStatusResponseError'),
    newBatchGetDocumentStatusResponseError,

    -- ** BatchPutDocumentResponseFailedDocument
    BatchPutDocumentResponseFailedDocument (BatchPutDocumentResponseFailedDocument'),
    newBatchPutDocumentResponseFailedDocument,

    -- ** BoxConfiguration
    BoxConfiguration (BoxConfiguration'),
    newBoxConfiguration,

    -- ** CapacityUnitsConfiguration
    CapacityUnitsConfiguration (CapacityUnitsConfiguration'),
    newCapacityUnitsConfiguration,

    -- ** ClickFeedback
    ClickFeedback (ClickFeedback'),
    newClickFeedback,

    -- ** ColumnConfiguration
    ColumnConfiguration (ColumnConfiguration'),
    newColumnConfiguration,

    -- ** ConfluenceAttachmentConfiguration
    ConfluenceAttachmentConfiguration (ConfluenceAttachmentConfiguration'),
    newConfluenceAttachmentConfiguration,

    -- ** ConfluenceAttachmentToIndexFieldMapping
    ConfluenceAttachmentToIndexFieldMapping (ConfluenceAttachmentToIndexFieldMapping'),
    newConfluenceAttachmentToIndexFieldMapping,

    -- ** ConfluenceBlogConfiguration
    ConfluenceBlogConfiguration (ConfluenceBlogConfiguration'),
    newConfluenceBlogConfiguration,

    -- ** ConfluenceBlogToIndexFieldMapping
    ConfluenceBlogToIndexFieldMapping (ConfluenceBlogToIndexFieldMapping'),
    newConfluenceBlogToIndexFieldMapping,

    -- ** ConfluenceConfiguration
    ConfluenceConfiguration (ConfluenceConfiguration'),
    newConfluenceConfiguration,

    -- ** ConfluencePageConfiguration
    ConfluencePageConfiguration (ConfluencePageConfiguration'),
    newConfluencePageConfiguration,

    -- ** ConfluencePageToIndexFieldMapping
    ConfluencePageToIndexFieldMapping (ConfluencePageToIndexFieldMapping'),
    newConfluencePageToIndexFieldMapping,

    -- ** ConfluenceSpaceConfiguration
    ConfluenceSpaceConfiguration (ConfluenceSpaceConfiguration'),
    newConfluenceSpaceConfiguration,

    -- ** ConfluenceSpaceToIndexFieldMapping
    ConfluenceSpaceToIndexFieldMapping (ConfluenceSpaceToIndexFieldMapping'),
    newConfluenceSpaceToIndexFieldMapping,

    -- ** ConnectionConfiguration
    ConnectionConfiguration (ConnectionConfiguration'),
    newConnectionConfiguration,

    -- ** ContentSourceConfiguration
    ContentSourceConfiguration (ContentSourceConfiguration'),
    newContentSourceConfiguration,

    -- ** Correction
    Correction (Correction'),
    newCorrection,

    -- ** CustomDocumentEnrichmentConfiguration
    CustomDocumentEnrichmentConfiguration (CustomDocumentEnrichmentConfiguration'),
    newCustomDocumentEnrichmentConfiguration,

    -- ** DataSourceConfiguration
    DataSourceConfiguration (DataSourceConfiguration'),
    newDataSourceConfiguration,

    -- ** DataSourceGroup
    DataSourceGroup (DataSourceGroup'),
    newDataSourceGroup,

    -- ** DataSourceSummary
    DataSourceSummary (DataSourceSummary'),
    newDataSourceSummary,

    -- ** DataSourceSyncJob
    DataSourceSyncJob (DataSourceSyncJob'),
    newDataSourceSyncJob,

    -- ** DataSourceSyncJobMetricTarget
    DataSourceSyncJobMetricTarget (DataSourceSyncJobMetricTarget'),
    newDataSourceSyncJobMetricTarget,

    -- ** DataSourceSyncJobMetrics
    DataSourceSyncJobMetrics (DataSourceSyncJobMetrics'),
    newDataSourceSyncJobMetrics,

    -- ** DataSourceToIndexFieldMapping
    DataSourceToIndexFieldMapping (DataSourceToIndexFieldMapping'),
    newDataSourceToIndexFieldMapping,

    -- ** DataSourceVpcConfiguration
    DataSourceVpcConfiguration (DataSourceVpcConfiguration'),
    newDataSourceVpcConfiguration,

    -- ** DatabaseConfiguration
    DatabaseConfiguration (DatabaseConfiguration'),
    newDatabaseConfiguration,

    -- ** Document
    Document (Document'),
    newDocument,

    -- ** DocumentAttribute
    DocumentAttribute (DocumentAttribute'),
    newDocumentAttribute,

    -- ** DocumentAttributeCondition
    DocumentAttributeCondition (DocumentAttributeCondition'),
    newDocumentAttributeCondition,

    -- ** DocumentAttributeTarget
    DocumentAttributeTarget (DocumentAttributeTarget'),
    newDocumentAttributeTarget,

    -- ** DocumentAttributeValue
    DocumentAttributeValue (DocumentAttributeValue'),
    newDocumentAttributeValue,

    -- ** DocumentAttributeValueCountPair
    DocumentAttributeValueCountPair (DocumentAttributeValueCountPair'),
    newDocumentAttributeValueCountPair,

    -- ** DocumentInfo
    DocumentInfo (DocumentInfo'),
    newDocumentInfo,

    -- ** DocumentMetadataConfiguration
    DocumentMetadataConfiguration (DocumentMetadataConfiguration'),
    newDocumentMetadataConfiguration,

    -- ** DocumentRelevanceConfiguration
    DocumentRelevanceConfiguration (DocumentRelevanceConfiguration'),
    newDocumentRelevanceConfiguration,

    -- ** DocumentsMetadataConfiguration
    DocumentsMetadataConfiguration (DocumentsMetadataConfiguration'),
    newDocumentsMetadataConfiguration,

    -- ** EntityConfiguration
    EntityConfiguration (EntityConfiguration'),
    newEntityConfiguration,

    -- ** EntityDisplayData
    EntityDisplayData (EntityDisplayData'),
    newEntityDisplayData,

    -- ** EntityPersonaConfiguration
    EntityPersonaConfiguration (EntityPersonaConfiguration'),
    newEntityPersonaConfiguration,

    -- ** ExperienceConfiguration
    ExperienceConfiguration (ExperienceConfiguration'),
    newExperienceConfiguration,

    -- ** ExperienceEndpoint
    ExperienceEndpoint (ExperienceEndpoint'),
    newExperienceEndpoint,

    -- ** ExperienceEntitiesSummary
    ExperienceEntitiesSummary (ExperienceEntitiesSummary'),
    newExperienceEntitiesSummary,

    -- ** ExperiencesSummary
    ExperiencesSummary (ExperiencesSummary'),
    newExperiencesSummary,

    -- ** Facet
    Facet (Facet'),
    newFacet,

    -- ** FacetResult
    FacetResult (FacetResult'),
    newFacetResult,

    -- ** FailedEntity
    FailedEntity (FailedEntity'),
    newFailedEntity,

    -- ** FaqStatistics
    FaqStatistics (FaqStatistics'),
    newFaqStatistics,

    -- ** FaqSummary
    FaqSummary (FaqSummary'),
    newFaqSummary,

    -- ** FsxConfiguration
    FsxConfiguration (FsxConfiguration'),
    newFsxConfiguration,

    -- ** GitHubConfiguration
    GitHubConfiguration (GitHubConfiguration'),
    newGitHubConfiguration,

    -- ** GitHubDocumentCrawlProperties
    GitHubDocumentCrawlProperties (GitHubDocumentCrawlProperties'),
    newGitHubDocumentCrawlProperties,

    -- ** GoogleDriveConfiguration
    GoogleDriveConfiguration (GoogleDriveConfiguration'),
    newGoogleDriveConfiguration,

    -- ** GroupMembers
    GroupMembers (GroupMembers'),
    newGroupMembers,

    -- ** GroupOrderingIdSummary
    GroupOrderingIdSummary (GroupOrderingIdSummary'),
    newGroupOrderingIdSummary,

    -- ** GroupSummary
    GroupSummary (GroupSummary'),
    newGroupSummary,

    -- ** HierarchicalPrincipal
    HierarchicalPrincipal (HierarchicalPrincipal'),
    newHierarchicalPrincipal,

    -- ** Highlight
    Highlight (Highlight'),
    newHighlight,

    -- ** HookConfiguration
    HookConfiguration (HookConfiguration'),
    newHookConfiguration,

    -- ** IndexConfigurationSummary
    IndexConfigurationSummary (IndexConfigurationSummary'),
    newIndexConfigurationSummary,

    -- ** IndexStatistics
    IndexStatistics (IndexStatistics'),
    newIndexStatistics,

    -- ** InlineCustomDocumentEnrichmentConfiguration
    InlineCustomDocumentEnrichmentConfiguration (InlineCustomDocumentEnrichmentConfiguration'),
    newInlineCustomDocumentEnrichmentConfiguration,

    -- ** JiraConfiguration
    JiraConfiguration (JiraConfiguration'),
    newJiraConfiguration,

    -- ** JsonTokenTypeConfiguration
    JsonTokenTypeConfiguration (JsonTokenTypeConfiguration'),
    newJsonTokenTypeConfiguration,

    -- ** JwtTokenTypeConfiguration
    JwtTokenTypeConfiguration (JwtTokenTypeConfiguration'),
    newJwtTokenTypeConfiguration,

    -- ** MemberGroup
    MemberGroup (MemberGroup'),
    newMemberGroup,

    -- ** MemberUser
    MemberUser (MemberUser'),
    newMemberUser,

    -- ** OnPremiseConfiguration
    OnPremiseConfiguration (OnPremiseConfiguration'),
    newOnPremiseConfiguration,

    -- ** OneDriveConfiguration
    OneDriveConfiguration (OneDriveConfiguration'),
    newOneDriveConfiguration,

    -- ** OneDriveUsers
    OneDriveUsers (OneDriveUsers'),
    newOneDriveUsers,

    -- ** PersonasSummary
    PersonasSummary (PersonasSummary'),
    newPersonasSummary,

    -- ** Principal
    Principal (Principal'),
    newPrincipal,

    -- ** ProxyConfiguration
    ProxyConfiguration (ProxyConfiguration'),
    newProxyConfiguration,

    -- ** QueryResultItem
    QueryResultItem (QueryResultItem'),
    newQueryResultItem,

    -- ** QuerySuggestionsBlockListSummary
    QuerySuggestionsBlockListSummary (QuerySuggestionsBlockListSummary'),
    newQuerySuggestionsBlockListSummary,

    -- ** QuipConfiguration
    QuipConfiguration (QuipConfiguration'),
    newQuipConfiguration,

    -- ** Relevance
    Relevance (Relevance'),
    newRelevance,

    -- ** RelevanceFeedback
    RelevanceFeedback (RelevanceFeedback'),
    newRelevanceFeedback,

    -- ** S3DataSourceConfiguration
    S3DataSourceConfiguration (S3DataSourceConfiguration'),
    newS3DataSourceConfiguration,

    -- ** S3Path
    S3Path (S3Path'),
    newS3Path,

    -- ** SaaSConfiguration
    SaaSConfiguration (SaaSConfiguration'),
    newSaaSConfiguration,

    -- ** SalesforceChatterFeedConfiguration
    SalesforceChatterFeedConfiguration (SalesforceChatterFeedConfiguration'),
    newSalesforceChatterFeedConfiguration,

    -- ** SalesforceConfiguration
    SalesforceConfiguration (SalesforceConfiguration'),
    newSalesforceConfiguration,

    -- ** SalesforceCustomKnowledgeArticleTypeConfiguration
    SalesforceCustomKnowledgeArticleTypeConfiguration (SalesforceCustomKnowledgeArticleTypeConfiguration'),
    newSalesforceCustomKnowledgeArticleTypeConfiguration,

    -- ** SalesforceKnowledgeArticleConfiguration
    SalesforceKnowledgeArticleConfiguration (SalesforceKnowledgeArticleConfiguration'),
    newSalesforceKnowledgeArticleConfiguration,

    -- ** SalesforceStandardKnowledgeArticleTypeConfiguration
    SalesforceStandardKnowledgeArticleTypeConfiguration (SalesforceStandardKnowledgeArticleTypeConfiguration'),
    newSalesforceStandardKnowledgeArticleTypeConfiguration,

    -- ** SalesforceStandardObjectAttachmentConfiguration
    SalesforceStandardObjectAttachmentConfiguration (SalesforceStandardObjectAttachmentConfiguration'),
    newSalesforceStandardObjectAttachmentConfiguration,

    -- ** SalesforceStandardObjectConfiguration
    SalesforceStandardObjectConfiguration (SalesforceStandardObjectConfiguration'),
    newSalesforceStandardObjectConfiguration,

    -- ** ScoreAttributes
    ScoreAttributes (ScoreAttributes'),
    newScoreAttributes,

    -- ** Search
    Search (Search'),
    newSearch,

    -- ** SeedUrlConfiguration
    SeedUrlConfiguration (SeedUrlConfiguration'),
    newSeedUrlConfiguration,

    -- ** ServerSideEncryptionConfiguration
    ServerSideEncryptionConfiguration (ServerSideEncryptionConfiguration'),
    newServerSideEncryptionConfiguration,

    -- ** ServiceNowConfiguration
    ServiceNowConfiguration (ServiceNowConfiguration'),
    newServiceNowConfiguration,

    -- ** ServiceNowKnowledgeArticleConfiguration
    ServiceNowKnowledgeArticleConfiguration (ServiceNowKnowledgeArticleConfiguration'),
    newServiceNowKnowledgeArticleConfiguration,

    -- ** ServiceNowServiceCatalogConfiguration
    ServiceNowServiceCatalogConfiguration (ServiceNowServiceCatalogConfiguration'),
    newServiceNowServiceCatalogConfiguration,

    -- ** SharePointConfiguration
    SharePointConfiguration (SharePointConfiguration'),
    newSharePointConfiguration,

    -- ** SiteMapsConfiguration
    SiteMapsConfiguration (SiteMapsConfiguration'),
    newSiteMapsConfiguration,

    -- ** SlackConfiguration
    SlackConfiguration (SlackConfiguration'),
    newSlackConfiguration,

    -- ** SortingConfiguration
    SortingConfiguration (SortingConfiguration'),
    newSortingConfiguration,

    -- ** SpellCorrectedQuery
    SpellCorrectedQuery (SpellCorrectedQuery'),
    newSpellCorrectedQuery,

    -- ** SpellCorrectionConfiguration
    SpellCorrectionConfiguration (SpellCorrectionConfiguration'),
    newSpellCorrectionConfiguration,

    -- ** SqlConfiguration
    SqlConfiguration (SqlConfiguration'),
    newSqlConfiguration,

    -- ** Status
    Status (Status'),
    newStatus,

    -- ** Suggestion
    Suggestion (Suggestion'),
    newSuggestion,

    -- ** SuggestionHighlight
    SuggestionHighlight (SuggestionHighlight'),
    newSuggestionHighlight,

    -- ** SuggestionTextWithHighlights
    SuggestionTextWithHighlights (SuggestionTextWithHighlights'),
    newSuggestionTextWithHighlights,

    -- ** SuggestionValue
    SuggestionValue (SuggestionValue'),
    newSuggestionValue,

    -- ** TableCell
    TableCell (TableCell'),
    newTableCell,

    -- ** TableExcerpt
    TableExcerpt (TableExcerpt'),
    newTableExcerpt,

    -- ** TableRow
    TableRow (TableRow'),
    newTableRow,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** Template
    Template (Template'),
    newTemplate,

    -- ** TemplateConfiguration
    TemplateConfiguration (TemplateConfiguration'),
    newTemplateConfiguration,

    -- ** TextDocumentStatistics
    TextDocumentStatistics (TextDocumentStatistics'),
    newTextDocumentStatistics,

    -- ** TextWithHighlights
    TextWithHighlights (TextWithHighlights'),
    newTextWithHighlights,

    -- ** ThesaurusSummary
    ThesaurusSummary (ThesaurusSummary'),
    newThesaurusSummary,

    -- ** TimeRange
    TimeRange (TimeRange'),
    newTimeRange,

    -- ** Urls
    Urls (Urls'),
    newUrls,

    -- ** UserContext
    UserContext (UserContext'),
    newUserContext,

    -- ** UserGroupResolutionConfiguration
    UserGroupResolutionConfiguration (UserGroupResolutionConfiguration'),
    newUserGroupResolutionConfiguration,

    -- ** UserIdentityConfiguration
    UserIdentityConfiguration (UserIdentityConfiguration'),
    newUserIdentityConfiguration,

    -- ** UserTokenConfiguration
    UserTokenConfiguration (UserTokenConfiguration'),
    newUserTokenConfiguration,

    -- ** Warning
    Warning (Warning'),
    newWarning,

    -- ** WebCrawlerConfiguration
    WebCrawlerConfiguration (WebCrawlerConfiguration'),
    newWebCrawlerConfiguration,

    -- ** WorkDocsConfiguration
    WorkDocsConfiguration (WorkDocsConfiguration'),
    newWorkDocsConfiguration,
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
import Amazonka.Kendra.Lens
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
import Amazonka.Kendra.Types
import Amazonka.Kendra.UntagResource
import Amazonka.Kendra.UpdateAccessControlConfiguration
import Amazonka.Kendra.UpdateDataSource
import Amazonka.Kendra.UpdateExperience
import Amazonka.Kendra.UpdateIndex
import Amazonka.Kendra.UpdateQuerySuggestionsBlockList
import Amazonka.Kendra.UpdateQuerySuggestionsConfig
import Amazonka.Kendra.UpdateThesaurus
import Amazonka.Kendra.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Kendra'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
