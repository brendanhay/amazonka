{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Kendra
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2019-02-03@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Kendra is a service for indexing large document sets.
module Network.AWS.Kendra
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ResourceUnavailableException
    _ResourceUnavailableException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceAlreadyExistException
    _ResourceAlreadyExistException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ResourceInUseException
    _ResourceInUseException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateDataSource
    UpdateDataSource (UpdateDataSource'),
    newUpdateDataSource,
    UpdateDataSourceResponse (UpdateDataSourceResponse'),
    newUpdateDataSourceResponse,

    -- ** DeleteDataSource
    DeleteDataSource (DeleteDataSource'),
    newDeleteDataSource,
    DeleteDataSourceResponse (DeleteDataSourceResponse'),
    newDeleteDataSourceResponse,

    -- ** ClearQuerySuggestions
    ClearQuerySuggestions (ClearQuerySuggestions'),
    newClearQuerySuggestions,
    ClearQuerySuggestionsResponse (ClearQuerySuggestionsResponse'),
    newClearQuerySuggestionsResponse,

    -- ** ListFaqs
    ListFaqs (ListFaqs'),
    newListFaqs,
    ListFaqsResponse (ListFaqsResponse'),
    newListFaqsResponse,

    -- ** DeleteIndex
    DeleteIndex (DeleteIndex'),
    newDeleteIndex,
    DeleteIndexResponse (DeleteIndexResponse'),
    newDeleteIndexResponse,

    -- ** UpdateIndex
    UpdateIndex (UpdateIndex'),
    newUpdateIndex,
    UpdateIndexResponse (UpdateIndexResponse'),
    newUpdateIndexResponse,

    -- ** ListQuerySuggestionsBlockLists
    ListQuerySuggestionsBlockLists (ListQuerySuggestionsBlockLists'),
    newListQuerySuggestionsBlockLists,
    ListQuerySuggestionsBlockListsResponse (ListQuerySuggestionsBlockListsResponse'),
    newListQuerySuggestionsBlockListsResponse,

    -- ** CreateFaq
    CreateFaq (CreateFaq'),
    newCreateFaq,
    CreateFaqResponse (CreateFaqResponse'),
    newCreateFaqResponse,

    -- ** CreateQuerySuggestionsBlockList
    CreateQuerySuggestionsBlockList (CreateQuerySuggestionsBlockList'),
    newCreateQuerySuggestionsBlockList,
    CreateQuerySuggestionsBlockListResponse (CreateQuerySuggestionsBlockListResponse'),
    newCreateQuerySuggestionsBlockListResponse,

    -- ** BatchPutDocument
    BatchPutDocument (BatchPutDocument'),
    newBatchPutDocument,
    BatchPutDocumentResponse (BatchPutDocumentResponse'),
    newBatchPutDocumentResponse,

    -- ** BatchDeleteDocument
    BatchDeleteDocument (BatchDeleteDocument'),
    newBatchDeleteDocument,
    BatchDeleteDocumentResponse (BatchDeleteDocumentResponse'),
    newBatchDeleteDocumentResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SubmitFeedback
    SubmitFeedback (SubmitFeedback'),
    newSubmitFeedback,
    SubmitFeedbackResponse (SubmitFeedbackResponse'),
    newSubmitFeedbackResponse,

    -- ** StopDataSourceSyncJob
    StopDataSourceSyncJob (StopDataSourceSyncJob'),
    newStopDataSourceSyncJob,
    StopDataSourceSyncJobResponse (StopDataSourceSyncJobResponse'),
    newStopDataSourceSyncJobResponse,

    -- ** DescribeDataSource
    DescribeDataSource (DescribeDataSource'),
    newDescribeDataSource,
    DescribeDataSourceResponse (DescribeDataSourceResponse'),
    newDescribeDataSourceResponse,

    -- ** DescribeIndex
    DescribeIndex (DescribeIndex'),
    newDescribeIndex,
    DescribeIndexResponse (DescribeIndexResponse'),
    newDescribeIndexResponse,

    -- ** UpdateQuerySuggestionsConfig
    UpdateQuerySuggestionsConfig (UpdateQuerySuggestionsConfig'),
    newUpdateQuerySuggestionsConfig,
    UpdateQuerySuggestionsConfigResponse (UpdateQuerySuggestionsConfigResponse'),
    newUpdateQuerySuggestionsConfigResponse,

    -- ** CreateDataSource
    CreateDataSource (CreateDataSource'),
    newCreateDataSource,
    CreateDataSourceResponse (CreateDataSourceResponse'),
    newCreateDataSourceResponse,

    -- ** BatchGetDocumentStatus
    BatchGetDocumentStatus (BatchGetDocumentStatus'),
    newBatchGetDocumentStatus,
    BatchGetDocumentStatusResponse (BatchGetDocumentStatusResponse'),
    newBatchGetDocumentStatusResponse,

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

    -- ** DeleteQuerySuggestionsBlockList
    DeleteQuerySuggestionsBlockList (DeleteQuerySuggestionsBlockList'),
    newDeleteQuerySuggestionsBlockList,
    DeleteQuerySuggestionsBlockListResponse (DeleteQuerySuggestionsBlockListResponse'),
    newDeleteQuerySuggestionsBlockListResponse,

    -- ** UpdateQuerySuggestionsBlockList
    UpdateQuerySuggestionsBlockList (UpdateQuerySuggestionsBlockList'),
    newUpdateQuerySuggestionsBlockList,
    UpdateQuerySuggestionsBlockListResponse (UpdateQuerySuggestionsBlockListResponse'),
    newUpdateQuerySuggestionsBlockListResponse,

    -- ** DeleteFaq
    DeleteFaq (DeleteFaq'),
    newDeleteFaq,
    DeleteFaqResponse (DeleteFaqResponse'),
    newDeleteFaqResponse,

    -- ** PutPrincipalMapping
    PutPrincipalMapping (PutPrincipalMapping'),
    newPutPrincipalMapping,
    PutPrincipalMappingResponse (PutPrincipalMappingResponse'),
    newPutPrincipalMappingResponse,

    -- ** DeletePrincipalMapping
    DeletePrincipalMapping (DeletePrincipalMapping'),
    newDeletePrincipalMapping,
    DeletePrincipalMappingResponse (DeletePrincipalMappingResponse'),
    newDeletePrincipalMappingResponse,

    -- ** DescribeThesaurus
    DescribeThesaurus (DescribeThesaurus'),
    newDescribeThesaurus,
    DescribeThesaurusResponse (DescribeThesaurusResponse'),
    newDescribeThesaurusResponse,

    -- ** ListThesauri
    ListThesauri (ListThesauri'),
    newListThesauri,
    ListThesauriResponse (ListThesauriResponse'),
    newListThesauriResponse,

    -- ** CreateIndex
    CreateIndex (CreateIndex'),
    newCreateIndex,
    CreateIndexResponse (CreateIndexResponse'),
    newCreateIndexResponse,

    -- ** Query
    Query (Query'),
    newQuery,
    QueryResponse (QueryResponse'),
    newQueryResponse,

    -- ** DescribeQuerySuggestionsConfig
    DescribeQuerySuggestionsConfig (DescribeQuerySuggestionsConfig'),
    newDescribeQuerySuggestionsConfig,
    DescribeQuerySuggestionsConfigResponse (DescribeQuerySuggestionsConfigResponse'),
    newDescribeQuerySuggestionsConfigResponse,

    -- ** StartDataSourceSyncJob
    StartDataSourceSyncJob (StartDataSourceSyncJob'),
    newStartDataSourceSyncJob,
    StartDataSourceSyncJobResponse (StartDataSourceSyncJobResponse'),
    newStartDataSourceSyncJobResponse,

    -- ** ListIndices
    ListIndices (ListIndices'),
    newListIndices,
    ListIndicesResponse (ListIndicesResponse'),
    newListIndicesResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetQuerySuggestions
    GetQuerySuggestions (GetQuerySuggestions'),
    newGetQuerySuggestions,
    GetQuerySuggestionsResponse (GetQuerySuggestionsResponse'),
    newGetQuerySuggestionsResponse,

    -- ** DeleteThesaurus
    DeleteThesaurus (DeleteThesaurus'),
    newDeleteThesaurus,
    DeleteThesaurusResponse (DeleteThesaurusResponse'),
    newDeleteThesaurusResponse,

    -- ** UpdateThesaurus
    UpdateThesaurus (UpdateThesaurus'),
    newUpdateThesaurus,
    UpdateThesaurusResponse (UpdateThesaurusResponse'),
    newUpdateThesaurusResponse,

    -- ** DescribeFaq
    DescribeFaq (DescribeFaq'),
    newDescribeFaq,
    DescribeFaqResponse (DescribeFaqResponse'),
    newDescribeFaqResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DescribeQuerySuggestionsBlockList
    DescribeQuerySuggestionsBlockList (DescribeQuerySuggestionsBlockList'),
    newDescribeQuerySuggestionsBlockList,
    DescribeQuerySuggestionsBlockListResponse (DescribeQuerySuggestionsBlockListResponse'),
    newDescribeQuerySuggestionsBlockListResponse,

    -- ** DescribePrincipalMapping
    DescribePrincipalMapping (DescribePrincipalMapping'),
    newDescribePrincipalMapping,
    DescribePrincipalMappingResponse (DescribePrincipalMappingResponse'),
    newDescribePrincipalMappingResponse,

    -- ** ListGroupsOlderThanOrderingId
    ListGroupsOlderThanOrderingId (ListGroupsOlderThanOrderingId'),
    newListGroupsOlderThanOrderingId,
    ListGroupsOlderThanOrderingIdResponse (ListGroupsOlderThanOrderingIdResponse'),
    newListGroupsOlderThanOrderingIdResponse,

    -- ** CreateThesaurus
    CreateThesaurus (CreateThesaurus'),
    newCreateThesaurus,
    CreateThesaurusResponse (CreateThesaurusResponse'),
    newCreateThesaurusResponse,

    -- * Types

    -- ** AdditionalResultAttributeValueType
    AdditionalResultAttributeValueType (..),

    -- ** ConfluenceAttachmentFieldName
    ConfluenceAttachmentFieldName (..),

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

    -- ** ErrorCode
    ErrorCode (..),

    -- ** FaqFileFormat
    FaqFileFormat (..),

    -- ** FaqStatus
    FaqStatus (..),

    -- ** HighlightType
    HighlightType (..),

    -- ** IndexEdition
    IndexEdition (..),

    -- ** IndexStatus
    IndexStatus (..),

    -- ** KeyLocation
    KeyLocation (..),

    -- ** Mode
    Mode (..),

    -- ** Order
    Order (..),

    -- ** PrincipalMappingStatus
    PrincipalMappingStatus (..),

    -- ** PrincipalType
    PrincipalType (..),

    -- ** QueryIdentifiersEnclosingOption
    QueryIdentifiersEnclosingOption (..),

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

    -- ** SharePointVersion
    SharePointVersion (..),

    -- ** SortOrder
    SortOrder (..),

    -- ** ThesaurusStatus
    ThesaurusStatus (..),

    -- ** UserContextPolicy
    UserContextPolicy (..),

    -- ** UserGroupResolutionMode
    UserGroupResolutionMode (..),

    -- ** WebCrawlerMode
    WebCrawlerMode (..),

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

    -- ** Facet
    Facet (Facet'),
    newFacet,

    -- ** FacetResult
    FacetResult (FacetResult'),
    newFacetResult,

    -- ** FaqStatistics
    FaqStatistics (FaqStatistics'),
    newFaqStatistics,

    -- ** FaqSummary
    FaqSummary (FaqSummary'),
    newFaqSummary,

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

    -- ** IndexConfigurationSummary
    IndexConfigurationSummary (IndexConfigurationSummary'),
    newIndexConfigurationSummary,

    -- ** IndexStatistics
    IndexStatistics (IndexStatistics'),
    newIndexStatistics,

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

    -- ** OneDriveConfiguration
    OneDriveConfiguration (OneDriveConfiguration'),
    newOneDriveConfiguration,

    -- ** OneDriveUsers
    OneDriveUsers (OneDriveUsers'),
    newOneDriveUsers,

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

    -- ** SortingConfiguration
    SortingConfiguration (SortingConfiguration'),
    newSortingConfiguration,

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

    -- ** Tag
    Tag (Tag'),
    newTag,

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

    -- ** UserTokenConfiguration
    UserTokenConfiguration (UserTokenConfiguration'),
    newUserTokenConfiguration,

    -- ** WebCrawlerConfiguration
    WebCrawlerConfiguration (WebCrawlerConfiguration'),
    newWebCrawlerConfiguration,

    -- ** WorkDocsConfiguration
    WorkDocsConfiguration (WorkDocsConfiguration'),
    newWorkDocsConfiguration,
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
import Network.AWS.Kendra.Lens
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
import Network.AWS.Kendra.Types
import Network.AWS.Kendra.UntagResource
import Network.AWS.Kendra.UpdateDataSource
import Network.AWS.Kendra.UpdateIndex
import Network.AWS.Kendra.UpdateQuerySuggestionsBlockList
import Network.AWS.Kendra.UpdateQuerySuggestionsConfig
import Network.AWS.Kendra.UpdateThesaurus
import Network.AWS.Kendra.Waiters

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
