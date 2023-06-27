{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CleanRooms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2022-02-17@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the /AWS Clean Rooms API Reference/.
--
-- AWS Clean Rooms is an AWS service that helps multiple parties to join
-- their data together in a secure collaboration workspace. In the
-- collaboration, members who can query and receive results can get
-- insights into the collective datasets without either party getting
-- access to the other party\'s raw data.
--
-- To learn more about AWS Clean Rooms concepts, procedures, and best
-- practices, see the
-- <https://docs.aws.amazon.com/clean-rooms/latest/userguide/what-is.html AWS Clean Rooms User Guide>.
module Amazonka.CleanRooms
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

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

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

    -- ** BatchGetSchema
    BatchGetSchema (BatchGetSchema'),
    newBatchGetSchema,
    BatchGetSchemaResponse (BatchGetSchemaResponse'),
    newBatchGetSchemaResponse,

    -- ** CreateCollaboration
    CreateCollaboration (CreateCollaboration'),
    newCreateCollaboration,
    CreateCollaborationResponse (CreateCollaborationResponse'),
    newCreateCollaborationResponse,

    -- ** CreateConfiguredTable
    CreateConfiguredTable (CreateConfiguredTable'),
    newCreateConfiguredTable,
    CreateConfiguredTableResponse (CreateConfiguredTableResponse'),
    newCreateConfiguredTableResponse,

    -- ** CreateConfiguredTableAnalysisRule
    CreateConfiguredTableAnalysisRule (CreateConfiguredTableAnalysisRule'),
    newCreateConfiguredTableAnalysisRule,
    CreateConfiguredTableAnalysisRuleResponse (CreateConfiguredTableAnalysisRuleResponse'),
    newCreateConfiguredTableAnalysisRuleResponse,

    -- ** CreateConfiguredTableAssociation
    CreateConfiguredTableAssociation (CreateConfiguredTableAssociation'),
    newCreateConfiguredTableAssociation,
    CreateConfiguredTableAssociationResponse (CreateConfiguredTableAssociationResponse'),
    newCreateConfiguredTableAssociationResponse,

    -- ** CreateMembership
    CreateMembership (CreateMembership'),
    newCreateMembership,
    CreateMembershipResponse (CreateMembershipResponse'),
    newCreateMembershipResponse,

    -- ** DeleteCollaboration
    DeleteCollaboration (DeleteCollaboration'),
    newDeleteCollaboration,
    DeleteCollaborationResponse (DeleteCollaborationResponse'),
    newDeleteCollaborationResponse,

    -- ** DeleteConfiguredTable
    DeleteConfiguredTable (DeleteConfiguredTable'),
    newDeleteConfiguredTable,
    DeleteConfiguredTableResponse (DeleteConfiguredTableResponse'),
    newDeleteConfiguredTableResponse,

    -- ** DeleteConfiguredTableAnalysisRule
    DeleteConfiguredTableAnalysisRule (DeleteConfiguredTableAnalysisRule'),
    newDeleteConfiguredTableAnalysisRule,
    DeleteConfiguredTableAnalysisRuleResponse (DeleteConfiguredTableAnalysisRuleResponse'),
    newDeleteConfiguredTableAnalysisRuleResponse,

    -- ** DeleteConfiguredTableAssociation
    DeleteConfiguredTableAssociation (DeleteConfiguredTableAssociation'),
    newDeleteConfiguredTableAssociation,
    DeleteConfiguredTableAssociationResponse (DeleteConfiguredTableAssociationResponse'),
    newDeleteConfiguredTableAssociationResponse,

    -- ** DeleteMember
    DeleteMember (DeleteMember'),
    newDeleteMember,
    DeleteMemberResponse (DeleteMemberResponse'),
    newDeleteMemberResponse,

    -- ** DeleteMembership
    DeleteMembership (DeleteMembership'),
    newDeleteMembership,
    DeleteMembershipResponse (DeleteMembershipResponse'),
    newDeleteMembershipResponse,

    -- ** GetCollaboration
    GetCollaboration (GetCollaboration'),
    newGetCollaboration,
    GetCollaborationResponse (GetCollaborationResponse'),
    newGetCollaborationResponse,

    -- ** GetConfiguredTable
    GetConfiguredTable (GetConfiguredTable'),
    newGetConfiguredTable,
    GetConfiguredTableResponse (GetConfiguredTableResponse'),
    newGetConfiguredTableResponse,

    -- ** GetConfiguredTableAnalysisRule
    GetConfiguredTableAnalysisRule (GetConfiguredTableAnalysisRule'),
    newGetConfiguredTableAnalysisRule,
    GetConfiguredTableAnalysisRuleResponse (GetConfiguredTableAnalysisRuleResponse'),
    newGetConfiguredTableAnalysisRuleResponse,

    -- ** GetConfiguredTableAssociation
    GetConfiguredTableAssociation (GetConfiguredTableAssociation'),
    newGetConfiguredTableAssociation,
    GetConfiguredTableAssociationResponse (GetConfiguredTableAssociationResponse'),
    newGetConfiguredTableAssociationResponse,

    -- ** GetMembership
    GetMembership (GetMembership'),
    newGetMembership,
    GetMembershipResponse (GetMembershipResponse'),
    newGetMembershipResponse,

    -- ** GetProtectedQuery
    GetProtectedQuery (GetProtectedQuery'),
    newGetProtectedQuery,
    GetProtectedQueryResponse (GetProtectedQueryResponse'),
    newGetProtectedQueryResponse,

    -- ** GetSchema
    GetSchema (GetSchema'),
    newGetSchema,
    GetSchemaResponse (GetSchemaResponse'),
    newGetSchemaResponse,

    -- ** GetSchemaAnalysisRule
    GetSchemaAnalysisRule (GetSchemaAnalysisRule'),
    newGetSchemaAnalysisRule,
    GetSchemaAnalysisRuleResponse (GetSchemaAnalysisRuleResponse'),
    newGetSchemaAnalysisRuleResponse,

    -- ** ListCollaborations (Paginated)
    ListCollaborations (ListCollaborations'),
    newListCollaborations,
    ListCollaborationsResponse (ListCollaborationsResponse'),
    newListCollaborationsResponse,

    -- ** ListConfiguredTableAssociations (Paginated)
    ListConfiguredTableAssociations (ListConfiguredTableAssociations'),
    newListConfiguredTableAssociations,
    ListConfiguredTableAssociationsResponse (ListConfiguredTableAssociationsResponse'),
    newListConfiguredTableAssociationsResponse,

    -- ** ListConfiguredTables (Paginated)
    ListConfiguredTables (ListConfiguredTables'),
    newListConfiguredTables,
    ListConfiguredTablesResponse (ListConfiguredTablesResponse'),
    newListConfiguredTablesResponse,

    -- ** ListMembers (Paginated)
    ListMembers (ListMembers'),
    newListMembers,
    ListMembersResponse (ListMembersResponse'),
    newListMembersResponse,

    -- ** ListMemberships (Paginated)
    ListMemberships (ListMemberships'),
    newListMemberships,
    ListMembershipsResponse (ListMembershipsResponse'),
    newListMembershipsResponse,

    -- ** ListProtectedQueries (Paginated)
    ListProtectedQueries (ListProtectedQueries'),
    newListProtectedQueries,
    ListProtectedQueriesResponse (ListProtectedQueriesResponse'),
    newListProtectedQueriesResponse,

    -- ** ListSchemas (Paginated)
    ListSchemas (ListSchemas'),
    newListSchemas,
    ListSchemasResponse (ListSchemasResponse'),
    newListSchemasResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** StartProtectedQuery
    StartProtectedQuery (StartProtectedQuery'),
    newStartProtectedQuery,
    StartProtectedQueryResponse (StartProtectedQueryResponse'),
    newStartProtectedQueryResponse,

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

    -- ** UpdateCollaboration
    UpdateCollaboration (UpdateCollaboration'),
    newUpdateCollaboration,
    UpdateCollaborationResponse (UpdateCollaborationResponse'),
    newUpdateCollaborationResponse,

    -- ** UpdateConfiguredTable
    UpdateConfiguredTable (UpdateConfiguredTable'),
    newUpdateConfiguredTable,
    UpdateConfiguredTableResponse (UpdateConfiguredTableResponse'),
    newUpdateConfiguredTableResponse,

    -- ** UpdateConfiguredTableAnalysisRule
    UpdateConfiguredTableAnalysisRule (UpdateConfiguredTableAnalysisRule'),
    newUpdateConfiguredTableAnalysisRule,
    UpdateConfiguredTableAnalysisRuleResponse (UpdateConfiguredTableAnalysisRuleResponse'),
    newUpdateConfiguredTableAnalysisRuleResponse,

    -- ** UpdateConfiguredTableAssociation
    UpdateConfiguredTableAssociation (UpdateConfiguredTableAssociation'),
    newUpdateConfiguredTableAssociation,
    UpdateConfiguredTableAssociationResponse (UpdateConfiguredTableAssociationResponse'),
    newUpdateConfiguredTableAssociationResponse,

    -- ** UpdateMembership
    UpdateMembership (UpdateMembership'),
    newUpdateMembership,
    UpdateMembershipResponse (UpdateMembershipResponse'),
    newUpdateMembershipResponse,

    -- ** UpdateProtectedQuery
    UpdateProtectedQuery (UpdateProtectedQuery'),
    newUpdateProtectedQuery,
    UpdateProtectedQueryResponse (UpdateProtectedQueryResponse'),
    newUpdateProtectedQueryResponse,

    -- * Types

    -- ** AggregateFunctionName
    AggregateFunctionName (..),

    -- ** AggregationType
    AggregationType (..),

    -- ** AnalysisMethod
    AnalysisMethod (..),

    -- ** AnalysisRuleType
    AnalysisRuleType (..),

    -- ** CollaborationQueryLogStatus
    CollaborationQueryLogStatus (..),

    -- ** ConfiguredTableAnalysisRuleType
    ConfiguredTableAnalysisRuleType (..),

    -- ** FilterableMemberStatus
    FilterableMemberStatus (..),

    -- ** JoinRequiredOption
    JoinRequiredOption (..),

    -- ** MemberAbility
    MemberAbility (..),

    -- ** MemberStatus
    MemberStatus (..),

    -- ** MembershipQueryLogStatus
    MembershipQueryLogStatus (..),

    -- ** MembershipStatus
    MembershipStatus (..),

    -- ** ProtectedQueryStatus
    ProtectedQueryStatus (..),

    -- ** ProtectedQueryType
    ProtectedQueryType (..),

    -- ** ResultFormat
    ResultFormat (..),

    -- ** ScalarFunctions
    ScalarFunctions (..),

    -- ** SchemaType
    SchemaType (..),

    -- ** TargetProtectedQueryStatus
    TargetProtectedQueryStatus (..),

    -- ** AggregateColumn
    AggregateColumn (AggregateColumn'),
    newAggregateColumn,

    -- ** AggregationConstraint
    AggregationConstraint (AggregationConstraint'),
    newAggregationConstraint,

    -- ** AnalysisRule
    AnalysisRule (AnalysisRule'),
    newAnalysisRule,

    -- ** AnalysisRuleAggregation
    AnalysisRuleAggregation (AnalysisRuleAggregation'),
    newAnalysisRuleAggregation,

    -- ** AnalysisRuleList
    AnalysisRuleList (AnalysisRuleList'),
    newAnalysisRuleList,

    -- ** AnalysisRulePolicy
    AnalysisRulePolicy (AnalysisRulePolicy'),
    newAnalysisRulePolicy,

    -- ** AnalysisRulePolicyV1
    AnalysisRulePolicyV1 (AnalysisRulePolicyV1'),
    newAnalysisRulePolicyV1,

    -- ** BatchGetSchemaError
    BatchGetSchemaError (BatchGetSchemaError'),
    newBatchGetSchemaError,

    -- ** Collaboration
    Collaboration (Collaboration'),
    newCollaboration,

    -- ** CollaborationSummary
    CollaborationSummary (CollaborationSummary'),
    newCollaborationSummary,

    -- ** Column
    Column (Column'),
    newColumn,

    -- ** ConfiguredTable
    ConfiguredTable (ConfiguredTable'),
    newConfiguredTable,

    -- ** ConfiguredTableAnalysisRule
    ConfiguredTableAnalysisRule (ConfiguredTableAnalysisRule'),
    newConfiguredTableAnalysisRule,

    -- ** ConfiguredTableAnalysisRulePolicy
    ConfiguredTableAnalysisRulePolicy (ConfiguredTableAnalysisRulePolicy'),
    newConfiguredTableAnalysisRulePolicy,

    -- ** ConfiguredTableAnalysisRulePolicyV1
    ConfiguredTableAnalysisRulePolicyV1 (ConfiguredTableAnalysisRulePolicyV1'),
    newConfiguredTableAnalysisRulePolicyV1,

    -- ** ConfiguredTableAssociation
    ConfiguredTableAssociation (ConfiguredTableAssociation'),
    newConfiguredTableAssociation,

    -- ** ConfiguredTableAssociationSummary
    ConfiguredTableAssociationSummary (ConfiguredTableAssociationSummary'),
    newConfiguredTableAssociationSummary,

    -- ** ConfiguredTableSummary
    ConfiguredTableSummary (ConfiguredTableSummary'),
    newConfiguredTableSummary,

    -- ** DataEncryptionMetadata
    DataEncryptionMetadata (DataEncryptionMetadata'),
    newDataEncryptionMetadata,

    -- ** GlueTableReference
    GlueTableReference (GlueTableReference'),
    newGlueTableReference,

    -- ** MemberSpecification
    MemberSpecification (MemberSpecification'),
    newMemberSpecification,

    -- ** MemberSummary
    MemberSummary (MemberSummary'),
    newMemberSummary,

    -- ** Membership
    Membership (Membership'),
    newMembership,

    -- ** MembershipSummary
    MembershipSummary (MembershipSummary'),
    newMembershipSummary,

    -- ** ProtectedQuery
    ProtectedQuery (ProtectedQuery'),
    newProtectedQuery,

    -- ** ProtectedQueryError
    ProtectedQueryError (ProtectedQueryError'),
    newProtectedQueryError,

    -- ** ProtectedQueryOutput
    ProtectedQueryOutput (ProtectedQueryOutput'),
    newProtectedQueryOutput,

    -- ** ProtectedQueryOutputConfiguration
    ProtectedQueryOutputConfiguration (ProtectedQueryOutputConfiguration'),
    newProtectedQueryOutputConfiguration,

    -- ** ProtectedQueryResult
    ProtectedQueryResult (ProtectedQueryResult'),
    newProtectedQueryResult,

    -- ** ProtectedQueryResultConfiguration
    ProtectedQueryResultConfiguration (ProtectedQueryResultConfiguration'),
    newProtectedQueryResultConfiguration,

    -- ** ProtectedQueryS3Output
    ProtectedQueryS3Output (ProtectedQueryS3Output'),
    newProtectedQueryS3Output,

    -- ** ProtectedQueryS3OutputConfiguration
    ProtectedQueryS3OutputConfiguration (ProtectedQueryS3OutputConfiguration'),
    newProtectedQueryS3OutputConfiguration,

    -- ** ProtectedQuerySQLParameters
    ProtectedQuerySQLParameters (ProtectedQuerySQLParameters'),
    newProtectedQuerySQLParameters,

    -- ** ProtectedQueryStatistics
    ProtectedQueryStatistics (ProtectedQueryStatistics'),
    newProtectedQueryStatistics,

    -- ** ProtectedQuerySummary
    ProtectedQuerySummary (ProtectedQuerySummary'),
    newProtectedQuerySummary,

    -- ** Schema
    Schema (Schema'),
    newSchema,

    -- ** SchemaSummary
    SchemaSummary (SchemaSummary'),
    newSchemaSummary,

    -- ** TableReference
    TableReference (TableReference'),
    newTableReference,
  )
where

import Amazonka.CleanRooms.BatchGetSchema
import Amazonka.CleanRooms.CreateCollaboration
import Amazonka.CleanRooms.CreateConfiguredTable
import Amazonka.CleanRooms.CreateConfiguredTableAnalysisRule
import Amazonka.CleanRooms.CreateConfiguredTableAssociation
import Amazonka.CleanRooms.CreateMembership
import Amazonka.CleanRooms.DeleteCollaboration
import Amazonka.CleanRooms.DeleteConfiguredTable
import Amazonka.CleanRooms.DeleteConfiguredTableAnalysisRule
import Amazonka.CleanRooms.DeleteConfiguredTableAssociation
import Amazonka.CleanRooms.DeleteMember
import Amazonka.CleanRooms.DeleteMembership
import Amazonka.CleanRooms.GetCollaboration
import Amazonka.CleanRooms.GetConfiguredTable
import Amazonka.CleanRooms.GetConfiguredTableAnalysisRule
import Amazonka.CleanRooms.GetConfiguredTableAssociation
import Amazonka.CleanRooms.GetMembership
import Amazonka.CleanRooms.GetProtectedQuery
import Amazonka.CleanRooms.GetSchema
import Amazonka.CleanRooms.GetSchemaAnalysisRule
import Amazonka.CleanRooms.Lens
import Amazonka.CleanRooms.ListCollaborations
import Amazonka.CleanRooms.ListConfiguredTableAssociations
import Amazonka.CleanRooms.ListConfiguredTables
import Amazonka.CleanRooms.ListMembers
import Amazonka.CleanRooms.ListMemberships
import Amazonka.CleanRooms.ListProtectedQueries
import Amazonka.CleanRooms.ListSchemas
import Amazonka.CleanRooms.ListTagsForResource
import Amazonka.CleanRooms.StartProtectedQuery
import Amazonka.CleanRooms.TagResource
import Amazonka.CleanRooms.Types
import Amazonka.CleanRooms.UntagResource
import Amazonka.CleanRooms.UpdateCollaboration
import Amazonka.CleanRooms.UpdateConfiguredTable
import Amazonka.CleanRooms.UpdateConfiguredTableAnalysisRule
import Amazonka.CleanRooms.UpdateConfiguredTableAssociation
import Amazonka.CleanRooms.UpdateMembership
import Amazonka.CleanRooms.UpdateProtectedQuery
import Amazonka.CleanRooms.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CleanRooms'.

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
