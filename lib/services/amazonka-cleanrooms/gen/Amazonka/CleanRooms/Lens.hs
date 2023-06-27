{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CleanRooms.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Lens
  ( -- * Operations

    -- ** BatchGetSchema
    batchGetSchema_collaborationIdentifier,
    batchGetSchema_names,
    batchGetSchemaResponse_httpStatus,
    batchGetSchemaResponse_schemas,
    batchGetSchemaResponse_errors,

    -- ** CreateCollaboration
    createCollaboration_dataEncryptionMetadata,
    createCollaboration_tags,
    createCollaboration_members,
    createCollaboration_name,
    createCollaboration_description,
    createCollaboration_creatorMemberAbilities,
    createCollaboration_creatorDisplayName,
    createCollaboration_queryLogStatus,
    createCollaborationResponse_httpStatus,
    createCollaborationResponse_collaboration,

    -- ** CreateConfiguredTable
    createConfiguredTable_description,
    createConfiguredTable_tags,
    createConfiguredTable_name,
    createConfiguredTable_tableReference,
    createConfiguredTable_allowedColumns,
    createConfiguredTable_analysisMethod,
    createConfiguredTableResponse_httpStatus,
    createConfiguredTableResponse_configuredTable,

    -- ** CreateConfiguredTableAnalysisRule
    createConfiguredTableAnalysisRule_configuredTableIdentifier,
    createConfiguredTableAnalysisRule_analysisRuleType,
    createConfiguredTableAnalysisRule_analysisRulePolicy,
    createConfiguredTableAnalysisRuleResponse_httpStatus,
    createConfiguredTableAnalysisRuleResponse_analysisRule,

    -- ** CreateConfiguredTableAssociation
    createConfiguredTableAssociation_description,
    createConfiguredTableAssociation_tags,
    createConfiguredTableAssociation_name,
    createConfiguredTableAssociation_membershipIdentifier,
    createConfiguredTableAssociation_configuredTableIdentifier,
    createConfiguredTableAssociation_roleArn,
    createConfiguredTableAssociationResponse_httpStatus,
    createConfiguredTableAssociationResponse_configuredTableAssociation,

    -- ** CreateMembership
    createMembership_tags,
    createMembership_collaborationIdentifier,
    createMembership_queryLogStatus,
    createMembershipResponse_httpStatus,
    createMembershipResponse_membership,

    -- ** DeleteCollaboration
    deleteCollaboration_collaborationIdentifier,
    deleteCollaborationResponse_httpStatus,

    -- ** DeleteConfiguredTable
    deleteConfiguredTable_configuredTableIdentifier,
    deleteConfiguredTableResponse_httpStatus,

    -- ** DeleteConfiguredTableAnalysisRule
    deleteConfiguredTableAnalysisRule_configuredTableIdentifier,
    deleteConfiguredTableAnalysisRule_analysisRuleType,
    deleteConfiguredTableAnalysisRuleResponse_httpStatus,

    -- ** DeleteConfiguredTableAssociation
    deleteConfiguredTableAssociation_configuredTableAssociationIdentifier,
    deleteConfiguredTableAssociation_membershipIdentifier,
    deleteConfiguredTableAssociationResponse_httpStatus,

    -- ** DeleteMember
    deleteMember_collaborationIdentifier,
    deleteMember_accountId,
    deleteMemberResponse_httpStatus,

    -- ** DeleteMembership
    deleteMembership_membershipIdentifier,
    deleteMembershipResponse_httpStatus,

    -- ** GetCollaboration
    getCollaboration_collaborationIdentifier,
    getCollaborationResponse_httpStatus,
    getCollaborationResponse_collaboration,

    -- ** GetConfiguredTable
    getConfiguredTable_configuredTableIdentifier,
    getConfiguredTableResponse_httpStatus,
    getConfiguredTableResponse_configuredTable,

    -- ** GetConfiguredTableAnalysisRule
    getConfiguredTableAnalysisRule_configuredTableIdentifier,
    getConfiguredTableAnalysisRule_analysisRuleType,
    getConfiguredTableAnalysisRuleResponse_httpStatus,
    getConfiguredTableAnalysisRuleResponse_analysisRule,

    -- ** GetConfiguredTableAssociation
    getConfiguredTableAssociation_configuredTableAssociationIdentifier,
    getConfiguredTableAssociation_membershipIdentifier,
    getConfiguredTableAssociationResponse_httpStatus,
    getConfiguredTableAssociationResponse_configuredTableAssociation,

    -- ** GetMembership
    getMembership_membershipIdentifier,
    getMembershipResponse_httpStatus,
    getMembershipResponse_membership,

    -- ** GetProtectedQuery
    getProtectedQuery_membershipIdentifier,
    getProtectedQuery_protectedQueryIdentifier,
    getProtectedQueryResponse_httpStatus,
    getProtectedQueryResponse_protectedQuery,

    -- ** GetSchema
    getSchema_collaborationIdentifier,
    getSchema_name,
    getSchemaResponse_httpStatus,
    getSchemaResponse_schema,

    -- ** GetSchemaAnalysisRule
    getSchemaAnalysisRule_collaborationIdentifier,
    getSchemaAnalysisRule_name,
    getSchemaAnalysisRule_type,
    getSchemaAnalysisRuleResponse_httpStatus,
    getSchemaAnalysisRuleResponse_analysisRule,

    -- ** ListCollaborations
    listCollaborations_maxResults,
    listCollaborations_memberStatus,
    listCollaborations_nextToken,
    listCollaborationsResponse_nextToken,
    listCollaborationsResponse_httpStatus,
    listCollaborationsResponse_collaborationList,

    -- ** ListConfiguredTableAssociations
    listConfiguredTableAssociations_maxResults,
    listConfiguredTableAssociations_nextToken,
    listConfiguredTableAssociations_membershipIdentifier,
    listConfiguredTableAssociationsResponse_nextToken,
    listConfiguredTableAssociationsResponse_httpStatus,
    listConfiguredTableAssociationsResponse_configuredTableAssociationSummaries,

    -- ** ListConfiguredTables
    listConfiguredTables_maxResults,
    listConfiguredTables_nextToken,
    listConfiguredTablesResponse_nextToken,
    listConfiguredTablesResponse_httpStatus,
    listConfiguredTablesResponse_configuredTableSummaries,

    -- ** ListMembers
    listMembers_maxResults,
    listMembers_nextToken,
    listMembers_collaborationIdentifier,
    listMembersResponse_nextToken,
    listMembersResponse_httpStatus,
    listMembersResponse_memberSummaries,

    -- ** ListMemberships
    listMemberships_maxResults,
    listMemberships_nextToken,
    listMemberships_status,
    listMembershipsResponse_nextToken,
    listMembershipsResponse_httpStatus,
    listMembershipsResponse_membershipSummaries,

    -- ** ListProtectedQueries
    listProtectedQueries_maxResults,
    listProtectedQueries_nextToken,
    listProtectedQueries_status,
    listProtectedQueries_membershipIdentifier,
    listProtectedQueriesResponse_nextToken,
    listProtectedQueriesResponse_httpStatus,
    listProtectedQueriesResponse_protectedQueries,

    -- ** ListSchemas
    listSchemas_maxResults,
    listSchemas_nextToken,
    listSchemas_schemaType,
    listSchemas_collaborationIdentifier,
    listSchemasResponse_nextToken,
    listSchemasResponse_httpStatus,
    listSchemasResponse_schemaSummaries,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** StartProtectedQuery
    startProtectedQuery_type,
    startProtectedQuery_membershipIdentifier,
    startProtectedQuery_sqlParameters,
    startProtectedQuery_resultConfiguration,
    startProtectedQueryResponse_httpStatus,
    startProtectedQueryResponse_protectedQuery,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateCollaboration
    updateCollaboration_description,
    updateCollaboration_name,
    updateCollaboration_collaborationIdentifier,
    updateCollaborationResponse_httpStatus,
    updateCollaborationResponse_collaboration,

    -- ** UpdateConfiguredTable
    updateConfiguredTable_description,
    updateConfiguredTable_name,
    updateConfiguredTable_configuredTableIdentifier,
    updateConfiguredTableResponse_httpStatus,
    updateConfiguredTableResponse_configuredTable,

    -- ** UpdateConfiguredTableAnalysisRule
    updateConfiguredTableAnalysisRule_configuredTableIdentifier,
    updateConfiguredTableAnalysisRule_analysisRuleType,
    updateConfiguredTableAnalysisRule_analysisRulePolicy,
    updateConfiguredTableAnalysisRuleResponse_httpStatus,
    updateConfiguredTableAnalysisRuleResponse_analysisRule,

    -- ** UpdateConfiguredTableAssociation
    updateConfiguredTableAssociation_description,
    updateConfiguredTableAssociation_roleArn,
    updateConfiguredTableAssociation_configuredTableAssociationIdentifier,
    updateConfiguredTableAssociation_membershipIdentifier,
    updateConfiguredTableAssociationResponse_httpStatus,
    updateConfiguredTableAssociationResponse_configuredTableAssociation,

    -- ** UpdateMembership
    updateMembership_queryLogStatus,
    updateMembership_membershipIdentifier,
    updateMembershipResponse_httpStatus,
    updateMembershipResponse_membership,

    -- ** UpdateProtectedQuery
    updateProtectedQuery_membershipIdentifier,
    updateProtectedQuery_protectedQueryIdentifier,
    updateProtectedQuery_targetStatus,
    updateProtectedQueryResponse_httpStatus,
    updateProtectedQueryResponse_protectedQuery,

    -- * Types

    -- ** AggregateColumn
    aggregateColumn_columnNames,
    aggregateColumn_function,

    -- ** AggregationConstraint
    aggregationConstraint_columnName,
    aggregationConstraint_minimum,
    aggregationConstraint_type,

    -- ** AnalysisRule
    analysisRule_collaborationId,
    analysisRule_type,
    analysisRule_name,
    analysisRule_createTime,
    analysisRule_updateTime,
    analysisRule_policy,

    -- ** AnalysisRuleAggregation
    analysisRuleAggregation_joinRequired,
    analysisRuleAggregation_aggregateColumns,
    analysisRuleAggregation_joinColumns,
    analysisRuleAggregation_dimensionColumns,
    analysisRuleAggregation_scalarFunctions,
    analysisRuleAggregation_outputConstraints,

    -- ** AnalysisRuleList
    analysisRuleList_joinColumns,
    analysisRuleList_listColumns,

    -- ** AnalysisRulePolicy
    analysisRulePolicy_v1,

    -- ** AnalysisRulePolicyV1
    analysisRulePolicyV1_aggregation,
    analysisRulePolicyV1_list,

    -- ** BatchGetSchemaError
    batchGetSchemaError_name,
    batchGetSchemaError_code,
    batchGetSchemaError_message,

    -- ** Collaboration
    collaboration_dataEncryptionMetadata,
    collaboration_description,
    collaboration_membershipArn,
    collaboration_membershipId,
    collaboration_id,
    collaboration_arn,
    collaboration_name,
    collaboration_creatorAccountId,
    collaboration_creatorDisplayName,
    collaboration_createTime,
    collaboration_updateTime,
    collaboration_memberStatus,
    collaboration_queryLogStatus,

    -- ** CollaborationSummary
    collaborationSummary_membershipArn,
    collaborationSummary_membershipId,
    collaborationSummary_id,
    collaborationSummary_arn,
    collaborationSummary_name,
    collaborationSummary_creatorAccountId,
    collaborationSummary_creatorDisplayName,
    collaborationSummary_createTime,
    collaborationSummary_updateTime,
    collaborationSummary_memberStatus,

    -- ** Column
    column_name,
    column_type,

    -- ** ConfiguredTable
    configuredTable_description,
    configuredTable_id,
    configuredTable_arn,
    configuredTable_name,
    configuredTable_tableReference,
    configuredTable_createTime,
    configuredTable_updateTime,
    configuredTable_analysisRuleTypes,
    configuredTable_analysisMethod,
    configuredTable_allowedColumns,

    -- ** ConfiguredTableAnalysisRule
    configuredTableAnalysisRule_configuredTableId,
    configuredTableAnalysisRule_configuredTableArn,
    configuredTableAnalysisRule_policy,
    configuredTableAnalysisRule_type,
    configuredTableAnalysisRule_createTime,
    configuredTableAnalysisRule_updateTime,

    -- ** ConfiguredTableAnalysisRulePolicy
    configuredTableAnalysisRulePolicy_v1,

    -- ** ConfiguredTableAnalysisRulePolicyV1
    configuredTableAnalysisRulePolicyV1_aggregation,
    configuredTableAnalysisRulePolicyV1_list,

    -- ** ConfiguredTableAssociation
    configuredTableAssociation_description,
    configuredTableAssociation_arn,
    configuredTableAssociation_id,
    configuredTableAssociation_configuredTableId,
    configuredTableAssociation_configuredTableArn,
    configuredTableAssociation_membershipId,
    configuredTableAssociation_membershipArn,
    configuredTableAssociation_roleArn,
    configuredTableAssociation_name,
    configuredTableAssociation_createTime,
    configuredTableAssociation_updateTime,

    -- ** ConfiguredTableAssociationSummary
    configuredTableAssociationSummary_configuredTableId,
    configuredTableAssociationSummary_membershipId,
    configuredTableAssociationSummary_membershipArn,
    configuredTableAssociationSummary_name,
    configuredTableAssociationSummary_createTime,
    configuredTableAssociationSummary_updateTime,
    configuredTableAssociationSummary_id,
    configuredTableAssociationSummary_arn,

    -- ** ConfiguredTableSummary
    configuredTableSummary_id,
    configuredTableSummary_arn,
    configuredTableSummary_name,
    configuredTableSummary_createTime,
    configuredTableSummary_updateTime,
    configuredTableSummary_analysisRuleTypes,
    configuredTableSummary_analysisMethod,

    -- ** DataEncryptionMetadata
    dataEncryptionMetadata_allowCleartext,
    dataEncryptionMetadata_allowDuplicates,
    dataEncryptionMetadata_allowJoinsOnColumnsWithDifferentNames,
    dataEncryptionMetadata_preserveNulls,

    -- ** GlueTableReference
    glueTableReference_tableName,
    glueTableReference_databaseName,

    -- ** MemberSpecification
    memberSpecification_accountId,
    memberSpecification_memberAbilities,
    memberSpecification_displayName,

    -- ** MemberSummary
    memberSummary_membershipArn,
    memberSummary_membershipId,
    memberSummary_accountId,
    memberSummary_status,
    memberSummary_displayName,
    memberSummary_abilities,
    memberSummary_createTime,
    memberSummary_updateTime,

    -- ** Membership
    membership_id,
    membership_arn,
    membership_collaborationArn,
    membership_collaborationId,
    membership_collaborationCreatorAccountId,
    membership_collaborationCreatorDisplayName,
    membership_collaborationName,
    membership_createTime,
    membership_updateTime,
    membership_status,
    membership_memberAbilities,
    membership_queryLogStatus,

    -- ** MembershipSummary
    membershipSummary_id,
    membershipSummary_arn,
    membershipSummary_collaborationArn,
    membershipSummary_collaborationId,
    membershipSummary_collaborationCreatorAccountId,
    membershipSummary_collaborationCreatorDisplayName,
    membershipSummary_collaborationName,
    membershipSummary_createTime,
    membershipSummary_updateTime,
    membershipSummary_status,
    membershipSummary_memberAbilities,

    -- ** ProtectedQuery
    protectedQuery_error,
    protectedQuery_result,
    protectedQuery_statistics,
    protectedQuery_id,
    protectedQuery_membershipId,
    protectedQuery_membershipArn,
    protectedQuery_createTime,
    protectedQuery_sqlParameters,
    protectedQuery_status,
    protectedQuery_resultConfiguration,

    -- ** ProtectedQueryError
    protectedQueryError_message,
    protectedQueryError_code,

    -- ** ProtectedQueryOutput
    protectedQueryOutput_s3,

    -- ** ProtectedQueryOutputConfiguration
    protectedQueryOutputConfiguration_s3,

    -- ** ProtectedQueryResult
    protectedQueryResult_output,

    -- ** ProtectedQueryResultConfiguration
    protectedQueryResultConfiguration_outputConfiguration,

    -- ** ProtectedQueryS3Output
    protectedQueryS3Output_location,

    -- ** ProtectedQueryS3OutputConfiguration
    protectedQueryS3OutputConfiguration_keyPrefix,
    protectedQueryS3OutputConfiguration_resultFormat,
    protectedQueryS3OutputConfiguration_bucket,

    -- ** ProtectedQuerySQLParameters
    protectedQuerySQLParameters_queryString,

    -- ** ProtectedQueryStatistics
    protectedQueryStatistics_totalDurationInMillis,

    -- ** ProtectedQuerySummary
    protectedQuerySummary_id,
    protectedQuerySummary_membershipId,
    protectedQuerySummary_membershipArn,
    protectedQuerySummary_createTime,
    protectedQuerySummary_status,

    -- ** Schema
    schema_analysisMethod,
    schema_columns,
    schema_partitionKeys,
    schema_analysisRuleTypes,
    schema_creatorAccountId,
    schema_name,
    schema_collaborationId,
    schema_collaborationArn,
    schema_description,
    schema_createTime,
    schema_updateTime,
    schema_type,

    -- ** SchemaSummary
    schemaSummary_analysisMethod,
    schemaSummary_name,
    schemaSummary_type,
    schemaSummary_creatorAccountId,
    schemaSummary_createTime,
    schemaSummary_updateTime,
    schemaSummary_collaborationId,
    schemaSummary_collaborationArn,
    schemaSummary_analysisRuleTypes,

    -- ** TableReference
    tableReference_glue,
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
import Amazonka.CleanRooms.Types.AggregateColumn
import Amazonka.CleanRooms.Types.AggregationConstraint
import Amazonka.CleanRooms.Types.AnalysisRule
import Amazonka.CleanRooms.Types.AnalysisRuleAggregation
import Amazonka.CleanRooms.Types.AnalysisRuleList
import Amazonka.CleanRooms.Types.AnalysisRulePolicy
import Amazonka.CleanRooms.Types.AnalysisRulePolicyV1
import Amazonka.CleanRooms.Types.BatchGetSchemaError
import Amazonka.CleanRooms.Types.Collaboration
import Amazonka.CleanRooms.Types.CollaborationSummary
import Amazonka.CleanRooms.Types.Column
import Amazonka.CleanRooms.Types.ConfiguredTable
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRule
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicy
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicyV1
import Amazonka.CleanRooms.Types.ConfiguredTableAssociation
import Amazonka.CleanRooms.Types.ConfiguredTableAssociationSummary
import Amazonka.CleanRooms.Types.ConfiguredTableSummary
import Amazonka.CleanRooms.Types.DataEncryptionMetadata
import Amazonka.CleanRooms.Types.GlueTableReference
import Amazonka.CleanRooms.Types.MemberSpecification
import Amazonka.CleanRooms.Types.MemberSummary
import Amazonka.CleanRooms.Types.Membership
import Amazonka.CleanRooms.Types.MembershipSummary
import Amazonka.CleanRooms.Types.ProtectedQuery
import Amazonka.CleanRooms.Types.ProtectedQueryError
import Amazonka.CleanRooms.Types.ProtectedQueryOutput
import Amazonka.CleanRooms.Types.ProtectedQueryOutputConfiguration
import Amazonka.CleanRooms.Types.ProtectedQueryResult
import Amazonka.CleanRooms.Types.ProtectedQueryResultConfiguration
import Amazonka.CleanRooms.Types.ProtectedQueryS3Output
import Amazonka.CleanRooms.Types.ProtectedQueryS3OutputConfiguration
import Amazonka.CleanRooms.Types.ProtectedQuerySQLParameters
import Amazonka.CleanRooms.Types.ProtectedQueryStatistics
import Amazonka.CleanRooms.Types.ProtectedQuerySummary
import Amazonka.CleanRooms.Types.Schema
import Amazonka.CleanRooms.Types.SchemaSummary
import Amazonka.CleanRooms.Types.TableReference
import Amazonka.CleanRooms.UntagResource
import Amazonka.CleanRooms.UpdateCollaboration
import Amazonka.CleanRooms.UpdateConfiguredTable
import Amazonka.CleanRooms.UpdateConfiguredTableAnalysisRule
import Amazonka.CleanRooms.UpdateConfiguredTableAssociation
import Amazonka.CleanRooms.UpdateMembership
import Amazonka.CleanRooms.UpdateProtectedQuery
