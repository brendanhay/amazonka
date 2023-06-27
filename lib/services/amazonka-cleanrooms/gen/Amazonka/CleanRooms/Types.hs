{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CleanRooms.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * AggregateFunctionName
    AggregateFunctionName (..),

    -- * AggregationType
    AggregationType (..),

    -- * AnalysisMethod
    AnalysisMethod (..),

    -- * AnalysisRuleType
    AnalysisRuleType (..),

    -- * CollaborationQueryLogStatus
    CollaborationQueryLogStatus (..),

    -- * ConfiguredTableAnalysisRuleType
    ConfiguredTableAnalysisRuleType (..),

    -- * FilterableMemberStatus
    FilterableMemberStatus (..),

    -- * JoinRequiredOption
    JoinRequiredOption (..),

    -- * MemberAbility
    MemberAbility (..),

    -- * MemberStatus
    MemberStatus (..),

    -- * MembershipQueryLogStatus
    MembershipQueryLogStatus (..),

    -- * MembershipStatus
    MembershipStatus (..),

    -- * ProtectedQueryStatus
    ProtectedQueryStatus (..),

    -- * ProtectedQueryType
    ProtectedQueryType (..),

    -- * ResultFormat
    ResultFormat (..),

    -- * ScalarFunctions
    ScalarFunctions (..),

    -- * SchemaType
    SchemaType (..),

    -- * TargetProtectedQueryStatus
    TargetProtectedQueryStatus (..),

    -- * AggregateColumn
    AggregateColumn (..),
    newAggregateColumn,
    aggregateColumn_columnNames,
    aggregateColumn_function,

    -- * AggregationConstraint
    AggregationConstraint (..),
    newAggregationConstraint,
    aggregationConstraint_columnName,
    aggregationConstraint_minimum,
    aggregationConstraint_type,

    -- * AnalysisRule
    AnalysisRule (..),
    newAnalysisRule,
    analysisRule_collaborationId,
    analysisRule_type,
    analysisRule_name,
    analysisRule_createTime,
    analysisRule_updateTime,
    analysisRule_policy,

    -- * AnalysisRuleAggregation
    AnalysisRuleAggregation (..),
    newAnalysisRuleAggregation,
    analysisRuleAggregation_joinRequired,
    analysisRuleAggregation_aggregateColumns,
    analysisRuleAggregation_joinColumns,
    analysisRuleAggregation_dimensionColumns,
    analysisRuleAggregation_scalarFunctions,
    analysisRuleAggregation_outputConstraints,

    -- * AnalysisRuleList
    AnalysisRuleList (..),
    newAnalysisRuleList,
    analysisRuleList_joinColumns,
    analysisRuleList_listColumns,

    -- * AnalysisRulePolicy
    AnalysisRulePolicy (..),
    newAnalysisRulePolicy,
    analysisRulePolicy_v1,

    -- * AnalysisRulePolicyV1
    AnalysisRulePolicyV1 (..),
    newAnalysisRulePolicyV1,
    analysisRulePolicyV1_aggregation,
    analysisRulePolicyV1_list,

    -- * BatchGetSchemaError
    BatchGetSchemaError (..),
    newBatchGetSchemaError,
    batchGetSchemaError_name,
    batchGetSchemaError_code,
    batchGetSchemaError_message,

    -- * Collaboration
    Collaboration (..),
    newCollaboration,
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

    -- * CollaborationSummary
    CollaborationSummary (..),
    newCollaborationSummary,
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

    -- * Column
    Column (..),
    newColumn,
    column_name,
    column_type,

    -- * ConfiguredTable
    ConfiguredTable (..),
    newConfiguredTable,
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

    -- * ConfiguredTableAnalysisRule
    ConfiguredTableAnalysisRule (..),
    newConfiguredTableAnalysisRule,
    configuredTableAnalysisRule_configuredTableId,
    configuredTableAnalysisRule_configuredTableArn,
    configuredTableAnalysisRule_policy,
    configuredTableAnalysisRule_type,
    configuredTableAnalysisRule_createTime,
    configuredTableAnalysisRule_updateTime,

    -- * ConfiguredTableAnalysisRulePolicy
    ConfiguredTableAnalysisRulePolicy (..),
    newConfiguredTableAnalysisRulePolicy,
    configuredTableAnalysisRulePolicy_v1,

    -- * ConfiguredTableAnalysisRulePolicyV1
    ConfiguredTableAnalysisRulePolicyV1 (..),
    newConfiguredTableAnalysisRulePolicyV1,
    configuredTableAnalysisRulePolicyV1_aggregation,
    configuredTableAnalysisRulePolicyV1_list,

    -- * ConfiguredTableAssociation
    ConfiguredTableAssociation (..),
    newConfiguredTableAssociation,
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

    -- * ConfiguredTableAssociationSummary
    ConfiguredTableAssociationSummary (..),
    newConfiguredTableAssociationSummary,
    configuredTableAssociationSummary_configuredTableId,
    configuredTableAssociationSummary_membershipId,
    configuredTableAssociationSummary_membershipArn,
    configuredTableAssociationSummary_name,
    configuredTableAssociationSummary_createTime,
    configuredTableAssociationSummary_updateTime,
    configuredTableAssociationSummary_id,
    configuredTableAssociationSummary_arn,

    -- * ConfiguredTableSummary
    ConfiguredTableSummary (..),
    newConfiguredTableSummary,
    configuredTableSummary_id,
    configuredTableSummary_arn,
    configuredTableSummary_name,
    configuredTableSummary_createTime,
    configuredTableSummary_updateTime,
    configuredTableSummary_analysisRuleTypes,
    configuredTableSummary_analysisMethod,

    -- * DataEncryptionMetadata
    DataEncryptionMetadata (..),
    newDataEncryptionMetadata,
    dataEncryptionMetadata_allowCleartext,
    dataEncryptionMetadata_allowDuplicates,
    dataEncryptionMetadata_allowJoinsOnColumnsWithDifferentNames,
    dataEncryptionMetadata_preserveNulls,

    -- * GlueTableReference
    GlueTableReference (..),
    newGlueTableReference,
    glueTableReference_tableName,
    glueTableReference_databaseName,

    -- * MemberSpecification
    MemberSpecification (..),
    newMemberSpecification,
    memberSpecification_accountId,
    memberSpecification_memberAbilities,
    memberSpecification_displayName,

    -- * MemberSummary
    MemberSummary (..),
    newMemberSummary,
    memberSummary_membershipArn,
    memberSummary_membershipId,
    memberSummary_accountId,
    memberSummary_status,
    memberSummary_displayName,
    memberSummary_abilities,
    memberSummary_createTime,
    memberSummary_updateTime,

    -- * Membership
    Membership (..),
    newMembership,
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

    -- * MembershipSummary
    MembershipSummary (..),
    newMembershipSummary,
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

    -- * ProtectedQuery
    ProtectedQuery (..),
    newProtectedQuery,
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

    -- * ProtectedQueryError
    ProtectedQueryError (..),
    newProtectedQueryError,
    protectedQueryError_message,
    protectedQueryError_code,

    -- * ProtectedQueryOutput
    ProtectedQueryOutput (..),
    newProtectedQueryOutput,
    protectedQueryOutput_s3,

    -- * ProtectedQueryOutputConfiguration
    ProtectedQueryOutputConfiguration (..),
    newProtectedQueryOutputConfiguration,
    protectedQueryOutputConfiguration_s3,

    -- * ProtectedQueryResult
    ProtectedQueryResult (..),
    newProtectedQueryResult,
    protectedQueryResult_output,

    -- * ProtectedQueryResultConfiguration
    ProtectedQueryResultConfiguration (..),
    newProtectedQueryResultConfiguration,
    protectedQueryResultConfiguration_outputConfiguration,

    -- * ProtectedQueryS3Output
    ProtectedQueryS3Output (..),
    newProtectedQueryS3Output,
    protectedQueryS3Output_location,

    -- * ProtectedQueryS3OutputConfiguration
    ProtectedQueryS3OutputConfiguration (..),
    newProtectedQueryS3OutputConfiguration,
    protectedQueryS3OutputConfiguration_keyPrefix,
    protectedQueryS3OutputConfiguration_resultFormat,
    protectedQueryS3OutputConfiguration_bucket,

    -- * ProtectedQuerySQLParameters
    ProtectedQuerySQLParameters (..),
    newProtectedQuerySQLParameters,
    protectedQuerySQLParameters_queryString,

    -- * ProtectedQueryStatistics
    ProtectedQueryStatistics (..),
    newProtectedQueryStatistics,
    protectedQueryStatistics_totalDurationInMillis,

    -- * ProtectedQuerySummary
    ProtectedQuerySummary (..),
    newProtectedQuerySummary,
    protectedQuerySummary_id,
    protectedQuerySummary_membershipId,
    protectedQuerySummary_membershipArn,
    protectedQuerySummary_createTime,
    protectedQuerySummary_status,

    -- * Schema
    Schema (..),
    newSchema,
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

    -- * SchemaSummary
    SchemaSummary (..),
    newSchemaSummary,
    schemaSummary_analysisMethod,
    schemaSummary_name,
    schemaSummary_type,
    schemaSummary_creatorAccountId,
    schemaSummary_createTime,
    schemaSummary_updateTime,
    schemaSummary_collaborationId,
    schemaSummary_collaborationArn,
    schemaSummary_analysisRuleTypes,

    -- * TableReference
    TableReference (..),
    newTableReference,
    tableReference_glue,
  )
where

import Amazonka.CleanRooms.Types.AggregateColumn
import Amazonka.CleanRooms.Types.AggregateFunctionName
import Amazonka.CleanRooms.Types.AggregationConstraint
import Amazonka.CleanRooms.Types.AggregationType
import Amazonka.CleanRooms.Types.AnalysisMethod
import Amazonka.CleanRooms.Types.AnalysisRule
import Amazonka.CleanRooms.Types.AnalysisRuleAggregation
import Amazonka.CleanRooms.Types.AnalysisRuleList
import Amazonka.CleanRooms.Types.AnalysisRulePolicy
import Amazonka.CleanRooms.Types.AnalysisRulePolicyV1
import Amazonka.CleanRooms.Types.AnalysisRuleType
import Amazonka.CleanRooms.Types.BatchGetSchemaError
import Amazonka.CleanRooms.Types.Collaboration
import Amazonka.CleanRooms.Types.CollaborationQueryLogStatus
import Amazonka.CleanRooms.Types.CollaborationSummary
import Amazonka.CleanRooms.Types.Column
import Amazonka.CleanRooms.Types.ConfiguredTable
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRule
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicy
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRulePolicyV1
import Amazonka.CleanRooms.Types.ConfiguredTableAnalysisRuleType
import Amazonka.CleanRooms.Types.ConfiguredTableAssociation
import Amazonka.CleanRooms.Types.ConfiguredTableAssociationSummary
import Amazonka.CleanRooms.Types.ConfiguredTableSummary
import Amazonka.CleanRooms.Types.DataEncryptionMetadata
import Amazonka.CleanRooms.Types.FilterableMemberStatus
import Amazonka.CleanRooms.Types.GlueTableReference
import Amazonka.CleanRooms.Types.JoinRequiredOption
import Amazonka.CleanRooms.Types.MemberAbility
import Amazonka.CleanRooms.Types.MemberSpecification
import Amazonka.CleanRooms.Types.MemberStatus
import Amazonka.CleanRooms.Types.MemberSummary
import Amazonka.CleanRooms.Types.Membership
import Amazonka.CleanRooms.Types.MembershipQueryLogStatus
import Amazonka.CleanRooms.Types.MembershipStatus
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
import Amazonka.CleanRooms.Types.ProtectedQueryStatus
import Amazonka.CleanRooms.Types.ProtectedQuerySummary
import Amazonka.CleanRooms.Types.ProtectedQueryType
import Amazonka.CleanRooms.Types.ResultFormat
import Amazonka.CleanRooms.Types.ScalarFunctions
import Amazonka.CleanRooms.Types.Schema
import Amazonka.CleanRooms.Types.SchemaSummary
import Amazonka.CleanRooms.Types.SchemaType
import Amazonka.CleanRooms.Types.TableReference
import Amazonka.CleanRooms.Types.TargetProtectedQueryStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2022-02-17@ of the Amazon Clean Rooms Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CleanRooms",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "cleanrooms",
      Core.signingName = "cleanrooms",
      Core.version = "2022-02-17",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CleanRooms",
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

-- | Caller does not have sufficient access to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Unexpected error during processing of request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Request references a resource which does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Request denied because service quota has been exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request was denied due to request throttling.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the specified constraints.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
