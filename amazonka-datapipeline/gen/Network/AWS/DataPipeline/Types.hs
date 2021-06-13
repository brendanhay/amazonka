{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _TaskNotFoundException,
    _InvalidRequestException,
    _PipelineNotFoundException,
    _PipelineDeletedException,
    _InternalServiceError,

    -- * OperatorType
    OperatorType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * Field
    Field (..),
    newField,
    field_stringValue,
    field_refValue,
    field_key,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_document,
    instanceIdentity_signature,

    -- * Operator
    Operator (..),
    newOperator,
    operator_values,
    operator_type,

    -- * ParameterAttribute
    ParameterAttribute (..),
    newParameterAttribute,
    parameterAttribute_key,
    parameterAttribute_stringValue,

    -- * ParameterObject
    ParameterObject (..),
    newParameterObject,
    parameterObject_id,
    parameterObject_attributes,

    -- * ParameterValue
    ParameterValue (..),
    newParameterValue,
    parameterValue_id,
    parameterValue_stringValue,

    -- * PipelineDescription
    PipelineDescription (..),
    newPipelineDescription,
    pipelineDescription_tags,
    pipelineDescription_description,
    pipelineDescription_pipelineId,
    pipelineDescription_name,
    pipelineDescription_fields,

    -- * PipelineIdName
    PipelineIdName (..),
    newPipelineIdName,
    pipelineIdName_id,
    pipelineIdName_name,

    -- * PipelineObject
    PipelineObject (..),
    newPipelineObject,
    pipelineObject_id,
    pipelineObject_name,
    pipelineObject_fields,

    -- * Query
    Query (..),
    newQuery,
    query_selectors,

    -- * Selector
    Selector (..),
    newSelector,
    selector_operator,
    selector_fieldName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TaskObject
    TaskObject (..),
    newTaskObject,
    taskObject_pipelineId,
    taskObject_objects,
    taskObject_taskId,
    taskObject_attemptId,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_id,
    validationError_errors,

    -- * ValidationWarning
    ValidationWarning (..),
    newValidationWarning,
    validationWarning_warnings,
    validationWarning_id,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.InstanceIdentity
import Network.AWS.DataPipeline.Types.Operator
import Network.AWS.DataPipeline.Types.OperatorType
import Network.AWS.DataPipeline.Types.ParameterAttribute
import Network.AWS.DataPipeline.Types.ParameterObject
import Network.AWS.DataPipeline.Types.ParameterValue
import Network.AWS.DataPipeline.Types.PipelineDescription
import Network.AWS.DataPipeline.Types.PipelineIdName
import Network.AWS.DataPipeline.Types.PipelineObject
import Network.AWS.DataPipeline.Types.Query
import Network.AWS.DataPipeline.Types.Selector
import Network.AWS.DataPipeline.Types.Tag
import Network.AWS.DataPipeline.Types.TaskObject
import Network.AWS.DataPipeline.Types.TaskStatus
import Network.AWS.DataPipeline.Types.ValidationError
import Network.AWS.DataPipeline.Types.ValidationWarning
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DataPipeline",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "datapipeline",
      Core._serviceSigningName = "datapipeline",
      Core._serviceVersion = "2012-10-29",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DataPipeline",
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
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
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
      | Prelude.otherwise = Prelude.Nothing

-- | The specified task was not found.
_TaskNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TaskNotFoundException =
  Core._MatchServiceError
    defaultService
    "TaskNotFoundException"

-- | The request was not valid. Verify that your request was properly
-- formatted, that the signature was generated with the correct
-- credentials, and that you haven\'t exceeded any of the service limits
-- for your account.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified pipeline was not found. Verify that you used the correct
-- user and account identifiers.
_PipelineNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PipelineNotFoundException =
  Core._MatchServiceError
    defaultService
    "PipelineNotFoundException"

-- | The specified pipeline has been deleted.
_PipelineDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PipelineDeletedException =
  Core._MatchServiceError
    defaultService
    "PipelineDeletedException"

-- | An internal service error occurred.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
