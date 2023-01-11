{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataPipeline.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataPipeline.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServiceError,
    _InvalidRequestException,
    _PipelineDeletedException,
    _PipelineNotFoundException,
    _TaskNotFoundException,

    -- * OperatorType
    OperatorType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * Field
    Field (..),
    newField,
    field_refValue,
    field_stringValue,
    field_key,

    -- * InstanceIdentity
    InstanceIdentity (..),
    newInstanceIdentity,
    instanceIdentity_document,
    instanceIdentity_signature,

    -- * Operator
    Operator (..),
    newOperator,
    operator_type,
    operator_values,

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
    pipelineDescription_description,
    pipelineDescription_tags,
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
    selector_fieldName,
    selector_operator,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TaskObject
    TaskObject (..),
    newTaskObject,
    taskObject_attemptId,
    taskObject_objects,
    taskObject_pipelineId,
    taskObject_taskId,

    -- * ValidationError
    ValidationError (..),
    newValidationError,
    validationError_errors,
    validationError_id,

    -- * ValidationWarning
    ValidationWarning (..),
    newValidationWarning,
    validationWarning_id,
    validationWarning_warnings,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataPipeline.Types.Field
import Amazonka.DataPipeline.Types.InstanceIdentity
import Amazonka.DataPipeline.Types.Operator
import Amazonka.DataPipeline.Types.OperatorType
import Amazonka.DataPipeline.Types.ParameterAttribute
import Amazonka.DataPipeline.Types.ParameterObject
import Amazonka.DataPipeline.Types.ParameterValue
import Amazonka.DataPipeline.Types.PipelineDescription
import Amazonka.DataPipeline.Types.PipelineIdName
import Amazonka.DataPipeline.Types.PipelineObject
import Amazonka.DataPipeline.Types.Query
import Amazonka.DataPipeline.Types.Selector
import Amazonka.DataPipeline.Types.Tag
import Amazonka.DataPipeline.Types.TaskObject
import Amazonka.DataPipeline.Types.TaskStatus
import Amazonka.DataPipeline.Types.ValidationError
import Amazonka.DataPipeline.Types.ValidationWarning
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DataPipeline",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "datapipeline",
      Core.signingName = "datapipeline",
      Core.version = "2012-10-29",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DataPipeline",
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

-- | An internal service error occurred.
_InternalServiceError :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"

-- | The request was not valid. Verify that your request was properly
-- formatted, that the signature was generated with the correct
-- credentials, and that you haven\'t exceeded any of the service limits
-- for your account.
_InvalidRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | The specified pipeline has been deleted.
_PipelineDeletedException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineDeletedException =
  Core._MatchServiceError
    defaultService
    "PipelineDeletedException"

-- | The specified pipeline was not found. Verify that you used the correct
-- user and account identifiers.
_PipelineNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_PipelineNotFoundException =
  Core._MatchServiceError
    defaultService
    "PipelineNotFoundException"

-- | The specified task was not found.
_TaskNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TaskNotFoundException =
  Core._MatchServiceError
    defaultService
    "TaskNotFoundException"
