-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _InvalidRequestException,
    _InternalServiceError,
    _PipelineDeletedException,
    _PipelineNotFoundException,
    _TaskNotFoundException,

    -- * ParameterObject
    ParameterObject (..),
    mkParameterObject,
    poId,
    poAttributes,

    -- * PipelineObject
    PipelineObject (..),
    mkPipelineObject,
    pId,
    pName,
    pFields,

    -- * LongString
    LongString (..),

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * Field
    Field (..),
    mkField,
    fKey,
    fRefValue,
    fStringValue,

    -- * AttributeValueString
    AttributeValueString (..),

    -- * ParameterValue
    ParameterValue (..),
    mkParameterValue,
    pvId,
    pvStringValue,

    -- * Selector
    Selector (..),
    mkSelector,
    sFieldName,
    sOperator,

    -- * ParameterAttribute
    ParameterAttribute (..),
    mkParameterAttribute,
    paKey,
    paStringValue,

    -- * String
    String (..),

    -- * TaskId
    TaskId (..),

    -- * Operator
    Operator (..),
    mkOperator,
    oType,
    oValues,

    -- * TaskObject
    TaskObject (..),
    mkTaskObject,
    toAttemptId,
    toObjects,
    toPipelineId,
    toTaskId,

    -- * ValidationError
    ValidationError (..),
    mkValidationError,
    veErrors,
    veId,

    -- * PipelineDescription
    PipelineDescription (..),
    mkPipelineDescription,
    pdPipelineId,
    pdName,
    pdFields,
    pdDescription,
    pdTags,

    -- * InstanceIdentity
    InstanceIdentity (..),
    mkInstanceIdentity,
    iiDocument,
    iiSignature,

    -- * Query
    Query (..),
    mkQuery,
    qSelectors,

    -- * Id
    Id (..),

    -- * ValidationMessage
    ValidationMessage (..),

    -- * OperatorType
    OperatorType (..),

    -- * PipelineIdName
    PipelineIdName (..),
    mkPipelineIdName,
    pinId,
    pinName,

    -- * TaskStatus
    TaskStatus (..),

    -- * ValidationWarning
    ValidationWarning (..),
    mkValidationWarning,
    vwId,
    vwWarnings,

    -- * Id
    Id (..),

    -- * Marker
    Marker (..),

    -- * PipelineId
    PipelineId (..),

    -- * Name
    Name (..),

    -- * ObjectId
    ObjectId (..),

    -- * Version
    Version (..),

    -- * WorkerGroup
    WorkerGroup (..),

    -- * Hostname
    Hostname (..),

    -- * Key
    Key (..),

    -- * Value
    Value (..),

    -- * RefValue
    RefValue (..),

    -- * StringValue
    StringValue (..),

    -- * FieldName
    FieldName (..),

    -- * AttemptId
    AttemptId (..),

    -- * ErrorMessage
    ErrorMessage (..),
  )
where

import Network.AWS.DataPipeline.Types.AttemptId
import Network.AWS.DataPipeline.Types.AttributeValueString
import Network.AWS.DataPipeline.Types.ErrorMessage
import Network.AWS.DataPipeline.Types.Field
import Network.AWS.DataPipeline.Types.FieldName
import Network.AWS.DataPipeline.Types.Hostname
import Network.AWS.DataPipeline.Types.Id
import Network.AWS.DataPipeline.Types.InstanceIdentity
import Network.AWS.DataPipeline.Types.Key
import Network.AWS.DataPipeline.Types.LongString
import Network.AWS.DataPipeline.Types.Marker
import Network.AWS.DataPipeline.Types.Name
import Network.AWS.DataPipeline.Types.ObjectId
import Network.AWS.DataPipeline.Types.Operator
import Network.AWS.DataPipeline.Types.OperatorType
import Network.AWS.DataPipeline.Types.ParameterAttribute
import Network.AWS.DataPipeline.Types.ParameterObject
import Network.AWS.DataPipeline.Types.ParameterValue
import Network.AWS.DataPipeline.Types.PipelineDescription
import Network.AWS.DataPipeline.Types.PipelineId
import Network.AWS.DataPipeline.Types.PipelineIdName
import Network.AWS.DataPipeline.Types.PipelineObject
import Network.AWS.DataPipeline.Types.Query
import Network.AWS.DataPipeline.Types.RefValue
import Network.AWS.DataPipeline.Types.Selector
import Network.AWS.DataPipeline.Types.String
import Network.AWS.DataPipeline.Types.StringValue
import Network.AWS.DataPipeline.Types.Tag
import Network.AWS.DataPipeline.Types.TaskId
import Network.AWS.DataPipeline.Types.TaskObject
import Network.AWS.DataPipeline.Types.TaskStatus
import Network.AWS.DataPipeline.Types.ValidationError
import Network.AWS.DataPipeline.Types.ValidationMessage
import Network.AWS.DataPipeline.Types.ValidationWarning
import Network.AWS.DataPipeline.Types.Value
import Network.AWS.DataPipeline.Types.Version
import Network.AWS.DataPipeline.Types.WorkerGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "DataPipeline",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "datapipeline",
      Core._svcVersion = "2012-10-29",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "DataPipeline",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The request was not valid. Verify that your request was properly formatted, that the signature was generated with the correct credentials, and that you haven't exceeded any of the service limits for your account.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError mkServiceConfig "InvalidRequestException"
{-# DEPRECATED _InvalidRequestException "Use generic-lens or generic-optics instead." #-}

-- | An internal service error occurred.
_InternalServiceError :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError mkServiceConfig "InternalServiceError"
{-# DEPRECATED _InternalServiceError "Use generic-lens or generic-optics instead." #-}

-- | The specified pipeline has been deleted.
_PipelineDeletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineDeletedException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineDeletedException"
{-# DEPRECATED _PipelineDeletedException "Use generic-lens or generic-optics instead." #-}

-- | The specified pipeline was not found. Verify that you used the correct user and account identifiers.
_PipelineNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PipelineNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "PipelineNotFoundException"
{-# DEPRECATED _PipelineNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The specified task was not found.
_TaskNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TaskNotFoundException =
  Core._MatchServiceError mkServiceConfig "TaskNotFoundException"
{-# DEPRECATED _TaskNotFoundException "Use generic-lens or generic-optics instead." #-}
