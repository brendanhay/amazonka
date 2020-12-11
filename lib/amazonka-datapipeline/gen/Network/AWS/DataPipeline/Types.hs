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
    dataPipelineService,

    -- * Errors

    -- * OperatorType
    OperatorType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * Field
    Field (..),
    mkField,
    fRefValue,
    fStringValue,
    fKey,

    -- * InstanceIdentity
    InstanceIdentity (..),
    mkInstanceIdentity,
    iiSignature,
    iiDocument,

    -- * Operator
    Operator (..),
    mkOperator,
    oValues,
    oType,

    -- * ParameterAttribute
    ParameterAttribute (..),
    mkParameterAttribute,
    paKey,
    paStringValue,

    -- * ParameterObject
    ParameterObject (..),
    mkParameterObject,
    poId,
    poAttributes,

    -- * ParameterValue
    ParameterValue (..),
    mkParameterValue,
    pvId,
    pvStringValue,

    -- * PipelineDescription
    PipelineDescription (..),
    mkPipelineDescription,
    pdDescription,
    pdTags,
    pdPipelineId,
    pdName,
    pdFields,

    -- * PipelineIdName
    PipelineIdName (..),
    mkPipelineIdName,
    pinName,
    pinId,

    -- * PipelineObject
    PipelineObject (..),
    mkPipelineObject,
    pId,
    pName,
    pFields,

    -- * Query
    Query (..),
    mkQuery,
    qSelectors,

    -- * Selector
    Selector (..),
    mkSelector,
    sOperator,
    sFieldName,

    -- * Tag
    Tag (..),
    mkTag,
    tKey,
    tValue,

    -- * TaskObject
    TaskObject (..),
    mkTaskObject,
    toPipelineId,
    toAttemptId,
    toTaskId,
    toObjects,

    -- * ValidationError
    ValidationError (..),
    mkValidationError,
    veId,
    veErrors,

    -- * ValidationWarning
    ValidationWarning (..),
    mkValidationWarning,
    vwWarnings,
    vwId,
  )
where

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
dataPipelineService :: Lude.Service
dataPipelineService =
  Lude.Service
    { Lude._svcAbbrev = "DataPipeline",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "datapipeline",
      Lude._svcVersion = "2012-10-29",
      Lude._svcEndpoint = Lude.defaultEndpoint dataPipelineService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "DataPipeline",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
