{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types
  ( -- * Service Configuration
    dataPipeline,

    -- * Errors

    -- * OperatorType
    OperatorType (..),

    -- * TaskStatus
    TaskStatus (..),

    -- * Field
    Field,
    field,
    fRefValue,
    fStringValue,
    fKey,

    -- * InstanceIdentity
    InstanceIdentity,
    instanceIdentity,
    iiSignature,
    iiDocument,

    -- * Operator
    Operator,
    operator,
    oValues,
    oType,

    -- * ParameterAttribute
    ParameterAttribute,
    parameterAttribute,
    paKey,
    paStringValue,

    -- * ParameterObject
    ParameterObject,
    parameterObject,
    poId,
    poAttributes,

    -- * ParameterValue
    ParameterValue,
    parameterValue,
    pvId,
    pvStringValue,

    -- * PipelineDescription
    PipelineDescription,
    pipelineDescription,
    pdDescription,
    pdTags,
    pdPipelineId,
    pdName,
    pdFields,

    -- * PipelineIdName
    PipelineIdName,
    pipelineIdName,
    pinName,
    pinId,

    -- * PipelineObject
    PipelineObject,
    pipelineObject,
    pId,
    pName,
    pFields,

    -- * Query
    Query,
    query,
    qSelectors,

    -- * Selector
    Selector,
    selector,
    sOperator,
    sFieldName,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TaskObject
    TaskObject,
    taskObject,
    toPipelineId,
    toAttemptId,
    toTaskId,
    toObjects,

    -- * ValidationError
    ValidationError,
    validationError,
    veId,
    veErrors,

    -- * ValidationWarning
    ValidationWarning,
    validationWarning,
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
dataPipeline :: Service
dataPipeline =
  Service
    { _svcAbbrev = "DataPipeline",
      _svcSigner = v4,
      _svcPrefix = "datapipeline",
      _svcVersion = "2012-10-29",
      _svcEndpoint = defaultEndpoint dataPipeline,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "DataPipeline",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
