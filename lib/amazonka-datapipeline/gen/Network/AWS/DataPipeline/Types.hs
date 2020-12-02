{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types
    (
    -- * Service Configuration
      dataPipeline

    -- * Errors
    , _InvalidRequestException
    , _InternalServiceError
    , _PipelineDeletedException
    , _PipelineNotFoundException
    , _TaskNotFoundException

    -- * OperatorType
    , OperatorType (..)

    -- * TaskStatus
    , TaskStatus (..)

    -- * Field
    , Field
    , field
    , fRefValue
    , fStringValue
    , fKey

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- * Operator
    , Operator
    , operator
    , oValues
    , oType

    -- * ParameterAttribute
    , ParameterAttribute
    , parameterAttribute
    , paKey
    , paStringValue

    -- * ParameterObject
    , ParameterObject
    , parameterObject
    , poId
    , poAttributes

    -- * ParameterValue
    , ParameterValue
    , parameterValue
    , pvId
    , pvStringValue

    -- * PipelineDescription
    , PipelineDescription
    , pipelineDescription
    , pdDescription
    , pdTags
    , pdPipelineId
    , pdName
    , pdFields

    -- * PipelineIdName
    , PipelineIdName
    , pipelineIdName
    , pinName
    , pinId

    -- * PipelineObject
    , PipelineObject
    , pipelineObject
    , pId
    , pName
    , pFields

    -- * Query
    , Query
    , query
    , qSelectors

    -- * Selector
    , Selector
    , selector
    , sOperator
    , sFieldName

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TaskObject
    , TaskObject
    , taskObject
    , toPipelineId
    , toAttemptId
    , toTaskId
    , toObjects

    -- * ValidationError
    , ValidationError
    , validationError
    , veId
    , veErrors

    -- * ValidationWarning
    , ValidationWarning
    , validationWarning
    , vwWarnings
    , vwId
    ) where

import Network.AWS.DataPipeline.Types.Product
import Network.AWS.DataPipeline.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2012-10-29@ of the Amazon Data Pipeline SDK configuration.
dataPipeline :: Service
dataPipeline =
  Service
    { _svcAbbrev = "DataPipeline"
    , _svcSigner = v4
    , _svcPrefix = "datapipeline"
    , _svcVersion = "2012-10-29"
    , _svcEndpoint = defaultEndpoint dataPipeline
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "DataPipeline"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The request was not valid. Verify that your request was properly formatted, that the signature was generated with the correct credentials, and that you haven't exceeded any of the service limits for your account.
--
--
_InvalidRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException =
  _MatchServiceError dataPipeline "InvalidRequestException"


-- | An internal service error occurred.
--
--
_InternalServiceError :: AsError a => Getting (First ServiceError) a ServiceError
_InternalServiceError = _MatchServiceError dataPipeline "InternalServiceError"


-- | The specified pipeline has been deleted.
--
--
_PipelineDeletedException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineDeletedException =
  _MatchServiceError dataPipeline "PipelineDeletedException"


-- | The specified pipeline was not found. Verify that you used the correct user and account identifiers.
--
--
_PipelineNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_PipelineNotFoundException =
  _MatchServiceError dataPipeline "PipelineNotFoundException"


-- | The specified task was not found.
--
--
_TaskNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_TaskNotFoundException = _MatchServiceError dataPipeline "TaskNotFoundException"

