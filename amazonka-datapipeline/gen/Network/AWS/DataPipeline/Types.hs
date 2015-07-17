{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DataPipeline.Types
    (
    -- * Service
      DataPipeline

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
    , fieRefValue
    , fieStringValue
    , fieKey

    -- * InstanceIdentity
    , InstanceIdentity
    , instanceIdentity
    , iiSignature
    , iiDocument

    -- * Operator
    , Operator
    , operator
    , opeValues
    , opeType

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
    , pipId
    , pipName
    , pipFields

    -- * Query
    , Query
    , query
    , queSelectors

    -- * Selector
    , Selector
    , selector
    , selOperator
    , selFieldName

    -- * Tag
    , Tag
    , tag
    , tagKey
    , tagValue

    -- * TaskObject
    , TaskObject
    , taskObject
    , toPipelineId
    , toTaskId
    , toAttemptId
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

import           Network.AWS.DataPipeline.Types.Product
import           Network.AWS.DataPipeline.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2012-10-29@ of the Amazon Data Pipeline SDK.
data DataPipeline

instance AWSService DataPipeline where
    type Sg DataPipeline = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "DataPipeline"
            , _svcPrefix = "datapipeline"
            , _svcVersion = "2012-10-29"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = Just 70000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The request was not valid. Verify that your request was properly
-- formatted, that the signature was generated with the correct
-- credentials, and that you haven\'t exceeded any of the service limits
-- for your account.
_InvalidRequestException :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidRequestException = _ServiceError . hasCode "InvalidRequestException"

-- | An internal service error occurred.
_InternalServiceError :: AWSError a => Getting (First ServiceError) a ServiceError
_InternalServiceError = _ServiceError . hasCode "InternalServiceError"

-- | The specified pipeline has been deleted.
_PipelineDeletedException :: AWSError a => Getting (First ServiceError) a ServiceError
_PipelineDeletedException = _ServiceError . hasCode "PipelineDeletedException"

-- | The specified pipeline was not found. Verify that you used the correct
-- user and account identifiers.
_PipelineNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_PipelineNotFoundException =
    _ServiceError . hasCode "PipelineNotFoundException"

-- | The specified task was not found.
_TaskNotFoundException :: AWSError a => Getting (First ServiceError) a ServiceError
_TaskNotFoundException = _ServiceError . hasCode "TaskNotFoundException"
