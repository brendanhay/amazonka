{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lex.Types
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lex.Types
    (
    -- * Service Configuration
      lex

    -- * Errors
    , _DependencyFailedException
    , _ConflictException
    , _NotFoundException
    , _LoopDetectedException
    , _InternalFailureException
    , _BadGatewayException
    , _BadRequestException
    , _LimitExceededException

    -- * ContentType
    , ContentType (..)

    -- * DialogState
    , DialogState (..)

    -- * Button
    , Button
    , button
    , bText
    , bValue

    -- * GenericAttachment
    , GenericAttachment
    , genericAttachment
    , gaButtons
    , gaSubTitle
    , gaImageURL
    , gaAttachmentLinkURL
    , gaTitle

    -- * ResponseCard
    , ResponseCard
    , responseCard
    , rcGenericAttachments
    , rcVersion
    , rcContentType
    ) where

import           Network.AWS.Lens
import           Network.AWS.Lex.Types.Product
import           Network.AWS.Lex.Types.Sum
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | API version @2016-11-28@ of the Amazon Lex Runtime Service SDK configuration.
lex :: Service
lex =
    Service
    { _svcAbbrev = "Lex"
    , _svcSigner = v4
    , _svcPrefix = "runtime.lex"
    , _svcVersion = "2016-11-28"
    , _svcEndpoint = defaultEndpoint lex
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "Lex"
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
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
          Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing

-- | One of the downstream dependencies, such as AWS Lambda or Amazon Polly, threw an exception. For example, if Amazon Lex does not have sufficient permissions to call a Lambda function which results in AWS Lambda throwing an exception.
--
--
_DependencyFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_DependencyFailedException =
    _MatchServiceError lex "DependencyFailedException" . hasStatus 424

-- | Two clients are using the same AWS account, Amazon Lex bot, and user ID.
--
--
_ConflictException :: AsError a => Getting (First ServiceError) a ServiceError
_ConflictException = _MatchServiceError lex "ConflictException" . hasStatus 409

-- | Resource (such as the Amazon Lex bot or an alias) referred is not found.
--
--
_NotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_NotFoundException = _MatchServiceError lex "NotFoundException" . hasStatus 404

-- | Lambda fulfilment function returned @DelegateDialogAction@ to Amazon Lex without changing any slot values.
--
--
_LoopDetectedException :: AsError a => Getting (First ServiceError) a ServiceError
_LoopDetectedException =
    _MatchServiceError lex "LoopDetectedException" . hasStatus 508

-- | Internal service error. Retry the call.
--
--
_InternalFailureException :: AsError a => Getting (First ServiceError) a ServiceError
_InternalFailureException =
    _MatchServiceError lex "InternalFailureException" . hasStatus 500

-- | Either the Amazon Lex bot is still building, or one of the dependent services (Amazon Polly, AWS Lambda) failed with an internal service error.
--
--
_BadGatewayException :: AsError a => Getting (First ServiceError) a ServiceError
_BadGatewayException =
    _MatchServiceError lex "BadGatewayException" . hasStatus 502

-- | Request validation failed, there is no usable message in the context, or the bot build failed.
--
--
_BadRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_BadRequestException =
    _MatchServiceError lex "BadRequestException" . hasStatus 400

-- | Prism for LimitExceededException' errors.
_LimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_LimitExceededException =
    _MatchServiceError lex "LimitExceededException" . hasStatus 429
