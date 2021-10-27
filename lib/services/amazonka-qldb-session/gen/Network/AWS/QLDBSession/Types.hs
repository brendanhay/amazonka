{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.QLDBSession.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QLDBSession.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidSessionException,
    _CapacityExceededException,
    _OccConflictException,
    _RateExceededException,
    _BadRequestException,
    _LimitExceededException,

    -- * AbortTransactionRequest
    AbortTransactionRequest (..),
    newAbortTransactionRequest,

    -- * AbortTransactionResult
    AbortTransactionResult (..),
    newAbortTransactionResult,
    abortTransactionResult_timingInformation,

    -- * CommitTransactionRequest
    CommitTransactionRequest (..),
    newCommitTransactionRequest,
    commitTransactionRequest_transactionId,
    commitTransactionRequest_commitDigest,

    -- * CommitTransactionResult
    CommitTransactionResult (..),
    newCommitTransactionResult,
    commitTransactionResult_timingInformation,
    commitTransactionResult_consumedIOs,
    commitTransactionResult_commitDigest,
    commitTransactionResult_transactionId,

    -- * EndSessionRequest
    EndSessionRequest (..),
    newEndSessionRequest,

    -- * EndSessionResult
    EndSessionResult (..),
    newEndSessionResult,
    endSessionResult_timingInformation,

    -- * ExecuteStatementRequest
    ExecuteStatementRequest (..),
    newExecuteStatementRequest,
    executeStatementRequest_parameters,
    executeStatementRequest_transactionId,
    executeStatementRequest_statement,

    -- * ExecuteStatementResult
    ExecuteStatementResult (..),
    newExecuteStatementResult,
    executeStatementResult_timingInformation,
    executeStatementResult_consumedIOs,
    executeStatementResult_firstPage,

    -- * FetchPageRequest
    FetchPageRequest (..),
    newFetchPageRequest,
    fetchPageRequest_transactionId,
    fetchPageRequest_nextPageToken,

    -- * FetchPageResult
    FetchPageResult (..),
    newFetchPageResult,
    fetchPageResult_timingInformation,
    fetchPageResult_consumedIOs,
    fetchPageResult_page,

    -- * IOUsage
    IOUsage (..),
    newIOUsage,
    iOUsage_readIOs,
    iOUsage_writeIOs,

    -- * Page
    Page (..),
    newPage,
    page_nextPageToken,
    page_values,

    -- * StartSessionRequest
    StartSessionRequest (..),
    newStartSessionRequest,
    startSessionRequest_ledgerName,

    -- * StartSessionResult
    StartSessionResult (..),
    newStartSessionResult,
    startSessionResult_timingInformation,
    startSessionResult_sessionToken,

    -- * StartTransactionRequest
    StartTransactionRequest (..),
    newStartTransactionRequest,

    -- * StartTransactionResult
    StartTransactionResult (..),
    newStartTransactionResult,
    startTransactionResult_timingInformation,
    startTransactionResult_transactionId,

    -- * TimingInformation
    TimingInformation (..),
    newTimingInformation,
    timingInformation_processingTimeMilliseconds,

    -- * ValueHolder
    ValueHolder (..),
    newValueHolder,
    valueHolder_ionText,
    valueHolder_ionBinary,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QLDBSession.Types.AbortTransactionRequest
import Network.AWS.QLDBSession.Types.AbortTransactionResult
import Network.AWS.QLDBSession.Types.CommitTransactionRequest
import Network.AWS.QLDBSession.Types.CommitTransactionResult
import Network.AWS.QLDBSession.Types.EndSessionRequest
import Network.AWS.QLDBSession.Types.EndSessionResult
import Network.AWS.QLDBSession.Types.ExecuteStatementRequest
import Network.AWS.QLDBSession.Types.ExecuteStatementResult
import Network.AWS.QLDBSession.Types.FetchPageRequest
import Network.AWS.QLDBSession.Types.FetchPageResult
import Network.AWS.QLDBSession.Types.IOUsage
import Network.AWS.QLDBSession.Types.Page
import Network.AWS.QLDBSession.Types.StartSessionRequest
import Network.AWS.QLDBSession.Types.StartSessionResult
import Network.AWS.QLDBSession.Types.StartTransactionRequest
import Network.AWS.QLDBSession.Types.StartTransactionResult
import Network.AWS.QLDBSession.Types.TimingInformation
import Network.AWS.QLDBSession.Types.ValueHolder
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2019-07-11@ of the Amazon QLDB Session SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "QLDBSession",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "session.qldb",
      Core._serviceSigningName = "qldb",
      Core._serviceVersion = "2019-07-11",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "QLDBSession",
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
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
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
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned if the session doesn\'t exist anymore because it timed out or
-- expired.
_InvalidSessionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSessionException =
  Core._MatchServiceError
    defaultService
    "InvalidSessionException"

-- | Returned when the request exceeds the processing capacity of the ledger.
_CapacityExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CapacityExceededException =
  Core._MatchServiceError
    defaultService
    "CapacityExceededException"

-- | Returned when a transaction cannot be written to the journal due to a
-- failure in the verification phase of /optimistic concurrency control/
-- (OCC).
_OccConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OccConflictException =
  Core._MatchServiceError
    defaultService
    "OccConflictException"

-- | Returned when the rate of requests exceeds the allowed throughput.
_RateExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RateExceededException =
  Core._MatchServiceError
    defaultService
    "RateExceededException"

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | Returned if a resource limit such as number of active sessions is
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
