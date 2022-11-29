{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QLDBSession.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _OccConflictException,
    _LimitExceededException,
    _CapacityExceededException,
    _BadRequestException,
    _RateExceededException,
    _InvalidSessionException,

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
    commitTransactionResult_commitDigest,
    commitTransactionResult_timingInformation,
    commitTransactionResult_consumedIOs,
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
    valueHolder_ionBinary,
    valueHolder_ionText,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.AbortTransactionRequest
import Amazonka.QLDBSession.Types.AbortTransactionResult
import Amazonka.QLDBSession.Types.CommitTransactionRequest
import Amazonka.QLDBSession.Types.CommitTransactionResult
import Amazonka.QLDBSession.Types.EndSessionRequest
import Amazonka.QLDBSession.Types.EndSessionResult
import Amazonka.QLDBSession.Types.ExecuteStatementRequest
import Amazonka.QLDBSession.Types.ExecuteStatementResult
import Amazonka.QLDBSession.Types.FetchPageRequest
import Amazonka.QLDBSession.Types.FetchPageResult
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.Page
import Amazonka.QLDBSession.Types.StartSessionRequest
import Amazonka.QLDBSession.Types.StartSessionResult
import Amazonka.QLDBSession.Types.StartTransactionRequest
import Amazonka.QLDBSession.Types.StartTransactionResult
import Amazonka.QLDBSession.Types.TimingInformation
import Amazonka.QLDBSession.Types.ValueHolder
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-07-11@ of the Amazon QLDB Session SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "QLDBSession",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "session.qldb",
      Core.signingName = "qldb",
      Core.version = "2019-07-11",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "QLDBSession",
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
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Returned when a transaction cannot be written to the journal due to a
-- failure in the verification phase of /optimistic concurrency control/
-- (OCC).
_OccConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OccConflictException =
  Core._MatchServiceError
    defaultService
    "OccConflictException"

-- | Returned if a resource limit such as number of active sessions is
-- exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Returned when the request exceeds the processing capacity of the ledger.
_CapacityExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CapacityExceededException =
  Core._MatchServiceError
    defaultService
    "CapacityExceededException"

-- | Returned if the request is malformed or contains an error such as an
-- invalid parameter value or a missing required parameter.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"

-- | Returned when the rate of requests exceeds the allowed throughput.
_RateExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RateExceededException =
  Core._MatchServiceError
    defaultService
    "RateExceededException"

-- | Returned if the session doesn\'t exist anymore because it timed out or
-- expired.
_InvalidSessionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSessionException =
  Core._MatchServiceError
    defaultService
    "InvalidSessionException"
