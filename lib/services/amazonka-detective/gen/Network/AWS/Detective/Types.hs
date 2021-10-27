{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Detective.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Detective.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * MemberDisabledReason
    MemberDisabledReason (..),

    -- * MemberStatus
    MemberStatus (..),

    -- * Account
    Account (..),
    newAccount,
    account_accountId,
    account_emailAddress,

    -- * Graph
    Graph (..),
    newGraph,
    graph_arn,
    graph_createdTime,

    -- * MemberDetail
    MemberDetail (..),
    newMemberDetail,
    memberDetail_percentOfGraphUtilizationUpdatedTime,
    memberDetail_status,
    memberDetail_invitedTime,
    memberDetail_administratorId,
    memberDetail_graphArn,
    memberDetail_masterId,
    memberDetail_accountId,
    memberDetail_disabledReason,
    memberDetail_percentOfGraphUtilization,
    memberDetail_emailAddress,
    memberDetail_volumeUsageUpdatedTime,
    memberDetail_updatedTime,
    memberDetail_volumeUsageInBytes,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_reason,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Detective.Types.Account
import Network.AWS.Detective.Types.Graph
import Network.AWS.Detective.Types.MemberDetail
import Network.AWS.Detective.Types.MemberDisabledReason
import Network.AWS.Detective.Types.MemberStatus
import Network.AWS.Detective.Types.UnprocessedAccount
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-10-26@ of the Amazon Detective SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Detective",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.detective",
      Core._serviceSigningName = "detective",
      Core._serviceVersion = "2018-10-26",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Detective",
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

-- | The request parameters are invalid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | The request attempted an invalid action.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | This request cannot be completed for one of the following reasons.
--
-- -   The request would cause the number of member accounts in the
--     behavior graph to exceed the maximum allowed. A behavior graph
--     cannot have more than 1000 member accounts.
--
-- -   The request would cause the data rate for the behavior graph to
--     exceed the maximum allowed.
--
-- -   Detective is unable to verify the data rate for the member account.
--     This is usually because the member account is not enrolled in Amazon
--     GuardDuty.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was valid but failed because of a problem with the service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request refers to a nonexistent resource.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
