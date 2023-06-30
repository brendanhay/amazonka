{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Detective.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Detective.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _TooManyRequestsException,
    _ValidationException,

    -- * DatasourcePackage
    DatasourcePackage (..),

    -- * DatasourcePackageIngestState
    DatasourcePackageIngestState (..),

    -- * InvitationType
    InvitationType (..),

    -- * MemberDisabledReason
    MemberDisabledReason (..),

    -- * MemberStatus
    MemberStatus (..),

    -- * Account
    Account (..),
    newAccount,
    account_accountId,
    account_emailAddress,

    -- * Administrator
    Administrator (..),
    newAdministrator,
    administrator_accountId,
    administrator_delegationTime,
    administrator_graphArn,

    -- * DatasourcePackageIngestDetail
    DatasourcePackageIngestDetail (..),
    newDatasourcePackageIngestDetail,
    datasourcePackageIngestDetail_datasourcePackageIngestState,
    datasourcePackageIngestDetail_lastIngestStateChange,

    -- * DatasourcePackageUsageInfo
    DatasourcePackageUsageInfo (..),
    newDatasourcePackageUsageInfo,
    datasourcePackageUsageInfo_volumeUsageInBytes,
    datasourcePackageUsageInfo_volumeUsageUpdateTime,

    -- * Graph
    Graph (..),
    newGraph,
    graph_arn,
    graph_createdTime,

    -- * MemberDetail
    MemberDetail (..),
    newMemberDetail,
    memberDetail_accountId,
    memberDetail_administratorId,
    memberDetail_datasourcePackageIngestStates,
    memberDetail_disabledReason,
    memberDetail_emailAddress,
    memberDetail_graphArn,
    memberDetail_invitationType,
    memberDetail_invitedTime,
    memberDetail_masterId,
    memberDetail_percentOfGraphUtilization,
    memberDetail_percentOfGraphUtilizationUpdatedTime,
    memberDetail_status,
    memberDetail_updatedTime,
    memberDetail_volumeUsageByDatasourcePackage,
    memberDetail_volumeUsageInBytes,
    memberDetail_volumeUsageUpdatedTime,

    -- * MembershipDatasources
    MembershipDatasources (..),
    newMembershipDatasources,
    membershipDatasources_accountId,
    membershipDatasources_datasourcePackageIngestHistory,
    membershipDatasources_graphArn,

    -- * TimestampForCollection
    TimestampForCollection (..),
    newTimestampForCollection,
    timestampForCollection_timestamp,

    -- * UnprocessedAccount
    UnprocessedAccount (..),
    newUnprocessedAccount,
    unprocessedAccount_accountId,
    unprocessedAccount_reason,

    -- * UnprocessedGraph
    UnprocessedGraph (..),
    newUnprocessedGraph,
    unprocessedGraph_graphArn,
    unprocessedGraph_reason,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Detective.Types.Account
import Amazonka.Detective.Types.Administrator
import Amazonka.Detective.Types.DatasourcePackage
import Amazonka.Detective.Types.DatasourcePackageIngestDetail
import Amazonka.Detective.Types.DatasourcePackageIngestState
import Amazonka.Detective.Types.DatasourcePackageUsageInfo
import Amazonka.Detective.Types.Graph
import Amazonka.Detective.Types.InvitationType
import Amazonka.Detective.Types.MemberDetail
import Amazonka.Detective.Types.MemberDisabledReason
import Amazonka.Detective.Types.MemberStatus
import Amazonka.Detective.Types.MembershipDatasources
import Amazonka.Detective.Types.TimestampForCollection
import Amazonka.Detective.Types.UnprocessedAccount
import Amazonka.Detective.Types.UnprocessedGraph
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-10-26@ of the Amazon Detective SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Detective",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.detective",
      Core.signingName = "detective",
      Core.version = "2018-10-26",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Detective",
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

-- | The request issuer does not have permission to access this resource or
-- perform this operation.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request attempted an invalid action.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request was valid but failed because of a problem with the service.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request refers to a nonexistent resource.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | This request cannot be completed for one of the following reasons.
--
-- -   The request would cause the number of member accounts in the
--     behavior graph to exceed the maximum allowed. A behavior graph
--     cannot have more than 1200 member accounts.
--
-- -   The request would cause the data rate for the behavior graph to
--     exceed the maximum allowed.
--
-- -   Detective is unable to verify the data rate for the member account.
--     This is usually because the member account is not enrolled in Amazon
--     GuardDuty.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request cannot be completed because too many other requests are
-- occurring at the same time.
_TooManyRequestsException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The request parameters are invalid.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
