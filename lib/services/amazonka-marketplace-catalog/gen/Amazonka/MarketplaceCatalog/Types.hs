{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MarketplaceCatalog.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServiceException,
    _ResourceInUseException,
    _ResourceNotFoundException,
    _ResourceNotSupportedException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

    -- * ChangeStatus
    ChangeStatus (..),

    -- * FailureCode
    FailureCode (..),

    -- * SortOrder
    SortOrder (..),

    -- * Change
    Change (..),
    newChange,
    change_changeName,
    change_entityTags,
    change_changeType,
    change_entity,
    change_details,

    -- * ChangeSetSummaryListItem
    ChangeSetSummaryListItem (..),
    newChangeSetSummaryListItem,
    changeSetSummaryListItem_changeSetArn,
    changeSetSummaryListItem_changeSetId,
    changeSetSummaryListItem_changeSetName,
    changeSetSummaryListItem_endTime,
    changeSetSummaryListItem_entityIdList,
    changeSetSummaryListItem_failureCode,
    changeSetSummaryListItem_startTime,
    changeSetSummaryListItem_status,

    -- * ChangeSummary
    ChangeSummary (..),
    newChangeSummary,
    changeSummary_changeName,
    changeSummary_changeType,
    changeSummary_details,
    changeSummary_entity,
    changeSummary_errorDetailList,

    -- * Entity
    Entity (..),
    newEntity,
    entity_identifier,
    entity_type,

    -- * EntitySummary
    EntitySummary (..),
    newEntitySummary,
    entitySummary_entityArn,
    entitySummary_entityId,
    entitySummary_entityType,
    entitySummary_lastModifiedDate,
    entitySummary_name,
    entitySummary_visibility,

    -- * ErrorDetail
    ErrorDetail (..),
    newErrorDetail,
    errorDetail_errorCode,
    errorDetail_errorMessage,

    -- * Filter
    Filter (..),
    newFilter,
    filter_name,
    filter_valueList,

    -- * Sort
    Sort (..),
    newSort,
    sort_sortBy,
    sort_sortOrder,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceCatalog.Types.Change
import Amazonka.MarketplaceCatalog.Types.ChangeSetSummaryListItem
import Amazonka.MarketplaceCatalog.Types.ChangeStatus
import Amazonka.MarketplaceCatalog.Types.ChangeSummary
import Amazonka.MarketplaceCatalog.Types.Entity
import Amazonka.MarketplaceCatalog.Types.EntitySummary
import Amazonka.MarketplaceCatalog.Types.ErrorDetail
import Amazonka.MarketplaceCatalog.Types.FailureCode
import Amazonka.MarketplaceCatalog.Types.Filter
import Amazonka.MarketplaceCatalog.Types.Sort
import Amazonka.MarketplaceCatalog.Types.SortOrder
import Amazonka.MarketplaceCatalog.Types.Tag
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-09-17@ of the Amazon Marketplace Catalog Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "MarketplaceCatalog",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "catalog.marketplace",
      Core.signingName = "aws-marketplace",
      Core.version = "2018-09-17",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "MarketplaceCatalog",
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

-- | Access is denied.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | There was an internal service exception.
_InternalServiceException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"
    Prelude.. Core.hasStatus 500

-- | The resource is currently in use.
_ResourceInUseException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceInUseException =
  Core._MatchServiceError
    defaultService
    "ResourceInUseException"
    Prelude.. Core.hasStatus 423

-- | The specified resource wasn\'t found.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Currently, the specified resource is not supported.
_ResourceNotSupportedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ResourceNotSupportedException"
    Prelude.. Core.hasStatus 415

-- | The maximum number of open requests per account has been exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Too many requests.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | An error occurred during validation.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 422
