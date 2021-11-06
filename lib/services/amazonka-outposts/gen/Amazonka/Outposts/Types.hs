{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _NotFoundException,
    _ServiceQuotaExceededException,
    _InternalServerException,

    -- * OrderStatus
    OrderStatus (..),

    -- * PaymentOption
    PaymentOption (..),

    -- * PaymentTerm
    PaymentTerm (..),

    -- * InstanceTypeItem
    InstanceTypeItem (..),
    newInstanceTypeItem,
    instanceTypeItem_instanceType,

    -- * LineItem
    LineItem (..),
    newLineItem,
    lineItem_status,
    lineItem_quantity,
    lineItem_catalogItemId,
    lineItem_lineItemId,

    -- * LineItemRequest
    LineItemRequest (..),
    newLineItemRequest,
    lineItemRequest_quantity,
    lineItemRequest_catalogItemId,

    -- * Order
    Order (..),
    newOrder,
    order_status,
    order_orderSubmissionDate,
    order_lineItems,
    order_orderFulfilledDate,
    order_orderId,
    order_outpostId,
    order_paymentOption,

    -- * Outpost
    Outpost (..),
    newOutpost,
    outpost_availabilityZoneId,
    outpost_outpostArn,
    outpost_ownerId,
    outpost_availabilityZone,
    outpost_name,
    outpost_lifeCycleStatus,
    outpost_outpostId,
    outpost_siteId,
    outpost_siteArn,
    outpost_description,
    outpost_tags,

    -- * Site
    Site (..),
    newSite,
    site_accountId,
    site_name,
    site_siteId,
    site_siteArn,
    site_description,
    site_tags,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Outposts.Types.InstanceTypeItem
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.LineItemRequest
import Amazonka.Outposts.Types.Order
import Amazonka.Outposts.Types.OrderStatus
import Amazonka.Outposts.Types.Outpost
import Amazonka.Outposts.Types.PaymentOption
import Amazonka.Outposts.Types.PaymentTerm
import Amazonka.Outposts.Types.Site
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2019-12-03@ of the Amazon Outposts SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Outposts",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "outposts",
      Core._serviceSigningName = "outposts",
      Core._serviceVersion = "2019-12-03",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Outposts",
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

-- | A parameter is not valid.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have permission to perform this operation.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting this resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The specified request is not valid.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | You have exceeded a service quota.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | An internal error has occurred.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500
