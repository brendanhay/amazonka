{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppIntegrationS.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppIntegrationS.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InvalidRequestException,
    _DuplicateResourceException,
    _ResourceQuotaExceededException,
    _ThrottlingException,
    _InternalServiceError,
    _ResourceNotFoundException,

    -- * DataIntegrationAssociationSummary
    DataIntegrationAssociationSummary (..),
    newDataIntegrationAssociationSummary,
    dataIntegrationAssociationSummary_clientId,
    dataIntegrationAssociationSummary_dataIntegrationAssociationArn,
    dataIntegrationAssociationSummary_dataIntegrationArn,

    -- * DataIntegrationSummary
    DataIntegrationSummary (..),
    newDataIntegrationSummary,
    dataIntegrationSummary_arn,
    dataIntegrationSummary_name,
    dataIntegrationSummary_sourceURI,

    -- * EventFilter
    EventFilter (..),
    newEventFilter,
    eventFilter_source,

    -- * EventIntegration
    EventIntegration (..),
    newEventIntegration,
    eventIntegration_eventBridgeBus,
    eventIntegration_eventFilter,
    eventIntegration_eventIntegrationArn,
    eventIntegration_name,
    eventIntegration_description,
    eventIntegration_tags,

    -- * EventIntegrationAssociation
    EventIntegrationAssociation (..),
    newEventIntegrationAssociation,
    eventIntegrationAssociation_clientId,
    eventIntegrationAssociation_eventIntegrationName,
    eventIntegrationAssociation_clientAssociationMetadata,
    eventIntegrationAssociation_eventIntegrationAssociationId,
    eventIntegrationAssociation_eventIntegrationAssociationArn,
    eventIntegrationAssociation_eventBridgeRuleName,

    -- * ScheduleConfiguration
    ScheduleConfiguration (..),
    newScheduleConfiguration,
    scheduleConfiguration_scheduleExpression,
    scheduleConfiguration_object,
    scheduleConfiguration_firstExecutionFrom,
  )
where

import Amazonka.AppIntegrationS.Types.DataIntegrationAssociationSummary
import Amazonka.AppIntegrationS.Types.DataIntegrationSummary
import Amazonka.AppIntegrationS.Types.EventFilter
import Amazonka.AppIntegrationS.Types.EventIntegration
import Amazonka.AppIntegrationS.Types.EventIntegrationAssociation
import Amazonka.AppIntegrationS.Types.ScheduleConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-29@ of the Amazon AppIntegrations Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "AppIntegrationS",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "app-integrations",
      Core._serviceSigningName = "app-integrations",
      Core._serviceVersion = "2020-07-29",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AppIntegrationS",
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

-- | You do not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | The request is not valid.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- | A resource with the specified name already exists.
_DuplicateResourceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DuplicateResourceException =
  Core._MatchServiceError
    defaultService
    "DuplicateResourceException"
    Prelude.. Core.hasStatus 409

-- | The allowed quota for the resource has been exceeded.
_ResourceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ResourceQuotaExceededException"
    Prelude.. Core.hasStatus 429

-- | The throttling limit has been exceeded.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Request processing failed due to an error or failure with the service.
_InternalServiceError :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceError =
  Core._MatchServiceError
    defaultService
    "InternalServiceError"
    Prelude.. Core.hasStatus 500

-- | The specified resource was not found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
