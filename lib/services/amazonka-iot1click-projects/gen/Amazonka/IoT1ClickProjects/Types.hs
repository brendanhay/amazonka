{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT1ClickProjects.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT1ClickProjects.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalFailureException,
    _InvalidRequestException,
    _ResourceConflictException,
    _ResourceNotFoundException,
    _TooManyRequestsException,

    -- * DeviceTemplate
    DeviceTemplate (..),
    newDeviceTemplate,
    deviceTemplate_callbackOverrides,
    deviceTemplate_deviceType,

    -- * PlacementDescription
    PlacementDescription (..),
    newPlacementDescription,
    placementDescription_projectName,
    placementDescription_placementName,
    placementDescription_attributes,
    placementDescription_createdDate,
    placementDescription_updatedDate,

    -- * PlacementSummary
    PlacementSummary (..),
    newPlacementSummary,
    placementSummary_projectName,
    placementSummary_placementName,
    placementSummary_createdDate,
    placementSummary_updatedDate,

    -- * PlacementTemplate
    PlacementTemplate (..),
    newPlacementTemplate,
    placementTemplate_defaultAttributes,
    placementTemplate_deviceTemplates,

    -- * ProjectDescription
    ProjectDescription (..),
    newProjectDescription,
    projectDescription_arn,
    projectDescription_description,
    projectDescription_placementTemplate,
    projectDescription_tags,
    projectDescription_projectName,
    projectDescription_createdDate,
    projectDescription_updatedDate,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_arn,
    projectSummary_tags,
    projectSummary_projectName,
    projectSummary_createdDate,
    projectSummary_updatedDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT1ClickProjects.Types.DeviceTemplate
import Amazonka.IoT1ClickProjects.Types.PlacementDescription
import Amazonka.IoT1ClickProjects.Types.PlacementSummary
import Amazonka.IoT1ClickProjects.Types.PlacementTemplate
import Amazonka.IoT1ClickProjects.Types.ProjectDescription
import Amazonka.IoT1ClickProjects.Types.ProjectSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-14@ of the Amazon IoT 1-Click Projects Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoT1ClickProjects",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "projects.iot1click",
      Core.signingName = "iot1click",
      Core.version = "2018-05-14",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoT1ClickProjects",
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

-- |
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500

-- |
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"
    Prelude.. Core.hasStatus 400

-- |
_ResourceConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceConflictException =
  Core._MatchServiceError
    defaultService
    "ResourceConflictException"
    Prelude.. Core.hasStatus 409

-- |
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- |
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429
