{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Mobile.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Mobile.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _UnauthorizedException,
    _NotFoundException,
    _ServiceUnavailableException,
    _LimitExceededException,
    _AccountActionRequiredException,
    _BadRequestException,
    _TooManyRequestsException,
    _InternalFailureException,

    -- * Platform
    Platform (..),

    -- * ProjectState
    ProjectState (..),

    -- * BundleDetails
    BundleDetails (..),
    newBundleDetails,
    bundleDetails_availablePlatforms,
    bundleDetails_description,
    bundleDetails_iconUrl,
    bundleDetails_title,
    bundleDetails_bundleId,
    bundleDetails_version,

    -- * ProjectDetails
    ProjectDetails (..),
    newProjectDetails,
    projectDetails_name,
    projectDetails_consoleUrl,
    projectDetails_lastUpdatedDate,
    projectDetails_state,
    projectDetails_projectId,
    projectDetails_region,
    projectDetails_createdDate,
    projectDetails_resources,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_name,
    projectSummary_projectId,

    -- * Resource
    Resource (..),
    newResource,
    resource_name,
    resource_type,
    resource_arn,
    resource_feature,
    resource_attributes,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Mobile.Types.BundleDetails
import Amazonka.Mobile.Types.Platform
import Amazonka.Mobile.Types.ProjectDetails
import Amazonka.Mobile.Types.ProjectState
import Amazonka.Mobile.Types.ProjectSummary
import Amazonka.Mobile.Types.Resource
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-01@ of the Amazon Mobile SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Mobile",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "mobile",
      Core.signingName = "AWSMobileHubService",
      Core.version = "2017-07-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Mobile",
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

-- | Credentials of the caller are insufficient to authorize the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | No entity can be found with the specified identifier.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

-- | The service is temporarily unavailable. The request should be retried
-- after some time delay.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | There are too many AWS Mobile Hub projects in the account or the account
-- has exceeded the maximum number of resources in some AWS service. You
-- should create another sub-account using AWS Organizations or remove some
-- resources and retry your request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Core.hasStatus 429

-- | Account Action is required in order to continue the request.
_AccountActionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccountActionRequiredException =
  Core._MatchServiceError
    defaultService
    "AccountActionRequiredException"
    Prelude.. Core.hasStatus 403

-- | The request cannot be processed because some parameter is not valid or
-- the project state prevents the operation from being performed.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

-- | Too many requests have been received for this AWS account in too short a
-- time. The request should be retried after some time delay.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Core.hasStatus 429

-- | The service has encountered an unexpected error condition which prevents
-- it from servicing the request.
_InternalFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalFailureException =
  Core._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Core.hasStatus 500
