{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _TooManyRequestsException,
    _InternalFailureException,
    _ServiceUnavailableException,
    _UnauthorizedException,
    _BadRequestException,
    _LimitExceededException,
    _AccountActionRequiredException,

    -- * Platform
    Platform (..),

    -- * ProjectState
    ProjectState (..),

    -- * BundleDetails
    BundleDetails (..),
    newBundleDetails,
    bundleDetails_availablePlatforms,
    bundleDetails_bundleId,
    bundleDetails_version,
    bundleDetails_iconUrl,
    bundleDetails_title,
    bundleDetails_description,

    -- * ProjectDetails
    ProjectDetails (..),
    newProjectDetails,
    projectDetails_state,
    projectDetails_resources,
    projectDetails_createdDate,
    projectDetails_consoleUrl,
    projectDetails_name,
    projectDetails_region,
    projectDetails_projectId,
    projectDetails_lastUpdatedDate,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_name,
    projectSummary_projectId,

    -- * Resource
    Resource (..),
    newResource,
    resource_feature,
    resource_arn,
    resource_name,
    resource_attributes,
    resource_type,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types.BundleDetails
import Network.AWS.Mobile.Types.Platform
import Network.AWS.Mobile.Types.ProjectDetails
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.ProjectSummary
import Network.AWS.Mobile.Types.Resource
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-07-01@ of the Amazon Mobile SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Mobile",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "mobile",
      Core._serviceSigningName = "AWSMobileHubService",
      Core._serviceVersion = "2017-07-01",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Mobile",
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

-- | No entity can be found with the specified identifier.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Core.hasStatus 404

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

-- | The service is temporarily unavailable. The request should be retried
-- after some time delay.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Core.hasStatus 503

-- | Credentials of the caller are insufficient to authorize the request.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Core.hasStatus 401

-- | The request cannot be processed because some parameter is not valid or
-- the project state prevents the operation from being performed.
_BadRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Core.hasStatus 400

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
