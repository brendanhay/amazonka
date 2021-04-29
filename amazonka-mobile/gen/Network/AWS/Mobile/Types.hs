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
    _BadRequestException,
    _UnauthorizedException,
    _ServiceUnavailableException,
    _LimitExceededException,
    _AccountActionRequiredException,
    _InternalFailureException,
    _TooManyRequestsException,

    -- * Platform
    Platform (..),

    -- * ProjectState
    ProjectState (..),

    -- * BundleDetails
    BundleDetails (..),
    newBundleDetails,
    bundleDetails_bundleId,
    bundleDetails_title,
    bundleDetails_iconUrl,
    bundleDetails_version,
    bundleDetails_description,
    bundleDetails_availablePlatforms,

    -- * ProjectDetails
    ProjectDetails (..),
    newProjectDetails,
    projectDetails_createdDate,
    projectDetails_lastUpdatedDate,
    projectDetails_projectId,
    projectDetails_state,
    projectDetails_name,
    projectDetails_consoleUrl,
    projectDetails_resources,
    projectDetails_region,

    -- * ProjectSummary
    ProjectSummary (..),
    newProjectSummary,
    projectSummary_projectId,
    projectSummary_name,

    -- * Resource
    Resource (..),
    newResource,
    resource_arn,
    resource_name,
    resource_attributes,
    resource_feature,
    resource_type,
  )
where

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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "Mobile",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "mobile",
      Prelude._svcVersion = "2017-07-01",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError = Prelude.parseJSONError "Mobile",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | No entity can be found with the specified identifier.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The request cannot be processed because some parameter is not valid or
-- the project state prevents the operation from being performed.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | Credentials of the caller are insufficient to authorize the request.
_UnauthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | The service is temporarily unavailable. The request should be retried
-- after some time delay.
_ServiceUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "ServiceUnavailableException"
    Prelude.. Prelude.hasStatus 503

-- | There are too many AWS Mobile Hub projects in the account or the account
-- has exceeded the maximum number of resources in some AWS service. You
-- should create another sub-account using AWS Organizations or remove some
-- resources and retry your request.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
    Prelude.. Prelude.hasStatus 429

-- | Account Action is required in order to continue the request.
_AccountActionRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AccountActionRequiredException =
  Prelude._MatchServiceError
    defaultService
    "AccountActionRequiredException"
    Prelude.. Prelude.hasStatus 403

-- | The service has encountered an unexpected error condition which prevents
-- it from servicing the request.
_InternalFailureException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalFailureException =
  Prelude._MatchServiceError
    defaultService
    "InternalFailureException"
    Prelude.. Prelude.hasStatus 500

-- | Too many requests have been received for this AWS account in too short a
-- time. The request should be retried after some time delay.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
