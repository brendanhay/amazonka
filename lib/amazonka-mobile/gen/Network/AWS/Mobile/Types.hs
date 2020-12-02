{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Mobile.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Mobile.Types
  ( -- * Service Configuration
    mobile,

    -- * Errors

    -- * Platform
    Platform (..),

    -- * ProjectState
    ProjectState (..),

    -- * BundleDetails
    BundleDetails,
    bundleDetails,
    bdAvailablePlatforms,
    bdBundleId,
    bdVersion,
    bdIconURL,
    bdTitle,
    bdDescription,

    -- * ProjectDetails
    ProjectDetails,
    projectDetails,
    pdState,
    pdResources,
    pdCreatedDate,
    pdConsoleURL,
    pdName,
    pdRegion,
    pdProjectId,
    pdLastUpdatedDate,

    -- * ProjectSummary
    ProjectSummary,
    projectSummary,
    psName,
    psProjectId,

    -- * Resource
    Resource,
    resource,
    rFeature,
    rArn,
    rName,
    rAttributes,
    rType,
  )
where

import Network.AWS.Lens
import Network.AWS.Mobile.Types.BundleDetails
import Network.AWS.Mobile.Types.Platform
import Network.AWS.Mobile.Types.ProjectDetails
import Network.AWS.Mobile.Types.ProjectState
import Network.AWS.Mobile.Types.ProjectSummary
import Network.AWS.Mobile.Types.Resource
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2017-07-01@ of the Amazon Mobile SDK configuration.
mobile :: Service
mobile =
  Service
    { _svcAbbrev = "Mobile",
      _svcSigner = v4,
      _svcPrefix = "mobile",
      _svcVersion = "2017-07-01",
      _svcEndpoint = defaultEndpoint mobile,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "Mobile",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
