{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroupsTagging.Types
  ( -- * Service Configuration
    resourceGroupsTagging,

    -- * Errors

    -- * GroupByAttribute
    GroupByAttribute (..),

    -- * ResourceErrorCode
    ResourceErrorCode (..),

    -- * TargetIdType
    TargetIdType (..),

    -- * ComplianceDetails
    ComplianceDetails,
    complianceDetails,
    cdKeysWithNoncompliantValues,
    cdComplianceStatus,
    cdNoncompliantKeys,

    -- * FailureInfo
    FailureInfo,
    failureInfo,
    fiErrorCode,
    fiErrorMessage,
    fiStatusCode,

    -- * ResourceTagMapping
    ResourceTagMapping,
    resourceTagMapping,
    rtmComplianceDetails,
    rtmResourceARN,
    rtmTags,

    -- * Summary
    Summary,
    summary,
    sTargetId,
    sLastUpdated,
    sResourceType,
    sNonCompliantResources,
    sTargetIdType,
    sRegion,

    -- * Tag
    Tag,
    tag,
    tagKey,
    tagValue,

    -- * TagFilter
    TagFilter,
    tagFilter,
    tfValues,
    tfKey,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroupsTagging.Types.ComplianceDetails
import Network.AWS.ResourceGroupsTagging.Types.FailureInfo
import Network.AWS.ResourceGroupsTagging.Types.GroupByAttribute
import Network.AWS.ResourceGroupsTagging.Types.ResourceErrorCode
import Network.AWS.ResourceGroupsTagging.Types.ResourceTagMapping
import Network.AWS.ResourceGroupsTagging.Types.Summary
import Network.AWS.ResourceGroupsTagging.Types.Tag
import Network.AWS.ResourceGroupsTagging.Types.TagFilter
import Network.AWS.ResourceGroupsTagging.Types.TargetIdType
import Network.AWS.Sign.V4

-- | API version @2017-01-26@ of the Amazon Resource Groups Tagging API SDK configuration.
resourceGroupsTagging :: Service
resourceGroupsTagging =
  Service
    { _svcAbbrev = "ResourceGroupsTagging",
      _svcSigner = v4,
      _svcPrefix = "tagging",
      _svcVersion = "2017-01-26",
      _svcEndpoint = defaultEndpoint resourceGroupsTagging,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ResourceGroupsTagging",
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
