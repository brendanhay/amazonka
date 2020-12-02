{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types
  ( -- * Service Configuration
    resourceGroups,

    -- * Errors

    -- * GroupConfigurationStatus
    GroupConfigurationStatus (..),

    -- * GroupFilterName
    GroupFilterName (..),

    -- * QueryErrorCode
    QueryErrorCode (..),

    -- * QueryType
    QueryType (..),

    -- * ResourceFilterName
    ResourceFilterName (..),

    -- * FailedResource
    FailedResource,
    failedResource,
    frResourceARN,
    frErrorCode,
    frErrorMessage,

    -- * Group
    Group,
    group',
    gDescription,
    gGroupARN,
    gName,

    -- * GroupConfiguration
    GroupConfiguration,
    groupConfiguration,
    gcStatus,
    gcFailureReason,
    gcProposedConfiguration,
    gcConfiguration,

    -- * GroupConfigurationItem
    GroupConfigurationItem,
    groupConfigurationItem,
    gciParameters,
    gciType,

    -- * GroupConfigurationParameter
    GroupConfigurationParameter,
    groupConfigurationParameter,
    gcpValues,
    gcpName,

    -- * GroupFilter
    GroupFilter,
    groupFilter,
    gfName,
    gfValues,

    -- * GroupIdentifier
    GroupIdentifier,
    groupIdentifier,
    giGroupARN,
    giGroupName,

    -- * GroupQuery
    GroupQuery,
    groupQuery,
    gqGroupName,
    gqResourceQuery,

    -- * QueryError
    QueryError,
    queryError,
    qeErrorCode,
    qeMessage,

    -- * ResourceFilter
    ResourceFilter,
    resourceFilter,
    rfName,
    rfValues,

    -- * ResourceIdentifier
    ResourceIdentifier,
    resourceIdentifier,
    riResourceType,
    riResourceARN,

    -- * ResourceQuery
    ResourceQuery,
    resourceQuery,
    rqType,
    rqSearchQuery,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.ResourceGroups.Types.FailedResource
import Network.AWS.ResourceGroups.Types.Group
import Network.AWS.ResourceGroups.Types.GroupConfiguration
import Network.AWS.ResourceGroups.Types.GroupConfigurationItem
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
import Network.AWS.ResourceGroups.Types.GroupConfigurationStatus
import Network.AWS.ResourceGroups.Types.GroupFilter
import Network.AWS.ResourceGroups.Types.GroupFilterName
import Network.AWS.ResourceGroups.Types.GroupIdentifier
import Network.AWS.ResourceGroups.Types.GroupQuery
import Network.AWS.ResourceGroups.Types.QueryError
import Network.AWS.ResourceGroups.Types.QueryErrorCode
import Network.AWS.ResourceGroups.Types.QueryType
import Network.AWS.ResourceGroups.Types.ResourceFilter
import Network.AWS.ResourceGroups.Types.ResourceFilterName
import Network.AWS.ResourceGroups.Types.ResourceIdentifier
import Network.AWS.ResourceGroups.Types.ResourceQuery
import Network.AWS.Sign.V4

-- | API version @2017-11-27@ of the Amazon Resource Groups SDK configuration.
resourceGroups :: Service
resourceGroups =
  Service
    { _svcAbbrev = "ResourceGroups",
      _svcSigner = v4,
      _svcPrefix = "resource-groups",
      _svcVersion = "2017-11-27",
      _svcEndpoint = defaultEndpoint resourceGroups,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "ResourceGroups",
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
