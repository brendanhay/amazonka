{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotFoundException,
    _BadRequestException,
    _UnauthorizedException,
    _InternalServerErrorException,
    _ForbiddenException,
    _MethodNotAllowedException,
    _TooManyRequestsException,

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

    -- * ResourceStatusValue
    ResourceStatusValue (..),

    -- * FailedResource
    FailedResource (..),
    newFailedResource,
    failedResource_resourceArn,
    failedResource_errorMessage,
    failedResource_errorCode,

    -- * Group
    Group (..),
    newGroup,
    group_description,
    group_groupArn,
    group_name,

    -- * GroupConfiguration
    GroupConfiguration (..),
    newGroupConfiguration,
    groupConfiguration_status,
    groupConfiguration_configuration,
    groupConfiguration_failureReason,
    groupConfiguration_proposedConfiguration,

    -- * GroupConfigurationItem
    GroupConfigurationItem (..),
    newGroupConfigurationItem,
    groupConfigurationItem_parameters,
    groupConfigurationItem_type,

    -- * GroupConfigurationParameter
    GroupConfigurationParameter (..),
    newGroupConfigurationParameter,
    groupConfigurationParameter_values,
    groupConfigurationParameter_name,

    -- * GroupFilter
    GroupFilter (..),
    newGroupFilter,
    groupFilter_name,
    groupFilter_values,

    -- * GroupIdentifier
    GroupIdentifier (..),
    newGroupIdentifier,
    groupIdentifier_groupName,
    groupIdentifier_groupArn,

    -- * GroupQuery
    GroupQuery (..),
    newGroupQuery,
    groupQuery_groupName,
    groupQuery_resourceQuery,

    -- * ListGroupResourcesItem
    ListGroupResourcesItem (..),
    newListGroupResourcesItem,
    listGroupResourcesItem_status,
    listGroupResourcesItem_identifier,

    -- * PendingResource
    PendingResource (..),
    newPendingResource,
    pendingResource_resourceArn,

    -- * QueryError
    QueryError (..),
    newQueryError,
    queryError_message,
    queryError_errorCode,

    -- * ResourceFilter
    ResourceFilter (..),
    newResourceFilter,
    resourceFilter_name,
    resourceFilter_values,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    newResourceIdentifier,
    resourceIdentifier_resourceArn,
    resourceIdentifier_resourceType,

    -- * ResourceQuery
    ResourceQuery (..),
    newResourceQuery,
    resourceQuery_type,
    resourceQuery_searchQuery,

    -- * ResourceStatus
    ResourceStatus (..),
    newResourceStatus,
    resourceStatus_name,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
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
import Network.AWS.ResourceGroups.Types.ListGroupResourcesItem
import Network.AWS.ResourceGroups.Types.PendingResource
import Network.AWS.ResourceGroups.Types.QueryError
import Network.AWS.ResourceGroups.Types.QueryErrorCode
import Network.AWS.ResourceGroups.Types.QueryType
import Network.AWS.ResourceGroups.Types.ResourceFilter
import Network.AWS.ResourceGroups.Types.ResourceFilterName
import Network.AWS.ResourceGroups.Types.ResourceIdentifier
import Network.AWS.ResourceGroups.Types.ResourceQuery
import Network.AWS.ResourceGroups.Types.ResourceStatus
import Network.AWS.ResourceGroups.Types.ResourceStatusValue
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Resource Groups SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ResourceGroups",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "resource-groups",
      Core._serviceSigningName = "resource-groups",
      Core._serviceVersion = "2017-11-27",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Core.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ResourceGroups",
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
      | Lens.has (Core.hasStatus 504) e =
        Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Core.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Core.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throttling_exception"
      | Lens.has
          (Core.hasCode "Throttling" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling"
      | Core.otherwise = Core.Nothing

-- | One or more of the specified resources don\'t exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"
    Core.. Core.hasStatus 404

-- | The request includes one or more parameters that violate validation
-- rules.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError
    defaultService
    "BadRequestException"
    Core.. Core.hasStatus 400

-- | The request was rejected because it doesn\'t have valid credentials for
-- the target resource.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError
    defaultService
    "UnauthorizedException"
    Core.. Core.hasStatus 401

-- | An internal error occurred while processing the request. Try again
-- later.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Core.. Core.hasStatus 500

-- | The caller isn\'t authorized to make the request. Check permissions.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError
    defaultService
    "ForbiddenException"
    Core.. Core.hasStatus 403

-- | The request uses an HTTP method that isn\'t allowed for the specified
-- resource.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Core.. Core.hasStatus 405

-- | You\'ve exceeded throttling limits by making too many requests in a
-- period of time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Core.. Core.hasStatus 429
