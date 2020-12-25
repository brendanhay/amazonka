-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroups.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ResourceGroups.Types
  ( -- * Service configuration
    mkServiceConfig,

    -- * Errors
    _ForbiddenException,
    _NotFoundException,
    _TooManyRequestsException,
    _InternalServerErrorException,
    _MethodNotAllowedException,
    _UnauthorizedException,
    _BadRequestException,

    -- * GroupFilterValue
    GroupFilterValue (..),

    -- * GroupIdentifier
    GroupIdentifier (..),
    mkGroupIdentifier,
    giGroupArn,
    giGroupName,

    -- * QueryErrorCode
    QueryErrorCode (..),

    -- * Group
    Group (..),
    mkGroup,
    gGroupArn,
    gName,
    gDescription,

    -- * GroupConfiguration
    GroupConfiguration (..),
    mkGroupConfiguration,
    gcConfiguration,
    gcFailureReason,
    gcProposedConfiguration,
    gcStatus,

    -- * ResourceFilter
    ResourceFilter (..),
    mkResourceFilter,
    rfName,
    rfValues,

    -- * ResourceType
    ResourceType (..),

    -- * QueryType
    QueryType (..),

    -- * GroupString
    GroupString (..),

    -- * GroupQuery
    GroupQuery (..),
    mkGroupQuery,
    gqGroupName,
    gqResourceQuery,

    -- * GroupConfigurationParameterValue
    GroupConfigurationParameterValue (..),

    -- * GroupFilterName
    GroupFilterName (..),

    -- * ResourceQuery
    ResourceQuery (..),
    mkResourceQuery,
    rqType,
    rqSearchQuery,

    -- * GroupConfigurationStatus
    GroupConfigurationStatus (..),

    -- * FailedResource
    FailedResource (..),
    mkFailedResource,
    frErrorCode,
    frErrorMessage,
    frResourceArn,

    -- * TagValue
    TagValue (..),

    -- * QueryErrorMessage
    QueryErrorMessage (..),

    -- * ResourceFilterValue
    ResourceFilterValue (..),

    -- * NextToken
    NextToken (..),

    -- * GroupFilter
    GroupFilter (..),
    mkGroupFilter,
    gfName,
    gfValues,

    -- * ResourceArn
    ResourceArn (..),

    -- * GroupConfigurationType
    GroupConfigurationType (..),

    -- * GroupConfigurationItem
    GroupConfigurationItem (..),
    mkGroupConfigurationItem,
    gciType,
    gciParameters,

    -- * QueryError
    QueryError (..),
    mkQueryError,
    qeErrorCode,
    qeMessage,

    -- * Query
    Query (..),

    -- * GroupArn
    GroupArn (..),

    -- * ErrorCode
    ErrorCode (..),

    -- * ResourceFilterName
    ResourceFilterName (..),

    -- * TagKey
    TagKey (..),

    -- * GroupName
    GroupName (..),

    -- * ErrorMessage
    ErrorMessage (..),

    -- * Description
    Description (..),

    -- * GroupConfigurationParameter
    GroupConfigurationParameter (..),
    mkGroupConfigurationParameter,
    gcpName,
    gcpValues,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riResourceArn,
    riResourceType,

    -- * Name
    Name (..),

    -- * FailureReason
    FailureReason (..),

    -- * Arn
    Arn (..),
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import Network.AWS.ResourceGroups.Types.Arn
import Network.AWS.ResourceGroups.Types.Description
import Network.AWS.ResourceGroups.Types.ErrorCode
import Network.AWS.ResourceGroups.Types.ErrorMessage
import Network.AWS.ResourceGroups.Types.FailedResource
import Network.AWS.ResourceGroups.Types.FailureReason
import Network.AWS.ResourceGroups.Types.Group
import Network.AWS.ResourceGroups.Types.GroupArn
import Network.AWS.ResourceGroups.Types.GroupConfiguration
import Network.AWS.ResourceGroups.Types.GroupConfigurationItem
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameter
import Network.AWS.ResourceGroups.Types.GroupConfigurationParameterValue
import Network.AWS.ResourceGroups.Types.GroupConfigurationStatus
import Network.AWS.ResourceGroups.Types.GroupConfigurationType
import Network.AWS.ResourceGroups.Types.GroupFilter
import Network.AWS.ResourceGroups.Types.GroupFilterName
import Network.AWS.ResourceGroups.Types.GroupFilterValue
import Network.AWS.ResourceGroups.Types.GroupIdentifier
import Network.AWS.ResourceGroups.Types.GroupName
import Network.AWS.ResourceGroups.Types.GroupQuery
import Network.AWS.ResourceGroups.Types.GroupString
import Network.AWS.ResourceGroups.Types.Name
import Network.AWS.ResourceGroups.Types.NextToken
import Network.AWS.ResourceGroups.Types.Query
import Network.AWS.ResourceGroups.Types.QueryError
import Network.AWS.ResourceGroups.Types.QueryErrorCode
import Network.AWS.ResourceGroups.Types.QueryErrorMessage
import Network.AWS.ResourceGroups.Types.QueryType
import Network.AWS.ResourceGroups.Types.ResourceArn
import Network.AWS.ResourceGroups.Types.ResourceFilter
import Network.AWS.ResourceGroups.Types.ResourceFilterName
import Network.AWS.ResourceGroups.Types.ResourceFilterValue
import Network.AWS.ResourceGroups.Types.ResourceIdentifier
import Network.AWS.ResourceGroups.Types.ResourceQuery
import Network.AWS.ResourceGroups.Types.ResourceType
import Network.AWS.ResourceGroups.Types.TagKey
import Network.AWS.ResourceGroups.Types.TagValue
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Resource Groups SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "ResourceGroups",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "resource-groups",
      Core._svcVersion = "2017-11-27",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "ResourceGroups",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The caller isn't authorized to make the request. Check permissions.
_ForbiddenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ForbiddenException =
  Core._MatchServiceError mkServiceConfig "ForbiddenException"
    Core.. Core.hasStatues 403
{-# DEPRECATED _ForbiddenException "Use generic-lens or generic-optics instead." #-}

-- | One or more of the specified resources don't exist.
_NotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError mkServiceConfig "NotFoundException"
    Core.. Core.hasStatues 404
{-# DEPRECATED _NotFoundException "Use generic-lens or generic-optics instead." #-}

-- | You've exceeded throttling limits by making too many requests in a period of time.
_TooManyRequestsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyRequestsException =
  Core._MatchServiceError
    mkServiceConfig
    "TooManyRequestsException"
    Core.. Core.hasStatues 429
{-# DEPRECATED _TooManyRequestsException "Use generic-lens or generic-optics instead." #-}

-- | An internal error occurred while processing the request. Try again later.
_InternalServerErrorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InternalServerErrorException =
  Core._MatchServiceError
    mkServiceConfig
    "InternalServerErrorException"
    Core.. Core.hasStatues 500
{-# DEPRECATED _InternalServerErrorException "Use generic-lens or generic-optics instead." #-}

-- | The request uses an HTTP method that isn't allowed for the specified resource.
_MethodNotAllowedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MethodNotAllowedException =
  Core._MatchServiceError
    mkServiceConfig
    "MethodNotAllowedException"
    Core.. Core.hasStatues 405
{-# DEPRECATED _MethodNotAllowedException "Use generic-lens or generic-optics instead." #-}

-- | The request was rejected because it doesn't have valid credentials for the target resource.
_UnauthorizedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_UnauthorizedException =
  Core._MatchServiceError mkServiceConfig "UnauthorizedException"
    Core.. Core.hasStatues 401
{-# DEPRECATED _UnauthorizedException "Use generic-lens or generic-optics instead." #-}

-- | The request includes one or more parameters that violate validation rules.
_BadRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BadRequestException =
  Core._MatchServiceError mkServiceConfig "BadRequestException"
    Core.. Core.hasStatues 400
{-# DEPRECATED _BadRequestException "Use generic-lens or generic-optics instead." #-}
