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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev =
        "ResourceGroups",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "resource-groups",
      Prelude._svcVersion = "2017-11-27",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "ResourceGroups",
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

-- | One or more of the specified resources don\'t exist.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"
    Prelude.. Prelude.hasStatus 404

-- | The request includes one or more parameters that violate validation
-- rules.
_BadRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BadRequestException =
  Prelude._MatchServiceError
    defaultService
    "BadRequestException"
    Prelude.. Prelude.hasStatus 400

-- | The request was rejected because it doesn\'t have valid credentials for
-- the target resource.
_UnauthorizedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_UnauthorizedException =
  Prelude._MatchServiceError
    defaultService
    "UnauthorizedException"
    Prelude.. Prelude.hasStatus 401

-- | An internal error occurred while processing the request. Try again
-- later.
_InternalServerErrorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServerErrorException =
  Prelude._MatchServiceError
    defaultService
    "InternalServerErrorException"
    Prelude.. Prelude.hasStatus 500

-- | The caller isn\'t authorized to make the request. Check permissions.
_ForbiddenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ForbiddenException =
  Prelude._MatchServiceError
    defaultService
    "ForbiddenException"
    Prelude.. Prelude.hasStatus 403

-- | The request uses an HTTP method that isn\'t allowed for the specified
-- resource.
_MethodNotAllowedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MethodNotAllowedException =
  Prelude._MatchServiceError
    defaultService
    "MethodNotAllowedException"
    Prelude.. Prelude.hasStatus 405

-- | You\'ve exceeded throttling limits by making too many requests in a
-- period of time.
_TooManyRequestsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyRequestsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyRequestsException"
    Prelude.. Prelude.hasStatus 429
