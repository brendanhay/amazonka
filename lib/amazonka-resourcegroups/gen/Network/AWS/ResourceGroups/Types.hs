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
    resourceGroupsService,

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
    FailedResource (..),
    mkFailedResource,
    frResourceARN,
    frErrorCode,
    frErrorMessage,

    -- * Group
    Group (..),
    mkGroup,
    gName,
    gGroupARN,
    gDescription,

    -- * GroupConfiguration
    GroupConfiguration (..),
    mkGroupConfiguration,
    gcStatus,
    gcFailureReason,
    gcProposedConfiguration,
    gcConfiguration,

    -- * GroupConfigurationItem
    GroupConfigurationItem (..),
    mkGroupConfigurationItem,
    gciParameters,
    gciType,

    -- * GroupConfigurationParameter
    GroupConfigurationParameter (..),
    mkGroupConfigurationParameter,
    gcpValues,
    gcpName,

    -- * GroupFilter
    GroupFilter (..),
    mkGroupFilter,
    gfValues,
    gfName,

    -- * GroupIdentifier
    GroupIdentifier (..),
    mkGroupIdentifier,
    giGroupARN,
    giGroupName,

    -- * GroupQuery
    GroupQuery (..),
    mkGroupQuery,
    gqResourceQuery,
    gqGroupName,

    -- * QueryError
    QueryError (..),
    mkQueryError,
    qeErrorCode,
    qeMessage,

    -- * ResourceFilter
    ResourceFilter (..),
    mkResourceFilter,
    rfValues,
    rfName,

    -- * ResourceIdentifier
    ResourceIdentifier (..),
    mkResourceIdentifier,
    riResourceType,
    riResourceARN,

    -- * ResourceQuery
    ResourceQuery (..),
    mkResourceQuery,
    rqSearchQuery,
    rqType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
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
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2017-11-27@ of the Amazon Resource Groups SDK configuration.
resourceGroupsService :: Lude.Service
resourceGroupsService =
  Lude.Service
    { Lude._svcAbbrev = "ResourceGroups",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "resource-groups",
      Lude._svcVersion = "2017-11-27",
      Lude._svcEndpoint = Lude.defaultEndpoint resourceGroupsService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "ResourceGroups",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
