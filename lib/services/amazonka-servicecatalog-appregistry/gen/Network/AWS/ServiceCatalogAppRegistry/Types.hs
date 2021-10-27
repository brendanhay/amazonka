{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalogAppRegistry.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalogAppRegistry.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * ResourceGroupState
    ResourceGroupState (..),

    -- * ResourceType
    ResourceType (..),

    -- * SyncAction
    SyncAction (..),

    -- * Application
    Application (..),
    newApplication,
    application_creationTime,
    application_arn,
    application_name,
    application_id,
    application_lastUpdateTime,
    application_description,
    application_tags,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_creationTime,
    applicationSummary_arn,
    applicationSummary_name,
    applicationSummary_id,
    applicationSummary_lastUpdateTime,
    applicationSummary_description,

    -- * AttributeGroup
    AttributeGroup (..),
    newAttributeGroup,
    attributeGroup_creationTime,
    attributeGroup_arn,
    attributeGroup_name,
    attributeGroup_id,
    attributeGroup_lastUpdateTime,
    attributeGroup_description,
    attributeGroup_tags,

    -- * AttributeGroupSummary
    AttributeGroupSummary (..),
    newAttributeGroupSummary,
    attributeGroupSummary_creationTime,
    attributeGroupSummary_arn,
    attributeGroupSummary_name,
    attributeGroupSummary_id,
    attributeGroupSummary_lastUpdateTime,
    attributeGroupSummary_description,

    -- * Integrations
    Integrations (..),
    newIntegrations,
    integrations_resourceGroup,

    -- * Resource
    Resource (..),
    newResource,
    resource_arn,
    resource_integrations,
    resource_associationTime,
    resource_name,

    -- * ResourceGroup
    ResourceGroup (..),
    newResourceGroup,
    resourceGroup_state,
    resourceGroup_arn,
    resourceGroup_errorMessage,

    -- * ResourceInfo
    ResourceInfo (..),
    newResourceInfo,
    resourceInfo_arn,
    resourceInfo_name,

    -- * ResourceIntegrations
    ResourceIntegrations (..),
    newResourceIntegrations,
    resourceIntegrations_resourceGroup,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalogAppRegistry.Types.Application
import Network.AWS.ServiceCatalogAppRegistry.Types.ApplicationSummary
import Network.AWS.ServiceCatalogAppRegistry.Types.AttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.Types.AttributeGroupSummary
import Network.AWS.ServiceCatalogAppRegistry.Types.Integrations
import Network.AWS.ServiceCatalogAppRegistry.Types.Resource
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceGroup
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceGroupState
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceInfo
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceIntegrations
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceType
import Network.AWS.ServiceCatalogAppRegistry.Types.SyncAction
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-06-24@ of the Amazon Service Catalog App Registry SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "ServiceCatalogAppRegistry",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix =
        "servicecatalog-appregistry",
      Core._serviceSigningName = "servicecatalog",
      Core._serviceVersion = "2020-06-24",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "ServiceCatalogAppRegistry",
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

-- | The request has invalid or missing parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | There was a conflict when processing the request (for example, a
-- resource with the given name already exists within the account).
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The maximum number of resources per account has been reached.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The service is experiencing internal problems.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
