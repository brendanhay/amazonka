{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ConflictException,
    _ValidationException,

    -- * ResourceGroupState
    ResourceGroupState (..),

    -- * ResourceType
    ResourceType (..),

    -- * SyncAction
    SyncAction (..),

    -- * AppRegistryConfiguration
    AppRegistryConfiguration (..),
    newAppRegistryConfiguration,
    appRegistryConfiguration_tagQueryConfiguration,

    -- * Application
    Application (..),
    newApplication,
    application_tags,
    application_name,
    application_arn,
    application_description,
    application_id,
    application_creationTime,
    application_lastUpdateTime,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_name,
    applicationSummary_arn,
    applicationSummary_description,
    applicationSummary_id,
    applicationSummary_creationTime,
    applicationSummary_lastUpdateTime,

    -- * AttributeGroup
    AttributeGroup (..),
    newAttributeGroup,
    attributeGroup_tags,
    attributeGroup_name,
    attributeGroup_arn,
    attributeGroup_description,
    attributeGroup_id,
    attributeGroup_creationTime,
    attributeGroup_lastUpdateTime,

    -- * AttributeGroupDetails
    AttributeGroupDetails (..),
    newAttributeGroupDetails,
    attributeGroupDetails_name,
    attributeGroupDetails_arn,
    attributeGroupDetails_id,

    -- * AttributeGroupSummary
    AttributeGroupSummary (..),
    newAttributeGroupSummary,
    attributeGroupSummary_name,
    attributeGroupSummary_arn,
    attributeGroupSummary_description,
    attributeGroupSummary_id,
    attributeGroupSummary_creationTime,
    attributeGroupSummary_lastUpdateTime,

    -- * Integrations
    Integrations (..),
    newIntegrations,
    integrations_resourceGroup,

    -- * Resource
    Resource (..),
    newResource,
    resource_name,
    resource_integrations,
    resource_arn,
    resource_associationTime,

    -- * ResourceDetails
    ResourceDetails (..),
    newResourceDetails,
    resourceDetails_tagValue,

    -- * ResourceGroup
    ResourceGroup (..),
    newResourceGroup,
    resourceGroup_errorMessage,
    resourceGroup_arn,
    resourceGroup_state,

    -- * ResourceInfo
    ResourceInfo (..),
    newResourceInfo,
    resourceInfo_resourceType,
    resourceInfo_name,
    resourceInfo_arn,
    resourceInfo_resourceDetails,

    -- * ResourceIntegrations
    ResourceIntegrations (..),
    newResourceIntegrations,
    resourceIntegrations_resourceGroup,

    -- * TagQueryConfiguration
    TagQueryConfiguration (..),
    newTagQueryConfiguration,
    tagQueryConfiguration_tagKey,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalogAppRegistry.Types.AppRegistryConfiguration
import Amazonka.ServiceCatalogAppRegistry.Types.Application
import Amazonka.ServiceCatalogAppRegistry.Types.ApplicationSummary
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroup
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupDetails
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupSummary
import Amazonka.ServiceCatalogAppRegistry.Types.Integrations
import Amazonka.ServiceCatalogAppRegistry.Types.Resource
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceDetails
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroup
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroupState
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceIntegrations
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceType
import Amazonka.ServiceCatalogAppRegistry.Types.SyncAction
import Amazonka.ServiceCatalogAppRegistry.Types.TagQueryConfiguration
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-06-24@ of the Amazon Service Catalog App Registry SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev =
        "ServiceCatalogAppRegistry",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "servicecatalog-appregistry",
      Core.signingName = "servicecatalog",
      Core.version = "2020-06-24",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error =
        Core.parseJSONError "ServiceCatalogAppRegistry",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | The service is experiencing internal problems.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The maximum number of resources per account has been reached.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The specified resource does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | There was a conflict when processing the request (for example, a
-- resource with the given name already exists within the account).
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request has invalid or missing parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
