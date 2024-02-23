{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMSAP.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMSAP.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * ApplicationStatus
    ApplicationStatus (..),

    -- * ApplicationType
    ApplicationType (..),

    -- * ComponentStatus
    ComponentStatus (..),

    -- * ComponentType
    ComponentType (..),

    -- * CredentialType
    CredentialType (..),

    -- * DatabaseStatus
    DatabaseStatus (..),

    -- * DatabaseType
    DatabaseType (..),

    -- * HostRole
    HostRole (..),

    -- * OperationStatus
    OperationStatus (..),

    -- * PermissionActionType
    PermissionActionType (..),

    -- * Application
    Application (..),
    newApplication,
    application_appRegistryArn,
    application_arn,
    application_components,
    application_id,
    application_lastUpdated,
    application_status,
    application_statusMessage,
    application_type,

    -- * ApplicationCredential
    ApplicationCredential (..),
    newApplicationCredential,
    applicationCredential_databaseName,
    applicationCredential_credentialType,
    applicationCredential_secretId,

    -- * ApplicationSummary
    ApplicationSummary (..),
    newApplicationSummary,
    applicationSummary_arn,
    applicationSummary_id,
    applicationSummary_tags,
    applicationSummary_type,

    -- * Component
    Component (..),
    newComponent,
    component_applicationId,
    component_componentId,
    component_componentType,
    component_databases,
    component_hosts,
    component_lastUpdated,
    component_primaryHost,
    component_status,

    -- * ComponentSummary
    ComponentSummary (..),
    newComponentSummary,
    componentSummary_applicationId,
    componentSummary_componentId,
    componentSummary_componentType,
    componentSummary_tags,

    -- * Database
    Database (..),
    newDatabase,
    database_applicationId,
    database_arn,
    database_componentId,
    database_credentials,
    database_databaseId,
    database_databaseName,
    database_databaseType,
    database_lastUpdated,
    database_primaryHost,
    database_sQLPort,
    database_status,

    -- * DatabaseSummary
    DatabaseSummary (..),
    newDatabaseSummary,
    databaseSummary_applicationId,
    databaseSummary_arn,
    databaseSummary_componentId,
    databaseSummary_databaseId,
    databaseSummary_databaseType,
    databaseSummary_tags,

    -- * Host
    Host (..),
    newHost,
    host_hostIp,
    host_hostName,
    host_hostRole,
    host_instanceId,

    -- * Operation
    Operation (..),
    newOperation,
    operation_endTime,
    operation_id,
    operation_lastUpdatedTime,
    operation_properties,
    operation_resourceArn,
    operation_resourceId,
    operation_resourceType,
    operation_startTime,
    operation_status,
    operation_statusMessage,
    operation_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMSAP.Types.Application
import Amazonka.SSMSAP.Types.ApplicationCredential
import Amazonka.SSMSAP.Types.ApplicationStatus
import Amazonka.SSMSAP.Types.ApplicationSummary
import Amazonka.SSMSAP.Types.ApplicationType
import Amazonka.SSMSAP.Types.Component
import Amazonka.SSMSAP.Types.ComponentStatus
import Amazonka.SSMSAP.Types.ComponentSummary
import Amazonka.SSMSAP.Types.ComponentType
import Amazonka.SSMSAP.Types.CredentialType
import Amazonka.SSMSAP.Types.Database
import Amazonka.SSMSAP.Types.DatabaseStatus
import Amazonka.SSMSAP.Types.DatabaseSummary
import Amazonka.SSMSAP.Types.DatabaseType
import Amazonka.SSMSAP.Types.Host
import Amazonka.SSMSAP.Types.HostRole
import Amazonka.SSMSAP.Types.Operation
import Amazonka.SSMSAP.Types.OperationStatus
import Amazonka.SSMSAP.Types.PermissionActionType
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-05-10@ of the Amazon Systems Manager for SAP SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "SSMSAP",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "ssm-sap",
      Core.signingName = "ssm-sap",
      Core.version = "2018-05-10",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "SSMSAP",
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
      | Lens.has (Core.hasStatus 502) e =
          Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
          Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
          Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
          Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
          Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
          Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
          Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
