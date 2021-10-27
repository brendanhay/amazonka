{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Grafana.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Grafana.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _ConflictException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AccountAccessType
    AccountAccessType (..),

    -- * AuthenticationProviderTypes
    AuthenticationProviderTypes (..),

    -- * DataSourceType
    DataSourceType (..),

    -- * LicenseType
    LicenseType (..),

    -- * NotificationDestinationType
    NotificationDestinationType (..),

    -- * PermissionType
    PermissionType (..),

    -- * Role
    Role (..),

    -- * SamlConfigurationStatus
    SamlConfigurationStatus (..),

    -- * UpdateAction
    UpdateAction (..),

    -- * UserType
    UserType (..),

    -- * WorkspaceStatus
    WorkspaceStatus (..),

    -- * AssertionAttributes
    AssertionAttributes (..),
    newAssertionAttributes,
    assertionAttributes_email,
    assertionAttributes_groups,
    assertionAttributes_org,
    assertionAttributes_role,
    assertionAttributes_name,
    assertionAttributes_login,

    -- * AuthenticationDescription
    AuthenticationDescription (..),
    newAuthenticationDescription,
    authenticationDescription_awsSso,
    authenticationDescription_saml,
    authenticationDescription_providers,

    -- * AuthenticationSummary
    AuthenticationSummary (..),
    newAuthenticationSummary,
    authenticationSummary_samlConfigurationStatus,
    authenticationSummary_providers,

    -- * AwsSsoAuthentication
    AwsSsoAuthentication (..),
    newAwsSsoAuthentication,
    awsSsoAuthentication_ssoClientId,

    -- * IdpMetadata
    IdpMetadata (..),
    newIdpMetadata,
    idpMetadata_url,
    idpMetadata_xml,

    -- * PermissionEntry
    PermissionEntry (..),
    newPermissionEntry,
    permissionEntry_role,
    permissionEntry_user,

    -- * RoleValues
    RoleValues (..),
    newRoleValues,
    roleValues_admin,
    roleValues_editor,

    -- * SamlAuthentication
    SamlAuthentication (..),
    newSamlAuthentication,
    samlAuthentication_configuration,
    samlAuthentication_status,

    -- * SamlConfiguration
    SamlConfiguration (..),
    newSamlConfiguration,
    samlConfiguration_loginValidityDuration,
    samlConfiguration_assertionAttributes,
    samlConfiguration_allowedOrganizations,
    samlConfiguration_roleValues,
    samlConfiguration_idpMetadata,

    -- * UpdateError
    UpdateError (..),
    newUpdateError,
    updateError_causedBy,
    updateError_code,
    updateError_message,

    -- * UpdateInstruction
    UpdateInstruction (..),
    newUpdateInstruction,
    updateInstruction_action,
    updateInstruction_role,
    updateInstruction_users,

    -- * User
    User (..),
    newUser,
    user_id,
    user_type,

    -- * WorkspaceDescription
    WorkspaceDescription (..),
    newWorkspaceDescription,
    workspaceDescription_workspaceRoleArn,
    workspaceDescription_freeTrialExpiration,
    workspaceDescription_licenseType,
    workspaceDescription_permissionType,
    workspaceDescription_name,
    workspaceDescription_notificationDestinations,
    workspaceDescription_accountAccessType,
    workspaceDescription_licenseExpiration,
    workspaceDescription_organizationRoleName,
    workspaceDescription_stackSetName,
    workspaceDescription_organizationalUnits,
    workspaceDescription_description,
    workspaceDescription_freeTrialConsumed,
    workspaceDescription_authentication,
    workspaceDescription_created,
    workspaceDescription_dataSources,
    workspaceDescription_endpoint,
    workspaceDescription_grafanaVersion,
    workspaceDescription_id,
    workspaceDescription_modified,
    workspaceDescription_status,

    -- * WorkspaceSummary
    WorkspaceSummary (..),
    newWorkspaceSummary,
    workspaceSummary_name,
    workspaceSummary_notificationDestinations,
    workspaceSummary_description,
    workspaceSummary_authentication,
    workspaceSummary_created,
    workspaceSummary_endpoint,
    workspaceSummary_grafanaVersion,
    workspaceSummary_id,
    workspaceSummary_modified,
    workspaceSummary_status,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Grafana.Types.AccountAccessType
import Network.AWS.Grafana.Types.AssertionAttributes
import Network.AWS.Grafana.Types.AuthenticationDescription
import Network.AWS.Grafana.Types.AuthenticationProviderTypes
import Network.AWS.Grafana.Types.AuthenticationSummary
import Network.AWS.Grafana.Types.AwsSsoAuthentication
import Network.AWS.Grafana.Types.DataSourceType
import Network.AWS.Grafana.Types.IdpMetadata
import Network.AWS.Grafana.Types.LicenseType
import Network.AWS.Grafana.Types.NotificationDestinationType
import Network.AWS.Grafana.Types.PermissionEntry
import Network.AWS.Grafana.Types.PermissionType
import Network.AWS.Grafana.Types.Role
import Network.AWS.Grafana.Types.RoleValues
import Network.AWS.Grafana.Types.SamlAuthentication
import Network.AWS.Grafana.Types.SamlConfiguration
import Network.AWS.Grafana.Types.SamlConfigurationStatus
import Network.AWS.Grafana.Types.UpdateAction
import Network.AWS.Grafana.Types.UpdateError
import Network.AWS.Grafana.Types.UpdateInstruction
import Network.AWS.Grafana.Types.User
import Network.AWS.Grafana.Types.UserType
import Network.AWS.Grafana.Types.WorkspaceDescription
import Network.AWS.Grafana.Types.WorkspaceStatus
import Network.AWS.Grafana.Types.WorkspaceSummary
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2020-08-18@ of the Amazon Managed Grafana SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Grafana",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "grafana",
      Core._serviceSigningName = "grafana",
      Core._serviceVersion = "2020-08-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Grafana",
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

-- | The value of a parameter in the request caused an error.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A resource was in an inconsistent state during an update or a deletion.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied because of request throttling. Retry the request.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | Unexpected error while processing the request. Retry the request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
