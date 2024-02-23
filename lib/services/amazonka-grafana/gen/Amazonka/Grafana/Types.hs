{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Grafana.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
    _ThrottlingException,
    _ValidationException,

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
    assertionAttributes_login,
    assertionAttributes_name,
    assertionAttributes_org,
    assertionAttributes_role,

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
    samlConfiguration_allowedOrganizations,
    samlConfiguration_assertionAttributes,
    samlConfiguration_loginValidityDuration,
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

    -- * VpcConfiguration
    VpcConfiguration (..),
    newVpcConfiguration,
    vpcConfiguration_securityGroupIds,
    vpcConfiguration_subnetIds,

    -- * WorkspaceDescription
    WorkspaceDescription (..),
    newWorkspaceDescription,
    workspaceDescription_accountAccessType,
    workspaceDescription_description,
    workspaceDescription_freeTrialConsumed,
    workspaceDescription_freeTrialExpiration,
    workspaceDescription_licenseExpiration,
    workspaceDescription_licenseType,
    workspaceDescription_name,
    workspaceDescription_notificationDestinations,
    workspaceDescription_organizationRoleName,
    workspaceDescription_organizationalUnits,
    workspaceDescription_permissionType,
    workspaceDescription_stackSetName,
    workspaceDescription_tags,
    workspaceDescription_vpcConfiguration,
    workspaceDescription_workspaceRoleArn,
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
    workspaceSummary_description,
    workspaceSummary_name,
    workspaceSummary_notificationDestinations,
    workspaceSummary_tags,
    workspaceSummary_authentication,
    workspaceSummary_created,
    workspaceSummary_endpoint,
    workspaceSummary_grafanaVersion,
    workspaceSummary_id,
    workspaceSummary_modified,
    workspaceSummary_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Grafana.Types.AccountAccessType
import Amazonka.Grafana.Types.AssertionAttributes
import Amazonka.Grafana.Types.AuthenticationDescription
import Amazonka.Grafana.Types.AuthenticationProviderTypes
import Amazonka.Grafana.Types.AuthenticationSummary
import Amazonka.Grafana.Types.AwsSsoAuthentication
import Amazonka.Grafana.Types.DataSourceType
import Amazonka.Grafana.Types.IdpMetadata
import Amazonka.Grafana.Types.LicenseType
import Amazonka.Grafana.Types.NotificationDestinationType
import Amazonka.Grafana.Types.PermissionEntry
import Amazonka.Grafana.Types.PermissionType
import Amazonka.Grafana.Types.Role
import Amazonka.Grafana.Types.RoleValues
import Amazonka.Grafana.Types.SamlAuthentication
import Amazonka.Grafana.Types.SamlConfiguration
import Amazonka.Grafana.Types.SamlConfigurationStatus
import Amazonka.Grafana.Types.UpdateAction
import Amazonka.Grafana.Types.UpdateError
import Amazonka.Grafana.Types.UpdateInstruction
import Amazonka.Grafana.Types.User
import Amazonka.Grafana.Types.UserType
import Amazonka.Grafana.Types.VpcConfiguration
import Amazonka.Grafana.Types.WorkspaceDescription
import Amazonka.Grafana.Types.WorkspaceStatus
import Amazonka.Grafana.Types.WorkspaceSummary
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-18@ of the Amazon Managed Grafana SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Grafana",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "grafana",
      Core.signingName = "grafana",
      Core.version = "2020-08-18",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Grafana",
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

-- | You do not have sufficient permissions to perform this action.
_AccessDeniedException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | A resource was in an inconsistent state during an update or a deletion.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Unexpected error while processing the request. Retry the request.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The request references a resource that does not exist.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The request was denied because of request throttling. Retry the request.
_ThrottlingException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The value of a parameter in the request caused an error.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
