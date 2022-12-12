{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AMP.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types
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

    -- * AlertManagerDefinitionStatusCode
    AlertManagerDefinitionStatusCode (..),

    -- * LoggingConfigurationStatusCode
    LoggingConfigurationStatusCode (..),

    -- * RuleGroupsNamespaceStatusCode
    RuleGroupsNamespaceStatusCode (..),

    -- * WorkspaceStatusCode
    WorkspaceStatusCode (..),

    -- * AlertManagerDefinitionDescription
    AlertManagerDefinitionDescription (..),
    newAlertManagerDefinitionDescription,
    alertManagerDefinitionDescription_createdAt,
    alertManagerDefinitionDescription_data,
    alertManagerDefinitionDescription_modifiedAt,
    alertManagerDefinitionDescription_status,

    -- * AlertManagerDefinitionStatus
    AlertManagerDefinitionStatus (..),
    newAlertManagerDefinitionStatus,
    alertManagerDefinitionStatus_statusReason,
    alertManagerDefinitionStatus_statusCode,

    -- * LoggingConfigurationMetadata
    LoggingConfigurationMetadata (..),
    newLoggingConfigurationMetadata,
    loggingConfigurationMetadata_createdAt,
    loggingConfigurationMetadata_logGroupArn,
    loggingConfigurationMetadata_modifiedAt,
    loggingConfigurationMetadata_status,
    loggingConfigurationMetadata_workspace,

    -- * LoggingConfigurationStatus
    LoggingConfigurationStatus (..),
    newLoggingConfigurationStatus,
    loggingConfigurationStatus_statusReason,
    loggingConfigurationStatus_statusCode,

    -- * RuleGroupsNamespaceDescription
    RuleGroupsNamespaceDescription (..),
    newRuleGroupsNamespaceDescription,
    ruleGroupsNamespaceDescription_tags,
    ruleGroupsNamespaceDescription_arn,
    ruleGroupsNamespaceDescription_createdAt,
    ruleGroupsNamespaceDescription_data,
    ruleGroupsNamespaceDescription_modifiedAt,
    ruleGroupsNamespaceDescription_name,
    ruleGroupsNamespaceDescription_status,

    -- * RuleGroupsNamespaceStatus
    RuleGroupsNamespaceStatus (..),
    newRuleGroupsNamespaceStatus,
    ruleGroupsNamespaceStatus_statusReason,
    ruleGroupsNamespaceStatus_statusCode,

    -- * RuleGroupsNamespaceSummary
    RuleGroupsNamespaceSummary (..),
    newRuleGroupsNamespaceSummary,
    ruleGroupsNamespaceSummary_tags,
    ruleGroupsNamespaceSummary_arn,
    ruleGroupsNamespaceSummary_createdAt,
    ruleGroupsNamespaceSummary_modifiedAt,
    ruleGroupsNamespaceSummary_name,
    ruleGroupsNamespaceSummary_status,

    -- * WorkspaceDescription
    WorkspaceDescription (..),
    newWorkspaceDescription,
    workspaceDescription_alias,
    workspaceDescription_prometheusEndpoint,
    workspaceDescription_tags,
    workspaceDescription_arn,
    workspaceDescription_createdAt,
    workspaceDescription_status,
    workspaceDescription_workspaceId,

    -- * WorkspaceStatus
    WorkspaceStatus (..),
    newWorkspaceStatus,
    workspaceStatus_statusCode,

    -- * WorkspaceSummary
    WorkspaceSummary (..),
    newWorkspaceSummary,
    workspaceSummary_alias,
    workspaceSummary_tags,
    workspaceSummary_arn,
    workspaceSummary_createdAt,
    workspaceSummary_status,
    workspaceSummary_workspaceId,
  )
where

import Amazonka.AMP.Types.AlertManagerDefinitionDescription
import Amazonka.AMP.Types.AlertManagerDefinitionStatus
import Amazonka.AMP.Types.AlertManagerDefinitionStatusCode
import Amazonka.AMP.Types.LoggingConfigurationMetadata
import Amazonka.AMP.Types.LoggingConfigurationStatus
import Amazonka.AMP.Types.LoggingConfigurationStatusCode
import Amazonka.AMP.Types.RuleGroupsNamespaceDescription
import Amazonka.AMP.Types.RuleGroupsNamespaceStatus
import Amazonka.AMP.Types.RuleGroupsNamespaceStatusCode
import Amazonka.AMP.Types.RuleGroupsNamespaceSummary
import Amazonka.AMP.Types.WorkspaceDescription
import Amazonka.AMP.Types.WorkspaceStatus
import Amazonka.AMP.Types.WorkspaceStatusCode
import Amazonka.AMP.Types.WorkspaceSummary
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-08-01@ of the Amazon Prometheus Service SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AMP",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "aps",
      Core.signingName = "aps",
      Core.version = "2020-08-01",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AMP",
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

-- | User does not have sufficient access to perform this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | Updating or deleting a resource can cause an inconsistent state.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 409

-- | Unexpected error during processing of request.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Request references a resource which does not exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Request would cause a service quota to be exceeded.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | Request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 429

-- | The input fails to satisfy the constraints specified by an AWS service.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
