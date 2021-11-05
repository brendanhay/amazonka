{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Proton.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types
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

    -- * DeploymentStatus
    DeploymentStatus (..),

    -- * DeploymentUpdateType
    DeploymentUpdateType (..),

    -- * EnvironmentAccountConnectionRequesterAccountType
    EnvironmentAccountConnectionRequesterAccountType (..),

    -- * EnvironmentAccountConnectionStatus
    EnvironmentAccountConnectionStatus (..),

    -- * Provisioning
    Provisioning (..),

    -- * ServiceStatus
    ServiceStatus (..),

    -- * TemplateVersionStatus
    TemplateVersionStatus (..),

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_pipelineServiceRoleArn,

    -- * CompatibleEnvironmentTemplate
    CompatibleEnvironmentTemplate (..),
    newCompatibleEnvironmentTemplate,
    compatibleEnvironmentTemplate_majorVersion,
    compatibleEnvironmentTemplate_templateName,

    -- * CompatibleEnvironmentTemplateInput
    CompatibleEnvironmentTemplateInput (..),
    newCompatibleEnvironmentTemplateInput,
    compatibleEnvironmentTemplateInput_majorVersion,
    compatibleEnvironmentTemplateInput_templateName,

    -- * Environment
    Environment (..),
    newEnvironment,
    environment_deploymentStatusMessage,
    environment_environmentAccountId,
    environment_provisioning,
    environment_protonServiceRoleArn,
    environment_environmentAccountConnectionId,
    environment_spec,
    environment_description,
    environment_arn,
    environment_createdAt,
    environment_deploymentStatus,
    environment_lastDeploymentAttemptedAt,
    environment_lastDeploymentSucceededAt,
    environment_name,
    environment_templateMajorVersion,
    environment_templateMinorVersion,
    environment_templateName,

    -- * EnvironmentAccountConnection
    EnvironmentAccountConnection (..),
    newEnvironmentAccountConnection,
    environmentAccountConnection_arn,
    environmentAccountConnection_environmentAccountId,
    environmentAccountConnection_environmentName,
    environmentAccountConnection_id,
    environmentAccountConnection_lastModifiedAt,
    environmentAccountConnection_managementAccountId,
    environmentAccountConnection_requestedAt,
    environmentAccountConnection_roleArn,
    environmentAccountConnection_status,

    -- * EnvironmentAccountConnectionSummary
    EnvironmentAccountConnectionSummary (..),
    newEnvironmentAccountConnectionSummary,
    environmentAccountConnectionSummary_arn,
    environmentAccountConnectionSummary_environmentAccountId,
    environmentAccountConnectionSummary_environmentName,
    environmentAccountConnectionSummary_id,
    environmentAccountConnectionSummary_lastModifiedAt,
    environmentAccountConnectionSummary_managementAccountId,
    environmentAccountConnectionSummary_requestedAt,
    environmentAccountConnectionSummary_roleArn,
    environmentAccountConnectionSummary_status,

    -- * EnvironmentSummary
    EnvironmentSummary (..),
    newEnvironmentSummary,
    environmentSummary_deploymentStatusMessage,
    environmentSummary_environmentAccountId,
    environmentSummary_provisioning,
    environmentSummary_protonServiceRoleArn,
    environmentSummary_environmentAccountConnectionId,
    environmentSummary_description,
    environmentSummary_arn,
    environmentSummary_createdAt,
    environmentSummary_deploymentStatus,
    environmentSummary_lastDeploymentAttemptedAt,
    environmentSummary_lastDeploymentSucceededAt,
    environmentSummary_name,
    environmentSummary_templateMajorVersion,
    environmentSummary_templateMinorVersion,
    environmentSummary_templateName,

    -- * EnvironmentTemplate
    EnvironmentTemplate (..),
    newEnvironmentTemplate,
    environmentTemplate_provisioning,
    environmentTemplate_recommendedVersion,
    environmentTemplate_displayName,
    environmentTemplate_encryptionKey,
    environmentTemplate_description,
    environmentTemplate_arn,
    environmentTemplate_createdAt,
    environmentTemplate_lastModifiedAt,
    environmentTemplate_name,

    -- * EnvironmentTemplateFilter
    EnvironmentTemplateFilter (..),
    newEnvironmentTemplateFilter,
    environmentTemplateFilter_majorVersion,
    environmentTemplateFilter_templateName,

    -- * EnvironmentTemplateSummary
    EnvironmentTemplateSummary (..),
    newEnvironmentTemplateSummary,
    environmentTemplateSummary_provisioning,
    environmentTemplateSummary_recommendedVersion,
    environmentTemplateSummary_displayName,
    environmentTemplateSummary_description,
    environmentTemplateSummary_arn,
    environmentTemplateSummary_createdAt,
    environmentTemplateSummary_lastModifiedAt,
    environmentTemplateSummary_name,

    -- * EnvironmentTemplateVersion
    EnvironmentTemplateVersion (..),
    newEnvironmentTemplateVersion,
    environmentTemplateVersion_schema,
    environmentTemplateVersion_statusMessage,
    environmentTemplateVersion_recommendedMinorVersion,
    environmentTemplateVersion_description,
    environmentTemplateVersion_arn,
    environmentTemplateVersion_createdAt,
    environmentTemplateVersion_lastModifiedAt,
    environmentTemplateVersion_majorVersion,
    environmentTemplateVersion_minorVersion,
    environmentTemplateVersion_status,
    environmentTemplateVersion_templateName,

    -- * EnvironmentTemplateVersionSummary
    EnvironmentTemplateVersionSummary (..),
    newEnvironmentTemplateVersionSummary,
    environmentTemplateVersionSummary_statusMessage,
    environmentTemplateVersionSummary_recommendedMinorVersion,
    environmentTemplateVersionSummary_description,
    environmentTemplateVersionSummary_arn,
    environmentTemplateVersionSummary_createdAt,
    environmentTemplateVersionSummary_lastModifiedAt,
    environmentTemplateVersionSummary_majorVersion,
    environmentTemplateVersionSummary_minorVersion,
    environmentTemplateVersionSummary_status,
    environmentTemplateVersionSummary_templateName,

    -- * S3ObjectSource
    S3ObjectSource (..),
    newS3ObjectSource,
    s3ObjectSource_bucket,
    s3ObjectSource_key,

    -- * Service
    Service (..),
    newService,
    service_branchName,
    service_statusMessage,
    service_repositoryId,
    service_pipeline,
    service_description,
    service_repositoryConnectionArn,
    service_arn,
    service_createdAt,
    service_lastModifiedAt,
    service_name,
    service_spec,
    service_status,
    service_templateName,

    -- * ServiceInstance
    ServiceInstance (..),
    newServiceInstance,
    serviceInstance_deploymentStatusMessage,
    serviceInstance_spec,
    serviceInstance_arn,
    serviceInstance_createdAt,
    serviceInstance_deploymentStatus,
    serviceInstance_environmentName,
    serviceInstance_lastDeploymentAttemptedAt,
    serviceInstance_lastDeploymentSucceededAt,
    serviceInstance_name,
    serviceInstance_serviceName,
    serviceInstance_templateMajorVersion,
    serviceInstance_templateMinorVersion,
    serviceInstance_templateName,

    -- * ServiceInstanceSummary
    ServiceInstanceSummary (..),
    newServiceInstanceSummary,
    serviceInstanceSummary_deploymentStatusMessage,
    serviceInstanceSummary_arn,
    serviceInstanceSummary_createdAt,
    serviceInstanceSummary_deploymentStatus,
    serviceInstanceSummary_environmentName,
    serviceInstanceSummary_lastDeploymentAttemptedAt,
    serviceInstanceSummary_lastDeploymentSucceededAt,
    serviceInstanceSummary_name,
    serviceInstanceSummary_serviceName,
    serviceInstanceSummary_templateMajorVersion,
    serviceInstanceSummary_templateMinorVersion,
    serviceInstanceSummary_templateName,

    -- * ServicePipeline
    ServicePipeline (..),
    newServicePipeline,
    servicePipeline_deploymentStatusMessage,
    servicePipeline_spec,
    servicePipeline_arn,
    servicePipeline_createdAt,
    servicePipeline_deploymentStatus,
    servicePipeline_lastDeploymentAttemptedAt,
    servicePipeline_lastDeploymentSucceededAt,
    servicePipeline_templateMajorVersion,
    servicePipeline_templateMinorVersion,
    servicePipeline_templateName,

    -- * ServiceSummary
    ServiceSummary (..),
    newServiceSummary,
    serviceSummary_statusMessage,
    serviceSummary_description,
    serviceSummary_arn,
    serviceSummary_createdAt,
    serviceSummary_lastModifiedAt,
    serviceSummary_name,
    serviceSummary_status,
    serviceSummary_templateName,

    -- * ServiceTemplate
    ServiceTemplate (..),
    newServiceTemplate,
    serviceTemplate_recommendedVersion,
    serviceTemplate_displayName,
    serviceTemplate_encryptionKey,
    serviceTemplate_pipelineProvisioning,
    serviceTemplate_description,
    serviceTemplate_arn,
    serviceTemplate_createdAt,
    serviceTemplate_lastModifiedAt,
    serviceTemplate_name,

    -- * ServiceTemplateSummary
    ServiceTemplateSummary (..),
    newServiceTemplateSummary,
    serviceTemplateSummary_recommendedVersion,
    serviceTemplateSummary_displayName,
    serviceTemplateSummary_pipelineProvisioning,
    serviceTemplateSummary_description,
    serviceTemplateSummary_arn,
    serviceTemplateSummary_createdAt,
    serviceTemplateSummary_lastModifiedAt,
    serviceTemplateSummary_name,

    -- * ServiceTemplateVersion
    ServiceTemplateVersion (..),
    newServiceTemplateVersion,
    serviceTemplateVersion_schema,
    serviceTemplateVersion_statusMessage,
    serviceTemplateVersion_recommendedMinorVersion,
    serviceTemplateVersion_description,
    serviceTemplateVersion_arn,
    serviceTemplateVersion_compatibleEnvironmentTemplates,
    serviceTemplateVersion_createdAt,
    serviceTemplateVersion_lastModifiedAt,
    serviceTemplateVersion_majorVersion,
    serviceTemplateVersion_minorVersion,
    serviceTemplateVersion_status,
    serviceTemplateVersion_templateName,

    -- * ServiceTemplateVersionSummary
    ServiceTemplateVersionSummary (..),
    newServiceTemplateVersionSummary,
    serviceTemplateVersionSummary_statusMessage,
    serviceTemplateVersionSummary_recommendedMinorVersion,
    serviceTemplateVersionSummary_description,
    serviceTemplateVersionSummary_arn,
    serviceTemplateVersionSummary_createdAt,
    serviceTemplateVersionSummary_lastModifiedAt,
    serviceTemplateVersionSummary_majorVersion,
    serviceTemplateVersionSummary_minorVersion,
    serviceTemplateVersionSummary_status,
    serviceTemplateVersionSummary_templateName,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * TemplateVersionSourceInput
    TemplateVersionSourceInput (..),
    newTemplateVersionSourceInput,
    templateVersionSourceInput_s3,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.AccountSettings
import Amazonka.Proton.Types.CompatibleEnvironmentTemplate
import Amazonka.Proton.Types.CompatibleEnvironmentTemplateInput
import Amazonka.Proton.Types.DeploymentStatus
import Amazonka.Proton.Types.DeploymentUpdateType
import Amazonka.Proton.Types.Environment
import Amazonka.Proton.Types.EnvironmentAccountConnection
import Amazonka.Proton.Types.EnvironmentAccountConnectionRequesterAccountType
import Amazonka.Proton.Types.EnvironmentAccountConnectionStatus
import Amazonka.Proton.Types.EnvironmentAccountConnectionSummary
import Amazonka.Proton.Types.EnvironmentSummary
import Amazonka.Proton.Types.EnvironmentTemplate
import Amazonka.Proton.Types.EnvironmentTemplateFilter
import Amazonka.Proton.Types.EnvironmentTemplateSummary
import Amazonka.Proton.Types.EnvironmentTemplateVersion
import Amazonka.Proton.Types.EnvironmentTemplateVersionSummary
import Amazonka.Proton.Types.Provisioning
import Amazonka.Proton.Types.S3ObjectSource
import Amazonka.Proton.Types.Service
import Amazonka.Proton.Types.ServiceInstance
import Amazonka.Proton.Types.ServiceInstanceSummary
import Amazonka.Proton.Types.ServicePipeline
import Amazonka.Proton.Types.ServiceStatus
import Amazonka.Proton.Types.ServiceSummary
import Amazonka.Proton.Types.ServiceTemplate
import Amazonka.Proton.Types.ServiceTemplateSummary
import Amazonka.Proton.Types.ServiceTemplateVersion
import Amazonka.Proton.Types.ServiceTemplateVersionSummary
import Amazonka.Proton.Types.Tag
import Amazonka.Proton.Types.TemplateVersionSourceInput
import Amazonka.Proton.Types.TemplateVersionStatus
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-07-20@ of the Amazon Proton SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Proton",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "proton",
      Core._serviceSigningName = "proton",
      Core._serviceVersion = "2020-07-20",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Proton",
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

-- | The input is invalid or an out-of-range value was supplied for the input
-- parameter.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"

-- | There /isn\'t/ sufficient access for performing this action.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"

-- | The request /couldn\'t/ be made due to a conflicting operation or
-- resource.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | A quota was exceeded. For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-limits.html AWS Proton Quotas>
-- in the /AWS Proton Administrator Guide/.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"

-- | The request failed to register with the service.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"

-- | The requested resource /wasn\'t/ found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
