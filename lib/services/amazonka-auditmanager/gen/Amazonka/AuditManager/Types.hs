{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _AccessDeniedException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * AccountStatus
    AccountStatus (..),

    -- * ActionEnum
    ActionEnum (..),

    -- * AssessmentReportDestinationType
    AssessmentReportDestinationType (..),

    -- * AssessmentReportStatus
    AssessmentReportStatus (..),

    -- * AssessmentStatus
    AssessmentStatus (..),

    -- * ControlResponse
    ControlResponse (..),

    -- * ControlSetStatus
    ControlSetStatus (..),

    -- * ControlStatus
    ControlStatus (..),

    -- * ControlType
    ControlType (..),

    -- * DelegationStatus
    DelegationStatus (..),

    -- * FrameworkType
    FrameworkType (..),

    -- * KeywordInputType
    KeywordInputType (..),

    -- * ObjectTypeEnum
    ObjectTypeEnum (..),

    -- * RoleType
    RoleType (..),

    -- * SettingAttribute
    SettingAttribute (..),

    -- * SourceFrequency
    SourceFrequency (..),

    -- * SourceSetUpOption
    SourceSetUpOption (..),

    -- * SourceType
    SourceType (..),

    -- * AWSAccount
    AWSAccount (..),
    newAWSAccount,
    aWSAccount_name,
    aWSAccount_emailAddress,
    aWSAccount_id,

    -- * AWSService
    AWSService (..),
    newAWSService,
    aWSService_serviceName,

    -- * Assessment
    Assessment (..),
    newAssessment,
    assessment_framework,
    assessment_arn,
    assessment_awsAccount,
    assessment_metadata,
    assessment_tags,

    -- * AssessmentControl
    AssessmentControl (..),
    newAssessmentControl,
    assessmentControl_status,
    assessmentControl_evidenceCount,
    assessmentControl_response,
    assessmentControl_name,
    assessmentControl_id,
    assessmentControl_evidenceSources,
    assessmentControl_comments,
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_description,

    -- * AssessmentControlSet
    AssessmentControlSet (..),
    newAssessmentControlSet,
    assessmentControlSet_status,
    assessmentControlSet_controls,
    assessmentControlSet_roles,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_delegations,
    assessmentControlSet_systemEvidenceCount,
    assessmentControlSet_id,
    assessmentControlSet_description,

    -- * AssessmentEvidenceFolder
    AssessmentEvidenceFolder (..),
    newAssessmentEvidenceFolder,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_totalEvidence,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,

    -- * AssessmentFramework
    AssessmentFramework (..),
    newAssessmentFramework,
    assessmentFramework_arn,
    assessmentFramework_controlSets,
    assessmentFramework_metadata,
    assessmentFramework_id,

    -- * AssessmentFrameworkMetadata
    AssessmentFrameworkMetadata (..),
    newAssessmentFrameworkMetadata,
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_createdAt,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_type,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_description,

    -- * AssessmentMetadata
    AssessmentMetadata (..),
    newAssessmentMetadata,
    assessmentMetadata_creationTime,
    assessmentMetadata_status,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_roles,
    assessmentMetadata_delegations,
    assessmentMetadata_name,
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_scope,
    assessmentMetadata_complianceType,
    assessmentMetadata_id,
    assessmentMetadata_description,

    -- * AssessmentMetadataItem
    AssessmentMetadataItem (..),
    newAssessmentMetadataItem,
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_status,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_roles,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_name,
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_id,

    -- * AssessmentReport
    AssessmentReport (..),
    newAssessmentReport,
    assessmentReport_creationTime,
    assessmentReport_status,
    assessmentReport_awsAccountId,
    assessmentReport_name,
    assessmentReport_author,
    assessmentReport_id,
    assessmentReport_assessmentId,
    assessmentReport_description,
    assessmentReport_assessmentName,

    -- * AssessmentReportEvidenceError
    AssessmentReportEvidenceError (..),
    newAssessmentReportEvidenceError,
    assessmentReportEvidenceError_errorCode,
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,

    -- * AssessmentReportMetadata
    AssessmentReportMetadata (..),
    newAssessmentReportMetadata,
    assessmentReportMetadata_creationTime,
    assessmentReportMetadata_status,
    assessmentReportMetadata_name,
    assessmentReportMetadata_author,
    assessmentReportMetadata_id,
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_description,
    assessmentReportMetadata_assessmentName,

    -- * AssessmentReportsDestination
    AssessmentReportsDestination (..),
    newAssessmentReportsDestination,
    assessmentReportsDestination_destination,
    assessmentReportsDestination_destinationType,

    -- * BatchCreateDelegationByAssessmentError
    BatchCreateDelegationByAssessmentError (..),
    newBatchCreateDelegationByAssessmentError,
    batchCreateDelegationByAssessmentError_createDelegationRequest,
    batchCreateDelegationByAssessmentError_errorCode,
    batchCreateDelegationByAssessmentError_errorMessage,

    -- * BatchDeleteDelegationByAssessmentError
    BatchDeleteDelegationByAssessmentError (..),
    newBatchDeleteDelegationByAssessmentError,
    batchDeleteDelegationByAssessmentError_delegationId,
    batchDeleteDelegationByAssessmentError_errorCode,
    batchDeleteDelegationByAssessmentError_errorMessage,

    -- * BatchImportEvidenceToAssessmentControlError
    BatchImportEvidenceToAssessmentControlError (..),
    newBatchImportEvidenceToAssessmentControlError,
    batchImportEvidenceToAssessmentControlError_errorCode,
    batchImportEvidenceToAssessmentControlError_errorMessage,
    batchImportEvidenceToAssessmentControlError_manualEvidence,

    -- * ChangeLog
    ChangeLog (..),
    newChangeLog,
    changeLog_objectName,
    changeLog_createdAt,
    changeLog_objectType,
    changeLog_createdBy,
    changeLog_action,

    -- * Control
    Control (..),
    newControl,
    control_lastUpdatedBy,
    control_testingInformation,
    control_lastUpdatedAt,
    control_arn,
    control_createdAt,
    control_controlMappingSources,
    control_createdBy,
    control_actionPlanInstructions,
    control_controlSources,
    control_name,
    control_actionPlanTitle,
    control_id,
    control_type,
    control_description,
    control_tags,

    -- * ControlComment
    ControlComment (..),
    newControlComment,
    controlComment_authorName,
    controlComment_postedDate,
    controlComment_commentBody,

    -- * ControlMappingSource
    ControlMappingSource (..),
    newControlMappingSource,
    controlMappingSource_sourceName,
    controlMappingSource_sourceType,
    controlMappingSource_troubleshootingText,
    controlMappingSource_sourceId,
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceSetUpOption,

    -- * ControlMetadata
    ControlMetadata (..),
    newControlMetadata,
    controlMetadata_lastUpdatedAt,
    controlMetadata_arn,
    controlMetadata_createdAt,
    controlMetadata_controlSources,
    controlMetadata_name,
    controlMetadata_id,

    -- * ControlSet
    ControlSet (..),
    newControlSet,
    controlSet_controls,
    controlSet_name,
    controlSet_id,

    -- * CreateAssessmentFrameworkControl
    CreateAssessmentFrameworkControl (..),
    newCreateAssessmentFrameworkControl,
    createAssessmentFrameworkControl_id,

    -- * CreateAssessmentFrameworkControlSet
    CreateAssessmentFrameworkControlSet (..),
    newCreateAssessmentFrameworkControlSet,
    createAssessmentFrameworkControlSet_controls,
    createAssessmentFrameworkControlSet_name,

    -- * CreateControlMappingSource
    CreateControlMappingSource (..),
    newCreateControlMappingSource,
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceType,
    createControlMappingSource_troubleshootingText,
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceSetUpOption,

    -- * CreateDelegationRequest
    CreateDelegationRequest (..),
    newCreateDelegationRequest,
    createDelegationRequest_roleType,
    createDelegationRequest_controlSetId,
    createDelegationRequest_comment,
    createDelegationRequest_roleArn,

    -- * Delegation
    Delegation (..),
    newDelegation,
    delegation_roleType,
    delegation_creationTime,
    delegation_status,
    delegation_lastUpdated,
    delegation_controlSetId,
    delegation_createdBy,
    delegation_id,
    delegation_assessmentId,
    delegation_comment,
    delegation_roleArn,
    delegation_assessmentName,

    -- * DelegationMetadata
    DelegationMetadata (..),
    newDelegationMetadata,
    delegationMetadata_creationTime,
    delegationMetadata_status,
    delegationMetadata_controlSetName,
    delegationMetadata_id,
    delegationMetadata_assessmentId,
    delegationMetadata_roleArn,
    delegationMetadata_assessmentName,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_time,
    evidence_assessmentReportSelection,
    evidence_evidenceByType,
    evidence_complianceCheck,
    evidence_awsOrganization,
    evidence_awsAccountId,
    evidence_attributes,
    evidence_evidenceAwsAccountId,
    evidence_id,
    evidence_dataSource,
    evidence_evidenceFolderId,
    evidence_iamId,
    evidence_eventName,
    evidence_resourcesIncluded,
    evidence_eventSource,

    -- * Framework
    Framework (..),
    newFramework,
    framework_lastUpdatedBy,
    framework_lastUpdatedAt,
    framework_arn,
    framework_createdAt,
    framework_createdBy,
    framework_controlSets,
    framework_controlSources,
    framework_name,
    framework_complianceType,
    framework_id,
    framework_type,
    framework_logo,
    framework_description,
    framework_tags,

    -- * FrameworkMetadata
    FrameworkMetadata (..),
    newFrameworkMetadata,
    frameworkMetadata_name,
    frameworkMetadata_complianceType,
    frameworkMetadata_logo,
    frameworkMetadata_description,

    -- * ManualEvidence
    ManualEvidence (..),
    newManualEvidence,
    manualEvidence_s3ResourcePath,

    -- * Notification
    Notification (..),
    newNotification,
    notification_controlSetName,
    notification_controlSetId,
    notification_eventTime,
    notification_source,
    notification_id,
    notification_assessmentId,
    notification_description,
    notification_assessmentName,

    -- * Resource
    Resource (..),
    newResource,
    resource_arn,
    resource_value,

    -- * Role
    Role (..),
    newRole,
    role_roleType,
    role_roleArn,

    -- * Scope
    Scope (..),
    newScope,
    scope_awsAccounts,
    scope_awsServices,

    -- * ServiceMetadata
    ServiceMetadata (..),
    newServiceMetadata,
    serviceMetadata_category,
    serviceMetadata_name,
    serviceMetadata_displayName,
    serviceMetadata_description,

    -- * Settings
    Settings (..),
    newSettings,
    settings_kmsKey,
    settings_defaultAssessmentReportsDestination,
    settings_snsTopic,
    settings_defaultProcessOwners,
    settings_isAwsOrgEnabled,

    -- * SourceKeyword
    SourceKeyword (..),
    newSourceKeyword,
    sourceKeyword_keywordInputType,
    sourceKeyword_keywordValue,

    -- * URL
    URL (..),
    newURL,
    url_link,
    url_hyperlinkName,

    -- * UpdateAssessmentFrameworkControlSet
    UpdateAssessmentFrameworkControlSet (..),
    newUpdateAssessmentFrameworkControlSet,
    updateAssessmentFrameworkControlSet_controls,
    updateAssessmentFrameworkControlSet_id,
    updateAssessmentFrameworkControlSet_name,
  )
where

import Amazonka.AuditManager.Types.AWSAccount
import Amazonka.AuditManager.Types.AWSService
import Amazonka.AuditManager.Types.AccountStatus
import Amazonka.AuditManager.Types.ActionEnum
import Amazonka.AuditManager.Types.Assessment
import Amazonka.AuditManager.Types.AssessmentControl
import Amazonka.AuditManager.Types.AssessmentControlSet
import Amazonka.AuditManager.Types.AssessmentEvidenceFolder
import Amazonka.AuditManager.Types.AssessmentFramework
import Amazonka.AuditManager.Types.AssessmentFrameworkMetadata
import Amazonka.AuditManager.Types.AssessmentMetadata
import Amazonka.AuditManager.Types.AssessmentMetadataItem
import Amazonka.AuditManager.Types.AssessmentReport
import Amazonka.AuditManager.Types.AssessmentReportDestinationType
import Amazonka.AuditManager.Types.AssessmentReportEvidenceError
import Amazonka.AuditManager.Types.AssessmentReportMetadata
import Amazonka.AuditManager.Types.AssessmentReportStatus
import Amazonka.AuditManager.Types.AssessmentReportsDestination
import Amazonka.AuditManager.Types.AssessmentStatus
import Amazonka.AuditManager.Types.BatchCreateDelegationByAssessmentError
import Amazonka.AuditManager.Types.BatchDeleteDelegationByAssessmentError
import Amazonka.AuditManager.Types.BatchImportEvidenceToAssessmentControlError
import Amazonka.AuditManager.Types.ChangeLog
import Amazonka.AuditManager.Types.Control
import Amazonka.AuditManager.Types.ControlComment
import Amazonka.AuditManager.Types.ControlMappingSource
import Amazonka.AuditManager.Types.ControlMetadata
import Amazonka.AuditManager.Types.ControlResponse
import Amazonka.AuditManager.Types.ControlSet
import Amazonka.AuditManager.Types.ControlSetStatus
import Amazonka.AuditManager.Types.ControlStatus
import Amazonka.AuditManager.Types.ControlType
import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControl
import Amazonka.AuditManager.Types.CreateAssessmentFrameworkControlSet
import Amazonka.AuditManager.Types.CreateControlMappingSource
import Amazonka.AuditManager.Types.CreateDelegationRequest
import Amazonka.AuditManager.Types.Delegation
import Amazonka.AuditManager.Types.DelegationMetadata
import Amazonka.AuditManager.Types.DelegationStatus
import Amazonka.AuditManager.Types.Evidence
import Amazonka.AuditManager.Types.Framework
import Amazonka.AuditManager.Types.FrameworkMetadata
import Amazonka.AuditManager.Types.FrameworkType
import Amazonka.AuditManager.Types.KeywordInputType
import Amazonka.AuditManager.Types.ManualEvidence
import Amazonka.AuditManager.Types.Notification
import Amazonka.AuditManager.Types.ObjectTypeEnum
import Amazonka.AuditManager.Types.Resource
import Amazonka.AuditManager.Types.Role
import Amazonka.AuditManager.Types.RoleType
import Amazonka.AuditManager.Types.Scope
import Amazonka.AuditManager.Types.ServiceMetadata
import Amazonka.AuditManager.Types.SettingAttribute
import Amazonka.AuditManager.Types.Settings
import Amazonka.AuditManager.Types.SourceFrequency
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.SourceSetUpOption
import Amazonka.AuditManager.Types.SourceType
import Amazonka.AuditManager.Types.URL
import Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Audit Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "AuditManager",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "auditmanager",
      Core._serviceSigningName = "auditmanager",
      Core._serviceVersion = "2017-07-25",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "AuditManager",
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

-- | Your account is not registered with Audit Manager. Check the delegated
-- administrator setup on the Audit Manager settings page, and try again.
_AccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AccessDeniedException =
  Core._MatchServiceError
    defaultService
    "AccessDeniedException"
    Prelude.. Core.hasStatus 403

-- | An internal service error occurred during the processing of your
-- request. Try again later.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | The resource specified in the request cannot be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
