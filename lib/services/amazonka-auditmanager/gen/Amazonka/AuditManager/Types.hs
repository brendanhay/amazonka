{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AuditManager.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AccessDeniedException,
    _InternalServerException,
    _ServiceQuotaExceededException,
    _ResourceNotFoundException,
    _ThrottlingException,
    _ValidationException,

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

    -- * EvidenceFinderBackfillStatus
    EvidenceFinderBackfillStatus (..),

    -- * EvidenceFinderEnablementStatus
    EvidenceFinderEnablementStatus (..),

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

    -- * ShareRequestAction
    ShareRequestAction (..),

    -- * ShareRequestStatus
    ShareRequestStatus (..),

    -- * ShareRequestType
    ShareRequestType (..),

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
    aWSAccount_id,
    aWSAccount_emailAddress,

    -- * AWSService
    AWSService (..),
    newAWSService,
    aWSService_serviceName,

    -- * Assessment
    Assessment (..),
    newAssessment,
    assessment_tags,
    assessment_metadata,
    assessment_awsAccount,
    assessment_arn,
    assessment_framework,

    -- * AssessmentControl
    AssessmentControl (..),
    newAssessmentControl,
    assessmentControl_name,
    assessmentControl_evidenceCount,
    assessmentControl_response,
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_status,
    assessmentControl_description,
    assessmentControl_id,
    assessmentControl_comments,
    assessmentControl_evidenceSources,

    -- * AssessmentControlSet
    AssessmentControlSet (..),
    newAssessmentControlSet,
    assessmentControlSet_systemEvidenceCount,
    assessmentControlSet_status,
    assessmentControlSet_description,
    assessmentControlSet_id,
    assessmentControlSet_controls,
    assessmentControlSet_delegations,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_roles,

    -- * AssessmentEvidenceFolder
    AssessmentEvidenceFolder (..),
    newAssessmentEvidenceFolder,
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_totalEvidence,
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,

    -- * AssessmentFramework
    AssessmentFramework (..),
    newAssessmentFramework,
    assessmentFramework_metadata,
    assessmentFramework_arn,
    assessmentFramework_id,
    assessmentFramework_controlSets,

    -- * AssessmentFrameworkMetadata
    AssessmentFrameworkMetadata (..),
    newAssessmentFrameworkMetadata,
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_type,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_description,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_createdAt,

    -- * AssessmentFrameworkShareRequest
    AssessmentFrameworkShareRequest (..),
    newAssessmentFrameworkShareRequest,
    assessmentFrameworkShareRequest_customControlsCount,
    assessmentFrameworkShareRequest_destinationAccount,
    assessmentFrameworkShareRequest_expirationTime,
    assessmentFrameworkShareRequest_frameworkDescription,
    assessmentFrameworkShareRequest_status,
    assessmentFrameworkShareRequest_id,
    assessmentFrameworkShareRequest_frameworkName,
    assessmentFrameworkShareRequest_frameworkId,
    assessmentFrameworkShareRequest_lastUpdated,
    assessmentFrameworkShareRequest_comment,
    assessmentFrameworkShareRequest_creationTime,
    assessmentFrameworkShareRequest_destinationRegion,
    assessmentFrameworkShareRequest_complianceType,
    assessmentFrameworkShareRequest_standardControlsCount,
    assessmentFrameworkShareRequest_sourceAccount,

    -- * AssessmentMetadata
    AssessmentMetadata (..),
    newAssessmentMetadata,
    assessmentMetadata_name,
    assessmentMetadata_status,
    assessmentMetadata_description,
    assessmentMetadata_id,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_scope,
    assessmentMetadata_creationTime,
    assessmentMetadata_delegations,
    assessmentMetadata_complianceType,
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_roles,

    -- * AssessmentMetadataItem
    AssessmentMetadataItem (..),
    newAssessmentMetadataItem,
    assessmentMetadataItem_name,
    assessmentMetadataItem_status,
    assessmentMetadataItem_id,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_roles,

    -- * AssessmentReport
    AssessmentReport (..),
    newAssessmentReport,
    assessmentReport_awsAccountId,
    assessmentReport_author,
    assessmentReport_name,
    assessmentReport_assessmentId,
    assessmentReport_assessmentName,
    assessmentReport_status,
    assessmentReport_description,
    assessmentReport_id,
    assessmentReport_creationTime,

    -- * AssessmentReportEvidenceError
    AssessmentReportEvidenceError (..),
    newAssessmentReportEvidenceError,
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,
    assessmentReportEvidenceError_errorCode,

    -- * AssessmentReportMetadata
    AssessmentReportMetadata (..),
    newAssessmentReportMetadata,
    assessmentReportMetadata_author,
    assessmentReportMetadata_name,
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_assessmentName,
    assessmentReportMetadata_status,
    assessmentReportMetadata_description,
    assessmentReportMetadata_id,
    assessmentReportMetadata_creationTime,

    -- * AssessmentReportsDestination
    AssessmentReportsDestination (..),
    newAssessmentReportsDestination,
    assessmentReportsDestination_destination,
    assessmentReportsDestination_destinationType,

    -- * BatchCreateDelegationByAssessmentError
    BatchCreateDelegationByAssessmentError (..),
    newBatchCreateDelegationByAssessmentError,
    batchCreateDelegationByAssessmentError_createDelegationRequest,
    batchCreateDelegationByAssessmentError_errorMessage,
    batchCreateDelegationByAssessmentError_errorCode,

    -- * BatchDeleteDelegationByAssessmentError
    BatchDeleteDelegationByAssessmentError (..),
    newBatchDeleteDelegationByAssessmentError,
    batchDeleteDelegationByAssessmentError_errorMessage,
    batchDeleteDelegationByAssessmentError_delegationId,
    batchDeleteDelegationByAssessmentError_errorCode,

    -- * BatchImportEvidenceToAssessmentControlError
    BatchImportEvidenceToAssessmentControlError (..),
    newBatchImportEvidenceToAssessmentControlError,
    batchImportEvidenceToAssessmentControlError_errorMessage,
    batchImportEvidenceToAssessmentControlError_errorCode,
    batchImportEvidenceToAssessmentControlError_manualEvidence,

    -- * ChangeLog
    ChangeLog (..),
    newChangeLog,
    changeLog_action,
    changeLog_objectName,
    changeLog_createdBy,
    changeLog_createdAt,
    changeLog_objectType,

    -- * Control
    Control (..),
    newControl,
    control_tags,
    control_name,
    control_type,
    control_lastUpdatedAt,
    control_actionPlanInstructions,
    control_arn,
    control_description,
    control_id,
    control_actionPlanTitle,
    control_controlMappingSources,
    control_createdBy,
    control_createdAt,
    control_lastUpdatedBy,
    control_controlSources,
    control_testingInformation,

    -- * ControlComment
    ControlComment (..),
    newControlComment,
    controlComment_postedDate,
    controlComment_authorName,
    controlComment_commentBody,

    -- * ControlDomainInsights
    ControlDomainInsights (..),
    newControlDomainInsights,
    controlDomainInsights_evidenceInsights,
    controlDomainInsights_name,
    controlDomainInsights_totalControlsCount,
    controlDomainInsights_id,
    controlDomainInsights_lastUpdated,
    controlDomainInsights_controlsCountByNoncompliantEvidence,

    -- * ControlInsightsMetadataByAssessmentItem
    ControlInsightsMetadataByAssessmentItem (..),
    newControlInsightsMetadataByAssessmentItem,
    controlInsightsMetadataByAssessmentItem_evidenceInsights,
    controlInsightsMetadataByAssessmentItem_name,
    controlInsightsMetadataByAssessmentItem_id,
    controlInsightsMetadataByAssessmentItem_lastUpdated,
    controlInsightsMetadataByAssessmentItem_controlSetName,

    -- * ControlInsightsMetadataItem
    ControlInsightsMetadataItem (..),
    newControlInsightsMetadataItem,
    controlInsightsMetadataItem_evidenceInsights,
    controlInsightsMetadataItem_name,
    controlInsightsMetadataItem_id,
    controlInsightsMetadataItem_lastUpdated,

    -- * ControlMappingSource
    ControlMappingSource (..),
    newControlMappingSource,
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceName,
    controlMappingSource_sourceId,
    controlMappingSource_sourceSetUpOption,
    controlMappingSource_troubleshootingText,
    controlMappingSource_sourceType,

    -- * ControlMetadata
    ControlMetadata (..),
    newControlMetadata,
    controlMetadata_name,
    controlMetadata_lastUpdatedAt,
    controlMetadata_arn,
    controlMetadata_id,
    controlMetadata_createdAt,
    controlMetadata_controlSources,

    -- * ControlSet
    ControlSet (..),
    newControlSet,
    controlSet_name,
    controlSet_id,
    controlSet_controls,

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
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceSetUpOption,
    createControlMappingSource_troubleshootingText,
    createControlMappingSource_sourceType,

    -- * CreateDelegationRequest
    CreateDelegationRequest (..),
    newCreateDelegationRequest,
    createDelegationRequest_roleType,
    createDelegationRequest_roleArn,
    createDelegationRequest_comment,
    createDelegationRequest_controlSetId,

    -- * Delegation
    Delegation (..),
    newDelegation,
    delegation_roleType,
    delegation_roleArn,
    delegation_assessmentId,
    delegation_assessmentName,
    delegation_status,
    delegation_id,
    delegation_lastUpdated,
    delegation_comment,
    delegation_controlSetId,
    delegation_creationTime,
    delegation_createdBy,

    -- * DelegationMetadata
    DelegationMetadata (..),
    newDelegationMetadata,
    delegationMetadata_roleArn,
    delegationMetadata_assessmentId,
    delegationMetadata_assessmentName,
    delegationMetadata_status,
    delegationMetadata_id,
    delegationMetadata_controlSetName,
    delegationMetadata_creationTime,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_awsAccountId,
    evidence_evidenceAwsAccountId,
    evidence_evidenceFolderId,
    evidence_resourcesIncluded,
    evidence_awsOrganization,
    evidence_time,
    evidence_id,
    evidence_evidenceByType,
    evidence_complianceCheck,
    evidence_iamId,
    evidence_eventName,
    evidence_dataSource,
    evidence_assessmentReportSelection,
    evidence_attributes,
    evidence_eventSource,

    -- * EvidenceFinderEnablement
    EvidenceFinderEnablement (..),
    newEvidenceFinderEnablement,
    evidenceFinderEnablement_eventDataStoreArn,
    evidenceFinderEnablement_enablementStatus,
    evidenceFinderEnablement_backfillStatus,
    evidenceFinderEnablement_error,

    -- * EvidenceInsights
    EvidenceInsights (..),
    newEvidenceInsights,
    evidenceInsights_compliantEvidenceCount,
    evidenceInsights_inconclusiveEvidenceCount,
    evidenceInsights_noncompliantEvidenceCount,

    -- * Framework
    Framework (..),
    newFramework,
    framework_tags,
    framework_name,
    framework_type,
    framework_lastUpdatedAt,
    framework_arn,
    framework_description,
    framework_id,
    framework_logo,
    framework_controlSets,
    framework_complianceType,
    framework_createdBy,
    framework_createdAt,
    framework_lastUpdatedBy,
    framework_controlSources,

    -- * FrameworkMetadata
    FrameworkMetadata (..),
    newFrameworkMetadata,
    frameworkMetadata_name,
    frameworkMetadata_description,
    frameworkMetadata_logo,
    frameworkMetadata_complianceType,

    -- * Insights
    Insights (..),
    newInsights,
    insights_totalAssessmentControlsCount,
    insights_compliantEvidenceCount,
    insights_lastUpdated,
    insights_activeAssessmentsCount,
    insights_inconclusiveEvidenceCount,
    insights_noncompliantEvidenceCount,
    insights_assessmentControlsCountByNoncompliantEvidence,

    -- * InsightsByAssessment
    InsightsByAssessment (..),
    newInsightsByAssessment,
    insightsByAssessment_totalAssessmentControlsCount,
    insightsByAssessment_compliantEvidenceCount,
    insightsByAssessment_lastUpdated,
    insightsByAssessment_inconclusiveEvidenceCount,
    insightsByAssessment_noncompliantEvidenceCount,
    insightsByAssessment_assessmentControlsCountByNoncompliantEvidence,

    -- * ManualEvidence
    ManualEvidence (..),
    newManualEvidence,
    manualEvidence_s3ResourcePath,

    -- * Notification
    Notification (..),
    newNotification,
    notification_assessmentId,
    notification_assessmentName,
    notification_description,
    notification_id,
    notification_source,
    notification_controlSetName,
    notification_controlSetId,
    notification_eventTime,

    -- * Resource
    Resource (..),
    newResource,
    resource_arn,
    resource_complianceCheck,
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
    serviceMetadata_name,
    serviceMetadata_displayName,
    serviceMetadata_description,
    serviceMetadata_category,

    -- * Settings
    Settings (..),
    newSettings,
    settings_defaultProcessOwners,
    settings_snsTopic,
    settings_isAwsOrgEnabled,
    settings_kmsKey,
    settings_evidenceFinderEnablement,
    settings_defaultAssessmentReportsDestination,

    -- * SourceKeyword
    SourceKeyword (..),
    newSourceKeyword,
    sourceKeyword_keywordValue,
    sourceKeyword_keywordInputType,

    -- * URL
    URL (..),
    newURL,
    url_link,
    url_hyperlinkName,

    -- * UpdateAssessmentFrameworkControlSet
    UpdateAssessmentFrameworkControlSet (..),
    newUpdateAssessmentFrameworkControlSet,
    updateAssessmentFrameworkControlSet_id,
    updateAssessmentFrameworkControlSet_name,
    updateAssessmentFrameworkControlSet_controls,
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
import Amazonka.AuditManager.Types.AssessmentFrameworkShareRequest
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
import Amazonka.AuditManager.Types.ControlDomainInsights
import Amazonka.AuditManager.Types.ControlInsightsMetadataByAssessmentItem
import Amazonka.AuditManager.Types.ControlInsightsMetadataItem
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
import Amazonka.AuditManager.Types.EvidenceFinderBackfillStatus
import Amazonka.AuditManager.Types.EvidenceFinderEnablement
import Amazonka.AuditManager.Types.EvidenceFinderEnablementStatus
import Amazonka.AuditManager.Types.EvidenceInsights
import Amazonka.AuditManager.Types.Framework
import Amazonka.AuditManager.Types.FrameworkMetadata
import Amazonka.AuditManager.Types.FrameworkType
import Amazonka.AuditManager.Types.Insights
import Amazonka.AuditManager.Types.InsightsByAssessment
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
import Amazonka.AuditManager.Types.ShareRequestAction
import Amazonka.AuditManager.Types.ShareRequestStatus
import Amazonka.AuditManager.Types.ShareRequestType
import Amazonka.AuditManager.Types.SourceFrequency
import Amazonka.AuditManager.Types.SourceKeyword
import Amazonka.AuditManager.Types.SourceSetUpOption
import Amazonka.AuditManager.Types.SourceType
import Amazonka.AuditManager.Types.URL
import Amazonka.AuditManager.Types.UpdateAssessmentFrameworkControlSet
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2017-07-25@ of the Amazon Audit Manager SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "AuditManager",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "auditmanager",
      Core.signingName = "auditmanager",
      Core.version = "2017-07-25",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "AuditManager",
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

-- | Your account isn\'t registered with Audit Manager. Check the delegated
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

-- | You\'ve reached your account quota for this resource type. To perform
-- the requested action, delete some existing resources or
-- <https://docs.aws.amazon.com/general/latest/gr/aws_service_limits.html request a quota increase>
-- from the Service Quotas console. For a list of Audit Manager service
-- quotas, see
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/service-quotas.html Quotas and restrictions for Audit Manager>.
_ServiceQuotaExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceQuotaExceededException =
  Core._MatchServiceError
    defaultService
    "ServiceQuotaExceededException"
    Prelude.. Core.hasStatus 402

-- | The resource that\'s specified in the request can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | The request was denied due to request throttling.
_ThrottlingException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ThrottlingException =
  Core._MatchServiceError
    defaultService
    "ThrottlingException"
    Prelude.. Core.hasStatus 400

-- | The request has invalid or missing parameters.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
