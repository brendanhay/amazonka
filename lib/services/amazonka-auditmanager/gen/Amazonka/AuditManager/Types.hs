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
    _ResourceNotFoundException,
    _ServiceQuotaExceededException,
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
    aWSAccount_emailAddress,
    aWSAccount_id,
    aWSAccount_name,

    -- * AWSService
    AWSService (..),
    newAWSService,
    aWSService_serviceName,

    -- * Assessment
    Assessment (..),
    newAssessment,
    assessment_arn,
    assessment_awsAccount,
    assessment_framework,
    assessment_metadata,
    assessment_tags,

    -- * AssessmentControl
    AssessmentControl (..),
    newAssessmentControl,
    assessmentControl_assessmentReportEvidenceCount,
    assessmentControl_comments,
    assessmentControl_description,
    assessmentControl_evidenceCount,
    assessmentControl_evidenceSources,
    assessmentControl_id,
    assessmentControl_name,
    assessmentControl_response,
    assessmentControl_status,

    -- * AssessmentControlSet
    AssessmentControlSet (..),
    newAssessmentControlSet,
    assessmentControlSet_controls,
    assessmentControlSet_delegations,
    assessmentControlSet_description,
    assessmentControlSet_id,
    assessmentControlSet_manualEvidenceCount,
    assessmentControlSet_roles,
    assessmentControlSet_status,
    assessmentControlSet_systemEvidenceCount,

    -- * AssessmentEvidenceFolder
    AssessmentEvidenceFolder (..),
    newAssessmentEvidenceFolder,
    assessmentEvidenceFolder_assessmentId,
    assessmentEvidenceFolder_assessmentReportSelectionCount,
    assessmentEvidenceFolder_author,
    assessmentEvidenceFolder_controlId,
    assessmentEvidenceFolder_controlName,
    assessmentEvidenceFolder_controlSetId,
    assessmentEvidenceFolder_dataSource,
    assessmentEvidenceFolder_date,
    assessmentEvidenceFolder_evidenceAwsServiceSourceCount,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckCount,
    assessmentEvidenceFolder_evidenceByTypeComplianceCheckIssuesCount,
    assessmentEvidenceFolder_evidenceByTypeConfigurationDataCount,
    assessmentEvidenceFolder_evidenceByTypeManualCount,
    assessmentEvidenceFolder_evidenceByTypeUserActivityCount,
    assessmentEvidenceFolder_evidenceResourcesIncludedCount,
    assessmentEvidenceFolder_id,
    assessmentEvidenceFolder_name,
    assessmentEvidenceFolder_totalEvidence,

    -- * AssessmentFramework
    AssessmentFramework (..),
    newAssessmentFramework,
    assessmentFramework_arn,
    assessmentFramework_controlSets,
    assessmentFramework_id,
    assessmentFramework_metadata,

    -- * AssessmentFrameworkMetadata
    AssessmentFrameworkMetadata (..),
    newAssessmentFrameworkMetadata,
    assessmentFrameworkMetadata_arn,
    assessmentFrameworkMetadata_complianceType,
    assessmentFrameworkMetadata_controlSetsCount,
    assessmentFrameworkMetadata_controlsCount,
    assessmentFrameworkMetadata_createdAt,
    assessmentFrameworkMetadata_description,
    assessmentFrameworkMetadata_id,
    assessmentFrameworkMetadata_lastUpdatedAt,
    assessmentFrameworkMetadata_logo,
    assessmentFrameworkMetadata_name,
    assessmentFrameworkMetadata_type,

    -- * AssessmentFrameworkShareRequest
    AssessmentFrameworkShareRequest (..),
    newAssessmentFrameworkShareRequest,
    assessmentFrameworkShareRequest_comment,
    assessmentFrameworkShareRequest_complianceType,
    assessmentFrameworkShareRequest_creationTime,
    assessmentFrameworkShareRequest_customControlsCount,
    assessmentFrameworkShareRequest_destinationAccount,
    assessmentFrameworkShareRequest_destinationRegion,
    assessmentFrameworkShareRequest_expirationTime,
    assessmentFrameworkShareRequest_frameworkDescription,
    assessmentFrameworkShareRequest_frameworkId,
    assessmentFrameworkShareRequest_frameworkName,
    assessmentFrameworkShareRequest_id,
    assessmentFrameworkShareRequest_lastUpdated,
    assessmentFrameworkShareRequest_sourceAccount,
    assessmentFrameworkShareRequest_standardControlsCount,
    assessmentFrameworkShareRequest_status,

    -- * AssessmentMetadata
    AssessmentMetadata (..),
    newAssessmentMetadata,
    assessmentMetadata_assessmentReportsDestination,
    assessmentMetadata_complianceType,
    assessmentMetadata_creationTime,
    assessmentMetadata_delegations,
    assessmentMetadata_description,
    assessmentMetadata_id,
    assessmentMetadata_lastUpdated,
    assessmentMetadata_name,
    assessmentMetadata_roles,
    assessmentMetadata_scope,
    assessmentMetadata_status,

    -- * AssessmentMetadataItem
    AssessmentMetadataItem (..),
    newAssessmentMetadataItem,
    assessmentMetadataItem_complianceType,
    assessmentMetadataItem_creationTime,
    assessmentMetadataItem_delegations,
    assessmentMetadataItem_id,
    assessmentMetadataItem_lastUpdated,
    assessmentMetadataItem_name,
    assessmentMetadataItem_roles,
    assessmentMetadataItem_status,

    -- * AssessmentReport
    AssessmentReport (..),
    newAssessmentReport,
    assessmentReport_assessmentId,
    assessmentReport_assessmentName,
    assessmentReport_author,
    assessmentReport_awsAccountId,
    assessmentReport_creationTime,
    assessmentReport_description,
    assessmentReport_id,
    assessmentReport_name,
    assessmentReport_status,

    -- * AssessmentReportEvidenceError
    AssessmentReportEvidenceError (..),
    newAssessmentReportEvidenceError,
    assessmentReportEvidenceError_errorCode,
    assessmentReportEvidenceError_errorMessage,
    assessmentReportEvidenceError_evidenceId,

    -- * AssessmentReportMetadata
    AssessmentReportMetadata (..),
    newAssessmentReportMetadata,
    assessmentReportMetadata_assessmentId,
    assessmentReportMetadata_assessmentName,
    assessmentReportMetadata_author,
    assessmentReportMetadata_creationTime,
    assessmentReportMetadata_description,
    assessmentReportMetadata_id,
    assessmentReportMetadata_name,
    assessmentReportMetadata_status,

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
    changeLog_action,
    changeLog_createdAt,
    changeLog_createdBy,
    changeLog_objectName,
    changeLog_objectType,

    -- * Control
    Control (..),
    newControl,
    control_actionPlanInstructions,
    control_actionPlanTitle,
    control_arn,
    control_controlMappingSources,
    control_controlSources,
    control_createdAt,
    control_createdBy,
    control_description,
    control_id,
    control_lastUpdatedAt,
    control_lastUpdatedBy,
    control_name,
    control_tags,
    control_testingInformation,
    control_type,

    -- * ControlComment
    ControlComment (..),
    newControlComment,
    controlComment_authorName,
    controlComment_commentBody,
    controlComment_postedDate,

    -- * ControlDomainInsights
    ControlDomainInsights (..),
    newControlDomainInsights,
    controlDomainInsights_controlsCountByNoncompliantEvidence,
    controlDomainInsights_evidenceInsights,
    controlDomainInsights_id,
    controlDomainInsights_lastUpdated,
    controlDomainInsights_name,
    controlDomainInsights_totalControlsCount,

    -- * ControlInsightsMetadataByAssessmentItem
    ControlInsightsMetadataByAssessmentItem (..),
    newControlInsightsMetadataByAssessmentItem,
    controlInsightsMetadataByAssessmentItem_controlSetName,
    controlInsightsMetadataByAssessmentItem_evidenceInsights,
    controlInsightsMetadataByAssessmentItem_id,
    controlInsightsMetadataByAssessmentItem_lastUpdated,
    controlInsightsMetadataByAssessmentItem_name,

    -- * ControlInsightsMetadataItem
    ControlInsightsMetadataItem (..),
    newControlInsightsMetadataItem,
    controlInsightsMetadataItem_evidenceInsights,
    controlInsightsMetadataItem_id,
    controlInsightsMetadataItem_lastUpdated,
    controlInsightsMetadataItem_name,

    -- * ControlMappingSource
    ControlMappingSource (..),
    newControlMappingSource,
    controlMappingSource_sourceDescription,
    controlMappingSource_sourceFrequency,
    controlMappingSource_sourceId,
    controlMappingSource_sourceKeyword,
    controlMappingSource_sourceName,
    controlMappingSource_sourceSetUpOption,
    controlMappingSource_sourceType,
    controlMappingSource_troubleshootingText,

    -- * ControlMetadata
    ControlMetadata (..),
    newControlMetadata,
    controlMetadata_arn,
    controlMetadata_controlSources,
    controlMetadata_createdAt,
    controlMetadata_id,
    controlMetadata_lastUpdatedAt,
    controlMetadata_name,

    -- * ControlSet
    ControlSet (..),
    newControlSet,
    controlSet_controls,
    controlSet_id,
    controlSet_name,

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
    createControlMappingSource_sourceDescription,
    createControlMappingSource_sourceFrequency,
    createControlMappingSource_sourceKeyword,
    createControlMappingSource_sourceName,
    createControlMappingSource_sourceSetUpOption,
    createControlMappingSource_sourceType,
    createControlMappingSource_troubleshootingText,

    -- * CreateDelegationRequest
    CreateDelegationRequest (..),
    newCreateDelegationRequest,
    createDelegationRequest_comment,
    createDelegationRequest_controlSetId,
    createDelegationRequest_roleArn,
    createDelegationRequest_roleType,

    -- * Delegation
    Delegation (..),
    newDelegation,
    delegation_assessmentId,
    delegation_assessmentName,
    delegation_comment,
    delegation_controlSetId,
    delegation_createdBy,
    delegation_creationTime,
    delegation_id,
    delegation_lastUpdated,
    delegation_roleArn,
    delegation_roleType,
    delegation_status,

    -- * DelegationMetadata
    DelegationMetadata (..),
    newDelegationMetadata,
    delegationMetadata_assessmentId,
    delegationMetadata_assessmentName,
    delegationMetadata_controlSetName,
    delegationMetadata_creationTime,
    delegationMetadata_id,
    delegationMetadata_roleArn,
    delegationMetadata_status,

    -- * Evidence
    Evidence (..),
    newEvidence,
    evidence_assessmentReportSelection,
    evidence_attributes,
    evidence_awsAccountId,
    evidence_awsOrganization,
    evidence_complianceCheck,
    evidence_dataSource,
    evidence_eventName,
    evidence_eventSource,
    evidence_evidenceAwsAccountId,
    evidence_evidenceByType,
    evidence_evidenceFolderId,
    evidence_iamId,
    evidence_id,
    evidence_resourcesIncluded,
    evidence_time,

    -- * EvidenceFinderEnablement
    EvidenceFinderEnablement (..),
    newEvidenceFinderEnablement,
    evidenceFinderEnablement_backfillStatus,
    evidenceFinderEnablement_enablementStatus,
    evidenceFinderEnablement_error,
    evidenceFinderEnablement_eventDataStoreArn,

    -- * EvidenceInsights
    EvidenceInsights (..),
    newEvidenceInsights,
    evidenceInsights_compliantEvidenceCount,
    evidenceInsights_inconclusiveEvidenceCount,
    evidenceInsights_noncompliantEvidenceCount,

    -- * Framework
    Framework (..),
    newFramework,
    framework_arn,
    framework_complianceType,
    framework_controlSets,
    framework_controlSources,
    framework_createdAt,
    framework_createdBy,
    framework_description,
    framework_id,
    framework_lastUpdatedAt,
    framework_lastUpdatedBy,
    framework_logo,
    framework_name,
    framework_tags,
    framework_type,

    -- * FrameworkMetadata
    FrameworkMetadata (..),
    newFrameworkMetadata,
    frameworkMetadata_complianceType,
    frameworkMetadata_description,
    frameworkMetadata_logo,
    frameworkMetadata_name,

    -- * Insights
    Insights (..),
    newInsights,
    insights_activeAssessmentsCount,
    insights_assessmentControlsCountByNoncompliantEvidence,
    insights_compliantEvidenceCount,
    insights_inconclusiveEvidenceCount,
    insights_lastUpdated,
    insights_noncompliantEvidenceCount,
    insights_totalAssessmentControlsCount,

    -- * InsightsByAssessment
    InsightsByAssessment (..),
    newInsightsByAssessment,
    insightsByAssessment_assessmentControlsCountByNoncompliantEvidence,
    insightsByAssessment_compliantEvidenceCount,
    insightsByAssessment_inconclusiveEvidenceCount,
    insightsByAssessment_lastUpdated,
    insightsByAssessment_noncompliantEvidenceCount,
    insightsByAssessment_totalAssessmentControlsCount,

    -- * ManualEvidence
    ManualEvidence (..),
    newManualEvidence,
    manualEvidence_s3ResourcePath,

    -- * Notification
    Notification (..),
    newNotification,
    notification_assessmentId,
    notification_assessmentName,
    notification_controlSetId,
    notification_controlSetName,
    notification_description,
    notification_eventTime,
    notification_id,
    notification_source,

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
    serviceMetadata_category,
    serviceMetadata_description,
    serviceMetadata_displayName,
    serviceMetadata_name,

    -- * Settings
    Settings (..),
    newSettings,
    settings_defaultAssessmentReportsDestination,
    settings_defaultProcessOwners,
    settings_evidenceFinderEnablement,
    settings_isAwsOrgEnabled,
    settings_kmsKey,
    settings_snsTopic,

    -- * SourceKeyword
    SourceKeyword (..),
    newSourceKeyword,
    sourceKeyword_keywordInputType,
    sourceKeyword_keywordValue,

    -- * URL
    URL (..),
    newURL,
    url_hyperlinkName,
    url_link,

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

-- | The resource that\'s specified in the request can\'t be found.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

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
