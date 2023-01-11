{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _AlreadyExistsException,
    _ConflictException,
    _DependencyFailureException,
    _InvalidParameterValueException,
    _InvalidRequestException,
    _InvalidResourceStateException,
    _LimitExceededException,
    _MissingParameterValueException,
    _ResourceNotFoundException,
    _ServiceUnavailableException,

    -- * BackupJobState
    BackupJobState (..),

    -- * BackupVaultEvent
    BackupVaultEvent (..),

    -- * ConditionType
    ConditionType (..),

    -- * CopyJobState
    CopyJobState (..),

    -- * LegalHoldStatus
    LegalHoldStatus (..),

    -- * RecoveryPointStatus
    RecoveryPointStatus (..),

    -- * RestoreJobStatus
    RestoreJobStatus (..),

    -- * StorageClass
    StorageClass (..),

    -- * AdvancedBackupSetting
    AdvancedBackupSetting (..),
    newAdvancedBackupSetting,
    advancedBackupSetting_backupOptions,
    advancedBackupSetting_resourceType,

    -- * BackupJob
    BackupJob (..),
    newBackupJob,
    backupJob_accountId,
    backupJob_backupJobId,
    backupJob_backupOptions,
    backupJob_backupSizeInBytes,
    backupJob_backupType,
    backupJob_backupVaultArn,
    backupJob_backupVaultName,
    backupJob_bytesTransferred,
    backupJob_completionDate,
    backupJob_createdBy,
    backupJob_creationDate,
    backupJob_expectedCompletionDate,
    backupJob_iamRoleArn,
    backupJob_isParent,
    backupJob_parentJobId,
    backupJob_percentDone,
    backupJob_recoveryPointArn,
    backupJob_resourceArn,
    backupJob_resourceType,
    backupJob_startBy,
    backupJob_state,
    backupJob_statusMessage,

    -- * BackupPlan
    BackupPlan (..),
    newBackupPlan,
    backupPlan_advancedBackupSettings,
    backupPlan_backupPlanName,
    backupPlan_rules,

    -- * BackupPlanInput
    BackupPlanInput (..),
    newBackupPlanInput,
    backupPlanInput_advancedBackupSettings,
    backupPlanInput_backupPlanName,
    backupPlanInput_rules,

    -- * BackupPlanTemplatesListMember
    BackupPlanTemplatesListMember (..),
    newBackupPlanTemplatesListMember,
    backupPlanTemplatesListMember_backupPlanTemplateId,
    backupPlanTemplatesListMember_backupPlanTemplateName,

    -- * BackupPlansListMember
    BackupPlansListMember (..),
    newBackupPlansListMember,
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_backupPlanName,
    backupPlansListMember_creationDate,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_deletionDate,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_versionId,

    -- * BackupRule
    BackupRule (..),
    newBackupRule,
    backupRule_completionWindowMinutes,
    backupRule_copyActions,
    backupRule_enableContinuousBackup,
    backupRule_lifecycle,
    backupRule_recoveryPointTags,
    backupRule_ruleId,
    backupRule_scheduleExpression,
    backupRule_startWindowMinutes,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- * BackupRuleInput
    BackupRuleInput (..),
    newBackupRuleInput,
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_copyActions,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_lifecycle,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_scheduleExpression,
    backupRuleInput_startWindowMinutes,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- * BackupSelection
    BackupSelection (..),
    newBackupSelection,
    backupSelection_conditions,
    backupSelection_listOfTags,
    backupSelection_notResources,
    backupSelection_resources,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- * BackupSelectionsListMember
    BackupSelectionsListMember (..),
    newBackupSelectionsListMember,
    backupSelectionsListMember_backupPlanId,
    backupSelectionsListMember_creationDate,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_selectionName,

    -- * BackupVaultListMember
    BackupVaultListMember (..),
    newBackupVaultListMember,
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_creationDate,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_lockDate,
    backupVaultListMember_locked,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_minRetentionDays,
    backupVaultListMember_numberOfRecoveryPoints,

    -- * CalculatedLifecycle
    CalculatedLifecycle (..),
    newCalculatedLifecycle,
    calculatedLifecycle_deleteAt,
    calculatedLifecycle_moveToColdStorageAt,

    -- * Condition
    Condition (..),
    newCondition,
    condition_conditionType,
    condition_conditionKey,
    condition_conditionValue,

    -- * ConditionParameter
    ConditionParameter (..),
    newConditionParameter,
    conditionParameter_conditionKey,
    conditionParameter_conditionValue,

    -- * Conditions
    Conditions (..),
    newConditions,
    conditions_stringEquals,
    conditions_stringLike,
    conditions_stringNotEquals,
    conditions_stringNotLike,

    -- * ControlInputParameter
    ControlInputParameter (..),
    newControlInputParameter,
    controlInputParameter_parameterName,
    controlInputParameter_parameterValue,

    -- * ControlScope
    ControlScope (..),
    newControlScope,
    controlScope_complianceResourceIds,
    controlScope_complianceResourceTypes,
    controlScope_tags,

    -- * CopyAction
    CopyAction (..),
    newCopyAction,
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- * CopyJob
    CopyJob (..),
    newCopyJob,
    copyJob_accountId,
    copyJob_backupSizeInBytes,
    copyJob_childJobsInState,
    copyJob_completionDate,
    copyJob_compositeMemberIdentifier,
    copyJob_copyJobId,
    copyJob_createdBy,
    copyJob_creationDate,
    copyJob_destinationBackupVaultArn,
    copyJob_destinationRecoveryPointArn,
    copyJob_iamRoleArn,
    copyJob_isParent,
    copyJob_numberOfChildJobs,
    copyJob_parentJobId,
    copyJob_resourceArn,
    copyJob_resourceType,
    copyJob_sourceBackupVaultArn,
    copyJob_sourceRecoveryPointArn,
    copyJob_state,
    copyJob_statusMessage,

    -- * DateRange
    DateRange (..),
    newDateRange,
    dateRange_fromDate,
    dateRange_toDate,

    -- * Framework
    Framework (..),
    newFramework,
    framework_creationTime,
    framework_deploymentStatus,
    framework_frameworkArn,
    framework_frameworkDescription,
    framework_frameworkName,
    framework_numberOfControls,

    -- * FrameworkControl
    FrameworkControl (..),
    newFrameworkControl,
    frameworkControl_controlInputParameters,
    frameworkControl_controlScope,
    frameworkControl_controlName,

    -- * LegalHold
    LegalHold (..),
    newLegalHold,
    legalHold_cancellationDate,
    legalHold_creationDate,
    legalHold_description,
    legalHold_legalHoldArn,
    legalHold_legalHoldId,
    legalHold_status,
    legalHold_title,

    -- * Lifecycle
    Lifecycle (..),
    newLifecycle,
    lifecycle_deleteAfterDays,
    lifecycle_moveToColdStorageAfterDays,

    -- * ProtectedResource
    ProtectedResource (..),
    newProtectedResource,
    protectedResource_lastBackupTime,
    protectedResource_resourceArn,
    protectedResource_resourceType,

    -- * RecoveryPointByBackupVault
    RecoveryPointByBackupVault (..),
    newRecoveryPointByBackupVault,
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_backupVaultName,
    recoveryPointByBackupVault_calculatedLifecycle,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_compositeMemberIdentifier,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_isParent,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_parentRecoveryPointArn,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_statusMessage,

    -- * RecoveryPointByResource
    RecoveryPointByResource (..),
    newRecoveryPointByResource,
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_backupVaultName,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_isParent,
    recoveryPointByResource_parentRecoveryPointArn,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,

    -- * RecoveryPointCreator
    RecoveryPointCreator (..),
    newRecoveryPointCreator,
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupPlanVersion,
    recoveryPointCreator_backupRuleId,

    -- * RecoveryPointMember
    RecoveryPointMember (..),
    newRecoveryPointMember,
    recoveryPointMember_recoveryPointArn,

    -- * RecoveryPointSelection
    RecoveryPointSelection (..),
    newRecoveryPointSelection,
    recoveryPointSelection_dateRange,
    recoveryPointSelection_resourceIdentifiers,
    recoveryPointSelection_vaultNames,

    -- * ReportDeliveryChannel
    ReportDeliveryChannel (..),
    newReportDeliveryChannel,
    reportDeliveryChannel_formats,
    reportDeliveryChannel_s3KeyPrefix,
    reportDeliveryChannel_s3BucketName,

    -- * ReportDestination
    ReportDestination (..),
    newReportDestination,
    reportDestination_s3BucketName,
    reportDestination_s3Keys,

    -- * ReportJob
    ReportJob (..),
    newReportJob,
    reportJob_completionTime,
    reportJob_creationTime,
    reportJob_reportDestination,
    reportJob_reportJobId,
    reportJob_reportPlanArn,
    reportJob_reportTemplate,
    reportJob_status,
    reportJob_statusMessage,

    -- * ReportPlan
    ReportPlan (..),
    newReportPlan,
    reportPlan_creationTime,
    reportPlan_deploymentStatus,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportDeliveryChannel,
    reportPlan_reportPlanArn,
    reportPlan_reportPlanDescription,
    reportPlan_reportPlanName,
    reportPlan_reportSetting,

    -- * ReportSetting
    ReportSetting (..),
    newReportSetting,
    reportSetting_accounts,
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_organizationUnits,
    reportSetting_regions,
    reportSetting_reportTemplate,

    -- * RestoreJobsListMember
    RestoreJobsListMember (..),
    newRestoreJobsListMember,
    restoreJobsListMember_accountId,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_completionDate,
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_percentDone,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_status,
    restoreJobsListMember_statusMessage,
  )
where

import Amazonka.Backup.Types.AdvancedBackupSetting
import Amazonka.Backup.Types.BackupJob
import Amazonka.Backup.Types.BackupJobState
import Amazonka.Backup.Types.BackupPlan
import Amazonka.Backup.Types.BackupPlanInput
import Amazonka.Backup.Types.BackupPlanTemplatesListMember
import Amazonka.Backup.Types.BackupPlansListMember
import Amazonka.Backup.Types.BackupRule
import Amazonka.Backup.Types.BackupRuleInput
import Amazonka.Backup.Types.BackupSelection
import Amazonka.Backup.Types.BackupSelectionsListMember
import Amazonka.Backup.Types.BackupVaultEvent
import Amazonka.Backup.Types.BackupVaultListMember
import Amazonka.Backup.Types.CalculatedLifecycle
import Amazonka.Backup.Types.Condition
import Amazonka.Backup.Types.ConditionParameter
import Amazonka.Backup.Types.ConditionType
import Amazonka.Backup.Types.Conditions
import Amazonka.Backup.Types.ControlInputParameter
import Amazonka.Backup.Types.ControlScope
import Amazonka.Backup.Types.CopyAction
import Amazonka.Backup.Types.CopyJob
import Amazonka.Backup.Types.CopyJobState
import Amazonka.Backup.Types.DateRange
import Amazonka.Backup.Types.Framework
import Amazonka.Backup.Types.FrameworkControl
import Amazonka.Backup.Types.LegalHold
import Amazonka.Backup.Types.LegalHoldStatus
import Amazonka.Backup.Types.Lifecycle
import Amazonka.Backup.Types.ProtectedResource
import Amazonka.Backup.Types.RecoveryPointByBackupVault
import Amazonka.Backup.Types.RecoveryPointByResource
import Amazonka.Backup.Types.RecoveryPointCreator
import Amazonka.Backup.Types.RecoveryPointMember
import Amazonka.Backup.Types.RecoveryPointSelection
import Amazonka.Backup.Types.RecoveryPointStatus
import Amazonka.Backup.Types.ReportDeliveryChannel
import Amazonka.Backup.Types.ReportDestination
import Amazonka.Backup.Types.ReportJob
import Amazonka.Backup.Types.ReportPlan
import Amazonka.Backup.Types.ReportSetting
import Amazonka.Backup.Types.RestoreJobStatus
import Amazonka.Backup.Types.RestoreJobsListMember
import Amazonka.Backup.Types.StorageClass
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2018-11-15@ of the Amazon Backup SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "Backup",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "backup",
      Core.signingName = "backup",
      Core.version = "2018-11-15",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "Backup",
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

-- | The required resource already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | Backup can\'t perform the action that you requested until it finishes
-- performing a previous action. Try again later.
_ConflictException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | A dependent Amazon Web Services service or resource returned an error to
-- the Backup service, and the action cannot be completed.
_DependencyFailureException :: Core.AsError a => Lens.Fold a Core.ServiceError
_DependencyFailureException =
  Core._MatchServiceError
    defaultService
    "DependencyFailureException"

-- | Indicates that something is wrong with a parameter\'s value. For
-- example, the value is out of range.
_InvalidParameterValueException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

-- | Indicates that something is wrong with the input to the request. For
-- example, a parameter is of the wrong type.
_InvalidRequestException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Backup is already performing an action on this recovery point. It can\'t
-- perform the action you requested until the first action finishes. Try
-- again later.
_InvalidResourceStateException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | A limit in the request has been exceeded; for example, a maximum number
-- of items allowed in a request.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Indicates that a required parameter is missing.
_MissingParameterValueException :: Core.AsError a => Lens.Fold a Core.ServiceError
_MissingParameterValueException =
  Core._MatchServiceError
    defaultService
    "MissingParameterValueException"

-- | A resource that is required for the action doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The request failed due to a temporary failure of the server.
_ServiceUnavailableException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"
