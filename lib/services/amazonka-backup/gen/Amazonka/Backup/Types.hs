{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _DependencyFailureException,
    _InvalidResourceStateException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _AlreadyExistsException,
    _LimitExceededException,
    _ConflictException,
    _MissingParameterValueException,
    _InvalidRequestException,
    _InvalidParameterValueException,

    -- * BackupJobState
    BackupJobState (..),

    -- * BackupVaultEvent
    BackupVaultEvent (..),

    -- * ConditionType
    ConditionType (..),

    -- * CopyJobState
    CopyJobState (..),

    -- * RecoveryPointStatus
    RecoveryPointStatus (..),

    -- * RestoreJobStatus
    RestoreJobStatus (..),

    -- * StorageClass
    StorageClass (..),

    -- * AdvancedBackupSetting
    AdvancedBackupSetting (..),
    newAdvancedBackupSetting,
    advancedBackupSetting_resourceType,
    advancedBackupSetting_backupOptions,

    -- * BackupJob
    BackupJob (..),
    newBackupJob,
    backupJob_resourceType,
    backupJob_recoveryPointArn,
    backupJob_completionDate,
    backupJob_backupVaultName,
    backupJob_state,
    backupJob_creationDate,
    backupJob_backupSizeInBytes,
    backupJob_backupVaultArn,
    backupJob_backupOptions,
    backupJob_iamRoleArn,
    backupJob_accountId,
    backupJob_backupType,
    backupJob_percentDone,
    backupJob_expectedCompletionDate,
    backupJob_backupJobId,
    backupJob_resourceArn,
    backupJob_startBy,
    backupJob_statusMessage,
    backupJob_createdBy,
    backupJob_bytesTransferred,

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
    backupPlanTemplatesListMember_backupPlanTemplateName,
    backupPlanTemplatesListMember_backupPlanTemplateId,

    -- * BackupPlansListMember
    BackupPlansListMember (..),
    newBackupPlansListMember,
    backupPlansListMember_backupPlanName,
    backupPlansListMember_creationDate,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_deletionDate,
    backupPlansListMember_versionId,

    -- * BackupRule
    BackupRule (..),
    newBackupRule,
    backupRule_startWindowMinutes,
    backupRule_lifecycle,
    backupRule_ruleId,
    backupRule_copyActions,
    backupRule_scheduleExpression,
    backupRule_enableContinuousBackup,
    backupRule_completionWindowMinutes,
    backupRule_recoveryPointTags,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- * BackupRuleInput
    BackupRuleInput (..),
    newBackupRuleInput,
    backupRuleInput_startWindowMinutes,
    backupRuleInput_lifecycle,
    backupRuleInput_copyActions,
    backupRuleInput_scheduleExpression,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- * BackupSelection
    BackupSelection (..),
    newBackupSelection,
    backupSelection_conditions,
    backupSelection_resources,
    backupSelection_listOfTags,
    backupSelection_notResources,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- * BackupSelectionsListMember
    BackupSelectionsListMember (..),
    newBackupSelectionsListMember,
    backupSelectionsListMember_selectionName,
    backupSelectionsListMember_creationDate,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_backupPlanId,

    -- * BackupVaultListMember
    BackupVaultListMember (..),
    newBackupVaultListMember,
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_minRetentionDays,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_creationDate,
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_numberOfRecoveryPoints,
    backupVaultListMember_locked,
    backupVaultListMember_lockDate,

    -- * CalculatedLifecycle
    CalculatedLifecycle (..),
    newCalculatedLifecycle,
    calculatedLifecycle_moveToColdStorageAt,
    calculatedLifecycle_deleteAt,

    -- * Condition
    Condition (..),
    newCondition,
    condition_conditionType,
    condition_conditionKey,
    condition_conditionValue,

    -- * ConditionParameter
    ConditionParameter (..),
    newConditionParameter,
    conditionParameter_conditionValue,
    conditionParameter_conditionKey,

    -- * Conditions
    Conditions (..),
    newConditions,
    conditions_stringNotEquals,
    conditions_stringNotLike,
    conditions_stringEquals,
    conditions_stringLike,

    -- * ControlInputParameter
    ControlInputParameter (..),
    newControlInputParameter,
    controlInputParameter_parameterValue,
    controlInputParameter_parameterName,

    -- * ControlScope
    ControlScope (..),
    newControlScope,
    controlScope_tags,
    controlScope_complianceResourceIds,
    controlScope_complianceResourceTypes,

    -- * CopyAction
    CopyAction (..),
    newCopyAction,
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- * CopyJob
    CopyJob (..),
    newCopyJob,
    copyJob_resourceType,
    copyJob_completionDate,
    copyJob_state,
    copyJob_creationDate,
    copyJob_destinationBackupVaultArn,
    copyJob_backupSizeInBytes,
    copyJob_iamRoleArn,
    copyJob_accountId,
    copyJob_destinationRecoveryPointArn,
    copyJob_copyJobId,
    copyJob_sourceBackupVaultArn,
    copyJob_resourceArn,
    copyJob_statusMessage,
    copyJob_createdBy,
    copyJob_sourceRecoveryPointArn,

    -- * Framework
    Framework (..),
    newFramework,
    framework_deploymentStatus,
    framework_frameworkArn,
    framework_frameworkDescription,
    framework_frameworkName,
    framework_numberOfControls,
    framework_creationTime,

    -- * FrameworkControl
    FrameworkControl (..),
    newFrameworkControl,
    frameworkControl_controlInputParameters,
    frameworkControl_controlScope,
    frameworkControl_controlName,

    -- * Lifecycle
    Lifecycle (..),
    newLifecycle,
    lifecycle_deleteAfterDays,
    lifecycle_moveToColdStorageAfterDays,

    -- * ProtectedResource
    ProtectedResource (..),
    newProtectedResource,
    protectedResource_resourceType,
    protectedResource_resourceArn,
    protectedResource_lastBackupTime,

    -- * RecoveryPointByBackupVault
    RecoveryPointByBackupVault (..),
    newRecoveryPointByBackupVault,
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_backupVaultName,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_statusMessage,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_calculatedLifecycle,

    -- * RecoveryPointByResource
    RecoveryPointByResource (..),
    newRecoveryPointByResource,
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_backupVaultName,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,

    -- * RecoveryPointCreator
    RecoveryPointCreator (..),
    newRecoveryPointCreator,
    recoveryPointCreator_backupPlanVersion,
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupRuleId,

    -- * ReportDeliveryChannel
    ReportDeliveryChannel (..),
    newReportDeliveryChannel,
    reportDeliveryChannel_s3KeyPrefix,
    reportDeliveryChannel_formats,
    reportDeliveryChannel_s3BucketName,

    -- * ReportDestination
    ReportDestination (..),
    newReportDestination,
    reportDestination_s3BucketName,
    reportDestination_s3Keys,

    -- * ReportJob
    ReportJob (..),
    newReportJob,
    reportJob_reportPlanArn,
    reportJob_reportTemplate,
    reportJob_reportJobId,
    reportJob_reportDestination,
    reportJob_status,
    reportJob_completionTime,
    reportJob_creationTime,
    reportJob_statusMessage,

    -- * ReportPlan
    ReportPlan (..),
    newReportPlan,
    reportPlan_deploymentStatus,
    reportPlan_reportPlanArn,
    reportPlan_reportDeliveryChannel,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportSetting,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_reportPlanName,
    reportPlan_creationTime,
    reportPlan_reportPlanDescription,

    -- * ReportSetting
    ReportSetting (..),
    newReportSetting,
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_reportTemplate,

    -- * RestoreJobsListMember
    RestoreJobsListMember (..),
    newRestoreJobsListMember,
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_completionDate,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_status,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_accountId,
    restoreJobsListMember_percentDone,
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
import Amazonka.Backup.Types.Framework
import Amazonka.Backup.Types.FrameworkControl
import Amazonka.Backup.Types.Lifecycle
import Amazonka.Backup.Types.ProtectedResource
import Amazonka.Backup.Types.RecoveryPointByBackupVault
import Amazonka.Backup.Types.RecoveryPointByResource
import Amazonka.Backup.Types.RecoveryPointCreator
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

-- | A dependent Amazon Web Services service or resource returned an error to
-- the Backup service, and the action cannot be completed.
_DependencyFailureException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DependencyFailureException =
  Core._MatchServiceError
    defaultService
    "DependencyFailureException"

-- | Backup is already performing an action on this recovery point. It can\'t
-- perform the action you requested until the first action finishes. Try
-- again later.
_InvalidResourceStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceStateException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceStateException"

-- | The request failed due to a temporary failure of the server.
_ServiceUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceUnavailableException =
  Core._MatchServiceError
    defaultService
    "ServiceUnavailableException"

-- | A resource that is required for the action doesn\'t exist.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"

-- | The required resource already exists.
_AlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "AlreadyExistsException"

-- | A limit in the request has been exceeded; for example, a maximum number
-- of items allowed in a request.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Backup can\'t perform the action that you requested until it finishes
-- performing a previous action. Try again later.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Indicates that a required parameter is missing.
_MissingParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingParameterValueException =
  Core._MatchServiceError
    defaultService
    "MissingParameterValueException"

-- | Indicates that something is wrong with the input to the request. For
-- example, a parameter is of the wrong type.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

-- | Indicates that something is wrong with a parameter\'s value. For
-- example, the value is out of range.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"
