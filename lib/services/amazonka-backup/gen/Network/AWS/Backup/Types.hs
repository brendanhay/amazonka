{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Backup.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidRequestException,
    _DependencyFailureException,
    _InvalidResourceStateException,
    _ConflictException,
    _InvalidParameterValueException,
    _ServiceUnavailableException,
    _ResourceNotFoundException,
    _AlreadyExistsException,
    _LimitExceededException,
    _MissingParameterValueException,

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
    backupJob_iamRoleArn,
    backupJob_state,
    backupJob_resourceType,
    backupJob_percentDone,
    backupJob_startBy,
    backupJob_createdBy,
    backupJob_expectedCompletionDate,
    backupJob_bytesTransferred,
    backupJob_backupVaultArn,
    backupJob_accountId,
    backupJob_backupJobId,
    backupJob_resourceArn,
    backupJob_statusMessage,
    backupJob_recoveryPointArn,
    backupJob_backupSizeInBytes,
    backupJob_creationDate,
    backupJob_completionDate,
    backupJob_backupVaultName,
    backupJob_backupType,
    backupJob_backupOptions,

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
    backupPlansListMember_versionId,
    backupPlansListMember_backupPlanName,
    backupPlansListMember_advancedBackupSettings,
    backupPlansListMember_backupPlanId,
    backupPlansListMember_creatorRequestId,
    backupPlansListMember_backupPlanArn,
    backupPlansListMember_lastExecutionDate,
    backupPlansListMember_creationDate,
    backupPlansListMember_deletionDate,

    -- * BackupRule
    BackupRule (..),
    newBackupRule,
    backupRule_ruleId,
    backupRule_lifecycle,
    backupRule_recoveryPointTags,
    backupRule_scheduleExpression,
    backupRule_enableContinuousBackup,
    backupRule_completionWindowMinutes,
    backupRule_copyActions,
    backupRule_startWindowMinutes,
    backupRule_ruleName,
    backupRule_targetBackupVaultName,

    -- * BackupRuleInput
    BackupRuleInput (..),
    newBackupRuleInput,
    backupRuleInput_lifecycle,
    backupRuleInput_recoveryPointTags,
    backupRuleInput_scheduleExpression,
    backupRuleInput_enableContinuousBackup,
    backupRuleInput_completionWindowMinutes,
    backupRuleInput_copyActions,
    backupRuleInput_startWindowMinutes,
    backupRuleInput_ruleName,
    backupRuleInput_targetBackupVaultName,

    -- * BackupSelection
    BackupSelection (..),
    newBackupSelection,
    backupSelection_resources,
    backupSelection_listOfTags,
    backupSelection_selectionName,
    backupSelection_iamRoleArn,

    -- * BackupSelectionsListMember
    BackupSelectionsListMember (..),
    newBackupSelectionsListMember,
    backupSelectionsListMember_iamRoleArn,
    backupSelectionsListMember_selectionName,
    backupSelectionsListMember_selectionId,
    backupSelectionsListMember_backupPlanId,
    backupSelectionsListMember_creatorRequestId,
    backupSelectionsListMember_creationDate,

    -- * BackupVaultListMember
    BackupVaultListMember (..),
    newBackupVaultListMember,
    backupVaultListMember_lockDate,
    backupVaultListMember_maxRetentionDays,
    backupVaultListMember_locked,
    backupVaultListMember_creatorRequestId,
    backupVaultListMember_numberOfRecoveryPoints,
    backupVaultListMember_backupVaultArn,
    backupVaultListMember_encryptionKeyArn,
    backupVaultListMember_creationDate,
    backupVaultListMember_backupVaultName,
    backupVaultListMember_minRetentionDays,

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

    -- * ControlInputParameter
    ControlInputParameter (..),
    newControlInputParameter,
    controlInputParameter_parameterValue,
    controlInputParameter_parameterName,

    -- * ControlScope
    ControlScope (..),
    newControlScope,
    controlScope_complianceResourceTypes,
    controlScope_tags,
    controlScope_complianceResourceIds,

    -- * CopyAction
    CopyAction (..),
    newCopyAction,
    copyAction_lifecycle,
    copyAction_destinationBackupVaultArn,

    -- * CopyJob
    CopyJob (..),
    newCopyJob,
    copyJob_iamRoleArn,
    copyJob_state,
    copyJob_sourceRecoveryPointArn,
    copyJob_resourceType,
    copyJob_destinationBackupVaultArn,
    copyJob_createdBy,
    copyJob_destinationRecoveryPointArn,
    copyJob_accountId,
    copyJob_sourceBackupVaultArn,
    copyJob_copyJobId,
    copyJob_resourceArn,
    copyJob_statusMessage,
    copyJob_backupSizeInBytes,
    copyJob_creationDate,
    copyJob_completionDate,

    -- * Framework
    Framework (..),
    newFramework,
    framework_creationTime,
    framework_frameworkDescription,
    framework_numberOfControls,
    framework_frameworkArn,
    framework_deploymentStatus,
    framework_frameworkName,

    -- * FrameworkControl
    FrameworkControl (..),
    newFrameworkControl,
    frameworkControl_controlScope,
    frameworkControl_controlInputParameters,
    frameworkControl_controlName,

    -- * Lifecycle
    Lifecycle (..),
    newLifecycle,
    lifecycle_moveToColdStorageAfterDays,
    lifecycle_deleteAfterDays,

    -- * ProtectedResource
    ProtectedResource (..),
    newProtectedResource,
    protectedResource_resourceType,
    protectedResource_lastBackupTime,
    protectedResource_resourceArn,

    -- * RecoveryPointByBackupVault
    RecoveryPointByBackupVault (..),
    newRecoveryPointByBackupVault,
    recoveryPointByBackupVault_isEncrypted,
    recoveryPointByBackupVault_status,
    recoveryPointByBackupVault_iamRoleArn,
    recoveryPointByBackupVault_resourceType,
    recoveryPointByBackupVault_createdBy,
    recoveryPointByBackupVault_calculatedLifecycle,
    recoveryPointByBackupVault_lifecycle,
    recoveryPointByBackupVault_backupVaultArn,
    recoveryPointByBackupVault_sourceBackupVaultArn,
    recoveryPointByBackupVault_lastRestoreTime,
    recoveryPointByBackupVault_resourceArn,
    recoveryPointByBackupVault_statusMessage,
    recoveryPointByBackupVault_recoveryPointArn,
    recoveryPointByBackupVault_encryptionKeyArn,
    recoveryPointByBackupVault_backupSizeInBytes,
    recoveryPointByBackupVault_creationDate,
    recoveryPointByBackupVault_completionDate,
    recoveryPointByBackupVault_backupVaultName,

    -- * RecoveryPointByResource
    RecoveryPointByResource (..),
    newRecoveryPointByResource,
    recoveryPointByResource_status,
    recoveryPointByResource_statusMessage,
    recoveryPointByResource_recoveryPointArn,
    recoveryPointByResource_backupSizeBytes,
    recoveryPointByResource_encryptionKeyArn,
    recoveryPointByResource_creationDate,
    recoveryPointByResource_backupVaultName,

    -- * RecoveryPointCreator
    RecoveryPointCreator (..),
    newRecoveryPointCreator,
    recoveryPointCreator_backupPlanId,
    recoveryPointCreator_backupPlanArn,
    recoveryPointCreator_backupPlanVersion,
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
    reportDestination_s3Keys,
    reportDestination_s3BucketName,

    -- * ReportJob
    ReportJob (..),
    newReportJob,
    reportJob_creationTime,
    reportJob_status,
    reportJob_reportPlanArn,
    reportJob_completionTime,
    reportJob_reportJobId,
    reportJob_statusMessage,
    reportJob_reportDestination,
    reportJob_reportTemplate,

    -- * ReportPlan
    ReportPlan (..),
    newReportPlan,
    reportPlan_creationTime,
    reportPlan_reportPlanName,
    reportPlan_reportPlanArn,
    reportPlan_reportSetting,
    reportPlan_reportPlanDescription,
    reportPlan_lastAttemptedExecutionTime,
    reportPlan_deploymentStatus,
    reportPlan_lastSuccessfulExecutionTime,
    reportPlan_reportDeliveryChannel,

    -- * ReportSetting
    ReportSetting (..),
    newReportSetting,
    reportSetting_frameworkArns,
    reportSetting_numberOfFrameworks,
    reportSetting_reportTemplate,

    -- * RestoreJobsListMember
    RestoreJobsListMember (..),
    newRestoreJobsListMember,
    restoreJobsListMember_status,
    restoreJobsListMember_iamRoleArn,
    restoreJobsListMember_expectedCompletionTimeMinutes,
    restoreJobsListMember_restoreJobId,
    restoreJobsListMember_resourceType,
    restoreJobsListMember_percentDone,
    restoreJobsListMember_accountId,
    restoreJobsListMember_createdResourceArn,
    restoreJobsListMember_statusMessage,
    restoreJobsListMember_recoveryPointArn,
    restoreJobsListMember_backupSizeInBytes,
    restoreJobsListMember_creationDate,
    restoreJobsListMember_completionDate,
  )
where

import Network.AWS.Backup.Types.AdvancedBackupSetting
import Network.AWS.Backup.Types.BackupJob
import Network.AWS.Backup.Types.BackupJobState
import Network.AWS.Backup.Types.BackupPlan
import Network.AWS.Backup.Types.BackupPlanInput
import Network.AWS.Backup.Types.BackupPlanTemplatesListMember
import Network.AWS.Backup.Types.BackupPlansListMember
import Network.AWS.Backup.Types.BackupRule
import Network.AWS.Backup.Types.BackupRuleInput
import Network.AWS.Backup.Types.BackupSelection
import Network.AWS.Backup.Types.BackupSelectionsListMember
import Network.AWS.Backup.Types.BackupVaultEvent
import Network.AWS.Backup.Types.BackupVaultListMember
import Network.AWS.Backup.Types.CalculatedLifecycle
import Network.AWS.Backup.Types.Condition
import Network.AWS.Backup.Types.ConditionType
import Network.AWS.Backup.Types.ControlInputParameter
import Network.AWS.Backup.Types.ControlScope
import Network.AWS.Backup.Types.CopyAction
import Network.AWS.Backup.Types.CopyJob
import Network.AWS.Backup.Types.CopyJobState
import Network.AWS.Backup.Types.Framework
import Network.AWS.Backup.Types.FrameworkControl
import Network.AWS.Backup.Types.Lifecycle
import Network.AWS.Backup.Types.ProtectedResource
import Network.AWS.Backup.Types.RecoveryPointByBackupVault
import Network.AWS.Backup.Types.RecoveryPointByResource
import Network.AWS.Backup.Types.RecoveryPointCreator
import Network.AWS.Backup.Types.RecoveryPointStatus
import Network.AWS.Backup.Types.ReportDeliveryChannel
import Network.AWS.Backup.Types.ReportDestination
import Network.AWS.Backup.Types.ReportJob
import Network.AWS.Backup.Types.ReportPlan
import Network.AWS.Backup.Types.ReportSetting
import Network.AWS.Backup.Types.RestoreJobStatus
import Network.AWS.Backup.Types.RestoreJobsListMember
import Network.AWS.Backup.Types.StorageClass
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2018-11-15@ of the Amazon Backup SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "Backup",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "backup",
      Core._serviceSigningName = "backup",
      Core._serviceVersion = "2018-11-15",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError = Core.parseJSONError "Backup",
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

-- | Indicates that something is wrong with the input to the request. For
-- example, a parameter is of the wrong type.
_InvalidRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRequestException =
  Core._MatchServiceError
    defaultService
    "InvalidRequestException"

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

-- | Backup can\'t perform the action that you requested until it finishes
-- performing a previous action. Try again later.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"

-- | Indicates that something is wrong with a parameter\'s value. For
-- example, the value is out of range.
_InvalidParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParameterValueException =
  Core._MatchServiceError
    defaultService
    "InvalidParameterValueException"

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

-- | Indicates that a required parameter is missing.
_MissingParameterValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MissingParameterValueException =
  Core._MatchServiceError
    defaultService
    "MissingParameterValueException"
