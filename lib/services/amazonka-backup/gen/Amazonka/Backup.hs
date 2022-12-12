{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.Backup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-11-15@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Backup
--
-- Backup is a unified backup service designed to protect Amazon Web
-- Services services and their associated data. Backup simplifies the
-- creation, migration, restoration, and deletion of backups, while also
-- providing reporting and auditing.
module Amazonka.Backup
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** ConflictException
    _ConflictException,

    -- ** DependencyFailureException
    _DependencyFailureException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingParameterValueException
    _MissingParameterValueException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelLegalHold
    CancelLegalHold (CancelLegalHold'),
    newCancelLegalHold,
    CancelLegalHoldResponse (CancelLegalHoldResponse'),
    newCancelLegalHoldResponse,

    -- ** CreateBackupPlan
    CreateBackupPlan (CreateBackupPlan'),
    newCreateBackupPlan,
    CreateBackupPlanResponse (CreateBackupPlanResponse'),
    newCreateBackupPlanResponse,

    -- ** CreateBackupSelection
    CreateBackupSelection (CreateBackupSelection'),
    newCreateBackupSelection,
    CreateBackupSelectionResponse (CreateBackupSelectionResponse'),
    newCreateBackupSelectionResponse,

    -- ** CreateBackupVault
    CreateBackupVault (CreateBackupVault'),
    newCreateBackupVault,
    CreateBackupVaultResponse (CreateBackupVaultResponse'),
    newCreateBackupVaultResponse,

    -- ** CreateFramework
    CreateFramework (CreateFramework'),
    newCreateFramework,
    CreateFrameworkResponse (CreateFrameworkResponse'),
    newCreateFrameworkResponse,

    -- ** CreateLegalHold
    CreateLegalHold (CreateLegalHold'),
    newCreateLegalHold,
    CreateLegalHoldResponse (CreateLegalHoldResponse'),
    newCreateLegalHoldResponse,

    -- ** CreateReportPlan
    CreateReportPlan (CreateReportPlan'),
    newCreateReportPlan,
    CreateReportPlanResponse (CreateReportPlanResponse'),
    newCreateReportPlanResponse,

    -- ** DeleteBackupPlan
    DeleteBackupPlan (DeleteBackupPlan'),
    newDeleteBackupPlan,
    DeleteBackupPlanResponse (DeleteBackupPlanResponse'),
    newDeleteBackupPlanResponse,

    -- ** DeleteBackupSelection
    DeleteBackupSelection (DeleteBackupSelection'),
    newDeleteBackupSelection,
    DeleteBackupSelectionResponse (DeleteBackupSelectionResponse'),
    newDeleteBackupSelectionResponse,

    -- ** DeleteBackupVault
    DeleteBackupVault (DeleteBackupVault'),
    newDeleteBackupVault,
    DeleteBackupVaultResponse (DeleteBackupVaultResponse'),
    newDeleteBackupVaultResponse,

    -- ** DeleteBackupVaultAccessPolicy
    DeleteBackupVaultAccessPolicy (DeleteBackupVaultAccessPolicy'),
    newDeleteBackupVaultAccessPolicy,
    DeleteBackupVaultAccessPolicyResponse (DeleteBackupVaultAccessPolicyResponse'),
    newDeleteBackupVaultAccessPolicyResponse,

    -- ** DeleteBackupVaultLockConfiguration
    DeleteBackupVaultLockConfiguration (DeleteBackupVaultLockConfiguration'),
    newDeleteBackupVaultLockConfiguration,
    DeleteBackupVaultLockConfigurationResponse (DeleteBackupVaultLockConfigurationResponse'),
    newDeleteBackupVaultLockConfigurationResponse,

    -- ** DeleteBackupVaultNotifications
    DeleteBackupVaultNotifications (DeleteBackupVaultNotifications'),
    newDeleteBackupVaultNotifications,
    DeleteBackupVaultNotificationsResponse (DeleteBackupVaultNotificationsResponse'),
    newDeleteBackupVaultNotificationsResponse,

    -- ** DeleteFramework
    DeleteFramework (DeleteFramework'),
    newDeleteFramework,
    DeleteFrameworkResponse (DeleteFrameworkResponse'),
    newDeleteFrameworkResponse,

    -- ** DeleteRecoveryPoint
    DeleteRecoveryPoint (DeleteRecoveryPoint'),
    newDeleteRecoveryPoint,
    DeleteRecoveryPointResponse (DeleteRecoveryPointResponse'),
    newDeleteRecoveryPointResponse,

    -- ** DeleteReportPlan
    DeleteReportPlan (DeleteReportPlan'),
    newDeleteReportPlan,
    DeleteReportPlanResponse (DeleteReportPlanResponse'),
    newDeleteReportPlanResponse,

    -- ** DescribeBackupJob
    DescribeBackupJob (DescribeBackupJob'),
    newDescribeBackupJob,
    DescribeBackupJobResponse (DescribeBackupJobResponse'),
    newDescribeBackupJobResponse,

    -- ** DescribeBackupVault
    DescribeBackupVault (DescribeBackupVault'),
    newDescribeBackupVault,
    DescribeBackupVaultResponse (DescribeBackupVaultResponse'),
    newDescribeBackupVaultResponse,

    -- ** DescribeCopyJob
    DescribeCopyJob (DescribeCopyJob'),
    newDescribeCopyJob,
    DescribeCopyJobResponse (DescribeCopyJobResponse'),
    newDescribeCopyJobResponse,

    -- ** DescribeFramework
    DescribeFramework (DescribeFramework'),
    newDescribeFramework,
    DescribeFrameworkResponse (DescribeFrameworkResponse'),
    newDescribeFrameworkResponse,

    -- ** DescribeGlobalSettings
    DescribeGlobalSettings (DescribeGlobalSettings'),
    newDescribeGlobalSettings,
    DescribeGlobalSettingsResponse (DescribeGlobalSettingsResponse'),
    newDescribeGlobalSettingsResponse,

    -- ** DescribeProtectedResource
    DescribeProtectedResource (DescribeProtectedResource'),
    newDescribeProtectedResource,
    DescribeProtectedResourceResponse (DescribeProtectedResourceResponse'),
    newDescribeProtectedResourceResponse,

    -- ** DescribeRecoveryPoint
    DescribeRecoveryPoint (DescribeRecoveryPoint'),
    newDescribeRecoveryPoint,
    DescribeRecoveryPointResponse (DescribeRecoveryPointResponse'),
    newDescribeRecoveryPointResponse,

    -- ** DescribeRegionSettings
    DescribeRegionSettings (DescribeRegionSettings'),
    newDescribeRegionSettings,
    DescribeRegionSettingsResponse (DescribeRegionSettingsResponse'),
    newDescribeRegionSettingsResponse,

    -- ** DescribeReportJob
    DescribeReportJob (DescribeReportJob'),
    newDescribeReportJob,
    DescribeReportJobResponse (DescribeReportJobResponse'),
    newDescribeReportJobResponse,

    -- ** DescribeReportPlan
    DescribeReportPlan (DescribeReportPlan'),
    newDescribeReportPlan,
    DescribeReportPlanResponse (DescribeReportPlanResponse'),
    newDescribeReportPlanResponse,

    -- ** DescribeRestoreJob
    DescribeRestoreJob (DescribeRestoreJob'),
    newDescribeRestoreJob,
    DescribeRestoreJobResponse (DescribeRestoreJobResponse'),
    newDescribeRestoreJobResponse,

    -- ** DisassociateRecoveryPoint
    DisassociateRecoveryPoint (DisassociateRecoveryPoint'),
    newDisassociateRecoveryPoint,
    DisassociateRecoveryPointResponse (DisassociateRecoveryPointResponse'),
    newDisassociateRecoveryPointResponse,

    -- ** DisassociateRecoveryPointFromParent
    DisassociateRecoveryPointFromParent (DisassociateRecoveryPointFromParent'),
    newDisassociateRecoveryPointFromParent,
    DisassociateRecoveryPointFromParentResponse (DisassociateRecoveryPointFromParentResponse'),
    newDisassociateRecoveryPointFromParentResponse,

    -- ** ExportBackupPlanTemplate
    ExportBackupPlanTemplate (ExportBackupPlanTemplate'),
    newExportBackupPlanTemplate,
    ExportBackupPlanTemplateResponse (ExportBackupPlanTemplateResponse'),
    newExportBackupPlanTemplateResponse,

    -- ** GetBackupPlan
    GetBackupPlan (GetBackupPlan'),
    newGetBackupPlan,
    GetBackupPlanResponse (GetBackupPlanResponse'),
    newGetBackupPlanResponse,

    -- ** GetBackupPlanFromJSON
    GetBackupPlanFromJSON (GetBackupPlanFromJSON'),
    newGetBackupPlanFromJSON,
    GetBackupPlanFromJSONResponse (GetBackupPlanFromJSONResponse'),
    newGetBackupPlanFromJSONResponse,

    -- ** GetBackupPlanFromTemplate
    GetBackupPlanFromTemplate (GetBackupPlanFromTemplate'),
    newGetBackupPlanFromTemplate,
    GetBackupPlanFromTemplateResponse (GetBackupPlanFromTemplateResponse'),
    newGetBackupPlanFromTemplateResponse,

    -- ** GetBackupSelection
    GetBackupSelection (GetBackupSelection'),
    newGetBackupSelection,
    GetBackupSelectionResponse (GetBackupSelectionResponse'),
    newGetBackupSelectionResponse,

    -- ** GetBackupVaultAccessPolicy
    GetBackupVaultAccessPolicy (GetBackupVaultAccessPolicy'),
    newGetBackupVaultAccessPolicy,
    GetBackupVaultAccessPolicyResponse (GetBackupVaultAccessPolicyResponse'),
    newGetBackupVaultAccessPolicyResponse,

    -- ** GetBackupVaultNotifications
    GetBackupVaultNotifications (GetBackupVaultNotifications'),
    newGetBackupVaultNotifications,
    GetBackupVaultNotificationsResponse (GetBackupVaultNotificationsResponse'),
    newGetBackupVaultNotificationsResponse,

    -- ** GetLegalHold
    GetLegalHold (GetLegalHold'),
    newGetLegalHold,
    GetLegalHoldResponse (GetLegalHoldResponse'),
    newGetLegalHoldResponse,

    -- ** GetRecoveryPointRestoreMetadata
    GetRecoveryPointRestoreMetadata (GetRecoveryPointRestoreMetadata'),
    newGetRecoveryPointRestoreMetadata,
    GetRecoveryPointRestoreMetadataResponse (GetRecoveryPointRestoreMetadataResponse'),
    newGetRecoveryPointRestoreMetadataResponse,

    -- ** GetSupportedResourceTypes
    GetSupportedResourceTypes (GetSupportedResourceTypes'),
    newGetSupportedResourceTypes,
    GetSupportedResourceTypesResponse (GetSupportedResourceTypesResponse'),
    newGetSupportedResourceTypesResponse,

    -- ** ListBackupJobs (Paginated)
    ListBackupJobs (ListBackupJobs'),
    newListBackupJobs,
    ListBackupJobsResponse (ListBackupJobsResponse'),
    newListBackupJobsResponse,

    -- ** ListBackupPlanTemplates (Paginated)
    ListBackupPlanTemplates (ListBackupPlanTemplates'),
    newListBackupPlanTemplates,
    ListBackupPlanTemplatesResponse (ListBackupPlanTemplatesResponse'),
    newListBackupPlanTemplatesResponse,

    -- ** ListBackupPlanVersions (Paginated)
    ListBackupPlanVersions (ListBackupPlanVersions'),
    newListBackupPlanVersions,
    ListBackupPlanVersionsResponse (ListBackupPlanVersionsResponse'),
    newListBackupPlanVersionsResponse,

    -- ** ListBackupPlans (Paginated)
    ListBackupPlans (ListBackupPlans'),
    newListBackupPlans,
    ListBackupPlansResponse (ListBackupPlansResponse'),
    newListBackupPlansResponse,

    -- ** ListBackupSelections (Paginated)
    ListBackupSelections (ListBackupSelections'),
    newListBackupSelections,
    ListBackupSelectionsResponse (ListBackupSelectionsResponse'),
    newListBackupSelectionsResponse,

    -- ** ListBackupVaults (Paginated)
    ListBackupVaults (ListBackupVaults'),
    newListBackupVaults,
    ListBackupVaultsResponse (ListBackupVaultsResponse'),
    newListBackupVaultsResponse,

    -- ** ListCopyJobs (Paginated)
    ListCopyJobs (ListCopyJobs'),
    newListCopyJobs,
    ListCopyJobsResponse (ListCopyJobsResponse'),
    newListCopyJobsResponse,

    -- ** ListFrameworks
    ListFrameworks (ListFrameworks'),
    newListFrameworks,
    ListFrameworksResponse (ListFrameworksResponse'),
    newListFrameworksResponse,

    -- ** ListLegalHolds (Paginated)
    ListLegalHolds (ListLegalHolds'),
    newListLegalHolds,
    ListLegalHoldsResponse (ListLegalHoldsResponse'),
    newListLegalHoldsResponse,

    -- ** ListProtectedResources (Paginated)
    ListProtectedResources (ListProtectedResources'),
    newListProtectedResources,
    ListProtectedResourcesResponse (ListProtectedResourcesResponse'),
    newListProtectedResourcesResponse,

    -- ** ListRecoveryPointsByBackupVault (Paginated)
    ListRecoveryPointsByBackupVault (ListRecoveryPointsByBackupVault'),
    newListRecoveryPointsByBackupVault,
    ListRecoveryPointsByBackupVaultResponse (ListRecoveryPointsByBackupVaultResponse'),
    newListRecoveryPointsByBackupVaultResponse,

    -- ** ListRecoveryPointsByLegalHold (Paginated)
    ListRecoveryPointsByLegalHold (ListRecoveryPointsByLegalHold'),
    newListRecoveryPointsByLegalHold,
    ListRecoveryPointsByLegalHoldResponse (ListRecoveryPointsByLegalHoldResponse'),
    newListRecoveryPointsByLegalHoldResponse,

    -- ** ListRecoveryPointsByResource (Paginated)
    ListRecoveryPointsByResource (ListRecoveryPointsByResource'),
    newListRecoveryPointsByResource,
    ListRecoveryPointsByResourceResponse (ListRecoveryPointsByResourceResponse'),
    newListRecoveryPointsByResourceResponse,

    -- ** ListReportJobs
    ListReportJobs (ListReportJobs'),
    newListReportJobs,
    ListReportJobsResponse (ListReportJobsResponse'),
    newListReportJobsResponse,

    -- ** ListReportPlans
    ListReportPlans (ListReportPlans'),
    newListReportPlans,
    ListReportPlansResponse (ListReportPlansResponse'),
    newListReportPlansResponse,

    -- ** ListRestoreJobs (Paginated)
    ListRestoreJobs (ListRestoreJobs'),
    newListRestoreJobs,
    ListRestoreJobsResponse (ListRestoreJobsResponse'),
    newListRestoreJobsResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** PutBackupVaultAccessPolicy
    PutBackupVaultAccessPolicy (PutBackupVaultAccessPolicy'),
    newPutBackupVaultAccessPolicy,
    PutBackupVaultAccessPolicyResponse (PutBackupVaultAccessPolicyResponse'),
    newPutBackupVaultAccessPolicyResponse,

    -- ** PutBackupVaultLockConfiguration
    PutBackupVaultLockConfiguration (PutBackupVaultLockConfiguration'),
    newPutBackupVaultLockConfiguration,
    PutBackupVaultLockConfigurationResponse (PutBackupVaultLockConfigurationResponse'),
    newPutBackupVaultLockConfigurationResponse,

    -- ** PutBackupVaultNotifications
    PutBackupVaultNotifications (PutBackupVaultNotifications'),
    newPutBackupVaultNotifications,
    PutBackupVaultNotificationsResponse (PutBackupVaultNotificationsResponse'),
    newPutBackupVaultNotificationsResponse,

    -- ** StartBackupJob
    StartBackupJob (StartBackupJob'),
    newStartBackupJob,
    StartBackupJobResponse (StartBackupJobResponse'),
    newStartBackupJobResponse,

    -- ** StartCopyJob
    StartCopyJob (StartCopyJob'),
    newStartCopyJob,
    StartCopyJobResponse (StartCopyJobResponse'),
    newStartCopyJobResponse,

    -- ** StartReportJob
    StartReportJob (StartReportJob'),
    newStartReportJob,
    StartReportJobResponse (StartReportJobResponse'),
    newStartReportJobResponse,

    -- ** StartRestoreJob
    StartRestoreJob (StartRestoreJob'),
    newStartRestoreJob,
    StartRestoreJobResponse (StartRestoreJobResponse'),
    newStartRestoreJobResponse,

    -- ** StopBackupJob
    StopBackupJob (StopBackupJob'),
    newStopBackupJob,
    StopBackupJobResponse (StopBackupJobResponse'),
    newStopBackupJobResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateBackupPlan
    UpdateBackupPlan (UpdateBackupPlan'),
    newUpdateBackupPlan,
    UpdateBackupPlanResponse (UpdateBackupPlanResponse'),
    newUpdateBackupPlanResponse,

    -- ** UpdateFramework
    UpdateFramework (UpdateFramework'),
    newUpdateFramework,
    UpdateFrameworkResponse (UpdateFrameworkResponse'),
    newUpdateFrameworkResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** UpdateRecoveryPointLifecycle
    UpdateRecoveryPointLifecycle (UpdateRecoveryPointLifecycle'),
    newUpdateRecoveryPointLifecycle,
    UpdateRecoveryPointLifecycleResponse (UpdateRecoveryPointLifecycleResponse'),
    newUpdateRecoveryPointLifecycleResponse,

    -- ** UpdateRegionSettings
    UpdateRegionSettings (UpdateRegionSettings'),
    newUpdateRegionSettings,
    UpdateRegionSettingsResponse (UpdateRegionSettingsResponse'),
    newUpdateRegionSettingsResponse,

    -- ** UpdateReportPlan
    UpdateReportPlan (UpdateReportPlan'),
    newUpdateReportPlan,
    UpdateReportPlanResponse (UpdateReportPlanResponse'),
    newUpdateReportPlanResponse,

    -- * Types

    -- ** BackupJobState
    BackupJobState (..),

    -- ** BackupVaultEvent
    BackupVaultEvent (..),

    -- ** ConditionType
    ConditionType (..),

    -- ** CopyJobState
    CopyJobState (..),

    -- ** LegalHoldStatus
    LegalHoldStatus (..),

    -- ** RecoveryPointStatus
    RecoveryPointStatus (..),

    -- ** RestoreJobStatus
    RestoreJobStatus (..),

    -- ** StorageClass
    StorageClass (..),

    -- ** AdvancedBackupSetting
    AdvancedBackupSetting (AdvancedBackupSetting'),
    newAdvancedBackupSetting,

    -- ** BackupJob
    BackupJob (BackupJob'),
    newBackupJob,

    -- ** BackupPlan
    BackupPlan (BackupPlan'),
    newBackupPlan,

    -- ** BackupPlanInput
    BackupPlanInput (BackupPlanInput'),
    newBackupPlanInput,

    -- ** BackupPlanTemplatesListMember
    BackupPlanTemplatesListMember (BackupPlanTemplatesListMember'),
    newBackupPlanTemplatesListMember,

    -- ** BackupPlansListMember
    BackupPlansListMember (BackupPlansListMember'),
    newBackupPlansListMember,

    -- ** BackupRule
    BackupRule (BackupRule'),
    newBackupRule,

    -- ** BackupRuleInput
    BackupRuleInput (BackupRuleInput'),
    newBackupRuleInput,

    -- ** BackupSelection
    BackupSelection (BackupSelection'),
    newBackupSelection,

    -- ** BackupSelectionsListMember
    BackupSelectionsListMember (BackupSelectionsListMember'),
    newBackupSelectionsListMember,

    -- ** BackupVaultListMember
    BackupVaultListMember (BackupVaultListMember'),
    newBackupVaultListMember,

    -- ** CalculatedLifecycle
    CalculatedLifecycle (CalculatedLifecycle'),
    newCalculatedLifecycle,

    -- ** Condition
    Condition (Condition'),
    newCondition,

    -- ** ConditionParameter
    ConditionParameter (ConditionParameter'),
    newConditionParameter,

    -- ** Conditions
    Conditions (Conditions'),
    newConditions,

    -- ** ControlInputParameter
    ControlInputParameter (ControlInputParameter'),
    newControlInputParameter,

    -- ** ControlScope
    ControlScope (ControlScope'),
    newControlScope,

    -- ** CopyAction
    CopyAction (CopyAction'),
    newCopyAction,

    -- ** CopyJob
    CopyJob (CopyJob'),
    newCopyJob,

    -- ** DateRange
    DateRange (DateRange'),
    newDateRange,

    -- ** Framework
    Framework (Framework'),
    newFramework,

    -- ** FrameworkControl
    FrameworkControl (FrameworkControl'),
    newFrameworkControl,

    -- ** LegalHold
    LegalHold (LegalHold'),
    newLegalHold,

    -- ** Lifecycle
    Lifecycle (Lifecycle'),
    newLifecycle,

    -- ** ProtectedResource
    ProtectedResource (ProtectedResource'),
    newProtectedResource,

    -- ** RecoveryPointByBackupVault
    RecoveryPointByBackupVault (RecoveryPointByBackupVault'),
    newRecoveryPointByBackupVault,

    -- ** RecoveryPointByResource
    RecoveryPointByResource (RecoveryPointByResource'),
    newRecoveryPointByResource,

    -- ** RecoveryPointCreator
    RecoveryPointCreator (RecoveryPointCreator'),
    newRecoveryPointCreator,

    -- ** RecoveryPointMember
    RecoveryPointMember (RecoveryPointMember'),
    newRecoveryPointMember,

    -- ** RecoveryPointSelection
    RecoveryPointSelection (RecoveryPointSelection'),
    newRecoveryPointSelection,

    -- ** ReportDeliveryChannel
    ReportDeliveryChannel (ReportDeliveryChannel'),
    newReportDeliveryChannel,

    -- ** ReportDestination
    ReportDestination (ReportDestination'),
    newReportDestination,

    -- ** ReportJob
    ReportJob (ReportJob'),
    newReportJob,

    -- ** ReportPlan
    ReportPlan (ReportPlan'),
    newReportPlan,

    -- ** ReportSetting
    ReportSetting (ReportSetting'),
    newReportSetting,

    -- ** RestoreJobsListMember
    RestoreJobsListMember (RestoreJobsListMember'),
    newRestoreJobsListMember,
  )
where

import Amazonka.Backup.CancelLegalHold
import Amazonka.Backup.CreateBackupPlan
import Amazonka.Backup.CreateBackupSelection
import Amazonka.Backup.CreateBackupVault
import Amazonka.Backup.CreateFramework
import Amazonka.Backup.CreateLegalHold
import Amazonka.Backup.CreateReportPlan
import Amazonka.Backup.DeleteBackupPlan
import Amazonka.Backup.DeleteBackupSelection
import Amazonka.Backup.DeleteBackupVault
import Amazonka.Backup.DeleteBackupVaultAccessPolicy
import Amazonka.Backup.DeleteBackupVaultLockConfiguration
import Amazonka.Backup.DeleteBackupVaultNotifications
import Amazonka.Backup.DeleteFramework
import Amazonka.Backup.DeleteRecoveryPoint
import Amazonka.Backup.DeleteReportPlan
import Amazonka.Backup.DescribeBackupJob
import Amazonka.Backup.DescribeBackupVault
import Amazonka.Backup.DescribeCopyJob
import Amazonka.Backup.DescribeFramework
import Amazonka.Backup.DescribeGlobalSettings
import Amazonka.Backup.DescribeProtectedResource
import Amazonka.Backup.DescribeRecoveryPoint
import Amazonka.Backup.DescribeRegionSettings
import Amazonka.Backup.DescribeReportJob
import Amazonka.Backup.DescribeReportPlan
import Amazonka.Backup.DescribeRestoreJob
import Amazonka.Backup.DisassociateRecoveryPoint
import Amazonka.Backup.DisassociateRecoveryPointFromParent
import Amazonka.Backup.ExportBackupPlanTemplate
import Amazonka.Backup.GetBackupPlan
import Amazonka.Backup.GetBackupPlanFromJSON
import Amazonka.Backup.GetBackupPlanFromTemplate
import Amazonka.Backup.GetBackupSelection
import Amazonka.Backup.GetBackupVaultAccessPolicy
import Amazonka.Backup.GetBackupVaultNotifications
import Amazonka.Backup.GetLegalHold
import Amazonka.Backup.GetRecoveryPointRestoreMetadata
import Amazonka.Backup.GetSupportedResourceTypes
import Amazonka.Backup.Lens
import Amazonka.Backup.ListBackupJobs
import Amazonka.Backup.ListBackupPlanTemplates
import Amazonka.Backup.ListBackupPlanVersions
import Amazonka.Backup.ListBackupPlans
import Amazonka.Backup.ListBackupSelections
import Amazonka.Backup.ListBackupVaults
import Amazonka.Backup.ListCopyJobs
import Amazonka.Backup.ListFrameworks
import Amazonka.Backup.ListLegalHolds
import Amazonka.Backup.ListProtectedResources
import Amazonka.Backup.ListRecoveryPointsByBackupVault
import Amazonka.Backup.ListRecoveryPointsByLegalHold
import Amazonka.Backup.ListRecoveryPointsByResource
import Amazonka.Backup.ListReportJobs
import Amazonka.Backup.ListReportPlans
import Amazonka.Backup.ListRestoreJobs
import Amazonka.Backup.ListTags
import Amazonka.Backup.PutBackupVaultAccessPolicy
import Amazonka.Backup.PutBackupVaultLockConfiguration
import Amazonka.Backup.PutBackupVaultNotifications
import Amazonka.Backup.StartBackupJob
import Amazonka.Backup.StartCopyJob
import Amazonka.Backup.StartReportJob
import Amazonka.Backup.StartRestoreJob
import Amazonka.Backup.StopBackupJob
import Amazonka.Backup.TagResource
import Amazonka.Backup.Types
import Amazonka.Backup.UntagResource
import Amazonka.Backup.UpdateBackupPlan
import Amazonka.Backup.UpdateFramework
import Amazonka.Backup.UpdateGlobalSettings
import Amazonka.Backup.UpdateRecoveryPointLifecycle
import Amazonka.Backup.UpdateRegionSettings
import Amazonka.Backup.UpdateReportPlan
import Amazonka.Backup.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'Backup'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
