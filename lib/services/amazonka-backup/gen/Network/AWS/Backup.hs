{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.Backup
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Backup
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** DependencyFailureException
    _DependencyFailureException,

    -- ** InvalidResourceStateException
    _InvalidResourceStateException,

    -- ** ConflictException
    _ConflictException,

    -- ** InvalidParameterValueException
    _InvalidParameterValueException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** MissingParameterValueException
    _MissingParameterValueException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateBackupPlan
    UpdateBackupPlan (UpdateBackupPlan'),
    newUpdateBackupPlan,
    UpdateBackupPlanResponse (UpdateBackupPlanResponse'),
    newUpdateBackupPlanResponse,

    -- ** DeleteBackupPlan
    DeleteBackupPlan (DeleteBackupPlan'),
    newDeleteBackupPlan,
    DeleteBackupPlanResponse (DeleteBackupPlanResponse'),
    newDeleteBackupPlanResponse,

    -- ** DescribeBackupJob
    DescribeBackupJob (DescribeBackupJob'),
    newDescribeBackupJob,
    DescribeBackupJobResponse (DescribeBackupJobResponse'),
    newDescribeBackupJobResponse,

    -- ** ListBackupPlanTemplates
    ListBackupPlanTemplates (ListBackupPlanTemplates'),
    newListBackupPlanTemplates,
    ListBackupPlanTemplatesResponse (ListBackupPlanTemplatesResponse'),
    newListBackupPlanTemplatesResponse,

    -- ** DeleteReportPlan
    DeleteReportPlan (DeleteReportPlan'),
    newDeleteReportPlan,
    DeleteReportPlanResponse (DeleteReportPlanResponse'),
    newDeleteReportPlanResponse,

    -- ** UpdateReportPlan
    UpdateReportPlan (UpdateReportPlan'),
    newUpdateReportPlan,
    UpdateReportPlanResponse (UpdateReportPlanResponse'),
    newUpdateReportPlanResponse,

    -- ** DescribeReportJob
    DescribeReportJob (DescribeReportJob'),
    newDescribeReportJob,
    DescribeReportJobResponse (DescribeReportJobResponse'),
    newDescribeReportJobResponse,

    -- ** UpdateRegionSettings
    UpdateRegionSettings (UpdateRegionSettings'),
    newUpdateRegionSettings,
    UpdateRegionSettingsResponse (UpdateRegionSettingsResponse'),
    newUpdateRegionSettingsResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** DeleteBackupSelection
    DeleteBackupSelection (DeleteBackupSelection'),
    newDeleteBackupSelection,
    DeleteBackupSelectionResponse (DeleteBackupSelectionResponse'),
    newDeleteBackupSelectionResponse,

    -- ** DescribeCopyJob
    DescribeCopyJob (DescribeCopyJob'),
    newDescribeCopyJob,
    DescribeCopyJobResponse (DescribeCopyJobResponse'),
    newDescribeCopyJobResponse,

    -- ** DescribeRecoveryPoint
    DescribeRecoveryPoint (DescribeRecoveryPoint'),
    newDescribeRecoveryPoint,
    DescribeRecoveryPointResponse (DescribeRecoveryPointResponse'),
    newDescribeRecoveryPointResponse,

    -- ** DescribeRestoreJob
    DescribeRestoreJob (DescribeRestoreJob'),
    newDescribeRestoreJob,
    DescribeRestoreJobResponse (DescribeRestoreJobResponse'),
    newDescribeRestoreJobResponse,

    -- ** StartCopyJob
    StartCopyJob (StartCopyJob'),
    newStartCopyJob,
    StartCopyJobResponse (StartCopyJobResponse'),
    newStartCopyJobResponse,

    -- ** GetBackupPlanFromTemplate
    GetBackupPlanFromTemplate (GetBackupPlanFromTemplate'),
    newGetBackupPlanFromTemplate,
    GetBackupPlanFromTemplateResponse (GetBackupPlanFromTemplateResponse'),
    newGetBackupPlanFromTemplateResponse,

    -- ** DisassociateRecoveryPoint
    DisassociateRecoveryPoint (DisassociateRecoveryPoint'),
    newDisassociateRecoveryPoint,
    DisassociateRecoveryPointResponse (DisassociateRecoveryPointResponse'),
    newDisassociateRecoveryPointResponse,

    -- ** DeleteBackupVault
    DeleteBackupVault (DeleteBackupVault'),
    newDeleteBackupVault,
    DeleteBackupVaultResponse (DeleteBackupVaultResponse'),
    newDeleteBackupVaultResponse,

    -- ** DeleteFramework
    DeleteFramework (DeleteFramework'),
    newDeleteFramework,
    DeleteFrameworkResponse (DeleteFrameworkResponse'),
    newDeleteFrameworkResponse,

    -- ** UpdateFramework
    UpdateFramework (UpdateFramework'),
    newUpdateFramework,
    UpdateFrameworkResponse (UpdateFrameworkResponse'),
    newUpdateFrameworkResponse,

    -- ** ListReportJobs
    ListReportJobs (ListReportJobs'),
    newListReportJobs,
    ListReportJobsResponse (ListReportJobsResponse'),
    newListReportJobsResponse,

    -- ** ListBackupJobs
    ListBackupJobs (ListBackupJobs'),
    newListBackupJobs,
    ListBackupJobsResponse (ListBackupJobsResponse'),
    newListBackupJobsResponse,

    -- ** DescribeReportPlan
    DescribeReportPlan (DescribeReportPlan'),
    newDescribeReportPlan,
    DescribeReportPlanResponse (DescribeReportPlanResponse'),
    newDescribeReportPlanResponse,

    -- ** DescribeRegionSettings
    DescribeRegionSettings (DescribeRegionSettings'),
    newDescribeRegionSettings,
    DescribeRegionSettingsResponse (DescribeRegionSettingsResponse'),
    newDescribeRegionSettingsResponse,

    -- ** GetBackupPlan
    GetBackupPlan (GetBackupPlan'),
    newGetBackupPlan,
    GetBackupPlanResponse (GetBackupPlanResponse'),
    newGetBackupPlanResponse,

    -- ** DescribeGlobalSettings
    DescribeGlobalSettings (DescribeGlobalSettings'),
    newDescribeGlobalSettings,
    DescribeGlobalSettingsResponse (DescribeGlobalSettingsResponse'),
    newDescribeGlobalSettingsResponse,

    -- ** ListBackupPlanVersions
    ListBackupPlanVersions (ListBackupPlanVersions'),
    newListBackupPlanVersions,
    ListBackupPlanVersionsResponse (ListBackupPlanVersionsResponse'),
    newListBackupPlanVersionsResponse,

    -- ** ListRestoreJobs
    ListRestoreJobs (ListRestoreJobs'),
    newListRestoreJobs,
    ListRestoreJobsResponse (ListRestoreJobsResponse'),
    newListRestoreJobsResponse,

    -- ** CreateReportPlan
    CreateReportPlan (CreateReportPlan'),
    newCreateReportPlan,
    CreateReportPlanResponse (CreateReportPlanResponse'),
    newCreateReportPlanResponse,

    -- ** ExportBackupPlanTemplate
    ExportBackupPlanTemplate (ExportBackupPlanTemplate'),
    newExportBackupPlanTemplate,
    ExportBackupPlanTemplateResponse (ExportBackupPlanTemplateResponse'),
    newExportBackupPlanTemplateResponse,

    -- ** StartBackupJob
    StartBackupJob (StartBackupJob'),
    newStartBackupJob,
    StartBackupJobResponse (StartBackupJobResponse'),
    newStartBackupJobResponse,

    -- ** DescribeFramework
    DescribeFramework (DescribeFramework'),
    newDescribeFramework,
    DescribeFrameworkResponse (DescribeFrameworkResponse'),
    newDescribeFrameworkResponse,

    -- ** CreateBackupPlan
    CreateBackupPlan (CreateBackupPlan'),
    newCreateBackupPlan,
    CreateBackupPlanResponse (CreateBackupPlanResponse'),
    newCreateBackupPlanResponse,

    -- ** ListProtectedResources
    ListProtectedResources (ListProtectedResources'),
    newListProtectedResources,
    ListProtectedResourcesResponse (ListProtectedResourcesResponse'),
    newListProtectedResourcesResponse,

    -- ** StartReportJob
    StartReportJob (StartReportJob'),
    newStartReportJob,
    StartReportJobResponse (StartReportJobResponse'),
    newStartReportJobResponse,

    -- ** DescribeBackupVault
    DescribeBackupVault (DescribeBackupVault'),
    newDescribeBackupVault,
    DescribeBackupVaultResponse (DescribeBackupVaultResponse'),
    newDescribeBackupVaultResponse,

    -- ** GetBackupVaultNotifications
    GetBackupVaultNotifications (GetBackupVaultNotifications'),
    newGetBackupVaultNotifications,
    GetBackupVaultNotificationsResponse (GetBackupVaultNotificationsResponse'),
    newGetBackupVaultNotificationsResponse,

    -- ** ListReportPlans
    ListReportPlans (ListReportPlans'),
    newListReportPlans,
    ListReportPlansResponse (ListReportPlansResponse'),
    newListReportPlansResponse,

    -- ** GetRecoveryPointRestoreMetadata
    GetRecoveryPointRestoreMetadata (GetRecoveryPointRestoreMetadata'),
    newGetRecoveryPointRestoreMetadata,
    GetRecoveryPointRestoreMetadataResponse (GetRecoveryPointRestoreMetadataResponse'),
    newGetRecoveryPointRestoreMetadataResponse,

    -- ** ListBackupPlans
    ListBackupPlans (ListBackupPlans'),
    newListBackupPlans,
    ListBackupPlansResponse (ListBackupPlansResponse'),
    newListBackupPlansResponse,

    -- ** StartRestoreJob
    StartRestoreJob (StartRestoreJob'),
    newStartRestoreJob,
    StartRestoreJobResponse (StartRestoreJobResponse'),
    newStartRestoreJobResponse,

    -- ** ListBackupSelections
    ListBackupSelections (ListBackupSelections'),
    newListBackupSelections,
    ListBackupSelectionsResponse (ListBackupSelectionsResponse'),
    newListBackupSelectionsResponse,

    -- ** ListRecoveryPointsByResource
    ListRecoveryPointsByResource (ListRecoveryPointsByResource'),
    newListRecoveryPointsByResource,
    ListRecoveryPointsByResourceResponse (ListRecoveryPointsByResourceResponse'),
    newListRecoveryPointsByResourceResponse,

    -- ** CreateBackupSelection
    CreateBackupSelection (CreateBackupSelection'),
    newCreateBackupSelection,
    CreateBackupSelectionResponse (CreateBackupSelectionResponse'),
    newCreateBackupSelectionResponse,

    -- ** ListFrameworks
    ListFrameworks (ListFrameworks'),
    newListFrameworks,
    ListFrameworksResponse (ListFrameworksResponse'),
    newListFrameworksResponse,

    -- ** DescribeProtectedResource
    DescribeProtectedResource (DescribeProtectedResource'),
    newDescribeProtectedResource,
    DescribeProtectedResourceResponse (DescribeProtectedResourceResponse'),
    newDescribeProtectedResourceResponse,

    -- ** GetBackupPlanFromJSON
    GetBackupPlanFromJSON (GetBackupPlanFromJSON'),
    newGetBackupPlanFromJSON,
    GetBackupPlanFromJSONResponse (GetBackupPlanFromJSONResponse'),
    newGetBackupPlanFromJSONResponse,

    -- ** ListBackupVaults
    ListBackupVaults (ListBackupVaults'),
    newListBackupVaults,
    ListBackupVaultsResponse (ListBackupVaultsResponse'),
    newListBackupVaultsResponse,

    -- ** GetBackupSelection
    GetBackupSelection (GetBackupSelection'),
    newGetBackupSelection,
    GetBackupSelectionResponse (GetBackupSelectionResponse'),
    newGetBackupSelectionResponse,

    -- ** CreateBackupVault
    CreateBackupVault (CreateBackupVault'),
    newCreateBackupVault,
    CreateBackupVaultResponse (CreateBackupVaultResponse'),
    newCreateBackupVaultResponse,

    -- ** UpdateRecoveryPointLifecycle
    UpdateRecoveryPointLifecycle (UpdateRecoveryPointLifecycle'),
    newUpdateRecoveryPointLifecycle,
    UpdateRecoveryPointLifecycleResponse (UpdateRecoveryPointLifecycleResponse'),
    newUpdateRecoveryPointLifecycleResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateFramework
    CreateFramework (CreateFramework'),
    newCreateFramework,
    CreateFrameworkResponse (CreateFrameworkResponse'),
    newCreateFrameworkResponse,

    -- ** PutBackupVaultNotifications
    PutBackupVaultNotifications (PutBackupVaultNotifications'),
    newPutBackupVaultNotifications,
    PutBackupVaultNotificationsResponse (PutBackupVaultNotificationsResponse'),
    newPutBackupVaultNotificationsResponse,

    -- ** DeleteBackupVaultNotifications
    DeleteBackupVaultNotifications (DeleteBackupVaultNotifications'),
    newDeleteBackupVaultNotifications,
    DeleteBackupVaultNotificationsResponse (DeleteBackupVaultNotificationsResponse'),
    newDeleteBackupVaultNotificationsResponse,

    -- ** ListTags
    ListTags (ListTags'),
    newListTags,
    ListTagsResponse (ListTagsResponse'),
    newListTagsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListCopyJobs
    ListCopyJobs (ListCopyJobs'),
    newListCopyJobs,
    ListCopyJobsResponse (ListCopyJobsResponse'),
    newListCopyJobsResponse,

    -- ** DeleteBackupVaultLockConfiguration
    DeleteBackupVaultLockConfiguration (DeleteBackupVaultLockConfiguration'),
    newDeleteBackupVaultLockConfiguration,
    DeleteBackupVaultLockConfigurationResponse (DeleteBackupVaultLockConfigurationResponse'),
    newDeleteBackupVaultLockConfigurationResponse,

    -- ** GetBackupVaultAccessPolicy
    GetBackupVaultAccessPolicy (GetBackupVaultAccessPolicy'),
    newGetBackupVaultAccessPolicy,
    GetBackupVaultAccessPolicyResponse (GetBackupVaultAccessPolicyResponse'),
    newGetBackupVaultAccessPolicyResponse,

    -- ** DeleteRecoveryPoint
    DeleteRecoveryPoint (DeleteRecoveryPoint'),
    newDeleteRecoveryPoint,
    DeleteRecoveryPointResponse (DeleteRecoveryPointResponse'),
    newDeleteRecoveryPointResponse,

    -- ** PutBackupVaultLockConfiguration
    PutBackupVaultLockConfiguration (PutBackupVaultLockConfiguration'),
    newPutBackupVaultLockConfiguration,
    PutBackupVaultLockConfigurationResponse (PutBackupVaultLockConfigurationResponse'),
    newPutBackupVaultLockConfigurationResponse,

    -- ** GetSupportedResourceTypes
    GetSupportedResourceTypes (GetSupportedResourceTypes'),
    newGetSupportedResourceTypes,
    GetSupportedResourceTypesResponse (GetSupportedResourceTypesResponse'),
    newGetSupportedResourceTypesResponse,

    -- ** StopBackupJob
    StopBackupJob (StopBackupJob'),
    newStopBackupJob,
    StopBackupJobResponse (StopBackupJobResponse'),
    newStopBackupJobResponse,

    -- ** ListRecoveryPointsByBackupVault
    ListRecoveryPointsByBackupVault (ListRecoveryPointsByBackupVault'),
    newListRecoveryPointsByBackupVault,
    ListRecoveryPointsByBackupVaultResponse (ListRecoveryPointsByBackupVaultResponse'),
    newListRecoveryPointsByBackupVaultResponse,

    -- ** PutBackupVaultAccessPolicy
    PutBackupVaultAccessPolicy (PutBackupVaultAccessPolicy'),
    newPutBackupVaultAccessPolicy,
    PutBackupVaultAccessPolicyResponse (PutBackupVaultAccessPolicyResponse'),
    newPutBackupVaultAccessPolicyResponse,

    -- ** DeleteBackupVaultAccessPolicy
    DeleteBackupVaultAccessPolicy (DeleteBackupVaultAccessPolicy'),
    newDeleteBackupVaultAccessPolicy,
    DeleteBackupVaultAccessPolicyResponse (DeleteBackupVaultAccessPolicyResponse'),
    newDeleteBackupVaultAccessPolicyResponse,

    -- * Types

    -- ** BackupJobState
    BackupJobState (..),

    -- ** BackupVaultEvent
    BackupVaultEvent (..),

    -- ** ConditionType
    ConditionType (..),

    -- ** CopyJobState
    CopyJobState (..),

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

    -- ** Framework
    Framework (Framework'),
    newFramework,

    -- ** FrameworkControl
    FrameworkControl (FrameworkControl'),
    newFrameworkControl,

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

import Network.AWS.Backup.CreateBackupPlan
import Network.AWS.Backup.CreateBackupSelection
import Network.AWS.Backup.CreateBackupVault
import Network.AWS.Backup.CreateFramework
import Network.AWS.Backup.CreateReportPlan
import Network.AWS.Backup.DeleteBackupPlan
import Network.AWS.Backup.DeleteBackupSelection
import Network.AWS.Backup.DeleteBackupVault
import Network.AWS.Backup.DeleteBackupVaultAccessPolicy
import Network.AWS.Backup.DeleteBackupVaultLockConfiguration
import Network.AWS.Backup.DeleteBackupVaultNotifications
import Network.AWS.Backup.DeleteFramework
import Network.AWS.Backup.DeleteRecoveryPoint
import Network.AWS.Backup.DeleteReportPlan
import Network.AWS.Backup.DescribeBackupJob
import Network.AWS.Backup.DescribeBackupVault
import Network.AWS.Backup.DescribeCopyJob
import Network.AWS.Backup.DescribeFramework
import Network.AWS.Backup.DescribeGlobalSettings
import Network.AWS.Backup.DescribeProtectedResource
import Network.AWS.Backup.DescribeRecoveryPoint
import Network.AWS.Backup.DescribeRegionSettings
import Network.AWS.Backup.DescribeReportJob
import Network.AWS.Backup.DescribeReportPlan
import Network.AWS.Backup.DescribeRestoreJob
import Network.AWS.Backup.DisassociateRecoveryPoint
import Network.AWS.Backup.ExportBackupPlanTemplate
import Network.AWS.Backup.GetBackupPlan
import Network.AWS.Backup.GetBackupPlanFromJSON
import Network.AWS.Backup.GetBackupPlanFromTemplate
import Network.AWS.Backup.GetBackupSelection
import Network.AWS.Backup.GetBackupVaultAccessPolicy
import Network.AWS.Backup.GetBackupVaultNotifications
import Network.AWS.Backup.GetRecoveryPointRestoreMetadata
import Network.AWS.Backup.GetSupportedResourceTypes
import Network.AWS.Backup.Lens
import Network.AWS.Backup.ListBackupJobs
import Network.AWS.Backup.ListBackupPlanTemplates
import Network.AWS.Backup.ListBackupPlanVersions
import Network.AWS.Backup.ListBackupPlans
import Network.AWS.Backup.ListBackupSelections
import Network.AWS.Backup.ListBackupVaults
import Network.AWS.Backup.ListCopyJobs
import Network.AWS.Backup.ListFrameworks
import Network.AWS.Backup.ListProtectedResources
import Network.AWS.Backup.ListRecoveryPointsByBackupVault
import Network.AWS.Backup.ListRecoveryPointsByResource
import Network.AWS.Backup.ListReportJobs
import Network.AWS.Backup.ListReportPlans
import Network.AWS.Backup.ListRestoreJobs
import Network.AWS.Backup.ListTags
import Network.AWS.Backup.PutBackupVaultAccessPolicy
import Network.AWS.Backup.PutBackupVaultLockConfiguration
import Network.AWS.Backup.PutBackupVaultNotifications
import Network.AWS.Backup.StartBackupJob
import Network.AWS.Backup.StartCopyJob
import Network.AWS.Backup.StartReportJob
import Network.AWS.Backup.StartRestoreJob
import Network.AWS.Backup.StopBackupJob
import Network.AWS.Backup.TagResource
import Network.AWS.Backup.Types
import Network.AWS.Backup.UntagResource
import Network.AWS.Backup.UpdateBackupPlan
import Network.AWS.Backup.UpdateFramework
import Network.AWS.Backup.UpdateGlobalSettings
import Network.AWS.Backup.UpdateRecoveryPointLifecycle
import Network.AWS.Backup.UpdateRegionSettings
import Network.AWS.Backup.UpdateReportPlan
import Network.AWS.Backup.Waiters

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
