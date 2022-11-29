{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.AuditManager
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-07-25@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Welcome to the Audit Manager API reference. This guide is for developers
-- who need detailed information about the Audit Manager API operations,
-- data types, and errors.
--
-- Audit Manager is a service that provides automated evidence collection
-- so that you can continually audit your Amazon Web Services usage. You
-- can use it to assess the effectiveness of your controls, manage risk,
-- and simplify compliance.
--
-- Audit Manager provides prebuilt frameworks that structure and automate
-- assessments for a given compliance standard. Frameworks include a
-- prebuilt collection of controls with descriptions and testing
-- procedures. These controls are grouped according to the requirements of
-- the specified compliance standard or regulation. You can also customize
-- frameworks and controls to support internal audits with specific
-- requirements.
--
-- Use the following links to get started with the Audit Manager API:
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_Operations.html Actions>:
--     An alphabetical list of all Audit Manager API operations.
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/API_Types.html Data types>:
--     An alphabetical list of all Audit Manager data types.
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/CommonParameters.html Common parameters>:
--     Parameters that all Query operations can use.
--
-- -   <https://docs.aws.amazon.com/audit-manager/latest/APIReference/CommonErrors.html Common errors>:
--     Client and server errors that all operations can return.
--
-- If you\'re new to Audit Manager, we recommend that you review the
-- <https://docs.aws.amazon.com/audit-manager/latest/userguide/what-is.html Audit Manager User Guide>.
module Amazonka.AuditManager
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateAssessmentReportEvidenceFolder
    AssociateAssessmentReportEvidenceFolder (AssociateAssessmentReportEvidenceFolder'),
    newAssociateAssessmentReportEvidenceFolder,
    AssociateAssessmentReportEvidenceFolderResponse (AssociateAssessmentReportEvidenceFolderResponse'),
    newAssociateAssessmentReportEvidenceFolderResponse,

    -- ** BatchAssociateAssessmentReportEvidence
    BatchAssociateAssessmentReportEvidence (BatchAssociateAssessmentReportEvidence'),
    newBatchAssociateAssessmentReportEvidence,
    BatchAssociateAssessmentReportEvidenceResponse (BatchAssociateAssessmentReportEvidenceResponse'),
    newBatchAssociateAssessmentReportEvidenceResponse,

    -- ** BatchCreateDelegationByAssessment
    BatchCreateDelegationByAssessment (BatchCreateDelegationByAssessment'),
    newBatchCreateDelegationByAssessment,
    BatchCreateDelegationByAssessmentResponse (BatchCreateDelegationByAssessmentResponse'),
    newBatchCreateDelegationByAssessmentResponse,

    -- ** BatchDeleteDelegationByAssessment
    BatchDeleteDelegationByAssessment (BatchDeleteDelegationByAssessment'),
    newBatchDeleteDelegationByAssessment,
    BatchDeleteDelegationByAssessmentResponse (BatchDeleteDelegationByAssessmentResponse'),
    newBatchDeleteDelegationByAssessmentResponse,

    -- ** BatchDisassociateAssessmentReportEvidence
    BatchDisassociateAssessmentReportEvidence (BatchDisassociateAssessmentReportEvidence'),
    newBatchDisassociateAssessmentReportEvidence,
    BatchDisassociateAssessmentReportEvidenceResponse (BatchDisassociateAssessmentReportEvidenceResponse'),
    newBatchDisassociateAssessmentReportEvidenceResponse,

    -- ** BatchImportEvidenceToAssessmentControl
    BatchImportEvidenceToAssessmentControl (BatchImportEvidenceToAssessmentControl'),
    newBatchImportEvidenceToAssessmentControl,
    BatchImportEvidenceToAssessmentControlResponse (BatchImportEvidenceToAssessmentControlResponse'),
    newBatchImportEvidenceToAssessmentControlResponse,

    -- ** CreateAssessment
    CreateAssessment (CreateAssessment'),
    newCreateAssessment,
    CreateAssessmentResponse (CreateAssessmentResponse'),
    newCreateAssessmentResponse,

    -- ** CreateAssessmentFramework
    CreateAssessmentFramework (CreateAssessmentFramework'),
    newCreateAssessmentFramework,
    CreateAssessmentFrameworkResponse (CreateAssessmentFrameworkResponse'),
    newCreateAssessmentFrameworkResponse,

    -- ** CreateAssessmentReport
    CreateAssessmentReport (CreateAssessmentReport'),
    newCreateAssessmentReport,
    CreateAssessmentReportResponse (CreateAssessmentReportResponse'),
    newCreateAssessmentReportResponse,

    -- ** CreateControl
    CreateControl (CreateControl'),
    newCreateControl,
    CreateControlResponse (CreateControlResponse'),
    newCreateControlResponse,

    -- ** DeleteAssessment
    DeleteAssessment (DeleteAssessment'),
    newDeleteAssessment,
    DeleteAssessmentResponse (DeleteAssessmentResponse'),
    newDeleteAssessmentResponse,

    -- ** DeleteAssessmentFramework
    DeleteAssessmentFramework (DeleteAssessmentFramework'),
    newDeleteAssessmentFramework,
    DeleteAssessmentFrameworkResponse (DeleteAssessmentFrameworkResponse'),
    newDeleteAssessmentFrameworkResponse,

    -- ** DeleteAssessmentFrameworkShare
    DeleteAssessmentFrameworkShare (DeleteAssessmentFrameworkShare'),
    newDeleteAssessmentFrameworkShare,
    DeleteAssessmentFrameworkShareResponse (DeleteAssessmentFrameworkShareResponse'),
    newDeleteAssessmentFrameworkShareResponse,

    -- ** DeleteAssessmentReport
    DeleteAssessmentReport (DeleteAssessmentReport'),
    newDeleteAssessmentReport,
    DeleteAssessmentReportResponse (DeleteAssessmentReportResponse'),
    newDeleteAssessmentReportResponse,

    -- ** DeleteControl
    DeleteControl (DeleteControl'),
    newDeleteControl,
    DeleteControlResponse (DeleteControlResponse'),
    newDeleteControlResponse,

    -- ** DeregisterAccount
    DeregisterAccount (DeregisterAccount'),
    newDeregisterAccount,
    DeregisterAccountResponse (DeregisterAccountResponse'),
    newDeregisterAccountResponse,

    -- ** DeregisterOrganizationAdminAccount
    DeregisterOrganizationAdminAccount (DeregisterOrganizationAdminAccount'),
    newDeregisterOrganizationAdminAccount,
    DeregisterOrganizationAdminAccountResponse (DeregisterOrganizationAdminAccountResponse'),
    newDeregisterOrganizationAdminAccountResponse,

    -- ** DisassociateAssessmentReportEvidenceFolder
    DisassociateAssessmentReportEvidenceFolder (DisassociateAssessmentReportEvidenceFolder'),
    newDisassociateAssessmentReportEvidenceFolder,
    DisassociateAssessmentReportEvidenceFolderResponse (DisassociateAssessmentReportEvidenceFolderResponse'),
    newDisassociateAssessmentReportEvidenceFolderResponse,

    -- ** GetAccountStatus
    GetAccountStatus (GetAccountStatus'),
    newGetAccountStatus,
    GetAccountStatusResponse (GetAccountStatusResponse'),
    newGetAccountStatusResponse,

    -- ** GetAssessment
    GetAssessment (GetAssessment'),
    newGetAssessment,
    GetAssessmentResponse (GetAssessmentResponse'),
    newGetAssessmentResponse,

    -- ** GetAssessmentFramework
    GetAssessmentFramework (GetAssessmentFramework'),
    newGetAssessmentFramework,
    GetAssessmentFrameworkResponse (GetAssessmentFrameworkResponse'),
    newGetAssessmentFrameworkResponse,

    -- ** GetAssessmentReportUrl
    GetAssessmentReportUrl (GetAssessmentReportUrl'),
    newGetAssessmentReportUrl,
    GetAssessmentReportUrlResponse (GetAssessmentReportUrlResponse'),
    newGetAssessmentReportUrlResponse,

    -- ** GetChangeLogs
    GetChangeLogs (GetChangeLogs'),
    newGetChangeLogs,
    GetChangeLogsResponse (GetChangeLogsResponse'),
    newGetChangeLogsResponse,

    -- ** GetControl
    GetControl (GetControl'),
    newGetControl,
    GetControlResponse (GetControlResponse'),
    newGetControlResponse,

    -- ** GetDelegations
    GetDelegations (GetDelegations'),
    newGetDelegations,
    GetDelegationsResponse (GetDelegationsResponse'),
    newGetDelegationsResponse,

    -- ** GetEvidence
    GetEvidence (GetEvidence'),
    newGetEvidence,
    GetEvidenceResponse (GetEvidenceResponse'),
    newGetEvidenceResponse,

    -- ** GetEvidenceByEvidenceFolder
    GetEvidenceByEvidenceFolder (GetEvidenceByEvidenceFolder'),
    newGetEvidenceByEvidenceFolder,
    GetEvidenceByEvidenceFolderResponse (GetEvidenceByEvidenceFolderResponse'),
    newGetEvidenceByEvidenceFolderResponse,

    -- ** GetEvidenceFolder
    GetEvidenceFolder (GetEvidenceFolder'),
    newGetEvidenceFolder,
    GetEvidenceFolderResponse (GetEvidenceFolderResponse'),
    newGetEvidenceFolderResponse,

    -- ** GetEvidenceFoldersByAssessment
    GetEvidenceFoldersByAssessment (GetEvidenceFoldersByAssessment'),
    newGetEvidenceFoldersByAssessment,
    GetEvidenceFoldersByAssessmentResponse (GetEvidenceFoldersByAssessmentResponse'),
    newGetEvidenceFoldersByAssessmentResponse,

    -- ** GetEvidenceFoldersByAssessmentControl
    GetEvidenceFoldersByAssessmentControl (GetEvidenceFoldersByAssessmentControl'),
    newGetEvidenceFoldersByAssessmentControl,
    GetEvidenceFoldersByAssessmentControlResponse (GetEvidenceFoldersByAssessmentControlResponse'),
    newGetEvidenceFoldersByAssessmentControlResponse,

    -- ** GetInsights
    GetInsights (GetInsights'),
    newGetInsights,
    GetInsightsResponse (GetInsightsResponse'),
    newGetInsightsResponse,

    -- ** GetInsightsByAssessment
    GetInsightsByAssessment (GetInsightsByAssessment'),
    newGetInsightsByAssessment,
    GetInsightsByAssessmentResponse (GetInsightsByAssessmentResponse'),
    newGetInsightsByAssessmentResponse,

    -- ** GetOrganizationAdminAccount
    GetOrganizationAdminAccount (GetOrganizationAdminAccount'),
    newGetOrganizationAdminAccount,
    GetOrganizationAdminAccountResponse (GetOrganizationAdminAccountResponse'),
    newGetOrganizationAdminAccountResponse,

    -- ** GetServicesInScope
    GetServicesInScope (GetServicesInScope'),
    newGetServicesInScope,
    GetServicesInScopeResponse (GetServicesInScopeResponse'),
    newGetServicesInScopeResponse,

    -- ** GetSettings
    GetSettings (GetSettings'),
    newGetSettings,
    GetSettingsResponse (GetSettingsResponse'),
    newGetSettingsResponse,

    -- ** ListAssessmentControlInsightsByControlDomain
    ListAssessmentControlInsightsByControlDomain (ListAssessmentControlInsightsByControlDomain'),
    newListAssessmentControlInsightsByControlDomain,
    ListAssessmentControlInsightsByControlDomainResponse (ListAssessmentControlInsightsByControlDomainResponse'),
    newListAssessmentControlInsightsByControlDomainResponse,

    -- ** ListAssessmentFrameworkShareRequests
    ListAssessmentFrameworkShareRequests (ListAssessmentFrameworkShareRequests'),
    newListAssessmentFrameworkShareRequests,
    ListAssessmentFrameworkShareRequestsResponse (ListAssessmentFrameworkShareRequestsResponse'),
    newListAssessmentFrameworkShareRequestsResponse,

    -- ** ListAssessmentFrameworks
    ListAssessmentFrameworks (ListAssessmentFrameworks'),
    newListAssessmentFrameworks,
    ListAssessmentFrameworksResponse (ListAssessmentFrameworksResponse'),
    newListAssessmentFrameworksResponse,

    -- ** ListAssessmentReports
    ListAssessmentReports (ListAssessmentReports'),
    newListAssessmentReports,
    ListAssessmentReportsResponse (ListAssessmentReportsResponse'),
    newListAssessmentReportsResponse,

    -- ** ListAssessments
    ListAssessments (ListAssessments'),
    newListAssessments,
    ListAssessmentsResponse (ListAssessmentsResponse'),
    newListAssessmentsResponse,

    -- ** ListControlDomainInsights
    ListControlDomainInsights (ListControlDomainInsights'),
    newListControlDomainInsights,
    ListControlDomainInsightsResponse (ListControlDomainInsightsResponse'),
    newListControlDomainInsightsResponse,

    -- ** ListControlDomainInsightsByAssessment
    ListControlDomainInsightsByAssessment (ListControlDomainInsightsByAssessment'),
    newListControlDomainInsightsByAssessment,
    ListControlDomainInsightsByAssessmentResponse (ListControlDomainInsightsByAssessmentResponse'),
    newListControlDomainInsightsByAssessmentResponse,

    -- ** ListControlInsightsByControlDomain
    ListControlInsightsByControlDomain (ListControlInsightsByControlDomain'),
    newListControlInsightsByControlDomain,
    ListControlInsightsByControlDomainResponse (ListControlInsightsByControlDomainResponse'),
    newListControlInsightsByControlDomainResponse,

    -- ** ListControls
    ListControls (ListControls'),
    newListControls,
    ListControlsResponse (ListControlsResponse'),
    newListControlsResponse,

    -- ** ListKeywordsForDataSource
    ListKeywordsForDataSource (ListKeywordsForDataSource'),
    newListKeywordsForDataSource,
    ListKeywordsForDataSourceResponse (ListKeywordsForDataSourceResponse'),
    newListKeywordsForDataSourceResponse,

    -- ** ListNotifications
    ListNotifications (ListNotifications'),
    newListNotifications,
    ListNotificationsResponse (ListNotificationsResponse'),
    newListNotificationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** RegisterAccount
    RegisterAccount (RegisterAccount'),
    newRegisterAccount,
    RegisterAccountResponse (RegisterAccountResponse'),
    newRegisterAccountResponse,

    -- ** RegisterOrganizationAdminAccount
    RegisterOrganizationAdminAccount (RegisterOrganizationAdminAccount'),
    newRegisterOrganizationAdminAccount,
    RegisterOrganizationAdminAccountResponse (RegisterOrganizationAdminAccountResponse'),
    newRegisterOrganizationAdminAccountResponse,

    -- ** StartAssessmentFrameworkShare
    StartAssessmentFrameworkShare (StartAssessmentFrameworkShare'),
    newStartAssessmentFrameworkShare,
    StartAssessmentFrameworkShareResponse (StartAssessmentFrameworkShareResponse'),
    newStartAssessmentFrameworkShareResponse,

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

    -- ** UpdateAssessment
    UpdateAssessment (UpdateAssessment'),
    newUpdateAssessment,
    UpdateAssessmentResponse (UpdateAssessmentResponse'),
    newUpdateAssessmentResponse,

    -- ** UpdateAssessmentControl
    UpdateAssessmentControl (UpdateAssessmentControl'),
    newUpdateAssessmentControl,
    UpdateAssessmentControlResponse (UpdateAssessmentControlResponse'),
    newUpdateAssessmentControlResponse,

    -- ** UpdateAssessmentControlSetStatus
    UpdateAssessmentControlSetStatus (UpdateAssessmentControlSetStatus'),
    newUpdateAssessmentControlSetStatus,
    UpdateAssessmentControlSetStatusResponse (UpdateAssessmentControlSetStatusResponse'),
    newUpdateAssessmentControlSetStatusResponse,

    -- ** UpdateAssessmentFramework
    UpdateAssessmentFramework (UpdateAssessmentFramework'),
    newUpdateAssessmentFramework,
    UpdateAssessmentFrameworkResponse (UpdateAssessmentFrameworkResponse'),
    newUpdateAssessmentFrameworkResponse,

    -- ** UpdateAssessmentFrameworkShare
    UpdateAssessmentFrameworkShare (UpdateAssessmentFrameworkShare'),
    newUpdateAssessmentFrameworkShare,
    UpdateAssessmentFrameworkShareResponse (UpdateAssessmentFrameworkShareResponse'),
    newUpdateAssessmentFrameworkShareResponse,

    -- ** UpdateAssessmentStatus
    UpdateAssessmentStatus (UpdateAssessmentStatus'),
    newUpdateAssessmentStatus,
    UpdateAssessmentStatusResponse (UpdateAssessmentStatusResponse'),
    newUpdateAssessmentStatusResponse,

    -- ** UpdateControl
    UpdateControl (UpdateControl'),
    newUpdateControl,
    UpdateControlResponse (UpdateControlResponse'),
    newUpdateControlResponse,

    -- ** UpdateSettings
    UpdateSettings (UpdateSettings'),
    newUpdateSettings,
    UpdateSettingsResponse (UpdateSettingsResponse'),
    newUpdateSettingsResponse,

    -- ** ValidateAssessmentReportIntegrity
    ValidateAssessmentReportIntegrity (ValidateAssessmentReportIntegrity'),
    newValidateAssessmentReportIntegrity,
    ValidateAssessmentReportIntegrityResponse (ValidateAssessmentReportIntegrityResponse'),
    newValidateAssessmentReportIntegrityResponse,

    -- * Types

    -- ** AccountStatus
    AccountStatus (..),

    -- ** ActionEnum
    ActionEnum (..),

    -- ** AssessmentReportDestinationType
    AssessmentReportDestinationType (..),

    -- ** AssessmentReportStatus
    AssessmentReportStatus (..),

    -- ** AssessmentStatus
    AssessmentStatus (..),

    -- ** ControlResponse
    ControlResponse (..),

    -- ** ControlSetStatus
    ControlSetStatus (..),

    -- ** ControlStatus
    ControlStatus (..),

    -- ** ControlType
    ControlType (..),

    -- ** DelegationStatus
    DelegationStatus (..),

    -- ** EvidenceFinderBackfillStatus
    EvidenceFinderBackfillStatus (..),

    -- ** EvidenceFinderEnablementStatus
    EvidenceFinderEnablementStatus (..),

    -- ** FrameworkType
    FrameworkType (..),

    -- ** KeywordInputType
    KeywordInputType (..),

    -- ** ObjectTypeEnum
    ObjectTypeEnum (..),

    -- ** RoleType
    RoleType (..),

    -- ** SettingAttribute
    SettingAttribute (..),

    -- ** ShareRequestAction
    ShareRequestAction (..),

    -- ** ShareRequestStatus
    ShareRequestStatus (..),

    -- ** ShareRequestType
    ShareRequestType (..),

    -- ** SourceFrequency
    SourceFrequency (..),

    -- ** SourceSetUpOption
    SourceSetUpOption (..),

    -- ** SourceType
    SourceType (..),

    -- ** AWSAccount
    AWSAccount (AWSAccount'),
    newAWSAccount,

    -- ** AWSService
    AWSService (AWSService'),
    newAWSService,

    -- ** Assessment
    Assessment (Assessment'),
    newAssessment,

    -- ** AssessmentControl
    AssessmentControl (AssessmentControl'),
    newAssessmentControl,

    -- ** AssessmentControlSet
    AssessmentControlSet (AssessmentControlSet'),
    newAssessmentControlSet,

    -- ** AssessmentEvidenceFolder
    AssessmentEvidenceFolder (AssessmentEvidenceFolder'),
    newAssessmentEvidenceFolder,

    -- ** AssessmentFramework
    AssessmentFramework (AssessmentFramework'),
    newAssessmentFramework,

    -- ** AssessmentFrameworkMetadata
    AssessmentFrameworkMetadata (AssessmentFrameworkMetadata'),
    newAssessmentFrameworkMetadata,

    -- ** AssessmentFrameworkShareRequest
    AssessmentFrameworkShareRequest (AssessmentFrameworkShareRequest'),
    newAssessmentFrameworkShareRequest,

    -- ** AssessmentMetadata
    AssessmentMetadata (AssessmentMetadata'),
    newAssessmentMetadata,

    -- ** AssessmentMetadataItem
    AssessmentMetadataItem (AssessmentMetadataItem'),
    newAssessmentMetadataItem,

    -- ** AssessmentReport
    AssessmentReport (AssessmentReport'),
    newAssessmentReport,

    -- ** AssessmentReportEvidenceError
    AssessmentReportEvidenceError (AssessmentReportEvidenceError'),
    newAssessmentReportEvidenceError,

    -- ** AssessmentReportMetadata
    AssessmentReportMetadata (AssessmentReportMetadata'),
    newAssessmentReportMetadata,

    -- ** AssessmentReportsDestination
    AssessmentReportsDestination (AssessmentReportsDestination'),
    newAssessmentReportsDestination,

    -- ** BatchCreateDelegationByAssessmentError
    BatchCreateDelegationByAssessmentError (BatchCreateDelegationByAssessmentError'),
    newBatchCreateDelegationByAssessmentError,

    -- ** BatchDeleteDelegationByAssessmentError
    BatchDeleteDelegationByAssessmentError (BatchDeleteDelegationByAssessmentError'),
    newBatchDeleteDelegationByAssessmentError,

    -- ** BatchImportEvidenceToAssessmentControlError
    BatchImportEvidenceToAssessmentControlError (BatchImportEvidenceToAssessmentControlError'),
    newBatchImportEvidenceToAssessmentControlError,

    -- ** ChangeLog
    ChangeLog (ChangeLog'),
    newChangeLog,

    -- ** Control
    Control (Control'),
    newControl,

    -- ** ControlComment
    ControlComment (ControlComment'),
    newControlComment,

    -- ** ControlDomainInsights
    ControlDomainInsights (ControlDomainInsights'),
    newControlDomainInsights,

    -- ** ControlInsightsMetadataByAssessmentItem
    ControlInsightsMetadataByAssessmentItem (ControlInsightsMetadataByAssessmentItem'),
    newControlInsightsMetadataByAssessmentItem,

    -- ** ControlInsightsMetadataItem
    ControlInsightsMetadataItem (ControlInsightsMetadataItem'),
    newControlInsightsMetadataItem,

    -- ** ControlMappingSource
    ControlMappingSource (ControlMappingSource'),
    newControlMappingSource,

    -- ** ControlMetadata
    ControlMetadata (ControlMetadata'),
    newControlMetadata,

    -- ** ControlSet
    ControlSet (ControlSet'),
    newControlSet,

    -- ** CreateAssessmentFrameworkControl
    CreateAssessmentFrameworkControl (CreateAssessmentFrameworkControl'),
    newCreateAssessmentFrameworkControl,

    -- ** CreateAssessmentFrameworkControlSet
    CreateAssessmentFrameworkControlSet (CreateAssessmentFrameworkControlSet'),
    newCreateAssessmentFrameworkControlSet,

    -- ** CreateControlMappingSource
    CreateControlMappingSource (CreateControlMappingSource'),
    newCreateControlMappingSource,

    -- ** CreateDelegationRequest
    CreateDelegationRequest (CreateDelegationRequest'),
    newCreateDelegationRequest,

    -- ** Delegation
    Delegation (Delegation'),
    newDelegation,

    -- ** DelegationMetadata
    DelegationMetadata (DelegationMetadata'),
    newDelegationMetadata,

    -- ** Evidence
    Evidence (Evidence'),
    newEvidence,

    -- ** EvidenceFinderEnablement
    EvidenceFinderEnablement (EvidenceFinderEnablement'),
    newEvidenceFinderEnablement,

    -- ** EvidenceInsights
    EvidenceInsights (EvidenceInsights'),
    newEvidenceInsights,

    -- ** Framework
    Framework (Framework'),
    newFramework,

    -- ** FrameworkMetadata
    FrameworkMetadata (FrameworkMetadata'),
    newFrameworkMetadata,

    -- ** Insights
    Insights (Insights'),
    newInsights,

    -- ** InsightsByAssessment
    InsightsByAssessment (InsightsByAssessment'),
    newInsightsByAssessment,

    -- ** ManualEvidence
    ManualEvidence (ManualEvidence'),
    newManualEvidence,

    -- ** Notification
    Notification (Notification'),
    newNotification,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** Role
    Role (Role'),
    newRole,

    -- ** Scope
    Scope (Scope'),
    newScope,

    -- ** ServiceMetadata
    ServiceMetadata (ServiceMetadata'),
    newServiceMetadata,

    -- ** Settings
    Settings (Settings'),
    newSettings,

    -- ** SourceKeyword
    SourceKeyword (SourceKeyword'),
    newSourceKeyword,

    -- ** URL
    URL (URL'),
    newURL,

    -- ** UpdateAssessmentFrameworkControlSet
    UpdateAssessmentFrameworkControlSet (UpdateAssessmentFrameworkControlSet'),
    newUpdateAssessmentFrameworkControlSet,
  )
where

import Amazonka.AuditManager.AssociateAssessmentReportEvidenceFolder
import Amazonka.AuditManager.BatchAssociateAssessmentReportEvidence
import Amazonka.AuditManager.BatchCreateDelegationByAssessment
import Amazonka.AuditManager.BatchDeleteDelegationByAssessment
import Amazonka.AuditManager.BatchDisassociateAssessmentReportEvidence
import Amazonka.AuditManager.BatchImportEvidenceToAssessmentControl
import Amazonka.AuditManager.CreateAssessment
import Amazonka.AuditManager.CreateAssessmentFramework
import Amazonka.AuditManager.CreateAssessmentReport
import Amazonka.AuditManager.CreateControl
import Amazonka.AuditManager.DeleteAssessment
import Amazonka.AuditManager.DeleteAssessmentFramework
import Amazonka.AuditManager.DeleteAssessmentFrameworkShare
import Amazonka.AuditManager.DeleteAssessmentReport
import Amazonka.AuditManager.DeleteControl
import Amazonka.AuditManager.DeregisterAccount
import Amazonka.AuditManager.DeregisterOrganizationAdminAccount
import Amazonka.AuditManager.DisassociateAssessmentReportEvidenceFolder
import Amazonka.AuditManager.GetAccountStatus
import Amazonka.AuditManager.GetAssessment
import Amazonka.AuditManager.GetAssessmentFramework
import Amazonka.AuditManager.GetAssessmentReportUrl
import Amazonka.AuditManager.GetChangeLogs
import Amazonka.AuditManager.GetControl
import Amazonka.AuditManager.GetDelegations
import Amazonka.AuditManager.GetEvidence
import Amazonka.AuditManager.GetEvidenceByEvidenceFolder
import Amazonka.AuditManager.GetEvidenceFolder
import Amazonka.AuditManager.GetEvidenceFoldersByAssessment
import Amazonka.AuditManager.GetEvidenceFoldersByAssessmentControl
import Amazonka.AuditManager.GetInsights
import Amazonka.AuditManager.GetInsightsByAssessment
import Amazonka.AuditManager.GetOrganizationAdminAccount
import Amazonka.AuditManager.GetServicesInScope
import Amazonka.AuditManager.GetSettings
import Amazonka.AuditManager.Lens
import Amazonka.AuditManager.ListAssessmentControlInsightsByControlDomain
import Amazonka.AuditManager.ListAssessmentFrameworkShareRequests
import Amazonka.AuditManager.ListAssessmentFrameworks
import Amazonka.AuditManager.ListAssessmentReports
import Amazonka.AuditManager.ListAssessments
import Amazonka.AuditManager.ListControlDomainInsights
import Amazonka.AuditManager.ListControlDomainInsightsByAssessment
import Amazonka.AuditManager.ListControlInsightsByControlDomain
import Amazonka.AuditManager.ListControls
import Amazonka.AuditManager.ListKeywordsForDataSource
import Amazonka.AuditManager.ListNotifications
import Amazonka.AuditManager.ListTagsForResource
import Amazonka.AuditManager.RegisterAccount
import Amazonka.AuditManager.RegisterOrganizationAdminAccount
import Amazonka.AuditManager.StartAssessmentFrameworkShare
import Amazonka.AuditManager.TagResource
import Amazonka.AuditManager.Types
import Amazonka.AuditManager.UntagResource
import Amazonka.AuditManager.UpdateAssessment
import Amazonka.AuditManager.UpdateAssessmentControl
import Amazonka.AuditManager.UpdateAssessmentControlSetStatus
import Amazonka.AuditManager.UpdateAssessmentFramework
import Amazonka.AuditManager.UpdateAssessmentFrameworkShare
import Amazonka.AuditManager.UpdateAssessmentStatus
import Amazonka.AuditManager.UpdateControl
import Amazonka.AuditManager.UpdateSettings
import Amazonka.AuditManager.ValidateAssessmentReportIntegrity
import Amazonka.AuditManager.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'AuditManager'.

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
