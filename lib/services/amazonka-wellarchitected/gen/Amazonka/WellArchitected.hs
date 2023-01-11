{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.WellArchitected
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Well-Architected Tool
--
-- This is the /Well-Architected Tool API Reference/. The WA Tool API
-- provides programmatic access to the
-- <http://aws.amazon.com/well-architected-tool Well-Architected Tool> in
-- the
-- <https://console.aws.amazon.com/wellarchitected Amazon Web Services Management Console>.
-- For information about the Well-Architected Tool, see the
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/intro.html Well-Architected Tool User Guide>.
module Amazonka.WellArchitected
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateLenses
    AssociateLenses (AssociateLenses'),
    newAssociateLenses,
    AssociateLensesResponse (AssociateLensesResponse'),
    newAssociateLensesResponse,

    -- ** CreateLensShare
    CreateLensShare (CreateLensShare'),
    newCreateLensShare,
    CreateLensShareResponse (CreateLensShareResponse'),
    newCreateLensShareResponse,

    -- ** CreateLensVersion
    CreateLensVersion (CreateLensVersion'),
    newCreateLensVersion,
    CreateLensVersionResponse (CreateLensVersionResponse'),
    newCreateLensVersionResponse,

    -- ** CreateMilestone
    CreateMilestone (CreateMilestone'),
    newCreateMilestone,
    CreateMilestoneResponse (CreateMilestoneResponse'),
    newCreateMilestoneResponse,

    -- ** CreateWorkload
    CreateWorkload (CreateWorkload'),
    newCreateWorkload,
    CreateWorkloadResponse (CreateWorkloadResponse'),
    newCreateWorkloadResponse,

    -- ** CreateWorkloadShare
    CreateWorkloadShare (CreateWorkloadShare'),
    newCreateWorkloadShare,
    CreateWorkloadShareResponse (CreateWorkloadShareResponse'),
    newCreateWorkloadShareResponse,

    -- ** DeleteLens
    DeleteLens (DeleteLens'),
    newDeleteLens,
    DeleteLensResponse (DeleteLensResponse'),
    newDeleteLensResponse,

    -- ** DeleteLensShare
    DeleteLensShare (DeleteLensShare'),
    newDeleteLensShare,
    DeleteLensShareResponse (DeleteLensShareResponse'),
    newDeleteLensShareResponse,

    -- ** DeleteWorkload
    DeleteWorkload (DeleteWorkload'),
    newDeleteWorkload,
    DeleteWorkloadResponse (DeleteWorkloadResponse'),
    newDeleteWorkloadResponse,

    -- ** DeleteWorkloadShare
    DeleteWorkloadShare (DeleteWorkloadShare'),
    newDeleteWorkloadShare,
    DeleteWorkloadShareResponse (DeleteWorkloadShareResponse'),
    newDeleteWorkloadShareResponse,

    -- ** DisassociateLenses
    DisassociateLenses (DisassociateLenses'),
    newDisassociateLenses,
    DisassociateLensesResponse (DisassociateLensesResponse'),
    newDisassociateLensesResponse,

    -- ** ExportLens
    ExportLens (ExportLens'),
    newExportLens,
    ExportLensResponse (ExportLensResponse'),
    newExportLensResponse,

    -- ** GetAnswer
    GetAnswer (GetAnswer'),
    newGetAnswer,
    GetAnswerResponse (GetAnswerResponse'),
    newGetAnswerResponse,

    -- ** GetLens
    GetLens (GetLens'),
    newGetLens,
    GetLensResponse (GetLensResponse'),
    newGetLensResponse,

    -- ** GetLensReview
    GetLensReview (GetLensReview'),
    newGetLensReview,
    GetLensReviewResponse (GetLensReviewResponse'),
    newGetLensReviewResponse,

    -- ** GetLensReviewReport
    GetLensReviewReport (GetLensReviewReport'),
    newGetLensReviewReport,
    GetLensReviewReportResponse (GetLensReviewReportResponse'),
    newGetLensReviewReportResponse,

    -- ** GetLensVersionDifference
    GetLensVersionDifference (GetLensVersionDifference'),
    newGetLensVersionDifference,
    GetLensVersionDifferenceResponse (GetLensVersionDifferenceResponse'),
    newGetLensVersionDifferenceResponse,

    -- ** GetMilestone
    GetMilestone (GetMilestone'),
    newGetMilestone,
    GetMilestoneResponse (GetMilestoneResponse'),
    newGetMilestoneResponse,

    -- ** GetWorkload
    GetWorkload (GetWorkload'),
    newGetWorkload,
    GetWorkloadResponse (GetWorkloadResponse'),
    newGetWorkloadResponse,

    -- ** ImportLens
    ImportLens (ImportLens'),
    newImportLens,
    ImportLensResponse (ImportLensResponse'),
    newImportLensResponse,

    -- ** ListAnswers
    ListAnswers (ListAnswers'),
    newListAnswers,
    ListAnswersResponse (ListAnswersResponse'),
    newListAnswersResponse,

    -- ** ListCheckDetails
    ListCheckDetails (ListCheckDetails'),
    newListCheckDetails,
    ListCheckDetailsResponse (ListCheckDetailsResponse'),
    newListCheckDetailsResponse,

    -- ** ListCheckSummaries
    ListCheckSummaries (ListCheckSummaries'),
    newListCheckSummaries,
    ListCheckSummariesResponse (ListCheckSummariesResponse'),
    newListCheckSummariesResponse,

    -- ** ListLensReviewImprovements
    ListLensReviewImprovements (ListLensReviewImprovements'),
    newListLensReviewImprovements,
    ListLensReviewImprovementsResponse (ListLensReviewImprovementsResponse'),
    newListLensReviewImprovementsResponse,

    -- ** ListLensReviews
    ListLensReviews (ListLensReviews'),
    newListLensReviews,
    ListLensReviewsResponse (ListLensReviewsResponse'),
    newListLensReviewsResponse,

    -- ** ListLensShares
    ListLensShares (ListLensShares'),
    newListLensShares,
    ListLensSharesResponse (ListLensSharesResponse'),
    newListLensSharesResponse,

    -- ** ListLenses
    ListLenses (ListLenses'),
    newListLenses,
    ListLensesResponse (ListLensesResponse'),
    newListLensesResponse,

    -- ** ListMilestones
    ListMilestones (ListMilestones'),
    newListMilestones,
    ListMilestonesResponse (ListMilestonesResponse'),
    newListMilestonesResponse,

    -- ** ListNotifications
    ListNotifications (ListNotifications'),
    newListNotifications,
    ListNotificationsResponse (ListNotificationsResponse'),
    newListNotificationsResponse,

    -- ** ListShareInvitations
    ListShareInvitations (ListShareInvitations'),
    newListShareInvitations,
    ListShareInvitationsResponse (ListShareInvitationsResponse'),
    newListShareInvitationsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** ListWorkloadShares
    ListWorkloadShares (ListWorkloadShares'),
    newListWorkloadShares,
    ListWorkloadSharesResponse (ListWorkloadSharesResponse'),
    newListWorkloadSharesResponse,

    -- ** ListWorkloads
    ListWorkloads (ListWorkloads'),
    newListWorkloads,
    ListWorkloadsResponse (ListWorkloadsResponse'),
    newListWorkloadsResponse,

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

    -- ** UpdateAnswer
    UpdateAnswer (UpdateAnswer'),
    newUpdateAnswer,
    UpdateAnswerResponse (UpdateAnswerResponse'),
    newUpdateAnswerResponse,

    -- ** UpdateGlobalSettings
    UpdateGlobalSettings (UpdateGlobalSettings'),
    newUpdateGlobalSettings,
    UpdateGlobalSettingsResponse (UpdateGlobalSettingsResponse'),
    newUpdateGlobalSettingsResponse,

    -- ** UpdateLensReview
    UpdateLensReview (UpdateLensReview'),
    newUpdateLensReview,
    UpdateLensReviewResponse (UpdateLensReviewResponse'),
    newUpdateLensReviewResponse,

    -- ** UpdateShareInvitation
    UpdateShareInvitation (UpdateShareInvitation'),
    newUpdateShareInvitation,
    UpdateShareInvitationResponse (UpdateShareInvitationResponse'),
    newUpdateShareInvitationResponse,

    -- ** UpdateWorkload
    UpdateWorkload (UpdateWorkload'),
    newUpdateWorkload,
    UpdateWorkloadResponse (UpdateWorkloadResponse'),
    newUpdateWorkloadResponse,

    -- ** UpdateWorkloadShare
    UpdateWorkloadShare (UpdateWorkloadShare'),
    newUpdateWorkloadShare,
    UpdateWorkloadShareResponse (UpdateWorkloadShareResponse'),
    newUpdateWorkloadShareResponse,

    -- ** UpgradeLensReview
    UpgradeLensReview (UpgradeLensReview'),
    newUpgradeLensReview,
    UpgradeLensReviewResponse (UpgradeLensReviewResponse'),
    newUpgradeLensReviewResponse,

    -- * Types

    -- ** AdditionalResourceType
    AdditionalResourceType (..),

    -- ** AnswerReason
    AnswerReason (..),

    -- ** CheckFailureReason
    CheckFailureReason (..),

    -- ** CheckProvider
    CheckProvider (..),

    -- ** CheckStatus
    CheckStatus (..),

    -- ** ChoiceReason
    ChoiceReason (..),

    -- ** ChoiceStatus
    ChoiceStatus (..),

    -- ** DifferenceStatus
    DifferenceStatus (..),

    -- ** ImportLensStatus
    ImportLensStatus (..),

    -- ** LensStatus
    LensStatus (..),

    -- ** LensStatusType
    LensStatusType (..),

    -- ** LensType
    LensType (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** OrganizationSharingStatus
    OrganizationSharingStatus (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** Risk
    Risk (..),

    -- ** ShareInvitationAction
    ShareInvitationAction (..),

    -- ** ShareResourceType
    ShareResourceType (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** TrustedAdvisorIntegrationStatus
    TrustedAdvisorIntegrationStatus (..),

    -- ** WorkloadEnvironment
    WorkloadEnvironment (..),

    -- ** WorkloadImprovementStatus
    WorkloadImprovementStatus (..),

    -- ** AdditionalResources
    AdditionalResources (AdditionalResources'),
    newAdditionalResources,

    -- ** Answer
    Answer (Answer'),
    newAnswer,

    -- ** AnswerSummary
    AnswerSummary (AnswerSummary'),
    newAnswerSummary,

    -- ** CheckDetail
    CheckDetail (CheckDetail'),
    newCheckDetail,

    -- ** CheckSummary
    CheckSummary (CheckSummary'),
    newCheckSummary,

    -- ** Choice
    Choice (Choice'),
    newChoice,

    -- ** ChoiceAnswer
    ChoiceAnswer (ChoiceAnswer'),
    newChoiceAnswer,

    -- ** ChoiceAnswerSummary
    ChoiceAnswerSummary (ChoiceAnswerSummary'),
    newChoiceAnswerSummary,

    -- ** ChoiceContent
    ChoiceContent (ChoiceContent'),
    newChoiceContent,

    -- ** ChoiceImprovementPlan
    ChoiceImprovementPlan (ChoiceImprovementPlan'),
    newChoiceImprovementPlan,

    -- ** ChoiceUpdate
    ChoiceUpdate (ChoiceUpdate'),
    newChoiceUpdate,

    -- ** ImprovementSummary
    ImprovementSummary (ImprovementSummary'),
    newImprovementSummary,

    -- ** Lens
    Lens (Lens'),
    newLens,

    -- ** LensReview
    LensReview (LensReview'),
    newLensReview,

    -- ** LensReviewReport
    LensReviewReport (LensReviewReport'),
    newLensReviewReport,

    -- ** LensReviewSummary
    LensReviewSummary (LensReviewSummary'),
    newLensReviewSummary,

    -- ** LensShareSummary
    LensShareSummary (LensShareSummary'),
    newLensShareSummary,

    -- ** LensSummary
    LensSummary (LensSummary'),
    newLensSummary,

    -- ** LensUpgradeSummary
    LensUpgradeSummary (LensUpgradeSummary'),
    newLensUpgradeSummary,

    -- ** Milestone
    Milestone (Milestone'),
    newMilestone,

    -- ** MilestoneSummary
    MilestoneSummary (MilestoneSummary'),
    newMilestoneSummary,

    -- ** NotificationSummary
    NotificationSummary (NotificationSummary'),
    newNotificationSummary,

    -- ** PillarDifference
    PillarDifference (PillarDifference'),
    newPillarDifference,

    -- ** PillarReviewSummary
    PillarReviewSummary (PillarReviewSummary'),
    newPillarReviewSummary,

    -- ** QuestionDifference
    QuestionDifference (QuestionDifference'),
    newQuestionDifference,

    -- ** ShareInvitation
    ShareInvitation (ShareInvitation'),
    newShareInvitation,

    -- ** ShareInvitationSummary
    ShareInvitationSummary (ShareInvitationSummary'),
    newShareInvitationSummary,

    -- ** VersionDifferences
    VersionDifferences (VersionDifferences'),
    newVersionDifferences,

    -- ** Workload
    Workload (Workload'),
    newWorkload,

    -- ** WorkloadDiscoveryConfig
    WorkloadDiscoveryConfig (WorkloadDiscoveryConfig'),
    newWorkloadDiscoveryConfig,

    -- ** WorkloadShare
    WorkloadShare (WorkloadShare'),
    newWorkloadShare,

    -- ** WorkloadShareSummary
    WorkloadShareSummary (WorkloadShareSummary'),
    newWorkloadShareSummary,

    -- ** WorkloadSummary
    WorkloadSummary (WorkloadSummary'),
    newWorkloadSummary,
  )
where

import Amazonka.WellArchitected.AssociateLenses
import Amazonka.WellArchitected.CreateLensShare
import Amazonka.WellArchitected.CreateLensVersion
import Amazonka.WellArchitected.CreateMilestone
import Amazonka.WellArchitected.CreateWorkload
import Amazonka.WellArchitected.CreateWorkloadShare
import Amazonka.WellArchitected.DeleteLens
import Amazonka.WellArchitected.DeleteLensShare
import Amazonka.WellArchitected.DeleteWorkload
import Amazonka.WellArchitected.DeleteWorkloadShare
import Amazonka.WellArchitected.DisassociateLenses
import Amazonka.WellArchitected.ExportLens
import Amazonka.WellArchitected.GetAnswer
import Amazonka.WellArchitected.GetLens
import Amazonka.WellArchitected.GetLensReview
import Amazonka.WellArchitected.GetLensReviewReport
import Amazonka.WellArchitected.GetLensVersionDifference
import Amazonka.WellArchitected.GetMilestone
import Amazonka.WellArchitected.GetWorkload
import Amazonka.WellArchitected.ImportLens
import Amazonka.WellArchitected.Lens
import Amazonka.WellArchitected.ListAnswers
import Amazonka.WellArchitected.ListCheckDetails
import Amazonka.WellArchitected.ListCheckSummaries
import Amazonka.WellArchitected.ListLensReviewImprovements
import Amazonka.WellArchitected.ListLensReviews
import Amazonka.WellArchitected.ListLensShares
import Amazonka.WellArchitected.ListLenses
import Amazonka.WellArchitected.ListMilestones
import Amazonka.WellArchitected.ListNotifications
import Amazonka.WellArchitected.ListShareInvitations
import Amazonka.WellArchitected.ListTagsForResource
import Amazonka.WellArchitected.ListWorkloadShares
import Amazonka.WellArchitected.ListWorkloads
import Amazonka.WellArchitected.TagResource
import Amazonka.WellArchitected.Types
import Amazonka.WellArchitected.UntagResource
import Amazonka.WellArchitected.UpdateAnswer
import Amazonka.WellArchitected.UpdateGlobalSettings
import Amazonka.WellArchitected.UpdateLensReview
import Amazonka.WellArchitected.UpdateShareInvitation
import Amazonka.WellArchitected.UpdateWorkload
import Amazonka.WellArchitected.UpdateWorkloadShare
import Amazonka.WellArchitected.UpgradeLensReview
import Amazonka.WellArchitected.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'WellArchitected'.

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
