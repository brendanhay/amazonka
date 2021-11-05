{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.WellArchitected
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-03-31@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Well-Architected Tool
--
-- This is the /AWS Well-Architected Tool API Reference/. The AWS
-- Well-Architected Tool API provides programmatic access to the
-- <http://aws.amazon.com/well-architected-tool AWS Well-Architected Tool>
-- in the
-- <https://console.aws.amazon.com/wellarchitected AWS Management Console>.
-- For information about the AWS Well-Architected Tool, see the
-- <https://docs.aws.amazon.com/wellarchitected/latest/userguide/intro.html AWS Well-Architected Tool User Guide>.
module Network.AWS.WellArchitected
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** AccessDeniedException
    _AccessDeniedException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListNotifications
    ListNotifications (ListNotifications'),
    newListNotifications,
    ListNotificationsResponse (ListNotificationsResponse'),
    newListNotificationsResponse,

    -- ** GetLensVersionDifference
    GetLensVersionDifference (GetLensVersionDifference'),
    newGetLensVersionDifference,
    GetLensVersionDifferenceResponse (GetLensVersionDifferenceResponse'),
    newGetLensVersionDifferenceResponse,

    -- ** ListLensReviewImprovements
    ListLensReviewImprovements (ListLensReviewImprovements'),
    newListLensReviewImprovements,
    ListLensReviewImprovementsResponse (ListLensReviewImprovementsResponse'),
    newListLensReviewImprovementsResponse,

    -- ** ListMilestones
    ListMilestones (ListMilestones'),
    newListMilestones,
    ListMilestonesResponse (ListMilestonesResponse'),
    newListMilestonesResponse,

    -- ** CreateMilestone
    CreateMilestone (CreateMilestone'),
    newCreateMilestone,
    CreateMilestoneResponse (CreateMilestoneResponse'),
    newCreateMilestoneResponse,

    -- ** GetAnswer
    GetAnswer (GetAnswer'),
    newGetAnswer,
    GetAnswerResponse (GetAnswerResponse'),
    newGetAnswerResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** UpdateAnswer
    UpdateAnswer (UpdateAnswer'),
    newUpdateAnswer,
    UpdateAnswerResponse (UpdateAnswerResponse'),
    newUpdateAnswerResponse,

    -- ** UpdateShareInvitation
    UpdateShareInvitation (UpdateShareInvitation'),
    newUpdateShareInvitation,
    UpdateShareInvitationResponse (UpdateShareInvitationResponse'),
    newUpdateShareInvitationResponse,

    -- ** ListAnswers
    ListAnswers (ListAnswers'),
    newListAnswers,
    ListAnswersResponse (ListAnswersResponse'),
    newListAnswersResponse,

    -- ** DisassociateLenses
    DisassociateLenses (DisassociateLenses'),
    newDisassociateLenses,
    DisassociateLensesResponse (DisassociateLensesResponse'),
    newDisassociateLensesResponse,

    -- ** GetMilestone
    GetMilestone (GetMilestone'),
    newGetMilestone,
    GetMilestoneResponse (GetMilestoneResponse'),
    newGetMilestoneResponse,

    -- ** ListLenses
    ListLenses (ListLenses'),
    newListLenses,
    ListLensesResponse (ListLensesResponse'),
    newListLensesResponse,

    -- ** ListWorkloadShares
    ListWorkloadShares (ListWorkloadShares'),
    newListWorkloadShares,
    ListWorkloadSharesResponse (ListWorkloadSharesResponse'),
    newListWorkloadSharesResponse,

    -- ** UpdateWorkload
    UpdateWorkload (UpdateWorkload'),
    newUpdateWorkload,
    UpdateWorkloadResponse (UpdateWorkloadResponse'),
    newUpdateWorkloadResponse,

    -- ** DeleteWorkload
    DeleteWorkload (DeleteWorkload'),
    newDeleteWorkload,
    DeleteWorkloadResponse (DeleteWorkloadResponse'),
    newDeleteWorkloadResponse,

    -- ** ListLensReviews
    ListLensReviews (ListLensReviews'),
    newListLensReviews,
    ListLensReviewsResponse (ListLensReviewsResponse'),
    newListLensReviewsResponse,

    -- ** UpdateLensReview
    UpdateLensReview (UpdateLensReview'),
    newUpdateLensReview,
    UpdateLensReviewResponse (UpdateLensReviewResponse'),
    newUpdateLensReviewResponse,

    -- ** ListShareInvitations
    ListShareInvitations (ListShareInvitations'),
    newListShareInvitations,
    ListShareInvitationsResponse (ListShareInvitationsResponse'),
    newListShareInvitationsResponse,

    -- ** GetLensReview
    GetLensReview (GetLensReview'),
    newGetLensReview,
    GetLensReviewResponse (GetLensReviewResponse'),
    newGetLensReviewResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** CreateWorkload
    CreateWorkload (CreateWorkload'),
    newCreateWorkload,
    CreateWorkloadResponse (CreateWorkloadResponse'),
    newCreateWorkloadResponse,

    -- ** DeleteWorkloadShare
    DeleteWorkloadShare (DeleteWorkloadShare'),
    newDeleteWorkloadShare,
    DeleteWorkloadShareResponse (DeleteWorkloadShareResponse'),
    newDeleteWorkloadShareResponse,

    -- ** UpdateWorkloadShare
    UpdateWorkloadShare (UpdateWorkloadShare'),
    newUpdateWorkloadShare,
    UpdateWorkloadShareResponse (UpdateWorkloadShareResponse'),
    newUpdateWorkloadShareResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** AssociateLenses
    AssociateLenses (AssociateLenses'),
    newAssociateLenses,
    AssociateLensesResponse (AssociateLensesResponse'),
    newAssociateLensesResponse,

    -- ** ListWorkloads
    ListWorkloads (ListWorkloads'),
    newListWorkloads,
    ListWorkloadsResponse (ListWorkloadsResponse'),
    newListWorkloadsResponse,

    -- ** CreateWorkloadShare
    CreateWorkloadShare (CreateWorkloadShare'),
    newCreateWorkloadShare,
    CreateWorkloadShareResponse (CreateWorkloadShareResponse'),
    newCreateWorkloadShareResponse,

    -- ** GetLensReviewReport
    GetLensReviewReport (GetLensReviewReport'),
    newGetLensReviewReport,
    GetLensReviewReportResponse (GetLensReviewReportResponse'),
    newGetLensReviewReportResponse,

    -- ** UpgradeLensReview
    UpgradeLensReview (UpgradeLensReview'),
    newUpgradeLensReview,
    UpgradeLensReviewResponse (UpgradeLensReviewResponse'),
    newUpgradeLensReviewResponse,

    -- ** GetWorkload
    GetWorkload (GetWorkload'),
    newGetWorkload,
    GetWorkloadResponse (GetWorkloadResponse'),
    newGetWorkloadResponse,

    -- * Types

    -- ** AnswerReason
    AnswerReason (..),

    -- ** ChoiceReason
    ChoiceReason (..),

    -- ** ChoiceStatus
    ChoiceStatus (..),

    -- ** DifferenceStatus
    DifferenceStatus (..),

    -- ** LensStatus
    LensStatus (..),

    -- ** NotificationType
    NotificationType (..),

    -- ** PermissionType
    PermissionType (..),

    -- ** Risk
    Risk (..),

    -- ** ShareInvitationAction
    ShareInvitationAction (..),

    -- ** ShareStatus
    ShareStatus (..),

    -- ** WorkloadEnvironment
    WorkloadEnvironment (..),

    -- ** WorkloadImprovementStatus
    WorkloadImprovementStatus (..),

    -- ** Answer
    Answer (Answer'),
    newAnswer,

    -- ** AnswerSummary
    AnswerSummary (AnswerSummary'),
    newAnswerSummary,

    -- ** Choice
    Choice (Choice'),
    newChoice,

    -- ** ChoiceAnswer
    ChoiceAnswer (ChoiceAnswer'),
    newChoiceAnswer,

    -- ** ChoiceAnswerSummary
    ChoiceAnswerSummary (ChoiceAnswerSummary'),
    newChoiceAnswerSummary,

    -- ** ChoiceUpdate
    ChoiceUpdate (ChoiceUpdate'),
    newChoiceUpdate,

    -- ** ImprovementSummary
    ImprovementSummary (ImprovementSummary'),
    newImprovementSummary,

    -- ** LensReview
    LensReview (LensReview'),
    newLensReview,

    -- ** LensReviewReport
    LensReviewReport (LensReviewReport'),
    newLensReviewReport,

    -- ** LensReviewSummary
    LensReviewSummary (LensReviewSummary'),
    newLensReviewSummary,

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

import Network.AWS.WellArchitected.AssociateLenses
import Network.AWS.WellArchitected.CreateMilestone
import Network.AWS.WellArchitected.CreateWorkload
import Network.AWS.WellArchitected.CreateWorkloadShare
import Network.AWS.WellArchitected.DeleteWorkload
import Network.AWS.WellArchitected.DeleteWorkloadShare
import Network.AWS.WellArchitected.DisassociateLenses
import Network.AWS.WellArchitected.GetAnswer
import Network.AWS.WellArchitected.GetLensReview
import Network.AWS.WellArchitected.GetLensReviewReport
import Network.AWS.WellArchitected.GetLensVersionDifference
import Network.AWS.WellArchitected.GetMilestone
import Network.AWS.WellArchitected.GetWorkload
import Network.AWS.WellArchitected.Lens
import Network.AWS.WellArchitected.ListAnswers
import Network.AWS.WellArchitected.ListLensReviewImprovements
import Network.AWS.WellArchitected.ListLensReviews
import Network.AWS.WellArchitected.ListLenses
import Network.AWS.WellArchitected.ListMilestones
import Network.AWS.WellArchitected.ListNotifications
import Network.AWS.WellArchitected.ListShareInvitations
import Network.AWS.WellArchitected.ListTagsForResource
import Network.AWS.WellArchitected.ListWorkloadShares
import Network.AWS.WellArchitected.ListWorkloads
import Network.AWS.WellArchitected.TagResource
import Network.AWS.WellArchitected.Types
import Network.AWS.WellArchitected.UntagResource
import Network.AWS.WellArchitected.UpdateAnswer
import Network.AWS.WellArchitected.UpdateLensReview
import Network.AWS.WellArchitected.UpdateShareInvitation
import Network.AWS.WellArchitected.UpdateWorkload
import Network.AWS.WellArchitected.UpdateWorkloadShare
import Network.AWS.WellArchitected.UpgradeLensReview
import Network.AWS.WellArchitected.Waiters

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
