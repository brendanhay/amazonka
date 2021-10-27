{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WellArchitected.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Lens
  ( -- * Operations

    -- ** ListNotifications
    listNotifications_nextToken,
    listNotifications_workloadId,
    listNotifications_maxResults,
    listNotificationsResponse_notificationSummaries,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_httpStatus,

    -- ** GetLensVersionDifference
    getLensVersionDifference_lensAlias,
    getLensVersionDifference_baseLensVersion,
    getLensVersionDifferenceResponse_lensAlias,
    getLensVersionDifferenceResponse_latestLensVersion,
    getLensVersionDifferenceResponse_baseLensVersion,
    getLensVersionDifferenceResponse_versionDifferences,
    getLensVersionDifferenceResponse_httpStatus,

    -- ** ListLensReviewImprovements
    listLensReviewImprovements_pillarId,
    listLensReviewImprovements_milestoneNumber,
    listLensReviewImprovements_nextToken,
    listLensReviewImprovements_maxResults,
    listLensReviewImprovements_workloadId,
    listLensReviewImprovements_lensAlias,
    listLensReviewImprovementsResponse_improvementSummaries,
    listLensReviewImprovementsResponse_lensAlias,
    listLensReviewImprovementsResponse_milestoneNumber,
    listLensReviewImprovementsResponse_nextToken,
    listLensReviewImprovementsResponse_workloadId,
    listLensReviewImprovementsResponse_httpStatus,

    -- ** ListMilestones
    listMilestones_nextToken,
    listMilestones_maxResults,
    listMilestones_workloadId,
    listMilestonesResponse_milestoneSummaries,
    listMilestonesResponse_nextToken,
    listMilestonesResponse_workloadId,
    listMilestonesResponse_httpStatus,

    -- ** CreateMilestone
    createMilestone_workloadId,
    createMilestone_milestoneName,
    createMilestone_clientRequestToken,
    createMilestoneResponse_milestoneNumber,
    createMilestoneResponse_workloadId,
    createMilestoneResponse_httpStatus,

    -- ** GetAnswer
    getAnswer_milestoneNumber,
    getAnswer_workloadId,
    getAnswer_lensAlias,
    getAnswer_questionId,
    getAnswerResponse_lensAlias,
    getAnswerResponse_milestoneNumber,
    getAnswerResponse_answer,
    getAnswerResponse_workloadId,
    getAnswerResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_workloadArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** UpdateAnswer
    updateAnswer_isApplicable,
    updateAnswer_selectedChoices,
    updateAnswer_reason,
    updateAnswer_notes,
    updateAnswer_choiceUpdates,
    updateAnswer_workloadId,
    updateAnswer_lensAlias,
    updateAnswer_questionId,
    updateAnswerResponse_lensAlias,
    updateAnswerResponse_answer,
    updateAnswerResponse_workloadId,
    updateAnswerResponse_httpStatus,

    -- ** UpdateShareInvitation
    updateShareInvitation_shareInvitationId,
    updateShareInvitation_shareInvitationAction,
    updateShareInvitationResponse_shareInvitation,
    updateShareInvitationResponse_httpStatus,

    -- ** ListAnswers
    listAnswers_pillarId,
    listAnswers_milestoneNumber,
    listAnswers_nextToken,
    listAnswers_maxResults,
    listAnswers_workloadId,
    listAnswers_lensAlias,
    listAnswersResponse_lensAlias,
    listAnswersResponse_milestoneNumber,
    listAnswersResponse_nextToken,
    listAnswersResponse_workloadId,
    listAnswersResponse_answerSummaries,
    listAnswersResponse_httpStatus,

    -- ** DisassociateLenses
    disassociateLenses_workloadId,
    disassociateLenses_lensAliases,

    -- ** GetMilestone
    getMilestone_workloadId,
    getMilestone_milestoneNumber,
    getMilestoneResponse_milestone,
    getMilestoneResponse_workloadId,
    getMilestoneResponse_httpStatus,

    -- ** ListLenses
    listLenses_nextToken,
    listLenses_maxResults,
    listLensesResponse_nextToken,
    listLensesResponse_lensSummaries,
    listLensesResponse_httpStatus,

    -- ** ListWorkloadShares
    listWorkloadShares_sharedWithPrefix,
    listWorkloadShares_nextToken,
    listWorkloadShares_maxResults,
    listWorkloadShares_workloadId,
    listWorkloadSharesResponse_workloadShareSummaries,
    listWorkloadSharesResponse_nextToken,
    listWorkloadSharesResponse_workloadId,
    listWorkloadSharesResponse_httpStatus,

    -- ** UpdateWorkload
    updateWorkload_isReviewOwnerUpdateAcknowledged,
    updateWorkload_architecturalDesign,
    updateWorkload_accountIds,
    updateWorkload_industry,
    updateWorkload_environment,
    updateWorkload_awsRegions,
    updateWorkload_improvementStatus,
    updateWorkload_industryType,
    updateWorkload_workloadName,
    updateWorkload_notes,
    updateWorkload_reviewOwner,
    updateWorkload_description,
    updateWorkload_pillarPriorities,
    updateWorkload_nonAwsRegions,
    updateWorkload_workloadId,
    updateWorkloadResponse_workload,
    updateWorkloadResponse_httpStatus,

    -- ** DeleteWorkload
    deleteWorkload_workloadId,
    deleteWorkload_clientRequestToken,

    -- ** ListLensReviews
    listLensReviews_milestoneNumber,
    listLensReviews_nextToken,
    listLensReviews_maxResults,
    listLensReviews_workloadId,
    listLensReviewsResponse_milestoneNumber,
    listLensReviewsResponse_nextToken,
    listLensReviewsResponse_lensReviewSummaries,
    listLensReviewsResponse_workloadId,
    listLensReviewsResponse_httpStatus,

    -- ** UpdateLensReview
    updateLensReview_lensNotes,
    updateLensReview_pillarNotes,
    updateLensReview_workloadId,
    updateLensReview_lensAlias,
    updateLensReviewResponse_workloadId,
    updateLensReviewResponse_lensReview,
    updateLensReviewResponse_httpStatus,

    -- ** ListShareInvitations
    listShareInvitations_nextToken,
    listShareInvitations_workloadNamePrefix,
    listShareInvitations_maxResults,
    listShareInvitationsResponse_shareInvitationSummaries,
    listShareInvitationsResponse_nextToken,
    listShareInvitationsResponse_httpStatus,

    -- ** GetLensReview
    getLensReview_milestoneNumber,
    getLensReview_workloadId,
    getLensReview_lensAlias,
    getLensReviewResponse_milestoneNumber,
    getLensReviewResponse_workloadId,
    getLensReviewResponse_lensReview,
    getLensReviewResponse_httpStatus,

    -- ** TagResource
    tagResource_workloadArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** CreateWorkload
    createWorkload_architecturalDesign,
    createWorkload_accountIds,
    createWorkload_industry,
    createWorkload_awsRegions,
    createWorkload_industryType,
    createWorkload_notes,
    createWorkload_pillarPriorities,
    createWorkload_nonAwsRegions,
    createWorkload_tags,
    createWorkload_workloadName,
    createWorkload_description,
    createWorkload_environment,
    createWorkload_reviewOwner,
    createWorkload_lenses,
    createWorkload_clientRequestToken,
    createWorkloadResponse_workloadArn,
    createWorkloadResponse_workloadId,
    createWorkloadResponse_httpStatus,

    -- ** DeleteWorkloadShare
    deleteWorkloadShare_shareId,
    deleteWorkloadShare_workloadId,
    deleteWorkloadShare_clientRequestToken,

    -- ** UpdateWorkloadShare
    updateWorkloadShare_shareId,
    updateWorkloadShare_workloadId,
    updateWorkloadShare_permissionType,
    updateWorkloadShareResponse_workloadShare,
    updateWorkloadShareResponse_workloadId,
    updateWorkloadShareResponse_httpStatus,

    -- ** UntagResource
    untagResource_workloadArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** AssociateLenses
    associateLenses_workloadId,
    associateLenses_lensAliases,

    -- ** ListWorkloads
    listWorkloads_nextToken,
    listWorkloads_workloadNamePrefix,
    listWorkloads_maxResults,
    listWorkloadsResponse_workloadSummaries,
    listWorkloadsResponse_nextToken,
    listWorkloadsResponse_httpStatus,

    -- ** CreateWorkloadShare
    createWorkloadShare_workloadId,
    createWorkloadShare_sharedWith,
    createWorkloadShare_permissionType,
    createWorkloadShare_clientRequestToken,
    createWorkloadShareResponse_workloadId,
    createWorkloadShareResponse_shareId,
    createWorkloadShareResponse_httpStatus,

    -- ** GetLensReviewReport
    getLensReviewReport_milestoneNumber,
    getLensReviewReport_workloadId,
    getLensReviewReport_lensAlias,
    getLensReviewReportResponse_milestoneNumber,
    getLensReviewReportResponse_workloadId,
    getLensReviewReportResponse_lensReviewReport,
    getLensReviewReportResponse_httpStatus,

    -- ** UpgradeLensReview
    upgradeLensReview_clientRequestToken,
    upgradeLensReview_workloadId,
    upgradeLensReview_lensAlias,
    upgradeLensReview_milestoneName,

    -- ** GetWorkload
    getWorkload_workloadId,
    getWorkloadResponse_workload,
    getWorkloadResponse_httpStatus,

    -- * Types

    -- ** Answer
    answer_choiceAnswers,
    answer_helpfulResourceUrl,
    answer_isApplicable,
    answer_pillarId,
    answer_improvementPlanUrl,
    answer_questionDescription,
    answer_risk,
    answer_questionTitle,
    answer_selectedChoices,
    answer_reason,
    answer_choices,
    answer_questionId,
    answer_notes,

    -- ** AnswerSummary
    answerSummary_isApplicable,
    answerSummary_pillarId,
    answerSummary_choiceAnswerSummaries,
    answerSummary_risk,
    answerSummary_questionTitle,
    answerSummary_selectedChoices,
    answerSummary_reason,
    answerSummary_choices,
    answerSummary_questionId,

    -- ** Choice
    choice_title,
    choice_description,
    choice_choiceId,

    -- ** ChoiceAnswer
    choiceAnswer_status,
    choiceAnswer_reason,
    choiceAnswer_notes,
    choiceAnswer_choiceId,

    -- ** ChoiceAnswerSummary
    choiceAnswerSummary_status,
    choiceAnswerSummary_reason,
    choiceAnswerSummary_choiceId,

    -- ** ChoiceUpdate
    choiceUpdate_reason,
    choiceUpdate_notes,
    choiceUpdate_status,

    -- ** ImprovementSummary
    improvementSummary_pillarId,
    improvementSummary_improvementPlanUrl,
    improvementSummary_risk,
    improvementSummary_questionTitle,
    improvementSummary_questionId,

    -- ** LensReview
    lensReview_lensAlias,
    lensReview_riskCounts,
    lensReview_lensName,
    lensReview_nextToken,
    lensReview_pillarReviewSummaries,
    lensReview_updatedAt,
    lensReview_lensStatus,
    lensReview_notes,
    lensReview_lensVersion,

    -- ** LensReviewReport
    lensReviewReport_lensAlias,
    lensReviewReport_base64String,

    -- ** LensReviewSummary
    lensReviewSummary_lensAlias,
    lensReviewSummary_riskCounts,
    lensReviewSummary_lensName,
    lensReviewSummary_updatedAt,
    lensReviewSummary_lensStatus,
    lensReviewSummary_lensVersion,

    -- ** LensSummary
    lensSummary_lensAlias,
    lensSummary_lensName,
    lensSummary_lensVersion,
    lensSummary_description,

    -- ** LensUpgradeSummary
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_workloadId,
    lensUpgradeSummary_workloadName,

    -- ** Milestone
    milestone_workload,
    milestone_milestoneNumber,
    milestone_milestoneName,
    milestone_recordedAt,

    -- ** MilestoneSummary
    milestoneSummary_milestoneNumber,
    milestoneSummary_milestoneName,
    milestoneSummary_recordedAt,
    milestoneSummary_workloadSummary,

    -- ** NotificationSummary
    notificationSummary_lensUpgradeSummary,
    notificationSummary_type,

    -- ** PillarDifference
    pillarDifference_pillarId,
    pillarDifference_questionDifferences,
    pillarDifference_differenceStatus,

    -- ** PillarReviewSummary
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,
    pillarReviewSummary_riskCounts,
    pillarReviewSummary_notes,

    -- ** QuestionDifference
    questionDifference_questionTitle,
    questionDifference_differenceStatus,
    questionDifference_questionId,

    -- ** ShareInvitation
    shareInvitation_workloadId,
    shareInvitation_shareInvitationId,

    -- ** ShareInvitationSummary
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_permissionType,
    shareInvitationSummary_workloadId,
    shareInvitationSummary_workloadName,
    shareInvitationSummary_shareInvitationId,

    -- ** VersionDifferences
    versionDifferences_pillarDifferences,

    -- ** Workload
    workload_isReviewOwnerUpdateAcknowledged,
    workload_architecturalDesign,
    workload_accountIds,
    workload_lenses,
    workload_reviewRestrictionDate,
    workload_industry,
    workload_environment,
    workload_riskCounts,
    workload_awsRegions,
    workload_owner,
    workload_improvementStatus,
    workload_workloadArn,
    workload_industryType,
    workload_workloadId,
    workload_workloadName,
    workload_updatedAt,
    workload_notes,
    workload_reviewOwner,
    workload_description,
    workload_pillarPriorities,
    workload_shareInvitationId,
    workload_nonAwsRegions,
    workload_tags,

    -- ** WorkloadShare
    workloadShare_status,
    workloadShare_sharedBy,
    workloadShare_sharedWith,
    workloadShare_permissionType,
    workloadShare_workloadId,
    workloadShare_workloadName,
    workloadShare_shareId,

    -- ** WorkloadShareSummary
    workloadShareSummary_status,
    workloadShareSummary_sharedWith,
    workloadShareSummary_permissionType,
    workloadShareSummary_shareId,

    -- ** WorkloadSummary
    workloadSummary_lenses,
    workloadSummary_riskCounts,
    workloadSummary_owner,
    workloadSummary_improvementStatus,
    workloadSummary_workloadArn,
    workloadSummary_workloadId,
    workloadSummary_workloadName,
    workloadSummary_updatedAt,
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
import Network.AWS.WellArchitected.Types.Answer
import Network.AWS.WellArchitected.Types.AnswerSummary
import Network.AWS.WellArchitected.Types.Choice
import Network.AWS.WellArchitected.Types.ChoiceAnswer
import Network.AWS.WellArchitected.Types.ChoiceAnswerSummary
import Network.AWS.WellArchitected.Types.ChoiceUpdate
import Network.AWS.WellArchitected.Types.ImprovementSummary
import Network.AWS.WellArchitected.Types.LensReview
import Network.AWS.WellArchitected.Types.LensReviewReport
import Network.AWS.WellArchitected.Types.LensReviewSummary
import Network.AWS.WellArchitected.Types.LensSummary
import Network.AWS.WellArchitected.Types.LensUpgradeSummary
import Network.AWS.WellArchitected.Types.Milestone
import Network.AWS.WellArchitected.Types.MilestoneSummary
import Network.AWS.WellArchitected.Types.NotificationSummary
import Network.AWS.WellArchitected.Types.PillarDifference
import Network.AWS.WellArchitected.Types.PillarReviewSummary
import Network.AWS.WellArchitected.Types.QuestionDifference
import Network.AWS.WellArchitected.Types.ShareInvitation
import Network.AWS.WellArchitected.Types.ShareInvitationSummary
import Network.AWS.WellArchitected.Types.VersionDifferences
import Network.AWS.WellArchitected.Types.Workload
import Network.AWS.WellArchitected.Types.WorkloadShare
import Network.AWS.WellArchitected.Types.WorkloadShareSummary
import Network.AWS.WellArchitected.Types.WorkloadSummary
import Network.AWS.WellArchitected.UntagResource
import Network.AWS.WellArchitected.UpdateAnswer
import Network.AWS.WellArchitected.UpdateLensReview
import Network.AWS.WellArchitected.UpdateShareInvitation
import Network.AWS.WellArchitected.UpdateWorkload
import Network.AWS.WellArchitected.UpdateWorkloadShare
import Network.AWS.WellArchitected.UpgradeLensReview
