{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Lens
  ( -- * Operations

    -- ** AssociateLenses
    associateLenses_workloadId,
    associateLenses_lensAliases,

    -- ** CreateLensShare
    createLensShare_lensAlias,
    createLensShare_sharedWith,
    createLensShare_clientRequestToken,
    createLensShareResponse_shareId,
    createLensShareResponse_httpStatus,

    -- ** CreateLensVersion
    createLensVersion_isMajorVersion,
    createLensVersion_lensAlias,
    createLensVersion_lensVersion,
    createLensVersion_clientRequestToken,
    createLensVersionResponse_lensArn,
    createLensVersionResponse_lensVersion,
    createLensVersionResponse_httpStatus,

    -- ** CreateMilestone
    createMilestone_workloadId,
    createMilestone_milestoneName,
    createMilestone_clientRequestToken,
    createMilestoneResponse_milestoneNumber,
    createMilestoneResponse_workloadId,
    createMilestoneResponse_httpStatus,

    -- ** CreateWorkload
    createWorkload_discoveryConfig,
    createWorkload_tags,
    createWorkload_accountIds,
    createWorkload_industry,
    createWorkload_applications,
    createWorkload_awsRegions,
    createWorkload_reviewOwner,
    createWorkload_nonAwsRegions,
    createWorkload_notes,
    createWorkload_industryType,
    createWorkload_architecturalDesign,
    createWorkload_pillarPriorities,
    createWorkload_workloadName,
    createWorkload_description,
    createWorkload_environment,
    createWorkload_lenses,
    createWorkload_clientRequestToken,
    createWorkloadResponse_workloadArn,
    createWorkloadResponse_workloadId,
    createWorkloadResponse_httpStatus,

    -- ** CreateWorkloadShare
    createWorkloadShare_workloadId,
    createWorkloadShare_sharedWith,
    createWorkloadShare_permissionType,
    createWorkloadShare_clientRequestToken,
    createWorkloadShareResponse_shareId,
    createWorkloadShareResponse_workloadId,
    createWorkloadShareResponse_httpStatus,

    -- ** DeleteLens
    deleteLens_lensAlias,
    deleteLens_clientRequestToken,
    deleteLens_lensStatus,

    -- ** DeleteLensShare
    deleteLensShare_shareId,
    deleteLensShare_lensAlias,
    deleteLensShare_clientRequestToken,

    -- ** DeleteWorkload
    deleteWorkload_workloadId,
    deleteWorkload_clientRequestToken,

    -- ** DeleteWorkloadShare
    deleteWorkloadShare_shareId,
    deleteWorkloadShare_workloadId,
    deleteWorkloadShare_clientRequestToken,

    -- ** DisassociateLenses
    disassociateLenses_workloadId,
    disassociateLenses_lensAliases,

    -- ** ExportLens
    exportLens_lensVersion,
    exportLens_lensAlias,
    exportLensResponse_lensJSON,
    exportLensResponse_httpStatus,

    -- ** GetAnswer
    getAnswer_milestoneNumber,
    getAnswer_workloadId,
    getAnswer_lensAlias,
    getAnswer_questionId,
    getAnswerResponse_lensArn,
    getAnswerResponse_lensAlias,
    getAnswerResponse_answer,
    getAnswerResponse_milestoneNumber,
    getAnswerResponse_workloadId,
    getAnswerResponse_httpStatus,

    -- ** GetLens
    getLens_lensVersion,
    getLens_lensAlias,
    getLensResponse_lens,
    getLensResponse_httpStatus,

    -- ** GetLensReview
    getLensReview_milestoneNumber,
    getLensReview_workloadId,
    getLensReview_lensAlias,
    getLensReviewResponse_lensReview,
    getLensReviewResponse_milestoneNumber,
    getLensReviewResponse_workloadId,
    getLensReviewResponse_httpStatus,

    -- ** GetLensReviewReport
    getLensReviewReport_milestoneNumber,
    getLensReviewReport_workloadId,
    getLensReviewReport_lensAlias,
    getLensReviewReportResponse_lensReviewReport,
    getLensReviewReportResponse_milestoneNumber,
    getLensReviewReportResponse_workloadId,
    getLensReviewReportResponse_httpStatus,

    -- ** GetLensVersionDifference
    getLensVersionDifference_targetLensVersion,
    getLensVersionDifference_baseLensVersion,
    getLensVersionDifference_lensAlias,
    getLensVersionDifferenceResponse_lensArn,
    getLensVersionDifferenceResponse_targetLensVersion,
    getLensVersionDifferenceResponse_lensAlias,
    getLensVersionDifferenceResponse_baseLensVersion,
    getLensVersionDifferenceResponse_versionDifferences,
    getLensVersionDifferenceResponse_latestLensVersion,
    getLensVersionDifferenceResponse_httpStatus,

    -- ** GetMilestone
    getMilestone_workloadId,
    getMilestone_milestoneNumber,
    getMilestoneResponse_milestone,
    getMilestoneResponse_workloadId,
    getMilestoneResponse_httpStatus,

    -- ** GetWorkload
    getWorkload_workloadId,
    getWorkloadResponse_workload,
    getWorkloadResponse_httpStatus,

    -- ** ImportLens
    importLens_tags,
    importLens_lensAlias,
    importLens_jSONString,
    importLens_clientRequestToken,
    importLensResponse_lensArn,
    importLensResponse_status,
    importLensResponse_httpStatus,

    -- ** ListAnswers
    listAnswers_nextToken,
    listAnswers_maxResults,
    listAnswers_milestoneNumber,
    listAnswers_pillarId,
    listAnswers_workloadId,
    listAnswers_lensAlias,
    listAnswersResponse_answerSummaries,
    listAnswersResponse_nextToken,
    listAnswersResponse_lensArn,
    listAnswersResponse_lensAlias,
    listAnswersResponse_milestoneNumber,
    listAnswersResponse_workloadId,
    listAnswersResponse_httpStatus,

    -- ** ListCheckDetails
    listCheckDetails_nextToken,
    listCheckDetails_maxResults,
    listCheckDetails_workloadId,
    listCheckDetails_lensArn,
    listCheckDetails_pillarId,
    listCheckDetails_questionId,
    listCheckDetails_choiceId,
    listCheckDetailsResponse_nextToken,
    listCheckDetailsResponse_checkDetails,
    listCheckDetailsResponse_httpStatus,

    -- ** ListCheckSummaries
    listCheckSummaries_nextToken,
    listCheckSummaries_maxResults,
    listCheckSummaries_workloadId,
    listCheckSummaries_lensArn,
    listCheckSummaries_pillarId,
    listCheckSummaries_questionId,
    listCheckSummaries_choiceId,
    listCheckSummariesResponse_nextToken,
    listCheckSummariesResponse_checkSummaries,
    listCheckSummariesResponse_httpStatus,

    -- ** ListLensReviewImprovements
    listLensReviewImprovements_nextToken,
    listLensReviewImprovements_maxResults,
    listLensReviewImprovements_milestoneNumber,
    listLensReviewImprovements_pillarId,
    listLensReviewImprovements_workloadId,
    listLensReviewImprovements_lensAlias,
    listLensReviewImprovementsResponse_nextToken,
    listLensReviewImprovementsResponse_lensArn,
    listLensReviewImprovementsResponse_lensAlias,
    listLensReviewImprovementsResponse_improvementSummaries,
    listLensReviewImprovementsResponse_milestoneNumber,
    listLensReviewImprovementsResponse_workloadId,
    listLensReviewImprovementsResponse_httpStatus,

    -- ** ListLensReviews
    listLensReviews_nextToken,
    listLensReviews_maxResults,
    listLensReviews_milestoneNumber,
    listLensReviews_workloadId,
    listLensReviewsResponse_nextToken,
    listLensReviewsResponse_lensReviewSummaries,
    listLensReviewsResponse_milestoneNumber,
    listLensReviewsResponse_workloadId,
    listLensReviewsResponse_httpStatus,

    -- ** ListLensShares
    listLensShares_nextToken,
    listLensShares_sharedWithPrefix,
    listLensShares_status,
    listLensShares_maxResults,
    listLensShares_lensAlias,
    listLensSharesResponse_nextToken,
    listLensSharesResponse_lensShareSummaries,
    listLensSharesResponse_httpStatus,

    -- ** ListLenses
    listLenses_nextToken,
    listLenses_lensName,
    listLenses_maxResults,
    listLenses_lensType,
    listLenses_lensStatus,
    listLensesResponse_lensSummaries,
    listLensesResponse_nextToken,
    listLensesResponse_httpStatus,

    -- ** ListMilestones
    listMilestones_nextToken,
    listMilestones_maxResults,
    listMilestones_workloadId,
    listMilestonesResponse_nextToken,
    listMilestonesResponse_milestoneSummaries,
    listMilestonesResponse_workloadId,
    listMilestonesResponse_httpStatus,

    -- ** ListNotifications
    listNotifications_nextToken,
    listNotifications_maxResults,
    listNotifications_workloadId,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_notificationSummaries,
    listNotificationsResponse_httpStatus,

    -- ** ListShareInvitations
    listShareInvitations_lensNamePrefix,
    listShareInvitations_nextToken,
    listShareInvitations_workloadNamePrefix,
    listShareInvitations_maxResults,
    listShareInvitations_shareResourceType,
    listShareInvitationsResponse_nextToken,
    listShareInvitationsResponse_shareInvitationSummaries,
    listShareInvitationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_workloadArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkloadShares
    listWorkloadShares_nextToken,
    listWorkloadShares_sharedWithPrefix,
    listWorkloadShares_status,
    listWorkloadShares_maxResults,
    listWorkloadShares_workloadId,
    listWorkloadSharesResponse_nextToken,
    listWorkloadSharesResponse_workloadShareSummaries,
    listWorkloadSharesResponse_workloadId,
    listWorkloadSharesResponse_httpStatus,

    -- ** ListWorkloads
    listWorkloads_nextToken,
    listWorkloads_workloadNamePrefix,
    listWorkloads_maxResults,
    listWorkloadsResponse_nextToken,
    listWorkloadsResponse_workloadSummaries,
    listWorkloadsResponse_httpStatus,

    -- ** TagResource
    tagResource_workloadArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_workloadArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateAnswer
    updateAnswer_selectedChoices,
    updateAnswer_choiceUpdates,
    updateAnswer_isApplicable,
    updateAnswer_reason,
    updateAnswer_notes,
    updateAnswer_workloadId,
    updateAnswer_lensAlias,
    updateAnswer_questionId,
    updateAnswerResponse_lensArn,
    updateAnswerResponse_lensAlias,
    updateAnswerResponse_answer,
    updateAnswerResponse_workloadId,
    updateAnswerResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_organizationSharingStatus,

    -- ** UpdateLensReview
    updateLensReview_pillarNotes,
    updateLensReview_lensNotes,
    updateLensReview_workloadId,
    updateLensReview_lensAlias,
    updateLensReviewResponse_lensReview,
    updateLensReviewResponse_workloadId,
    updateLensReviewResponse_httpStatus,

    -- ** UpdateShareInvitation
    updateShareInvitation_shareInvitationId,
    updateShareInvitation_shareInvitationAction,
    updateShareInvitationResponse_shareInvitation,
    updateShareInvitationResponse_httpStatus,

    -- ** UpdateWorkload
    updateWorkload_discoveryConfig,
    updateWorkload_accountIds,
    updateWorkload_environment,
    updateWorkload_isReviewOwnerUpdateAcknowledged,
    updateWorkload_industry,
    updateWorkload_applications,
    updateWorkload_awsRegions,
    updateWorkload_workloadName,
    updateWorkload_reviewOwner,
    updateWorkload_nonAwsRegions,
    updateWorkload_description,
    updateWorkload_notes,
    updateWorkload_industryType,
    updateWorkload_architecturalDesign,
    updateWorkload_pillarPriorities,
    updateWorkload_improvementStatus,
    updateWorkload_workloadId,
    updateWorkloadResponse_workload,
    updateWorkloadResponse_httpStatus,

    -- ** UpdateWorkloadShare
    updateWorkloadShare_shareId,
    updateWorkloadShare_workloadId,
    updateWorkloadShare_permissionType,
    updateWorkloadShareResponse_workloadShare,
    updateWorkloadShareResponse_workloadId,
    updateWorkloadShareResponse_httpStatus,

    -- ** UpgradeLensReview
    upgradeLensReview_clientRequestToken,
    upgradeLensReview_workloadId,
    upgradeLensReview_lensAlias,
    upgradeLensReview_milestoneName,

    -- * Types

    -- ** AdditionalResources
    additionalResources_type,
    additionalResources_content,

    -- ** Answer
    answer_choices,
    answer_selectedChoices,
    answer_helpfulResourceDisplayText,
    answer_risk,
    answer_questionId,
    answer_questionDescription,
    answer_improvementPlanUrl,
    answer_isApplicable,
    answer_reason,
    answer_notes,
    answer_questionTitle,
    answer_helpfulResourceUrl,
    answer_pillarId,
    answer_choiceAnswers,

    -- ** AnswerSummary
    answerSummary_choices,
    answerSummary_selectedChoices,
    answerSummary_risk,
    answerSummary_choiceAnswerSummaries,
    answerSummary_questionId,
    answerSummary_isApplicable,
    answerSummary_reason,
    answerSummary_questionTitle,
    answerSummary_pillarId,

    -- ** CheckDetail
    checkDetail_name,
    checkDetail_lensArn,
    checkDetail_provider,
    checkDetail_questionId,
    checkDetail_status,
    checkDetail_id,
    checkDetail_description,
    checkDetail_choiceId,
    checkDetail_accountId,
    checkDetail_reason,
    checkDetail_flaggedResources,
    checkDetail_pillarId,
    checkDetail_updatedAt,

    -- ** CheckSummary
    checkSummary_name,
    checkSummary_lensArn,
    checkSummary_accountSummary,
    checkSummary_provider,
    checkSummary_questionId,
    checkSummary_status,
    checkSummary_id,
    checkSummary_description,
    checkSummary_choiceId,
    checkSummary_pillarId,
    checkSummary_updatedAt,

    -- ** Choice
    choice_description,
    choice_choiceId,
    choice_title,
    choice_helpfulResource,
    choice_additionalResources,
    choice_improvementPlan,

    -- ** ChoiceAnswer
    choiceAnswer_status,
    choiceAnswer_choiceId,
    choiceAnswer_reason,
    choiceAnswer_notes,

    -- ** ChoiceAnswerSummary
    choiceAnswerSummary_status,
    choiceAnswerSummary_choiceId,
    choiceAnswerSummary_reason,

    -- ** ChoiceContent
    choiceContent_displayText,
    choiceContent_url,

    -- ** ChoiceImprovementPlan
    choiceImprovementPlan_displayText,
    choiceImprovementPlan_improvementPlanUrl,
    choiceImprovementPlan_choiceId,

    -- ** ChoiceUpdate
    choiceUpdate_reason,
    choiceUpdate_notes,
    choiceUpdate_status,

    -- ** ImprovementSummary
    improvementSummary_risk,
    improvementSummary_questionId,
    improvementSummary_improvementPlanUrl,
    improvementSummary_improvementPlans,
    improvementSummary_questionTitle,
    improvementSummary_pillarId,

    -- ** Lens
    lens_tags,
    lens_name,
    lens_lensArn,
    lens_shareInvitationId,
    lens_owner,
    lens_lensVersion,
    lens_description,

    -- ** LensReview
    lensReview_nextToken,
    lensReview_lensArn,
    lensReview_riskCounts,
    lensReview_lensAlias,
    lensReview_lensVersion,
    lensReview_lensName,
    lensReview_pillarReviewSummaries,
    lensReview_notes,
    lensReview_lensStatus,
    lensReview_updatedAt,

    -- ** LensReviewReport
    lensReviewReport_lensArn,
    lensReviewReport_lensAlias,
    lensReviewReport_base64String,

    -- ** LensReviewSummary
    lensReviewSummary_lensArn,
    lensReviewSummary_riskCounts,
    lensReviewSummary_lensAlias,
    lensReviewSummary_lensVersion,
    lensReviewSummary_lensName,
    lensReviewSummary_lensStatus,
    lensReviewSummary_updatedAt,

    -- ** LensShareSummary
    lensShareSummary_sharedWith,
    lensShareSummary_status,
    lensShareSummary_shareId,
    lensShareSummary_statusMessage,

    -- ** LensSummary
    lensSummary_lensArn,
    lensSummary_lensAlias,
    lensSummary_owner,
    lensSummary_lensVersion,
    lensSummary_description,
    lensSummary_lensName,
    lensSummary_lensType,
    lensSummary_lensStatus,
    lensSummary_createdAt,
    lensSummary_updatedAt,

    -- ** LensUpgradeSummary
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_lensArn,
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_workloadName,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_workloadId,

    -- ** Milestone
    milestone_recordedAt,
    milestone_milestoneName,
    milestone_milestoneNumber,
    milestone_workload,

    -- ** MilestoneSummary
    milestoneSummary_recordedAt,
    milestoneSummary_milestoneName,
    milestoneSummary_workloadSummary,
    milestoneSummary_milestoneNumber,

    -- ** NotificationSummary
    notificationSummary_type,
    notificationSummary_lensUpgradeSummary,

    -- ** PillarDifference
    pillarDifference_differenceStatus,
    pillarDifference_questionDifferences,
    pillarDifference_pillarId,
    pillarDifference_pillarName,

    -- ** PillarReviewSummary
    pillarReviewSummary_riskCounts,
    pillarReviewSummary_notes,
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,

    -- ** QuestionDifference
    questionDifference_questionId,
    questionDifference_differenceStatus,
    questionDifference_questionTitle,

    -- ** ShareInvitation
    shareInvitation_lensArn,
    shareInvitation_lensAlias,
    shareInvitation_shareInvitationId,
    shareInvitation_shareResourceType,
    shareInvitation_workloadId,

    -- ** ShareInvitationSummary
    shareInvitationSummary_permissionType,
    shareInvitationSummary_lensArn,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_shareInvitationId,
    shareInvitationSummary_workloadName,
    shareInvitationSummary_lensName,
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_shareResourceType,
    shareInvitationSummary_workloadId,

    -- ** VersionDifferences
    versionDifferences_pillarDifferences,

    -- ** Workload
    workload_discoveryConfig,
    workload_tags,
    workload_accountIds,
    workload_environment,
    workload_riskCounts,
    workload_isReviewOwnerUpdateAcknowledged,
    workload_industry,
    workload_applications,
    workload_shareInvitationId,
    workload_workloadArn,
    workload_awsRegions,
    workload_workloadName,
    workload_reviewOwner,
    workload_owner,
    workload_nonAwsRegions,
    workload_description,
    workload_notes,
    workload_industryType,
    workload_architecturalDesign,
    workload_pillarPriorities,
    workload_improvementStatus,
    workload_lenses,
    workload_reviewRestrictionDate,
    workload_updatedAt,
    workload_workloadId,

    -- ** WorkloadDiscoveryConfig
    workloadDiscoveryConfig_trustedAdvisorIntegrationStatus,

    -- ** WorkloadShare
    workloadShare_permissionType,
    workloadShare_sharedWith,
    workloadShare_workloadName,
    workloadShare_status,
    workloadShare_shareId,
    workloadShare_sharedBy,
    workloadShare_workloadId,

    -- ** WorkloadShareSummary
    workloadShareSummary_permissionType,
    workloadShareSummary_sharedWith,
    workloadShareSummary_status,
    workloadShareSummary_shareId,
    workloadShareSummary_statusMessage,

    -- ** WorkloadSummary
    workloadSummary_riskCounts,
    workloadSummary_workloadArn,
    workloadSummary_workloadName,
    workloadSummary_owner,
    workloadSummary_improvementStatus,
    workloadSummary_lenses,
    workloadSummary_updatedAt,
    workloadSummary_workloadId,
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
import Amazonka.WellArchitected.Types.AdditionalResources
import Amazonka.WellArchitected.Types.Answer
import Amazonka.WellArchitected.Types.AnswerSummary
import Amazonka.WellArchitected.Types.CheckDetail
import Amazonka.WellArchitected.Types.CheckSummary
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswer
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.ChoiceContent
import Amazonka.WellArchitected.Types.ChoiceImprovementPlan
import Amazonka.WellArchitected.Types.ChoiceUpdate
import Amazonka.WellArchitected.Types.ImprovementSummary
import Amazonka.WellArchitected.Types.Lens
import Amazonka.WellArchitected.Types.LensReview
import Amazonka.WellArchitected.Types.LensReviewReport
import Amazonka.WellArchitected.Types.LensReviewSummary
import Amazonka.WellArchitected.Types.LensShareSummary
import Amazonka.WellArchitected.Types.LensSummary
import Amazonka.WellArchitected.Types.LensUpgradeSummary
import Amazonka.WellArchitected.Types.Milestone
import Amazonka.WellArchitected.Types.MilestoneSummary
import Amazonka.WellArchitected.Types.NotificationSummary
import Amazonka.WellArchitected.Types.PillarDifference
import Amazonka.WellArchitected.Types.PillarReviewSummary
import Amazonka.WellArchitected.Types.QuestionDifference
import Amazonka.WellArchitected.Types.ShareInvitation
import Amazonka.WellArchitected.Types.ShareInvitationSummary
import Amazonka.WellArchitected.Types.VersionDifferences
import Amazonka.WellArchitected.Types.Workload
import Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig
import Amazonka.WellArchitected.Types.WorkloadShare
import Amazonka.WellArchitected.Types.WorkloadShareSummary
import Amazonka.WellArchitected.Types.WorkloadSummary
import Amazonka.WellArchitected.UntagResource
import Amazonka.WellArchitected.UpdateAnswer
import Amazonka.WellArchitected.UpdateGlobalSettings
import Amazonka.WellArchitected.UpdateLensReview
import Amazonka.WellArchitected.UpdateShareInvitation
import Amazonka.WellArchitected.UpdateWorkload
import Amazonka.WellArchitected.UpdateWorkloadShare
import Amazonka.WellArchitected.UpgradeLensReview
