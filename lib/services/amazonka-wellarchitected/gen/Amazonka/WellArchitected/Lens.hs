{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Lens
  ( -- * Operations

    -- ** AssociateLenses
    associateLenses_workloadId,
    associateLenses_lensAliases,

    -- ** AssociateProfiles
    associateProfiles_workloadId,
    associateProfiles_profileArns,

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

    -- ** CreateProfile
    createProfile_tags,
    createProfile_profileName,
    createProfile_profileDescription,
    createProfile_profileQuestions,
    createProfile_clientRequestToken,
    createProfileResponse_profileArn,
    createProfileResponse_profileVersion,
    createProfileResponse_httpStatus,

    -- ** CreateProfileShare
    createProfileShare_profileArn,
    createProfileShare_sharedWith,
    createProfileShare_clientRequestToken,
    createProfileShareResponse_profileArn,
    createProfileShareResponse_shareId,
    createProfileShareResponse_httpStatus,

    -- ** CreateWorkload
    createWorkload_accountIds,
    createWorkload_applications,
    createWorkload_architecturalDesign,
    createWorkload_awsRegions,
    createWorkload_discoveryConfig,
    createWorkload_industry,
    createWorkload_industryType,
    createWorkload_nonAwsRegions,
    createWorkload_notes,
    createWorkload_pillarPriorities,
    createWorkload_profileArns,
    createWorkload_reviewOwner,
    createWorkload_tags,
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

    -- ** DeleteProfile
    deleteProfile_profileArn,
    deleteProfile_clientRequestToken,

    -- ** DeleteProfileShare
    deleteProfileShare_shareId,
    deleteProfileShare_profileArn,
    deleteProfileShare_clientRequestToken,

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

    -- ** DisassociateProfiles
    disassociateProfiles_workloadId,
    disassociateProfiles_profileArns,

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
    getAnswerResponse_answer,
    getAnswerResponse_lensAlias,
    getAnswerResponse_lensArn,
    getAnswerResponse_milestoneNumber,
    getAnswerResponse_workloadId,
    getAnswerResponse_httpStatus,

    -- ** GetConsolidatedReport
    getConsolidatedReport_includeSharedResources,
    getConsolidatedReport_maxResults,
    getConsolidatedReport_nextToken,
    getConsolidatedReport_format,
    getConsolidatedReportResponse_base64String,
    getConsolidatedReportResponse_metrics,
    getConsolidatedReportResponse_nextToken,
    getConsolidatedReportResponse_httpStatus,

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
    getLensVersionDifference_baseLensVersion,
    getLensVersionDifference_targetLensVersion,
    getLensVersionDifference_lensAlias,
    getLensVersionDifferenceResponse_baseLensVersion,
    getLensVersionDifferenceResponse_latestLensVersion,
    getLensVersionDifferenceResponse_lensAlias,
    getLensVersionDifferenceResponse_lensArn,
    getLensVersionDifferenceResponse_targetLensVersion,
    getLensVersionDifferenceResponse_versionDifferences,
    getLensVersionDifferenceResponse_httpStatus,

    -- ** GetMilestone
    getMilestone_workloadId,
    getMilestone_milestoneNumber,
    getMilestoneResponse_milestone,
    getMilestoneResponse_workloadId,
    getMilestoneResponse_httpStatus,

    -- ** GetProfile
    getProfile_profileVersion,
    getProfile_profileArn,
    getProfileResponse_profile,
    getProfileResponse_httpStatus,

    -- ** GetProfileTemplate
    getProfileTemplateResponse_profileTemplate,
    getProfileTemplateResponse_httpStatus,

    -- ** GetWorkload
    getWorkload_workloadId,
    getWorkloadResponse_workload,
    getWorkloadResponse_httpStatus,

    -- ** ImportLens
    importLens_lensAlias,
    importLens_tags,
    importLens_jSONString,
    importLens_clientRequestToken,
    importLensResponse_lensArn,
    importLensResponse_status,
    importLensResponse_httpStatus,

    -- ** ListAnswers
    listAnswers_maxResults,
    listAnswers_milestoneNumber,
    listAnswers_nextToken,
    listAnswers_pillarId,
    listAnswers_questionPriority,
    listAnswers_workloadId,
    listAnswers_lensAlias,
    listAnswersResponse_answerSummaries,
    listAnswersResponse_lensAlias,
    listAnswersResponse_lensArn,
    listAnswersResponse_milestoneNumber,
    listAnswersResponse_nextToken,
    listAnswersResponse_workloadId,
    listAnswersResponse_httpStatus,

    -- ** ListCheckDetails
    listCheckDetails_maxResults,
    listCheckDetails_nextToken,
    listCheckDetails_workloadId,
    listCheckDetails_lensArn,
    listCheckDetails_pillarId,
    listCheckDetails_questionId,
    listCheckDetails_choiceId,
    listCheckDetailsResponse_checkDetails,
    listCheckDetailsResponse_nextToken,
    listCheckDetailsResponse_httpStatus,

    -- ** ListCheckSummaries
    listCheckSummaries_maxResults,
    listCheckSummaries_nextToken,
    listCheckSummaries_workloadId,
    listCheckSummaries_lensArn,
    listCheckSummaries_pillarId,
    listCheckSummaries_questionId,
    listCheckSummaries_choiceId,
    listCheckSummariesResponse_checkSummaries,
    listCheckSummariesResponse_nextToken,
    listCheckSummariesResponse_httpStatus,

    -- ** ListLensReviewImprovements
    listLensReviewImprovements_maxResults,
    listLensReviewImprovements_milestoneNumber,
    listLensReviewImprovements_nextToken,
    listLensReviewImprovements_pillarId,
    listLensReviewImprovements_questionPriority,
    listLensReviewImprovements_workloadId,
    listLensReviewImprovements_lensAlias,
    listLensReviewImprovementsResponse_improvementSummaries,
    listLensReviewImprovementsResponse_lensAlias,
    listLensReviewImprovementsResponse_lensArn,
    listLensReviewImprovementsResponse_milestoneNumber,
    listLensReviewImprovementsResponse_nextToken,
    listLensReviewImprovementsResponse_workloadId,
    listLensReviewImprovementsResponse_httpStatus,

    -- ** ListLensReviews
    listLensReviews_maxResults,
    listLensReviews_milestoneNumber,
    listLensReviews_nextToken,
    listLensReviews_workloadId,
    listLensReviewsResponse_lensReviewSummaries,
    listLensReviewsResponse_milestoneNumber,
    listLensReviewsResponse_nextToken,
    listLensReviewsResponse_workloadId,
    listLensReviewsResponse_httpStatus,

    -- ** ListLensShares
    listLensShares_maxResults,
    listLensShares_nextToken,
    listLensShares_sharedWithPrefix,
    listLensShares_status,
    listLensShares_lensAlias,
    listLensSharesResponse_lensShareSummaries,
    listLensSharesResponse_nextToken,
    listLensSharesResponse_httpStatus,

    -- ** ListLenses
    listLenses_lensName,
    listLenses_lensStatus,
    listLenses_lensType,
    listLenses_maxResults,
    listLenses_nextToken,
    listLensesResponse_lensSummaries,
    listLensesResponse_nextToken,
    listLensesResponse_httpStatus,

    -- ** ListMilestones
    listMilestones_maxResults,
    listMilestones_nextToken,
    listMilestones_workloadId,
    listMilestonesResponse_milestoneSummaries,
    listMilestonesResponse_nextToken,
    listMilestonesResponse_workloadId,
    listMilestonesResponse_httpStatus,

    -- ** ListNotifications
    listNotifications_maxResults,
    listNotifications_nextToken,
    listNotifications_workloadId,
    listNotificationsResponse_nextToken,
    listNotificationsResponse_notificationSummaries,
    listNotificationsResponse_httpStatus,

    -- ** ListProfileNotifications
    listProfileNotifications_maxResults,
    listProfileNotifications_nextToken,
    listProfileNotifications_workloadId,
    listProfileNotificationsResponse_nextToken,
    listProfileNotificationsResponse_notificationSummaries,
    listProfileNotificationsResponse_httpStatus,

    -- ** ListProfileShares
    listProfileShares_maxResults,
    listProfileShares_nextToken,
    listProfileShares_sharedWithPrefix,
    listProfileShares_status,
    listProfileShares_profileArn,
    listProfileSharesResponse_nextToken,
    listProfileSharesResponse_profileShareSummaries,
    listProfileSharesResponse_httpStatus,

    -- ** ListProfiles
    listProfiles_maxResults,
    listProfiles_nextToken,
    listProfiles_profileNamePrefix,
    listProfiles_profileOwnerType,
    listProfilesResponse_nextToken,
    listProfilesResponse_profileSummaries,
    listProfilesResponse_httpStatus,

    -- ** ListShareInvitations
    listShareInvitations_lensNamePrefix,
    listShareInvitations_maxResults,
    listShareInvitations_nextToken,
    listShareInvitations_profileNamePrefix,
    listShareInvitations_shareResourceType,
    listShareInvitations_workloadNamePrefix,
    listShareInvitationsResponse_nextToken,
    listShareInvitationsResponse_shareInvitationSummaries,
    listShareInvitationsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_workloadArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListWorkloadShares
    listWorkloadShares_maxResults,
    listWorkloadShares_nextToken,
    listWorkloadShares_sharedWithPrefix,
    listWorkloadShares_status,
    listWorkloadShares_workloadId,
    listWorkloadSharesResponse_nextToken,
    listWorkloadSharesResponse_workloadId,
    listWorkloadSharesResponse_workloadShareSummaries,
    listWorkloadSharesResponse_httpStatus,

    -- ** ListWorkloads
    listWorkloads_maxResults,
    listWorkloads_nextToken,
    listWorkloads_workloadNamePrefix,
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
    updateAnswer_choiceUpdates,
    updateAnswer_isApplicable,
    updateAnswer_notes,
    updateAnswer_reason,
    updateAnswer_selectedChoices,
    updateAnswer_workloadId,
    updateAnswer_lensAlias,
    updateAnswer_questionId,
    updateAnswerResponse_answer,
    updateAnswerResponse_lensAlias,
    updateAnswerResponse_lensArn,
    updateAnswerResponse_workloadId,
    updateAnswerResponse_httpStatus,

    -- ** UpdateGlobalSettings
    updateGlobalSettings_discoveryIntegrationStatus,
    updateGlobalSettings_organizationSharingStatus,

    -- ** UpdateLensReview
    updateLensReview_lensNotes,
    updateLensReview_pillarNotes,
    updateLensReview_workloadId,
    updateLensReview_lensAlias,
    updateLensReviewResponse_lensReview,
    updateLensReviewResponse_workloadId,
    updateLensReviewResponse_httpStatus,

    -- ** UpdateProfile
    updateProfile_profileDescription,
    updateProfile_profileQuestions,
    updateProfile_profileArn,
    updateProfileResponse_profile,
    updateProfileResponse_httpStatus,

    -- ** UpdateShareInvitation
    updateShareInvitation_shareInvitationId,
    updateShareInvitation_shareInvitationAction,
    updateShareInvitationResponse_shareInvitation,
    updateShareInvitationResponse_httpStatus,

    -- ** UpdateWorkload
    updateWorkload_accountIds,
    updateWorkload_applications,
    updateWorkload_architecturalDesign,
    updateWorkload_awsRegions,
    updateWorkload_description,
    updateWorkload_discoveryConfig,
    updateWorkload_environment,
    updateWorkload_improvementStatus,
    updateWorkload_industry,
    updateWorkload_industryType,
    updateWorkload_isReviewOwnerUpdateAcknowledged,
    updateWorkload_nonAwsRegions,
    updateWorkload_notes,
    updateWorkload_pillarPriorities,
    updateWorkload_reviewOwner,
    updateWorkload_workloadName,
    updateWorkload_workloadId,
    updateWorkloadResponse_workload,
    updateWorkloadResponse_httpStatus,

    -- ** UpdateWorkloadShare
    updateWorkloadShare_shareId,
    updateWorkloadShare_workloadId,
    updateWorkloadShare_permissionType,
    updateWorkloadShareResponse_workloadId,
    updateWorkloadShareResponse_workloadShare,
    updateWorkloadShareResponse_httpStatus,

    -- ** UpgradeLensReview
    upgradeLensReview_clientRequestToken,
    upgradeLensReview_workloadId,
    upgradeLensReview_lensAlias,
    upgradeLensReview_milestoneName,

    -- ** UpgradeProfileVersion
    upgradeProfileVersion_clientRequestToken,
    upgradeProfileVersion_milestoneName,
    upgradeProfileVersion_workloadId,
    upgradeProfileVersion_profileArn,

    -- * Types

    -- ** AdditionalResources
    additionalResources_content,
    additionalResources_type,

    -- ** Answer
    answer_choiceAnswers,
    answer_choices,
    answer_helpfulResourceDisplayText,
    answer_helpfulResourceUrl,
    answer_improvementPlanUrl,
    answer_isApplicable,
    answer_notes,
    answer_pillarId,
    answer_questionDescription,
    answer_questionId,
    answer_questionTitle,
    answer_reason,
    answer_risk,
    answer_selectedChoices,

    -- ** AnswerSummary
    answerSummary_choiceAnswerSummaries,
    answerSummary_choices,
    answerSummary_isApplicable,
    answerSummary_pillarId,
    answerSummary_questionId,
    answerSummary_questionTitle,
    answerSummary_questionType,
    answerSummary_reason,
    answerSummary_risk,
    answerSummary_selectedChoices,

    -- ** BestPractice
    bestPractice_choiceId,
    bestPractice_choiceTitle,

    -- ** CheckDetail
    checkDetail_accountId,
    checkDetail_choiceId,
    checkDetail_description,
    checkDetail_flaggedResources,
    checkDetail_id,
    checkDetail_lensArn,
    checkDetail_name,
    checkDetail_pillarId,
    checkDetail_provider,
    checkDetail_questionId,
    checkDetail_reason,
    checkDetail_status,
    checkDetail_updatedAt,

    -- ** CheckSummary
    checkSummary_accountSummary,
    checkSummary_choiceId,
    checkSummary_description,
    checkSummary_id,
    checkSummary_lensArn,
    checkSummary_name,
    checkSummary_pillarId,
    checkSummary_provider,
    checkSummary_questionId,
    checkSummary_status,
    checkSummary_updatedAt,

    -- ** Choice
    choice_additionalResources,
    choice_choiceId,
    choice_description,
    choice_helpfulResource,
    choice_improvementPlan,
    choice_title,

    -- ** ChoiceAnswer
    choiceAnswer_choiceId,
    choiceAnswer_notes,
    choiceAnswer_reason,
    choiceAnswer_status,

    -- ** ChoiceAnswerSummary
    choiceAnswerSummary_choiceId,
    choiceAnswerSummary_reason,
    choiceAnswerSummary_status,

    -- ** ChoiceContent
    choiceContent_displayText,
    choiceContent_url,

    -- ** ChoiceImprovementPlan
    choiceImprovementPlan_choiceId,
    choiceImprovementPlan_displayText,
    choiceImprovementPlan_improvementPlanUrl,

    -- ** ChoiceUpdate
    choiceUpdate_notes,
    choiceUpdate_reason,
    choiceUpdate_status,

    -- ** ConsolidatedReportMetric
    consolidatedReportMetric_lenses,
    consolidatedReportMetric_lensesAppliedCount,
    consolidatedReportMetric_metricType,
    consolidatedReportMetric_riskCounts,
    consolidatedReportMetric_updatedAt,
    consolidatedReportMetric_workloadArn,
    consolidatedReportMetric_workloadId,
    consolidatedReportMetric_workloadName,

    -- ** ImprovementSummary
    improvementSummary_improvementPlanUrl,
    improvementSummary_improvementPlans,
    improvementSummary_pillarId,
    improvementSummary_questionId,
    improvementSummary_questionTitle,
    improvementSummary_risk,

    -- ** Lens
    lens_description,
    lens_lensArn,
    lens_lensVersion,
    lens_name,
    lens_owner,
    lens_shareInvitationId,
    lens_tags,

    -- ** LensMetric
    lensMetric_lensArn,
    lensMetric_pillars,
    lensMetric_riskCounts,

    -- ** LensReview
    lensReview_lensAlias,
    lensReview_lensArn,
    lensReview_lensName,
    lensReview_lensStatus,
    lensReview_lensVersion,
    lensReview_nextToken,
    lensReview_notes,
    lensReview_pillarReviewSummaries,
    lensReview_prioritizedRiskCounts,
    lensReview_profiles,
    lensReview_riskCounts,
    lensReview_updatedAt,

    -- ** LensReviewReport
    lensReviewReport_base64String,
    lensReviewReport_lensAlias,
    lensReviewReport_lensArn,

    -- ** LensReviewSummary
    lensReviewSummary_lensAlias,
    lensReviewSummary_lensArn,
    lensReviewSummary_lensName,
    lensReviewSummary_lensStatus,
    lensReviewSummary_lensVersion,
    lensReviewSummary_prioritizedRiskCounts,
    lensReviewSummary_profiles,
    lensReviewSummary_riskCounts,
    lensReviewSummary_updatedAt,

    -- ** LensShareSummary
    lensShareSummary_shareId,
    lensShareSummary_sharedWith,
    lensShareSummary_status,
    lensShareSummary_statusMessage,

    -- ** LensSummary
    lensSummary_createdAt,
    lensSummary_description,
    lensSummary_lensAlias,
    lensSummary_lensArn,
    lensSummary_lensName,
    lensSummary_lensStatus,
    lensSummary_lensType,
    lensSummary_lensVersion,
    lensSummary_owner,
    lensSummary_updatedAt,

    -- ** LensUpgradeSummary
    lensUpgradeSummary_currentLensVersion,
    lensUpgradeSummary_latestLensVersion,
    lensUpgradeSummary_lensAlias,
    lensUpgradeSummary_lensArn,
    lensUpgradeSummary_workloadId,
    lensUpgradeSummary_workloadName,

    -- ** Milestone
    milestone_milestoneName,
    milestone_milestoneNumber,
    milestone_recordedAt,
    milestone_workload,

    -- ** MilestoneSummary
    milestoneSummary_milestoneName,
    milestoneSummary_milestoneNumber,
    milestoneSummary_recordedAt,
    milestoneSummary_workloadSummary,

    -- ** NotificationSummary
    notificationSummary_lensUpgradeSummary,
    notificationSummary_type,

    -- ** PillarDifference
    pillarDifference_differenceStatus,
    pillarDifference_pillarId,
    pillarDifference_pillarName,
    pillarDifference_questionDifferences,

    -- ** PillarMetric
    pillarMetric_pillarId,
    pillarMetric_questions,
    pillarMetric_riskCounts,

    -- ** PillarReviewSummary
    pillarReviewSummary_notes,
    pillarReviewSummary_pillarId,
    pillarReviewSummary_pillarName,
    pillarReviewSummary_prioritizedRiskCounts,
    pillarReviewSummary_riskCounts,

    -- ** Profile
    profile_createdAt,
    profile_owner,
    profile_profileArn,
    profile_profileDescription,
    profile_profileName,
    profile_profileQuestions,
    profile_profileVersion,
    profile_shareInvitationId,
    profile_tags,
    profile_updatedAt,

    -- ** ProfileChoice
    profileChoice_choiceDescription,
    profileChoice_choiceId,
    profileChoice_choiceTitle,

    -- ** ProfileNotificationSummary
    profileNotificationSummary_currentProfileVersion,
    profileNotificationSummary_latestProfileVersion,
    profileNotificationSummary_profileArn,
    profileNotificationSummary_profileName,
    profileNotificationSummary_type,
    profileNotificationSummary_workloadId,
    profileNotificationSummary_workloadName,

    -- ** ProfileQuestion
    profileQuestion_maxSelectedChoices,
    profileQuestion_minSelectedChoices,
    profileQuestion_questionChoices,
    profileQuestion_questionDescription,
    profileQuestion_questionId,
    profileQuestion_questionTitle,
    profileQuestion_selectedChoiceIds,

    -- ** ProfileQuestionUpdate
    profileQuestionUpdate_questionId,
    profileQuestionUpdate_selectedChoiceIds,

    -- ** ProfileShareSummary
    profileShareSummary_shareId,
    profileShareSummary_sharedWith,
    profileShareSummary_status,
    profileShareSummary_statusMessage,

    -- ** ProfileSummary
    profileSummary_createdAt,
    profileSummary_owner,
    profileSummary_profileArn,
    profileSummary_profileDescription,
    profileSummary_profileName,
    profileSummary_profileVersion,
    profileSummary_updatedAt,

    -- ** ProfileTemplate
    profileTemplate_createdAt,
    profileTemplate_templateName,
    profileTemplate_templateQuestions,
    profileTemplate_updatedAt,

    -- ** ProfileTemplateChoice
    profileTemplateChoice_choiceDescription,
    profileTemplateChoice_choiceId,
    profileTemplateChoice_choiceTitle,

    -- ** ProfileTemplateQuestion
    profileTemplateQuestion_maxSelectedChoices,
    profileTemplateQuestion_minSelectedChoices,
    profileTemplateQuestion_questionChoices,
    profileTemplateQuestion_questionDescription,
    profileTemplateQuestion_questionId,
    profileTemplateQuestion_questionTitle,

    -- ** QuestionDifference
    questionDifference_differenceStatus,
    questionDifference_questionId,
    questionDifference_questionTitle,

    -- ** QuestionMetric
    questionMetric_bestPractices,
    questionMetric_questionId,
    questionMetric_risk,

    -- ** ShareInvitation
    shareInvitation_lensAlias,
    shareInvitation_lensArn,
    shareInvitation_profileArn,
    shareInvitation_shareInvitationId,
    shareInvitation_shareResourceType,
    shareInvitation_workloadId,

    -- ** ShareInvitationSummary
    shareInvitationSummary_lensArn,
    shareInvitationSummary_lensName,
    shareInvitationSummary_permissionType,
    shareInvitationSummary_profileArn,
    shareInvitationSummary_profileName,
    shareInvitationSummary_shareInvitationId,
    shareInvitationSummary_shareResourceType,
    shareInvitationSummary_sharedBy,
    shareInvitationSummary_sharedWith,
    shareInvitationSummary_workloadId,
    shareInvitationSummary_workloadName,

    -- ** VersionDifferences
    versionDifferences_pillarDifferences,

    -- ** Workload
    workload_accountIds,
    workload_applications,
    workload_architecturalDesign,
    workload_awsRegions,
    workload_description,
    workload_discoveryConfig,
    workload_environment,
    workload_improvementStatus,
    workload_industry,
    workload_industryType,
    workload_isReviewOwnerUpdateAcknowledged,
    workload_lenses,
    workload_nonAwsRegions,
    workload_notes,
    workload_owner,
    workload_pillarPriorities,
    workload_prioritizedRiskCounts,
    workload_profiles,
    workload_reviewOwner,
    workload_reviewRestrictionDate,
    workload_riskCounts,
    workload_shareInvitationId,
    workload_tags,
    workload_updatedAt,
    workload_workloadArn,
    workload_workloadId,
    workload_workloadName,

    -- ** WorkloadDiscoveryConfig
    workloadDiscoveryConfig_trustedAdvisorIntegrationStatus,
    workloadDiscoveryConfig_workloadResourceDefinition,

    -- ** WorkloadProfile
    workloadProfile_profileArn,
    workloadProfile_profileVersion,

    -- ** WorkloadShare
    workloadShare_permissionType,
    workloadShare_shareId,
    workloadShare_sharedBy,
    workloadShare_sharedWith,
    workloadShare_status,
    workloadShare_workloadId,
    workloadShare_workloadName,

    -- ** WorkloadShareSummary
    workloadShareSummary_permissionType,
    workloadShareSummary_shareId,
    workloadShareSummary_sharedWith,
    workloadShareSummary_status,
    workloadShareSummary_statusMessage,

    -- ** WorkloadSummary
    workloadSummary_improvementStatus,
    workloadSummary_lenses,
    workloadSummary_owner,
    workloadSummary_prioritizedRiskCounts,
    workloadSummary_profiles,
    workloadSummary_riskCounts,
    workloadSummary_updatedAt,
    workloadSummary_workloadArn,
    workloadSummary_workloadId,
    workloadSummary_workloadName,
  )
where

import Amazonka.WellArchitected.AssociateLenses
import Amazonka.WellArchitected.AssociateProfiles
import Amazonka.WellArchitected.CreateLensShare
import Amazonka.WellArchitected.CreateLensVersion
import Amazonka.WellArchitected.CreateMilestone
import Amazonka.WellArchitected.CreateProfile
import Amazonka.WellArchitected.CreateProfileShare
import Amazonka.WellArchitected.CreateWorkload
import Amazonka.WellArchitected.CreateWorkloadShare
import Amazonka.WellArchitected.DeleteLens
import Amazonka.WellArchitected.DeleteLensShare
import Amazonka.WellArchitected.DeleteProfile
import Amazonka.WellArchitected.DeleteProfileShare
import Amazonka.WellArchitected.DeleteWorkload
import Amazonka.WellArchitected.DeleteWorkloadShare
import Amazonka.WellArchitected.DisassociateLenses
import Amazonka.WellArchitected.DisassociateProfiles
import Amazonka.WellArchitected.ExportLens
import Amazonka.WellArchitected.GetAnswer
import Amazonka.WellArchitected.GetConsolidatedReport
import Amazonka.WellArchitected.GetLens
import Amazonka.WellArchitected.GetLensReview
import Amazonka.WellArchitected.GetLensReviewReport
import Amazonka.WellArchitected.GetLensVersionDifference
import Amazonka.WellArchitected.GetMilestone
import Amazonka.WellArchitected.GetProfile
import Amazonka.WellArchitected.GetProfileTemplate
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
import Amazonka.WellArchitected.ListProfileNotifications
import Amazonka.WellArchitected.ListProfileShares
import Amazonka.WellArchitected.ListProfiles
import Amazonka.WellArchitected.ListShareInvitations
import Amazonka.WellArchitected.ListTagsForResource
import Amazonka.WellArchitected.ListWorkloadShares
import Amazonka.WellArchitected.ListWorkloads
import Amazonka.WellArchitected.TagResource
import Amazonka.WellArchitected.Types.AdditionalResources
import Amazonka.WellArchitected.Types.Answer
import Amazonka.WellArchitected.Types.AnswerSummary
import Amazonka.WellArchitected.Types.BestPractice
import Amazonka.WellArchitected.Types.CheckDetail
import Amazonka.WellArchitected.Types.CheckSummary
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswer
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.ChoiceContent
import Amazonka.WellArchitected.Types.ChoiceImprovementPlan
import Amazonka.WellArchitected.Types.ChoiceUpdate
import Amazonka.WellArchitected.Types.ConsolidatedReportMetric
import Amazonka.WellArchitected.Types.ImprovementSummary
import Amazonka.WellArchitected.Types.Lens
import Amazonka.WellArchitected.Types.LensMetric
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
import Amazonka.WellArchitected.Types.PillarMetric
import Amazonka.WellArchitected.Types.PillarReviewSummary
import Amazonka.WellArchitected.Types.Profile
import Amazonka.WellArchitected.Types.ProfileChoice
import Amazonka.WellArchitected.Types.ProfileNotificationSummary
import Amazonka.WellArchitected.Types.ProfileQuestion
import Amazonka.WellArchitected.Types.ProfileQuestionUpdate
import Amazonka.WellArchitected.Types.ProfileShareSummary
import Amazonka.WellArchitected.Types.ProfileSummary
import Amazonka.WellArchitected.Types.ProfileTemplate
import Amazonka.WellArchitected.Types.ProfileTemplateChoice
import Amazonka.WellArchitected.Types.ProfileTemplateQuestion
import Amazonka.WellArchitected.Types.QuestionDifference
import Amazonka.WellArchitected.Types.QuestionMetric
import Amazonka.WellArchitected.Types.ShareInvitation
import Amazonka.WellArchitected.Types.ShareInvitationSummary
import Amazonka.WellArchitected.Types.VersionDifferences
import Amazonka.WellArchitected.Types.Workload
import Amazonka.WellArchitected.Types.WorkloadDiscoveryConfig
import Amazonka.WellArchitected.Types.WorkloadProfile
import Amazonka.WellArchitected.Types.WorkloadShare
import Amazonka.WellArchitected.Types.WorkloadShareSummary
import Amazonka.WellArchitected.Types.WorkloadSummary
import Amazonka.WellArchitected.UntagResource
import Amazonka.WellArchitected.UpdateAnswer
import Amazonka.WellArchitected.UpdateGlobalSettings
import Amazonka.WellArchitected.UpdateLensReview
import Amazonka.WellArchitected.UpdateProfile
import Amazonka.WellArchitected.UpdateShareInvitation
import Amazonka.WellArchitected.UpdateWorkload
import Amazonka.WellArchitected.UpdateWorkloadShare
import Amazonka.WellArchitected.UpgradeLensReview
import Amazonka.WellArchitected.UpgradeProfileVersion
