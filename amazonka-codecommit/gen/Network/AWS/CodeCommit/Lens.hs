{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Lens
  ( -- * Operations

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories
    batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName,
    batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors,

    -- ** PutFile
    putFile_parentCommitId,
    putFile_commitMessage,
    putFile_name,
    putFile_email,
    putFile_fileMode,
    putFile_repositoryName,
    putFile_branchName,
    putFile_fileContent,
    putFile_filePath,
    putFileResponse_httpStatus,
    putFileResponse_commitId,
    putFileResponse_blobId,
    putFileResponse_treeId,

    -- ** GetRepositoryTriggers
    getRepositoryTriggers_repositoryName,
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_httpStatus,

    -- ** DisassociateApprovalRuleTemplateFromRepository
    disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName,
    disassociateApprovalRuleTemplateFromRepository_repositoryName,

    -- ** DeletePullRequestApprovalRule
    deletePullRequestApprovalRule_pullRequestId,
    deletePullRequestApprovalRule_approvalRuleName,
    deletePullRequestApprovalRuleResponse_httpStatus,
    deletePullRequestApprovalRuleResponse_approvalRuleId,

    -- ** ListRepositoriesForApprovalRuleTemplate
    listRepositoriesForApprovalRuleTemplate_nextToken,
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName,
    listRepositoriesForApprovalRuleTemplateResponse_nextToken,
    listRepositoriesForApprovalRuleTemplateResponse_repositoryNames,
    listRepositoriesForApprovalRuleTemplateResponse_httpStatus,

    -- ** UpdateRepositoryName
    updateRepositoryName_oldName,
    updateRepositoryName_newName,

    -- ** BatchDescribeMergeConflicts
    batchDescribeMergeConflicts_maxMergeHunks,
    batchDescribeMergeConflicts_nextToken,
    batchDescribeMergeConflicts_maxConflictFiles,
    batchDescribeMergeConflicts_conflictDetailLevel,
    batchDescribeMergeConflicts_conflictResolutionStrategy,
    batchDescribeMergeConflicts_filePaths,
    batchDescribeMergeConflicts_repositoryName,
    batchDescribeMergeConflicts_destinationCommitSpecifier,
    batchDescribeMergeConflicts_sourceCommitSpecifier,
    batchDescribeMergeConflicts_mergeOption,
    batchDescribeMergeConflictsResponse_nextToken,
    batchDescribeMergeConflictsResponse_baseCommitId,
    batchDescribeMergeConflictsResponse_errors,
    batchDescribeMergeConflictsResponse_httpStatus,
    batchDescribeMergeConflictsResponse_conflicts,
    batchDescribeMergeConflictsResponse_destinationCommitId,
    batchDescribeMergeConflictsResponse_sourceCommitId,

    -- ** GetRepository
    getRepository_repositoryName,
    getRepositoryResponse_repositoryMetadata,
    getRepositoryResponse_httpStatus,

    -- ** UpdatePullRequestStatus
    updatePullRequestStatus_pullRequestId,
    updatePullRequestStatus_pullRequestStatus,
    updatePullRequestStatusResponse_httpStatus,
    updatePullRequestStatusResponse_pullRequest,

    -- ** PostCommentReply
    postCommentReply_clientRequestToken,
    postCommentReply_inReplyTo,
    postCommentReply_content,
    postCommentReplyResponse_comment,
    postCommentReplyResponse_httpStatus,

    -- ** GetPullRequestOverrideState
    getPullRequestOverrideState_pullRequestId,
    getPullRequestOverrideState_revisionId,
    getPullRequestOverrideStateResponse_overridden,
    getPullRequestOverrideStateResponse_overrider,
    getPullRequestOverrideStateResponse_httpStatus,

    -- ** GetCommentsForPullRequest
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_repositoryName,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_pullRequestId,
    getCommentsForPullRequestResponse_commentsForPullRequestData,
    getCommentsForPullRequestResponse_nextToken,
    getCommentsForPullRequestResponse_httpStatus,

    -- ** UpdateDefaultBranch
    updateDefaultBranch_repositoryName,
    updateDefaultBranch_defaultBranchName,

    -- ** BatchGetRepositories
    batchGetRepositories_repositoryNames,
    batchGetRepositoriesResponse_repositoriesNotFound,
    batchGetRepositoriesResponse_repositories,
    batchGetRepositoriesResponse_httpStatus,

    -- ** GetMergeOptions
    getMergeOptions_conflictDetailLevel,
    getMergeOptions_conflictResolutionStrategy,
    getMergeOptions_repositoryName,
    getMergeOptions_sourceCommitSpecifier,
    getMergeOptions_destinationCommitSpecifier,
    getMergeOptionsResponse_httpStatus,
    getMergeOptionsResponse_mergeOptions,
    getMergeOptionsResponse_sourceCommitId,
    getMergeOptionsResponse_destinationCommitId,
    getMergeOptionsResponse_baseCommitId,

    -- ** MergePullRequestByThreeWay
    mergePullRequestByThreeWay_commitMessage,
    mergePullRequestByThreeWay_authorName,
    mergePullRequestByThreeWay_email,
    mergePullRequestByThreeWay_sourceCommitId,
    mergePullRequestByThreeWay_conflictDetailLevel,
    mergePullRequestByThreeWay_conflictResolutionStrategy,
    mergePullRequestByThreeWay_keepEmptyFolders,
    mergePullRequestByThreeWay_conflictResolution,
    mergePullRequestByThreeWay_pullRequestId,
    mergePullRequestByThreeWay_repositoryName,
    mergePullRequestByThreeWayResponse_pullRequest,
    mergePullRequestByThreeWayResponse_httpStatus,

    -- ** UpdatePullRequestDescription
    updatePullRequestDescription_pullRequestId,
    updatePullRequestDescription_description,
    updatePullRequestDescriptionResponse_httpStatus,
    updatePullRequestDescriptionResponse_pullRequest,

    -- ** GetMergeConflicts
    getMergeConflicts_nextToken,
    getMergeConflicts_maxConflictFiles,
    getMergeConflicts_conflictDetailLevel,
    getMergeConflicts_conflictResolutionStrategy,
    getMergeConflicts_repositoryName,
    getMergeConflicts_destinationCommitSpecifier,
    getMergeConflicts_sourceCommitSpecifier,
    getMergeConflicts_mergeOption,
    getMergeConflictsResponse_nextToken,
    getMergeConflictsResponse_baseCommitId,
    getMergeConflictsResponse_httpStatus,
    getMergeConflictsResponse_mergeable,
    getMergeConflictsResponse_destinationCommitId,
    getMergeConflictsResponse_sourceCommitId,
    getMergeConflictsResponse_conflictMetadataList,

    -- ** PutCommentReaction
    putCommentReaction_commentId,
    putCommentReaction_reactionValue,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** DeleteApprovalRuleTemplate
    deleteApprovalRuleTemplate_approvalRuleTemplateName,
    deleteApprovalRuleTemplateResponse_httpStatus,
    deleteApprovalRuleTemplateResponse_approvalRuleTemplateId,

    -- ** ListBranches
    listBranches_nextToken,
    listBranches_repositoryName,
    listBranchesResponse_nextToken,
    listBranchesResponse_branches,
    listBranchesResponse_httpStatus,

    -- ** ListApprovalRuleTemplates
    listApprovalRuleTemplates_nextToken,
    listApprovalRuleTemplates_maxResults,
    listApprovalRuleTemplatesResponse_nextToken,
    listApprovalRuleTemplatesResponse_approvalRuleTemplateNames,
    listApprovalRuleTemplatesResponse_httpStatus,

    -- ** PutRepositoryTriggers
    putRepositoryTriggers_repositoryName,
    putRepositoryTriggers_triggers,
    putRepositoryTriggersResponse_configurationId,
    putRepositoryTriggersResponse_httpStatus,

    -- ** CreateBranch
    createBranch_repositoryName,
    createBranch_branchName,
    createBranch_commitId,

    -- ** MergeBranchesByThreeWay
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_conflictResolution,
    mergeBranchesByThreeWay_targetBranch,
    mergeBranchesByThreeWay_repositoryName,
    mergeBranchesByThreeWay_sourceCommitSpecifier,
    mergeBranchesByThreeWay_destinationCommitSpecifier,
    mergeBranchesByThreeWayResponse_commitId,
    mergeBranchesByThreeWayResponse_treeId,
    mergeBranchesByThreeWayResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** GetFile
    getFile_commitSpecifier,
    getFile_repositoryName,
    getFile_filePath,
    getFileResponse_httpStatus,
    getFileResponse_commitId,
    getFileResponse_blobId,
    getFileResponse_filePath,
    getFileResponse_fileMode,
    getFileResponse_fileSize,
    getFileResponse_fileContent,

    -- ** UpdatePullRequestApprovalRuleContent
    updatePullRequestApprovalRuleContent_existingRuleContentSha256,
    updatePullRequestApprovalRuleContent_pullRequestId,
    updatePullRequestApprovalRuleContent_approvalRuleName,
    updatePullRequestApprovalRuleContent_newRuleContent,
    updatePullRequestApprovalRuleContentResponse_httpStatus,
    updatePullRequestApprovalRuleContentResponse_approvalRule,

    -- ** UpdatePullRequestTitle
    updatePullRequestTitle_pullRequestId,
    updatePullRequestTitle_title,
    updatePullRequestTitleResponse_httpStatus,
    updatePullRequestTitleResponse_pullRequest,

    -- ** UpdateRepositoryDescription
    updateRepositoryDescription_repositoryDescription,
    updateRepositoryDescription_repositoryName,

    -- ** OverridePullRequestApprovalRules
    overridePullRequestApprovalRules_pullRequestId,
    overridePullRequestApprovalRules_revisionId,
    overridePullRequestApprovalRules_overrideStatus,

    -- ** GetPullRequest
    getPullRequest_pullRequestId,
    getPullRequestResponse_httpStatus,
    getPullRequestResponse_pullRequest,

    -- ** UpdateComment
    updateComment_commentId,
    updateComment_content,
    updateCommentResponse_comment,
    updateCommentResponse_httpStatus,

    -- ** GetDifferences
    getDifferences_nextToken,
    getDifferences_beforeCommitSpecifier,
    getDifferences_maxResults,
    getDifferences_beforePath,
    getDifferences_afterPath,
    getDifferences_repositoryName,
    getDifferences_afterCommitSpecifier,
    getDifferencesResponse_nextToken,
    getDifferencesResponse_differences,
    getDifferencesResponse_httpStatus,

    -- ** GetMergeCommit
    getMergeCommit_conflictDetailLevel,
    getMergeCommit_conflictResolutionStrategy,
    getMergeCommit_repositoryName,
    getMergeCommit_sourceCommitSpecifier,
    getMergeCommit_destinationCommitSpecifier,
    getMergeCommitResponse_baseCommitId,
    getMergeCommitResponse_mergedCommitId,
    getMergeCommitResponse_sourceCommitId,
    getMergeCommitResponse_destinationCommitId,
    getMergeCommitResponse_httpStatus,

    -- ** GetApprovalRuleTemplate
    getApprovalRuleTemplate_approvalRuleTemplateName,
    getApprovalRuleTemplateResponse_httpStatus,
    getApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** GetCommit
    getCommit_repositoryName,
    getCommit_commitId,
    getCommitResponse_httpStatus,
    getCommitResponse_commit,

    -- ** UpdateApprovalRuleTemplateName
    updateApprovalRuleTemplateName_oldApprovalRuleTemplateName,
    updateApprovalRuleTemplateName_newApprovalRuleTemplateName,
    updateApprovalRuleTemplateNameResponse_httpStatus,
    updateApprovalRuleTemplateNameResponse_approvalRuleTemplate,

    -- ** GetCommentReactions
    getCommentReactions_nextToken,
    getCommentReactions_maxResults,
    getCommentReactions_reactionUserArn,
    getCommentReactions_commentId,
    getCommentReactionsResponse_nextToken,
    getCommentReactionsResponse_httpStatus,
    getCommentReactionsResponse_reactionsForComment,

    -- ** TestRepositoryTriggers
    testRepositoryTriggers_repositoryName,
    testRepositoryTriggers_triggers,
    testRepositoryTriggersResponse_successfulExecutions,
    testRepositoryTriggersResponse_failedExecutions,
    testRepositoryTriggersResponse_httpStatus,

    -- ** DeleteFile
    deleteFile_commitMessage,
    deleteFile_name,
    deleteFile_email,
    deleteFile_keepEmptyFolders,
    deleteFile_repositoryName,
    deleteFile_branchName,
    deleteFile_filePath,
    deleteFile_parentCommitId,
    deleteFileResponse_httpStatus,
    deleteFileResponse_commitId,
    deleteFileResponse_blobId,
    deleteFileResponse_treeId,
    deleteFileResponse_filePath,

    -- ** GetCommentsForComparedCommit
    getCommentsForComparedCommit_nextToken,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_httpStatus,

    -- ** MergeBranchesBySquash
    mergeBranchesBySquash_commitMessage,
    mergeBranchesBySquash_authorName,
    mergeBranchesBySquash_email,
    mergeBranchesBySquash_conflictDetailLevel,
    mergeBranchesBySquash_conflictResolutionStrategy,
    mergeBranchesBySquash_keepEmptyFolders,
    mergeBranchesBySquash_conflictResolution,
    mergeBranchesBySquash_targetBranch,
    mergeBranchesBySquash_repositoryName,
    mergeBranchesBySquash_sourceCommitSpecifier,
    mergeBranchesBySquash_destinationCommitSpecifier,
    mergeBranchesBySquashResponse_commitId,
    mergeBranchesBySquashResponse_treeId,
    mergeBranchesBySquashResponse_httpStatus,

    -- ** PostCommentForPullRequest
    postCommentForPullRequest_clientRequestToken,
    postCommentForPullRequest_location,
    postCommentForPullRequest_pullRequestId,
    postCommentForPullRequest_repositoryName,
    postCommentForPullRequest_beforeCommitId,
    postCommentForPullRequest_afterCommitId,
    postCommentForPullRequest_content,
    postCommentForPullRequestResponse_beforeBlobId,
    postCommentForPullRequestResponse_comment,
    postCommentForPullRequestResponse_repositoryName,
    postCommentForPullRequestResponse_beforeCommitId,
    postCommentForPullRequestResponse_afterBlobId,
    postCommentForPullRequestResponse_pullRequestId,
    postCommentForPullRequestResponse_afterCommitId,
    postCommentForPullRequestResponse_location,
    postCommentForPullRequestResponse_httpStatus,

    -- ** MergePullRequestByFastForward
    mergePullRequestByFastForward_sourceCommitId,
    mergePullRequestByFastForward_pullRequestId,
    mergePullRequestByFastForward_repositoryName,
    mergePullRequestByFastForwardResponse_pullRequest,
    mergePullRequestByFastForwardResponse_httpStatus,

    -- ** CreatePullRequestApprovalRule
    createPullRequestApprovalRule_pullRequestId,
    createPullRequestApprovalRule_approvalRuleName,
    createPullRequestApprovalRule_approvalRuleContent,
    createPullRequestApprovalRuleResponse_httpStatus,
    createPullRequestApprovalRuleResponse_approvalRule,

    -- ** CreateUnreferencedMergeCommit
    createUnreferencedMergeCommit_commitMessage,
    createUnreferencedMergeCommit_authorName,
    createUnreferencedMergeCommit_email,
    createUnreferencedMergeCommit_conflictDetailLevel,
    createUnreferencedMergeCommit_conflictResolutionStrategy,
    createUnreferencedMergeCommit_keepEmptyFolders,
    createUnreferencedMergeCommit_conflictResolution,
    createUnreferencedMergeCommit_repositoryName,
    createUnreferencedMergeCommit_sourceCommitSpecifier,
    createUnreferencedMergeCommit_destinationCommitSpecifier,
    createUnreferencedMergeCommit_mergeOption,
    createUnreferencedMergeCommitResponse_commitId,
    createUnreferencedMergeCommitResponse_treeId,
    createUnreferencedMergeCommitResponse_httpStatus,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    listAssociatedApprovalRuleTemplatesForRepository_nextToken,
    listAssociatedApprovalRuleTemplatesForRepository_maxResults,
    listAssociatedApprovalRuleTemplatesForRepository_repositoryName,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus,

    -- ** GetPullRequestApprovalStates
    getPullRequestApprovalStates_pullRequestId,
    getPullRequestApprovalStates_revisionId,
    getPullRequestApprovalStatesResponse_approvals,
    getPullRequestApprovalStatesResponse_httpStatus,

    -- ** UpdateApprovalRuleTemplateContent
    updateApprovalRuleTemplateContent_existingRuleContentSha256,
    updateApprovalRuleTemplateContent_approvalRuleTemplateName,
    updateApprovalRuleTemplateContent_newRuleContent,
    updateApprovalRuleTemplateContentResponse_httpStatus,
    updateApprovalRuleTemplateContentResponse_approvalRuleTemplate,

    -- ** ListRepositories
    listRepositories_nextToken,
    listRepositories_sortBy,
    listRepositories_order,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,

    -- ** UpdateApprovalRuleTemplateDescription
    updateApprovalRuleTemplateDescription_approvalRuleTemplateName,
    updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription,
    updateApprovalRuleTemplateDescriptionResponse_httpStatus,
    updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate,

    -- ** CreateRepository
    createRepository_repositoryDescription,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repositoryMetadata,
    createRepositoryResponse_httpStatus,

    -- ** DescribePullRequestEvents
    describePullRequestEvents_nextToken,
    describePullRequestEvents_maxResults,
    describePullRequestEvents_pullRequestEventType,
    describePullRequestEvents_actorArn,
    describePullRequestEvents_pullRequestId,
    describePullRequestEventsResponse_nextToken,
    describePullRequestEventsResponse_httpStatus,
    describePullRequestEventsResponse_pullRequestEvents,

    -- ** DeleteCommentContent
    deleteCommentContent_commentId,
    deleteCommentContentResponse_comment,
    deleteCommentContentResponse_httpStatus,

    -- ** DeleteRepository
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repositoryId,
    deleteRepositoryResponse_httpStatus,

    -- ** DescribeMergeConflicts
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_nextToken,
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_conflictResolutionStrategy,
    describeMergeConflicts_repositoryName,
    describeMergeConflicts_destinationCommitSpecifier,
    describeMergeConflicts_sourceCommitSpecifier,
    describeMergeConflicts_mergeOption,
    describeMergeConflicts_filePath,
    describeMergeConflictsResponse_nextToken,
    describeMergeConflictsResponse_baseCommitId,
    describeMergeConflictsResponse_httpStatus,
    describeMergeConflictsResponse_conflictMetadata,
    describeMergeConflictsResponse_mergeHunks,
    describeMergeConflictsResponse_destinationCommitId,
    describeMergeConflictsResponse_sourceCommitId,

    -- ** BatchGetCommits
    batchGetCommits_commitIds,
    batchGetCommits_repositoryName,
    batchGetCommitsResponse_commits,
    batchGetCommitsResponse_errors,
    batchGetCommitsResponse_httpStatus,

    -- ** GetFolder
    getFolder_commitSpecifier,
    getFolder_repositoryName,
    getFolder_folderPath,
    getFolderResponse_symbolicLinks,
    getFolderResponse_subFolders,
    getFolderResponse_treeId,
    getFolderResponse_subModules,
    getFolderResponse_files,
    getFolderResponse_httpStatus,
    getFolderResponse_commitId,
    getFolderResponse_folderPath,

    -- ** CreatePullRequest
    createPullRequest_description,
    createPullRequest_clientRequestToken,
    createPullRequest_title,
    createPullRequest_targets,
    createPullRequestResponse_httpStatus,
    createPullRequestResponse_pullRequest,

    -- ** EvaluatePullRequestApprovalRules
    evaluatePullRequestApprovalRules_pullRequestId,
    evaluatePullRequestApprovalRules_revisionId,
    evaluatePullRequestApprovalRulesResponse_httpStatus,
    evaluatePullRequestApprovalRulesResponse_evaluation,

    -- ** UpdatePullRequestApprovalState
    updatePullRequestApprovalState_pullRequestId,
    updatePullRequestApprovalState_revisionId,
    updatePullRequestApprovalState_approvalState,

    -- ** CreateCommit
    createCommit_deleteFiles,
    createCommit_setFileModes,
    createCommit_parentCommitId,
    createCommit_commitMessage,
    createCommit_authorName,
    createCommit_putFiles,
    createCommit_email,
    createCommit_keepEmptyFolders,
    createCommit_repositoryName,
    createCommit_branchName,
    createCommitResponse_commitId,
    createCommitResponse_treeId,
    createCommitResponse_filesAdded,
    createCommitResponse_filesUpdated,
    createCommitResponse_filesDeleted,
    createCommitResponse_httpStatus,

    -- ** AssociateApprovalRuleTemplateWithRepository
    associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName,
    associateApprovalRuleTemplateWithRepository_repositoryName,

    -- ** GetBlob
    getBlob_repositoryName,
    getBlob_blobId,
    getBlobResponse_httpStatus,
    getBlobResponse_content,

    -- ** CreateApprovalRuleTemplate
    createApprovalRuleTemplate_approvalRuleTemplateDescription,
    createApprovalRuleTemplate_approvalRuleTemplateName,
    createApprovalRuleTemplate_approvalRuleTemplateContent,
    createApprovalRuleTemplateResponse_httpStatus,
    createApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** ListPullRequests
    listPullRequests_nextToken,
    listPullRequests_maxResults,
    listPullRequests_pullRequestStatus,
    listPullRequests_authorArn,
    listPullRequests_repositoryName,
    listPullRequestsResponse_nextToken,
    listPullRequestsResponse_httpStatus,
    listPullRequestsResponse_pullRequestIds,

    -- ** DeleteBranch
    deleteBranch_repositoryName,
    deleteBranch_branchName,
    deleteBranchResponse_deletedBranch,
    deleteBranchResponse_httpStatus,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories
    batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName,
    batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors,

    -- ** GetComment
    getComment_commentId,
    getCommentResponse_comment,
    getCommentResponse_httpStatus,

    -- ** GetBranch
    getBranch_branchName,
    getBranch_repositoryName,
    getBranchResponse_branch,
    getBranchResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** MergeBranchesByFastForward
    mergeBranchesByFastForward_targetBranch,
    mergeBranchesByFastForward_repositoryName,
    mergeBranchesByFastForward_sourceCommitSpecifier,
    mergeBranchesByFastForward_destinationCommitSpecifier,
    mergeBranchesByFastForwardResponse_commitId,
    mergeBranchesByFastForwardResponse_treeId,
    mergeBranchesByFastForwardResponse_httpStatus,

    -- ** PostCommentForComparedCommit
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_httpStatus,

    -- ** MergePullRequestBySquash
    mergePullRequestBySquash_commitMessage,
    mergePullRequestBySquash_authorName,
    mergePullRequestBySquash_email,
    mergePullRequestBySquash_sourceCommitId,
    mergePullRequestBySquash_conflictDetailLevel,
    mergePullRequestBySquash_conflictResolutionStrategy,
    mergePullRequestBySquash_keepEmptyFolders,
    mergePullRequestBySquash_conflictResolution,
    mergePullRequestBySquash_pullRequestId,
    mergePullRequestBySquash_repositoryName,
    mergePullRequestBySquashResponse_pullRequest,
    mergePullRequestBySquashResponse_httpStatus,

    -- * Types

    -- ** Approval
    approval_userArn,
    approval_approvalState,

    -- ** ApprovalRule
    approvalRule_lastModifiedDate,
    approvalRule_approvalRuleContent,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_creationDate,
    approvalRule_ruleContentSha256,
    approvalRule_approvalRuleId,
    approvalRule_approvalRuleName,
    approvalRule_lastModifiedUser,

    -- ** ApprovalRuleEventMetadata
    approvalRuleEventMetadata_approvalRuleContent,
    approvalRuleEventMetadata_approvalRuleId,
    approvalRuleEventMetadata_approvalRuleName,

    -- ** ApprovalRuleOverriddenEventMetadata
    approvalRuleOverriddenEventMetadata_revisionId,
    approvalRuleOverriddenEventMetadata_overrideStatus,

    -- ** ApprovalRuleTemplate
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_approvalRuleTemplateId,
    approvalRuleTemplate_approvalRuleTemplateName,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_ruleContentSha256,
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_lastModifiedUser,

    -- ** ApprovalStateChangedEventMetadata
    approvalStateChangedEventMetadata_revisionId,
    approvalStateChangedEventMetadata_approvalStatus,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode,

    -- ** BatchDescribeMergeConflictsError
    batchDescribeMergeConflictsError_filePath,
    batchDescribeMergeConflictsError_exceptionName,
    batchDescribeMergeConflictsError_message,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode,

    -- ** BatchGetCommitsError
    batchGetCommitsError_commitId,
    batchGetCommitsError_errorMessage,
    batchGetCommitsError_errorCode,

    -- ** BlobMetadata
    blobMetadata_mode,
    blobMetadata_blobId,
    blobMetadata_path,

    -- ** BranchInfo
    branchInfo_commitId,
    branchInfo_branchName,

    -- ** Comment
    comment_callerReactions,
    comment_lastModifiedDate,
    comment_creationDate,
    comment_reactionCounts,
    comment_content,
    comment_commentId,
    comment_inReplyTo,
    comment_clientRequestToken,
    comment_authorArn,
    comment_deleted,

    -- ** CommentsForComparedCommit
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_repositoryName,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_afterBlobId,
    commentsForComparedCommit_comments,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_location,

    -- ** CommentsForPullRequest
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_afterBlobId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_comments,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_location,

    -- ** Commit
    commit_parents,
    commit_commitId,
    commit_additionalData,
    commit_message,
    commit_treeId,
    commit_author,
    commit_committer,

    -- ** Conflict
    conflict_mergeHunks,
    conflict_conflictMetadata,

    -- ** ConflictMetadata
    conflictMetadata_mergeOperations,
    conflictMetadata_fileModeConflict,
    conflictMetadata_filePath,
    conflictMetadata_isBinaryFile,
    conflictMetadata_objectTypeConflict,
    conflictMetadata_numberOfConflicts,
    conflictMetadata_contentConflict,
    conflictMetadata_objectTypes,
    conflictMetadata_fileModes,
    conflictMetadata_fileSizes,

    -- ** ConflictResolution
    conflictResolution_deleteFiles,
    conflictResolution_setFileModes,
    conflictResolution_replaceContents,

    -- ** DeleteFileEntry
    deleteFileEntry_filePath,

    -- ** Difference
    difference_changeType,
    difference_afterBlob,
    difference_beforeBlob,

    -- ** Evaluation
    evaluation_overridden,
    evaluation_approvalRulesSatisfied,
    evaluation_approved,
    evaluation_approvalRulesNotSatisfied,

    -- ** File
    file_absolutePath,
    file_relativePath,
    file_blobId,
    file_fileMode,

    -- ** FileMetadata
    fileMetadata_absolutePath,
    fileMetadata_blobId,
    fileMetadata_fileMode,

    -- ** FileModes
    fileModes_source,
    fileModes_destination,
    fileModes_base,

    -- ** FileSizes
    fileSizes_source,
    fileSizes_destination,
    fileSizes_base,

    -- ** Folder
    folder_treeId,
    folder_absolutePath,
    folder_relativePath,

    -- ** IsBinaryFile
    isBinaryFile_source,
    isBinaryFile_destination,
    isBinaryFile_base,

    -- ** Location
    location_filePath,
    location_filePosition,
    location_relativeFileVersion,

    -- ** MergeHunk
    mergeHunk_source,
    mergeHunk_isConflict,
    mergeHunk_destination,
    mergeHunk_base,

    -- ** MergeHunkDetail
    mergeHunkDetail_hunkContent,
    mergeHunkDetail_startLine,
    mergeHunkDetail_endLine,

    -- ** MergeMetadata
    mergeMetadata_mergedBy,
    mergeMetadata_mergeCommitId,
    mergeMetadata_isMerged,
    mergeMetadata_mergeOption,

    -- ** MergeOperations
    mergeOperations_source,
    mergeOperations_destination,

    -- ** ObjectTypes
    objectTypes_source,
    objectTypes_destination,
    objectTypes_base,

    -- ** OriginApprovalRuleTemplate
    originApprovalRuleTemplate_approvalRuleTemplateId,
    originApprovalRuleTemplate_approvalRuleTemplateName,

    -- ** PullRequest
    pullRequest_revisionId,
    pullRequest_pullRequestTargets,
    pullRequest_title,
    pullRequest_pullRequestStatus,
    pullRequest_creationDate,
    pullRequest_pullRequestId,
    pullRequest_description,
    pullRequest_clientRequestToken,
    pullRequest_lastActivityDate,
    pullRequest_authorArn,
    pullRequest_approvalRules,

    -- ** PullRequestCreatedEventMetadata
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_sourceCommitId,
    pullRequestCreatedEventMetadata_destinationCommitId,
    pullRequestCreatedEventMetadata_mergeBase,

    -- ** PullRequestEvent
    pullRequestEvent_pullRequestMergedStateChangedEventMetadata,
    pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestEvent_approvalStateChangedEventMetadata,
    pullRequestEvent_pullRequestEventType,
    pullRequestEvent_eventDate,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,
    pullRequestEvent_actorArn,
    pullRequestEvent_pullRequestStatusChangedEventMetadata,
    pullRequestEvent_approvalRuleEventMetadata,

    -- ** PullRequestMergedStateChangedEventMetadata
    pullRequestMergedStateChangedEventMetadata_destinationReference,
    pullRequestMergedStateChangedEventMetadata_mergeMetadata,
    pullRequestMergedStateChangedEventMetadata_repositoryName,

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    pullRequestSourceReferenceUpdatedEventMetadata_repositoryName,
    pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_mergeBase,

    -- ** PullRequestStatusChangedEventMetadata
    pullRequestStatusChangedEventMetadata_pullRequestStatus,

    -- ** PullRequestTarget
    pullRequestTarget_destinationReference,
    pullRequestTarget_sourceCommit,
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_repositoryName,
    pullRequestTarget_sourceReference,
    pullRequestTarget_destinationCommit,
    pullRequestTarget_mergeBase,

    -- ** PutFileEntry
    putFileEntry_fileContent,
    putFileEntry_sourceFile,
    putFileEntry_fileMode,
    putFileEntry_filePath,

    -- ** ReactionForComment
    reactionForComment_reaction,
    reactionForComment_reactionUsers,
    reactionForComment_reactionsFromDeletedUsersCount,

    -- ** ReactionValueFormats
    reactionValueFormats_unicode,
    reactionValueFormats_shortCode,
    reactionValueFormats_emoji,

    -- ** ReplaceContentEntry
    replaceContentEntry_content,
    replaceContentEntry_fileMode,
    replaceContentEntry_filePath,
    replaceContentEntry_replacementType,

    -- ** RepositoryMetadata
    repositoryMetadata_lastModifiedDate,
    repositoryMetadata_defaultBranch,
    repositoryMetadata_accountId,
    repositoryMetadata_cloneUrlSsh,
    repositoryMetadata_cloneUrlHttp,
    repositoryMetadata_arn,
    repositoryMetadata_creationDate,
    repositoryMetadata_repositoryName,
    repositoryMetadata_repositoryId,
    repositoryMetadata_repositoryDescription,

    -- ** RepositoryNameIdPair
    repositoryNameIdPair_repositoryName,
    repositoryNameIdPair_repositoryId,

    -- ** RepositoryTrigger
    repositoryTrigger_customData,
    repositoryTrigger_branches,
    repositoryTrigger_name,
    repositoryTrigger_destinationArn,
    repositoryTrigger_events,

    -- ** RepositoryTriggerExecutionFailure
    repositoryTriggerExecutionFailure_failureMessage,
    repositoryTriggerExecutionFailure_trigger,

    -- ** SetFileModeEntry
    setFileModeEntry_filePath,
    setFileModeEntry_fileMode,

    -- ** SourceFileSpecifier
    sourceFileSpecifier_isMove,
    sourceFileSpecifier_filePath,

    -- ** SubModule
    subModule_commitId,
    subModule_absolutePath,
    subModule_relativePath,

    -- ** SymbolicLink
    symbolicLink_absolutePath,
    symbolicLink_relativePath,
    symbolicLink_blobId,
    symbolicLink_fileMode,

    -- ** Target
    target_destinationReference,
    target_repositoryName,
    target_sourceReference,

    -- ** UserInfo
    userInfo_name,
    userInfo_date,
    userInfo_email,
  )
where

import Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Network.AWS.CodeCommit.BatchDescribeMergeConflicts
import Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Network.AWS.CodeCommit.BatchGetCommits
import Network.AWS.CodeCommit.BatchGetRepositories
import Network.AWS.CodeCommit.CreateApprovalRuleTemplate
import Network.AWS.CodeCommit.CreateBranch
import Network.AWS.CodeCommit.CreateCommit
import Network.AWS.CodeCommit.CreatePullRequest
import Network.AWS.CodeCommit.CreatePullRequestApprovalRule
import Network.AWS.CodeCommit.CreateRepository
import Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
import Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
import Network.AWS.CodeCommit.DeleteBranch
import Network.AWS.CodeCommit.DeleteCommentContent
import Network.AWS.CodeCommit.DeleteFile
import Network.AWS.CodeCommit.DeletePullRequestApprovalRule
import Network.AWS.CodeCommit.DeleteRepository
import Network.AWS.CodeCommit.DescribeMergeConflicts
import Network.AWS.CodeCommit.DescribePullRequestEvents
import Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
import Network.AWS.CodeCommit.GetApprovalRuleTemplate
import Network.AWS.CodeCommit.GetBlob
import Network.AWS.CodeCommit.GetBranch
import Network.AWS.CodeCommit.GetComment
import Network.AWS.CodeCommit.GetCommentReactions
import Network.AWS.CodeCommit.GetCommentsForComparedCommit
import Network.AWS.CodeCommit.GetCommentsForPullRequest
import Network.AWS.CodeCommit.GetCommit
import Network.AWS.CodeCommit.GetDifferences
import Network.AWS.CodeCommit.GetFile
import Network.AWS.CodeCommit.GetFolder
import Network.AWS.CodeCommit.GetMergeCommit
import Network.AWS.CodeCommit.GetMergeConflicts
import Network.AWS.CodeCommit.GetMergeOptions
import Network.AWS.CodeCommit.GetPullRequest
import Network.AWS.CodeCommit.GetPullRequestApprovalStates
import Network.AWS.CodeCommit.GetPullRequestOverrideState
import Network.AWS.CodeCommit.GetRepository
import Network.AWS.CodeCommit.GetRepositoryTriggers
import Network.AWS.CodeCommit.ListApprovalRuleTemplates
import Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import Network.AWS.CodeCommit.ListBranches
import Network.AWS.CodeCommit.ListPullRequests
import Network.AWS.CodeCommit.ListRepositories
import Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Network.AWS.CodeCommit.ListTagsForResource
import Network.AWS.CodeCommit.MergeBranchesByFastForward
import Network.AWS.CodeCommit.MergeBranchesBySquash
import Network.AWS.CodeCommit.MergeBranchesByThreeWay
import Network.AWS.CodeCommit.MergePullRequestByFastForward
import Network.AWS.CodeCommit.MergePullRequestBySquash
import Network.AWS.CodeCommit.MergePullRequestByThreeWay
import Network.AWS.CodeCommit.OverridePullRequestApprovalRules
import Network.AWS.CodeCommit.PostCommentForComparedCommit
import Network.AWS.CodeCommit.PostCommentForPullRequest
import Network.AWS.CodeCommit.PostCommentReply
import Network.AWS.CodeCommit.PutCommentReaction
import Network.AWS.CodeCommit.PutFile
import Network.AWS.CodeCommit.PutRepositoryTriggers
import Network.AWS.CodeCommit.TagResource
import Network.AWS.CodeCommit.TestRepositoryTriggers
import Network.AWS.CodeCommit.Types.Approval
import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
import Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Network.AWS.CodeCommit.Types.BatchGetCommitsError
import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.BranchInfo
import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.CommentsForComparedCommit
import Network.AWS.CodeCommit.Types.CommentsForPullRequest
import Network.AWS.CodeCommit.Types.Commit
import Network.AWS.CodeCommit.Types.Conflict
import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.ConflictResolution
import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.Difference
import Network.AWS.CodeCommit.Types.Evaluation
import Network.AWS.CodeCommit.Types.File
import Network.AWS.CodeCommit.Types.FileMetadata
import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.Folder
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.Location
import Network.AWS.CodeCommit.Types.MergeHunk
import Network.AWS.CodeCommit.Types.MergeHunkDetail
import Network.AWS.CodeCommit.Types.MergeMetadata
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.ObjectTypes
import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.PullRequest
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEvent
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestTarget
import Network.AWS.CodeCommit.Types.PutFileEntry
import Network.AWS.CodeCommit.Types.ReactionForComment
import Network.AWS.CodeCommit.Types.ReactionValueFormats
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.RepositoryMetadata
import Network.AWS.CodeCommit.Types.RepositoryNameIdPair
import Network.AWS.CodeCommit.Types.RepositoryTrigger
import Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import Network.AWS.CodeCommit.Types.SubModule
import Network.AWS.CodeCommit.Types.SymbolicLink
import Network.AWS.CodeCommit.Types.Target
import Network.AWS.CodeCommit.Types.UserInfo
import Network.AWS.CodeCommit.UntagResource
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
import Network.AWS.CodeCommit.UpdateComment
import Network.AWS.CodeCommit.UpdateDefaultBranch
import Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
import Network.AWS.CodeCommit.UpdatePullRequestApprovalState
import Network.AWS.CodeCommit.UpdatePullRequestDescription
import Network.AWS.CodeCommit.UpdatePullRequestStatus
import Network.AWS.CodeCommit.UpdatePullRequestTitle
import Network.AWS.CodeCommit.UpdateRepositoryDescription
import Network.AWS.CodeCommit.UpdateRepositoryName
