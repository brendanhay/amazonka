{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Lens
  ( -- * Operations

    -- ** AssociateApprovalRuleTemplateWithRepository
    associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName,
    associateApprovalRuleTemplateWithRepository_repositoryName,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories
    batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName,
    batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors,

    -- ** BatchDescribeMergeConflicts
    batchDescribeMergeConflicts_nextToken,
    batchDescribeMergeConflicts_filePaths,
    batchDescribeMergeConflicts_maxMergeHunks,
    batchDescribeMergeConflicts_conflictResolutionStrategy,
    batchDescribeMergeConflicts_maxConflictFiles,
    batchDescribeMergeConflicts_conflictDetailLevel,
    batchDescribeMergeConflicts_repositoryName,
    batchDescribeMergeConflicts_destinationCommitSpecifier,
    batchDescribeMergeConflicts_sourceCommitSpecifier,
    batchDescribeMergeConflicts_mergeOption,
    batchDescribeMergeConflictsResponse_baseCommitId,
    batchDescribeMergeConflictsResponse_nextToken,
    batchDescribeMergeConflictsResponse_errors,
    batchDescribeMergeConflictsResponse_httpStatus,
    batchDescribeMergeConflictsResponse_conflicts,
    batchDescribeMergeConflictsResponse_destinationCommitId,
    batchDescribeMergeConflictsResponse_sourceCommitId,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories
    batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName,
    batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors,

    -- ** BatchGetCommits
    batchGetCommits_commitIds,
    batchGetCommits_repositoryName,
    batchGetCommitsResponse_commits,
    batchGetCommitsResponse_errors,
    batchGetCommitsResponse_httpStatus,

    -- ** BatchGetRepositories
    batchGetRepositories_repositoryNames,
    batchGetRepositoriesResponse_repositories,
    batchGetRepositoriesResponse_repositoriesNotFound,
    batchGetRepositoriesResponse_httpStatus,

    -- ** CreateApprovalRuleTemplate
    createApprovalRuleTemplate_approvalRuleTemplateDescription,
    createApprovalRuleTemplate_approvalRuleTemplateName,
    createApprovalRuleTemplate_approvalRuleTemplateContent,
    createApprovalRuleTemplateResponse_httpStatus,
    createApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** CreateBranch
    createBranch_repositoryName,
    createBranch_branchName,
    createBranch_commitId,

    -- ** CreateCommit
    createCommit_parentCommitId,
    createCommit_keepEmptyFolders,
    createCommit_email,
    createCommit_authorName,
    createCommit_setFileModes,
    createCommit_deleteFiles,
    createCommit_putFiles,
    createCommit_commitMessage,
    createCommit_repositoryName,
    createCommit_branchName,
    createCommitResponse_commitId,
    createCommitResponse_filesAdded,
    createCommitResponse_treeId,
    createCommitResponse_filesUpdated,
    createCommitResponse_filesDeleted,
    createCommitResponse_httpStatus,

    -- ** CreatePullRequest
    createPullRequest_clientRequestToken,
    createPullRequest_description,
    createPullRequest_title,
    createPullRequest_targets,
    createPullRequestResponse_httpStatus,
    createPullRequestResponse_pullRequest,

    -- ** CreatePullRequestApprovalRule
    createPullRequestApprovalRule_pullRequestId,
    createPullRequestApprovalRule_approvalRuleName,
    createPullRequestApprovalRule_approvalRuleContent,
    createPullRequestApprovalRuleResponse_httpStatus,
    createPullRequestApprovalRuleResponse_approvalRule,

    -- ** CreateRepository
    createRepository_tags,
    createRepository_repositoryDescription,
    createRepository_repositoryName,
    createRepositoryResponse_repositoryMetadata,
    createRepositoryResponse_httpStatus,

    -- ** CreateUnreferencedMergeCommit
    createUnreferencedMergeCommit_keepEmptyFolders,
    createUnreferencedMergeCommit_conflictResolution,
    createUnreferencedMergeCommit_email,
    createUnreferencedMergeCommit_authorName,
    createUnreferencedMergeCommit_commitMessage,
    createUnreferencedMergeCommit_conflictResolutionStrategy,
    createUnreferencedMergeCommit_conflictDetailLevel,
    createUnreferencedMergeCommit_repositoryName,
    createUnreferencedMergeCommit_sourceCommitSpecifier,
    createUnreferencedMergeCommit_destinationCommitSpecifier,
    createUnreferencedMergeCommit_mergeOption,
    createUnreferencedMergeCommitResponse_commitId,
    createUnreferencedMergeCommitResponse_treeId,
    createUnreferencedMergeCommitResponse_httpStatus,

    -- ** DeleteApprovalRuleTemplate
    deleteApprovalRuleTemplate_approvalRuleTemplateName,
    deleteApprovalRuleTemplateResponse_httpStatus,
    deleteApprovalRuleTemplateResponse_approvalRuleTemplateId,

    -- ** DeleteBranch
    deleteBranch_repositoryName,
    deleteBranch_branchName,
    deleteBranchResponse_deletedBranch,
    deleteBranchResponse_httpStatus,

    -- ** DeleteCommentContent
    deleteCommentContent_commentId,
    deleteCommentContentResponse_comment,
    deleteCommentContentResponse_httpStatus,

    -- ** DeleteFile
    deleteFile_keepEmptyFolders,
    deleteFile_name,
    deleteFile_email,
    deleteFile_commitMessage,
    deleteFile_repositoryName,
    deleteFile_branchName,
    deleteFile_filePath,
    deleteFile_parentCommitId,
    deleteFileResponse_httpStatus,
    deleteFileResponse_commitId,
    deleteFileResponse_blobId,
    deleteFileResponse_treeId,
    deleteFileResponse_filePath,

    -- ** DeletePullRequestApprovalRule
    deletePullRequestApprovalRule_pullRequestId,
    deletePullRequestApprovalRule_approvalRuleName,
    deletePullRequestApprovalRuleResponse_httpStatus,
    deletePullRequestApprovalRuleResponse_approvalRuleId,

    -- ** DeleteRepository
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repositoryId,
    deleteRepositoryResponse_httpStatus,

    -- ** DescribeMergeConflicts
    describeMergeConflicts_nextToken,
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_conflictResolutionStrategy,
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_repositoryName,
    describeMergeConflicts_destinationCommitSpecifier,
    describeMergeConflicts_sourceCommitSpecifier,
    describeMergeConflicts_mergeOption,
    describeMergeConflicts_filePath,
    describeMergeConflictsResponse_baseCommitId,
    describeMergeConflictsResponse_nextToken,
    describeMergeConflictsResponse_httpStatus,
    describeMergeConflictsResponse_conflictMetadata,
    describeMergeConflictsResponse_mergeHunks,
    describeMergeConflictsResponse_destinationCommitId,
    describeMergeConflictsResponse_sourceCommitId,

    -- ** DescribePullRequestEvents
    describePullRequestEvents_nextToken,
    describePullRequestEvents_maxResults,
    describePullRequestEvents_actorArn,
    describePullRequestEvents_pullRequestEventType,
    describePullRequestEvents_pullRequestId,
    describePullRequestEventsResponse_nextToken,
    describePullRequestEventsResponse_httpStatus,
    describePullRequestEventsResponse_pullRequestEvents,

    -- ** DisassociateApprovalRuleTemplateFromRepository
    disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName,
    disassociateApprovalRuleTemplateFromRepository_repositoryName,

    -- ** EvaluatePullRequestApprovalRules
    evaluatePullRequestApprovalRules_pullRequestId,
    evaluatePullRequestApprovalRules_revisionId,
    evaluatePullRequestApprovalRulesResponse_httpStatus,
    evaluatePullRequestApprovalRulesResponse_evaluation,

    -- ** GetApprovalRuleTemplate
    getApprovalRuleTemplate_approvalRuleTemplateName,
    getApprovalRuleTemplateResponse_httpStatus,
    getApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** GetBlob
    getBlob_repositoryName,
    getBlob_blobId,
    getBlobResponse_httpStatus,
    getBlobResponse_content,

    -- ** GetBranch
    getBranch_branchName,
    getBranch_repositoryName,
    getBranchResponse_branch,
    getBranchResponse_httpStatus,

    -- ** GetComment
    getComment_commentId,
    getCommentResponse_comment,
    getCommentResponse_httpStatus,

    -- ** GetCommentReactions
    getCommentReactions_nextToken,
    getCommentReactions_maxResults,
    getCommentReactions_reactionUserArn,
    getCommentReactions_commentId,
    getCommentReactionsResponse_nextToken,
    getCommentReactionsResponse_httpStatus,
    getCommentReactionsResponse_reactionsForComment,

    -- ** GetCommentsForComparedCommit
    getCommentsForComparedCommit_nextToken,
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_httpStatus,

    -- ** GetCommentsForPullRequest
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_repositoryName,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_pullRequestId,
    getCommentsForPullRequestResponse_commentsForPullRequestData,
    getCommentsForPullRequestResponse_nextToken,
    getCommentsForPullRequestResponse_httpStatus,

    -- ** GetCommit
    getCommit_repositoryName,
    getCommit_commitId,
    getCommitResponse_httpStatus,
    getCommitResponse_commit,

    -- ** GetDifferences
    getDifferences_nextToken,
    getDifferences_afterPath,
    getDifferences_maxResults,
    getDifferences_beforePath,
    getDifferences_beforeCommitSpecifier,
    getDifferences_repositoryName,
    getDifferences_afterCommitSpecifier,
    getDifferencesResponse_nextToken,
    getDifferencesResponse_differences,
    getDifferencesResponse_httpStatus,

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

    -- ** GetFolder
    getFolder_commitSpecifier,
    getFolder_repositoryName,
    getFolder_folderPath,
    getFolderResponse_files,
    getFolderResponse_symbolicLinks,
    getFolderResponse_subFolders,
    getFolderResponse_treeId,
    getFolderResponse_subModules,
    getFolderResponse_httpStatus,
    getFolderResponse_commitId,
    getFolderResponse_folderPath,

    -- ** GetMergeCommit
    getMergeCommit_conflictResolutionStrategy,
    getMergeCommit_conflictDetailLevel,
    getMergeCommit_repositoryName,
    getMergeCommit_sourceCommitSpecifier,
    getMergeCommit_destinationCommitSpecifier,
    getMergeCommitResponse_baseCommitId,
    getMergeCommitResponse_mergedCommitId,
    getMergeCommitResponse_sourceCommitId,
    getMergeCommitResponse_destinationCommitId,
    getMergeCommitResponse_httpStatus,

    -- ** GetMergeConflicts
    getMergeConflicts_nextToken,
    getMergeConflicts_conflictResolutionStrategy,
    getMergeConflicts_maxConflictFiles,
    getMergeConflicts_conflictDetailLevel,
    getMergeConflicts_repositoryName,
    getMergeConflicts_destinationCommitSpecifier,
    getMergeConflicts_sourceCommitSpecifier,
    getMergeConflicts_mergeOption,
    getMergeConflictsResponse_baseCommitId,
    getMergeConflictsResponse_nextToken,
    getMergeConflictsResponse_httpStatus,
    getMergeConflictsResponse_mergeable,
    getMergeConflictsResponse_destinationCommitId,
    getMergeConflictsResponse_sourceCommitId,
    getMergeConflictsResponse_conflictMetadataList,

    -- ** GetMergeOptions
    getMergeOptions_conflictResolutionStrategy,
    getMergeOptions_conflictDetailLevel,
    getMergeOptions_repositoryName,
    getMergeOptions_sourceCommitSpecifier,
    getMergeOptions_destinationCommitSpecifier,
    getMergeOptionsResponse_httpStatus,
    getMergeOptionsResponse_mergeOptions,
    getMergeOptionsResponse_sourceCommitId,
    getMergeOptionsResponse_destinationCommitId,
    getMergeOptionsResponse_baseCommitId,

    -- ** GetPullRequest
    getPullRequest_pullRequestId,
    getPullRequestResponse_httpStatus,
    getPullRequestResponse_pullRequest,

    -- ** GetPullRequestApprovalStates
    getPullRequestApprovalStates_pullRequestId,
    getPullRequestApprovalStates_revisionId,
    getPullRequestApprovalStatesResponse_approvals,
    getPullRequestApprovalStatesResponse_httpStatus,

    -- ** GetPullRequestOverrideState
    getPullRequestOverrideState_pullRequestId,
    getPullRequestOverrideState_revisionId,
    getPullRequestOverrideStateResponse_overridden,
    getPullRequestOverrideStateResponse_overrider,
    getPullRequestOverrideStateResponse_httpStatus,

    -- ** GetRepository
    getRepository_repositoryName,
    getRepositoryResponse_repositoryMetadata,
    getRepositoryResponse_httpStatus,

    -- ** GetRepositoryTriggers
    getRepositoryTriggers_repositoryName,
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_httpStatus,

    -- ** ListApprovalRuleTemplates
    listApprovalRuleTemplates_nextToken,
    listApprovalRuleTemplates_maxResults,
    listApprovalRuleTemplatesResponse_nextToken,
    listApprovalRuleTemplatesResponse_approvalRuleTemplateNames,
    listApprovalRuleTemplatesResponse_httpStatus,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    listAssociatedApprovalRuleTemplatesForRepository_nextToken,
    listAssociatedApprovalRuleTemplatesForRepository_maxResults,
    listAssociatedApprovalRuleTemplatesForRepository_repositoryName,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus,

    -- ** ListBranches
    listBranches_nextToken,
    listBranches_repositoryName,
    listBranchesResponse_nextToken,
    listBranchesResponse_branches,
    listBranchesResponse_httpStatus,

    -- ** ListPullRequests
    listPullRequests_nextToken,
    listPullRequests_pullRequestStatus,
    listPullRequests_maxResults,
    listPullRequests_authorArn,
    listPullRequests_repositoryName,
    listPullRequestsResponse_nextToken,
    listPullRequestsResponse_httpStatus,
    listPullRequestsResponse_pullRequestIds,

    -- ** ListRepositories
    listRepositories_nextToken,
    listRepositories_sortBy,
    listRepositories_order,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,

    -- ** ListRepositoriesForApprovalRuleTemplate
    listRepositoriesForApprovalRuleTemplate_nextToken,
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName,
    listRepositoriesForApprovalRuleTemplateResponse_nextToken,
    listRepositoriesForApprovalRuleTemplateResponse_repositoryNames,
    listRepositoriesForApprovalRuleTemplateResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_httpStatus,

    -- ** MergeBranchesByFastForward
    mergeBranchesByFastForward_targetBranch,
    mergeBranchesByFastForward_repositoryName,
    mergeBranchesByFastForward_sourceCommitSpecifier,
    mergeBranchesByFastForward_destinationCommitSpecifier,
    mergeBranchesByFastForwardResponse_commitId,
    mergeBranchesByFastForwardResponse_treeId,
    mergeBranchesByFastForwardResponse_httpStatus,

    -- ** MergeBranchesBySquash
    mergeBranchesBySquash_keepEmptyFolders,
    mergeBranchesBySquash_targetBranch,
    mergeBranchesBySquash_conflictResolution,
    mergeBranchesBySquash_email,
    mergeBranchesBySquash_authorName,
    mergeBranchesBySquash_commitMessage,
    mergeBranchesBySquash_conflictResolutionStrategy,
    mergeBranchesBySquash_conflictDetailLevel,
    mergeBranchesBySquash_repositoryName,
    mergeBranchesBySquash_sourceCommitSpecifier,
    mergeBranchesBySquash_destinationCommitSpecifier,
    mergeBranchesBySquashResponse_commitId,
    mergeBranchesBySquashResponse_treeId,
    mergeBranchesBySquashResponse_httpStatus,

    -- ** MergeBranchesByThreeWay
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_targetBranch,
    mergeBranchesByThreeWay_conflictResolution,
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_repositoryName,
    mergeBranchesByThreeWay_sourceCommitSpecifier,
    mergeBranchesByThreeWay_destinationCommitSpecifier,
    mergeBranchesByThreeWayResponse_commitId,
    mergeBranchesByThreeWayResponse_treeId,
    mergeBranchesByThreeWayResponse_httpStatus,

    -- ** MergePullRequestByFastForward
    mergePullRequestByFastForward_sourceCommitId,
    mergePullRequestByFastForward_pullRequestId,
    mergePullRequestByFastForward_repositoryName,
    mergePullRequestByFastForwardResponse_pullRequest,
    mergePullRequestByFastForwardResponse_httpStatus,

    -- ** MergePullRequestBySquash
    mergePullRequestBySquash_keepEmptyFolders,
    mergePullRequestBySquash_conflictResolution,
    mergePullRequestBySquash_email,
    mergePullRequestBySquash_sourceCommitId,
    mergePullRequestBySquash_authorName,
    mergePullRequestBySquash_commitMessage,
    mergePullRequestBySquash_conflictResolutionStrategy,
    mergePullRequestBySquash_conflictDetailLevel,
    mergePullRequestBySquash_pullRequestId,
    mergePullRequestBySquash_repositoryName,
    mergePullRequestBySquashResponse_pullRequest,
    mergePullRequestBySquashResponse_httpStatus,

    -- ** MergePullRequestByThreeWay
    mergePullRequestByThreeWay_keepEmptyFolders,
    mergePullRequestByThreeWay_conflictResolution,
    mergePullRequestByThreeWay_email,
    mergePullRequestByThreeWay_sourceCommitId,
    mergePullRequestByThreeWay_authorName,
    mergePullRequestByThreeWay_commitMessage,
    mergePullRequestByThreeWay_conflictResolutionStrategy,
    mergePullRequestByThreeWay_conflictDetailLevel,
    mergePullRequestByThreeWay_pullRequestId,
    mergePullRequestByThreeWay_repositoryName,
    mergePullRequestByThreeWayResponse_pullRequest,
    mergePullRequestByThreeWayResponse_httpStatus,

    -- ** OverridePullRequestApprovalRules
    overridePullRequestApprovalRules_pullRequestId,
    overridePullRequestApprovalRules_revisionId,
    overridePullRequestApprovalRules_overrideStatus,

    -- ** PostCommentForComparedCommit
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_httpStatus,

    -- ** PostCommentForPullRequest
    postCommentForPullRequest_clientRequestToken,
    postCommentForPullRequest_location,
    postCommentForPullRequest_pullRequestId,
    postCommentForPullRequest_repositoryName,
    postCommentForPullRequest_beforeCommitId,
    postCommentForPullRequest_afterCommitId,
    postCommentForPullRequest_content,
    postCommentForPullRequestResponse_beforeBlobId,
    postCommentForPullRequestResponse_afterCommitId,
    postCommentForPullRequestResponse_pullRequestId,
    postCommentForPullRequestResponse_repositoryName,
    postCommentForPullRequestResponse_beforeCommitId,
    postCommentForPullRequestResponse_location,
    postCommentForPullRequestResponse_comment,
    postCommentForPullRequestResponse_afterBlobId,
    postCommentForPullRequestResponse_httpStatus,

    -- ** PostCommentReply
    postCommentReply_clientRequestToken,
    postCommentReply_inReplyTo,
    postCommentReply_content,
    postCommentReplyResponse_comment,
    postCommentReplyResponse_httpStatus,

    -- ** PutCommentReaction
    putCommentReaction_commentId,
    putCommentReaction_reactionValue,

    -- ** PutFile
    putFile_fileMode,
    putFile_parentCommitId,
    putFile_name,
    putFile_email,
    putFile_commitMessage,
    putFile_repositoryName,
    putFile_branchName,
    putFile_fileContent,
    putFile_filePath,
    putFileResponse_httpStatus,
    putFileResponse_commitId,
    putFileResponse_blobId,
    putFileResponse_treeId,

    -- ** PutRepositoryTriggers
    putRepositoryTriggers_repositoryName,
    putRepositoryTriggers_triggers,
    putRepositoryTriggersResponse_configurationId,
    putRepositoryTriggersResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** TestRepositoryTriggers
    testRepositoryTriggers_repositoryName,
    testRepositoryTriggers_triggers,
    testRepositoryTriggersResponse_failedExecutions,
    testRepositoryTriggersResponse_successfulExecutions,
    testRepositoryTriggersResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** UpdateApprovalRuleTemplateContent
    updateApprovalRuleTemplateContent_existingRuleContentSha256,
    updateApprovalRuleTemplateContent_approvalRuleTemplateName,
    updateApprovalRuleTemplateContent_newRuleContent,
    updateApprovalRuleTemplateContentResponse_httpStatus,
    updateApprovalRuleTemplateContentResponse_approvalRuleTemplate,

    -- ** UpdateApprovalRuleTemplateDescription
    updateApprovalRuleTemplateDescription_approvalRuleTemplateName,
    updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription,
    updateApprovalRuleTemplateDescriptionResponse_httpStatus,
    updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate,

    -- ** UpdateApprovalRuleTemplateName
    updateApprovalRuleTemplateName_oldApprovalRuleTemplateName,
    updateApprovalRuleTemplateName_newApprovalRuleTemplateName,
    updateApprovalRuleTemplateNameResponse_httpStatus,
    updateApprovalRuleTemplateNameResponse_approvalRuleTemplate,

    -- ** UpdateComment
    updateComment_commentId,
    updateComment_content,
    updateCommentResponse_comment,
    updateCommentResponse_httpStatus,

    -- ** UpdateDefaultBranch
    updateDefaultBranch_repositoryName,
    updateDefaultBranch_defaultBranchName,

    -- ** UpdatePullRequestApprovalRuleContent
    updatePullRequestApprovalRuleContent_existingRuleContentSha256,
    updatePullRequestApprovalRuleContent_pullRequestId,
    updatePullRequestApprovalRuleContent_approvalRuleName,
    updatePullRequestApprovalRuleContent_newRuleContent,
    updatePullRequestApprovalRuleContentResponse_httpStatus,
    updatePullRequestApprovalRuleContentResponse_approvalRule,

    -- ** UpdatePullRequestApprovalState
    updatePullRequestApprovalState_pullRequestId,
    updatePullRequestApprovalState_revisionId,
    updatePullRequestApprovalState_approvalState,

    -- ** UpdatePullRequestDescription
    updatePullRequestDescription_pullRequestId,
    updatePullRequestDescription_description,
    updatePullRequestDescriptionResponse_httpStatus,
    updatePullRequestDescriptionResponse_pullRequest,

    -- ** UpdatePullRequestStatus
    updatePullRequestStatus_pullRequestId,
    updatePullRequestStatus_pullRequestStatus,
    updatePullRequestStatusResponse_httpStatus,
    updatePullRequestStatusResponse_pullRequest,

    -- ** UpdatePullRequestTitle
    updatePullRequestTitle_pullRequestId,
    updatePullRequestTitle_title,
    updatePullRequestTitleResponse_httpStatus,
    updatePullRequestTitleResponse_pullRequest,

    -- ** UpdateRepositoryDescription
    updateRepositoryDescription_repositoryDescription,
    updateRepositoryDescription_repositoryName,

    -- ** UpdateRepositoryName
    updateRepositoryName_oldName,
    updateRepositoryName_newName,

    -- * Types

    -- ** Approval
    approval_approvalState,
    approval_userArn,

    -- ** ApprovalRule
    approvalRule_lastModifiedUser,
    approvalRule_lastModifiedDate,
    approvalRule_creationDate,
    approvalRule_approvalRuleName,
    approvalRule_ruleContentSha256,
    approvalRule_approvalRuleContent,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_approvalRuleId,

    -- ** ApprovalRuleEventMetadata
    approvalRuleEventMetadata_approvalRuleName,
    approvalRuleEventMetadata_approvalRuleContent,
    approvalRuleEventMetadata_approvalRuleId,

    -- ** ApprovalRuleOverriddenEventMetadata
    approvalRuleOverriddenEventMetadata_revisionId,
    approvalRuleOverriddenEventMetadata_overrideStatus,

    -- ** ApprovalRuleTemplate
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_lastModifiedUser,
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_approvalRuleTemplateName,
    approvalRuleTemplate_ruleContentSha256,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_approvalRuleTemplateId,

    -- ** ApprovalStateChangedEventMetadata
    approvalStateChangedEventMetadata_revisionId,
    approvalStateChangedEventMetadata_approvalStatus,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode,

    -- ** BatchDescribeMergeConflictsError
    batchDescribeMergeConflictsError_filePath,
    batchDescribeMergeConflictsError_exceptionName,
    batchDescribeMergeConflictsError_message,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode,

    -- ** BatchGetCommitsError
    batchGetCommitsError_commitId,
    batchGetCommitsError_errorMessage,
    batchGetCommitsError_errorCode,

    -- ** BlobMetadata
    blobMetadata_path,
    blobMetadata_mode,
    blobMetadata_blobId,

    -- ** BranchInfo
    branchInfo_commitId,
    branchInfo_branchName,

    -- ** Comment
    comment_clientRequestToken,
    comment_reactionCounts,
    comment_lastModifiedDate,
    comment_deleted,
    comment_creationDate,
    comment_commentId,
    comment_inReplyTo,
    comment_authorArn,
    comment_content,
    comment_callerReactions,

    -- ** CommentsForComparedCommit
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_repositoryName,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_comments,
    commentsForComparedCommit_location,
    commentsForComparedCommit_afterBlobId,

    -- ** CommentsForPullRequest
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_comments,
    commentsForPullRequest_location,
    commentsForPullRequest_afterBlobId,

    -- ** Commit
    commit_message,
    commit_author,
    commit_commitId,
    commit_parents,
    commit_committer,
    commit_treeId,
    commit_additionalData,

    -- ** Conflict
    conflict_mergeHunks,
    conflict_conflictMetadata,

    -- ** ConflictMetadata
    conflictMetadata_filePath,
    conflictMetadata_contentConflict,
    conflictMetadata_fileSizes,
    conflictMetadata_numberOfConflicts,
    conflictMetadata_mergeOperations,
    conflictMetadata_fileModes,
    conflictMetadata_isBinaryFile,
    conflictMetadata_fileModeConflict,
    conflictMetadata_objectTypeConflict,
    conflictMetadata_objectTypes,

    -- ** ConflictResolution
    conflictResolution_replaceContents,
    conflictResolution_setFileModes,
    conflictResolution_deleteFiles,

    -- ** DeleteFileEntry
    deleteFileEntry_filePath,

    -- ** Difference
    difference_changeType,
    difference_afterBlob,
    difference_beforeBlob,

    -- ** Evaluation
    evaluation_overridden,
    evaluation_approvalRulesNotSatisfied,
    evaluation_approvalRulesSatisfied,
    evaluation_approved,

    -- ** File
    file_fileMode,
    file_absolutePath,
    file_blobId,
    file_relativePath,

    -- ** FileMetadata
    fileMetadata_fileMode,
    fileMetadata_absolutePath,
    fileMetadata_blobId,

    -- ** FileModes
    fileModes_destination,
    fileModes_base,
    fileModes_source,

    -- ** FileSizes
    fileSizes_destination,
    fileSizes_base,
    fileSizes_source,

    -- ** Folder
    folder_absolutePath,
    folder_relativePath,
    folder_treeId,

    -- ** IsBinaryFile
    isBinaryFile_destination,
    isBinaryFile_base,
    isBinaryFile_source,

    -- ** Location
    location_filePath,
    location_filePosition,
    location_relativeFileVersion,

    -- ** MergeHunk
    mergeHunk_destination,
    mergeHunk_isConflict,
    mergeHunk_base,
    mergeHunk_source,

    -- ** MergeHunkDetail
    mergeHunkDetail_endLine,
    mergeHunkDetail_hunkContent,
    mergeHunkDetail_startLine,

    -- ** MergeMetadata
    mergeMetadata_mergeOption,
    mergeMetadata_mergeCommitId,
    mergeMetadata_isMerged,
    mergeMetadata_mergedBy,

    -- ** MergeOperations
    mergeOperations_destination,
    mergeOperations_source,

    -- ** ObjectTypes
    objectTypes_destination,
    objectTypes_base,
    objectTypes_source,

    -- ** OriginApprovalRuleTemplate
    originApprovalRuleTemplate_approvalRuleTemplateName,
    originApprovalRuleTemplate_approvalRuleTemplateId,

    -- ** PullRequest
    pullRequest_approvalRules,
    pullRequest_clientRequestToken,
    pullRequest_pullRequestId,
    pullRequest_pullRequestStatus,
    pullRequest_pullRequestTargets,
    pullRequest_creationDate,
    pullRequest_description,
    pullRequest_lastActivityDate,
    pullRequest_title,
    pullRequest_revisionId,
    pullRequest_authorArn,

    -- ** PullRequestCreatedEventMetadata
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_mergeBase,
    pullRequestCreatedEventMetadata_sourceCommitId,
    pullRequestCreatedEventMetadata_destinationCommitId,

    -- ** PullRequestEvent
    pullRequestEvent_pullRequestMergedStateChangedEventMetadata,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_approvalStateChangedEventMetadata,
    pullRequestEvent_eventDate,
    pullRequestEvent_pullRequestStatusChangedEventMetadata,
    pullRequestEvent_approvalRuleEventMetadata,
    pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestEvent_actorArn,
    pullRequestEvent_pullRequestEventType,

    -- ** PullRequestMergedStateChangedEventMetadata
    pullRequestMergedStateChangedEventMetadata_mergeMetadata,
    pullRequestMergedStateChangedEventMetadata_repositoryName,
    pullRequestMergedStateChangedEventMetadata_destinationReference,

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_repositoryName,
    pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_mergeBase,

    -- ** PullRequestStatusChangedEventMetadata
    pullRequestStatusChangedEventMetadata_pullRequestStatus,

    -- ** PullRequestTarget
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_sourceReference,
    pullRequestTarget_sourceCommit,
    pullRequestTarget_repositoryName,
    pullRequestTarget_destinationReference,
    pullRequestTarget_mergeBase,
    pullRequestTarget_destinationCommit,

    -- ** PutFileEntry
    putFileEntry_fileMode,
    putFileEntry_sourceFile,
    putFileEntry_fileContent,
    putFileEntry_filePath,

    -- ** ReactionForComment
    reactionForComment_reactionsFromDeletedUsersCount,
    reactionForComment_reactionUsers,
    reactionForComment_reaction,

    -- ** ReactionValueFormats
    reactionValueFormats_emoji,
    reactionValueFormats_shortCode,
    reactionValueFormats_unicode,

    -- ** ReplaceContentEntry
    replaceContentEntry_fileMode,
    replaceContentEntry_content,
    replaceContentEntry_filePath,
    replaceContentEntry_replacementType,

    -- ** RepositoryMetadata
    repositoryMetadata_lastModifiedDate,
    repositoryMetadata_repositoryName,
    repositoryMetadata_arn,
    repositoryMetadata_defaultBranch,
    repositoryMetadata_creationDate,
    repositoryMetadata_accountId,
    repositoryMetadata_cloneUrlHttp,
    repositoryMetadata_repositoryDescription,
    repositoryMetadata_repositoryId,
    repositoryMetadata_cloneUrlSsh,

    -- ** RepositoryNameIdPair
    repositoryNameIdPair_repositoryName,
    repositoryNameIdPair_repositoryId,

    -- ** RepositoryTrigger
    repositoryTrigger_branches,
    repositoryTrigger_customData,
    repositoryTrigger_name,
    repositoryTrigger_destinationArn,
    repositoryTrigger_events,

    -- ** RepositoryTriggerExecutionFailure
    repositoryTriggerExecutionFailure_trigger,
    repositoryTriggerExecutionFailure_failureMessage,

    -- ** SetFileModeEntry
    setFileModeEntry_filePath,
    setFileModeEntry_fileMode,

    -- ** SourceFileSpecifier
    sourceFileSpecifier_isMove,
    sourceFileSpecifier_filePath,

    -- ** SubModule
    subModule_absolutePath,
    subModule_commitId,
    subModule_relativePath,

    -- ** SymbolicLink
    symbolicLink_fileMode,
    symbolicLink_absolutePath,
    symbolicLink_blobId,
    symbolicLink_relativePath,

    -- ** Target
    target_destinationReference,
    target_repositoryName,
    target_sourceReference,

    -- ** UserInfo
    userInfo_name,
    userInfo_email,
    userInfo_date,
  )
where

import Amazonka.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Amazonka.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Amazonka.CodeCommit.BatchDescribeMergeConflicts
import Amazonka.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Amazonka.CodeCommit.BatchGetCommits
import Amazonka.CodeCommit.BatchGetRepositories
import Amazonka.CodeCommit.CreateApprovalRuleTemplate
import Amazonka.CodeCommit.CreateBranch
import Amazonka.CodeCommit.CreateCommit
import Amazonka.CodeCommit.CreatePullRequest
import Amazonka.CodeCommit.CreatePullRequestApprovalRule
import Amazonka.CodeCommit.CreateRepository
import Amazonka.CodeCommit.CreateUnreferencedMergeCommit
import Amazonka.CodeCommit.DeleteApprovalRuleTemplate
import Amazonka.CodeCommit.DeleteBranch
import Amazonka.CodeCommit.DeleteCommentContent
import Amazonka.CodeCommit.DeleteFile
import Amazonka.CodeCommit.DeletePullRequestApprovalRule
import Amazonka.CodeCommit.DeleteRepository
import Amazonka.CodeCommit.DescribeMergeConflicts
import Amazonka.CodeCommit.DescribePullRequestEvents
import Amazonka.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Amazonka.CodeCommit.EvaluatePullRequestApprovalRules
import Amazonka.CodeCommit.GetApprovalRuleTemplate
import Amazonka.CodeCommit.GetBlob
import Amazonka.CodeCommit.GetBranch
import Amazonka.CodeCommit.GetComment
import Amazonka.CodeCommit.GetCommentReactions
import Amazonka.CodeCommit.GetCommentsForComparedCommit
import Amazonka.CodeCommit.GetCommentsForPullRequest
import Amazonka.CodeCommit.GetCommit
import Amazonka.CodeCommit.GetDifferences
import Amazonka.CodeCommit.GetFile
import Amazonka.CodeCommit.GetFolder
import Amazonka.CodeCommit.GetMergeCommit
import Amazonka.CodeCommit.GetMergeConflicts
import Amazonka.CodeCommit.GetMergeOptions
import Amazonka.CodeCommit.GetPullRequest
import Amazonka.CodeCommit.GetPullRequestApprovalStates
import Amazonka.CodeCommit.GetPullRequestOverrideState
import Amazonka.CodeCommit.GetRepository
import Amazonka.CodeCommit.GetRepositoryTriggers
import Amazonka.CodeCommit.ListApprovalRuleTemplates
import Amazonka.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import Amazonka.CodeCommit.ListBranches
import Amazonka.CodeCommit.ListPullRequests
import Amazonka.CodeCommit.ListRepositories
import Amazonka.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Amazonka.CodeCommit.ListTagsForResource
import Amazonka.CodeCommit.MergeBranchesByFastForward
import Amazonka.CodeCommit.MergeBranchesBySquash
import Amazonka.CodeCommit.MergeBranchesByThreeWay
import Amazonka.CodeCommit.MergePullRequestByFastForward
import Amazonka.CodeCommit.MergePullRequestBySquash
import Amazonka.CodeCommit.MergePullRequestByThreeWay
import Amazonka.CodeCommit.OverridePullRequestApprovalRules
import Amazonka.CodeCommit.PostCommentForComparedCommit
import Amazonka.CodeCommit.PostCommentForPullRequest
import Amazonka.CodeCommit.PostCommentReply
import Amazonka.CodeCommit.PutCommentReaction
import Amazonka.CodeCommit.PutFile
import Amazonka.CodeCommit.PutRepositoryTriggers
import Amazonka.CodeCommit.TagResource
import Amazonka.CodeCommit.TestRepositoryTriggers
import Amazonka.CodeCommit.Types.Approval
import Amazonka.CodeCommit.Types.ApprovalRule
import Amazonka.CodeCommit.Types.ApprovalRuleEventMetadata
import Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Amazonka.CodeCommit.Types.ApprovalRuleTemplate
import Amazonka.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Amazonka.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Amazonka.CodeCommit.Types.BatchDescribeMergeConflictsError
import Amazonka.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Amazonka.CodeCommit.Types.BatchGetCommitsError
import Amazonka.CodeCommit.Types.BlobMetadata
import Amazonka.CodeCommit.Types.BranchInfo
import Amazonka.CodeCommit.Types.Comment
import Amazonka.CodeCommit.Types.CommentsForComparedCommit
import Amazonka.CodeCommit.Types.CommentsForPullRequest
import Amazonka.CodeCommit.Types.Commit
import Amazonka.CodeCommit.Types.Conflict
import Amazonka.CodeCommit.Types.ConflictMetadata
import Amazonka.CodeCommit.Types.ConflictResolution
import Amazonka.CodeCommit.Types.DeleteFileEntry
import Amazonka.CodeCommit.Types.Difference
import Amazonka.CodeCommit.Types.Evaluation
import Amazonka.CodeCommit.Types.File
import Amazonka.CodeCommit.Types.FileMetadata
import Amazonka.CodeCommit.Types.FileModes
import Amazonka.CodeCommit.Types.FileSizes
import Amazonka.CodeCommit.Types.Folder
import Amazonka.CodeCommit.Types.IsBinaryFile
import Amazonka.CodeCommit.Types.Location
import Amazonka.CodeCommit.Types.MergeHunk
import Amazonka.CodeCommit.Types.MergeHunkDetail
import Amazonka.CodeCommit.Types.MergeMetadata
import Amazonka.CodeCommit.Types.MergeOperations
import Amazonka.CodeCommit.Types.ObjectTypes
import Amazonka.CodeCommit.Types.OriginApprovalRuleTemplate
import Amazonka.CodeCommit.Types.PullRequest
import Amazonka.CodeCommit.Types.PullRequestCreatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestEvent
import Amazonka.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestTarget
import Amazonka.CodeCommit.Types.PutFileEntry
import Amazonka.CodeCommit.Types.ReactionForComment
import Amazonka.CodeCommit.Types.ReactionValueFormats
import Amazonka.CodeCommit.Types.ReplaceContentEntry
import Amazonka.CodeCommit.Types.RepositoryMetadata
import Amazonka.CodeCommit.Types.RepositoryNameIdPair
import Amazonka.CodeCommit.Types.RepositoryTrigger
import Amazonka.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Amazonka.CodeCommit.Types.SetFileModeEntry
import Amazonka.CodeCommit.Types.SourceFileSpecifier
import Amazonka.CodeCommit.Types.SubModule
import Amazonka.CodeCommit.Types.SymbolicLink
import Amazonka.CodeCommit.Types.Target
import Amazonka.CodeCommit.Types.UserInfo
import Amazonka.CodeCommit.UntagResource
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateContent
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateDescription
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateName
import Amazonka.CodeCommit.UpdateComment
import Amazonka.CodeCommit.UpdateDefaultBranch
import Amazonka.CodeCommit.UpdatePullRequestApprovalRuleContent
import Amazonka.CodeCommit.UpdatePullRequestApprovalState
import Amazonka.CodeCommit.UpdatePullRequestDescription
import Amazonka.CodeCommit.UpdatePullRequestStatus
import Amazonka.CodeCommit.UpdatePullRequestTitle
import Amazonka.CodeCommit.UpdateRepositoryDescription
import Amazonka.CodeCommit.UpdateRepositoryName
