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

    -- ** MergePullRequestByFastForward
    mergePullRequestByFastForward_sourceCommitId,
    mergePullRequestByFastForward_pullRequestId,
    mergePullRequestByFastForward_repositoryName,
    mergePullRequestByFastForwardResponse_pullRequest,
    mergePullRequestByFastForwardResponse_httpStatus,

    -- ** UpdateRepositoryName
    updateRepositoryName_oldName,
    updateRepositoryName_newName,

    -- ** PostCommentForPullRequest
    postCommentForPullRequest_location,
    postCommentForPullRequest_clientRequestToken,
    postCommentForPullRequest_pullRequestId,
    postCommentForPullRequest_repositoryName,
    postCommentForPullRequest_beforeCommitId,
    postCommentForPullRequest_afterCommitId,
    postCommentForPullRequest_content,
    postCommentForPullRequestResponse_beforeBlobId,
    postCommentForPullRequestResponse_location,
    postCommentForPullRequestResponse_afterCommitId,
    postCommentForPullRequestResponse_pullRequestId,
    postCommentForPullRequestResponse_afterBlobId,
    postCommentForPullRequestResponse_beforeCommitId,
    postCommentForPullRequestResponse_repositoryName,
    postCommentForPullRequestResponse_comment,
    postCommentForPullRequestResponse_httpStatus,

    -- ** MergeBranchesBySquash
    mergeBranchesBySquash_email,
    mergeBranchesBySquash_authorName,
    mergeBranchesBySquash_targetBranch,
    mergeBranchesBySquash_conflictDetailLevel,
    mergeBranchesBySquash_commitMessage,
    mergeBranchesBySquash_conflictResolution,
    mergeBranchesBySquash_conflictResolutionStrategy,
    mergeBranchesBySquash_keepEmptyFolders,
    mergeBranchesBySquash_repositoryName,
    mergeBranchesBySquash_sourceCommitSpecifier,
    mergeBranchesBySquash_destinationCommitSpecifier,
    mergeBranchesBySquashResponse_commitId,
    mergeBranchesBySquashResponse_treeId,
    mergeBranchesBySquashResponse_httpStatus,

    -- ** GetCommit
    getCommit_repositoryName,
    getCommit_commitId,
    getCommitResponse_httpStatus,
    getCommitResponse_commit,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories
    batchAssociateApprovalRuleTemplateWithRepositories_approvalRuleTemplateName,
    batchAssociateApprovalRuleTemplateWithRepositories_repositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_httpStatus,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_associatedRepositoryNames,
    batchAssociateApprovalRuleTemplateWithRepositoriesResponse_errors,

    -- ** GetCommentReactions
    getCommentReactions_nextToken,
    getCommentReactions_reactionUserArn,
    getCommentReactions_maxResults,
    getCommentReactions_commentId,
    getCommentReactionsResponse_nextToken,
    getCommentReactionsResponse_httpStatus,
    getCommentReactionsResponse_reactionsForComment,

    -- ** GetApprovalRuleTemplate
    getApprovalRuleTemplate_approvalRuleTemplateName,
    getApprovalRuleTemplateResponse_httpStatus,
    getApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** DisassociateApprovalRuleTemplateFromRepository
    disassociateApprovalRuleTemplateFromRepository_approvalRuleTemplateName,
    disassociateApprovalRuleTemplateFromRepository_repositoryName,

    -- ** GetBranch
    getBranch_branchName,
    getBranch_repositoryName,
    getBranchResponse_branch,
    getBranchResponse_httpStatus,

    -- ** GetDifferences
    getDifferences_afterPath,
    getDifferences_nextToken,
    getDifferences_beforeCommitSpecifier,
    getDifferences_beforePath,
    getDifferences_maxResults,
    getDifferences_repositoryName,
    getDifferences_afterCommitSpecifier,
    getDifferencesResponse_nextToken,
    getDifferencesResponse_differences,
    getDifferencesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_nextToken,
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_nextToken,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetPullRequest
    getPullRequest_pullRequestId,
    getPullRequestResponse_httpStatus,
    getPullRequestResponse_pullRequest,

    -- ** OverridePullRequestApprovalRules
    overridePullRequestApprovalRules_pullRequestId,
    overridePullRequestApprovalRules_revisionId,
    overridePullRequestApprovalRules_overrideStatus,

    -- ** ListPullRequests
    listPullRequests_authorArn,
    listPullRequests_nextToken,
    listPullRequests_pullRequestStatus,
    listPullRequests_maxResults,
    listPullRequests_repositoryName,
    listPullRequestsResponse_nextToken,
    listPullRequestsResponse_httpStatus,
    listPullRequestsResponse_pullRequestIds,

    -- ** CreateCommit
    createCommit_setFileModes,
    createCommit_email,
    createCommit_authorName,
    createCommit_parentCommitId,
    createCommit_deleteFiles,
    createCommit_putFiles,
    createCommit_commitMessage,
    createCommit_keepEmptyFolders,
    createCommit_repositoryName,
    createCommit_branchName,
    createCommitResponse_commitId,
    createCommitResponse_treeId,
    createCommitResponse_filesAdded,
    createCommitResponse_filesUpdated,
    createCommitResponse_filesDeleted,
    createCommitResponse_httpStatus,

    -- ** UpdatePullRequestApprovalState
    updatePullRequestApprovalState_pullRequestId,
    updatePullRequestApprovalState_revisionId,
    updatePullRequestApprovalState_approvalState,

    -- ** EvaluatePullRequestApprovalRules
    evaluatePullRequestApprovalRules_pullRequestId,
    evaluatePullRequestApprovalRules_revisionId,
    evaluatePullRequestApprovalRulesResponse_httpStatus,
    evaluatePullRequestApprovalRulesResponse_evaluation,

    -- ** GetComment
    getComment_commentId,
    getCommentResponse_comment,
    getCommentResponse_httpStatus,

    -- ** CreateApprovalRuleTemplate
    createApprovalRuleTemplate_approvalRuleTemplateDescription,
    createApprovalRuleTemplate_approvalRuleTemplateName,
    createApprovalRuleTemplate_approvalRuleTemplateContent,
    createApprovalRuleTemplateResponse_httpStatus,
    createApprovalRuleTemplateResponse_approvalRuleTemplate,

    -- ** DeleteBranch
    deleteBranch_repositoryName,
    deleteBranch_branchName,
    deleteBranchResponse_deletedBranch,
    deleteBranchResponse_httpStatus,

    -- ** UpdateRepositoryDescription
    updateRepositoryDescription_repositoryDescription,
    updateRepositoryDescription_repositoryName,

    -- ** CreateBranch
    createBranch_repositoryName,
    createBranch_branchName,
    createBranch_commitId,

    -- ** GetFolder
    getFolder_commitSpecifier,
    getFolder_repositoryName,
    getFolder_folderPath,
    getFolderResponse_subModules,
    getFolderResponse_treeId,
    getFolderResponse_subFolders,
    getFolderResponse_symbolicLinks,
    getFolderResponse_files,
    getFolderResponse_httpStatus,
    getFolderResponse_commitId,
    getFolderResponse_folderPath,

    -- ** CreatePullRequest
    createPullRequest_clientRequestToken,
    createPullRequest_description,
    createPullRequest_title,
    createPullRequest_targets,
    createPullRequestResponse_httpStatus,
    createPullRequestResponse_pullRequest,

    -- ** DeleteApprovalRuleTemplate
    deleteApprovalRuleTemplate_approvalRuleTemplateName,
    deleteApprovalRuleTemplateResponse_httpStatus,
    deleteApprovalRuleTemplateResponse_approvalRuleTemplateId,

    -- ** ListBranches
    listBranches_nextToken,
    listBranches_repositoryName,
    listBranchesResponse_branches,
    listBranchesResponse_nextToken,
    listBranchesResponse_httpStatus,

    -- ** BatchGetCommits
    batchGetCommits_commitIds,
    batchGetCommits_repositoryName,
    batchGetCommitsResponse_commits,
    batchGetCommitsResponse_errors,
    batchGetCommitsResponse_httpStatus,

    -- ** PutCommentReaction
    putCommentReaction_commentId,
    putCommentReaction_reactionValue,

    -- ** UpdatePullRequestDescription
    updatePullRequestDescription_pullRequestId,
    updatePullRequestDescription_description,
    updatePullRequestDescriptionResponse_httpStatus,
    updatePullRequestDescriptionResponse_pullRequest,

    -- ** ListRepositories
    listRepositories_nextToken,
    listRepositories_order,
    listRepositories_sortBy,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_httpStatus,

    -- ** CreateRepository
    createRepository_repositoryDescription,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repositoryMetadata,
    createRepositoryResponse_httpStatus,

    -- ** UpdateDefaultBranch
    updateDefaultBranch_repositoryName,
    updateDefaultBranch_defaultBranchName,

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

    -- ** CreatePullRequestApprovalRule
    createPullRequestApprovalRule_pullRequestId,
    createPullRequestApprovalRule_approvalRuleName,
    createPullRequestApprovalRule_approvalRuleContent,
    createPullRequestApprovalRuleResponse_httpStatus,
    createPullRequestApprovalRuleResponse_approvalRule,

    -- ** PostCommentReply
    postCommentReply_clientRequestToken,
    postCommentReply_inReplyTo,
    postCommentReply_content,
    postCommentReplyResponse_comment,
    postCommentReplyResponse_httpStatus,

    -- ** UpdateApprovalRuleTemplateContent
    updateApprovalRuleTemplateContent_existingRuleContentSha256,
    updateApprovalRuleTemplateContent_approvalRuleTemplateName,
    updateApprovalRuleTemplateContent_newRuleContent,
    updateApprovalRuleTemplateContentResponse_httpStatus,
    updateApprovalRuleTemplateContentResponse_approvalRuleTemplate,

    -- ** CreateUnreferencedMergeCommit
    createUnreferencedMergeCommit_email,
    createUnreferencedMergeCommit_authorName,
    createUnreferencedMergeCommit_conflictDetailLevel,
    createUnreferencedMergeCommit_commitMessage,
    createUnreferencedMergeCommit_conflictResolution,
    createUnreferencedMergeCommit_conflictResolutionStrategy,
    createUnreferencedMergeCommit_keepEmptyFolders,
    createUnreferencedMergeCommit_repositoryName,
    createUnreferencedMergeCommit_sourceCommitSpecifier,
    createUnreferencedMergeCommit_destinationCommitSpecifier,
    createUnreferencedMergeCommit_mergeOption,
    createUnreferencedMergeCommitResponse_commitId,
    createUnreferencedMergeCommitResponse_treeId,
    createUnreferencedMergeCommitResponse_httpStatus,

    -- ** ListRepositoriesForApprovalRuleTemplate
    listRepositoriesForApprovalRuleTemplate_nextToken,
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName,
    listRepositoriesForApprovalRuleTemplateResponse_repositoryNames,
    listRepositoriesForApprovalRuleTemplateResponse_nextToken,
    listRepositoriesForApprovalRuleTemplateResponse_httpStatus,

    -- ** GetRepository
    getRepository_repositoryName,
    getRepositoryResponse_repositoryMetadata,
    getRepositoryResponse_httpStatus,

    -- ** BatchDescribeMergeConflicts
    batchDescribeMergeConflicts_filePaths,
    batchDescribeMergeConflicts_conflictDetailLevel,
    batchDescribeMergeConflicts_nextToken,
    batchDescribeMergeConflicts_maxConflictFiles,
    batchDescribeMergeConflicts_maxMergeHunks,
    batchDescribeMergeConflicts_conflictResolutionStrategy,
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

    -- ** DeletePullRequestApprovalRule
    deletePullRequestApprovalRule_pullRequestId,
    deletePullRequestApprovalRule_approvalRuleName,
    deletePullRequestApprovalRuleResponse_httpStatus,
    deletePullRequestApprovalRuleResponse_approvalRuleId,

    -- ** GetRepositoryTriggers
    getRepositoryTriggers_repositoryName,
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_httpStatus,

    -- ** UpdateApprovalRuleTemplateName
    updateApprovalRuleTemplateName_oldApprovalRuleTemplateName,
    updateApprovalRuleTemplateName_newApprovalRuleTemplateName,
    updateApprovalRuleTemplateNameResponse_httpStatus,
    updateApprovalRuleTemplateNameResponse_approvalRuleTemplate,

    -- ** PutFile
    putFile_email,
    putFile_fileMode,
    putFile_parentCommitId,
    putFile_name,
    putFile_commitMessage,
    putFile_repositoryName,
    putFile_branchName,
    putFile_fileContent,
    putFile_filePath,
    putFileResponse_httpStatus,
    putFileResponse_commitId,
    putFileResponse_blobId,
    putFileResponse_treeId,

    -- ** DeleteFile
    deleteFile_email,
    deleteFile_name,
    deleteFile_commitMessage,
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
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_httpStatus,

    -- ** GetMergeCommit
    getMergeCommit_conflictDetailLevel,
    getMergeCommit_conflictResolutionStrategy,
    getMergeCommit_repositoryName,
    getMergeCommit_sourceCommitSpecifier,
    getMergeCommit_destinationCommitSpecifier,
    getMergeCommitResponse_mergedCommitId,
    getMergeCommitResponse_destinationCommitId,
    getMergeCommitResponse_baseCommitId,
    getMergeCommitResponse_sourceCommitId,
    getMergeCommitResponse_httpStatus,

    -- ** TestRepositoryTriggers
    testRepositoryTriggers_repositoryName,
    testRepositoryTriggers_triggers,
    testRepositoryTriggersResponse_failedExecutions,
    testRepositoryTriggersResponse_successfulExecutions,
    testRepositoryTriggersResponse_httpStatus,

    -- ** MergePullRequestBySquash
    mergePullRequestBySquash_email,
    mergePullRequestBySquash_authorName,
    mergePullRequestBySquash_conflictDetailLevel,
    mergePullRequestBySquash_commitMessage,
    mergePullRequestBySquash_conflictResolution,
    mergePullRequestBySquash_conflictResolutionStrategy,
    mergePullRequestBySquash_keepEmptyFolders,
    mergePullRequestBySquash_sourceCommitId,
    mergePullRequestBySquash_pullRequestId,
    mergePullRequestBySquash_repositoryName,
    mergePullRequestBySquashResponse_pullRequest,
    mergePullRequestBySquashResponse_httpStatus,

    -- ** UpdateComment
    updateComment_commentId,
    updateComment_content,
    updateCommentResponse_comment,
    updateCommentResponse_httpStatus,

    -- ** PostCommentForComparedCommit
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_httpStatus,

    -- ** MergeBranchesByFastForward
    mergeBranchesByFastForward_targetBranch,
    mergeBranchesByFastForward_repositoryName,
    mergeBranchesByFastForward_sourceCommitSpecifier,
    mergeBranchesByFastForward_destinationCommitSpecifier,
    mergeBranchesByFastForwardResponse_commitId,
    mergeBranchesByFastForwardResponse_treeId,
    mergeBranchesByFastForwardResponse_httpStatus,

    -- ** UpdatePullRequestTitle
    updatePullRequestTitle_pullRequestId,
    updatePullRequestTitle_title,
    updatePullRequestTitleResponse_httpStatus,
    updatePullRequestTitleResponse_pullRequest,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories
    batchDisassociateApprovalRuleTemplateFromRepositories_approvalRuleTemplateName,
    batchDisassociateApprovalRuleTemplateFromRepositories_repositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_httpStatus,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_disassociatedRepositoryNames,
    batchDisassociateApprovalRuleTemplateFromRepositoriesResponse_errors,

    -- ** UpdatePullRequestApprovalRuleContent
    updatePullRequestApprovalRuleContent_existingRuleContentSha256,
    updatePullRequestApprovalRuleContent_pullRequestId,
    updatePullRequestApprovalRuleContent_approvalRuleName,
    updatePullRequestApprovalRuleContent_newRuleContent,
    updatePullRequestApprovalRuleContentResponse_httpStatus,
    updatePullRequestApprovalRuleContentResponse_approvalRule,

    -- ** GetBlob
    getBlob_repositoryName,
    getBlob_blobId,
    getBlobResponse_httpStatus,
    getBlobResponse_content,

    -- ** AssociateApprovalRuleTemplateWithRepository
    associateApprovalRuleTemplateWithRepository_approvalRuleTemplateName,
    associateApprovalRuleTemplateWithRepository_repositoryName,

    -- ** PutRepositoryTriggers
    putRepositoryTriggers_repositoryName,
    putRepositoryTriggers_triggers,
    putRepositoryTriggersResponse_configurationId,
    putRepositoryTriggersResponse_httpStatus,

    -- ** ListApprovalRuleTemplates
    listApprovalRuleTemplates_nextToken,
    listApprovalRuleTemplates_maxResults,
    listApprovalRuleTemplatesResponse_nextToken,
    listApprovalRuleTemplatesResponse_approvalRuleTemplateNames,
    listApprovalRuleTemplatesResponse_httpStatus,

    -- ** DescribeMergeConflicts
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_nextToken,
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_conflictResolutionStrategy,
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

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,

    -- ** MergeBranchesByThreeWay
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_targetBranch,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_conflictResolution,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_repositoryName,
    mergeBranchesByThreeWay_sourceCommitSpecifier,
    mergeBranchesByThreeWay_destinationCommitSpecifier,
    mergeBranchesByThreeWayResponse_commitId,
    mergeBranchesByThreeWayResponse_treeId,
    mergeBranchesByThreeWayResponse_httpStatus,

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

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,

    -- ** GetMergeConflicts
    getMergeConflicts_conflictDetailLevel,
    getMergeConflicts_nextToken,
    getMergeConflicts_maxConflictFiles,
    getMergeConflicts_conflictResolutionStrategy,
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

    -- ** DeleteRepository
    deleteRepository_repositoryName,
    deleteRepositoryResponse_repositoryId,
    deleteRepositoryResponse_httpStatus,

    -- ** DeleteCommentContent
    deleteCommentContent_commentId,
    deleteCommentContentResponse_comment,
    deleteCommentContentResponse_httpStatus,

    -- ** MergePullRequestByThreeWay
    mergePullRequestByThreeWay_email,
    mergePullRequestByThreeWay_authorName,
    mergePullRequestByThreeWay_conflictDetailLevel,
    mergePullRequestByThreeWay_commitMessage,
    mergePullRequestByThreeWay_conflictResolution,
    mergePullRequestByThreeWay_conflictResolutionStrategy,
    mergePullRequestByThreeWay_keepEmptyFolders,
    mergePullRequestByThreeWay_sourceCommitId,
    mergePullRequestByThreeWay_pullRequestId,
    mergePullRequestByThreeWay_repositoryName,
    mergePullRequestByThreeWayResponse_pullRequest,
    mergePullRequestByThreeWayResponse_httpStatus,

    -- ** DescribePullRequestEvents
    describePullRequestEvents_pullRequestEventType,
    describePullRequestEvents_actorArn,
    describePullRequestEvents_nextToken,
    describePullRequestEvents_maxResults,
    describePullRequestEvents_pullRequestId,
    describePullRequestEventsResponse_nextToken,
    describePullRequestEventsResponse_httpStatus,
    describePullRequestEventsResponse_pullRequestEvents,

    -- ** BatchGetRepositories
    batchGetRepositories_repositoryNames,
    batchGetRepositoriesResponse_repositories,
    batchGetRepositoriesResponse_repositoriesNotFound,
    batchGetRepositoriesResponse_httpStatus,

    -- ** UpdateApprovalRuleTemplateDescription
    updateApprovalRuleTemplateDescription_approvalRuleTemplateName,
    updateApprovalRuleTemplateDescription_approvalRuleTemplateDescription,
    updateApprovalRuleTemplateDescriptionResponse_httpStatus,
    updateApprovalRuleTemplateDescriptionResponse_approvalRuleTemplate,

    -- ** GetPullRequestOverrideState
    getPullRequestOverrideState_pullRequestId,
    getPullRequestOverrideState_revisionId,
    getPullRequestOverrideStateResponse_overridden,
    getPullRequestOverrideStateResponse_overrider,
    getPullRequestOverrideStateResponse_httpStatus,

    -- ** GetPullRequestApprovalStates
    getPullRequestApprovalStates_pullRequestId,
    getPullRequestApprovalStates_revisionId,
    getPullRequestApprovalStatesResponse_approvals,
    getPullRequestApprovalStatesResponse_httpStatus,

    -- ** GetCommentsForPullRequest
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_repositoryName,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_pullRequestId,
    getCommentsForPullRequestResponse_commentsForPullRequestData,
    getCommentsForPullRequestResponse_nextToken,
    getCommentsForPullRequestResponse_httpStatus,

    -- ** UpdatePullRequestStatus
    updatePullRequestStatus_pullRequestId,
    updatePullRequestStatus_pullRequestStatus,
    updatePullRequestStatusResponse_httpStatus,
    updatePullRequestStatusResponse_pullRequest,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    listAssociatedApprovalRuleTemplatesForRepository_nextToken,
    listAssociatedApprovalRuleTemplatesForRepository_maxResults,
    listAssociatedApprovalRuleTemplatesForRepository_repositoryName,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus,

    -- * Types

    -- ** Approval
    approval_approvalState,
    approval_userArn,

    -- ** ApprovalRule
    approvalRule_ruleContentSha256,
    approvalRule_lastModifiedDate,
    approvalRule_approvalRuleName,
    approvalRule_approvalRuleId,
    approvalRule_lastModifiedUser,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_creationDate,
    approvalRule_approvalRuleContent,

    -- ** ApprovalRuleEventMetadata
    approvalRuleEventMetadata_approvalRuleName,
    approvalRuleEventMetadata_approvalRuleId,
    approvalRuleEventMetadata_approvalRuleContent,

    -- ** ApprovalRuleOverriddenEventMetadata
    approvalRuleOverriddenEventMetadata_overrideStatus,
    approvalRuleOverriddenEventMetadata_revisionId,

    -- ** ApprovalRuleTemplate
    approvalRuleTemplate_ruleContentSha256,
    approvalRuleTemplate_approvalRuleTemplateId,
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_lastModifiedUser,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_approvalRuleTemplateName,

    -- ** ApprovalStateChangedEventMetadata
    approvalStateChangedEventMetadata_approvalStatus,
    approvalStateChangedEventMetadata_revisionId,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,

    -- ** BatchDescribeMergeConflictsError
    batchDescribeMergeConflictsError_filePath,
    batchDescribeMergeConflictsError_exceptionName,
    batchDescribeMergeConflictsError_message,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,

    -- ** BatchGetCommitsError
    batchGetCommitsError_commitId,
    batchGetCommitsError_errorCode,
    batchGetCommitsError_errorMessage,

    -- ** BlobMetadata
    blobMetadata_path,
    blobMetadata_mode,
    blobMetadata_blobId,

    -- ** BranchInfo
    branchInfo_commitId,
    branchInfo_branchName,

    -- ** Comment
    comment_lastModifiedDate,
    comment_authorArn,
    comment_content,
    comment_callerReactions,
    comment_creationDate,
    comment_deleted,
    comment_clientRequestToken,
    comment_commentId,
    comment_inReplyTo,
    comment_reactionCounts,

    -- ** CommentsForComparedCommit
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_location,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_afterBlobId,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_repositoryName,
    commentsForComparedCommit_comments,

    -- ** CommentsForPullRequest
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_location,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_afterBlobId,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_comments,

    -- ** Commit
    commit_commitId,
    commit_committer,
    commit_treeId,
    commit_additionalData,
    commit_parents,
    commit_author,
    commit_message,

    -- ** Conflict
    conflict_mergeHunks,
    conflict_conflictMetadata,

    -- ** ConflictMetadata
    conflictMetadata_numberOfConflicts,
    conflictMetadata_contentConflict,
    conflictMetadata_fileSizes,
    conflictMetadata_filePath,
    conflictMetadata_isBinaryFile,
    conflictMetadata_fileModeConflict,
    conflictMetadata_objectTypeConflict,
    conflictMetadata_mergeOperations,
    conflictMetadata_objectTypes,
    conflictMetadata_fileModes,

    -- ** ConflictResolution
    conflictResolution_setFileModes,
    conflictResolution_deleteFiles,
    conflictResolution_replaceContents,

    -- ** DeleteFileEntry
    deleteFileEntry_filePath,

    -- ** Difference
    difference_afterBlob,
    difference_beforeBlob,
    difference_changeType,

    -- ** Evaluation
    evaluation_approvalRulesSatisfied,
    evaluation_approvalRulesNotSatisfied,
    evaluation_approved,
    evaluation_overridden,

    -- ** File
    file_absolutePath,
    file_fileMode,
    file_blobId,
    file_relativePath,

    -- ** FileMetadata
    fileMetadata_absolutePath,
    fileMetadata_fileMode,
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
    folder_treeId,
    folder_relativePath,

    -- ** IsBinaryFile
    isBinaryFile_destination,
    isBinaryFile_base,
    isBinaryFile_source,

    -- ** Location
    location_relativeFileVersion,
    location_filePath,
    location_filePosition,

    -- ** MergeHunk
    mergeHunk_destination,
    mergeHunk_base,
    mergeHunk_isConflict,
    mergeHunk_source,

    -- ** MergeHunkDetail
    mergeHunkDetail_startLine,
    mergeHunkDetail_endLine,
    mergeHunkDetail_hunkContent,

    -- ** MergeMetadata
    mergeMetadata_mergedBy,
    mergeMetadata_mergeOption,
    mergeMetadata_isMerged,
    mergeMetadata_mergeCommitId,

    -- ** MergeOperations
    mergeOperations_destination,
    mergeOperations_source,

    -- ** ObjectTypes
    objectTypes_destination,
    objectTypes_base,
    objectTypes_source,

    -- ** OriginApprovalRuleTemplate
    originApprovalRuleTemplate_approvalRuleTemplateId,
    originApprovalRuleTemplate_approvalRuleTemplateName,

    -- ** PullRequest
    pullRequest_approvalRules,
    pullRequest_authorArn,
    pullRequest_pullRequestId,
    pullRequest_creationDate,
    pullRequest_pullRequestStatus,
    pullRequest_title,
    pullRequest_clientRequestToken,
    pullRequest_lastActivityDate,
    pullRequest_revisionId,
    pullRequest_pullRequestTargets,
    pullRequest_description,

    -- ** PullRequestCreatedEventMetadata
    pullRequestCreatedEventMetadata_destinationCommitId,
    pullRequestCreatedEventMetadata_mergeBase,
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_sourceCommitId,

    -- ** PullRequestEvent
    pullRequestEvent_pullRequestMergedStateChangedEventMetadata,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_approvalRuleEventMetadata,
    pullRequestEvent_pullRequestEventType,
    pullRequestEvent_pullRequestStatusChangedEventMetadata,
    pullRequestEvent_actorArn,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_eventDate,
    pullRequestEvent_approvalStateChangedEventMetadata,
    pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,

    -- ** PullRequestMergedStateChangedEventMetadata
    pullRequestMergedStateChangedEventMetadata_destinationReference,
    pullRequestMergedStateChangedEventMetadata_mergeMetadata,
    pullRequestMergedStateChangedEventMetadata_repositoryName,

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_mergeBase,
    pullRequestSourceReferenceUpdatedEventMetadata_repositoryName,

    -- ** PullRequestStatusChangedEventMetadata
    pullRequestStatusChangedEventMetadata_pullRequestStatus,

    -- ** PullRequestTarget
    pullRequestTarget_sourceCommit,
    pullRequestTarget_destinationReference,
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_mergeBase,
    pullRequestTarget_destinationCommit,
    pullRequestTarget_repositoryName,
    pullRequestTarget_sourceReference,

    -- ** PutFileEntry
    putFileEntry_fileContent,
    putFileEntry_fileMode,
    putFileEntry_sourceFile,
    putFileEntry_filePath,

    -- ** ReactionForComment
    reactionForComment_reactionUsers,
    reactionForComment_reactionsFromDeletedUsersCount,
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
    repositoryMetadata_repositoryDescription,
    repositoryMetadata_lastModifiedDate,
    repositoryMetadata_arn,
    repositoryMetadata_cloneUrlHttp,
    repositoryMetadata_accountId,
    repositoryMetadata_defaultBranch,
    repositoryMetadata_repositoryId,
    repositoryMetadata_repositoryName,
    repositoryMetadata_creationDate,
    repositoryMetadata_cloneUrlSsh,

    -- ** RepositoryNameIdPair
    repositoryNameIdPair_repositoryId,
    repositoryNameIdPair_repositoryName,

    -- ** RepositoryTrigger
    repositoryTrigger_branches,
    repositoryTrigger_customData,
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
    symbolicLink_fileMode,
    symbolicLink_blobId,
    symbolicLink_relativePath,

    -- ** Target
    target_destinationReference,
    target_repositoryName,
    target_sourceReference,

    -- ** UserInfo
    userInfo_email,
    userInfo_date,
    userInfo_name,
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
