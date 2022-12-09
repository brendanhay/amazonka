{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    batchDescribeMergeConflicts_conflictDetailLevel,
    batchDescribeMergeConflicts_conflictResolutionStrategy,
    batchDescribeMergeConflicts_filePaths,
    batchDescribeMergeConflicts_maxConflictFiles,
    batchDescribeMergeConflicts_maxMergeHunks,
    batchDescribeMergeConflicts_nextToken,
    batchDescribeMergeConflicts_repositoryName,
    batchDescribeMergeConflicts_destinationCommitSpecifier,
    batchDescribeMergeConflicts_sourceCommitSpecifier,
    batchDescribeMergeConflicts_mergeOption,
    batchDescribeMergeConflictsResponse_baseCommitId,
    batchDescribeMergeConflictsResponse_errors,
    batchDescribeMergeConflictsResponse_nextToken,
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
    createCommit_authorName,
    createCommit_commitMessage,
    createCommit_deleteFiles,
    createCommit_email,
    createCommit_keepEmptyFolders,
    createCommit_parentCommitId,
    createCommit_putFiles,
    createCommit_setFileModes,
    createCommit_repositoryName,
    createCommit_branchName,
    createCommitResponse_commitId,
    createCommitResponse_filesAdded,
    createCommitResponse_filesDeleted,
    createCommitResponse_filesUpdated,
    createCommitResponse_treeId,
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
    createRepository_repositoryDescription,
    createRepository_tags,
    createRepository_repositoryName,
    createRepositoryResponse_repositoryMetadata,
    createRepositoryResponse_httpStatus,

    -- ** CreateUnreferencedMergeCommit
    createUnreferencedMergeCommit_authorName,
    createUnreferencedMergeCommit_commitMessage,
    createUnreferencedMergeCommit_conflictDetailLevel,
    createUnreferencedMergeCommit_conflictResolution,
    createUnreferencedMergeCommit_conflictResolutionStrategy,
    createUnreferencedMergeCommit_email,
    createUnreferencedMergeCommit_keepEmptyFolders,
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
    deleteFile_commitMessage,
    deleteFile_email,
    deleteFile_keepEmptyFolders,
    deleteFile_name,
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
    describeMergeConflicts_conflictDetailLevel,
    describeMergeConflicts_conflictResolutionStrategy,
    describeMergeConflicts_maxMergeHunks,
    describeMergeConflicts_nextToken,
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
    describePullRequestEvents_actorArn,
    describePullRequestEvents_maxResults,
    describePullRequestEvents_nextToken,
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
    getCommentReactions_maxResults,
    getCommentReactions_nextToken,
    getCommentReactions_reactionUserArn,
    getCommentReactions_commentId,
    getCommentReactionsResponse_nextToken,
    getCommentReactionsResponse_httpStatus,
    getCommentReactionsResponse_reactionsForComment,

    -- ** GetCommentsForComparedCommit
    getCommentsForComparedCommit_beforeCommitId,
    getCommentsForComparedCommit_maxResults,
    getCommentsForComparedCommit_nextToken,
    getCommentsForComparedCommit_repositoryName,
    getCommentsForComparedCommit_afterCommitId,
    getCommentsForComparedCommitResponse_commentsForComparedCommitData,
    getCommentsForComparedCommitResponse_nextToken,
    getCommentsForComparedCommitResponse_httpStatus,

    -- ** GetCommentsForPullRequest
    getCommentsForPullRequest_afterCommitId,
    getCommentsForPullRequest_beforeCommitId,
    getCommentsForPullRequest_maxResults,
    getCommentsForPullRequest_nextToken,
    getCommentsForPullRequest_repositoryName,
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
    getDifferences_maxResults,
    getDifferences_nextToken,
    getDifferences_afterPath,
    getDifferences_beforeCommitSpecifier,
    getDifferences_beforePath,
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
    getFolderResponse_subFolders,
    getFolderResponse_subModules,
    getFolderResponse_symbolicLinks,
    getFolderResponse_treeId,
    getFolderResponse_httpStatus,
    getFolderResponse_commitId,
    getFolderResponse_folderPath,

    -- ** GetMergeCommit
    getMergeCommit_conflictDetailLevel,
    getMergeCommit_conflictResolutionStrategy,
    getMergeCommit_repositoryName,
    getMergeCommit_sourceCommitSpecifier,
    getMergeCommit_destinationCommitSpecifier,
    getMergeCommitResponse_baseCommitId,
    getMergeCommitResponse_destinationCommitId,
    getMergeCommitResponse_mergedCommitId,
    getMergeCommitResponse_sourceCommitId,
    getMergeCommitResponse_httpStatus,

    -- ** GetMergeConflicts
    getMergeConflicts_conflictDetailLevel,
    getMergeConflicts_conflictResolutionStrategy,
    getMergeConflicts_maxConflictFiles,
    getMergeConflicts_nextToken,
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
    getRepositoryTriggersResponse_configurationId,
    getRepositoryTriggersResponse_triggers,
    getRepositoryTriggersResponse_httpStatus,

    -- ** ListApprovalRuleTemplates
    listApprovalRuleTemplates_maxResults,
    listApprovalRuleTemplates_nextToken,
    listApprovalRuleTemplatesResponse_approvalRuleTemplateNames,
    listApprovalRuleTemplatesResponse_nextToken,
    listApprovalRuleTemplatesResponse_httpStatus,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    listAssociatedApprovalRuleTemplatesForRepository_maxResults,
    listAssociatedApprovalRuleTemplatesForRepository_nextToken,
    listAssociatedApprovalRuleTemplatesForRepository_repositoryName,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_approvalRuleTemplateNames,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_nextToken,
    listAssociatedApprovalRuleTemplatesForRepositoryResponse_httpStatus,

    -- ** ListBranches
    listBranches_nextToken,
    listBranches_repositoryName,
    listBranchesResponse_branches,
    listBranchesResponse_nextToken,
    listBranchesResponse_httpStatus,

    -- ** ListPullRequests
    listPullRequests_authorArn,
    listPullRequests_maxResults,
    listPullRequests_nextToken,
    listPullRequests_pullRequestStatus,
    listPullRequests_repositoryName,
    listPullRequestsResponse_nextToken,
    listPullRequestsResponse_httpStatus,
    listPullRequestsResponse_pullRequestIds,

    -- ** ListRepositories
    listRepositories_nextToken,
    listRepositories_order,
    listRepositories_sortBy,
    listRepositoriesResponse_nextToken,
    listRepositoriesResponse_repositories,
    listRepositoriesResponse_httpStatus,

    -- ** ListRepositoriesForApprovalRuleTemplate
    listRepositoriesForApprovalRuleTemplate_maxResults,
    listRepositoriesForApprovalRuleTemplate_nextToken,
    listRepositoriesForApprovalRuleTemplate_approvalRuleTemplateName,
    listRepositoriesForApprovalRuleTemplateResponse_nextToken,
    listRepositoriesForApprovalRuleTemplateResponse_repositoryNames,
    listRepositoriesForApprovalRuleTemplateResponse_httpStatus,

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

    -- ** MergeBranchesBySquash
    mergeBranchesBySquash_authorName,
    mergeBranchesBySquash_commitMessage,
    mergeBranchesBySquash_conflictDetailLevel,
    mergeBranchesBySquash_conflictResolution,
    mergeBranchesBySquash_conflictResolutionStrategy,
    mergeBranchesBySquash_email,
    mergeBranchesBySquash_keepEmptyFolders,
    mergeBranchesBySquash_targetBranch,
    mergeBranchesBySquash_repositoryName,
    mergeBranchesBySquash_sourceCommitSpecifier,
    mergeBranchesBySquash_destinationCommitSpecifier,
    mergeBranchesBySquashResponse_commitId,
    mergeBranchesBySquashResponse_treeId,
    mergeBranchesBySquashResponse_httpStatus,

    -- ** MergeBranchesByThreeWay
    mergeBranchesByThreeWay_authorName,
    mergeBranchesByThreeWay_commitMessage,
    mergeBranchesByThreeWay_conflictDetailLevel,
    mergeBranchesByThreeWay_conflictResolution,
    mergeBranchesByThreeWay_conflictResolutionStrategy,
    mergeBranchesByThreeWay_email,
    mergeBranchesByThreeWay_keepEmptyFolders,
    mergeBranchesByThreeWay_targetBranch,
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
    mergePullRequestBySquash_authorName,
    mergePullRequestBySquash_commitMessage,
    mergePullRequestBySquash_conflictDetailLevel,
    mergePullRequestBySquash_conflictResolution,
    mergePullRequestBySquash_conflictResolutionStrategy,
    mergePullRequestBySquash_email,
    mergePullRequestBySquash_keepEmptyFolders,
    mergePullRequestBySquash_sourceCommitId,
    mergePullRequestBySquash_pullRequestId,
    mergePullRequestBySquash_repositoryName,
    mergePullRequestBySquashResponse_pullRequest,
    mergePullRequestBySquashResponse_httpStatus,

    -- ** MergePullRequestByThreeWay
    mergePullRequestByThreeWay_authorName,
    mergePullRequestByThreeWay_commitMessage,
    mergePullRequestByThreeWay_conflictDetailLevel,
    mergePullRequestByThreeWay_conflictResolution,
    mergePullRequestByThreeWay_conflictResolutionStrategy,
    mergePullRequestByThreeWay_email,
    mergePullRequestByThreeWay_keepEmptyFolders,
    mergePullRequestByThreeWay_sourceCommitId,
    mergePullRequestByThreeWay_pullRequestId,
    mergePullRequestByThreeWay_repositoryName,
    mergePullRequestByThreeWayResponse_pullRequest,
    mergePullRequestByThreeWayResponse_httpStatus,

    -- ** OverridePullRequestApprovalRules
    overridePullRequestApprovalRules_pullRequestId,
    overridePullRequestApprovalRules_revisionId,
    overridePullRequestApprovalRules_overrideStatus,

    -- ** PostCommentForComparedCommit
    postCommentForComparedCommit_beforeCommitId,
    postCommentForComparedCommit_clientRequestToken,
    postCommentForComparedCommit_location,
    postCommentForComparedCommit_repositoryName,
    postCommentForComparedCommit_afterCommitId,
    postCommentForComparedCommit_content,
    postCommentForComparedCommitResponse_afterBlobId,
    postCommentForComparedCommitResponse_afterCommitId,
    postCommentForComparedCommitResponse_beforeBlobId,
    postCommentForComparedCommitResponse_beforeCommitId,
    postCommentForComparedCommitResponse_comment,
    postCommentForComparedCommitResponse_location,
    postCommentForComparedCommitResponse_repositoryName,
    postCommentForComparedCommitResponse_httpStatus,

    -- ** PostCommentForPullRequest
    postCommentForPullRequest_clientRequestToken,
    postCommentForPullRequest_location,
    postCommentForPullRequest_pullRequestId,
    postCommentForPullRequest_repositoryName,
    postCommentForPullRequest_beforeCommitId,
    postCommentForPullRequest_afterCommitId,
    postCommentForPullRequest_content,
    postCommentForPullRequestResponse_afterBlobId,
    postCommentForPullRequestResponse_afterCommitId,
    postCommentForPullRequestResponse_beforeBlobId,
    postCommentForPullRequestResponse_beforeCommitId,
    postCommentForPullRequestResponse_comment,
    postCommentForPullRequestResponse_location,
    postCommentForPullRequestResponse_pullRequestId,
    postCommentForPullRequestResponse_repositoryName,
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
    putFile_commitMessage,
    putFile_email,
    putFile_fileMode,
    putFile_name,
    putFile_parentCommitId,
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
    approvalRule_approvalRuleContent,
    approvalRule_approvalRuleId,
    approvalRule_approvalRuleName,
    approvalRule_creationDate,
    approvalRule_lastModifiedDate,
    approvalRule_lastModifiedUser,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_ruleContentSha256,

    -- ** ApprovalRuleEventMetadata
    approvalRuleEventMetadata_approvalRuleContent,
    approvalRuleEventMetadata_approvalRuleId,
    approvalRuleEventMetadata_approvalRuleName,

    -- ** ApprovalRuleOverriddenEventMetadata
    approvalRuleOverriddenEventMetadata_overrideStatus,
    approvalRuleOverriddenEventMetadata_revisionId,

    -- ** ApprovalRuleTemplate
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_approvalRuleTemplateId,
    approvalRuleTemplate_approvalRuleTemplateName,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_lastModifiedUser,
    approvalRuleTemplate_ruleContentSha256,

    -- ** ApprovalStateChangedEventMetadata
    approvalStateChangedEventMetadata_approvalStatus,
    approvalStateChangedEventMetadata_revisionId,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,

    -- ** BatchDescribeMergeConflictsError
    batchDescribeMergeConflictsError_filePath,
    batchDescribeMergeConflictsError_exceptionName,
    batchDescribeMergeConflictsError_message,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,

    -- ** BatchGetCommitsError
    batchGetCommitsError_commitId,
    batchGetCommitsError_errorCode,
    batchGetCommitsError_errorMessage,

    -- ** BlobMetadata
    blobMetadata_blobId,
    blobMetadata_mode,
    blobMetadata_path,

    -- ** BranchInfo
    branchInfo_branchName,
    branchInfo_commitId,

    -- ** Comment
    comment_authorArn,
    comment_callerReactions,
    comment_clientRequestToken,
    comment_commentId,
    comment_content,
    comment_creationDate,
    comment_deleted,
    comment_inReplyTo,
    comment_lastModifiedDate,
    comment_reactionCounts,

    -- ** CommentsForComparedCommit
    commentsForComparedCommit_afterBlobId,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_comments,
    commentsForComparedCommit_location,
    commentsForComparedCommit_repositoryName,

    -- ** CommentsForPullRequest
    commentsForPullRequest_afterBlobId,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_comments,
    commentsForPullRequest_location,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_repositoryName,

    -- ** Commit
    commit_additionalData,
    commit_author,
    commit_commitId,
    commit_committer,
    commit_message,
    commit_parents,
    commit_treeId,

    -- ** Conflict
    conflict_conflictMetadata,
    conflict_mergeHunks,

    -- ** ConflictMetadata
    conflictMetadata_contentConflict,
    conflictMetadata_fileModeConflict,
    conflictMetadata_fileModes,
    conflictMetadata_filePath,
    conflictMetadata_fileSizes,
    conflictMetadata_isBinaryFile,
    conflictMetadata_mergeOperations,
    conflictMetadata_numberOfConflicts,
    conflictMetadata_objectTypeConflict,
    conflictMetadata_objectTypes,

    -- ** ConflictResolution
    conflictResolution_deleteFiles,
    conflictResolution_replaceContents,
    conflictResolution_setFileModes,

    -- ** DeleteFileEntry
    deleteFileEntry_filePath,

    -- ** Difference
    difference_afterBlob,
    difference_beforeBlob,
    difference_changeType,

    -- ** Evaluation
    evaluation_approvalRulesNotSatisfied,
    evaluation_approvalRulesSatisfied,
    evaluation_approved,
    evaluation_overridden,

    -- ** File
    file_absolutePath,
    file_blobId,
    file_fileMode,
    file_relativePath,

    -- ** FileMetadata
    fileMetadata_absolutePath,
    fileMetadata_blobId,
    fileMetadata_fileMode,

    -- ** FileModes
    fileModes_base,
    fileModes_destination,
    fileModes_source,

    -- ** FileSizes
    fileSizes_base,
    fileSizes_destination,
    fileSizes_source,

    -- ** Folder
    folder_absolutePath,
    folder_relativePath,
    folder_treeId,

    -- ** IsBinaryFile
    isBinaryFile_base,
    isBinaryFile_destination,
    isBinaryFile_source,

    -- ** Location
    location_filePath,
    location_filePosition,
    location_relativeFileVersion,

    -- ** MergeHunk
    mergeHunk_base,
    mergeHunk_destination,
    mergeHunk_isConflict,
    mergeHunk_source,

    -- ** MergeHunkDetail
    mergeHunkDetail_endLine,
    mergeHunkDetail_hunkContent,
    mergeHunkDetail_startLine,

    -- ** MergeMetadata
    mergeMetadata_isMerged,
    mergeMetadata_mergeCommitId,
    mergeMetadata_mergeOption,
    mergeMetadata_mergedBy,

    -- ** MergeOperations
    mergeOperations_destination,
    mergeOperations_source,

    -- ** ObjectTypes
    objectTypes_base,
    objectTypes_destination,
    objectTypes_source,

    -- ** OriginApprovalRuleTemplate
    originApprovalRuleTemplate_approvalRuleTemplateId,
    originApprovalRuleTemplate_approvalRuleTemplateName,

    -- ** PullRequest
    pullRequest_approvalRules,
    pullRequest_authorArn,
    pullRequest_clientRequestToken,
    pullRequest_creationDate,
    pullRequest_description,
    pullRequest_lastActivityDate,
    pullRequest_pullRequestId,
    pullRequest_pullRequestStatus,
    pullRequest_pullRequestTargets,
    pullRequest_revisionId,
    pullRequest_title,

    -- ** PullRequestCreatedEventMetadata
    pullRequestCreatedEventMetadata_destinationCommitId,
    pullRequestCreatedEventMetadata_mergeBase,
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_sourceCommitId,

    -- ** PullRequestEvent
    pullRequestEvent_actorArn,
    pullRequestEvent_approvalRuleEventMetadata,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,
    pullRequestEvent_approvalStateChangedEventMetadata,
    pullRequestEvent_eventDate,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_pullRequestEventType,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_pullRequestMergedStateChangedEventMetadata,
    pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestEvent_pullRequestStatusChangedEventMetadata,

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
    pullRequestTarget_destinationCommit,
    pullRequestTarget_destinationReference,
    pullRequestTarget_mergeBase,
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_repositoryName,
    pullRequestTarget_sourceCommit,
    pullRequestTarget_sourceReference,

    -- ** PutFileEntry
    putFileEntry_fileContent,
    putFileEntry_fileMode,
    putFileEntry_sourceFile,
    putFileEntry_filePath,

    -- ** ReactionForComment
    reactionForComment_reaction,
    reactionForComment_reactionUsers,
    reactionForComment_reactionsFromDeletedUsersCount,

    -- ** ReactionValueFormats
    reactionValueFormats_emoji,
    reactionValueFormats_shortCode,
    reactionValueFormats_unicode,

    -- ** ReplaceContentEntry
    replaceContentEntry_content,
    replaceContentEntry_fileMode,
    replaceContentEntry_filePath,
    replaceContentEntry_replacementType,

    -- ** RepositoryMetadata
    repositoryMetadata_arn,
    repositoryMetadata_accountId,
    repositoryMetadata_cloneUrlHttp,
    repositoryMetadata_cloneUrlSsh,
    repositoryMetadata_creationDate,
    repositoryMetadata_defaultBranch,
    repositoryMetadata_lastModifiedDate,
    repositoryMetadata_repositoryDescription,
    repositoryMetadata_repositoryId,
    repositoryMetadata_repositoryName,

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
    subModule_absolutePath,
    subModule_commitId,
    subModule_relativePath,

    -- ** SymbolicLink
    symbolicLink_absolutePath,
    symbolicLink_blobId,
    symbolicLink_fileMode,
    symbolicLink_relativePath,

    -- ** Target
    target_destinationReference,
    target_repositoryName,
    target_sourceReference,

    -- ** UserInfo
    userInfo_date,
    userInfo_email,
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
