{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types
  ( -- * Service Configuration
    codeCommit,

    -- * Errors

    -- * ApprovalState
    ApprovalState (..),

    -- * ChangeTypeEnum
    ChangeTypeEnum (..),

    -- * ConflictDetailLevelTypeEnum
    ConflictDetailLevelTypeEnum (..),

    -- * ConflictResolutionStrategyTypeEnum
    ConflictResolutionStrategyTypeEnum (..),

    -- * FileModeTypeEnum
    FileModeTypeEnum (..),

    -- * MergeOptionTypeEnum
    MergeOptionTypeEnum (..),

    -- * ObjectTypeEnum
    ObjectTypeEnum (..),

    -- * OrderEnum
    OrderEnum (..),

    -- * OverrideStatus
    OverrideStatus (..),

    -- * PullRequestEventType
    PullRequestEventType (..),

    -- * PullRequestStatusEnum
    PullRequestStatusEnum (..),

    -- * RelativeFileVersionEnum
    RelativeFileVersionEnum (..),

    -- * ReplacementTypeEnum
    ReplacementTypeEnum (..),

    -- * RepositoryTriggerEventEnum
    RepositoryTriggerEventEnum (..),

    -- * SortByEnum
    SortByEnum (..),

    -- * Approval
    Approval,
    approval,
    aApprovalState,
    aUserARN,

    -- * ApprovalRule
    ApprovalRule,
    approvalRule,
    arRuleContentSha256,
    arLastModifiedDate,
    arApprovalRuleName,
    arApprovalRuleId,
    arLastModifiedUser,
    arOriginApprovalRuleTemplate,
    arCreationDate,
    arApprovalRuleContent,

    -- * ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata,
    approvalRuleEventMetadata,
    aremApprovalRuleName,
    aremApprovalRuleId,
    aremApprovalRuleContent,

    -- * ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata,
    approvalRuleOverriddenEventMetadata,
    aroemOverrideStatus,
    aroemRevisionId,

    -- * ApprovalRuleTemplate
    ApprovalRuleTemplate,
    approvalRuleTemplate,
    artRuleContentSha256,
    artApprovalRuleTemplateId,
    artLastModifiedDate,
    artApprovalRuleTemplateDescription,
    artApprovalRuleTemplateContent,
    artLastModifiedUser,
    artCreationDate,
    artApprovalRuleTemplateName,

    -- * ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata,
    approvalStateChangedEventMetadata,
    ascemApprovalStatus,
    ascemRevisionId,

    -- * BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError,
    batchAssociateApprovalRuleTemplateWithRepositoriesError,
    baartwreErrorCode,
    baartwreRepositoryName,
    baartwreErrorMessage,

    -- * BatchDescribeMergeConflictsError
    BatchDescribeMergeConflictsError,
    batchDescribeMergeConflictsError,
    bdmceFilePath,
    bdmceExceptionName,
    bdmceMessage,

    -- * BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError,
    bdartfreErrorCode,
    bdartfreRepositoryName,
    bdartfreErrorMessage,

    -- * BatchGetCommitsError
    BatchGetCommitsError,
    batchGetCommitsError,
    bgceCommitId,
    bgceErrorCode,
    bgceErrorMessage,

    -- * BlobMetadata
    BlobMetadata,
    blobMetadata,
    bmPath,
    bmMode,
    bmBlobId,

    -- * BranchInfo
    BranchInfo,
    branchInfo,
    biCommitId,
    biBranchName,

    -- * Comment
    Comment,
    comment,
    cLastModifiedDate,
    cAuthorARN,
    cContent,
    cCallerReactions,
    cCreationDate,
    cDeleted,
    cClientRequestToken,
    cCommentId,
    cInReplyTo,
    cReactionCounts,

    -- * CommentsForComparedCommit
    CommentsForComparedCommit,
    commentsForComparedCommit,
    cfccBeforeBlobId,
    cfccLocation,
    cfccAfterCommitId,
    cfccAfterBlobId,
    cfccBeforeCommitId,
    cfccRepositoryName,
    cfccComments,

    -- * CommentsForPullRequest
    CommentsForPullRequest,
    commentsForPullRequest,
    cfprBeforeBlobId,
    cfprLocation,
    cfprAfterCommitId,
    cfprPullRequestId,
    cfprAfterBlobId,
    cfprBeforeCommitId,
    cfprRepositoryName,
    cfprComments,

    -- * Commit
    Commit,
    commit,
    cCommitId,
    cCommitter,
    cTreeId,
    cAdditionalData,
    cParents,
    cAuthor,
    cMessage,

    -- * Conflict
    Conflict,
    conflict,
    cMergeHunks,
    cConflictMetadata,

    -- * ConflictMetadata
    ConflictMetadata,
    conflictMetadata,
    cmNumberOfConflicts,
    cmContentConflict,
    cmFileSizes,
    cmFilePath,
    cmIsBinaryFile,
    cmFileModeConflict,
    cmObjectTypeConflict,
    cmMergeOperations,
    cmObjectTypes,
    cmFileModes,

    -- * ConflictResolution
    ConflictResolution,
    conflictResolution,
    crSetFileModes,
    crDeleteFiles,
    crReplaceContents,

    -- * DeleteFileEntry
    DeleteFileEntry,
    deleteFileEntry,
    dfeFilePath,

    -- * Difference
    Difference,
    difference,
    dAfterBlob,
    dBeforeBlob,
    dChangeType,

    -- * Evaluation
    Evaluation,
    evaluation,
    eApprovalRulesSatisfied,
    eApprovalRulesNotSatisfied,
    eApproved,
    eOverridden,

    -- * File
    File,
    file,
    fAbsolutePath,
    fFileMode,
    fBlobId,
    fRelativePath,

    -- * FileMetadata
    FileMetadata,
    fileMetadata,
    fmAbsolutePath,
    fmFileMode,
    fmBlobId,

    -- * FileModes
    FileModes,
    fileModes,
    fmDestination,
    fmBase,
    fmSource,

    -- * FileSizes
    FileSizes,
    fileSizes,
    fsDestination,
    fsBase,
    fsSource,

    -- * Folder
    Folder,
    folder,
    folAbsolutePath,
    folTreeId,
    folRelativePath,

    -- * IsBinaryFile
    IsBinaryFile,
    isBinaryFile,
    ibfDestination,
    ibfBase,
    ibfSource,

    -- * Location
    Location,
    location,
    lRelativeFileVersion,
    lFilePath,
    lFilePosition,

    -- * MergeHunk
    MergeHunk,
    mergeHunk,
    mhDestination,
    mhBase,
    mhIsConflict,
    mhSource,

    -- * MergeHunkDetail
    MergeHunkDetail,
    mergeHunkDetail,
    mhdStartLine,
    mhdEndLine,
    mhdHunkContent,

    -- * MergeMetadata
    MergeMetadata,
    mergeMetadata,
    mmMergedBy,
    mmMergeOption,
    mmIsMerged,
    mmMergeCommitId,

    -- * MergeOperations
    MergeOperations,
    mergeOperations,
    moDestination,
    moSource,

    -- * ObjectTypes
    ObjectTypes,
    objectTypes,
    otDestination,
    otBase,
    otSource,

    -- * OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate,
    originApprovalRuleTemplate,
    oartApprovalRuleTemplateId,
    oartApprovalRuleTemplateName,

    -- * PullRequest
    PullRequest,
    pullRequest,
    prApprovalRules,
    prAuthorARN,
    prPullRequestId,
    prCreationDate,
    prPullRequestStatus,
    prTitle,
    prClientRequestToken,
    prLastActivityDate,
    prRevisionId,
    prPullRequestTargets,
    prDescription,

    -- * PullRequestCreatedEventMetadata
    PullRequestCreatedEventMetadata,
    pullRequestCreatedEventMetadata,
    prcemDestinationCommitId,
    prcemMergeBase,
    prcemRepositoryName,
    prcemSourceCommitId,

    -- * PullRequestEvent
    PullRequestEvent,
    pullRequestEvent,
    prePullRequestMergedStateChangedEventMetadata,
    prePullRequestCreatedEventMetadata,
    preApprovalRuleEventMetadata,
    prePullRequestEventType,
    prePullRequestStatusChangedEventMetadata,
    preActorARN,
    prePullRequestId,
    preEventDate,
    preApprovalStateChangedEventMetadata,
    prePullRequestSourceReferenceUpdatedEventMetadata,
    preApprovalRuleOverriddenEventMetadata,

    -- * PullRequestMergedStateChangedEventMetadata
    PullRequestMergedStateChangedEventMetadata,
    pullRequestMergedStateChangedEventMetadata,
    prmscemDestinationReference,
    prmscemMergeMetadata,
    prmscemRepositoryName,

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestSourceReferenceUpdatedEventMetadata,
    prsruemAfterCommitId,
    prsruemBeforeCommitId,
    prsruemMergeBase,
    prsruemRepositoryName,

    -- * PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata,
    pullRequestStatusChangedEventMetadata,
    prscemPullRequestStatus,

    -- * PullRequestTarget
    PullRequestTarget,
    pullRequestTarget,
    prtSourceCommit,
    prtDestinationReference,
    prtMergeMetadata,
    prtMergeBase,
    prtDestinationCommit,
    prtRepositoryName,
    prtSourceReference,

    -- * PutFileEntry
    PutFileEntry,
    putFileEntry,
    pfeFileContent,
    pfeFileMode,
    pfeSourceFile,
    pfeFilePath,

    -- * ReactionForComment
    ReactionForComment,
    reactionForComment,
    rfcReactionUsers,
    rfcReactionsFromDeletedUsersCount,
    rfcReaction,

    -- * ReactionValueFormats
    ReactionValueFormats,
    reactionValueFormats,
    rvfEmoji,
    rvfShortCode,
    rvfUnicode,

    -- * ReplaceContentEntry
    ReplaceContentEntry,
    replaceContentEntry,
    rceFileMode,
    rceContent,
    rceFilePath,
    rceReplacementType,

    -- * RepositoryMetadata
    RepositoryMetadata,
    repositoryMetadata,
    rmRepositoryDescription,
    rmLastModifiedDate,
    rmARN,
    rmCloneURLHTTP,
    rmAccountId,
    rmDefaultBranch,
    rmRepositoryId,
    rmRepositoryName,
    rmCreationDate,
    rmCloneURLSSH,

    -- * RepositoryNameIdPair
    RepositoryNameIdPair,
    repositoryNameIdPair,
    rnipRepositoryId,
    rnipRepositoryName,

    -- * RepositoryTrigger
    RepositoryTrigger,
    repositoryTrigger,
    rtBranches,
    rtCustomData,
    rtName,
    rtDestinationARN,
    rtEvents,

    -- * RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure,
    repositoryTriggerExecutionFailure,
    rtefFailureMessage,
    rtefTrigger,

    -- * SetFileModeEntry
    SetFileModeEntry,
    setFileModeEntry,
    sfmeFilePath,
    sfmeFileMode,

    -- * SourceFileSpecifier
    SourceFileSpecifier,
    sourceFileSpecifier,
    sfsIsMove,
    sfsFilePath,

    -- * SubModule
    SubModule,
    subModule,
    smCommitId,
    smAbsolutePath,
    smRelativePath,

    -- * SymbolicLink
    SymbolicLink,
    symbolicLink,
    slAbsolutePath,
    slFileMode,
    slBlobId,
    slRelativePath,

    -- * Target
    Target,
    target,
    tDestinationReference,
    tRepositoryName,
    tSourceReference,

    -- * UserInfo
    UserInfo,
    userInfo,
    uiEmail,
    uiDate,
    uiName,
  )
where

import Network.AWS.CodeCommit.Types.Approval
import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.ApprovalState
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
import Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Network.AWS.CodeCommit.Types.BatchGetCommitsError
import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.BranchInfo
import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.CommentsForComparedCommit
import Network.AWS.CodeCommit.Types.CommentsForPullRequest
import Network.AWS.CodeCommit.Types.Commit
import Network.AWS.CodeCommit.Types.Conflict
import Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.ConflictResolution
import Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.Difference
import Network.AWS.CodeCommit.Types.Evaluation
import Network.AWS.CodeCommit.Types.File
import Network.AWS.CodeCommit.Types.FileMetadata
import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.Folder
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.Location
import Network.AWS.CodeCommit.Types.MergeHunk
import Network.AWS.CodeCommit.Types.MergeHunkDetail
import Network.AWS.CodeCommit.Types.MergeMetadata
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
import Network.AWS.CodeCommit.Types.ObjectTypeEnum
import Network.AWS.CodeCommit.Types.ObjectTypes
import Network.AWS.CodeCommit.Types.OrderEnum
import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.OverrideStatus
import Network.AWS.CodeCommit.Types.PullRequest
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEvent
import Network.AWS.CodeCommit.Types.PullRequestEventType
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.CodeCommit.Types.PullRequestTarget
import Network.AWS.CodeCommit.Types.PutFileEntry
import Network.AWS.CodeCommit.Types.ReactionForComment
import Network.AWS.CodeCommit.Types.ReactionValueFormats
import Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.ReplacementTypeEnum
import Network.AWS.CodeCommit.Types.RepositoryMetadata
import Network.AWS.CodeCommit.Types.RepositoryNameIdPair
import Network.AWS.CodeCommit.Types.RepositoryTrigger
import Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
import Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import Network.AWS.CodeCommit.Types.SortByEnum
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import Network.AWS.CodeCommit.Types.SubModule
import Network.AWS.CodeCommit.Types.SymbolicLink
import Network.AWS.CodeCommit.Types.Target
import Network.AWS.CodeCommit.Types.UserInfo
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
codeCommit :: Service
codeCommit =
  Service
    { _svcAbbrev = "CodeCommit",
      _svcSigner = v4,
      _svcPrefix = "codecommit",
      _svcVersion = "2015-04-13",
      _svcEndpoint = defaultEndpoint codeCommit,
      _svcTimeout = Just 70,
      _svcCheck = statusSuccess,
      _svcError = parseJSONError "CodeCommit",
      _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2,
          _retryGrowth = 2,
          _retryAttempts = 5,
          _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has
          (hasCode "ProvisionedThroughputExceededException" . hasStatus 400)
          e =
        Just "throughput_exceeded"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing
