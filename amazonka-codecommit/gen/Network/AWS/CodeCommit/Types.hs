{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _InvalidConflictDetailLevelException,
    _ActorDoesNotExistException,
    _CommitRequiredException,
    _ApprovalRuleNameAlreadyExistsException,
    _PullRequestIdRequiredException,
    _ReferenceDoesNotExistException,
    _NameLengthExceededException,
    _InvalidPullRequestStatusUpdateException,
    _InvalidPullRequestIdException,
    _TargetRequiredException,
    _FileNameConflictsWithDirectoryNameException,
    _InvalidTargetsException,
    _CommentContentRequiredException,
    _FileEntryRequiredException,
    _InvalidSystemTagUsageException,
    _BranchNameExistsException,
    _EncryptionKeyNotFoundException,
    _TargetsRequiredException,
    _ManualMergeRequiredException,
    _InvalidApprovalRuleTemplateDescriptionException,
    _SourceAndDestinationAreSameException,
    _EncryptionIntegrityChecksFailedException,
    _TooManyTagsException,
    _SourceFileOrContentRequiredException,
    _CommentDeletedException,
    _SamePathRequestException,
    _RepositoryTriggerEventsListRequiredException,
    _ParentCommitDoesNotExistException,
    _AuthorDoesNotExistException,
    _InvalidReplacementContentException,
    _FileContentRequiredException,
    _TitleRequiredException,
    _InvalidConflictResolutionException,
    _ApprovalRuleContentRequiredException,
    _PullRequestApprovalRulesNotSatisfiedException,
    _PullRequestStatusRequiredException,
    _InvalidPullRequestStatusException,
    _InvalidTitleException,
    _InvalidSourceCommitSpecifierException,
    _ParentCommitIdRequiredException,
    _RepositoryDoesNotExistException,
    _InvalidApprovalRuleContentException,
    _InvalidOverrideStatusException,
    _MaximumItemsToCompareExceededException,
    _RepositoryTriggerNameRequiredException,
    _NumberOfRuleTemplatesExceededException,
    _RepositoryNameRequiredException,
    _InvalidFileModeException,
    _InvalidRepositoryTriggerEventsException,
    _InvalidRepositoryTriggerNameException,
    _FileContentSizeLimitExceededException,
    _InvalidPathException,
    _InvalidTargetBranchException,
    _FileTooLargeException,
    _InvalidBlobIdException,
    _InvalidApprovalRuleNameException,
    _ApprovalRuleTemplateNameRequiredException,
    _CannotDeleteApprovalRuleFromTemplateException,
    _InvalidActorArnException,
    _PullRequestDoesNotExistException,
    _RepositoryNamesRequiredException,
    _CommitIdDoesNotExistException,
    _MaximumFileEntriesExceededException,
    _ReplacementTypeRequiredException,
    _InvalidReplacementTypeException,
    _RevisionIdRequiredException,
    _ReactionLimitExceededException,
    _RevisionNotCurrentException,
    _IdempotencyParameterMismatchException,
    _MultipleConflictResolutionEntriesException,
    _BranchNameRequiredException,
    _RepositoryTriggerBranchNameListRequiredException,
    _RepositoryTriggersListRequiredException,
    _RepositoryTriggerDestinationArnRequiredException,
    _EncryptionKeyUnavailableException,
    _TagPolicyException,
    _InvalidRepositoryTriggerDestinationArnException,
    _InvalidRelativeFileVersionEnumException,
    _InvalidConflictResolutionStrategyException,
    _ApprovalRuleDoesNotExistException,
    _InvalidClientRequestTokenException,
    _ReferenceNameRequiredException,
    _MaximumNumberOfApprovalsExceededException,
    _CommitMessageLengthExceededException,
    _MaximumRuleTemplatesAssociatedWithRepositoryException,
    _TagKeysListRequiredException,
    _InvalidReferenceNameException,
    _FileDoesNotExistException,
    _InvalidTagKeysListException,
    _CommitIdRequiredException,
    _OverrideAlreadySetException,
    _CommentDoesNotExistException,
    _InvalidDestinationCommitSpecifierException,
    _CommentNotCreatedByCallerException,
    _InvalidTargetException,
    _ReferenceTypeNotSupportedException,
    _InvalidSortByException,
    _EncryptionKeyDisabledException,
    _InvalidCommitException,
    _ApprovalRuleTemplateNameAlreadyExistsException,
    _InvalidRepositoryTriggerCustomDataException,
    _InvalidRepositoryTriggerBranchNameException,
    _DirectoryNameConflictsWithFileNameException,
    _MaximumOpenPullRequestsExceededException,
    _PullRequestAlreadyClosedException,
    _ReactionValueRequiredException,
    _EncryptionKeyAccessDeniedException,
    _InvalidRepositoryTriggerRegionException,
    _InvalidReactionValueException,
    _InvalidContinuationTokenException,
    _TipsDivergenceExceededException,
    _RestrictedSourceFileException,
    _InvalidFilePositionException,
    _InvalidMaxMergeHunksException,
    _ReplacementContentRequiredException,
    _ResourceArnRequiredException,
    _CannotModifyApprovalRuleFromTemplateException,
    _ParentCommitIdOutdatedException,
    _InvalidMaxConflictFilesException,
    _PathDoesNotExistException,
    _InvalidResourceArnException,
    _ApprovalRuleTemplateContentRequiredException,
    _MaximumBranchesExceededException,
    _OverrideStatusRequiredException,
    _ApprovalStateRequiredException,
    _InvalidPullRequestEventTypeException,
    _CommitIdsLimitExceededException,
    _RepositoryNotAssociatedWithPullRequestException,
    _MaximumFileContentToLoadExceededException,
    _ConcurrentReferenceUpdateException,
    _InvalidParentCommitIdException,
    _MaximumConflictResolutionEntriesExceededException,
    _CommentContentSizeLimitExceededException,
    _InvalidApprovalRuleTemplateContentException,
    _InvalidApprovalStateException,
    _InvalidAuthorArnException,
    _PathRequiredException,
    _InvalidTagsMapException,
    _CommitIdsListRequiredException,
    _BranchNameIsTagNameException,
    _DefaultBranchCannotBeDeletedException,
    _FileModeRequiredException,
    _TagsMapRequiredException,
    _FolderContentSizeLimitExceededException,
    _InvalidRepositoryNameException,
    _NumberOfRulesExceededException,
    _InvalidReactionUserArnException,
    _InvalidDeletionParameterException,
    _RepositoryLimitExceededException,
    _FilePathConflictsWithSubmodulePathException,
    _BranchDoesNotExistException,
    _InvalidDescriptionException,
    _InvalidCommentIdException,
    _InvalidOrderException,
    _BlobIdRequiredException,
    _InvalidApprovalRuleTemplateNameException,
    _MultipleRepositoriesInPullRequestException,
    _InvalidMaxResultsException,
    _ApprovalRuleNameRequiredException,
    _InvalidRevisionIdException,
    _PullRequestCannotBeApprovedByAuthorException,
    _CommentIdRequiredException,
    _NoChangeException,
    _InvalidBranchNameException,
    _InvalidMergeOptionException,
    _CommitDoesNotExistException,
    _BeforeCommitIdAndAfterCommitIdAreSameException,
    _MaximumRepositoryTriggersExceededException,
    _MergeOptionRequiredException,
    _InvalidFileLocationException,
    _ApprovalRuleTemplateDoesNotExistException,
    _ClientRequestTokenRequiredException,
    _InvalidCommitIdException,
    _FileContentAndSourceFileSpecifiedException,
    _SameFileContentException,
    _TipOfSourceReferenceIsDifferentException,
    _InvalidEmailException,
    _MaximumRepositoryNamesExceededException,
    _RepositoryNameExistsException,
    _PutFileEntryConflictException,
    _FolderDoesNotExistException,
    _InvalidRepositoryDescriptionException,
    _InvalidRuleContentSha256Exception,
    _BlobIdDoesNotExistException,
    _ApprovalRuleTemplateInUseException,

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
    Approval (..),
    newApproval,
    approval_userArn,
    approval_approvalState,

    -- * ApprovalRule
    ApprovalRule (..),
    newApprovalRule,
    approvalRule_lastModifiedDate,
    approvalRule_approvalRuleContent,
    approvalRule_creationDate,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_ruleContentSha256,
    approvalRule_approvalRuleId,
    approvalRule_approvalRuleName,
    approvalRule_lastModifiedUser,

    -- * ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata (..),
    newApprovalRuleEventMetadata,
    approvalRuleEventMetadata_approvalRuleContent,
    approvalRuleEventMetadata_approvalRuleId,
    approvalRuleEventMetadata_approvalRuleName,

    -- * ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata (..),
    newApprovalRuleOverriddenEventMetadata,
    approvalRuleOverriddenEventMetadata_revisionId,
    approvalRuleOverriddenEventMetadata_overrideStatus,

    -- * ApprovalRuleTemplate
    ApprovalRuleTemplate (..),
    newApprovalRuleTemplate,
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_approvalRuleTemplateId,
    approvalRuleTemplate_approvalRuleTemplateName,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_ruleContentSha256,
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_lastModifiedUser,

    -- * ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata (..),
    newApprovalStateChangedEventMetadata,
    approvalStateChangedEventMetadata_revisionId,
    approvalStateChangedEventMetadata_approvalStatus,

    -- * BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError (..),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesError,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorCode,

    -- * BatchDescribeMergeConflictsError
    BatchDescribeMergeConflictsError (..),
    newBatchDescribeMergeConflictsError,
    batchDescribeMergeConflictsError_filePath,
    batchDescribeMergeConflictsError_exceptionName,
    batchDescribeMergeConflictsError_message,

    -- * BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError (..),
    newBatchDisassociateApprovalRuleTemplateFromRepositoriesError,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorCode,

    -- * BatchGetCommitsError
    BatchGetCommitsError (..),
    newBatchGetCommitsError,
    batchGetCommitsError_commitId,
    batchGetCommitsError_errorMessage,
    batchGetCommitsError_errorCode,

    -- * BlobMetadata
    BlobMetadata (..),
    newBlobMetadata,
    blobMetadata_mode,
    blobMetadata_blobId,
    blobMetadata_path,

    -- * BranchInfo
    BranchInfo (..),
    newBranchInfo,
    branchInfo_commitId,
    branchInfo_branchName,

    -- * Comment
    Comment (..),
    newComment,
    comment_callerReactions,
    comment_lastModifiedDate,
    comment_creationDate,
    comment_content,
    comment_reactionCounts,
    comment_inReplyTo,
    comment_commentId,
    comment_clientRequestToken,
    comment_authorArn,
    comment_deleted,

    -- * CommentsForComparedCommit
    CommentsForComparedCommit (..),
    newCommentsForComparedCommit,
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_repositoryName,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_afterBlobId,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_comments,
    commentsForComparedCommit_location,

    -- * CommentsForPullRequest
    CommentsForPullRequest (..),
    newCommentsForPullRequest,
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_afterBlobId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_comments,
    commentsForPullRequest_location,

    -- * Commit
    Commit (..),
    newCommit,
    commit_parents,
    commit_commitId,
    commit_additionalData,
    commit_message,
    commit_treeId,
    commit_author,
    commit_committer,

    -- * Conflict
    Conflict (..),
    newConflict,
    conflict_conflictMetadata,
    conflict_mergeHunks,

    -- * ConflictMetadata
    ConflictMetadata (..),
    newConflictMetadata,
    conflictMetadata_mergeOperations,
    conflictMetadata_filePath,
    conflictMetadata_isBinaryFile,
    conflictMetadata_objectTypeConflict,
    conflictMetadata_fileModeConflict,
    conflictMetadata_numberOfConflicts,
    conflictMetadata_contentConflict,
    conflictMetadata_fileModes,
    conflictMetadata_objectTypes,
    conflictMetadata_fileSizes,

    -- * ConflictResolution
    ConflictResolution (..),
    newConflictResolution,
    conflictResolution_deleteFiles,
    conflictResolution_setFileModes,
    conflictResolution_replaceContents,

    -- * DeleteFileEntry
    DeleteFileEntry (..),
    newDeleteFileEntry,
    deleteFileEntry_filePath,

    -- * Difference
    Difference (..),
    newDifference,
    difference_changeType,
    difference_afterBlob,
    difference_beforeBlob,

    -- * Evaluation
    Evaluation (..),
    newEvaluation,
    evaluation_overridden,
    evaluation_approvalRulesSatisfied,
    evaluation_approved,
    evaluation_approvalRulesNotSatisfied,

    -- * File
    File (..),
    newFile,
    file_absolutePath,
    file_relativePath,
    file_blobId,
    file_fileMode,

    -- * FileMetadata
    FileMetadata (..),
    newFileMetadata,
    fileMetadata_absolutePath,
    fileMetadata_blobId,
    fileMetadata_fileMode,

    -- * FileModes
    FileModes (..),
    newFileModes,
    fileModes_source,
    fileModes_destination,
    fileModes_base,

    -- * FileSizes
    FileSizes (..),
    newFileSizes,
    fileSizes_source,
    fileSizes_destination,
    fileSizes_base,

    -- * Folder
    Folder (..),
    newFolder,
    folder_treeId,
    folder_absolutePath,
    folder_relativePath,

    -- * IsBinaryFile
    IsBinaryFile (..),
    newIsBinaryFile,
    isBinaryFile_source,
    isBinaryFile_destination,
    isBinaryFile_base,

    -- * Location
    Location (..),
    newLocation,
    location_filePath,
    location_filePosition,
    location_relativeFileVersion,

    -- * MergeHunk
    MergeHunk (..),
    newMergeHunk,
    mergeHunk_source,
    mergeHunk_destination,
    mergeHunk_isConflict,
    mergeHunk_base,

    -- * MergeHunkDetail
    MergeHunkDetail (..),
    newMergeHunkDetail,
    mergeHunkDetail_hunkContent,
    mergeHunkDetail_startLine,
    mergeHunkDetail_endLine,

    -- * MergeMetadata
    MergeMetadata (..),
    newMergeMetadata,
    mergeMetadata_mergedBy,
    mergeMetadata_mergeCommitId,
    mergeMetadata_isMerged,
    mergeMetadata_mergeOption,

    -- * MergeOperations
    MergeOperations (..),
    newMergeOperations,
    mergeOperations_source,
    mergeOperations_destination,

    -- * ObjectTypes
    ObjectTypes (..),
    newObjectTypes,
    objectTypes_source,
    objectTypes_destination,
    objectTypes_base,

    -- * OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate (..),
    newOriginApprovalRuleTemplate,
    originApprovalRuleTemplate_approvalRuleTemplateId,
    originApprovalRuleTemplate_approvalRuleTemplateName,

    -- * PullRequest
    PullRequest (..),
    newPullRequest,
    pullRequest_pullRequestTargets,
    pullRequest_revisionId,
    pullRequest_title,
    pullRequest_pullRequestStatus,
    pullRequest_creationDate,
    pullRequest_pullRequestId,
    pullRequest_description,
    pullRequest_lastActivityDate,
    pullRequest_clientRequestToken,
    pullRequest_authorArn,
    pullRequest_approvalRules,

    -- * PullRequestCreatedEventMetadata
    PullRequestCreatedEventMetadata (..),
    newPullRequestCreatedEventMetadata,
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_sourceCommitId,
    pullRequestCreatedEventMetadata_destinationCommitId,
    pullRequestCreatedEventMetadata_mergeBase,

    -- * PullRequestEvent
    PullRequestEvent (..),
    newPullRequestEvent,
    pullRequestEvent_pullRequestMergedStateChangedEventMetadata,
    pullRequestEvent_pullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestEvent_approvalStateChangedEventMetadata,
    pullRequestEvent_eventDate,
    pullRequestEvent_pullRequestEventType,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_actorArn,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,
    pullRequestEvent_pullRequestStatusChangedEventMetadata,
    pullRequestEvent_approvalRuleEventMetadata,

    -- * PullRequestMergedStateChangedEventMetadata
    PullRequestMergedStateChangedEventMetadata (..),
    newPullRequestMergedStateChangedEventMetadata,
    pullRequestMergedStateChangedEventMetadata_destinationReference,
    pullRequestMergedStateChangedEventMetadata_mergeMetadata,
    pullRequestMergedStateChangedEventMetadata_repositoryName,

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata (..),
    newPullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestSourceReferenceUpdatedEventMetadata_repositoryName,
    pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_mergeBase,

    -- * PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata (..),
    newPullRequestStatusChangedEventMetadata,
    pullRequestStatusChangedEventMetadata_pullRequestStatus,

    -- * PullRequestTarget
    PullRequestTarget (..),
    newPullRequestTarget,
    pullRequestTarget_destinationReference,
    pullRequestTarget_sourceCommit,
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_repositoryName,
    pullRequestTarget_sourceReference,
    pullRequestTarget_mergeBase,
    pullRequestTarget_destinationCommit,

    -- * PutFileEntry
    PutFileEntry (..),
    newPutFileEntry,
    putFileEntry_fileContent,
    putFileEntry_sourceFile,
    putFileEntry_fileMode,
    putFileEntry_filePath,

    -- * ReactionForComment
    ReactionForComment (..),
    newReactionForComment,
    reactionForComment_reaction,
    reactionForComment_reactionUsers,
    reactionForComment_reactionsFromDeletedUsersCount,

    -- * ReactionValueFormats
    ReactionValueFormats (..),
    newReactionValueFormats,
    reactionValueFormats_shortCode,
    reactionValueFormats_unicode,
    reactionValueFormats_emoji,

    -- * ReplaceContentEntry
    ReplaceContentEntry (..),
    newReplaceContentEntry,
    replaceContentEntry_content,
    replaceContentEntry_fileMode,
    replaceContentEntry_filePath,
    replaceContentEntry_replacementType,

    -- * RepositoryMetadata
    RepositoryMetadata (..),
    newRepositoryMetadata,
    repositoryMetadata_lastModifiedDate,
    repositoryMetadata_defaultBranch,
    repositoryMetadata_accountId,
    repositoryMetadata_cloneUrlHttp,
    repositoryMetadata_cloneUrlSsh,
    repositoryMetadata_arn,
    repositoryMetadata_creationDate,
    repositoryMetadata_repositoryName,
    repositoryMetadata_repositoryId,
    repositoryMetadata_repositoryDescription,

    -- * RepositoryNameIdPair
    RepositoryNameIdPair (..),
    newRepositoryNameIdPair,
    repositoryNameIdPair_repositoryName,
    repositoryNameIdPair_repositoryId,

    -- * RepositoryTrigger
    RepositoryTrigger (..),
    newRepositoryTrigger,
    repositoryTrigger_customData,
    repositoryTrigger_branches,
    repositoryTrigger_name,
    repositoryTrigger_destinationArn,
    repositoryTrigger_events,

    -- * RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure (..),
    newRepositoryTriggerExecutionFailure,
    repositoryTriggerExecutionFailure_failureMessage,
    repositoryTriggerExecutionFailure_trigger,

    -- * SetFileModeEntry
    SetFileModeEntry (..),
    newSetFileModeEntry,
    setFileModeEntry_filePath,
    setFileModeEntry_fileMode,

    -- * SourceFileSpecifier
    SourceFileSpecifier (..),
    newSourceFileSpecifier,
    sourceFileSpecifier_isMove,
    sourceFileSpecifier_filePath,

    -- * SubModule
    SubModule (..),
    newSubModule,
    subModule_commitId,
    subModule_absolutePath,
    subModule_relativePath,

    -- * SymbolicLink
    SymbolicLink (..),
    newSymbolicLink,
    symbolicLink_absolutePath,
    symbolicLink_relativePath,
    symbolicLink_blobId,
    symbolicLink_fileMode,

    -- * Target
    Target (..),
    newTarget,
    target_destinationReference,
    target_repositoryName,
    target_sourceReference,

    -- * UserInfo
    UserInfo (..),
    newUserInfo,
    userInfo_name,
    userInfo_date,
    userInfo_email,
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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "CodeCommit",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "codecommit",
      Core._serviceSigningName = "codecommit",
      Core._serviceVersion = "2015-04-13",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "CodeCommit",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified conflict detail level is not valid.
_InvalidConflictDetailLevelException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictDetailLevelException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictDetailLevelException"

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_ActorDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActorDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ActorDoesNotExistException"

-- | A commit was not specified.
_CommitRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitRequiredException"

-- | An approval rule with that name already exists. Approval rule names must
-- be unique within the scope of a pull request.
_ApprovalRuleNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleNameAlreadyExistsException"

-- | A pull request ID is required, but none was provided.
_PullRequestIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestIdRequiredException =
  Core._MatchServiceError
    defaultService
    "PullRequestIdRequiredException"

-- | The specified reference does not exist. You must provide a full commit
-- ID.
_ReferenceDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ReferenceDoesNotExistException"

-- | The user name is not valid because it has exceeded the character limit
-- for author names.
_NameLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameLengthExceededException =
  Core._MatchServiceError
    defaultService
    "NameLengthExceededException"

-- | The pull request status update is not valid. The only valid update is
-- from @OPEN@ to @CLOSED@.
_InvalidPullRequestStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestStatusUpdateException"

-- | The pull request ID is not valid. Make sure that you have provided the
-- full ID and that the pull request is in the specified repository, and
-- then try again.
_InvalidPullRequestIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestIdException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestIdException"

-- | A pull request target is required. It cannot be empty or null. A pull
-- request target must contain the full values for the repository name,
-- source branch, and destination branch for the pull request.
_TargetRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetRequiredException =
  Core._MatchServiceError
    defaultService
    "TargetRequiredException"

-- | A file cannot be added to the repository because the specified file name
-- has the same name as a directory in this repository. Either provide
-- another name for the file, or add the file in a directory that does not
-- match the file name.
_FileNameConflictsWithDirectoryNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileNameConflictsWithDirectoryNameException =
  Core._MatchServiceError
    defaultService
    "FileNameConflictsWithDirectoryNameException"

-- | The targets for the pull request is not valid or not in a valid format.
-- Targets are a list of target objects. Each target object must contain
-- the full values for the repository name, source branch, and destination
-- branch for a pull request.
_InvalidTargetsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetsException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetsException"

-- | The comment is empty. You must provide some content for a comment. The
-- content cannot be null.
_CommentContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentContentRequiredException =
  Core._MatchServiceError
    defaultService
    "CommentContentRequiredException"

-- | The commit cannot be created because no files have been specified as
-- added, updated, or changed (PutFile or DeleteFile) for the commit.
_FileEntryRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileEntryRequiredException =
  Core._MatchServiceError
    defaultService
    "FileEntryRequiredException"

-- | The specified tag is not valid. Key names cannot be prefixed with aws:.
_InvalidSystemTagUsageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSystemTagUsageException =
  Core._MatchServiceError
    defaultService
    "InvalidSystemTagUsageException"

-- | Cannot create the branch with the specified name because the commit
-- conflicts with an existing branch with the same name. Branch names must
-- be unique.
_BranchNameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameExistsException =
  Core._MatchServiceError
    defaultService
    "BranchNameExistsException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyNotFoundException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyNotFoundException"

-- | An array of target objects is required. It cannot be empty or null.
_TargetsRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetsRequiredException =
  Core._MatchServiceError
    defaultService
    "TargetsRequiredException"

-- | The pull request cannot be merged automatically into the destination
-- branch. You must manually merge the branches and resolve any conflicts.
_ManualMergeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ManualMergeRequiredException =
  Core._MatchServiceError
    defaultService
    "ManualMergeRequiredException"

-- | The description for the approval rule template is not valid because it
-- exceeds the maximum characters allowed for a description. For more
-- information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateDescriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateDescriptionException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateDescriptionException"

-- | The source branch and destination branch for the pull request are the
-- same. You must specify different branches for the source and
-- destination.
_SourceAndDestinationAreSameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceAndDestinationAreSameException =
  Core._MatchServiceError
    defaultService
    "SourceAndDestinationAreSameException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionIntegrityChecksFailedException =
  Core._MatchServiceError
    defaultService
    "EncryptionIntegrityChecksFailedException"

-- | The maximum number of tags for an AWS CodeCommit resource has been
-- exceeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The commit cannot be created because no source files or file content
-- have been specified for the commit.
_SourceFileOrContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceFileOrContentRequiredException =
  Core._MatchServiceError
    defaultService
    "SourceFileOrContentRequiredException"

-- | This comment has already been deleted. You cannot edit or delete a
-- deleted comment.
_CommentDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentDeletedException =
  Core._MatchServiceError
    defaultService
    "CommentDeletedException"

-- | The commit cannot be created because one or more changes in this commit
-- duplicate actions in the same file path. For example, you cannot make
-- the same delete request to the same file in the same file path twice, or
-- make a delete request and a move request to the same file as part of the
-- same commit.
_SamePathRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SamePathRequestException =
  Core._MatchServiceError
    defaultService
    "SamePathRequestException"

-- | At least one event for the trigger is required, but was not specified.
_RepositoryTriggerEventsListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerEventsListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerEventsListRequiredException"

-- | The parent commit ID is not valid because it does not exist. The
-- specified parent commit ID does not exist in the specified branch of the
-- repository.
_ParentCommitDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ParentCommitDoesNotExistException"

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_AuthorDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "AuthorDoesNotExistException"

-- | Automerge was specified for resolving the conflict, but the replacement
-- type is not valid or content is missing.
_InvalidReplacementContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementContentException =
  Core._MatchServiceError
    defaultService
    "InvalidReplacementContentException"

-- | The file cannot be added because it is empty. Empty files cannot be
-- added to the repository with this API.
_FileContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentRequiredException =
  Core._MatchServiceError
    defaultService
    "FileContentRequiredException"

-- | A pull request title is required. It cannot be empty or null.
_TitleRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TitleRequiredException =
  Core._MatchServiceError
    defaultService
    "TitleRequiredException"

-- | The specified conflict resolution list is not valid.
_InvalidConflictResolutionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictResolutionException"

-- | The content for the approval rule is empty. You must provide some
-- content for an approval rule. The content cannot be null.
_ApprovalRuleContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleContentRequiredException"

-- | The pull request cannot be merged because one or more approval rules
-- applied to the pull request have conditions that have not been met.
_PullRequestApprovalRulesNotSatisfiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestApprovalRulesNotSatisfiedException =
  Core._MatchServiceError
    defaultService
    "PullRequestApprovalRulesNotSatisfiedException"

-- | A pull request status is required, but none was provided.
_PullRequestStatusRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestStatusRequiredException =
  Core._MatchServiceError
    defaultService
    "PullRequestStatusRequiredException"

-- | The pull request status is not valid. The only valid values are @OPEN@
-- and @CLOSED@.
_InvalidPullRequestStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestStatusException"

-- | The title of the pull request is not valid. Pull request titles cannot
-- exceed 100 characters in length.
_InvalidTitleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTitleException =
  Core._MatchServiceError
    defaultService
    "InvalidTitleException"

-- | The source commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidSourceCommitSpecifierException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSourceCommitSpecifierException =
  Core._MatchServiceError
    defaultService
    "InvalidSourceCommitSpecifierException"

-- | A parent commit ID is required. To view the full commit ID of a branch
-- in a repository, use GetBranch or a Git command (for example, git pull
-- or git log).
_ParentCommitIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdRequiredException =
  Core._MatchServiceError
    defaultService
    "ParentCommitIdRequiredException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RepositoryDoesNotExistException"

-- | The content for the approval rule is not valid.
_InvalidApprovalRuleContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleContentException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleContentException"

-- | The override status is not valid. Valid statuses are OVERRIDE and
-- REVOKE.
_InvalidOverrideStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOverrideStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidOverrideStatusException"

-- | The number of items to compare between the source or destination
-- branches and the merge base has exceeded the maximum allowed.
_MaximumItemsToCompareExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumItemsToCompareExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumItemsToCompareExceededException"

-- | A name for the trigger is required, but was not specified.
_RepositoryTriggerNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerNameRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerNameRequiredException"

-- | The maximum number of approval rule templates has been exceeded for this
-- AWS Region.
_NumberOfRuleTemplatesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfRuleTemplatesExceededException =
  Core._MatchServiceError
    defaultService
    "NumberOfRuleTemplatesExceededException"

-- | A repository name is required, but was not specified.
_RepositoryNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNameRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryNameRequiredException"

-- | The specified file mode permission is not valid. For a list of valid
-- file mode permissions, see PutFile.
_InvalidFileModeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileModeException =
  Core._MatchServiceError
    defaultService
    "InvalidFileModeException"

-- | One or more events specified for the trigger is not valid. Check to make
-- sure that all events specified match the requirements for allowed
-- events.
_InvalidRepositoryTriggerEventsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerEventsException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerEventsException"

-- | The name of the trigger is not valid.
_InvalidRepositoryTriggerNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerNameException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerNameException"

-- | The file cannot be added because it is too large. The maximum file size
-- is 6 MB, and the combined file content change size is 7 MB. Consider
-- making these changes using a Git client.
_FileContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FileContentSizeLimitExceededException"

-- | The specified path is not valid.
_InvalidPathException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPathException =
  Core._MatchServiceError
    defaultService
    "InvalidPathException"

-- | The specified target branch is not valid.
_InvalidTargetBranchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetBranchException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetBranchException"

-- | The specified file exceeds the file size limit for AWS CodeCommit. For
-- more information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_FileTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileTooLargeException =
  Core._MatchServiceError
    defaultService
    "FileTooLargeException"

-- | The specified blob is not valid.
_InvalidBlobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBlobIdException =
  Core._MatchServiceError
    defaultService
    "InvalidBlobIdException"

-- | The name for the approval rule is not valid.
_InvalidApprovalRuleNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleNameException"

-- | An approval rule template name is required, but was not specified.
_ApprovalRuleTemplateNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameRequiredException"

-- | The approval rule cannot be deleted from the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotDeleteApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotDeleteApprovalRuleFromTemplateException =
  Core._MatchServiceError
    defaultService
    "CannotDeleteApprovalRuleFromTemplateException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the user who initiated the change for the pull
-- request, and then try again.
_InvalidActorArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActorArnException =
  Core._MatchServiceError
    defaultService
    "InvalidActorArnException"

-- | The pull request ID could not be found. Make sure that you have
-- specified the correct repository name and pull request ID, and then try
-- again.
_PullRequestDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "PullRequestDoesNotExistException"

-- | At least one repository name object is required, but was not specified.
_RepositoryNamesRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNamesRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryNamesRequiredException"

-- | The specified commit ID does not exist.
_CommitIdDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommitIdDoesNotExistException"

-- | The number of specified files to change as part of this commit exceeds
-- the maximum number of files that can be changed in a single commit.
-- Consider using a Git client for these changes.
_MaximumFileEntriesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumFileEntriesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumFileEntriesExceededException"

-- | A replacement type is required.
_ReplacementTypeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplacementTypeRequiredException =
  Core._MatchServiceError
    defaultService
    "ReplacementTypeRequiredException"

-- | Automerge was specified for resolving the conflict, but the specified
-- replacement type is not valid.
_InvalidReplacementTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidReplacementTypeException"

-- | A revision ID is required, but was not provided.
_RevisionIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionIdRequiredException =
  Core._MatchServiceError
    defaultService
    "RevisionIdRequiredException"

-- | The number of reactions has been exceeded. Reactions are limited to one
-- reaction per user for each individual comment ID.
_ReactionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReactionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ReactionLimitExceededException"

-- | The revision ID provided in the request does not match the current
-- revision ID. Use GetPullRequest to retrieve the current revision ID.
_RevisionNotCurrentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionNotCurrentException =
  Core._MatchServiceError
    defaultService
    "RevisionNotCurrentException"

-- | The client request token is not valid. Either the token is not in a
-- valid format, or the token has been used in a previous request and
-- cannot be reused.
_IdempotencyParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotencyParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotencyParameterMismatchException"

-- | More than one conflict resolution entries exists for the conflict. A
-- conflict can have only one conflict resolution entry.
_MultipleConflictResolutionEntriesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleConflictResolutionEntriesException =
  Core._MatchServiceError
    defaultService
    "MultipleConflictResolutionEntriesException"

-- | A branch name is required, but was not specified.
_BranchNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameRequiredException =
  Core._MatchServiceError
    defaultService
    "BranchNameRequiredException"

-- | At least one branch name is required, but was not specified in the
-- trigger configuration.
_RepositoryTriggerBranchNameListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerBranchNameListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerBranchNameListRequiredException"

-- | The list of triggers for the repository is required, but was not
-- specified.
_RepositoryTriggersListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggersListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggersListRequiredException"

-- | A destination ARN for the target service for the trigger is required,
-- but was not specified.
_RepositoryTriggerDestinationArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerDestinationArnRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerDestinationArnRequiredException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyUnavailableException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyUnavailableException"

-- | The tag policy is not valid.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the
-- specified destination. The most common reason for this error is that the
-- ARN does not meet the requirements for the service type.
_InvalidRepositoryTriggerDestinationArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerDestinationArnException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerDestinationArnException"

-- | Either the enum is not in a valid format, or the specified file version
-- enum is not valid in respect to the current file version.
_InvalidRelativeFileVersionEnumException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRelativeFileVersionEnumException =
  Core._MatchServiceError
    defaultService
    "InvalidRelativeFileVersionEnumException"

-- | The specified conflict resolution strategy is not valid.
_InvalidConflictResolutionStrategyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionStrategyException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictResolutionStrategyException"

-- | The specified approval rule does not exist.
_ApprovalRuleDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleDoesNotExistException"

-- | The client request token is not valid.
_InvalidClientRequestTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientRequestTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidClientRequestTokenException"

-- | A reference name is required, but none was provided.
_ReferenceNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ReferenceNameRequiredException"

-- | The number of approvals required for the approval rule exceeds the
-- maximum number allowed.
_MaximumNumberOfApprovalsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumNumberOfApprovalsExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumNumberOfApprovalsExceededException"

-- | The commit message is too long. Provide a shorter string.
_CommitMessageLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitMessageLengthExceededException =
  Core._MatchServiceError
    defaultService
    "CommitMessageLengthExceededException"

-- | The maximum number of approval rule templates for a repository has been
-- exceeded. You cannot associate more than 25 approval rule templates with
-- a repository.
_MaximumRuleTemplatesAssociatedWithRepositoryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRuleTemplatesAssociatedWithRepositoryException =
  Core._MatchServiceError
    defaultService
    "MaximumRuleTemplatesAssociatedWithRepositoryException"

-- | A list of tag keys is required. The list cannot be empty or null.
_TagKeysListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagKeysListRequiredException =
  Core._MatchServiceError
    defaultService
    "TagKeysListRequiredException"

-- | The specified reference name format is not valid. Reference names must
-- conform to the Git references format (for example, refs\/heads\/master).
-- For more information, see
-- <https://git-scm.com/book/en/v2/Git-Internals-Git-References Git Internals - Git References>
-- or consult your Git documentation.
_InvalidReferenceNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReferenceNameException =
  Core._MatchServiceError
    defaultService
    "InvalidReferenceNameException"

-- | The specified file does not exist. Verify that you have used the correct
-- file name, full path, and extension.
_FileDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "FileDoesNotExistException"

-- | The list of tags is not valid.
_InvalidTagKeysListException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagKeysListException =
  Core._MatchServiceError
    defaultService
    "InvalidTagKeysListException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitIdRequiredException"

-- | The pull request has already had its approval rules set to override.
_OverrideAlreadySetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverrideAlreadySetException =
  Core._MatchServiceError
    defaultService
    "OverrideAlreadySetException"

-- | No comment exists with the provided ID. Verify that you have used the
-- correct ID, and then try again.
_CommentDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommentDoesNotExistException"

-- | The destination commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidDestinationCommitSpecifierException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDestinationCommitSpecifierException =
  Core._MatchServiceError
    defaultService
    "InvalidDestinationCommitSpecifierException"

-- | You cannot modify or delete this comment. Only comment authors can
-- modify or delete their comments.
_CommentNotCreatedByCallerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentNotCreatedByCallerException =
  Core._MatchServiceError
    defaultService
    "CommentNotCreatedByCallerException"

-- | The target for the pull request is not valid. A target must contain the
-- full values for the repository name, source branch, and destination
-- branch for the pull request.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The specified reference is not a supported type.
_ReferenceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ReferenceTypeNotSupportedException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException =
  Core._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyDisabledException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyDisabledException"

-- | The specified commit is not valid.
_InvalidCommitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommitException =
  Core._MatchServiceError
    defaultService
    "InvalidCommitException"

-- | You cannot create an approval rule template with that name because a
-- template with that name already exists in this AWS Region for your AWS
-- account. Approval rule template names must be unique.
_ApprovalRuleTemplateNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameAlreadyExistsException"

-- | The custom data provided for the trigger is not valid.
_InvalidRepositoryTriggerCustomDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerCustomDataException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerCustomDataException"

-- | One or more branch names specified for the trigger is not valid.
_InvalidRepositoryTriggerBranchNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerBranchNameException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerBranchNameException"

-- | A file cannot be added to the repository because the specified path name
-- has the same name as a file that already exists in this repository.
-- Either provide a different name for the file, or specify a different
-- path for the file.
_DirectoryNameConflictsWithFileNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNameConflictsWithFileNameException =
  Core._MatchServiceError
    defaultService
    "DirectoryNameConflictsWithFileNameException"

-- | You cannot create the pull request because the repository has too many
-- open pull requests. The maximum number of open pull requests for a
-- repository is 1,000. Close one or more open pull requests, and then try
-- again.
_MaximumOpenPullRequestsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumOpenPullRequestsExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumOpenPullRequestsExceededException"

-- | The pull request status cannot be updated because it is already closed.
_PullRequestAlreadyClosedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestAlreadyClosedException =
  Core._MatchServiceError
    defaultService
    "PullRequestAlreadyClosedException"

-- | A reaction value is required.
_ReactionValueRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReactionValueRequiredException =
  Core._MatchServiceError
    defaultService
    "ReactionValueRequiredException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyAccessDeniedException"

-- | The AWS Region for the trigger target does not match the AWS Region for
-- the repository. Triggers must be created in the same Region as the
-- target for the trigger.
_InvalidRepositoryTriggerRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerRegionException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerRegionException"

-- | The value of the reaction is not valid. For more information, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.
_InvalidReactionValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReactionValueException =
  Core._MatchServiceError
    defaultService
    "InvalidReactionValueException"

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidContinuationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidContinuationTokenException"

-- | The divergence between the tips of the provided commit specifiers is too
-- great to determine whether there might be any merge conflicts. Locally
-- compare the specifiers using @git diff@ or a diff tool.
_TipsDivergenceExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TipsDivergenceExceededException =
  Core._MatchServiceError
    defaultService
    "TipsDivergenceExceededException"

-- | The commit cannot be created because one of the changes specifies
-- copying or moving a .gitkeep file.
_RestrictedSourceFileException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RestrictedSourceFileException =
  Core._MatchServiceError
    defaultService
    "RestrictedSourceFileException"

-- | The position is not valid. Make sure that the line number exists in the
-- version of the file you want to comment on.
_InvalidFilePositionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilePositionException =
  Core._MatchServiceError
    defaultService
    "InvalidFilePositionException"

-- | The specified value for the number of merge hunks to return is not
-- valid.
_InvalidMaxMergeHunksException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxMergeHunksException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxMergeHunksException"

-- | USE_NEW_CONTENT was specified, but no replacement content has been
-- provided.
_ReplacementContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplacementContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ReplacementContentRequiredException"

-- | A valid Amazon Resource Name (ARN) for an AWS CodeCommit resource is
-- required. For a list of valid resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException =
  Core._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | The approval rule cannot be modified for the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotModifyApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotModifyApprovalRuleFromTemplateException =
  Core._MatchServiceError
    defaultService
    "CannotModifyApprovalRuleFromTemplateException"

-- | The file could not be added because the provided parent commit ID is not
-- the current tip of the specified branch. To view the full commit ID of
-- the current head of the branch, use GetBranch.
_ParentCommitIdOutdatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdOutdatedException =
  Core._MatchServiceError
    defaultService
    "ParentCommitIdOutdatedException"

-- | The specified value for the number of conflict files to return is not
-- valid.
_InvalidMaxConflictFilesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxConflictFilesException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxConflictFilesException"

-- | The specified path does not exist.
_PathDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PathDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "PathDoesNotExistException"

-- | The value for the resource ARN is not valid. For more information about
-- resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_InvalidResourceArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceArnException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceArnException"

-- | The content for the approval rule template is empty. You must provide
-- some content for an approval rule template. The content cannot be null.
_ApprovalRuleTemplateContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateContentRequiredException"

-- | The number of branches for the trigger was exceeded.
_MaximumBranchesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumBranchesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumBranchesExceededException"

-- | An override status is required, but no value was provided. Valid values
-- include OVERRIDE and REVOKE.
_OverrideStatusRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverrideStatusRequiredException =
  Core._MatchServiceError
    defaultService
    "OverrideStatusRequiredException"

-- | An approval state is required, but was not specified.
_ApprovalStateRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalStateRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalStateRequiredException"

-- | The pull request event type is not valid.
_InvalidPullRequestEventTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestEventTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestEventTypeException"

-- | The maximum number of allowed commit IDs in a batch request is 100.
-- Verify that your batch requests contains no more than 100 commit IDs,
-- and then try again.
_CommitIdsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CommitIdsLimitExceededException"

-- | The repository does not contain any pull requests with that pull request
-- ID. Use GetPullRequest to verify the correct repository name for the
-- pull request ID.
_RepositoryNotAssociatedWithPullRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNotAssociatedWithPullRequestException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotAssociatedWithPullRequestException"

-- | The number of files to load exceeds the allowed limit.
_MaximumFileContentToLoadExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumFileContentToLoadExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumFileContentToLoadExceededException"

-- | The merge cannot be completed because the target branch has been
-- modified. Another user might have modified the target branch while the
-- merge was in progress. Wait a few minutes, and then try again.
_ConcurrentReferenceUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentReferenceUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentReferenceUpdateException"

-- | The parent commit ID is not valid. The commit ID cannot be empty, and
-- must match the head commit ID for the branch of the repository where you
-- want to add or update a file.
_InvalidParentCommitIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParentCommitIdException =
  Core._MatchServiceError
    defaultService
    "InvalidParentCommitIdException"

-- | The number of allowed conflict resolution entries was exceeded.
_MaximumConflictResolutionEntriesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumConflictResolutionEntriesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumConflictResolutionEntriesExceededException"

-- | The comment is too large. Comments are limited to 1,000 characters.
_CommentContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CommentContentSizeLimitExceededException"

-- | The content of the approval rule template is not valid.
_InvalidApprovalRuleTemplateContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateContentException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateContentException"

-- | The state for the approval is not valid. Valid values include APPROVE
-- and REVOKE.
_InvalidApprovalStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalStateException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalStateException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the author of the pull request, and then try
-- again.
_InvalidAuthorArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAuthorArnException =
  Core._MatchServiceError
    defaultService
    "InvalidAuthorArnException"

-- | The folderPath for a location cannot be null.
_PathRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PathRequiredException =
  Core._MatchServiceError
    defaultService
    "PathRequiredException"

-- | The map of tags is not valid.
_InvalidTagsMapException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagsMapException =
  Core._MatchServiceError
    defaultService
    "InvalidTagsMapException"

-- | A list of commit IDs is required, but was either not specified or the
-- list was empty.
_CommitIdsListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdsListRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitIdsListRequiredException"

-- | The specified branch name is not valid because it is a tag name. Enter
-- the name of a branch in the repository. For a list of valid branch
-- names, use ListBranches.
_BranchNameIsTagNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameIsTagNameException =
  Core._MatchServiceError
    defaultService
    "BranchNameIsTagNameException"

-- | The specified branch is the default branch for the repository, and
-- cannot be deleted. To delete this branch, you must first set another
-- branch as the default branch.
_DefaultBranchCannotBeDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultBranchCannotBeDeletedException =
  Core._MatchServiceError
    defaultService
    "DefaultBranchCannotBeDeletedException"

-- | The commit cannot be created because no file mode has been specified. A
-- file mode is required to update mode permissions for a file.
_FileModeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileModeRequiredException =
  Core._MatchServiceError
    defaultService
    "FileModeRequiredException"

-- | A map of tags is required.
_TagsMapRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagsMapRequiredException =
  Core._MatchServiceError
    defaultService
    "TagsMapRequiredException"

-- | The commit cannot be created because at least one of the overall changes
-- in the commit results in a folder whose contents exceed the limit of 6
-- MB. Either reduce the number and size of your changes, or split the
-- changes across multiple folders.
_FolderContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FolderContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FolderContentSizeLimitExceededException"

-- | A specified repository name is not valid.
--
-- This exception occurs only when a specified repository name is not
-- valid. Other exceptions occur when a required repository parameter is
-- missing, or when a specified repository does not exist.
_InvalidRepositoryNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryNameException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryNameException"

-- | The approval rule cannot be added. The pull request has the maximum
-- number of approval rules associated with it.
_NumberOfRulesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfRulesExceededException =
  Core._MatchServiceError
    defaultService
    "NumberOfRulesExceededException"

-- | The Amazon Resource Name (ARN) of the user or identity is not valid.
_InvalidReactionUserArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReactionUserArnException =
  Core._MatchServiceError
    defaultService
    "InvalidReactionUserArnException"

-- | The specified deletion parameter is not valid.
_InvalidDeletionParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionParameterException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RepositoryLimitExceededException"

-- | The commit cannot be created because a specified file path points to a
-- submodule. Verify that the destination files have valid file paths that
-- do not point to a submodule.
_FilePathConflictsWithSubmodulePathException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FilePathConflictsWithSubmodulePathException =
  Core._MatchServiceError
    defaultService
    "FilePathConflictsWithSubmodulePathException"

-- | The specified branch does not exist.
_BranchDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "BranchDoesNotExistException"

-- | The pull request description is not valid. Descriptions cannot be more
-- than 1,000 characters.
_InvalidDescriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDescriptionException =
  Core._MatchServiceError
    defaultService
    "InvalidDescriptionException"

-- | The comment ID is not in a valid format. Make sure that you have
-- provided the full comment ID.
_InvalidCommentIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommentIdException =
  Core._MatchServiceError
    defaultService
    "InvalidCommentIdException"

-- | The specified sort order is not valid.
_InvalidOrderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOrderException =
  Core._MatchServiceError
    defaultService
    "InvalidOrderException"

-- | A blob ID is required, but was not specified.
_BlobIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BlobIdRequiredException =
  Core._MatchServiceError
    defaultService
    "BlobIdRequiredException"

-- | The name of the approval rule template is not valid. Template names must
-- be between 1 and 100 valid characters in length. For more information
-- about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateNameException"

-- | You cannot include more than one repository in a pull request. Make sure
-- you have specified only one repository name in your request, and then
-- try again.
_MultipleRepositoriesInPullRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleRepositoriesInPullRequestException =
  Core._MatchServiceError
    defaultService
    "MultipleRepositoriesInPullRequestException"

-- | The specified number of maximum results is not valid.
_InvalidMaxResultsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxResultsException"

-- | An approval rule name is required, but was not specified.
_ApprovalRuleNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleNameRequiredException"

-- | The revision ID is not valid. Use GetPullRequest to determine the value.
_InvalidRevisionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRevisionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidRevisionIdException"

-- | The approval cannot be applied because the user approving the pull
-- request matches the user who created the pull request. You cannot
-- approve a pull request that you created.
_PullRequestCannotBeApprovedByAuthorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestCannotBeApprovedByAuthorException =
  Core._MatchServiceError
    defaultService
    "PullRequestCannotBeApprovedByAuthorException"

-- | The comment ID is missing or null. A comment ID is required.
_CommentIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentIdRequiredException =
  Core._MatchServiceError
    defaultService
    "CommentIdRequiredException"

-- | The commit cannot be created because no changes will be made to the
-- repository as a result of this commit. A commit must contain at least
-- one change.
_NoChangeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoChangeException =
  Core._MatchServiceError
    defaultService
    "NoChangeException"

-- | The specified reference name is not valid.
_InvalidBranchNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBranchNameException =
  Core._MatchServiceError
    defaultService
    "InvalidBranchNameException"

-- | The specified merge option is not valid for this operation. Not all
-- merge strategies are supported for all operations.
_InvalidMergeOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMergeOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidMergeOptionException"

-- | The specified commit does not exist or no commit was specified, and the
-- specified repository has no default branch.
_CommitDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommitDoesNotExistException"

-- | The before commit ID and the after commit ID are the same, which is not
-- valid. The before commit ID and the after commit ID must be different
-- commit IDs.
_BeforeCommitIdAndAfterCommitIdAreSameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BeforeCommitIdAndAfterCommitIdAreSameException =
  Core._MatchServiceError
    defaultService
    "BeforeCommitIdAndAfterCommitIdAreSameException"

-- | The number of triggers allowed for the repository was exceeded.
_MaximumRepositoryTriggersExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryTriggersExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumRepositoryTriggersExceededException"

-- | A merge option or stategy is required, and none was provided.
_MergeOptionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MergeOptionRequiredException =
  Core._MatchServiceError
    defaultService
    "MergeOptionRequiredException"

-- | The location of the file is not valid. Make sure that you include the
-- file name and extension.
_InvalidFileLocationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileLocationException =
  Core._MatchServiceError
    defaultService
    "InvalidFileLocationException"

-- | The specified approval rule template does not exist. Verify that the
-- name is correct and that you are signed in to the AWS Region where the
-- template was created, and then try again.
_ApprovalRuleTemplateDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateDoesNotExistException"

-- | A client request token is required. A client request token is an unique,
-- client-generated idempotency token that, when provided in a request,
-- ensures the request cannot be repeated with a changed parameter. If a
-- request is received with the same parameters and a token is included,
-- the request returns information about the initial request that used that
-- token.
_ClientRequestTokenRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ClientRequestTokenRequiredException =
  Core._MatchServiceError
    defaultService
    "ClientRequestTokenRequiredException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommitIdException =
  Core._MatchServiceError
    defaultService
    "InvalidCommitIdException"

-- | The commit cannot be created because both a source file and file content
-- have been specified for the same file. You cannot provide both. Either
-- specify a source file or provide the file content directly.
_FileContentAndSourceFileSpecifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentAndSourceFileSpecifiedException =
  Core._MatchServiceError
    defaultService
    "FileContentAndSourceFileSpecifiedException"

-- | The file was not added or updated because the content of the file is
-- exactly the same as the content of that file in the repository and
-- branch that you specified.
_SameFileContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SameFileContentException =
  Core._MatchServiceError
    defaultService
    "SameFileContentException"

-- | The tip of the source branch in the destination repository does not
-- match the tip of the source branch specified in your request. The pull
-- request might have been updated. Make sure that you have the latest
-- changes.
_TipOfSourceReferenceIsDifferentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TipOfSourceReferenceIsDifferentException =
  Core._MatchServiceError
    defaultService
    "TipOfSourceReferenceIsDifferentException"

-- | The specified email address either contains one or more characters that
-- are not allowed, or it exceeds the maximum number of characters allowed
-- for an email address.
_InvalidEmailException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEmailException =
  Core._MatchServiceError
    defaultService
    "InvalidEmailException"

-- | The maximum number of allowed repository names was exceeded. Currently,
-- this number is 100.
_MaximumRepositoryNamesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryNamesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumRepositoryNamesExceededException"

-- | The specified repository name already exists.
_RepositoryNameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNameExistsException =
  Core._MatchServiceError
    defaultService
    "RepositoryNameExistsException"

-- | The commit cannot be created because one or more files specified in the
-- commit reference both a file and a folder.
_PutFileEntryConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PutFileEntryConflictException =
  Core._MatchServiceError
    defaultService
    "PutFileEntryConflictException"

-- | The specified folder does not exist. Either the folder name is not
-- correct, or you did not enter the full path to the folder.
_FolderDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FolderDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "FolderDoesNotExistException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryDescriptionException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryDescriptionException"

-- | The SHA-256 hash signature for the rule content is not valid.
_InvalidRuleContentSha256Exception :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuleContentSha256Exception =
  Core._MatchServiceError
    defaultService
    "InvalidRuleContentSha256Exception"

-- | The specified blob does not exist.
_BlobIdDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BlobIdDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "BlobIdDoesNotExistException"

-- | The approval rule template is associated with one or more repositories.
-- You cannot delete a template that is associated with a repository.
-- Remove all associations, and then try again.
_ApprovalRuleTemplateInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateInUseException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateInUseException"
