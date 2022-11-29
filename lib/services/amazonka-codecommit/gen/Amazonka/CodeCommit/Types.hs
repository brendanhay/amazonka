{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeCommit.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _RepositoryTriggerDestinationArnRequiredException,
    _InvalidTitleException,
    _InvalidRuleContentSha256Exception,
    _ActorDoesNotExistException,
    _CommitIdRequiredException,
    _InvalidSourceCommitSpecifierException,
    _FileEntryRequiredException,
    _InvalidRepositoryTriggerEventsException,
    _InvalidRepositoryTriggerCustomDataException,
    _TagKeysListRequiredException,
    _OverrideAlreadySetException,
    _InvalidReactionUserArnException,
    _EncryptionKeyAccessDeniedException,
    _DirectoryNameConflictsWithFileNameException,
    _OverrideStatusRequiredException,
    _CommitIdsLimitExceededException,
    _BranchNameExistsException,
    _FileTooLargeException,
    _InvalidFileModeException,
    _RepositoryNotAssociatedWithPullRequestException,
    _InvalidContinuationTokenException,
    _ParentCommitIdOutdatedException,
    _ReferenceDoesNotExistException,
    _MaximumFileEntriesExceededException,
    _InvalidFileLocationException,
    _TipsDivergenceExceededException,
    _MaximumRuleTemplatesAssociatedWithRepositoryException,
    _EncryptionIntegrityChecksFailedException,
    _InvalidTargetsException,
    _BranchDoesNotExistException,
    _InvalidDescriptionException,
    _RepositoryNameExistsException,
    _FilePathConflictsWithSubmodulePathException,
    _ApprovalRuleTemplateDoesNotExistException,
    _InvalidPullRequestStatusUpdateException,
    _TargetRequiredException,
    _CannotModifyApprovalRuleFromTemplateException,
    _CommitRequiredException,
    _InvalidMaxMergeHunksException,
    _RestrictedSourceFileException,
    _MultipleConflictResolutionEntriesException,
    _BranchNameIsTagNameException,
    _InvalidCommentIdException,
    _InvalidActorArnException,
    _PullRequestAlreadyClosedException,
    _ApprovalRuleTemplateInUseException,
    _InvalidRepositoryTriggerNameException,
    _ResourceArnRequiredException,
    _SourceFileOrContentRequiredException,
    _InvalidReplacementTypeException,
    _InvalidRepositoryNameException,
    _TooManyTagsException,
    _SourceAndDestinationAreSameException,
    _InvalidDestinationCommitSpecifierException,
    _InvalidFilePositionException,
    _NameLengthExceededException,
    _InvalidBlobIdException,
    _PathDoesNotExistException,
    _ParentCommitDoesNotExistException,
    _InvalidTagsMapException,
    _CommitMessageLengthExceededException,
    _RepositoryDoesNotExistException,
    _InvalidReplacementContentException,
    _InvalidOrderException,
    _InvalidTagKeysListException,
    _PullRequestStatusRequiredException,
    _InvalidPullRequestIdException,
    _InvalidApprovalRuleTemplateDescriptionException,
    _PullRequestCannotBeApprovedByAuthorException,
    _EncryptionKeyDisabledException,
    _RevisionIdRequiredException,
    _CommentContentRequiredException,
    _InvalidEmailException,
    _RevisionNotCurrentException,
    _MaximumFileContentToLoadExceededException,
    _ApprovalRuleNameAlreadyExistsException,
    _InvalidRepositoryTriggerRegionException,
    _ApprovalRuleTemplateNameAlreadyExistsException,
    _InvalidReferenceNameException,
    _CommentIdRequiredException,
    _ReactionValueRequiredException,
    _SameFileContentException,
    _InvalidMaxResultsException,
    _InvalidRevisionIdException,
    _InvalidBranchNameException,
    _InvalidReactionValueException,
    _InvalidApprovalRuleContentException,
    _ApprovalRuleTemplateNameRequiredException,
    _InvalidApprovalRuleTemplateContentException,
    _InvalidClientRequestTokenException,
    _InvalidTargetBranchException,
    _NoChangeException,
    _MaximumRepositoryTriggersExceededException,
    _PullRequestDoesNotExistException,
    _ReplacementTypeRequiredException,
    _MaximumItemsToCompareExceededException,
    _FileContentAndSourceFileSpecifiedException,
    _RepositoryNamesRequiredException,
    _TitleRequiredException,
    _PathRequiredException,
    _ReplacementContentRequiredException,
    _InvalidApprovalRuleTemplateNameException,
    _FileContentRequiredException,
    _ReferenceTypeNotSupportedException,
    _RepositoryTriggerBranchNameListRequiredException,
    _MaximumNumberOfApprovalsExceededException,
    _RepositoryTriggerEventsListRequiredException,
    _FileDoesNotExistException,
    _InvalidApprovalStateException,
    _CommentNotCreatedByCallerException,
    _ReactionLimitExceededException,
    _MaximumBranchesExceededException,
    _InvalidConflictDetailLevelException,
    _InvalidRepositoryTriggerBranchNameException,
    _FolderDoesNotExistException,
    _BeforeCommitIdAndAfterCommitIdAreSameException,
    _BlobIdDoesNotExistException,
    _RepositoryLimitExceededException,
    _CommentDeletedException,
    _ReferenceNameRequiredException,
    _ConcurrentReferenceUpdateException,
    _TargetsRequiredException,
    _ClientRequestTokenRequiredException,
    _BranchNameRequiredException,
    _InvalidDeletionParameterException,
    _InvalidCommitException,
    _MaximumConflictResolutionEntriesExceededException,
    _InvalidSortByException,
    _PutFileEntryConflictException,
    _SamePathRequestException,
    _InvalidMaxConflictFilesException,
    _InvalidTargetException,
    _MergeOptionRequiredException,
    _PullRequestIdRequiredException,
    _InvalidSystemTagUsageException,
    _InvalidPullRequestEventTypeException,
    _EncryptionKeyUnavailableException,
    _EncryptionKeyNotFoundException,
    _ApprovalRuleDoesNotExistException,
    _MaximumRepositoryNamesExceededException,
    _MaximumOpenPullRequestsExceededException,
    _InvalidConflictResolutionStrategyException,
    _InvalidPathException,
    _InvalidRepositoryTriggerDestinationArnException,
    _FileContentSizeLimitExceededException,
    _InvalidResourceArnException,
    _ManualMergeRequiredException,
    _InvalidOverrideStatusException,
    _NumberOfRuleTemplatesExceededException,
    _BlobIdRequiredException,
    _RepositoryTriggerNameRequiredException,
    _NumberOfRulesExceededException,
    _PullRequestApprovalRulesNotSatisfiedException,
    _CommitIdsListRequiredException,
    _FolderContentSizeLimitExceededException,
    _RepositoryNameRequiredException,
    _IdempotencyParameterMismatchException,
    _FileNameConflictsWithDirectoryNameException,
    _CommentContentSizeLimitExceededException,
    _CommitIdDoesNotExistException,
    _CannotDeleteApprovalRuleFromTemplateException,
    _ParentCommitIdRequiredException,
    _CommentDoesNotExistException,
    _TagsMapRequiredException,
    _CommitDoesNotExistException,
    _InvalidConflictResolutionException,
    _FileModeRequiredException,
    _InvalidMergeOptionException,
    _InvalidAuthorArnException,
    _InvalidPullRequestStatusException,
    _InvalidRepositoryDescriptionException,
    _ApprovalRuleContentRequiredException,
    _MultipleRepositoriesInPullRequestException,
    _InvalidCommitIdException,
    _TipOfSourceReferenceIsDifferentException,
    _ApprovalRuleTemplateContentRequiredException,
    _ApprovalRuleNameRequiredException,
    _TagPolicyException,
    _InvalidApprovalRuleNameException,
    _AuthorDoesNotExistException,
    _InvalidRelativeFileVersionEnumException,
    _RepositoryTriggersListRequiredException,
    _ApprovalStateRequiredException,
    _DefaultBranchCannotBeDeletedException,
    _InvalidParentCommitIdException,

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
    approval_approvalState,
    approval_userArn,

    -- * ApprovalRule
    ApprovalRule (..),
    newApprovalRule,
    approvalRule_lastModifiedUser,
    approvalRule_lastModifiedDate,
    approvalRule_creationDate,
    approvalRule_approvalRuleName,
    approvalRule_ruleContentSha256,
    approvalRule_approvalRuleContent,
    approvalRule_originApprovalRuleTemplate,
    approvalRule_approvalRuleId,

    -- * ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata (..),
    newApprovalRuleEventMetadata,
    approvalRuleEventMetadata_approvalRuleName,
    approvalRuleEventMetadata_approvalRuleContent,
    approvalRuleEventMetadata_approvalRuleId,

    -- * ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata (..),
    newApprovalRuleOverriddenEventMetadata,
    approvalRuleOverriddenEventMetadata_revisionId,
    approvalRuleOverriddenEventMetadata_overrideStatus,

    -- * ApprovalRuleTemplate
    ApprovalRuleTemplate (..),
    newApprovalRuleTemplate,
    approvalRuleTemplate_approvalRuleTemplateContent,
    approvalRuleTemplate_lastModifiedUser,
    approvalRuleTemplate_lastModifiedDate,
    approvalRuleTemplate_creationDate,
    approvalRuleTemplate_approvalRuleTemplateName,
    approvalRuleTemplate_ruleContentSha256,
    approvalRuleTemplate_approvalRuleTemplateDescription,
    approvalRuleTemplate_approvalRuleTemplateId,

    -- * ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata (..),
    newApprovalStateChangedEventMetadata,
    approvalStateChangedEventMetadata_revisionId,
    approvalStateChangedEventMetadata_approvalStatus,

    -- * BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError (..),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesError,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_errorMessage,
    batchAssociateApprovalRuleTemplateWithRepositoriesError_repositoryName,
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
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_errorMessage,
    batchDisassociateApprovalRuleTemplateFromRepositoriesError_repositoryName,
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
    blobMetadata_path,
    blobMetadata_mode,
    blobMetadata_blobId,

    -- * BranchInfo
    BranchInfo (..),
    newBranchInfo,
    branchInfo_commitId,
    branchInfo_branchName,

    -- * Comment
    Comment (..),
    newComment,
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

    -- * CommentsForComparedCommit
    CommentsForComparedCommit (..),
    newCommentsForComparedCommit,
    commentsForComparedCommit_beforeBlobId,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_repositoryName,
    commentsForComparedCommit_beforeCommitId,
    commentsForComparedCommit_comments,
    commentsForComparedCommit_location,
    commentsForComparedCommit_afterBlobId,

    -- * CommentsForPullRequest
    CommentsForPullRequest (..),
    newCommentsForPullRequest,
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_afterCommitId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_comments,
    commentsForPullRequest_location,
    commentsForPullRequest_afterBlobId,

    -- * Commit
    Commit (..),
    newCommit,
    commit_message,
    commit_author,
    commit_commitId,
    commit_parents,
    commit_committer,
    commit_treeId,
    commit_additionalData,

    -- * Conflict
    Conflict (..),
    newConflict,
    conflict_mergeHunks,
    conflict_conflictMetadata,

    -- * ConflictMetadata
    ConflictMetadata (..),
    newConflictMetadata,
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

    -- * ConflictResolution
    ConflictResolution (..),
    newConflictResolution,
    conflictResolution_replaceContents,
    conflictResolution_setFileModes,
    conflictResolution_deleteFiles,

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
    evaluation_approvalRulesNotSatisfied,
    evaluation_approvalRulesSatisfied,
    evaluation_approved,

    -- * File
    File (..),
    newFile,
    file_fileMode,
    file_absolutePath,
    file_blobId,
    file_relativePath,

    -- * FileMetadata
    FileMetadata (..),
    newFileMetadata,
    fileMetadata_fileMode,
    fileMetadata_absolutePath,
    fileMetadata_blobId,

    -- * FileModes
    FileModes (..),
    newFileModes,
    fileModes_destination,
    fileModes_base,
    fileModes_source,

    -- * FileSizes
    FileSizes (..),
    newFileSizes,
    fileSizes_destination,
    fileSizes_base,
    fileSizes_source,

    -- * Folder
    Folder (..),
    newFolder,
    folder_absolutePath,
    folder_relativePath,
    folder_treeId,

    -- * IsBinaryFile
    IsBinaryFile (..),
    newIsBinaryFile,
    isBinaryFile_destination,
    isBinaryFile_base,
    isBinaryFile_source,

    -- * Location
    Location (..),
    newLocation,
    location_filePath,
    location_filePosition,
    location_relativeFileVersion,

    -- * MergeHunk
    MergeHunk (..),
    newMergeHunk,
    mergeHunk_destination,
    mergeHunk_isConflict,
    mergeHunk_base,
    mergeHunk_source,

    -- * MergeHunkDetail
    MergeHunkDetail (..),
    newMergeHunkDetail,
    mergeHunkDetail_endLine,
    mergeHunkDetail_hunkContent,
    mergeHunkDetail_startLine,

    -- * MergeMetadata
    MergeMetadata (..),
    newMergeMetadata,
    mergeMetadata_mergeOption,
    mergeMetadata_mergeCommitId,
    mergeMetadata_isMerged,
    mergeMetadata_mergedBy,

    -- * MergeOperations
    MergeOperations (..),
    newMergeOperations,
    mergeOperations_destination,
    mergeOperations_source,

    -- * ObjectTypes
    ObjectTypes (..),
    newObjectTypes,
    objectTypes_destination,
    objectTypes_base,
    objectTypes_source,

    -- * OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate (..),
    newOriginApprovalRuleTemplate,
    originApprovalRuleTemplate_approvalRuleTemplateName,
    originApprovalRuleTemplate_approvalRuleTemplateId,

    -- * PullRequest
    PullRequest (..),
    newPullRequest,
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

    -- * PullRequestCreatedEventMetadata
    PullRequestCreatedEventMetadata (..),
    newPullRequestCreatedEventMetadata,
    pullRequestCreatedEventMetadata_repositoryName,
    pullRequestCreatedEventMetadata_mergeBase,
    pullRequestCreatedEventMetadata_sourceCommitId,
    pullRequestCreatedEventMetadata_destinationCommitId,

    -- * PullRequestEvent
    PullRequestEvent (..),
    newPullRequestEvent,
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

    -- * PullRequestMergedStateChangedEventMetadata
    PullRequestMergedStateChangedEventMetadata (..),
    newPullRequestMergedStateChangedEventMetadata,
    pullRequestMergedStateChangedEventMetadata_mergeMetadata,
    pullRequestMergedStateChangedEventMetadata_repositoryName,
    pullRequestMergedStateChangedEventMetadata_destinationReference,

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata (..),
    newPullRequestSourceReferenceUpdatedEventMetadata,
    pullRequestSourceReferenceUpdatedEventMetadata_afterCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_repositoryName,
    pullRequestSourceReferenceUpdatedEventMetadata_beforeCommitId,
    pullRequestSourceReferenceUpdatedEventMetadata_mergeBase,

    -- * PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata (..),
    newPullRequestStatusChangedEventMetadata,
    pullRequestStatusChangedEventMetadata_pullRequestStatus,

    -- * PullRequestTarget
    PullRequestTarget (..),
    newPullRequestTarget,
    pullRequestTarget_mergeMetadata,
    pullRequestTarget_sourceReference,
    pullRequestTarget_sourceCommit,
    pullRequestTarget_repositoryName,
    pullRequestTarget_destinationReference,
    pullRequestTarget_mergeBase,
    pullRequestTarget_destinationCommit,

    -- * PutFileEntry
    PutFileEntry (..),
    newPutFileEntry,
    putFileEntry_fileMode,
    putFileEntry_sourceFile,
    putFileEntry_fileContent,
    putFileEntry_filePath,

    -- * ReactionForComment
    ReactionForComment (..),
    newReactionForComment,
    reactionForComment_reactionsFromDeletedUsersCount,
    reactionForComment_reactionUsers,
    reactionForComment_reaction,

    -- * ReactionValueFormats
    ReactionValueFormats (..),
    newReactionValueFormats,
    reactionValueFormats_emoji,
    reactionValueFormats_shortCode,
    reactionValueFormats_unicode,

    -- * ReplaceContentEntry
    ReplaceContentEntry (..),
    newReplaceContentEntry,
    replaceContentEntry_fileMode,
    replaceContentEntry_content,
    replaceContentEntry_filePath,
    replaceContentEntry_replacementType,

    -- * RepositoryMetadata
    RepositoryMetadata (..),
    newRepositoryMetadata,
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

    -- * RepositoryNameIdPair
    RepositoryNameIdPair (..),
    newRepositoryNameIdPair,
    repositoryNameIdPair_repositoryName,
    repositoryNameIdPair_repositoryId,

    -- * RepositoryTrigger
    RepositoryTrigger (..),
    newRepositoryTrigger,
    repositoryTrigger_branches,
    repositoryTrigger_customData,
    repositoryTrigger_name,
    repositoryTrigger_destinationArn,
    repositoryTrigger_events,

    -- * RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure (..),
    newRepositoryTriggerExecutionFailure,
    repositoryTriggerExecutionFailure_trigger,
    repositoryTriggerExecutionFailure_failureMessage,

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
    subModule_absolutePath,
    subModule_commitId,
    subModule_relativePath,

    -- * SymbolicLink
    SymbolicLink (..),
    newSymbolicLink,
    symbolicLink_fileMode,
    symbolicLink_absolutePath,
    symbolicLink_blobId,
    symbolicLink_relativePath,

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
    userInfo_email,
    userInfo_date,
  )
where

import Amazonka.CodeCommit.Types.Approval
import Amazonka.CodeCommit.Types.ApprovalRule
import Amazonka.CodeCommit.Types.ApprovalRuleEventMetadata
import Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Amazonka.CodeCommit.Types.ApprovalRuleTemplate
import Amazonka.CodeCommit.Types.ApprovalState
import Amazonka.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Amazonka.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Amazonka.CodeCommit.Types.BatchDescribeMergeConflictsError
import Amazonka.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Amazonka.CodeCommit.Types.BatchGetCommitsError
import Amazonka.CodeCommit.Types.BlobMetadata
import Amazonka.CodeCommit.Types.BranchInfo
import Amazonka.CodeCommit.Types.ChangeTypeEnum
import Amazonka.CodeCommit.Types.Comment
import Amazonka.CodeCommit.Types.CommentsForComparedCommit
import Amazonka.CodeCommit.Types.CommentsForPullRequest
import Amazonka.CodeCommit.Types.Commit
import Amazonka.CodeCommit.Types.Conflict
import Amazonka.CodeCommit.Types.ConflictDetailLevelTypeEnum
import Amazonka.CodeCommit.Types.ConflictMetadata
import Amazonka.CodeCommit.Types.ConflictResolution
import Amazonka.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
import Amazonka.CodeCommit.Types.DeleteFileEntry
import Amazonka.CodeCommit.Types.Difference
import Amazonka.CodeCommit.Types.Evaluation
import Amazonka.CodeCommit.Types.File
import Amazonka.CodeCommit.Types.FileMetadata
import Amazonka.CodeCommit.Types.FileModeTypeEnum
import Amazonka.CodeCommit.Types.FileModes
import Amazonka.CodeCommit.Types.FileSizes
import Amazonka.CodeCommit.Types.Folder
import Amazonka.CodeCommit.Types.IsBinaryFile
import Amazonka.CodeCommit.Types.Location
import Amazonka.CodeCommit.Types.MergeHunk
import Amazonka.CodeCommit.Types.MergeHunkDetail
import Amazonka.CodeCommit.Types.MergeMetadata
import Amazonka.CodeCommit.Types.MergeOperations
import Amazonka.CodeCommit.Types.MergeOptionTypeEnum
import Amazonka.CodeCommit.Types.ObjectTypeEnum
import Amazonka.CodeCommit.Types.ObjectTypes
import Amazonka.CodeCommit.Types.OrderEnum
import Amazonka.CodeCommit.Types.OriginApprovalRuleTemplate
import Amazonka.CodeCommit.Types.OverrideStatus
import Amazonka.CodeCommit.Types.PullRequest
import Amazonka.CodeCommit.Types.PullRequestCreatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestEvent
import Amazonka.CodeCommit.Types.PullRequestEventType
import Amazonka.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Amazonka.CodeCommit.Types.PullRequestStatusEnum
import Amazonka.CodeCommit.Types.PullRequestTarget
import Amazonka.CodeCommit.Types.PutFileEntry
import Amazonka.CodeCommit.Types.ReactionForComment
import Amazonka.CodeCommit.Types.ReactionValueFormats
import Amazonka.CodeCommit.Types.RelativeFileVersionEnum
import Amazonka.CodeCommit.Types.ReplaceContentEntry
import Amazonka.CodeCommit.Types.ReplacementTypeEnum
import Amazonka.CodeCommit.Types.RepositoryMetadata
import Amazonka.CodeCommit.Types.RepositoryNameIdPair
import Amazonka.CodeCommit.Types.RepositoryTrigger
import Amazonka.CodeCommit.Types.RepositoryTriggerEventEnum
import Amazonka.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Amazonka.CodeCommit.Types.SetFileModeEntry
import Amazonka.CodeCommit.Types.SortByEnum
import Amazonka.CodeCommit.Types.SourceFileSpecifier
import Amazonka.CodeCommit.Types.SubModule
import Amazonka.CodeCommit.Types.SymbolicLink
import Amazonka.CodeCommit.Types.Target
import Amazonka.CodeCommit.Types.UserInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "CodeCommit",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "codecommit",
      Core.signingName = "codecommit",
      Core.version = "2015-04-13",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "CodeCommit",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | A destination ARN for the target service for the trigger is required,
-- but was not specified.
_RepositoryTriggerDestinationArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerDestinationArnRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerDestinationArnRequiredException"

-- | The title of the pull request is not valid. Pull request titles cannot
-- exceed 100 characters in length.
_InvalidTitleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTitleException =
  Core._MatchServiceError
    defaultService
    "InvalidTitleException"

-- | The SHA-256 hash signature for the rule content is not valid.
_InvalidRuleContentSha256Exception :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRuleContentSha256Exception =
  Core._MatchServiceError
    defaultService
    "InvalidRuleContentSha256Exception"

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_ActorDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ActorDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ActorDoesNotExistException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitIdRequiredException"

-- | The source commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidSourceCommitSpecifierException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSourceCommitSpecifierException =
  Core._MatchServiceError
    defaultService
    "InvalidSourceCommitSpecifierException"

-- | The commit cannot be created because no files have been specified as
-- added, updated, or changed (PutFile or DeleteFile) for the commit.
_FileEntryRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileEntryRequiredException =
  Core._MatchServiceError
    defaultService
    "FileEntryRequiredException"

-- | One or more events specified for the trigger is not valid. Check to make
-- sure that all events specified match the requirements for allowed
-- events.
_InvalidRepositoryTriggerEventsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerEventsException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerEventsException"

-- | The custom data provided for the trigger is not valid.
_InvalidRepositoryTriggerCustomDataException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerCustomDataException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerCustomDataException"

-- | A list of tag keys is required. The list cannot be empty or null.
_TagKeysListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagKeysListRequiredException =
  Core._MatchServiceError
    defaultService
    "TagKeysListRequiredException"

-- | The pull request has already had its approval rules set to override.
_OverrideAlreadySetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverrideAlreadySetException =
  Core._MatchServiceError
    defaultService
    "OverrideAlreadySetException"

-- | The Amazon Resource Name (ARN) of the user or identity is not valid.
_InvalidReactionUserArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReactionUserArnException =
  Core._MatchServiceError
    defaultService
    "InvalidReactionUserArnException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyAccessDeniedException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyAccessDeniedException"

-- | A file cannot be added to the repository because the specified path name
-- has the same name as a file that already exists in this repository.
-- Either provide a different name for the file, or specify a different
-- path for the file.
_DirectoryNameConflictsWithFileNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DirectoryNameConflictsWithFileNameException =
  Core._MatchServiceError
    defaultService
    "DirectoryNameConflictsWithFileNameException"

-- | An override status is required, but no value was provided. Valid values
-- include OVERRIDE and REVOKE.
_OverrideStatusRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_OverrideStatusRequiredException =
  Core._MatchServiceError
    defaultService
    "OverrideStatusRequiredException"

-- | The maximum number of allowed commit IDs in a batch request is 100.
-- Verify that your batch requests contains no more than 100 commit IDs,
-- and then try again.
_CommitIdsLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdsLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CommitIdsLimitExceededException"

-- | Cannot create the branch with the specified name because the commit
-- conflicts with an existing branch with the same name. Branch names must
-- be unique.
_BranchNameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameExistsException =
  Core._MatchServiceError
    defaultService
    "BranchNameExistsException"

-- | The specified file exceeds the file size limit for AWS CodeCommit. For
-- more information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_FileTooLargeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileTooLargeException =
  Core._MatchServiceError
    defaultService
    "FileTooLargeException"

-- | The specified file mode permission is not valid. For a list of valid
-- file mode permissions, see PutFile.
_InvalidFileModeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileModeException =
  Core._MatchServiceError
    defaultService
    "InvalidFileModeException"

-- | The repository does not contain any pull requests with that pull request
-- ID. Use GetPullRequest to verify the correct repository name for the
-- pull request ID.
_RepositoryNotAssociatedWithPullRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNotAssociatedWithPullRequestException =
  Core._MatchServiceError
    defaultService
    "RepositoryNotAssociatedWithPullRequestException"

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidContinuationTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidContinuationTokenException"

-- | The file could not be added because the provided parent commit ID is not
-- the current tip of the specified branch. To view the full commit ID of
-- the current head of the branch, use GetBranch.
_ParentCommitIdOutdatedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdOutdatedException =
  Core._MatchServiceError
    defaultService
    "ParentCommitIdOutdatedException"

-- | The specified reference does not exist. You must provide a full commit
-- ID.
_ReferenceDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ReferenceDoesNotExistException"

-- | The number of specified files to change as part of this commit exceeds
-- the maximum number of files that can be changed in a single commit.
-- Consider using a Git client for these changes.
_MaximumFileEntriesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumFileEntriesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumFileEntriesExceededException"

-- | The location of the file is not valid. Make sure that you include the
-- file name and extension.
_InvalidFileLocationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFileLocationException =
  Core._MatchServiceError
    defaultService
    "InvalidFileLocationException"

-- | The divergence between the tips of the provided commit specifiers is too
-- great to determine whether there might be any merge conflicts. Locally
-- compare the specifiers using @git diff@ or a diff tool.
_TipsDivergenceExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TipsDivergenceExceededException =
  Core._MatchServiceError
    defaultService
    "TipsDivergenceExceededException"

-- | The maximum number of approval rule templates for a repository has been
-- exceeded. You cannot associate more than 25 approval rule templates with
-- a repository.
_MaximumRuleTemplatesAssociatedWithRepositoryException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRuleTemplatesAssociatedWithRepositoryException =
  Core._MatchServiceError
    defaultService
    "MaximumRuleTemplatesAssociatedWithRepositoryException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionIntegrityChecksFailedException =
  Core._MatchServiceError
    defaultService
    "EncryptionIntegrityChecksFailedException"

-- | The targets for the pull request is not valid or not in a valid format.
-- Targets are a list of target objects. Each target object must contain
-- the full values for the repository name, source branch, and destination
-- branch for a pull request.
_InvalidTargetsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetsException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetsException"

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

-- | The specified repository name already exists.
_RepositoryNameExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNameExistsException =
  Core._MatchServiceError
    defaultService
    "RepositoryNameExistsException"

-- | The commit cannot be created because a specified file path points to a
-- submodule. Verify that the destination files have valid file paths that
-- do not point to a submodule.
_FilePathConflictsWithSubmodulePathException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FilePathConflictsWithSubmodulePathException =
  Core._MatchServiceError
    defaultService
    "FilePathConflictsWithSubmodulePathException"

-- | The specified approval rule template does not exist. Verify that the
-- name is correct and that you are signed in to the AWS Region where the
-- template was created, and then try again.
_ApprovalRuleTemplateDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateDoesNotExistException"

-- | The pull request status update is not valid. The only valid update is
-- from @OPEN@ to @CLOSED@.
_InvalidPullRequestStatusUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusUpdateException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestStatusUpdateException"

-- | A pull request target is required. It cannot be empty or null. A pull
-- request target must contain the full values for the repository name,
-- source branch, and destination branch for the pull request.
_TargetRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetRequiredException =
  Core._MatchServiceError
    defaultService
    "TargetRequiredException"

-- | The approval rule cannot be modified for the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotModifyApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotModifyApprovalRuleFromTemplateException =
  Core._MatchServiceError
    defaultService
    "CannotModifyApprovalRuleFromTemplateException"

-- | A commit was not specified.
_CommitRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitRequiredException"

-- | The specified value for the number of merge hunks to return is not
-- valid.
_InvalidMaxMergeHunksException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxMergeHunksException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxMergeHunksException"

-- | The commit cannot be created because one of the changes specifies
-- copying or moving a .gitkeep file.
_RestrictedSourceFileException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RestrictedSourceFileException =
  Core._MatchServiceError
    defaultService
    "RestrictedSourceFileException"

-- | More than one conflict resolution entries exists for the conflict. A
-- conflict can have only one conflict resolution entry.
_MultipleConflictResolutionEntriesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleConflictResolutionEntriesException =
  Core._MatchServiceError
    defaultService
    "MultipleConflictResolutionEntriesException"

-- | The specified branch name is not valid because it is a tag name. Enter
-- the name of a branch in the repository. For a list of valid branch
-- names, use ListBranches.
_BranchNameIsTagNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameIsTagNameException =
  Core._MatchServiceError
    defaultService
    "BranchNameIsTagNameException"

-- | The comment ID is not in a valid format. Make sure that you have
-- provided the full comment ID.
_InvalidCommentIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommentIdException =
  Core._MatchServiceError
    defaultService
    "InvalidCommentIdException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the user who initiated the change for the pull
-- request, and then try again.
_InvalidActorArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidActorArnException =
  Core._MatchServiceError
    defaultService
    "InvalidActorArnException"

-- | The pull request status cannot be updated because it is already closed.
_PullRequestAlreadyClosedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestAlreadyClosedException =
  Core._MatchServiceError
    defaultService
    "PullRequestAlreadyClosedException"

-- | The approval rule template is associated with one or more repositories.
-- You cannot delete a template that is associated with a repository.
-- Remove all associations, and then try again.
_ApprovalRuleTemplateInUseException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateInUseException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateInUseException"

-- | The name of the trigger is not valid.
_InvalidRepositoryTriggerNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerNameException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerNameException"

-- | A valid Amazon Resource Name (ARN) for an AWS CodeCommit resource is
-- required. For a list of valid resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException =
  Core._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | The commit cannot be created because no source files or file content
-- have been specified for the commit.
_SourceFileOrContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceFileOrContentRequiredException =
  Core._MatchServiceError
    defaultService
    "SourceFileOrContentRequiredException"

-- | Automerge was specified for resolving the conflict, but the specified
-- replacement type is not valid.
_InvalidReplacementTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidReplacementTypeException"

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

-- | The maximum number of tags for an AWS CodeCommit resource has been
-- exceeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The source branch and destination branch for the pull request are the
-- same. You must specify different branches for the source and
-- destination.
_SourceAndDestinationAreSameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SourceAndDestinationAreSameException =
  Core._MatchServiceError
    defaultService
    "SourceAndDestinationAreSameException"

-- | The destination commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidDestinationCommitSpecifierException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDestinationCommitSpecifierException =
  Core._MatchServiceError
    defaultService
    "InvalidDestinationCommitSpecifierException"

-- | The position is not valid. Make sure that the line number exists in the
-- version of the file you want to comment on.
_InvalidFilePositionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidFilePositionException =
  Core._MatchServiceError
    defaultService
    "InvalidFilePositionException"

-- | The user name is not valid because it has exceeded the character limit
-- for author names.
_NameLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NameLengthExceededException =
  Core._MatchServiceError
    defaultService
    "NameLengthExceededException"

-- | The specified blob is not valid.
_InvalidBlobIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBlobIdException =
  Core._MatchServiceError
    defaultService
    "InvalidBlobIdException"

-- | The specified path does not exist.
_PathDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PathDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "PathDoesNotExistException"

-- | The parent commit ID is not valid because it does not exist. The
-- specified parent commit ID does not exist in the specified branch of the
-- repository.
_ParentCommitDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ParentCommitDoesNotExistException"

-- | The map of tags is not valid.
_InvalidTagsMapException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagsMapException =
  Core._MatchServiceError
    defaultService
    "InvalidTagsMapException"

-- | The commit message is too long. Provide a shorter string.
_CommitMessageLengthExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitMessageLengthExceededException =
  Core._MatchServiceError
    defaultService
    "CommitMessageLengthExceededException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "RepositoryDoesNotExistException"

-- | Automerge was specified for resolving the conflict, but the replacement
-- type is not valid or content is missing.
_InvalidReplacementContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementContentException =
  Core._MatchServiceError
    defaultService
    "InvalidReplacementContentException"

-- | The specified sort order is not valid.
_InvalidOrderException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOrderException =
  Core._MatchServiceError
    defaultService
    "InvalidOrderException"

-- | The list of tags is not valid.
_InvalidTagKeysListException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTagKeysListException =
  Core._MatchServiceError
    defaultService
    "InvalidTagKeysListException"

-- | A pull request status is required, but none was provided.
_PullRequestStatusRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestStatusRequiredException =
  Core._MatchServiceError
    defaultService
    "PullRequestStatusRequiredException"

-- | The pull request ID is not valid. Make sure that you have provided the
-- full ID and that the pull request is in the specified repository, and
-- then try again.
_InvalidPullRequestIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestIdException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestIdException"

-- | The description for the approval rule template is not valid because it
-- exceeds the maximum characters allowed for a description. For more
-- information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateDescriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateDescriptionException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateDescriptionException"

-- | The approval cannot be applied because the user approving the pull
-- request matches the user who created the pull request. You cannot
-- approve a pull request that you created.
_PullRequestCannotBeApprovedByAuthorException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestCannotBeApprovedByAuthorException =
  Core._MatchServiceError
    defaultService
    "PullRequestCannotBeApprovedByAuthorException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyDisabledException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyDisabledException"

-- | A revision ID is required, but was not provided.
_RevisionIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionIdRequiredException =
  Core._MatchServiceError
    defaultService
    "RevisionIdRequiredException"

-- | The comment is empty. You must provide some content for a comment. The
-- content cannot be null.
_CommentContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentContentRequiredException =
  Core._MatchServiceError
    defaultService
    "CommentContentRequiredException"

-- | The specified email address either contains one or more characters that
-- are not allowed, or it exceeds the maximum number of characters allowed
-- for an email address.
_InvalidEmailException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidEmailException =
  Core._MatchServiceError
    defaultService
    "InvalidEmailException"

-- | The revision ID provided in the request does not match the current
-- revision ID. Use GetPullRequest to retrieve the current revision ID.
_RevisionNotCurrentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RevisionNotCurrentException =
  Core._MatchServiceError
    defaultService
    "RevisionNotCurrentException"

-- | The number of files to load exceeds the allowed limit.
_MaximumFileContentToLoadExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumFileContentToLoadExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumFileContentToLoadExceededException"

-- | An approval rule with that name already exists. Approval rule names must
-- be unique within the scope of a pull request.
_ApprovalRuleNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleNameAlreadyExistsException"

-- | The AWS Region for the trigger target does not match the AWS Region for
-- the repository. Triggers must be created in the same Region as the
-- target for the trigger.
_InvalidRepositoryTriggerRegionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerRegionException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerRegionException"

-- | You cannot create an approval rule template with that name because a
-- template with that name already exists in this AWS Region for your AWS
-- account. Approval rule template names must be unique.
_ApprovalRuleTemplateNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameAlreadyExistsException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameAlreadyExistsException"

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

-- | The comment ID is missing or null. A comment ID is required.
_CommentIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentIdRequiredException =
  Core._MatchServiceError
    defaultService
    "CommentIdRequiredException"

-- | A reaction value is required.
_ReactionValueRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReactionValueRequiredException =
  Core._MatchServiceError
    defaultService
    "ReactionValueRequiredException"

-- | The file was not added or updated because the content of the file is
-- exactly the same as the content of that file in the repository and
-- branch that you specified.
_SameFileContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_SameFileContentException =
  Core._MatchServiceError
    defaultService
    "SameFileContentException"

-- | The specified number of maximum results is not valid.
_InvalidMaxResultsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxResultsException"

-- | The revision ID is not valid. Use GetPullRequest to determine the value.
_InvalidRevisionIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRevisionIdException =
  Core._MatchServiceError
    defaultService
    "InvalidRevisionIdException"

-- | The specified reference name is not valid.
_InvalidBranchNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidBranchNameException =
  Core._MatchServiceError
    defaultService
    "InvalidBranchNameException"

-- | The value of the reaction is not valid. For more information, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.
_InvalidReactionValueException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidReactionValueException =
  Core._MatchServiceError
    defaultService
    "InvalidReactionValueException"

-- | The content for the approval rule is not valid.
_InvalidApprovalRuleContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleContentException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleContentException"

-- | An approval rule template name is required, but was not specified.
_ApprovalRuleTemplateNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameRequiredException"

-- | The content of the approval rule template is not valid.
_InvalidApprovalRuleTemplateContentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateContentException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateContentException"

-- | The client request token is not valid.
_InvalidClientRequestTokenException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidClientRequestTokenException =
  Core._MatchServiceError
    defaultService
    "InvalidClientRequestTokenException"

-- | The specified target branch is not valid.
_InvalidTargetBranchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetBranchException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetBranchException"

-- | The commit cannot be created because no changes will be made to the
-- repository as a result of this commit. A commit must contain at least
-- one change.
_NoChangeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NoChangeException =
  Core._MatchServiceError
    defaultService
    "NoChangeException"

-- | The number of triggers allowed for the repository was exceeded.
_MaximumRepositoryTriggersExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryTriggersExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumRepositoryTriggersExceededException"

-- | The pull request ID could not be found. Make sure that you have
-- specified the correct repository name and pull request ID, and then try
-- again.
_PullRequestDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "PullRequestDoesNotExistException"

-- | A replacement type is required.
_ReplacementTypeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplacementTypeRequiredException =
  Core._MatchServiceError
    defaultService
    "ReplacementTypeRequiredException"

-- | The number of items to compare between the source or destination
-- branches and the merge base has exceeded the maximum allowed.
_MaximumItemsToCompareExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumItemsToCompareExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumItemsToCompareExceededException"

-- | The commit cannot be created because both a source file and file content
-- have been specified for the same file. You cannot provide both. Either
-- specify a source file or provide the file content directly.
_FileContentAndSourceFileSpecifiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentAndSourceFileSpecifiedException =
  Core._MatchServiceError
    defaultService
    "FileContentAndSourceFileSpecifiedException"

-- | At least one repository name object is required, but was not specified.
_RepositoryNamesRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNamesRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryNamesRequiredException"

-- | A pull request title is required. It cannot be empty or null.
_TitleRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TitleRequiredException =
  Core._MatchServiceError
    defaultService
    "TitleRequiredException"

-- | The folderPath for a location cannot be null.
_PathRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PathRequiredException =
  Core._MatchServiceError
    defaultService
    "PathRequiredException"

-- | USE_NEW_CONTENT was specified, but no replacement content has been
-- provided.
_ReplacementContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReplacementContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ReplacementContentRequiredException"

-- | The name of the approval rule template is not valid. Template names must
-- be between 1 and 100 valid characters in length. For more information
-- about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateNameException"

-- | The file cannot be added because it is empty. Empty files cannot be
-- added to the repository with this API.
_FileContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentRequiredException =
  Core._MatchServiceError
    defaultService
    "FileContentRequiredException"

-- | The specified reference is not a supported type.
_ReferenceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceTypeNotSupportedException =
  Core._MatchServiceError
    defaultService
    "ReferenceTypeNotSupportedException"

-- | At least one branch name is required, but was not specified in the
-- trigger configuration.
_RepositoryTriggerBranchNameListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerBranchNameListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerBranchNameListRequiredException"

-- | The number of approvals required for the approval rule exceeds the
-- maximum number allowed.
_MaximumNumberOfApprovalsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumNumberOfApprovalsExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumNumberOfApprovalsExceededException"

-- | At least one event for the trigger is required, but was not specified.
_RepositoryTriggerEventsListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerEventsListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerEventsListRequiredException"

-- | The specified file does not exist. Verify that you have used the correct
-- file name, full path, and extension.
_FileDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "FileDoesNotExistException"

-- | The state for the approval is not valid. Valid values include APPROVE
-- and REVOKE.
_InvalidApprovalStateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalStateException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalStateException"

-- | You cannot modify or delete this comment. Only comment authors can
-- modify or delete their comments.
_CommentNotCreatedByCallerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentNotCreatedByCallerException =
  Core._MatchServiceError
    defaultService
    "CommentNotCreatedByCallerException"

-- | The number of reactions has been exceeded. Reactions are limited to one
-- reaction per user for each individual comment ID.
_ReactionLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReactionLimitExceededException =
  Core._MatchServiceError
    defaultService
    "ReactionLimitExceededException"

-- | The number of branches for the trigger was exceeded.
_MaximumBranchesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumBranchesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumBranchesExceededException"

-- | The specified conflict detail level is not valid.
_InvalidConflictDetailLevelException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictDetailLevelException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictDetailLevelException"

-- | One or more branch names specified for the trigger is not valid.
_InvalidRepositoryTriggerBranchNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerBranchNameException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerBranchNameException"

-- | The specified folder does not exist. Either the folder name is not
-- correct, or you did not enter the full path to the folder.
_FolderDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FolderDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "FolderDoesNotExistException"

-- | The before commit ID and the after commit ID are the same, which is not
-- valid. The before commit ID and the after commit ID must be different
-- commit IDs.
_BeforeCommitIdAndAfterCommitIdAreSameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BeforeCommitIdAndAfterCommitIdAreSameException =
  Core._MatchServiceError
    defaultService
    "BeforeCommitIdAndAfterCommitIdAreSameException"

-- | The specified blob does not exist.
_BlobIdDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BlobIdDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "BlobIdDoesNotExistException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryLimitExceededException =
  Core._MatchServiceError
    defaultService
    "RepositoryLimitExceededException"

-- | This comment has already been deleted. You cannot edit or delete a
-- deleted comment.
_CommentDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentDeletedException =
  Core._MatchServiceError
    defaultService
    "CommentDeletedException"

-- | A reference name is required, but none was provided.
_ReferenceNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ReferenceNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ReferenceNameRequiredException"

-- | The merge cannot be completed because the target branch has been
-- modified. Another user might have modified the target branch while the
-- merge was in progress. Wait a few minutes, and then try again.
_ConcurrentReferenceUpdateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConcurrentReferenceUpdateException =
  Core._MatchServiceError
    defaultService
    "ConcurrentReferenceUpdateException"

-- | An array of target objects is required. It cannot be empty or null.
_TargetsRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TargetsRequiredException =
  Core._MatchServiceError
    defaultService
    "TargetsRequiredException"

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

-- | A branch name is required, but was not specified.
_BranchNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BranchNameRequiredException =
  Core._MatchServiceError
    defaultService
    "BranchNameRequiredException"

-- | The specified deletion parameter is not valid.
_InvalidDeletionParameterException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionParameterException =
  Core._MatchServiceError
    defaultService
    "InvalidDeletionParameterException"

-- | The specified commit is not valid.
_InvalidCommitException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommitException =
  Core._MatchServiceError
    defaultService
    "InvalidCommitException"

-- | The number of allowed conflict resolution entries was exceeded.
_MaximumConflictResolutionEntriesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumConflictResolutionEntriesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumConflictResolutionEntriesExceededException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException =
  Core._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The commit cannot be created because one or more files specified in the
-- commit reference both a file and a folder.
_PutFileEntryConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PutFileEntryConflictException =
  Core._MatchServiceError
    defaultService
    "PutFileEntryConflictException"

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

-- | The specified value for the number of conflict files to return is not
-- valid.
_InvalidMaxConflictFilesException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMaxConflictFilesException =
  Core._MatchServiceError
    defaultService
    "InvalidMaxConflictFilesException"

-- | The target for the pull request is not valid. A target must contain the
-- full values for the repository name, source branch, and destination
-- branch for the pull request.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | A merge option or stategy is required, and none was provided.
_MergeOptionRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MergeOptionRequiredException =
  Core._MatchServiceError
    defaultService
    "MergeOptionRequiredException"

-- | A pull request ID is required, but none was provided.
_PullRequestIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestIdRequiredException =
  Core._MatchServiceError
    defaultService
    "PullRequestIdRequiredException"

-- | The specified tag is not valid. Key names cannot be prefixed with aws:.
_InvalidSystemTagUsageException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidSystemTagUsageException =
  Core._MatchServiceError
    defaultService
    "InvalidSystemTagUsageException"

-- | The pull request event type is not valid.
_InvalidPullRequestEventTypeException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestEventTypeException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestEventTypeException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyUnavailableException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyUnavailableException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyNotFoundException =
  Core._MatchServiceError
    defaultService
    "EncryptionKeyNotFoundException"

-- | The specified approval rule does not exist.
_ApprovalRuleDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleDoesNotExistException"

-- | The maximum number of allowed repository names was exceeded. Currently,
-- this number is 100.
_MaximumRepositoryNamesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryNamesExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumRepositoryNamesExceededException"

-- | You cannot create the pull request because the repository has too many
-- open pull requests. The maximum number of open pull requests for a
-- repository is 1,000. Close one or more open pull requests, and then try
-- again.
_MaximumOpenPullRequestsExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MaximumOpenPullRequestsExceededException =
  Core._MatchServiceError
    defaultService
    "MaximumOpenPullRequestsExceededException"

-- | The specified conflict resolution strategy is not valid.
_InvalidConflictResolutionStrategyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionStrategyException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictResolutionStrategyException"

-- | The specified path is not valid.
_InvalidPathException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPathException =
  Core._MatchServiceError
    defaultService
    "InvalidPathException"

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the
-- specified destination. The most common reason for this error is that the
-- ARN does not meet the requirements for the service type.
_InvalidRepositoryTriggerDestinationArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerDestinationArnException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerDestinationArnException"

-- | The file cannot be added because it is too large. The maximum file size
-- is 6 MB, and the combined file content change size is 7 MB. Consider
-- making these changes using a Git client.
_FileContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FileContentSizeLimitExceededException"

-- | The value for the resource ARN is not valid. For more information about
-- resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_InvalidResourceArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidResourceArnException =
  Core._MatchServiceError
    defaultService
    "InvalidResourceArnException"

-- | The pull request cannot be merged automatically into the destination
-- branch. You must manually merge the branches and resolve any conflicts.
_ManualMergeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ManualMergeRequiredException =
  Core._MatchServiceError
    defaultService
    "ManualMergeRequiredException"

-- | The override status is not valid. Valid statuses are OVERRIDE and
-- REVOKE.
_InvalidOverrideStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOverrideStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidOverrideStatusException"

-- | The maximum number of approval rule templates has been exceeded for this
-- AWS Region.
_NumberOfRuleTemplatesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfRuleTemplatesExceededException =
  Core._MatchServiceError
    defaultService
    "NumberOfRuleTemplatesExceededException"

-- | A blob ID is required, but was not specified.
_BlobIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_BlobIdRequiredException =
  Core._MatchServiceError
    defaultService
    "BlobIdRequiredException"

-- | A name for the trigger is required, but was not specified.
_RepositoryTriggerNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerNameRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggerNameRequiredException"

-- | The approval rule cannot be added. The pull request has the maximum
-- number of approval rules associated with it.
_NumberOfRulesExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NumberOfRulesExceededException =
  Core._MatchServiceError
    defaultService
    "NumberOfRulesExceededException"

-- | The pull request cannot be merged because one or more approval rules
-- applied to the pull request have conditions that have not been met.
_PullRequestApprovalRulesNotSatisfiedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_PullRequestApprovalRulesNotSatisfiedException =
  Core._MatchServiceError
    defaultService
    "PullRequestApprovalRulesNotSatisfiedException"

-- | A list of commit IDs is required, but was either not specified or the
-- list was empty.
_CommitIdsListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdsListRequiredException =
  Core._MatchServiceError
    defaultService
    "CommitIdsListRequiredException"

-- | The commit cannot be created because at least one of the overall changes
-- in the commit results in a folder whose contents exceed the limit of 6
-- MB. Either reduce the number and size of your changes, or split the
-- changes across multiple folders.
_FolderContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FolderContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "FolderContentSizeLimitExceededException"

-- | A repository name is required, but was not specified.
_RepositoryNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryNameRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryNameRequiredException"

-- | The client request token is not valid. Either the token is not in a
-- valid format, or the token has been used in a previous request and
-- cannot be reused.
_IdempotencyParameterMismatchException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotencyParameterMismatchException =
  Core._MatchServiceError
    defaultService
    "IdempotencyParameterMismatchException"

-- | A file cannot be added to the repository because the specified file name
-- has the same name as a directory in this repository. Either provide
-- another name for the file, or add the file in a directory that does not
-- match the file name.
_FileNameConflictsWithDirectoryNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileNameConflictsWithDirectoryNameException =
  Core._MatchServiceError
    defaultService
    "FileNameConflictsWithDirectoryNameException"

-- | The comment is too large. Comments are limited to 1,000 characters.
_CommentContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentContentSizeLimitExceededException =
  Core._MatchServiceError
    defaultService
    "CommentContentSizeLimitExceededException"

-- | The specified commit ID does not exist.
_CommitIdDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitIdDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommitIdDoesNotExistException"

-- | The approval rule cannot be deleted from the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotDeleteApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotDeleteApprovalRuleFromTemplateException =
  Core._MatchServiceError
    defaultService
    "CannotDeleteApprovalRuleFromTemplateException"

-- | A parent commit ID is required. To view the full commit ID of a branch
-- in a repository, use GetBranch or a Git command (for example, git pull
-- or git log).
_ParentCommitIdRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdRequiredException =
  Core._MatchServiceError
    defaultService
    "ParentCommitIdRequiredException"

-- | No comment exists with the provided ID. Verify that you have used the
-- correct ID, and then try again.
_CommentDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommentDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommentDoesNotExistException"

-- | A map of tags is required.
_TagsMapRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagsMapRequiredException =
  Core._MatchServiceError
    defaultService
    "TagsMapRequiredException"

-- | The specified commit does not exist or no commit was specified, and the
-- specified repository has no default branch.
_CommitDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CommitDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "CommitDoesNotExistException"

-- | The specified conflict resolution list is not valid.
_InvalidConflictResolutionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionException =
  Core._MatchServiceError
    defaultService
    "InvalidConflictResolutionException"

-- | The commit cannot be created because no file mode has been specified. A
-- file mode is required to update mode permissions for a file.
_FileModeRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_FileModeRequiredException =
  Core._MatchServiceError
    defaultService
    "FileModeRequiredException"

-- | The specified merge option is not valid for this operation. Not all
-- merge strategies are supported for all operations.
_InvalidMergeOptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidMergeOptionException =
  Core._MatchServiceError
    defaultService
    "InvalidMergeOptionException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the author of the pull request, and then try
-- again.
_InvalidAuthorArnException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidAuthorArnException =
  Core._MatchServiceError
    defaultService
    "InvalidAuthorArnException"

-- | The pull request status is not valid. The only valid values are @OPEN@
-- and @CLOSED@.
_InvalidPullRequestStatusException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusException =
  Core._MatchServiceError
    defaultService
    "InvalidPullRequestStatusException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryDescriptionException =
  Core._MatchServiceError
    defaultService
    "InvalidRepositoryDescriptionException"

-- | The content for the approval rule is empty. You must provide some
-- content for an approval rule. The content cannot be null.
_ApprovalRuleContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleContentRequiredException"

-- | You cannot include more than one repository in a pull request. Make sure
-- you have specified only one repository name in your request, and then
-- try again.
_MultipleRepositoriesInPullRequestException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_MultipleRepositoriesInPullRequestException =
  Core._MatchServiceError
    defaultService
    "MultipleRepositoriesInPullRequestException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidCommitIdException =
  Core._MatchServiceError
    defaultService
    "InvalidCommitIdException"

-- | The tip of the source branch in the destination repository does not
-- match the tip of the source branch specified in your request. The pull
-- request might have been updated. Make sure that you have the latest
-- changes.
_TipOfSourceReferenceIsDifferentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TipOfSourceReferenceIsDifferentException =
  Core._MatchServiceError
    defaultService
    "TipOfSourceReferenceIsDifferentException"

-- | The content for the approval rule template is empty. You must provide
-- some content for an approval rule template. The content cannot be null.
_ApprovalRuleTemplateContentRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateContentRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleTemplateContentRequiredException"

-- | An approval rule name is required, but was not specified.
_ApprovalRuleNameRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalRuleNameRequiredException"

-- | The tag policy is not valid.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The name for the approval rule is not valid.
_InvalidApprovalRuleNameException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleNameException =
  Core._MatchServiceError
    defaultService
    "InvalidApprovalRuleNameException"

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_AuthorDoesNotExistException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_AuthorDoesNotExistException =
  Core._MatchServiceError
    defaultService
    "AuthorDoesNotExistException"

-- | Either the enum is not in a valid format, or the specified file version
-- enum is not valid in respect to the current file version.
_InvalidRelativeFileVersionEnumException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidRelativeFileVersionEnumException =
  Core._MatchServiceError
    defaultService
    "InvalidRelativeFileVersionEnumException"

-- | The list of triggers for the repository is required, but was not
-- specified.
_RepositoryTriggersListRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggersListRequiredException =
  Core._MatchServiceError
    defaultService
    "RepositoryTriggersListRequiredException"

-- | An approval state is required, but was not specified.
_ApprovalStateRequiredException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ApprovalStateRequiredException =
  Core._MatchServiceError
    defaultService
    "ApprovalStateRequiredException"

-- | The specified branch is the default branch for the repository, and
-- cannot be deleted. To delete this branch, you must first set another
-- branch as the default branch.
_DefaultBranchCannotBeDeletedException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_DefaultBranchCannotBeDeletedException =
  Core._MatchServiceError
    defaultService
    "DefaultBranchCannotBeDeletedException"

-- | The parent commit ID is not valid. The commit ID cannot be empty, and
-- must match the head commit ID for the branch of the repository where you
-- want to add or update a file.
_InvalidParentCommitIdException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidParentCommitIdException =
  Core._MatchServiceError
    defaultService
    "InvalidParentCommitIdException"
