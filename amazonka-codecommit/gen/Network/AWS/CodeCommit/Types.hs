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
    _ActorDoesNotExistException,
    _InvalidPullRequestStatusUpdateException,
    _NameLengthExceededException,
    _ApprovalRuleNameAlreadyExistsException,
    _PullRequestIdRequiredException,
    _InvalidConflictDetailLevelException,
    _InvalidPullRequestIdException,
    _TargetRequiredException,
    _CommitRequiredException,
    _FileNameConflictsWithDirectoryNameException,
    _ReferenceDoesNotExistException,
    _EncryptionKeyNotFoundException,
    _TargetsRequiredException,
    _CommentContentRequiredException,
    _InvalidSystemTagUsageException,
    _FileEntryRequiredException,
    _InvalidTargetsException,
    _ManualMergeRequiredException,
    _BranchNameExistsException,
    _ParentCommitDoesNotExistException,
    _SamePathRequestException,
    _AuthorDoesNotExistException,
    _TooManyTagsException,
    _SourceAndDestinationAreSameException,
    _EncryptionIntegrityChecksFailedException,
    _InvalidApprovalRuleTemplateDescriptionException,
    _CommentDeletedException,
    _InvalidReplacementContentException,
    _RepositoryTriggerEventsListRequiredException,
    _FileContentRequiredException,
    _SourceFileOrContentRequiredException,
    _InvalidTitleException,
    _InvalidApprovalRuleContentException,
    _InvalidSourceCommitSpecifierException,
    _InvalidConflictResolutionException,
    _InvalidOverrideStatusException,
    _PullRequestStatusRequiredException,
    _MaximumItemsToCompareExceededException,
    _InvalidPullRequestStatusException,
    _ParentCommitIdRequiredException,
    _RepositoryDoesNotExistException,
    _PullRequestApprovalRulesNotSatisfiedException,
    _ApprovalRuleContentRequiredException,
    _TitleRequiredException,
    _FileContentSizeLimitExceededException,
    _InvalidRepositoryTriggerNameException,
    _RepositoryTriggerNameRequiredException,
    _NumberOfRuleTemplatesExceededException,
    _RepositoryNameRequiredException,
    _InvalidFileModeException,
    _InvalidRepositoryTriggerEventsException,
    _InvalidPathException,
    _InvalidTargetBranchException,
    _InvalidActorArnException,
    _RevisionIdRequiredException,
    _InvalidBlobIdException,
    _MaximumFileEntriesExceededException,
    _ApprovalRuleTemplateNameRequiredException,
    _RepositoryNamesRequiredException,
    _PullRequestDoesNotExistException,
    _ReplacementTypeRequiredException,
    _InvalidReplacementTypeException,
    _FileTooLargeException,
    _CannotDeleteApprovalRuleFromTemplateException,
    _ReactionLimitExceededException,
    _RevisionNotCurrentException,
    _InvalidApprovalRuleNameException,
    _CommitIdDoesNotExistException,
    _RepositoryTriggersListRequiredException,
    _InvalidRelativeFileVersionEnumException,
    _TagPolicyException,
    _ApprovalRuleDoesNotExistException,
    _RepositoryTriggerBranchNameListRequiredException,
    _InvalidClientRequestTokenException,
    _IdempotencyParameterMismatchException,
    _InvalidRepositoryTriggerDestinationArnException,
    _BranchNameRequiredException,
    _RepositoryTriggerDestinationArnRequiredException,
    _EncryptionKeyUnavailableException,
    _InvalidConflictResolutionStrategyException,
    _MultipleConflictResolutionEntriesException,
    _FileDoesNotExistException,
    _TagKeysListRequiredException,
    _InvalidReferenceNameException,
    _CommitIdRequiredException,
    _ReferenceNameRequiredException,
    _MaximumRuleTemplatesAssociatedWithRepositoryException,
    _CommitMessageLengthExceededException,
    _MaximumNumberOfApprovalsExceededException,
    _InvalidTagKeysListException,
    _EncryptionKeyDisabledException,
    _DirectoryNameConflictsWithFileNameException,
    _InvalidSortByException,
    _InvalidDestinationCommitSpecifierException,
    _CommentDoesNotExistException,
    _ApprovalRuleTemplateNameAlreadyExistsException,
    _InvalidRepositoryTriggerBranchNameException,
    _MaximumOpenPullRequestsExceededException,
    _PullRequestAlreadyClosedException,
    _InvalidCommitException,
    _ReferenceTypeNotSupportedException,
    _CommentNotCreatedByCallerException,
    _InvalidTargetException,
    _InvalidRepositoryTriggerCustomDataException,
    _OverrideAlreadySetException,
    _InvalidContinuationTokenException,
    _InvalidRepositoryTriggerRegionException,
    _InvalidReactionValueException,
    _TipsDivergenceExceededException,
    _EncryptionKeyAccessDeniedException,
    _ReactionValueRequiredException,
    _ResourceArnRequiredException,
    _PathDoesNotExistException,
    _ReplacementContentRequiredException,
    _InvalidResourceArnException,
    _ParentCommitIdOutdatedException,
    _InvalidMaxMergeHunksException,
    _RestrictedSourceFileException,
    _InvalidFilePositionException,
    _CannotModifyApprovalRuleFromTemplateException,
    _InvalidMaxConflictFilesException,
    _CommentContentSizeLimitExceededException,
    _InvalidApprovalStateException,
    _MaximumBranchesExceededException,
    _OverrideStatusRequiredException,
    _InvalidPullRequestEventTypeException,
    _InvalidApprovalRuleTemplateContentException,
    _CommitIdsLimitExceededException,
    _ApprovalStateRequiredException,
    _InvalidAuthorArnException,
    _MaximumConflictResolutionEntriesExceededException,
    _InvalidParentCommitIdException,
    _RepositoryNotAssociatedWithPullRequestException,
    _ApprovalRuleTemplateContentRequiredException,
    _ConcurrentReferenceUpdateException,
    _MaximumFileContentToLoadExceededException,
    _TagsMapRequiredException,
    _InvalidRepositoryNameException,
    _FolderContentSizeLimitExceededException,
    _CommitIdsListRequiredException,
    _FileModeRequiredException,
    _InvalidTagsMapException,
    _RepositoryLimitExceededException,
    _NumberOfRulesExceededException,
    _InvalidDeletionParameterException,
    _InvalidReactionUserArnException,
    _DefaultBranchCannotBeDeletedException,
    _BranchNameIsTagNameException,
    _PathRequiredException,
    _FilePathConflictsWithSubmodulePathException,
    _BranchDoesNotExistException,
    _InvalidCommentIdException,
    _InvalidMaxResultsException,
    _InvalidRevisionIdException,
    _PullRequestCannotBeApprovedByAuthorException,
    _ApprovalRuleNameRequiredException,
    _BlobIdRequiredException,
    _InvalidApprovalRuleTemplateNameException,
    _InvalidDescriptionException,
    _NoChangeException,
    _CommentIdRequiredException,
    _MultipleRepositoriesInPullRequestException,
    _InvalidOrderException,
    _ClientRequestTokenRequiredException,
    _InvalidMergeOptionException,
    _MergeOptionRequiredException,
    _InvalidBranchNameException,
    _InvalidFileLocationException,
    _ApprovalRuleTemplateDoesNotExistException,
    _MaximumRepositoryTriggersExceededException,
    _CommitDoesNotExistException,
    _BeforeCommitIdAndAfterCommitIdAreSameException,
    _RepositoryNameExistsException,
    _InvalidCommitIdException,
    _InvalidRepositoryDescriptionException,
    _MaximumRepositoryNamesExceededException,
    _SameFileContentException,
    _ApprovalRuleTemplateInUseException,
    _InvalidEmailException,
    _TipOfSourceReferenceIsDifferentException,
    _FolderDoesNotExistException,
    _InvalidRuleContentSha256Exception,
    _BlobIdDoesNotExistException,
    _PutFileEntryConflictException,
    _FileContentAndSourceFileSpecifiedException,

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
    approvalRule_originApprovalRuleTemplate,
    approvalRule_creationDate,
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
    comment_reactionCounts,
    comment_content,
    comment_commentId,
    comment_inReplyTo,
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
    commentsForComparedCommit_comments,
    commentsForComparedCommit_afterCommitId,
    commentsForComparedCommit_location,

    -- * CommentsForPullRequest
    CommentsForPullRequest (..),
    newCommentsForPullRequest,
    commentsForPullRequest_beforeBlobId,
    commentsForPullRequest_repositoryName,
    commentsForPullRequest_beforeCommitId,
    commentsForPullRequest_afterBlobId,
    commentsForPullRequest_pullRequestId,
    commentsForPullRequest_comments,
    commentsForPullRequest_afterCommitId,
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
    conflict_mergeHunks,
    conflict_conflictMetadata,

    -- * ConflictMetadata
    ConflictMetadata (..),
    newConflictMetadata,
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
    mergeHunk_isConflict,
    mergeHunk_destination,
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
    pullRequestEvent_pullRequestEventType,
    pullRequestEvent_eventDate,
    pullRequestEvent_pullRequestCreatedEventMetadata,
    pullRequestEvent_pullRequestId,
    pullRequestEvent_approvalRuleOverriddenEventMetadata,
    pullRequestEvent_actorArn,
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
    pullRequestTarget_destinationCommit,
    pullRequestTarget_mergeBase,

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
    reactionValueFormats_unicode,
    reactionValueFormats_shortCode,
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
    repositoryMetadata_cloneUrlSsh,
    repositoryMetadata_cloneUrlHttp,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "CodeCommit",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcPrefix = "codecommit",
      Prelude._svcVersion = "2015-04-13",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "CodeCommit",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_ActorDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ActorDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ActorDoesNotExistException"

-- | The pull request status update is not valid. The only valid update is
-- from @OPEN@ to @CLOSED@.
_InvalidPullRequestStatusUpdateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPullRequestStatusUpdateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPullRequestStatusUpdateException"

-- | The user name is not valid because it has exceeded the character limit
-- for author names.
_NameLengthExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NameLengthExceededException =
  Prelude._MatchServiceError
    defaultService
    "NameLengthExceededException"

-- | An approval rule with that name already exists. Approval rule names must
-- be unique within the scope of a pull request.
_ApprovalRuleNameAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleNameAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleNameAlreadyExistsException"

-- | A pull request ID is required, but none was provided.
_PullRequestIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestIdRequiredException"

-- | The specified conflict detail level is not valid.
_InvalidConflictDetailLevelException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConflictDetailLevelException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConflictDetailLevelException"

-- | The pull request ID is not valid. Make sure that you have provided the
-- full ID and that the pull request is in the specified repository, and
-- then try again.
_InvalidPullRequestIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPullRequestIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPullRequestIdException"

-- | A pull request target is required. It cannot be empty or null. A pull
-- request target must contain the full values for the repository name,
-- source branch, and destination branch for the pull request.
_TargetRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TargetRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TargetRequiredException"

-- | A commit was not specified.
_CommitRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitRequiredException =
  Prelude._MatchServiceError
    defaultService
    "CommitRequiredException"

-- | A file cannot be added to the repository because the specified file name
-- has the same name as a directory in this repository. Either provide
-- another name for the file, or add the file in a directory that does not
-- match the file name.
_FileNameConflictsWithDirectoryNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileNameConflictsWithDirectoryNameException =
  Prelude._MatchServiceError
    defaultService
    "FileNameConflictsWithDirectoryNameException"

-- | The specified reference does not exist. You must provide a full commit
-- ID.
_ReferenceDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReferenceDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ReferenceDoesNotExistException"

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionKeyNotFoundException =
  Prelude._MatchServiceError
    defaultService
    "EncryptionKeyNotFoundException"

-- | An array of target objects is required. It cannot be empty or null.
_TargetsRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TargetsRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TargetsRequiredException"

-- | The comment is empty. You must provide some content for a comment. The
-- content cannot be null.
_CommentContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "CommentContentRequiredException"

-- | The specified tag is not valid. Key names cannot be prefixed with aws:.
_InvalidSystemTagUsageException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSystemTagUsageException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSystemTagUsageException"

-- | The commit cannot be created because no files have been specified as
-- added, updated, or changed (PutFile or DeleteFile) for the commit.
_FileEntryRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileEntryRequiredException =
  Prelude._MatchServiceError
    defaultService
    "FileEntryRequiredException"

-- | The targets for the pull request is not valid or not in a valid format.
-- Targets are a list of target objects. Each target object must contain
-- the full values for the repository name, source branch, and destination
-- branch for a pull request.
_InvalidTargetsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetsException"

-- | The pull request cannot be merged automatically into the destination
-- branch. You must manually merge the branches and resolve any conflicts.
_ManualMergeRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ManualMergeRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ManualMergeRequiredException"

-- | Cannot create the branch with the specified name because the commit
-- conflicts with an existing branch with the same name. Branch names must
-- be unique.
_BranchNameExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BranchNameExistsException =
  Prelude._MatchServiceError
    defaultService
    "BranchNameExistsException"

-- | The parent commit ID is not valid because it does not exist. The
-- specified parent commit ID does not exist in the specified branch of the
-- repository.
_ParentCommitDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParentCommitDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ParentCommitDoesNotExistException"

-- | The commit cannot be created because one or more changes in this commit
-- duplicate actions in the same file path. For example, you cannot make
-- the same delete request to the same file in the same file path twice, or
-- make a delete request and a move request to the same file as part of the
-- same commit.
_SamePathRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SamePathRequestException =
  Prelude._MatchServiceError
    defaultService
    "SamePathRequestException"

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS
-- account.
_AuthorDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_AuthorDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "AuthorDoesNotExistException"

-- | The maximum number of tags for an AWS CodeCommit resource has been
-- exceeded.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The source branch and destination branch for the pull request are the
-- same. You must specify different branches for the source and
-- destination.
_SourceAndDestinationAreSameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SourceAndDestinationAreSameException =
  Prelude._MatchServiceError
    defaultService
    "SourceAndDestinationAreSameException"

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionIntegrityChecksFailedException =
  Prelude._MatchServiceError
    defaultService
    "EncryptionIntegrityChecksFailedException"

-- | The description for the approval rule template is not valid because it
-- exceeds the maximum characters allowed for a description. For more
-- information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateDescriptionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalRuleTemplateDescriptionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateDescriptionException"

-- | This comment has already been deleted. You cannot edit or delete a
-- deleted comment.
_CommentDeletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentDeletedException =
  Prelude._MatchServiceError
    defaultService
    "CommentDeletedException"

-- | Automerge was specified for resolving the conflict, but the replacement
-- type is not valid or content is missing.
_InvalidReplacementContentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReplacementContentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidReplacementContentException"

-- | At least one event for the trigger is required, but was not specified.
_RepositoryTriggerEventsListRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryTriggerEventsListRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryTriggerEventsListRequiredException"

-- | The file cannot be added because it is empty. Empty files cannot be
-- added to the repository with this API.
_FileContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "FileContentRequiredException"

-- | The commit cannot be created because no source files or file content
-- have been specified for the commit.
_SourceFileOrContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SourceFileOrContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "SourceFileOrContentRequiredException"

-- | The title of the pull request is not valid. Pull request titles cannot
-- exceed 100 characters in length.
_InvalidTitleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTitleException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTitleException"

-- | The content for the approval rule is not valid.
_InvalidApprovalRuleContentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalRuleContentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalRuleContentException"

-- | The source commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidSourceCommitSpecifierException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSourceCommitSpecifierException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSourceCommitSpecifierException"

-- | The specified conflict resolution list is not valid.
_InvalidConflictResolutionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConflictResolutionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConflictResolutionException"

-- | The override status is not valid. Valid statuses are OVERRIDE and
-- REVOKE.
_InvalidOverrideStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOverrideStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOverrideStatusException"

-- | A pull request status is required, but none was provided.
_PullRequestStatusRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestStatusRequiredException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestStatusRequiredException"

-- | The number of items to compare between the source or destination
-- branches and the merge base has exceeded the maximum allowed.
_MaximumItemsToCompareExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumItemsToCompareExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumItemsToCompareExceededException"

-- | The pull request status is not valid. The only valid values are @OPEN@
-- and @CLOSED@.
_InvalidPullRequestStatusException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPullRequestStatusException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPullRequestStatusException"

-- | A parent commit ID is required. To view the full commit ID of a branch
-- in a repository, use GetBranch or a Git command (for example, git pull
-- or git log).
_ParentCommitIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParentCommitIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ParentCommitIdRequiredException"

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryDoesNotExistException"

-- | The pull request cannot be merged because one or more approval rules
-- applied to the pull request have conditions that have not been met.
_PullRequestApprovalRulesNotSatisfiedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestApprovalRulesNotSatisfiedException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestApprovalRulesNotSatisfiedException"

-- | The content for the approval rule is empty. You must provide some
-- content for an approval rule. The content cannot be null.
_ApprovalRuleContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleContentRequiredException"

-- | A pull request title is required. It cannot be empty or null.
_TitleRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TitleRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TitleRequiredException"

-- | The file cannot be added because it is too large. The maximum file size
-- is 6 MB, and the combined file content change size is 7 MB. Consider
-- making these changes using a Git client.
_FileContentSizeLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileContentSizeLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "FileContentSizeLimitExceededException"

-- | The name of the trigger is not valid.
_InvalidRepositoryTriggerNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerNameException"

-- | A name for the trigger is required, but was not specified.
_RepositoryTriggerNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryTriggerNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryTriggerNameRequiredException"

-- | The maximum number of approval rule templates has been exceeded for this
-- AWS Region.
_NumberOfRuleTemplatesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberOfRuleTemplatesExceededException =
  Prelude._MatchServiceError
    defaultService
    "NumberOfRuleTemplatesExceededException"

-- | A repository name is required, but was not specified.
_RepositoryNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNameRequiredException"

-- | The specified file mode permission is not valid. For a list of valid
-- file mode permissions, see PutFile.
_InvalidFileModeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFileModeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFileModeException"

-- | One or more events specified for the trigger is not valid. Check to make
-- sure that all events specified match the requirements for allowed
-- events.
_InvalidRepositoryTriggerEventsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerEventsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerEventsException"

-- | The specified path is not valid.
_InvalidPathException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPathException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPathException"

-- | The specified target branch is not valid.
_InvalidTargetBranchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetBranchException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetBranchException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the user who initiated the change for the pull
-- request, and then try again.
_InvalidActorArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidActorArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidActorArnException"

-- | A revision ID is required, but was not provided.
_RevisionIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RevisionIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RevisionIdRequiredException"

-- | The specified blob is not valid.
_InvalidBlobIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidBlobIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidBlobIdException"

-- | The number of specified files to change as part of this commit exceeds
-- the maximum number of files that can be changed in a single commit.
-- Consider using a Git client for these changes.
_MaximumFileEntriesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumFileEntriesExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumFileEntriesExceededException"

-- | An approval rule template name is required, but was not specified.
_ApprovalRuleTemplateNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleTemplateNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameRequiredException"

-- | At least one repository name object is required, but was not specified.
_RepositoryNamesRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNamesRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNamesRequiredException"

-- | The pull request ID could not be found. Make sure that you have
-- specified the correct repository name and pull request ID, and then try
-- again.
_PullRequestDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestDoesNotExistException"

-- | A replacement type is required.
_ReplacementTypeRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReplacementTypeRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ReplacementTypeRequiredException"

-- | Automerge was specified for resolving the conflict, but the specified
-- replacement type is not valid.
_InvalidReplacementTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReplacementTypeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidReplacementTypeException"

-- | The specified file exceeds the file size limit for AWS CodeCommit. For
-- more information about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_FileTooLargeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileTooLargeException =
  Prelude._MatchServiceError
    defaultService
    "FileTooLargeException"

-- | The approval rule cannot be deleted from the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotDeleteApprovalRuleFromTemplateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotDeleteApprovalRuleFromTemplateException =
  Prelude._MatchServiceError
    defaultService
    "CannotDeleteApprovalRuleFromTemplateException"

-- | The number of reactions has been exceeded. Reactions are limited to one
-- reaction per user for each individual comment ID.
_ReactionLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReactionLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "ReactionLimitExceededException"

-- | The revision ID provided in the request does not match the current
-- revision ID. Use GetPullRequest to retrieve the current revision ID.
_RevisionNotCurrentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RevisionNotCurrentException =
  Prelude._MatchServiceError
    defaultService
    "RevisionNotCurrentException"

-- | The name for the approval rule is not valid.
_InvalidApprovalRuleNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalRuleNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalRuleNameException"

-- | The specified commit ID does not exist.
_CommitIdDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitIdDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "CommitIdDoesNotExistException"

-- | The list of triggers for the repository is required, but was not
-- specified.
_RepositoryTriggersListRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryTriggersListRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryTriggersListRequiredException"

-- | Either the enum is not in a valid format, or the specified file version
-- enum is not valid in respect to the current file version.
_InvalidRelativeFileVersionEnumException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRelativeFileVersionEnumException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRelativeFileVersionEnumException"

-- | The tag policy is not valid.
_TagPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagPolicyException =
  Prelude._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The specified approval rule does not exist.
_ApprovalRuleDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleDoesNotExistException"

-- | At least one branch name is required, but was not specified in the
-- trigger configuration.
_RepositoryTriggerBranchNameListRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryTriggerBranchNameListRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryTriggerBranchNameListRequiredException"

-- | The client request token is not valid.
_InvalidClientRequestTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidClientRequestTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidClientRequestTokenException"

-- | The client request token is not valid. Either the token is not in a
-- valid format, or the token has been used in a previous request and
-- cannot be reused.
_IdempotencyParameterMismatchException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IdempotencyParameterMismatchException =
  Prelude._MatchServiceError
    defaultService
    "IdempotencyParameterMismatchException"

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the
-- specified destination. The most common reason for this error is that the
-- ARN does not meet the requirements for the service type.
_InvalidRepositoryTriggerDestinationArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerDestinationArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerDestinationArnException"

-- | A branch name is required, but was not specified.
_BranchNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BranchNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "BranchNameRequiredException"

-- | A destination ARN for the target service for the trigger is required,
-- but was not specified.
_RepositoryTriggerDestinationArnRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryTriggerDestinationArnRequiredException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryTriggerDestinationArnRequiredException"

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionKeyUnavailableException =
  Prelude._MatchServiceError
    defaultService
    "EncryptionKeyUnavailableException"

-- | The specified conflict resolution strategy is not valid.
_InvalidConflictResolutionStrategyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidConflictResolutionStrategyException =
  Prelude._MatchServiceError
    defaultService
    "InvalidConflictResolutionStrategyException"

-- | More than one conflict resolution entries exists for the conflict. A
-- conflict can have only one conflict resolution entry.
_MultipleConflictResolutionEntriesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MultipleConflictResolutionEntriesException =
  Prelude._MatchServiceError
    defaultService
    "MultipleConflictResolutionEntriesException"

-- | The specified file does not exist. Verify that you have used the correct
-- file name, full path, and extension.
_FileDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "FileDoesNotExistException"

-- | A list of tag keys is required. The list cannot be empty or null.
_TagKeysListRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagKeysListRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TagKeysListRequiredException"

-- | The specified reference name format is not valid. Reference names must
-- conform to the Git references format (for example, refs\/heads\/master).
-- For more information, see
-- <https://git-scm.com/book/en/v2/Git-Internals-Git-References Git Internals - Git References>
-- or consult your Git documentation.
_InvalidReferenceNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReferenceNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidReferenceNameException"

-- | A commit ID was not specified.
_CommitIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "CommitIdRequiredException"

-- | A reference name is required, but none was provided.
_ReferenceNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReferenceNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ReferenceNameRequiredException"

-- | The maximum number of approval rule templates for a repository has been
-- exceeded. You cannot associate more than 25 approval rule templates with
-- a repository.
_MaximumRuleTemplatesAssociatedWithRepositoryException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumRuleTemplatesAssociatedWithRepositoryException =
  Prelude._MatchServiceError
    defaultService
    "MaximumRuleTemplatesAssociatedWithRepositoryException"

-- | The commit message is too long. Provide a shorter string.
_CommitMessageLengthExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitMessageLengthExceededException =
  Prelude._MatchServiceError
    defaultService
    "CommitMessageLengthExceededException"

-- | The number of approvals required for the approval rule exceeds the
-- maximum number allowed.
_MaximumNumberOfApprovalsExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumNumberOfApprovalsExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumNumberOfApprovalsExceededException"

-- | The list of tags is not valid.
_InvalidTagKeysListException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagKeysListException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagKeysListException"

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionKeyDisabledException =
  Prelude._MatchServiceError
    defaultService
    "EncryptionKeyDisabledException"

-- | A file cannot be added to the repository because the specified path name
-- has the same name as a file that already exists in this repository.
-- Either provide a different name for the file, or specify a different
-- path for the file.
_DirectoryNameConflictsWithFileNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DirectoryNameConflictsWithFileNameException =
  Prelude._MatchServiceError
    defaultService
    "DirectoryNameConflictsWithFileNameException"

-- | The specified sort by value is not valid.
_InvalidSortByException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidSortByException =
  Prelude._MatchServiceError
    defaultService
    "InvalidSortByException"

-- | The destination commit specifier is not valid. You must provide a valid
-- branch name, tag, or full commit ID.
_InvalidDestinationCommitSpecifierException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDestinationCommitSpecifierException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDestinationCommitSpecifierException"

-- | No comment exists with the provided ID. Verify that you have used the
-- correct ID, and then try again.
_CommentDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "CommentDoesNotExistException"

-- | You cannot create an approval rule template with that name because a
-- template with that name already exists in this AWS Region for your AWS
-- account. Approval rule template names must be unique.
_ApprovalRuleTemplateNameAlreadyExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleTemplateNameAlreadyExistsException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleTemplateNameAlreadyExistsException"

-- | One or more branch names specified for the trigger is not valid.
_InvalidRepositoryTriggerBranchNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerBranchNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerBranchNameException"

-- | You cannot create the pull request because the repository has too many
-- open pull requests. The maximum number of open pull requests for a
-- repository is 1,000. Close one or more open pull requests, and then try
-- again.
_MaximumOpenPullRequestsExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumOpenPullRequestsExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumOpenPullRequestsExceededException"

-- | The pull request status cannot be updated because it is already closed.
_PullRequestAlreadyClosedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestAlreadyClosedException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestAlreadyClosedException"

-- | The specified commit is not valid.
_InvalidCommitException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCommitException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCommitException"

-- | The specified reference is not a supported type.
_ReferenceTypeNotSupportedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReferenceTypeNotSupportedException =
  Prelude._MatchServiceError
    defaultService
    "ReferenceTypeNotSupportedException"

-- | You cannot modify or delete this comment. Only comment authors can
-- modify or delete their comments.
_CommentNotCreatedByCallerException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentNotCreatedByCallerException =
  Prelude._MatchServiceError
    defaultService
    "CommentNotCreatedByCallerException"

-- | The target for the pull request is not valid. A target must contain the
-- full values for the repository name, source branch, and destination
-- branch for the pull request.
_InvalidTargetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTargetException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTargetException"

-- | The custom data provided for the trigger is not valid.
_InvalidRepositoryTriggerCustomDataException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerCustomDataException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerCustomDataException"

-- | The pull request has already had its approval rules set to override.
_OverrideAlreadySetException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OverrideAlreadySetException =
  Prelude._MatchServiceError
    defaultService
    "OverrideAlreadySetException"

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidContinuationTokenException =
  Prelude._MatchServiceError
    defaultService
    "InvalidContinuationTokenException"

-- | The AWS Region for the trigger target does not match the AWS Region for
-- the repository. Triggers must be created in the same Region as the
-- target for the trigger.
_InvalidRepositoryTriggerRegionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryTriggerRegionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryTriggerRegionException"

-- | The value of the reaction is not valid. For more information, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.
_InvalidReactionValueException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReactionValueException =
  Prelude._MatchServiceError
    defaultService
    "InvalidReactionValueException"

-- | The divergence between the tips of the provided commit specifiers is too
-- great to determine whether there might be any merge conflicts. Locally
-- compare the specifiers using @git diff@ or a diff tool.
_TipsDivergenceExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TipsDivergenceExceededException =
  Prelude._MatchServiceError
    defaultService
    "TipsDivergenceExceededException"

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_EncryptionKeyAccessDeniedException =
  Prelude._MatchServiceError
    defaultService
    "EncryptionKeyAccessDeniedException"

-- | A reaction value is required.
_ReactionValueRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReactionValueRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ReactionValueRequiredException"

-- | A valid Amazon Resource Name (ARN) for an AWS CodeCommit resource is
-- required. For a list of valid resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_ResourceArnRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ResourceArnRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ResourceArnRequiredException"

-- | The specified path does not exist.
_PathDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PathDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "PathDoesNotExistException"

-- | USE_NEW_CONTENT was specified, but no replacement content has been
-- provided.
_ReplacementContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ReplacementContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ReplacementContentRequiredException"

-- | The value for the resource ARN is not valid. For more information about
-- resources in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations>
-- in the AWS CodeCommit User Guide.
_InvalidResourceArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidResourceArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidResourceArnException"

-- | The file could not be added because the provided parent commit ID is not
-- the current tip of the specified branch. To view the full commit ID of
-- the current head of the branch, use GetBranch.
_ParentCommitIdOutdatedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ParentCommitIdOutdatedException =
  Prelude._MatchServiceError
    defaultService
    "ParentCommitIdOutdatedException"

-- | The specified value for the number of merge hunks to return is not
-- valid.
_InvalidMaxMergeHunksException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMaxMergeHunksException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMaxMergeHunksException"

-- | The commit cannot be created because one of the changes specifies
-- copying or moving a .gitkeep file.
_RestrictedSourceFileException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RestrictedSourceFileException =
  Prelude._MatchServiceError
    defaultService
    "RestrictedSourceFileException"

-- | The position is not valid. Make sure that the line number exists in the
-- version of the file you want to comment on.
_InvalidFilePositionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFilePositionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFilePositionException"

-- | The approval rule cannot be modified for the pull request because it was
-- created by an approval rule template and applied to the pull request
-- automatically.
_CannotModifyApprovalRuleFromTemplateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotModifyApprovalRuleFromTemplateException =
  Prelude._MatchServiceError
    defaultService
    "CannotModifyApprovalRuleFromTemplateException"

-- | The specified value for the number of conflict files to return is not
-- valid.
_InvalidMaxConflictFilesException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMaxConflictFilesException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMaxConflictFilesException"

-- | The comment is too large. Comments are limited to 1,000 characters.
_CommentContentSizeLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentContentSizeLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "CommentContentSizeLimitExceededException"

-- | The state for the approval is not valid. Valid values include APPROVE
-- and REVOKE.
_InvalidApprovalStateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalStateException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalStateException"

-- | The number of branches for the trigger was exceeded.
_MaximumBranchesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumBranchesExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumBranchesExceededException"

-- | An override status is required, but no value was provided. Valid values
-- include OVERRIDE and REVOKE.
_OverrideStatusRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_OverrideStatusRequiredException =
  Prelude._MatchServiceError
    defaultService
    "OverrideStatusRequiredException"

-- | The pull request event type is not valid.
_InvalidPullRequestEventTypeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidPullRequestEventTypeException =
  Prelude._MatchServiceError
    defaultService
    "InvalidPullRequestEventTypeException"

-- | The content of the approval rule template is not valid.
_InvalidApprovalRuleTemplateContentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalRuleTemplateContentException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateContentException"

-- | The maximum number of allowed commit IDs in a batch request is 100.
-- Verify that your batch requests contains no more than 100 commit IDs,
-- and then try again.
_CommitIdsLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitIdsLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "CommitIdsLimitExceededException"

-- | An approval state is required, but was not specified.
_ApprovalStateRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalStateRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalStateRequiredException"

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have
-- provided the full ARN for the author of the pull request, and then try
-- again.
_InvalidAuthorArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidAuthorArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidAuthorArnException"

-- | The number of allowed conflict resolution entries was exceeded.
_MaximumConflictResolutionEntriesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumConflictResolutionEntriesExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumConflictResolutionEntriesExceededException"

-- | The parent commit ID is not valid. The commit ID cannot be empty, and
-- must match the head commit ID for the branch of the repository where you
-- want to add or update a file.
_InvalidParentCommitIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidParentCommitIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidParentCommitIdException"

-- | The repository does not contain any pull requests with that pull request
-- ID. Use GetPullRequest to verify the correct repository name for the
-- pull request ID.
_RepositoryNotAssociatedWithPullRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNotAssociatedWithPullRequestException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNotAssociatedWithPullRequestException"

-- | The content for the approval rule template is empty. You must provide
-- some content for an approval rule template. The content cannot be null.
_ApprovalRuleTemplateContentRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleTemplateContentRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleTemplateContentRequiredException"

-- | The merge cannot be completed because the target branch has been
-- modified. Another user might have modified the target branch while the
-- merge was in progress. Wait a few minutes, and then try again.
_ConcurrentReferenceUpdateException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ConcurrentReferenceUpdateException =
  Prelude._MatchServiceError
    defaultService
    "ConcurrentReferenceUpdateException"

-- | The number of files to load exceeds the allowed limit.
_MaximumFileContentToLoadExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumFileContentToLoadExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumFileContentToLoadExceededException"

-- | A map of tags is required.
_TagsMapRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagsMapRequiredException =
  Prelude._MatchServiceError
    defaultService
    "TagsMapRequiredException"

-- | A specified repository name is not valid.
--
-- This exception occurs only when a specified repository name is not
-- valid. Other exceptions occur when a required repository parameter is
-- missing, or when a specified repository does not exist.
_InvalidRepositoryNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryNameException"

-- | The commit cannot be created because at least one of the overall changes
-- in the commit results in a folder whose contents exceed the limit of 6
-- MB. Either reduce the number and size of your changes, or split the
-- changes across multiple folders.
_FolderContentSizeLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FolderContentSizeLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "FolderContentSizeLimitExceededException"

-- | A list of commit IDs is required, but was either not specified or the
-- list was empty.
_CommitIdsListRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitIdsListRequiredException =
  Prelude._MatchServiceError
    defaultService
    "CommitIdsListRequiredException"

-- | The commit cannot be created because no file mode has been specified. A
-- file mode is required to update mode permissions for a file.
_FileModeRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileModeRequiredException =
  Prelude._MatchServiceError
    defaultService
    "FileModeRequiredException"

-- | The map of tags is not valid.
_InvalidTagsMapException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidTagsMapException =
  Prelude._MatchServiceError
    defaultService
    "InvalidTagsMapException"

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryLimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryLimitExceededException"

-- | The approval rule cannot be added. The pull request has the maximum
-- number of approval rules associated with it.
_NumberOfRulesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NumberOfRulesExceededException =
  Prelude._MatchServiceError
    defaultService
    "NumberOfRulesExceededException"

-- | The specified deletion parameter is not valid.
_InvalidDeletionParameterException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDeletionParameterException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDeletionParameterException"

-- | The Amazon Resource Name (ARN) of the user or identity is not valid.
_InvalidReactionUserArnException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidReactionUserArnException =
  Prelude._MatchServiceError
    defaultService
    "InvalidReactionUserArnException"

-- | The specified branch is the default branch for the repository, and
-- cannot be deleted. To delete this branch, you must first set another
-- branch as the default branch.
_DefaultBranchCannotBeDeletedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_DefaultBranchCannotBeDeletedException =
  Prelude._MatchServiceError
    defaultService
    "DefaultBranchCannotBeDeletedException"

-- | The specified branch name is not valid because it is a tag name. Enter
-- the name of a branch in the repository. For a list of valid branch
-- names, use ListBranches.
_BranchNameIsTagNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BranchNameIsTagNameException =
  Prelude._MatchServiceError
    defaultService
    "BranchNameIsTagNameException"

-- | The folderPath for a location cannot be null.
_PathRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PathRequiredException =
  Prelude._MatchServiceError
    defaultService
    "PathRequiredException"

-- | The commit cannot be created because a specified file path points to a
-- submodule. Verify that the destination files have valid file paths that
-- do not point to a submodule.
_FilePathConflictsWithSubmodulePathException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FilePathConflictsWithSubmodulePathException =
  Prelude._MatchServiceError
    defaultService
    "FilePathConflictsWithSubmodulePathException"

-- | The specified branch does not exist.
_BranchDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BranchDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "BranchDoesNotExistException"

-- | The comment ID is not in a valid format. Make sure that you have
-- provided the full comment ID.
_InvalidCommentIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCommentIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCommentIdException"

-- | The specified number of maximum results is not valid.
_InvalidMaxResultsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMaxResultsException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMaxResultsException"

-- | The revision ID is not valid. Use GetPullRequest to determine the value.
_InvalidRevisionIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRevisionIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRevisionIdException"

-- | The approval cannot be applied because the user approving the pull
-- request matches the user who created the pull request. You cannot
-- approve a pull request that you created.
_PullRequestCannotBeApprovedByAuthorException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PullRequestCannotBeApprovedByAuthorException =
  Prelude._MatchServiceError
    defaultService
    "PullRequestCannotBeApprovedByAuthorException"

-- | An approval rule name is required, but was not specified.
_ApprovalRuleNameRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleNameRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleNameRequiredException"

-- | A blob ID is required, but was not specified.
_BlobIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BlobIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "BlobIdRequiredException"

-- | The name of the approval rule template is not valid. Template names must
-- be between 1 and 100 valid characters in length. For more information
-- about limits in AWS CodeCommit, see
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide>.
_InvalidApprovalRuleTemplateNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidApprovalRuleTemplateNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidApprovalRuleTemplateNameException"

-- | The pull request description is not valid. Descriptions cannot be more
-- than 1,000 characters.
_InvalidDescriptionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidDescriptionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidDescriptionException"

-- | The commit cannot be created because no changes will be made to the
-- repository as a result of this commit. A commit must contain at least
-- one change.
_NoChangeException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NoChangeException =
  Prelude._MatchServiceError
    defaultService
    "NoChangeException"

-- | The comment ID is missing or null. A comment ID is required.
_CommentIdRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommentIdRequiredException =
  Prelude._MatchServiceError
    defaultService
    "CommentIdRequiredException"

-- | You cannot include more than one repository in a pull request. Make sure
-- you have specified only one repository name in your request, and then
-- try again.
_MultipleRepositoriesInPullRequestException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MultipleRepositoriesInPullRequestException =
  Prelude._MatchServiceError
    defaultService
    "MultipleRepositoriesInPullRequestException"

-- | The specified sort order is not valid.
_InvalidOrderException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOrderException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOrderException"

-- | A client request token is required. A client request token is an unique,
-- client-generated idempotency token that, when provided in a request,
-- ensures the request cannot be repeated with a changed parameter. If a
-- request is received with the same parameters and a token is included,
-- the request returns information about the initial request that used that
-- token.
_ClientRequestTokenRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ClientRequestTokenRequiredException =
  Prelude._MatchServiceError
    defaultService
    "ClientRequestTokenRequiredException"

-- | The specified merge option is not valid for this operation. Not all
-- merge strategies are supported for all operations.
_InvalidMergeOptionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidMergeOptionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidMergeOptionException"

-- | A merge option or stategy is required, and none was provided.
_MergeOptionRequiredException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MergeOptionRequiredException =
  Prelude._MatchServiceError
    defaultService
    "MergeOptionRequiredException"

-- | The specified reference name is not valid.
_InvalidBranchNameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidBranchNameException =
  Prelude._MatchServiceError
    defaultService
    "InvalidBranchNameException"

-- | The location of the file is not valid. Make sure that you include the
-- file name and extension.
_InvalidFileLocationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidFileLocationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidFileLocationException"

-- | The specified approval rule template does not exist. Verify that the
-- name is correct and that you are signed in to the AWS Region where the
-- template was created, and then try again.
_ApprovalRuleTemplateDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleTemplateDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleTemplateDoesNotExistException"

-- | The number of triggers allowed for the repository was exceeded.
_MaximumRepositoryTriggersExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumRepositoryTriggersExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumRepositoryTriggersExceededException"

-- | The specified commit does not exist or no commit was specified, and the
-- specified repository has no default branch.
_CommitDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CommitDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "CommitDoesNotExistException"

-- | The before commit ID and the after commit ID are the same, which is not
-- valid. The before commit ID and the after commit ID must be different
-- commit IDs.
_BeforeCommitIdAndAfterCommitIdAreSameException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BeforeCommitIdAndAfterCommitIdAreSameException =
  Prelude._MatchServiceError
    defaultService
    "BeforeCommitIdAndAfterCommitIdAreSameException"

-- | The specified repository name already exists.
_RepositoryNameExistsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_RepositoryNameExistsException =
  Prelude._MatchServiceError
    defaultService
    "RepositoryNameExistsException"

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidCommitIdException =
  Prelude._MatchServiceError
    defaultService
    "InvalidCommitIdException"

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRepositoryDescriptionException =
  Prelude._MatchServiceError
    defaultService
    "InvalidRepositoryDescriptionException"

-- | The maximum number of allowed repository names was exceeded. Currently,
-- this number is 100.
_MaximumRepositoryNamesExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_MaximumRepositoryNamesExceededException =
  Prelude._MatchServiceError
    defaultService
    "MaximumRepositoryNamesExceededException"

-- | The file was not added or updated because the content of the file is
-- exactly the same as the content of that file in the repository and
-- branch that you specified.
_SameFileContentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_SameFileContentException =
  Prelude._MatchServiceError
    defaultService
    "SameFileContentException"

-- | The approval rule template is associated with one or more repositories.
-- You cannot delete a template that is associated with a repository.
-- Remove all associations, and then try again.
_ApprovalRuleTemplateInUseException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ApprovalRuleTemplateInUseException =
  Prelude._MatchServiceError
    defaultService
    "ApprovalRuleTemplateInUseException"

-- | The specified email address either contains one or more characters that
-- are not allowed, or it exceeds the maximum number of characters allowed
-- for an email address.
_InvalidEmailException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidEmailException =
  Prelude._MatchServiceError
    defaultService
    "InvalidEmailException"

-- | The tip of the source branch in the destination repository does not
-- match the tip of the source branch specified in your request. The pull
-- request might have been updated. Make sure that you have the latest
-- changes.
_TipOfSourceReferenceIsDifferentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TipOfSourceReferenceIsDifferentException =
  Prelude._MatchServiceError
    defaultService
    "TipOfSourceReferenceIsDifferentException"

-- | The specified folder does not exist. Either the folder name is not
-- correct, or you did not enter the full path to the folder.
_FolderDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FolderDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "FolderDoesNotExistException"

-- | The SHA-256 hash signature for the rule content is not valid.
_InvalidRuleContentSha256Exception :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidRuleContentSha256Exception =
  Prelude._MatchServiceError
    defaultService
    "InvalidRuleContentSha256Exception"

-- | The specified blob does not exist.
_BlobIdDoesNotExistException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_BlobIdDoesNotExistException =
  Prelude._MatchServiceError
    defaultService
    "BlobIdDoesNotExistException"

-- | The commit cannot be created because one or more files specified in the
-- commit reference both a file and a folder.
_PutFileEntryConflictException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_PutFileEntryConflictException =
  Prelude._MatchServiceError
    defaultService
    "PutFileEntryConflictException"

-- | The commit cannot be created because both a source file and file content
-- have been specified for the same file. You cannot provide both. Either
-- specify a source file or provide the file content directly.
_FileContentAndSourceFileSpecifiedException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_FileContentAndSourceFileSpecifiedException =
  Prelude._MatchServiceError
    defaultService
    "FileContentAndSourceFileSpecifiedException"
