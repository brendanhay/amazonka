-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types
  ( -- * Service configuration
    codeCommitService,

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
    Approval (..),
    mkApproval,
    aApprovalState,
    aUserARN,

    -- * ApprovalRule
    ApprovalRule (..),
    mkApprovalRule,
    arRuleContentSha256,
    arLastModifiedDate,
    arApprovalRuleName,
    arApprovalRuleId,
    arLastModifiedUser,
    arOriginApprovalRuleTemplate,
    arCreationDate,
    arApprovalRuleContent,

    -- * ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata (..),
    mkApprovalRuleEventMetadata,
    aremApprovalRuleName,
    aremApprovalRuleId,
    aremApprovalRuleContent,

    -- * ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata (..),
    mkApprovalRuleOverriddenEventMetadata,
    aroemOverrideStatus,
    aroemRevisionId,

    -- * ApprovalRuleTemplate
    ApprovalRuleTemplate (..),
    mkApprovalRuleTemplate,
    artRuleContentSha256,
    artApprovalRuleTemplateId,
    artLastModifiedDate,
    artApprovalRuleTemplateDescription,
    artApprovalRuleTemplateContent,
    artLastModifiedUser,
    artCreationDate,
    artApprovalRuleTemplateName,

    -- * ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata (..),
    mkApprovalStateChangedEventMetadata,
    ascemApprovalStatus,
    ascemRevisionId,

    -- * BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositoriesError,
    baartwreErrorCode,
    baartwreRepositoryName,
    baartwreErrorMessage,

    -- * BatchDescribeMergeConflictsError
    BatchDescribeMergeConflictsError (..),
    mkBatchDescribeMergeConflictsError,
    bdmceFilePath,
    bdmceExceptionName,
    bdmceMessage,

    -- * BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError,
    bdartfreErrorCode,
    bdartfreRepositoryName,
    bdartfreErrorMessage,

    -- * BatchGetCommitsError
    BatchGetCommitsError (..),
    mkBatchGetCommitsError,
    bgceCommitId,
    bgceErrorCode,
    bgceErrorMessage,

    -- * BlobMetadata
    BlobMetadata (..),
    mkBlobMetadata,
    bmPath,
    bmMode,
    bmBlobId,

    -- * BranchInfo
    BranchInfo (..),
    mkBranchInfo,
    biCommitId,
    biBranchName,

    -- * Comment
    Comment (..),
    mkComment,
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
    CommentsForComparedCommit (..),
    mkCommentsForComparedCommit,
    cfccBeforeBlobId,
    cfccLocation,
    cfccAfterCommitId,
    cfccAfterBlobId,
    cfccBeforeCommitId,
    cfccRepositoryName,
    cfccComments,

    -- * CommentsForPullRequest
    CommentsForPullRequest (..),
    mkCommentsForPullRequest,
    cfprBeforeBlobId,
    cfprLocation,
    cfprAfterCommitId,
    cfprPullRequestId,
    cfprAfterBlobId,
    cfprBeforeCommitId,
    cfprRepositoryName,
    cfprComments,

    -- * Commit
    Commit (..),
    mkCommit,
    cCommitId,
    cCommitter,
    cTreeId,
    cAdditionalData,
    cParents,
    cAuthor,
    cMessage,

    -- * Conflict
    Conflict (..),
    mkConflict,
    cMergeHunks,
    cConflictMetadata,

    -- * ConflictMetadata
    ConflictMetadata (..),
    mkConflictMetadata,
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
    ConflictResolution (..),
    mkConflictResolution,
    crSetFileModes,
    crDeleteFiles,
    crReplaceContents,

    -- * DeleteFileEntry
    DeleteFileEntry (..),
    mkDeleteFileEntry,
    dfeFilePath,

    -- * Difference
    Difference (..),
    mkDifference,
    dAfterBlob,
    dBeforeBlob,
    dChangeType,

    -- * Evaluation
    Evaluation (..),
    mkEvaluation,
    eApprovalRulesSatisfied,
    eApprovalRulesNotSatisfied,
    eApproved,
    eOverridden,

    -- * File
    File (..),
    mkFile,
    fAbsolutePath,
    fFileMode,
    fBlobId,
    fRelativePath,

    -- * FileMetadata
    FileMetadata (..),
    mkFileMetadata,
    fmAbsolutePath,
    fmFileMode,
    fmBlobId,

    -- * FileModes
    FileModes (..),
    mkFileModes,
    fmDestination,
    fmBase,
    fmSource,

    -- * FileSizes
    FileSizes (..),
    mkFileSizes,
    fsDestination,
    fsBase,
    fsSource,

    -- * Folder
    Folder (..),
    mkFolder,
    ffAbsolutePath,
    ffTreeId,
    ffRelativePath,

    -- * IsBinaryFile
    IsBinaryFile (..),
    mkIsBinaryFile,
    ibfDestination,
    ibfBase,
    ibfSource,

    -- * Location
    Location (..),
    mkLocation,
    lRelativeFileVersion,
    lFilePath,
    lFilePosition,

    -- * MergeHunk
    MergeHunk (..),
    mkMergeHunk,
    mhDestination,
    mhBase,
    mhIsConflict,
    mhSource,

    -- * MergeHunkDetail
    MergeHunkDetail (..),
    mkMergeHunkDetail,
    mhdStartLine,
    mhdEndLine,
    mhdHunkContent,

    -- * MergeMetadata
    MergeMetadata (..),
    mkMergeMetadata,
    mmMergedBy,
    mmMergeOption,
    mmIsMerged,
    mmMergeCommitId,

    -- * MergeOperations
    MergeOperations (..),
    mkMergeOperations,
    moDestination,
    moSource,

    -- * ObjectTypes
    ObjectTypes (..),
    mkObjectTypes,
    otDestination,
    otBase,
    otSource,

    -- * OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate (..),
    mkOriginApprovalRuleTemplate,
    oartApprovalRuleTemplateId,
    oartApprovalRuleTemplateName,

    -- * PullRequest
    PullRequest (..),
    mkPullRequest,
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
    PullRequestCreatedEventMetadata (..),
    mkPullRequestCreatedEventMetadata,
    prcemDestinationCommitId,
    prcemMergeBase,
    prcemRepositoryName,
    prcemSourceCommitId,

    -- * PullRequestEvent
    PullRequestEvent (..),
    mkPullRequestEvent,
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
    PullRequestMergedStateChangedEventMetadata (..),
    mkPullRequestMergedStateChangedEventMetadata,
    prmscemDestinationReference,
    prmscemMergeMetadata,
    prmscemRepositoryName,

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata (..),
    mkPullRequestSourceReferenceUpdatedEventMetadata,
    prsruemAfterCommitId,
    prsruemBeforeCommitId,
    prsruemMergeBase,
    prsruemRepositoryName,

    -- * PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata (..),
    mkPullRequestStatusChangedEventMetadata,
    prscemPullRequestStatus,

    -- * PullRequestTarget
    PullRequestTarget (..),
    mkPullRequestTarget,
    prtSourceCommit,
    prtDestinationReference,
    prtMergeMetadata,
    prtMergeBase,
    prtDestinationCommit,
    prtRepositoryName,
    prtSourceReference,

    -- * PutFileEntry
    PutFileEntry (..),
    mkPutFileEntry,
    pfeFileContent,
    pfeFileMode,
    pfeFilePath,
    pfeSourceFile,

    -- * ReactionForComment
    ReactionForComment (..),
    mkReactionForComment,
    rfcReactionUsers,
    rfcReactionsFromDeletedUsersCount,
    rfcReaction,

    -- * ReactionValueFormats
    ReactionValueFormats (..),
    mkReactionValueFormats,
    rvfEmoji,
    rvfShortCode,
    rvfUnicode,

    -- * ReplaceContentEntry
    ReplaceContentEntry (..),
    mkReplaceContentEntry,
    rceFileMode,
    rceFilePath,
    rceReplacementType,
    rceContent,

    -- * RepositoryMetadata
    RepositoryMetadata (..),
    mkRepositoryMetadata,
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
    RepositoryNameIdPair (..),
    mkRepositoryNameIdPair,
    rnipRepositoryId,
    rnipRepositoryName,

    -- * RepositoryTrigger
    RepositoryTrigger (..),
    mkRepositoryTrigger,
    rtBranches,
    rtCustomData,
    rtDestinationARN,
    rtName,
    rtEvents,

    -- * RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure (..),
    mkRepositoryTriggerExecutionFailure,
    rtefFailureMessage,
    rtefTrigger,

    -- * SetFileModeEntry
    SetFileModeEntry (..),
    mkSetFileModeEntry,
    sfmeFileMode,
    sfmeFilePath,

    -- * SourceFileSpecifier
    SourceFileSpecifier (..),
    mkSourceFileSpecifier,
    sfsFilePath,
    sfsIsMove,

    -- * SubModule
    SubModule (..),
    mkSubModule,
    smCommitId,
    smAbsolutePath,
    smRelativePath,

    -- * SymbolicLink
    SymbolicLink (..),
    mkSymbolicLink,
    slAbsolutePath,
    slFileMode,
    slBlobId,
    slRelativePath,

    -- * Target
    Target (..),
    mkTarget,
    tDestinationReference,
    tRepositoryName,
    tSourceReference,

    -- * UserInfo
    UserInfo (..),
    mkUserInfo,
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
codeCommitService :: Lude.Service
codeCommitService =
  Lude.Service
    { Lude._svcAbbrev = "CodeCommit",
      Lude._svcSigner = Sign.v4,
      Lude._svcPrefix = "codecommit",
      Lude._svcVersion = "2015-04-13",
      Lude._svcEndpoint = Lude.defaultEndpoint codeCommitService,
      Lude._svcTimeout = Lude.Just 70,
      Lude._svcCheck = Lude.statusSuccess,
      Lude._svcError = Lude.parseJSONError "CodeCommit",
      Lude._svcRetry = retry
    }
  where
    retry =
      Lude.Exponential
        { Lude._retryBase = 5.0e-2,
          Lude._retryGrowth = 2,
          Lude._retryAttempts = 5,
          Lude._retryCheck = check
        }
    check e
      | Lens.has
          (Lude.hasCode "ThrottledException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttled_exception"
      | Lens.has (Lude.hasStatus 429) e = Lude.Just "too_many_requests"
      | Lens.has
          (Lude.hasCode "ThrottlingException" Lude.. Lude.hasStatus 400)
          e =
        Lude.Just "throttling_exception"
      | Lens.has (Lude.hasCode "Throttling" Lude.. Lude.hasStatus 400) e =
        Lude.Just "throttling"
      | Lens.has
          ( Lude.hasCode "ProvisionedThroughputExceededException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "throughput_exceeded"
      | Lens.has (Lude.hasStatus 504) e = Lude.Just "gateway_timeout"
      | Lens.has
          ( Lude.hasCode "RequestThrottledException"
              Lude.. Lude.hasStatus 400
          )
          e =
        Lude.Just "request_throttled_exception"
      | Lens.has (Lude.hasStatus 502) e = Lude.Just "bad_gateway"
      | Lens.has (Lude.hasStatus 503) e = Lude.Just "service_unavailable"
      | Lens.has (Lude.hasStatus 500) e =
        Lude.Just "general_server_error"
      | Lens.has (Lude.hasStatus 509) e = Lude.Just "limit_exceeded"
      | Lude.otherwise = Lude.Nothing
