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
    mkServiceConfig,

    -- * Errors
    _InvalidReactionValueException,
    _InvalidRepositoryTriggerRegionException,
    _InvalidContinuationTokenException,
    _ManualMergeRequiredException,
    _TargetsRequiredException,
    _InvalidSystemTagUsageException,
    _FileEntryRequiredException,
    _EncryptionKeyNotFoundException,
    _TipsDivergenceExceededException,
    _InvalidRepositoryTriggerBranchNameException,
    _PullRequestAlreadyClosedException,
    _InvalidRepositoryTriggerCustomDataException,
    _DirectoryNameConflictsWithFileNameException,
    _ReferenceDoesNotExistException,
    _ApprovalRuleNameAlreadyExistsException,
    _ActorDoesNotExistException,
    _PullRequestIdRequiredException,
    _OverrideAlreadySetException,
    _InvalidRuleContentSha256Exception,
    _InvalidEmailException,
    _CommitMessageLengthExceededException,
    _BlobIdDoesNotExistException,
    _MaximumRepositoryNamesExceededException,
    _TagKeysListRequiredException,
    _PutFileEntryConflictException,
    _FolderDoesNotExistException,
    _InvalidRepositoryDescriptionException,
    _RepositoryNameExistsException,
    _ReferenceNameRequiredException,
    _MaximumRepositoryTriggersExceededException,
    _ApprovalRuleDoesNotExistException,
    _InvalidBranchNameException,
    _BranchNameRequiredException,
    _MergeOptionRequiredException,
    _InvalidFileLocationException,
    _BeforeCommitIdAndAfterCommitIdAreSameException,
    _RepositoryTriggersListRequiredException,
    _IdempotencyParameterMismatchException,
    _EncryptionKeyUnavailableException,
    _InvalidRelativeFileVersionEnumException,
    _InvalidRepositoryTriggerDestinationArnException,
    _ReactionLimitExceededException,
    _BlobIdRequiredException,
    _RepositoryNamesRequiredException,
    _ReplacementTypeRequiredException,
    _InvalidActorArnException,
    _InvalidCommentIdException,
    _FilePathConflictsWithSubmodulePathException,
    _InvalidDescriptionException,
    _ApprovalRuleNameRequiredException,
    _InvalidBlobIdException,
    _PullRequestDoesNotExistException,
    _NoChangeException,
    _InvalidOrderException,
    _InvalidApprovalRuleNameException,
    _BranchDoesNotExistException,
    _DefaultBranchCannotBeDeletedException,
    _FolderContentSizeLimitExceededException,
    _InvalidDeletionParameterException,
    _InvalidReactionUserArnException,
    _InvalidTagsMapException,
    _InvalidPathException,
    _PathRequiredException,
    _InvalidTargetBranchException,
    _RepositoryTriggerNameRequiredException,
    _InvalidFileModeException,
    _NumberOfRuleTemplatesExceededException,
    _FileModeRequiredException,
    _InvalidPullRequestStatusException,
    _ApprovalRuleTemplateContentRequiredException,
    _ApprovalStateRequiredException,
    _ConcurrentReferenceUpdateException,
    _ParentCommitIdRequiredException,
    _InvalidSourceCommitSpecifierException,
    _RepositoryDoesNotExistException,
    _InvalidApprovalRuleContentException,
    _MaximumBranchesExceededException,
    _InvalidTitleException,
    _CommentContentSizeLimitExceededException,
    _PullRequestApprovalRulesNotSatisfiedException,
    _InvalidParentCommitIdException,
    _InvalidPullRequestEventTypeException,
    _FileContentRequiredException,
    _SourceAndDestinationAreSameException,
    _ReplacementContentRequiredException,
    _RestrictedSourceFileException,
    _PathDoesNotExistException,
    _InvalidResourceArnException,
    _TooManyTagsException,
    _EncryptionIntegrityChecksFailedException,
    _SamePathRequestException,
    _SourceFileOrContentRequiredException,
    _InvalidMaxMergeHunksException,
    _CannotModifyApprovalRuleFromTemplateException,
    _InvalidReplacementContentException,
    _ParentCommitIdOutdatedException,
    _RepositoryTriggerEventsListRequiredException,
    _CommentContentRequiredException,
    _ReactionValueRequiredException,
    _InvalidTargetsException,
    _EncryptionKeyAccessDeniedException,
    _BranchNameExistsException,
    _InvalidCommitException,
    _TargetRequiredException,
    _InvalidConflictDetailLevelException,
    _InvalidDestinationCommitSpecifierException,
    _CommentDoesNotExistException,
    _ReferenceTypeNotSupportedException,
    _FileNameConflictsWithDirectoryNameException,
    _NameLengthExceededException,
    _InvalidSortByException,
    _EncryptionKeyDisabledException,
    _CommitRequiredException,
    _MaximumOpenPullRequestsExceededException,
    _ApprovalRuleTemplateNameAlreadyExistsException,
    _InvalidTargetException,
    _InvalidPullRequestIdException,
    _CommentNotCreatedByCallerException,
    _InvalidPullRequestStatusUpdateException,
    _InvalidReferenceNameException,
    _MaximumRuleTemplatesAssociatedWithRepositoryException,
    _SameFileContentException,
    _ApprovalRuleTemplateInUseException,
    _MaximumNumberOfApprovalsExceededException,
    _CommitIdRequiredException,
    _FileDoesNotExistException,
    _InvalidCommitIdException,
    _InvalidTagKeysListException,
    _FileContentAndSourceFileSpecifiedException,
    _TipOfSourceReferenceIsDifferentException,
    _RepositoryTriggerDestinationArnRequiredException,
    _InvalidConflictResolutionStrategyException,
    _InvalidClientRequestTokenException,
    _MultipleConflictResolutionEntriesException,
    _CommitDoesNotExistException,
    _RepositoryTriggerBranchNameListRequiredException,
    _ClientRequestTokenRequiredException,
    _ApprovalRuleTemplateDoesNotExistException,
    _TagPolicyException,
    _InvalidMergeOptionException,
    _CannotDeleteApprovalRuleFromTemplateException,
    _CommentIdRequiredException,
    _InvalidMaxResultsException,
    _FileTooLargeException,
    _ApprovalRuleTemplateNameRequiredException,
    _MaximumFileEntriesExceededException,
    _CommitIdDoesNotExistException,
    _InvalidReplacementTypeException,
    _InvalidRevisionIdException,
    _RevisionNotCurrentException,
    _InvalidApprovalRuleTemplateNameException,
    _PullRequestCannotBeApprovedByAuthorException,
    _MultipleRepositoriesInPullRequestException,
    _RevisionIdRequiredException,
    _FileContentSizeLimitExceededException,
    _InvalidRepositoryTriggerNameException,
    _RepositoryNameRequiredException,
    _RepositoryLimitExceededException,
    _TagsMapRequiredException,
    _InvalidRepositoryTriggerEventsException,
    _NumberOfRulesExceededException,
    _BranchNameIsTagNameException,
    _InvalidRepositoryNameException,
    _CommitIdsListRequiredException,
    _CommitIdsLimitExceededException,
    _InvalidAuthorArnException,
    _MaximumItemsToCompareExceededException,
    _OverrideStatusRequiredException,
    _ApprovalRuleContentRequiredException,
    _MaximumConflictResolutionEntriesExceededException,
    _PullRequestStatusRequiredException,
    _InvalidConflictResolutionException,
    _InvalidApprovalRuleTemplateContentException,
    _InvalidApprovalStateException,
    _RepositoryNotAssociatedWithPullRequestException,
    _MaximumFileContentToLoadExceededException,
    _TitleRequiredException,
    _InvalidOverrideStatusException,
    _InvalidFilePositionException,
    _CommentDeletedException,
    _ParentCommitDoesNotExistException,
    _InvalidApprovalRuleTemplateDescriptionException,
    _ResourceArnRequiredException,
    _InvalidMaxConflictFilesException,
    _AuthorDoesNotExistException,

    -- * CommitId
    CommitId (..),

    -- * SymbolicLink
    SymbolicLink (..),
    mkSymbolicLink,
    slAbsolutePath,
    slBlobId,
    slFileMode,
    slRelativePath,

    -- * Email
    Email (..),

    -- * PullRequestMergedStateChangedEventMetadata
    PullRequestMergedStateChangedEventMetadata (..),
    mkPullRequestMergedStateChangedEventMetadata,
    prmscemDestinationReference,
    prmscemMergeMetadata,
    prmscemRepositoryName,

    -- * RuleContentSha256
    RuleContentSha256 (..),

    -- * RepositoryDescription
    RepositoryDescription (..),

    -- * ApprovalRuleTemplateId
    ApprovalRuleTemplateId (..),

    -- * CommentsForPullRequest
    CommentsForPullRequest (..),
    mkCommentsForPullRequest,
    cfprAfterBlobId,
    cfprAfterCommitId,
    cfprBeforeBlobId,
    cfprBeforeCommitId,
    cfprComments,
    cfprLocation,
    cfprPullRequestId,
    cfprRepositoryName,

    -- * ApprovalRuleTemplateDescription
    ApprovalRuleTemplateDescription (..),

    -- * UserInfo
    UserInfo (..),
    mkUserInfo,
    uiDate,
    uiEmail,
    uiName,

    -- * ReactionValue
    ReactionValue (..),

    -- * ReactionValueFormats
    ReactionValueFormats (..),
    mkReactionValueFormats,
    rvfEmoji,
    rvfShortCode,
    rvfUnicode,

    -- * SortByEnum
    SortByEnum (..),

    -- * ConflictDetailLevelTypeEnum
    ConflictDetailLevelTypeEnum (..),

    -- * Location
    Location (..),
    mkLocation,
    lFilePath,
    lFilePosition,
    lRelativeFileVersion,

    -- * PullRequestTarget
    PullRequestTarget (..),
    mkPullRequestTarget,
    prtDestinationCommit,
    prtDestinationReference,
    prtMergeBase,
    prtMergeMetadata,
    prtRepositoryName,
    prtSourceCommit,
    prtSourceReference,

    -- * PullRequestCreatedEventMetadata
    PullRequestCreatedEventMetadata (..),
    mkPullRequestCreatedEventMetadata,
    prcemDestinationCommitId,
    prcemMergeBase,
    prcemRepositoryName,
    prcemSourceCommitId,

    -- * MergeMetadata
    MergeMetadata (..),
    mkMergeMetadata,
    mmIsMerged,
    mmMergeCommitId,
    mmMergeOption,
    mmMergedBy,

    -- * ReplacementTypeEnum
    ReplacementTypeEnum (..),

    -- * Arn
    Arn (..),

    -- * RepositoryTriggerEventEnum
    RepositoryTriggerEventEnum (..),

    -- * RepositoryTriggerName
    RepositoryTriggerName (..),

    -- * Path
    Path (..),

    -- * BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError (..),
    mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError,
    bdartfreErrorCode,
    bdartfreErrorMessage,
    bdartfreRepositoryName,

    -- * CloneUrlHttp
    CloneUrlHttp (..),

    -- * PullRequestEventType
    PullRequestEventType (..),

    -- * ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata (..),
    mkApprovalRuleEventMetadata,
    aremApprovalRuleContent,
    aremApprovalRuleId,
    aremApprovalRuleName,

    -- * ApprovalRuleTemplateContent
    ApprovalRuleTemplateContent (..),

    -- * ApprovalState
    ApprovalState (..),

    -- * FileSizes
    FileSizes (..),
    mkFileSizes,
    fsBase,
    fsDestination,
    fsSource,

    -- * MergeHunk
    MergeHunk (..),
    mkMergeHunk,
    mhBase,
    mhDestination,
    mhIsConflict,
    mhSource,

    -- * BatchGetCommitsError
    BatchGetCommitsError (..),
    mkBatchGetCommitsError,
    bgceCommitId,
    bgceErrorCode,
    bgceErrorMessage,

    -- * MergeOptionTypeEnum
    MergeOptionTypeEnum (..),

    -- * ObjectId
    ObjectId (..),

    -- * PullRequestStatusEnum
    PullRequestStatusEnum (..),

    -- * BranchName
    BranchName (..),

    -- * PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata (..),
    mkPullRequestStatusChangedEventMetadata,
    prscemPullRequestStatus,

    -- * RepositoryTriggerExecutionFailureMessage
    RepositoryTriggerExecutionFailureMessage (..),

    -- * SetFileModeEntry
    SetFileModeEntry (..),
    mkSetFileModeEntry,
    sfmeFilePath,
    sfmeFileMode,

    -- * IsBinaryFile
    IsBinaryFile (..),
    mkIsBinaryFile,
    ibfBase,
    ibfDestination,
    ibfSource,

    -- * Mode
    Mode (..),

    -- * MergeOperations
    MergeOperations (..),
    mkMergeOperations,
    moDestination,
    moSource,

    -- * BlobMetadata
    BlobMetadata (..),
    mkBlobMetadata,
    bmBlobId,
    bmMode,
    bmPath,

    -- * SubModule
    SubModule (..),
    mkSubModule,
    smAbsolutePath,
    smCommitId,
    smRelativePath,

    -- * Folder
    Folder (..),
    mkFolder,
    ffAbsolutePath,
    ffRelativePath,
    ffTreeId,

    -- * ObjectTypeEnum
    ObjectTypeEnum (..),

    -- * ApprovalRuleName
    ApprovalRuleName (..),

    -- * CommentsForComparedCommit
    CommentsForComparedCommit (..),
    mkCommentsForComparedCommit,
    cfccAfterBlobId,
    cfccAfterCommitId,
    cfccBeforeBlobId,
    cfccBeforeCommitId,
    cfccComments,
    cfccLocation,
    cfccRepositoryName,

    -- * ApprovalRule
    ApprovalRule (..),
    mkApprovalRule,
    arApprovalRuleContent,
    arApprovalRuleId,
    arApprovalRuleName,
    arCreationDate,
    arLastModifiedDate,
    arLastModifiedUser,
    arOriginApprovalRuleTemplate,
    arRuleContentSha256,

    -- * TagValue
    TagValue (..),

    -- * Content
    Content (..),

    -- * AdditionalData
    AdditionalData (..),

    -- * ReactionForComment
    ReactionForComment (..),
    mkReactionForComment,
    rfcReaction,
    rfcReactionUsers,
    rfcReactionsFromDeletedUsersCount,

    -- * ReplaceContentEntry
    ReplaceContentEntry (..),
    mkReplaceContentEntry,
    rceFilePath,
    rceReplacementType,
    rceContent,
    rceFileMode,

    -- * PullRequestId
    PullRequestId (..),

    -- * AccountId
    AccountId (..),

    -- * NextToken
    NextToken (..),

    -- * BatchDescribeMergeConflictsError
    BatchDescribeMergeConflictsError (..),
    mkBatchDescribeMergeConflictsError,
    bdmceFilePath,
    bdmceExceptionName,
    bdmceMessage,

    -- * PullRequest
    PullRequest (..),
    mkPullRequest,
    prApprovalRules,
    prAuthorArn,
    prClientRequestToken,
    prCreationDate,
    prDescription,
    prLastActivityDate,
    prPullRequestId,
    prPullRequestStatus,
    prPullRequestTargets,
    prRevisionId,
    prTitle,

    -- * ReferenceName
    ReferenceName (..),

    -- * Date
    Date (..),

    -- * ApprovalRuleId
    ApprovalRuleId (..),

    -- * Difference
    Difference (..),
    mkDifference,
    dAfterBlob,
    dBeforeBlob,
    dChangeType,

    -- * RepositoryMetadata
    RepositoryMetadata (..),
    mkRepositoryMetadata,
    rmArn,
    rmAccountId,
    rmCloneUrlHttp,
    rmCloneUrlSsh,
    rmCreationDate,
    rmDefaultBranch,
    rmLastModifiedDate,
    rmRepositoryDescription,
    rmRepositoryId,
    rmRepositoryName,

    -- * ResourceArn
    ResourceArn (..),

    -- * Approval
    Approval (..),
    mkApproval,
    aApprovalState,
    aUserArn,

    -- * RepositoryId
    RepositoryId (..),

    -- * SourceFileSpecifier
    SourceFileSpecifier (..),
    mkSourceFileSpecifier,
    sfsFilePath,
    sfsIsMove,

    -- * Name
    Name (..),

    -- * BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError (..),
    mkBatchAssociateApprovalRuleTemplateWithRepositoriesError,
    baartwreErrorCode,
    baartwreErrorMessage,
    baartwreRepositoryName,

    -- * RepositoryTrigger
    RepositoryTrigger (..),
    mkRepositoryTrigger,
    rtName,
    rtDestinationArn,
    rtEvents,
    rtBranches,
    rtCustomData,

    -- * RepositoryNameIdPair
    RepositoryNameIdPair (..),
    mkRepositoryNameIdPair,
    rnipRepositoryId,
    rnipRepositoryName,

    -- * BranchInfo
    BranchInfo (..),
    mkBranchInfo,
    biBranchName,
    biCommitId,

    -- * ErrorCode
    ErrorCode (..),

    -- * OrderEnum
    OrderEnum (..),

    -- * OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate (..),
    mkOriginApprovalRuleTemplate,
    oartApprovalRuleTemplateId,
    oartApprovalRuleTemplateName,

    -- * RepositoryName
    RepositoryName (..),

    -- * ChangeTypeEnum
    ChangeTypeEnum (..),

    -- * TagKey
    TagKey (..),

    -- * ConflictResolution
    ConflictResolution (..),
    mkConflictResolution,
    crDeleteFiles,
    crReplaceContents,
    crSetFileModes,

    -- * Title
    Title (..),

    -- * CloneUrlSsh
    CloneUrlSsh (..),

    -- * ConflictResolutionStrategyTypeEnum
    ConflictResolutionStrategyTypeEnum (..),

    -- * RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure (..),
    mkRepositoryTriggerExecutionFailure,
    rtefFailureMessage,
    rtefTrigger,

    -- * ApprovalRuleContent
    ApprovalRuleContent (..),

    -- * PullRequestEvent
    PullRequestEvent (..),
    mkPullRequestEvent,
    preActorArn,
    preApprovalRuleEventMetadata,
    preApprovalRuleOverriddenEventMetadata,
    preApprovalStateChangedEventMetadata,
    preEventDate,
    prePullRequestCreatedEventMetadata,
    prePullRequestEventType,
    prePullRequestId,
    prePullRequestMergedStateChangedEventMetadata,
    prePullRequestSourceReferenceUpdatedEventMetadata,
    prePullRequestStatusChangedEventMetadata,

    -- * ExceptionName
    ExceptionName (..),

    -- * OverrideStatus
    OverrideStatus (..),

    -- * DeleteFileEntry
    DeleteFileEntry (..),
    mkDeleteFileEntry,
    dfeFilePath,

    -- * ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata (..),
    mkApprovalStateChangedEventMetadata,
    ascemApprovalStatus,
    ascemRevisionId,

    -- * FileMetadata
    FileMetadata (..),
    mkFileMetadata,
    fmAbsolutePath,
    fmBlobId,
    fmFileMode,

    -- * ConflictMetadata
    ConflictMetadata (..),
    mkConflictMetadata,
    cmContentConflict,
    cmFileModeConflict,
    cmFileModes,
    cmFilePath,
    cmFileSizes,
    cmIsBinaryFile,
    cmMergeOperations,
    cmNumberOfConflicts,
    cmObjectTypeConflict,
    cmObjectTypes,

    -- * PutFileEntry
    PutFileEntry (..),
    mkPutFileEntry,
    pfeFilePath,
    pfeFileContent,
    pfeFileMode,
    pfeSourceFile,

    -- * ErrorMessage
    ErrorMessage (..),

    -- * Message
    Message (..),

    -- * RelativeFileVersionEnum
    RelativeFileVersionEnum (..),

    -- * ClientRequestToken
    ClientRequestToken (..),

    -- * Comment
    Comment (..),
    mkComment,
    cAuthorArn,
    cCallerReactions,
    cClientRequestToken,
    cCommentId,
    cContent,
    cCreationDate,
    cDeleted,
    cInReplyTo,
    cLastModifiedDate,
    cReactionCounts,

    -- * CommitName
    CommitName (..),

    -- * CommentId
    CommentId (..),

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata (..),
    mkPullRequestSourceReferenceUpdatedEventMetadata,
    prsruemAfterCommitId,
    prsruemBeforeCommitId,
    prsruemMergeBase,
    prsruemRepositoryName,

    -- * ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata (..),
    mkApprovalRuleOverriddenEventMetadata,
    aroemOverrideStatus,
    aroemRevisionId,

    -- * Description
    Description (..),

    -- * Conflict
    Conflict (..),
    mkConflict,
    cConflictMetadata,
    cMergeHunks,

    -- * Evaluation
    Evaluation (..),
    mkEvaluation,
    eApprovalRulesNotSatisfied,
    eApprovalRulesSatisfied,
    eApproved,
    eOverridden,

    -- * ObjectTypes
    ObjectTypes (..),
    mkObjectTypes,
    otBase,
    otDestination,
    otSource,

    -- * ApprovalRuleTemplateName
    ApprovalRuleTemplateName (..),

    -- * FileModes
    FileModes (..),
    mkFileModes,
    fmBase,
    fmDestination,
    fmSource,

    -- * File
    File (..),
    mkFile,
    fAbsolutePath,
    fBlobId,
    fFileMode,
    fRelativePath,

    -- * RevisionId
    RevisionId (..),

    -- * FileModeTypeEnum
    FileModeTypeEnum (..),

    -- * HunkContent
    HunkContent (..),

    -- * MergeHunkDetail
    MergeHunkDetail (..),
    mkMergeHunkDetail,
    mhdEndLine,
    mhdHunkContent,
    mhdStartLine,

    -- * ApprovalRuleTemplate
    ApprovalRuleTemplate (..),
    mkApprovalRuleTemplate,
    artApprovalRuleTemplateContent,
    artApprovalRuleTemplateDescription,
    artApprovalRuleTemplateId,
    artApprovalRuleTemplateName,
    artCreationDate,
    artLastModifiedDate,
    artLastModifiedUser,
    artRuleContentSha256,

    -- * Commit
    Commit (..),
    mkCommit,
    cAdditionalData,
    cAuthor,
    cCommitId,
    cCommitter,
    cMessage,
    cParents,
    cTreeId,

    -- * Target
    Target (..),
    mkTarget,
    tRepositoryName,
    tSourceReference,
    tDestinationReference,

    -- * SourceCommitSpecifier
    SourceCommitSpecifier (..),

    -- * DestinationCommitSpecifier
    DestinationCommitSpecifier (..),

    -- * TargetBranch
    TargetBranch (..),

    -- * DestinationCommitId
    DestinationCommitId (..),

    -- * SourceCommitId
    SourceCommitId (..),

    -- * BaseCommitId
    BaseCommitId (..),

    -- * AuthorName
    AuthorName (..),

    -- * CommitMessage
    CommitMessage (..),

    -- * AbsolutePath
    AbsolutePath (..),

    -- * BlobId
    BlobId (..),

    -- * RelativePath
    RelativePath (..),

    -- * DestinationReference
    DestinationReference (..),

    -- * ConfigurationId
    ConfigurationId (..),

    -- * NewRuleContent
    NewRuleContent (..),

    -- * AfterBlobId
    AfterBlobId (..),

    -- * BeforeBlobId
    BeforeBlobId (..),

    -- * TreeId
    TreeId (..),

    -- * FilePath
    FilePath (..),

    -- * InReplyTo
    InReplyTo (..),

    -- * Emoji
    Emoji (..),

    -- * ShortCode
    ShortCode (..),

    -- * Unicode
    Unicode (..),

    -- * SourceReference
    SourceReference (..),

    -- * MergedBy
    MergedBy (..),

    -- * DefaultBranchName
    DefaultBranchName (..),

    -- * MergedCommitId
    MergedCommitId (..),

    -- * CommitSpecifier
    CommitSpecifier (..),

    -- * AfterCommitSpecifier
    AfterCommitSpecifier (..),

    -- * BeforeCommitSpecifier
    BeforeCommitSpecifier (..),

    -- * OldName
    OldName (..),

    -- * NewName
    NewName (..),

    -- * CustomData
    CustomData (..),
  )
where

import Network.AWS.CodeCommit.Types.AbsolutePath
import Network.AWS.CodeCommit.Types.AccountId
import Network.AWS.CodeCommit.Types.AdditionalData
import Network.AWS.CodeCommit.Types.AfterBlobId
import Network.AWS.CodeCommit.Types.AfterCommitSpecifier
import Network.AWS.CodeCommit.Types.Approval
import Network.AWS.CodeCommit.Types.ApprovalRule
import Network.AWS.CodeCommit.Types.ApprovalRuleContent
import Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleId
import Network.AWS.CodeCommit.Types.ApprovalRuleName
import Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplateContent
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplateDescription
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplateId
import Network.AWS.CodeCommit.Types.ApprovalRuleTemplateName
import Network.AWS.CodeCommit.Types.ApprovalState
import Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.Arn
import Network.AWS.CodeCommit.Types.AuthorName
import Network.AWS.CodeCommit.Types.BaseCommitId
import Network.AWS.CodeCommit.Types.BatchAssociateApprovalRuleTemplateWithRepositoriesError
import Network.AWS.CodeCommit.Types.BatchDescribeMergeConflictsError
import Network.AWS.CodeCommit.Types.BatchDisassociateApprovalRuleTemplateFromRepositoriesError
import Network.AWS.CodeCommit.Types.BatchGetCommitsError
import Network.AWS.CodeCommit.Types.BeforeBlobId
import Network.AWS.CodeCommit.Types.BeforeCommitSpecifier
import Network.AWS.CodeCommit.Types.BlobId
import Network.AWS.CodeCommit.Types.BlobMetadata
import Network.AWS.CodeCommit.Types.BranchInfo
import Network.AWS.CodeCommit.Types.BranchName
import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import Network.AWS.CodeCommit.Types.ClientRequestToken
import Network.AWS.CodeCommit.Types.CloneUrlHttp
import Network.AWS.CodeCommit.Types.CloneUrlSsh
import Network.AWS.CodeCommit.Types.Comment
import Network.AWS.CodeCommit.Types.CommentId
import Network.AWS.CodeCommit.Types.CommentsForComparedCommit
import Network.AWS.CodeCommit.Types.CommentsForPullRequest
import Network.AWS.CodeCommit.Types.Commit
import Network.AWS.CodeCommit.Types.CommitId
import Network.AWS.CodeCommit.Types.CommitMessage
import Network.AWS.CodeCommit.Types.CommitName
import Network.AWS.CodeCommit.Types.CommitSpecifier
import Network.AWS.CodeCommit.Types.ConfigurationId
import Network.AWS.CodeCommit.Types.Conflict
import Network.AWS.CodeCommit.Types.ConflictDetailLevelTypeEnum
import Network.AWS.CodeCommit.Types.ConflictMetadata
import Network.AWS.CodeCommit.Types.ConflictResolution
import Network.AWS.CodeCommit.Types.ConflictResolutionStrategyTypeEnum
import Network.AWS.CodeCommit.Types.Content
import Network.AWS.CodeCommit.Types.CustomData
import Network.AWS.CodeCommit.Types.Date
import Network.AWS.CodeCommit.Types.DefaultBranchName
import Network.AWS.CodeCommit.Types.DeleteFileEntry
import Network.AWS.CodeCommit.Types.Description
import Network.AWS.CodeCommit.Types.DestinationCommitId
import Network.AWS.CodeCommit.Types.DestinationCommitSpecifier
import Network.AWS.CodeCommit.Types.DestinationReference
import Network.AWS.CodeCommit.Types.Difference
import Network.AWS.CodeCommit.Types.Email
import Network.AWS.CodeCommit.Types.Emoji
import Network.AWS.CodeCommit.Types.ErrorCode
import Network.AWS.CodeCommit.Types.ErrorMessage
import Network.AWS.CodeCommit.Types.Evaluation
import Network.AWS.CodeCommit.Types.ExceptionName
import Network.AWS.CodeCommit.Types.File
import Network.AWS.CodeCommit.Types.FileMetadata
import Network.AWS.CodeCommit.Types.FileModeTypeEnum
import Network.AWS.CodeCommit.Types.FileModes
import Network.AWS.CodeCommit.Types.FilePath
import Network.AWS.CodeCommit.Types.FileSizes
import Network.AWS.CodeCommit.Types.Folder
import Network.AWS.CodeCommit.Types.HunkContent
import Network.AWS.CodeCommit.Types.InReplyTo
import Network.AWS.CodeCommit.Types.IsBinaryFile
import Network.AWS.CodeCommit.Types.Location
import Network.AWS.CodeCommit.Types.MergeHunk
import Network.AWS.CodeCommit.Types.MergeHunkDetail
import Network.AWS.CodeCommit.Types.MergeMetadata
import Network.AWS.CodeCommit.Types.MergeOperations
import Network.AWS.CodeCommit.Types.MergeOptionTypeEnum
import Network.AWS.CodeCommit.Types.MergedBy
import Network.AWS.CodeCommit.Types.MergedCommitId
import Network.AWS.CodeCommit.Types.Message
import Network.AWS.CodeCommit.Types.Mode
import Network.AWS.CodeCommit.Types.Name
import Network.AWS.CodeCommit.Types.NewName
import Network.AWS.CodeCommit.Types.NewRuleContent
import Network.AWS.CodeCommit.Types.NextToken
import Network.AWS.CodeCommit.Types.ObjectId
import Network.AWS.CodeCommit.Types.ObjectTypeEnum
import Network.AWS.CodeCommit.Types.ObjectTypes
import Network.AWS.CodeCommit.Types.OldName
import Network.AWS.CodeCommit.Types.OrderEnum
import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import Network.AWS.CodeCommit.Types.OverrideStatus
import Network.AWS.CodeCommit.Types.Path
import Network.AWS.CodeCommit.Types.PullRequest
import Network.AWS.CodeCommit.Types.PullRequestCreatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestEvent
import Network.AWS.CodeCommit.Types.PullRequestEventType
import Network.AWS.CodeCommit.Types.PullRequestId
import Network.AWS.CodeCommit.Types.PullRequestMergedStateChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestSourceReferenceUpdatedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusChangedEventMetadata
import Network.AWS.CodeCommit.Types.PullRequestStatusEnum
import Network.AWS.CodeCommit.Types.PullRequestTarget
import Network.AWS.CodeCommit.Types.PutFileEntry
import Network.AWS.CodeCommit.Types.ReactionForComment
import Network.AWS.CodeCommit.Types.ReactionValue
import Network.AWS.CodeCommit.Types.ReactionValueFormats
import Network.AWS.CodeCommit.Types.ReferenceName
import Network.AWS.CodeCommit.Types.RelativeFileVersionEnum
import Network.AWS.CodeCommit.Types.RelativePath
import Network.AWS.CodeCommit.Types.ReplaceContentEntry
import Network.AWS.CodeCommit.Types.ReplacementTypeEnum
import Network.AWS.CodeCommit.Types.RepositoryDescription
import Network.AWS.CodeCommit.Types.RepositoryId
import Network.AWS.CodeCommit.Types.RepositoryMetadata
import Network.AWS.CodeCommit.Types.RepositoryName
import Network.AWS.CodeCommit.Types.RepositoryNameIdPair
import Network.AWS.CodeCommit.Types.RepositoryTrigger
import Network.AWS.CodeCommit.Types.RepositoryTriggerEventEnum
import Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailure
import Network.AWS.CodeCommit.Types.RepositoryTriggerExecutionFailureMessage
import Network.AWS.CodeCommit.Types.RepositoryTriggerName
import Network.AWS.CodeCommit.Types.ResourceArn
import Network.AWS.CodeCommit.Types.RevisionId
import Network.AWS.CodeCommit.Types.RuleContentSha256
import Network.AWS.CodeCommit.Types.SetFileModeEntry
import Network.AWS.CodeCommit.Types.ShortCode
import Network.AWS.CodeCommit.Types.SortByEnum
import Network.AWS.CodeCommit.Types.SourceCommitId
import Network.AWS.CodeCommit.Types.SourceCommitSpecifier
import Network.AWS.CodeCommit.Types.SourceFileSpecifier
import Network.AWS.CodeCommit.Types.SourceReference
import Network.AWS.CodeCommit.Types.SubModule
import Network.AWS.CodeCommit.Types.SymbolicLink
import Network.AWS.CodeCommit.Types.TagKey
import Network.AWS.CodeCommit.Types.TagValue
import Network.AWS.CodeCommit.Types.Target
import Network.AWS.CodeCommit.Types.TargetBranch
import Network.AWS.CodeCommit.Types.Title
import Network.AWS.CodeCommit.Types.TreeId
import Network.AWS.CodeCommit.Types.Unicode
import Network.AWS.CodeCommit.Types.UserInfo
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
mkServiceConfig :: Core.Service
mkServiceConfig =
  Core.Service
    { Core._svcAbbrev = "CodeCommit",
      Core._svcSigner = Sign.v4,
      Core._svcPrefix = "codecommit",
      Core._svcVersion = "2015-04-13",
      Core._svcTimeout = Core.Just 70,
      Core._svcCheck = Core.statusSuccess,
      Core._svcRetry = retry,
      Core._svcError = Core.parseJSONError "CodeCommit",
      Core._svcEndpoint = Core.defaultEndpoint mkServiceConfig
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
      | Lens.has
          (Core.hasCode "ThrottledException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e = Core.Just "too_many_requests"
      | Lens.has
          (Core.hasCode "ThrottlingException" Core.. Core.hasStatus 400)
          e =
        Core.Just "throttling_exception"
      | Lens.has (Core.hasCode "Throttling" Core.. Core.hasStatus 400) e =
        Core.Just "throttling"
      | Lens.has
          ( Core.hasCode "ProvisionedThroughputExceededException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e = Core.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Core.. Core.hasStatus 400
          )
          e =
        Core.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e = Core.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e = Core.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Core.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e = Core.Just "limit_exceeded"
      | Core.otherwise = Core.Nothing

-- | The value of the reaction is not valid. For more information, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide> .
_InvalidReactionValueException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReactionValueException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReactionValueException"
{-# DEPRECATED _InvalidReactionValueException "Use generic-lens or generic-optics instead." #-}

-- | The AWS Region for the trigger target does not match the AWS Region for the repository. Triggers must be created in the same Region as the target for the trigger.
_InvalidRepositoryTriggerRegionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerRegionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerRegionException"
{-# DEPRECATED _InvalidRepositoryTriggerRegionException "Use generic-lens or generic-optics instead." #-}

-- | The specified continuation token is not valid.
_InvalidContinuationTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidContinuationTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidContinuationTokenException"
{-# DEPRECATED _InvalidContinuationTokenException "Use generic-lens or generic-optics instead." #-}

-- | The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.
_ManualMergeRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ManualMergeRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ManualMergeRequiredException"
{-# DEPRECATED _ManualMergeRequiredException "Use generic-lens or generic-optics instead." #-}

-- | An array of target objects is required. It cannot be empty or null.
_TargetsRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetsRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "TargetsRequiredException"
{-# DEPRECATED _TargetsRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified tag is not valid. Key names cannot be prefixed with aws:.
_InvalidSystemTagUsageException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSystemTagUsageException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSystemTagUsageException"
{-# DEPRECATED _InvalidSystemTagUsageException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because no files have been specified as added, updated, or changed (PutFile or DeleteFile) for the commit.
_FileEntryRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileEntryRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "FileEntryRequiredException"
{-# DEPRECATED _FileEntryRequiredException "Use generic-lens or generic-optics instead." #-}

-- | No encryption key was found.
_EncryptionKeyNotFoundException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyNotFoundException =
  Core._MatchServiceError
    mkServiceConfig
    "EncryptionKeyNotFoundException"
{-# DEPRECATED _EncryptionKeyNotFoundException "Use generic-lens or generic-optics instead." #-}

-- | The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using @git diff@ or a diff tool.
_TipsDivergenceExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TipsDivergenceExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "TipsDivergenceExceededException"
{-# DEPRECATED _TipsDivergenceExceededException "Use generic-lens or generic-optics instead." #-}

-- | One or more branch names specified for the trigger is not valid.
_InvalidRepositoryTriggerBranchNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerBranchNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerBranchNameException"
{-# DEPRECATED _InvalidRepositoryTriggerBranchNameException "Use generic-lens or generic-optics instead." #-}

-- | The pull request status cannot be updated because it is already closed.
_PullRequestAlreadyClosedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestAlreadyClosedException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestAlreadyClosedException"
{-# DEPRECATED _PullRequestAlreadyClosedException "Use generic-lens or generic-optics instead." #-}

-- | The custom data provided for the trigger is not valid.
_InvalidRepositoryTriggerCustomDataException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerCustomDataException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerCustomDataException"
{-# DEPRECATED _InvalidRepositoryTriggerCustomDataException "Use generic-lens or generic-optics instead." #-}

-- | A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.
_DirectoryNameConflictsWithFileNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DirectoryNameConflictsWithFileNameException =
  Core._MatchServiceError
    mkServiceConfig
    "DirectoryNameConflictsWithFileNameException"
{-# DEPRECATED _DirectoryNameConflictsWithFileNameException "Use generic-lens or generic-optics instead." #-}

-- | The specified reference does not exist. You must provide a full commit ID.
_ReferenceDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReferenceDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "ReferenceDoesNotExistException"
{-# DEPRECATED _ReferenceDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | An approval rule with that name already exists. Approval rule names must be unique within the scope of a pull request.
_ApprovalRuleNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleNameAlreadyExistsException"
{-# DEPRECATED _ApprovalRuleNameAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS account.
_ActorDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ActorDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "ActorDoesNotExistException"
{-# DEPRECATED _ActorDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | A pull request ID is required, but none was provided.
_PullRequestIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestIdRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestIdRequiredException"
{-# DEPRECATED _PullRequestIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The pull request has already had its approval rules set to override.
_OverrideAlreadySetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OverrideAlreadySetException =
  Core._MatchServiceError
    mkServiceConfig
    "OverrideAlreadySetException"
{-# DEPRECATED _OverrideAlreadySetException "Use generic-lens or generic-optics instead." #-}

-- | The SHA-256 hash signature for the rule content is not valid.
_InvalidRuleContentSha256Exception :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRuleContentSha256Exception =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRuleContentSha256Exception"
{-# DEPRECATED _InvalidRuleContentSha256Exception "Use generic-lens or generic-optics instead." #-}

-- | The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.
_InvalidEmailException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidEmailException =
  Core._MatchServiceError mkServiceConfig "InvalidEmailException"
{-# DEPRECATED _InvalidEmailException "Use generic-lens or generic-optics instead." #-}

-- | The commit message is too long. Provide a shorter string.
_CommitMessageLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitMessageLengthExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitMessageLengthExceededException"
{-# DEPRECATED _CommitMessageLengthExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified blob does not exist.
_BlobIdDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BlobIdDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "BlobIdDoesNotExistException"
{-# DEPRECATED _BlobIdDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of allowed repository names was exceeded. Currently, this number is 100.
_MaximumRepositoryNamesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryNamesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumRepositoryNamesExceededException"
{-# DEPRECATED _MaximumRepositoryNamesExceededException "Use generic-lens or generic-optics instead." #-}

-- | A list of tag keys is required. The list cannot be empty or null.
_TagKeysListRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagKeysListRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "TagKeysListRequiredException"
{-# DEPRECATED _TagKeysListRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because one or more files specified in the commit reference both a file and a folder.
_PutFileEntryConflictException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PutFileEntryConflictException =
  Core._MatchServiceError
    mkServiceConfig
    "PutFileEntryConflictException"
{-# DEPRECATED _PutFileEntryConflictException "Use generic-lens or generic-optics instead." #-}

-- | The specified folder does not exist. Either the folder name is not correct, or you did not enter the full path to the folder.
_FolderDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FolderDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "FolderDoesNotExistException"
{-# DEPRECATED _FolderDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The specified repository description is not valid.
_InvalidRepositoryDescriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryDescriptionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryDescriptionException"
{-# DEPRECATED _InvalidRepositoryDescriptionException "Use generic-lens or generic-optics instead." #-}

-- | The specified repository name already exists.
_RepositoryNameExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNameExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryNameExistsException"
{-# DEPRECATED _RepositoryNameExistsException "Use generic-lens or generic-optics instead." #-}

-- | A reference name is required, but none was provided.
_ReferenceNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReferenceNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ReferenceNameRequiredException"
{-# DEPRECATED _ReferenceNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The number of triggers allowed for the repository was exceeded.
_MaximumRepositoryTriggersExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumRepositoryTriggersExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumRepositoryTriggersExceededException"
{-# DEPRECATED _MaximumRepositoryTriggersExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified approval rule does not exist.
_ApprovalRuleDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleDoesNotExistException"
{-# DEPRECATED _ApprovalRuleDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The specified reference name is not valid.
_InvalidBranchNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBranchNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidBranchNameException"
{-# DEPRECATED _InvalidBranchNameException "Use generic-lens or generic-optics instead." #-}

-- | A branch name is required, but was not specified.
_BranchNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BranchNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "BranchNameRequiredException"
{-# DEPRECATED _BranchNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | A merge option or stategy is required, and none was provided.
_MergeOptionRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MergeOptionRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "MergeOptionRequiredException"
{-# DEPRECATED _MergeOptionRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The location of the file is not valid. Make sure that you include the file name and extension.
_InvalidFileLocationException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFileLocationException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidFileLocationException"
{-# DEPRECATED _InvalidFileLocationException "Use generic-lens or generic-optics instead." #-}

-- | The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.
_BeforeCommitIdAndAfterCommitIdAreSameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BeforeCommitIdAndAfterCommitIdAreSameException =
  Core._MatchServiceError
    mkServiceConfig
    "BeforeCommitIdAndAfterCommitIdAreSameException"
{-# DEPRECATED _BeforeCommitIdAndAfterCommitIdAreSameException "Use generic-lens or generic-optics instead." #-}

-- | The list of triggers for the repository is required, but was not specified.
_RepositoryTriggersListRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggersListRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryTriggersListRequiredException"
{-# DEPRECATED _RepositoryTriggersListRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be reused.
_IdempotencyParameterMismatchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_IdempotencyParameterMismatchException =
  Core._MatchServiceError
    mkServiceConfig
    "IdempotencyParameterMismatchException"
{-# DEPRECATED _IdempotencyParameterMismatchException "Use generic-lens or generic-optics instead." #-}

-- | The encryption key is not available.
_EncryptionKeyUnavailableException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyUnavailableException =
  Core._MatchServiceError
    mkServiceConfig
    "EncryptionKeyUnavailableException"
{-# DEPRECATED _EncryptionKeyUnavailableException "Use generic-lens or generic-optics instead." #-}

-- | Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.
_InvalidRelativeFileVersionEnumException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRelativeFileVersionEnumException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRelativeFileVersionEnumException"
{-# DEPRECATED _InvalidRelativeFileVersionEnumException "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.
_InvalidRepositoryTriggerDestinationArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerDestinationArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerDestinationArnException"
{-# DEPRECATED _InvalidRepositoryTriggerDestinationArnException "Use generic-lens or generic-optics instead." #-}

-- | The number of reactions has been exceeded. Reactions are limited to one reaction per user for each individual comment ID.
_ReactionLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReactionLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "ReactionLimitExceededException"
{-# DEPRECATED _ReactionLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | A blob ID is required, but was not specified.
_BlobIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BlobIdRequiredException =
  Core._MatchServiceError mkServiceConfig "BlobIdRequiredException"
{-# DEPRECATED _BlobIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | At least one repository name object is required, but was not specified.
_RepositoryNamesRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNamesRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryNamesRequiredException"
{-# DEPRECATED _RepositoryNamesRequiredException "Use generic-lens or generic-optics instead." #-}

-- | A replacement type is required.
_ReplacementTypeRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplacementTypeRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplacementTypeRequiredException"
{-# DEPRECATED _ReplacementTypeRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.
_InvalidActorArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidActorArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidActorArnException"
{-# DEPRECATED _InvalidActorArnException "Use generic-lens or generic-optics instead." #-}

-- | The comment ID is not in a valid format. Make sure that you have provided the full comment ID.
_InvalidCommentIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommentIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCommentIdException"
{-# DEPRECATED _InvalidCommentIdException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because a specified file path points to a submodule. Verify that the destination files have valid file paths that do not point to a submodule.
_FilePathConflictsWithSubmodulePathException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FilePathConflictsWithSubmodulePathException =
  Core._MatchServiceError
    mkServiceConfig
    "FilePathConflictsWithSubmodulePathException"
{-# DEPRECATED _FilePathConflictsWithSubmodulePathException "Use generic-lens or generic-optics instead." #-}

-- | The pull request description is not valid. Descriptions cannot be more than 1,000 characters.
_InvalidDescriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDescriptionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDescriptionException"
{-# DEPRECATED _InvalidDescriptionException "Use generic-lens or generic-optics instead." #-}

-- | An approval rule name is required, but was not specified.
_ApprovalRuleNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleNameRequiredException"
{-# DEPRECATED _ApprovalRuleNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified blob is not valid.
_InvalidBlobIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidBlobIdException =
  Core._MatchServiceError mkServiceConfig "InvalidBlobIdException"
{-# DEPRECATED _InvalidBlobIdException "Use generic-lens or generic-optics instead." #-}

-- | The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.
_PullRequestDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestDoesNotExistException"
{-# DEPRECATED _PullRequestDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because no changes will be made to the repository as a result of this commit. A commit must contain at least one change.
_NoChangeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NoChangeException =
  Core._MatchServiceError mkServiceConfig "NoChangeException"
{-# DEPRECATED _NoChangeException "Use generic-lens or generic-optics instead." #-}

-- | The specified sort order is not valid.
_InvalidOrderException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOrderException =
  Core._MatchServiceError mkServiceConfig "InvalidOrderException"
{-# DEPRECATED _InvalidOrderException "Use generic-lens or generic-optics instead." #-}

-- | The name for the approval rule is not valid.
_InvalidApprovalRuleNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalRuleNameException"
{-# DEPRECATED _InvalidApprovalRuleNameException "Use generic-lens or generic-optics instead." #-}

-- | The specified branch does not exist.
_BranchDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BranchDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "BranchDoesNotExistException"
{-# DEPRECATED _BranchDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.
_DefaultBranchCannotBeDeletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_DefaultBranchCannotBeDeletedException =
  Core._MatchServiceError
    mkServiceConfig
    "DefaultBranchCannotBeDeletedException"
{-# DEPRECATED _DefaultBranchCannotBeDeletedException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because at least one of the overall changes in the commit results in a folder whose contents exceed the limit of 6 MB. Either reduce the number and size of your changes, or split the changes across multiple folders.
_FolderContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FolderContentSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "FolderContentSizeLimitExceededException"
{-# DEPRECATED _FolderContentSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified deletion parameter is not valid.
_InvalidDeletionParameterException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDeletionParameterException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDeletionParameterException"
{-# DEPRECATED _InvalidDeletionParameterException "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Resource Name (ARN) of the user or identity is not valid.
_InvalidReactionUserArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReactionUserArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReactionUserArnException"
{-# DEPRECATED _InvalidReactionUserArnException "Use generic-lens or generic-optics instead." #-}

-- | The map of tags is not valid.
_InvalidTagsMapException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagsMapException =
  Core._MatchServiceError mkServiceConfig "InvalidTagsMapException"
{-# DEPRECATED _InvalidTagsMapException "Use generic-lens or generic-optics instead." #-}

-- | The specified path is not valid.
_InvalidPathException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPathException =
  Core._MatchServiceError mkServiceConfig "InvalidPathException"
{-# DEPRECATED _InvalidPathException "Use generic-lens or generic-optics instead." #-}

-- | The folderPath for a location cannot be null.
_PathRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PathRequiredException =
  Core._MatchServiceError mkServiceConfig "PathRequiredException"
{-# DEPRECATED _PathRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified target branch is not valid.
_InvalidTargetBranchException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetBranchException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTargetBranchException"
{-# DEPRECATED _InvalidTargetBranchException "Use generic-lens or generic-optics instead." #-}

-- | A name for the trigger is required, but was not specified.
_RepositoryTriggerNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryTriggerNameRequiredException"
{-# DEPRECATED _RepositoryTriggerNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified file mode permission is not valid. For a list of valid file mode permissions, see 'PutFile' .
_InvalidFileModeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFileModeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidFileModeException"
{-# DEPRECATED _InvalidFileModeException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of approval rule templates has been exceeded for this AWS Region.
_NumberOfRuleTemplatesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberOfRuleTemplatesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "NumberOfRuleTemplatesExceededException"
{-# DEPRECATED _NumberOfRuleTemplatesExceededException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because no file mode has been specified. A file mode is required to update mode permissions for a file.
_FileModeRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileModeRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "FileModeRequiredException"
{-# DEPRECATED _FileModeRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The pull request status is not valid. The only valid values are @OPEN@ and @CLOSED@ .
_InvalidPullRequestStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPullRequestStatusException"
{-# DEPRECATED _InvalidPullRequestStatusException "Use generic-lens or generic-optics instead." #-}

-- | The content for the approval rule template is empty. You must provide some content for an approval rule template. The content cannot be null.
_ApprovalRuleTemplateContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleTemplateContentRequiredException"
{-# DEPRECATED _ApprovalRuleTemplateContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | An approval state is required, but was not specified.
_ApprovalStateRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalStateRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalStateRequiredException"
{-# DEPRECATED _ApprovalStateRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The merge cannot be completed because the target branch has been modified. Another user might have modified the target branch while the merge was in progress. Wait a few minutes, and then try again.
_ConcurrentReferenceUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ConcurrentReferenceUpdateException =
  Core._MatchServiceError
    mkServiceConfig
    "ConcurrentReferenceUpdateException"
{-# DEPRECATED _ConcurrentReferenceUpdateException "Use generic-lens or generic-optics instead." #-}

-- | A parent commit ID is required. To view the full commit ID of a branch in a repository, use 'GetBranch' or a Git command (for example, git pull or git log).
_ParentCommitIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ParentCommitIdRequiredException"
{-# DEPRECATED _ParentCommitIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.
_InvalidSourceCommitSpecifierException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSourceCommitSpecifierException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidSourceCommitSpecifierException"
{-# DEPRECATED _InvalidSourceCommitSpecifierException "Use generic-lens or generic-optics instead." #-}

-- | The specified repository does not exist.
_RepositoryDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryDoesNotExistException"
{-# DEPRECATED _RepositoryDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The content for the approval rule is not valid.
_InvalidApprovalRuleContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleContentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalRuleContentException"
{-# DEPRECATED _InvalidApprovalRuleContentException "Use generic-lens or generic-optics instead." #-}

-- | The number of branches for the trigger was exceeded.
_MaximumBranchesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumBranchesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumBranchesExceededException"
{-# DEPRECATED _MaximumBranchesExceededException "Use generic-lens or generic-optics instead." #-}

-- | The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.
_InvalidTitleException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTitleException =
  Core._MatchServiceError mkServiceConfig "InvalidTitleException"
{-# DEPRECATED _InvalidTitleException "Use generic-lens or generic-optics instead." #-}

-- | The comment is too large. Comments are limited to 1,000 characters.
_CommentContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentContentSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "CommentContentSizeLimitExceededException"
{-# DEPRECATED _CommentContentSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The pull request cannot be merged because one or more approval rules applied to the pull request have conditions that have not been met.
_PullRequestApprovalRulesNotSatisfiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestApprovalRulesNotSatisfiedException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestApprovalRulesNotSatisfiedException"
{-# DEPRECATED _PullRequestApprovalRulesNotSatisfiedException "Use generic-lens or generic-optics instead." #-}

-- | The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.
_InvalidParentCommitIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidParentCommitIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidParentCommitIdException"
{-# DEPRECATED _InvalidParentCommitIdException "Use generic-lens or generic-optics instead." #-}

-- | The pull request event type is not valid.
_InvalidPullRequestEventTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestEventTypeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPullRequestEventTypeException"
{-# DEPRECATED _InvalidPullRequestEventTypeException "Use generic-lens or generic-optics instead." #-}

-- | The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.
_FileContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "FileContentRequiredException"
{-# DEPRECATED _FileContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The source branch and destination branch for the pull request are the same. You must specify different branches for the source and destination.
_SourceAndDestinationAreSameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceAndDestinationAreSameException =
  Core._MatchServiceError
    mkServiceConfig
    "SourceAndDestinationAreSameException"
{-# DEPRECATED _SourceAndDestinationAreSameException "Use generic-lens or generic-optics instead." #-}

-- | USE_NEW_CONTENT was specified, but no replacement content has been provided.
_ReplacementContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReplacementContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ReplacementContentRequiredException"
{-# DEPRECATED _ReplacementContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because one of the changes specifies copying or moving a .gitkeep file.
_RestrictedSourceFileException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RestrictedSourceFileException =
  Core._MatchServiceError
    mkServiceConfig
    "RestrictedSourceFileException"
{-# DEPRECATED _RestrictedSourceFileException "Use generic-lens or generic-optics instead." #-}

-- | The specified path does not exist.
_PathDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PathDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "PathDoesNotExistException"
{-# DEPRECATED _PathDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The value for the resource ARN is not valid. For more information about resources in AWS CodeCommit, see <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations> in the AWS CodeCommit User Guide.
_InvalidResourceArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidResourceArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidResourceArnException"
{-# DEPRECATED _InvalidResourceArnException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of tags for an AWS CodeCommit resource has been exceeded.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError mkServiceConfig "TooManyTagsException"
{-# DEPRECATED _TooManyTagsException "Use generic-lens or generic-optics instead." #-}

-- | An encryption integrity check failed.
_EncryptionIntegrityChecksFailedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionIntegrityChecksFailedException =
  Core._MatchServiceError
    mkServiceConfig
    "EncryptionIntegrityChecksFailedException"
{-# DEPRECATED _EncryptionIntegrityChecksFailedException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because one or more changes in this commit duplicate actions in the same file path. For example, you cannot make the same delete request to the same file in the same file path twice, or make a delete request and a move request to the same file as part of the same commit.
_SamePathRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SamePathRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "SamePathRequestException"
{-# DEPRECATED _SamePathRequestException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because no source files or file content have been specified for the commit.
_SourceFileOrContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SourceFileOrContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "SourceFileOrContentRequiredException"
{-# DEPRECATED _SourceFileOrContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified value for the number of merge hunks to return is not valid.
_InvalidMaxMergeHunksException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMaxMergeHunksException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidMaxMergeHunksException"
{-# DEPRECATED _InvalidMaxMergeHunksException "Use generic-lens or generic-optics instead." #-}

-- | The approval rule cannot be modified for the pull request because it was created by an approval rule template and applied to the pull request automatically.
_CannotModifyApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotModifyApprovalRuleFromTemplateException =
  Core._MatchServiceError
    mkServiceConfig
    "CannotModifyApprovalRuleFromTemplateException"
{-# DEPRECATED _CannotModifyApprovalRuleFromTemplateException "Use generic-lens or generic-optics instead." #-}

-- | Automerge was specified for resolving the conflict, but the replacement type is not valid or content is missing.
_InvalidReplacementContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementContentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReplacementContentException"
{-# DEPRECATED _InvalidReplacementContentException "Use generic-lens or generic-optics instead." #-}

-- | The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use 'GetBranch' .
_ParentCommitIdOutdatedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParentCommitIdOutdatedException =
  Core._MatchServiceError
    mkServiceConfig
    "ParentCommitIdOutdatedException"
{-# DEPRECATED _ParentCommitIdOutdatedException "Use generic-lens or generic-optics instead." #-}

-- | At least one event for the trigger is required, but was not specified.
_RepositoryTriggerEventsListRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerEventsListRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryTriggerEventsListRequiredException"
{-# DEPRECATED _RepositoryTriggerEventsListRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The comment is empty. You must provide some content for a comment. The content cannot be null.
_CommentContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "CommentContentRequiredException"
{-# DEPRECATED _CommentContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | A reaction value is required.
_ReactionValueRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReactionValueRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ReactionValueRequiredException"
{-# DEPRECATED _ReactionValueRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.
_InvalidTargetsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetsException =
  Core._MatchServiceError mkServiceConfig "InvalidTargetsException"
{-# DEPRECATED _InvalidTargetsException "Use generic-lens or generic-optics instead." #-}

-- | An encryption key could not be accessed.
_EncryptionKeyAccessDeniedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyAccessDeniedException =
  Core._MatchServiceError
    mkServiceConfig
    "EncryptionKeyAccessDeniedException"
{-# DEPRECATED _EncryptionKeyAccessDeniedException "Use generic-lens or generic-optics instead." #-}

-- | Cannot create the branch with the specified name because the commit conflicts with an existing branch with the same name. Branch names must be unique.
_BranchNameExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BranchNameExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "BranchNameExistsException"
{-# DEPRECATED _BranchNameExistsException "Use generic-lens or generic-optics instead." #-}

-- | The specified commit is not valid.
_InvalidCommitException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommitException =
  Core._MatchServiceError mkServiceConfig "InvalidCommitException"
{-# DEPRECATED _InvalidCommitException "Use generic-lens or generic-optics instead." #-}

-- | A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.
_TargetRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TargetRequiredException =
  Core._MatchServiceError mkServiceConfig "TargetRequiredException"
{-# DEPRECATED _TargetRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified conflict detail level is not valid.
_InvalidConflictDetailLevelException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConflictDetailLevelException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidConflictDetailLevelException"
{-# DEPRECATED _InvalidConflictDetailLevelException "Use generic-lens or generic-optics instead." #-}

-- | The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.
_InvalidDestinationCommitSpecifierException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidDestinationCommitSpecifierException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidDestinationCommitSpecifierException"
{-# DEPRECATED _InvalidDestinationCommitSpecifierException "Use generic-lens or generic-optics instead." #-}

-- | No comment exists with the provided ID. Verify that you have used the correct ID, and then try again.
_CommentDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "CommentDoesNotExistException"
{-# DEPRECATED _CommentDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The specified reference is not a supported type.
_ReferenceTypeNotSupportedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ReferenceTypeNotSupportedException =
  Core._MatchServiceError
    mkServiceConfig
    "ReferenceTypeNotSupportedException"
{-# DEPRECATED _ReferenceTypeNotSupportedException "Use generic-lens or generic-optics instead." #-}

-- | A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.
_FileNameConflictsWithDirectoryNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileNameConflictsWithDirectoryNameException =
  Core._MatchServiceError
    mkServiceConfig
    "FileNameConflictsWithDirectoryNameException"
{-# DEPRECATED _FileNameConflictsWithDirectoryNameException "Use generic-lens or generic-optics instead." #-}

-- | The user name is not valid because it has exceeded the character limit for author names.
_NameLengthExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NameLengthExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "NameLengthExceededException"
{-# DEPRECATED _NameLengthExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified sort by value is not valid.
_InvalidSortByException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidSortByException =
  Core._MatchServiceError mkServiceConfig "InvalidSortByException"
{-# DEPRECATED _InvalidSortByException "Use generic-lens or generic-optics instead." #-}

-- | The encryption key is disabled.
_EncryptionKeyDisabledException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_EncryptionKeyDisabledException =
  Core._MatchServiceError
    mkServiceConfig
    "EncryptionKeyDisabledException"
{-# DEPRECATED _EncryptionKeyDisabledException "Use generic-lens or generic-optics instead." #-}

-- | A commit was not specified.
_CommitRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitRequiredException =
  Core._MatchServiceError mkServiceConfig "CommitRequiredException"
{-# DEPRECATED _CommitRequiredException "Use generic-lens or generic-optics instead." #-}

-- | You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.
_MaximumOpenPullRequestsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumOpenPullRequestsExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumOpenPullRequestsExceededException"
{-# DEPRECATED _MaximumOpenPullRequestsExceededException "Use generic-lens or generic-optics instead." #-}

-- | You cannot create an approval rule template with that name because a template with that name already exists in this AWS Region for your AWS account. Approval rule template names must be unique.
_ApprovalRuleTemplateNameAlreadyExistsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameAlreadyExistsException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleTemplateNameAlreadyExistsException"
{-# DEPRECATED _ApprovalRuleTemplateNameAlreadyExistsException "Use generic-lens or generic-optics instead." #-}

-- | The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.
_InvalidTargetException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTargetException =
  Core._MatchServiceError mkServiceConfig "InvalidTargetException"
{-# DEPRECATED _InvalidTargetException "Use generic-lens or generic-optics instead." #-}

-- | The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.
_InvalidPullRequestIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPullRequestIdException"
{-# DEPRECATED _InvalidPullRequestIdException "Use generic-lens or generic-optics instead." #-}

-- | You cannot modify or delete this comment. Only comment authors can modify or delete their comments.
_CommentNotCreatedByCallerException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentNotCreatedByCallerException =
  Core._MatchServiceError
    mkServiceConfig
    "CommentNotCreatedByCallerException"
{-# DEPRECATED _CommentNotCreatedByCallerException "Use generic-lens or generic-optics instead." #-}

-- | The pull request status update is not valid. The only valid update is from @OPEN@ to @CLOSED@ .
_InvalidPullRequestStatusUpdateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidPullRequestStatusUpdateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidPullRequestStatusUpdateException"
{-# DEPRECATED _InvalidPullRequestStatusUpdateException "Use generic-lens or generic-optics instead." #-}

-- | The specified reference name format is not valid. Reference names must conform to the Git references format (for example, refs/heads/master). For more information, see <https://git-scm.com/book/en/v2/Git-Internals-Git-References Git Internals - Git References> or consult your Git documentation.
_InvalidReferenceNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReferenceNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReferenceNameException"
{-# DEPRECATED _InvalidReferenceNameException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of approval rule templates for a repository has been exceeded. You cannot associate more than 25 approval rule templates with a repository.
_MaximumRuleTemplatesAssociatedWithRepositoryException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumRuleTemplatesAssociatedWithRepositoryException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumRuleTemplatesAssociatedWithRepositoryException"
{-# DEPRECATED _MaximumRuleTemplatesAssociatedWithRepositoryException "Use generic-lens or generic-optics instead." #-}

-- | The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.
_SameFileContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_SameFileContentException =
  Core._MatchServiceError
    mkServiceConfig
    "SameFileContentException"
{-# DEPRECATED _SameFileContentException "Use generic-lens or generic-optics instead." #-}

-- | The approval rule template is associated with one or more repositories. You cannot delete a template that is associated with a repository. Remove all associations, and then try again.
_ApprovalRuleTemplateInUseException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateInUseException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleTemplateInUseException"
{-# DEPRECATED _ApprovalRuleTemplateInUseException "Use generic-lens or generic-optics instead." #-}

-- | The number of approvals required for the approval rule exceeds the maximum number allowed.
_MaximumNumberOfApprovalsExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumNumberOfApprovalsExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumNumberOfApprovalsExceededException"
{-# DEPRECATED _MaximumNumberOfApprovalsExceededException "Use generic-lens or generic-optics instead." #-}

-- | A commit ID was not specified.
_CommitIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitIdRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitIdRequiredException"
{-# DEPRECATED _CommitIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified file does not exist. Verify that you have used the correct file name, full path, and extension.
_FileDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "FileDoesNotExistException"
{-# DEPRECATED _FileDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The specified commit ID is not valid.
_InvalidCommitIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidCommitIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidCommitIdException"
{-# DEPRECATED _InvalidCommitIdException "Use generic-lens or generic-optics instead." #-}

-- | The list of tags is not valid.
_InvalidTagKeysListException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidTagKeysListException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidTagKeysListException"
{-# DEPRECATED _InvalidTagKeysListException "Use generic-lens or generic-optics instead." #-}

-- | The commit cannot be created because both a source file and file content have been specified for the same file. You cannot provide both. Either specify a source file or provide the file content directly.
_FileContentAndSourceFileSpecifiedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileContentAndSourceFileSpecifiedException =
  Core._MatchServiceError
    mkServiceConfig
    "FileContentAndSourceFileSpecifiedException"
{-# DEPRECATED _FileContentAndSourceFileSpecifiedException "Use generic-lens or generic-optics instead." #-}

-- | The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.
_TipOfSourceReferenceIsDifferentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TipOfSourceReferenceIsDifferentException =
  Core._MatchServiceError
    mkServiceConfig
    "TipOfSourceReferenceIsDifferentException"
{-# DEPRECATED _TipOfSourceReferenceIsDifferentException "Use generic-lens or generic-optics instead." #-}

-- | A destination ARN for the target service for the trigger is required, but was not specified.
_RepositoryTriggerDestinationArnRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerDestinationArnRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryTriggerDestinationArnRequiredException"
{-# DEPRECATED _RepositoryTriggerDestinationArnRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified conflict resolution strategy is not valid.
_InvalidConflictResolutionStrategyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionStrategyException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidConflictResolutionStrategyException"
{-# DEPRECATED _InvalidConflictResolutionStrategyException "Use generic-lens or generic-optics instead." #-}

-- | The client request token is not valid.
_InvalidClientRequestTokenException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidClientRequestTokenException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidClientRequestTokenException"
{-# DEPRECATED _InvalidClientRequestTokenException "Use generic-lens or generic-optics instead." #-}

-- | More than one conflict resolution entries exists for the conflict. A conflict can have only one conflict resolution entry.
_MultipleConflictResolutionEntriesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MultipleConflictResolutionEntriesException =
  Core._MatchServiceError
    mkServiceConfig
    "MultipleConflictResolutionEntriesException"
{-# DEPRECATED _MultipleConflictResolutionEntriesException "Use generic-lens or generic-optics instead." #-}

-- | The specified commit does not exist or no commit was specified, and the specified repository has no default branch.
_CommitDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitDoesNotExistException"
{-# DEPRECATED _CommitDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | At least one branch name is required, but was not specified in the trigger configuration.
_RepositoryTriggerBranchNameListRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryTriggerBranchNameListRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryTriggerBranchNameListRequiredException"
{-# DEPRECATED _RepositoryTriggerBranchNameListRequiredException "Use generic-lens or generic-optics instead." #-}

-- | A client request token is required. A client request token is an unique, client-generated idempotency token that, when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request returns information about the initial request that used that token.
_ClientRequestTokenRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ClientRequestTokenRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ClientRequestTokenRequiredException"
{-# DEPRECATED _ClientRequestTokenRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified approval rule template does not exist. Verify that the name is correct and that you are signed in to the AWS Region where the template was created, and then try again.
_ApprovalRuleTemplateDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleTemplateDoesNotExistException"
{-# DEPRECATED _ApprovalRuleTemplateDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The tag policy is not valid.
_TagPolicyException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError mkServiceConfig "TagPolicyException"
{-# DEPRECATED _TagPolicyException "Use generic-lens or generic-optics instead." #-}

-- | The specified merge option is not valid for this operation. Not all merge strategies are supported for all operations.
_InvalidMergeOptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMergeOptionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidMergeOptionException"
{-# DEPRECATED _InvalidMergeOptionException "Use generic-lens or generic-optics instead." #-}

-- | The approval rule cannot be deleted from the pull request because it was created by an approval rule template and applied to the pull request automatically.
_CannotDeleteApprovalRuleFromTemplateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CannotDeleteApprovalRuleFromTemplateException =
  Core._MatchServiceError
    mkServiceConfig
    "CannotDeleteApprovalRuleFromTemplateException"
{-# DEPRECATED _CannotDeleteApprovalRuleFromTemplateException "Use generic-lens or generic-optics instead." #-}

-- | The comment ID is missing or null. A comment ID is required.
_CommentIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentIdRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "CommentIdRequiredException"
{-# DEPRECATED _CommentIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified number of maximum results is not valid.
_InvalidMaxResultsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMaxResultsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidMaxResultsException"
{-# DEPRECATED _InvalidMaxResultsException "Use generic-lens or generic-optics instead." #-}

-- | The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide> .
_FileTooLargeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileTooLargeException =
  Core._MatchServiceError mkServiceConfig "FileTooLargeException"
{-# DEPRECATED _FileTooLargeException "Use generic-lens or generic-optics instead." #-}

-- | An approval rule template name is required, but was not specified.
_ApprovalRuleTemplateNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleTemplateNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleTemplateNameRequiredException"
{-# DEPRECATED _ApprovalRuleTemplateNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The number of specified files to change as part of this commit exceeds the maximum number of files that can be changed in a single commit. Consider using a Git client for these changes.
_MaximumFileEntriesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumFileEntriesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumFileEntriesExceededException"
{-# DEPRECATED _MaximumFileEntriesExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified commit ID does not exist.
_CommitIdDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitIdDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitIdDoesNotExistException"
{-# DEPRECATED _CommitIdDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | Automerge was specified for resolving the conflict, but the specified replacement type is not valid.
_InvalidReplacementTypeException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidReplacementTypeException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidReplacementTypeException"
{-# DEPRECATED _InvalidReplacementTypeException "Use generic-lens or generic-optics instead." #-}

-- | The revision ID is not valid. Use GetPullRequest to determine the value.
_InvalidRevisionIdException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRevisionIdException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRevisionIdException"
{-# DEPRECATED _InvalidRevisionIdException "Use generic-lens or generic-optics instead." #-}

-- | The revision ID provided in the request does not match the current revision ID. Use GetPullRequest to retrieve the current revision ID.
_RevisionNotCurrentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RevisionNotCurrentException =
  Core._MatchServiceError
    mkServiceConfig
    "RevisionNotCurrentException"
{-# DEPRECATED _RevisionNotCurrentException "Use generic-lens or generic-optics instead." #-}

-- | The name of the approval rule template is not valid. Template names must be between 1 and 100 valid characters in length. For more information about limits in AWS CodeCommit, see <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide> .
_InvalidApprovalRuleTemplateNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalRuleTemplateNameException"
{-# DEPRECATED _InvalidApprovalRuleTemplateNameException "Use generic-lens or generic-optics instead." #-}

-- | The approval cannot be applied because the user approving the pull request matches the user who created the pull request. You cannot approve a pull request that you created.
_PullRequestCannotBeApprovedByAuthorException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestCannotBeApprovedByAuthorException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestCannotBeApprovedByAuthorException"
{-# DEPRECATED _PullRequestCannotBeApprovedByAuthorException "Use generic-lens or generic-optics instead." #-}

-- | You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.
_MultipleRepositoriesInPullRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MultipleRepositoriesInPullRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "MultipleRepositoriesInPullRequestException"
{-# DEPRECATED _MultipleRepositoriesInPullRequestException "Use generic-lens or generic-optics instead." #-}

-- | A revision ID is required, but was not provided.
_RevisionIdRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RevisionIdRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RevisionIdRequiredException"
{-# DEPRECATED _RevisionIdRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The file cannot be added because it is too large. The maximum file size is 6 MB, and the combined file content change size is 7 MB. Consider making these changes using a Git client.
_FileContentSizeLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_FileContentSizeLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "FileContentSizeLimitExceededException"
{-# DEPRECATED _FileContentSizeLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The name of the trigger is not valid.
_InvalidRepositoryTriggerNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerNameException"
{-# DEPRECATED _InvalidRepositoryTriggerNameException "Use generic-lens or generic-optics instead." #-}

-- | A repository name is required, but was not specified.
_RepositoryNameRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNameRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryNameRequiredException"
{-# DEPRECATED _RepositoryNameRequiredException "Use generic-lens or generic-optics instead." #-}

-- | A repository resource limit was exceeded.
_RepositoryLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryLimitExceededException"
{-# DEPRECATED _RepositoryLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | A map of tags is required.
_TagsMapRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TagsMapRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "TagsMapRequiredException"
{-# DEPRECATED _TagsMapRequiredException "Use generic-lens or generic-optics instead." #-}

-- | One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.
_InvalidRepositoryTriggerEventsException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryTriggerEventsException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryTriggerEventsException"
{-# DEPRECATED _InvalidRepositoryTriggerEventsException "Use generic-lens or generic-optics instead." #-}

-- | The approval rule cannot be added. The pull request has the maximum number of approval rules associated with it.
_NumberOfRulesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_NumberOfRulesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "NumberOfRulesExceededException"
{-# DEPRECATED _NumberOfRulesExceededException "Use generic-lens or generic-optics instead." #-}

-- | The specified branch name is not valid because it is a tag name. Enter the name of a branch in the repository. For a list of valid branch names, use 'ListBranches' .
_BranchNameIsTagNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_BranchNameIsTagNameException =
  Core._MatchServiceError
    mkServiceConfig
    "BranchNameIsTagNameException"
{-# DEPRECATED _BranchNameIsTagNameException "Use generic-lens or generic-optics instead." #-}

-- | A specified repository name is not valid.
_InvalidRepositoryNameException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidRepositoryNameException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidRepositoryNameException"
{-# DEPRECATED _InvalidRepositoryNameException "Use generic-lens or generic-optics instead." #-}

-- | A list of commit IDs is required, but was either not specified or the list was empty.
_CommitIdsListRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitIdsListRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitIdsListRequiredException"
{-# DEPRECATED _CommitIdsListRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The maximum number of allowed commit IDs in a batch request is 100. Verify that your batch requests contains no more than 100 commit IDs, and then try again.
_CommitIdsLimitExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommitIdsLimitExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "CommitIdsLimitExceededException"
{-# DEPRECATED _CommitIdsLimitExceededException "Use generic-lens or generic-optics instead." #-}

-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.
_InvalidAuthorArnException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidAuthorArnException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidAuthorArnException"
{-# DEPRECATED _InvalidAuthorArnException "Use generic-lens or generic-optics instead." #-}

-- | The number of items to compare between the source or destination branches and the merge base has exceeded the maximum allowed.
_MaximumItemsToCompareExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumItemsToCompareExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumItemsToCompareExceededException"
{-# DEPRECATED _MaximumItemsToCompareExceededException "Use generic-lens or generic-optics instead." #-}

-- | An override status is required, but no value was provided. Valid values include OVERRIDE and REVOKE.
_OverrideStatusRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_OverrideStatusRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "OverrideStatusRequiredException"
{-# DEPRECATED _OverrideStatusRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The content for the approval rule is empty. You must provide some content for an approval rule. The content cannot be null.
_ApprovalRuleContentRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ApprovalRuleContentRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ApprovalRuleContentRequiredException"
{-# DEPRECATED _ApprovalRuleContentRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The number of allowed conflict resolution entries was exceeded.
_MaximumConflictResolutionEntriesExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumConflictResolutionEntriesExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumConflictResolutionEntriesExceededException"
{-# DEPRECATED _MaximumConflictResolutionEntriesExceededException "Use generic-lens or generic-optics instead." #-}

-- | A pull request status is required, but none was provided.
_PullRequestStatusRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_PullRequestStatusRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "PullRequestStatusRequiredException"
{-# DEPRECATED _PullRequestStatusRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified conflict resolution list is not valid.
_InvalidConflictResolutionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidConflictResolutionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidConflictResolutionException"
{-# DEPRECATED _InvalidConflictResolutionException "Use generic-lens or generic-optics instead." #-}

-- | The content of the approval rule template is not valid.
_InvalidApprovalRuleTemplateContentException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateContentException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalRuleTemplateContentException"
{-# DEPRECATED _InvalidApprovalRuleTemplateContentException "Use generic-lens or generic-optics instead." #-}

-- | The state for the approval is not valid. Valid values include APPROVE and REVOKE.
_InvalidApprovalStateException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalStateException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalStateException"
{-# DEPRECATED _InvalidApprovalStateException "Use generic-lens or generic-optics instead." #-}

-- | The repository does not contain any pull requests with that pull request ID. Use GetPullRequest to verify the correct repository name for the pull request ID.
_RepositoryNotAssociatedWithPullRequestException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_RepositoryNotAssociatedWithPullRequestException =
  Core._MatchServiceError
    mkServiceConfig
    "RepositoryNotAssociatedWithPullRequestException"
{-# DEPRECATED _RepositoryNotAssociatedWithPullRequestException "Use generic-lens or generic-optics instead." #-}

-- | The number of files to load exceeds the allowed limit.
_MaximumFileContentToLoadExceededException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_MaximumFileContentToLoadExceededException =
  Core._MatchServiceError
    mkServiceConfig
    "MaximumFileContentToLoadExceededException"
{-# DEPRECATED _MaximumFileContentToLoadExceededException "Use generic-lens or generic-optics instead." #-}

-- | A pull request title is required. It cannot be empty or null.
_TitleRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_TitleRequiredException =
  Core._MatchServiceError mkServiceConfig "TitleRequiredException"
{-# DEPRECATED _TitleRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The override status is not valid. Valid statuses are OVERRIDE and REVOKE.
_InvalidOverrideStatusException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidOverrideStatusException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidOverrideStatusException"
{-# DEPRECATED _InvalidOverrideStatusException "Use generic-lens or generic-optics instead." #-}

-- | The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.
_InvalidFilePositionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidFilePositionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidFilePositionException"
{-# DEPRECATED _InvalidFilePositionException "Use generic-lens or generic-optics instead." #-}

-- | This comment has already been deleted. You cannot edit or delete a deleted comment.
_CommentDeletedException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_CommentDeletedException =
  Core._MatchServiceError mkServiceConfig "CommentDeletedException"
{-# DEPRECATED _CommentDeletedException "Use generic-lens or generic-optics instead." #-}

-- | The parent commit ID is not valid because it does not exist. The specified parent commit ID does not exist in the specified branch of the repository.
_ParentCommitDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ParentCommitDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "ParentCommitDoesNotExistException"
{-# DEPRECATED _ParentCommitDoesNotExistException "Use generic-lens or generic-optics instead." #-}

-- | The description for the approval rule template is not valid because it exceeds the maximum characters allowed for a description. For more information about limits in AWS CodeCommit, see <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide> .
_InvalidApprovalRuleTemplateDescriptionException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidApprovalRuleTemplateDescriptionException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidApprovalRuleTemplateDescriptionException"
{-# DEPRECATED _InvalidApprovalRuleTemplateDescriptionException "Use generic-lens or generic-optics instead." #-}

-- | A valid Amazon Resource Name (ARN) for an AWS CodeCommit resource is required. For a list of valid resources in AWS CodeCommit, see <https://docs.aws.amazon.com/codecommit/latest/userguide/auth-and-access-control-iam-access-control-identity-based.html#arn-formats CodeCommit Resources and Operations> in the AWS CodeCommit User Guide.
_ResourceArnRequiredException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_ResourceArnRequiredException =
  Core._MatchServiceError
    mkServiceConfig
    "ResourceArnRequiredException"
{-# DEPRECATED _ResourceArnRequiredException "Use generic-lens or generic-optics instead." #-}

-- | The specified value for the number of conflict files to return is not valid.
_InvalidMaxConflictFilesException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_InvalidMaxConflictFilesException =
  Core._MatchServiceError
    mkServiceConfig
    "InvalidMaxConflictFilesException"
{-# DEPRECATED _InvalidMaxConflictFilesException "Use generic-lens or generic-optics instead." #-}

-- | The specified Amazon Resource Name (ARN) does not exist in the AWS account.
_AuthorDoesNotExistException :: Core.AsError a => Lens.Getting (Core.First Core.ServiceError) a Core.ServiceError
_AuthorDoesNotExistException =
  Core._MatchServiceError
    mkServiceConfig
    "AuthorDoesNotExistException"
{-# DEPRECATED _AuthorDoesNotExistException "Use generic-lens or generic-optics instead." #-}
