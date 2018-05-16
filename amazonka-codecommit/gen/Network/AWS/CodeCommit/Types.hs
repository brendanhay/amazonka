{-# LANGUAGE OverloadedStrings #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types
    (
    -- * Service Configuration
      codeCommit

    -- * Errors
    , _InvalidRepositoryTriggerRegionException
    , _InvalidContinuationTokenException
    , _ManualMergeRequiredException
    , _TargetsRequiredException
    , _EncryptionKeyNotFoundException
    , _TipsDivergenceExceededException
    , _InvalidRepositoryTriggerBranchNameException
    , _PullRequestAlreadyClosedException
    , _InvalidRepositoryTriggerCustomDataException
    , _DirectoryNameConflictsWithFileNameException
    , _ReferenceDoesNotExistException
    , _ActorDoesNotExistException
    , _PullRequestIdRequiredException
    , _InvalidEmailException
    , _CommitMessageLengthExceededException
    , _BlobIdDoesNotExistException
    , _MaximumRepositoryNamesExceededException
    , _InvalidRepositoryDescriptionException
    , _RepositoryNameExistsException
    , _ReferenceNameRequiredException
    , _MaximumRepositoryTriggersExceededException
    , _InvalidBranchNameException
    , _BranchNameRequiredException
    , _MergeOptionRequiredException
    , _InvalidFileLocationException
    , _BeforeCommitIdAndAfterCommitIdAreSameException
    , _RepositoryTriggersListRequiredException
    , _IdempotencyParameterMismatchException
    , _EncryptionKeyUnavailableException
    , _InvalidRelativeFileVersionEnumException
    , _InvalidRepositoryTriggerDestinationARNException
    , _BlobIdRequiredException
    , _RepositoryNamesRequiredException
    , _InvalidActorARNException
    , _InvalidCommentIdException
    , _InvalidDescriptionException
    , _InvalidBlobIdException
    , _PullRequestDoesNotExistException
    , _InvalidOrderException
    , _BranchDoesNotExistException
    , _DefaultBranchCannotBeDeletedException
    , _InvalidPathException
    , _PathRequiredException
    , _RepositoryTriggerNameRequiredException
    , _InvalidFileModeException
    , _InvalidPullRequestStatusException
    , _ParentCommitIdRequiredException
    , _InvalidSourceCommitSpecifierException
    , _RepositoryDoesNotExistException
    , _MaximumBranchesExceededException
    , _InvalidTitleException
    , _CommentContentSizeLimitExceededException
    , _InvalidParentCommitIdException
    , _InvalidPullRequestEventTypeException
    , _FileContentRequiredException
    , _SourceAndDestinationAreSameException
    , _PathDoesNotExistException
    , _EncryptionIntegrityChecksFailedException
    , _ParentCommitIdOutdatedException
    , _RepositoryTriggerEventsListRequiredException
    , _CommentContentRequiredException
    , _InvalidTargetsException
    , _EncryptionKeyAccessDeniedException
    , _BranchNameExistsException
    , _InvalidCommitException
    , _TargetRequiredException
    , _InvalidDestinationCommitSpecifierException
    , _CommentDoesNotExistException
    , _ReferenceTypeNotSupportedException
    , _FileNameConflictsWithDirectoryNameException
    , _NameLengthExceededException
    , _InvalidSortByException
    , _EncryptionKeyDisabledException
    , _CommitRequiredException
    , _MaximumOpenPullRequestsExceededException
    , _InvalidTargetException
    , _InvalidPullRequestIdException
    , _CommentNotCreatedByCallerException
    , _InvalidPullRequestStatusUpdateException
    , _InvalidReferenceNameException
    , _SameFileContentException
    , _CommitIdRequiredException
    , _InvalidCommitIdException
    , _TipOfSourceReferenceIsDifferentException
    , _RepositoryTriggerDestinationARNRequiredException
    , _InvalidClientRequestTokenException
    , _CommitDoesNotExistException
    , _RepositoryTriggerBranchNameListRequiredException
    , _ClientRequestTokenRequiredException
    , _InvalidMergeOptionException
    , _CommentIdRequiredException
    , _InvalidMaxResultsException
    , _FileTooLargeException
    , _CommitIdDoesNotExistException
    , _MultipleRepositoriesInPullRequestException
    , _FileContentSizeLimitExceededException
    , _InvalidRepositoryTriggerNameException
    , _RepositoryNameRequiredException
    , _RepositoryLimitExceededException
    , _InvalidRepositoryTriggerEventsException
    , _BranchNameIsTagNameException
    , _InvalidRepositoryNameException
    , _InvalidAuthorARNException
    , _PullRequestStatusRequiredException
    , _RepositoryNotAssociatedWithPullRequestException
    , _TitleRequiredException
    , _InvalidFilePositionException
    , _CommentDeletedException
    , _ParentCommitDoesNotExistException
    , _AuthorDoesNotExistException

    -- * ChangeTypeEnum
    , ChangeTypeEnum (..)

    -- * FileModeTypeEnum
    , FileModeTypeEnum (..)

    -- * MergeOptionTypeEnum
    , MergeOptionTypeEnum (..)

    -- * OrderEnum
    , OrderEnum (..)

    -- * PullRequestEventType
    , PullRequestEventType (..)

    -- * PullRequestStatusEnum
    , PullRequestStatusEnum (..)

    -- * RelativeFileVersionEnum
    , RelativeFileVersionEnum (..)

    -- * RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- * SortByEnum
    , SortByEnum (..)

    -- * BlobMetadata
    , BlobMetadata
    , blobMetadata
    , bmPath
    , bmMode
    , bmBlobId

    -- * BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- * Comment
    , Comment
    , comment
    , cLastModifiedDate
    , cAuthorARN
    , cContent
    , cCreationDate
    , cDeleted
    , cClientRequestToken
    , cCommentId
    , cInReplyTo

    -- * CommentsForComparedCommit
    , CommentsForComparedCommit
    , commentsForComparedCommit
    , cfccBeforeBlobId
    , cfccLocation
    , cfccAfterCommitId
    , cfccAfterBlobId
    , cfccBeforeCommitId
    , cfccRepositoryName
    , cfccComments

    -- * CommentsForPullRequest
    , CommentsForPullRequest
    , commentsForPullRequest
    , cfprBeforeBlobId
    , cfprLocation
    , cfprAfterCommitId
    , cfprPullRequestId
    , cfprAfterBlobId
    , cfprBeforeCommitId
    , cfprRepositoryName
    , cfprComments

    -- * Commit
    , Commit
    , commit
    , cCommitId
    , cCommitter
    , cTreeId
    , cAdditionalData
    , cParents
    , cAuthor
    , cMessage

    -- * Difference
    , Difference
    , difference
    , dAfterBlob
    , dBeforeBlob
    , dChangeType

    -- * Location
    , Location
    , location
    , lRelativeFileVersion
    , lFilePath
    , lFilePosition

    -- * MergeMetadata
    , MergeMetadata
    , mergeMetadata
    , mmMergedBy
    , mmIsMerged

    -- * PullRequest
    , PullRequest
    , pullRequest
    , prAuthorARN
    , prPullRequestId
    , prCreationDate
    , prPullRequestStatus
    , prTitle
    , prClientRequestToken
    , prLastActivityDate
    , prPullRequestTargets
    , prDescription

    -- * PullRequestEvent
    , PullRequestEvent
    , pullRequestEvent
    , prePullRequestMergedStateChangedEventMetadata
    , prePullRequestEventType
    , prePullRequestStatusChangedEventMetadata
    , preActorARN
    , prePullRequestId
    , preEventDate
    , prePullRequestSourceReferenceUpdatedEventMetadata

    -- * PullRequestMergedStateChangedEventMetadata
    , PullRequestMergedStateChangedEventMetadata
    , pullRequestMergedStateChangedEventMetadata
    , prmscemDestinationReference
    , prmscemMergeMetadata
    , prmscemRepositoryName

    -- * PullRequestSourceReferenceUpdatedEventMetadata
    , PullRequestSourceReferenceUpdatedEventMetadata
    , pullRequestSourceReferenceUpdatedEventMetadata
    , prsruemAfterCommitId
    , prsruemBeforeCommitId
    , prsruemRepositoryName

    -- * PullRequestStatusChangedEventMetadata
    , PullRequestStatusChangedEventMetadata
    , pullRequestStatusChangedEventMetadata
    , prscemPullRequestStatus

    -- * PullRequestTarget
    , PullRequestTarget
    , pullRequestTarget
    , prtSourceCommit
    , prtDestinationReference
    , prtMergeMetadata
    , prtDestinationCommit
    , prtRepositoryName
    , prtSourceReference

    -- * RepositoryMetadata
    , RepositoryMetadata
    , repositoryMetadata
    , rmRepositoryDescription
    , rmLastModifiedDate
    , rmARN
    , rmCloneURLHTTP
    , rmAccountId
    , rmDefaultBranch
    , rmRepositoryId
    , rmRepositoryName
    , rmCreationDate
    , rmCloneURLSSH

    -- * RepositoryNameIdPair
    , RepositoryNameIdPair
    , repositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName

    -- * RepositoryTrigger
    , RepositoryTrigger
    , repositoryTrigger
    , rtBranches
    , rtCustomData
    , rtName
    , rtDestinationARN
    , rtEvents

    -- * RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure
    , repositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- * Target
    , Target
    , target
    , tDestinationReference
    , tRepositoryName
    , tSourceReference

    -- * UserInfo
    , UserInfo
    , userInfo
    , uiEmail
    , uiDate
    , uiName
    ) where

import Network.AWS.CodeCommit.Types.Product
import Network.AWS.CodeCommit.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | API version @2015-04-13@ of the Amazon CodeCommit SDK configuration.
codeCommit :: Service
codeCommit =
  Service
    { _svcAbbrev = "CodeCommit"
    , _svcSigner = v4
    , _svcPrefix = "codecommit"
    , _svcVersion = "2015-04-13"
    , _svcEndpoint = defaultEndpoint codeCommit
    , _svcTimeout = Just 70
    , _svcCheck = statusSuccess
    , _svcError = parseJSONError "CodeCommit"
    , _svcRetry = retry
    }
  where
    retry =
      Exponential
        { _retryBase = 5.0e-2
        , _retryGrowth = 2
        , _retryAttempts = 5
        , _retryCheck = check
        }
    check e
      | has (hasCode "ThrottledException" . hasStatus 400) e =
        Just "throttled_exception"
      | has (hasStatus 429) e = Just "too_many_requests"
      | has (hasCode "ThrottlingException" . hasStatus 400) e =
        Just "throttling_exception"
      | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
      | has (hasStatus 504) e = Just "gateway_timeout"
      | has (hasCode "RequestThrottledException" . hasStatus 400) e =
        Just "request_throttled_exception"
      | has (hasStatus 502) e = Just "bad_gateway"
      | has (hasStatus 503) e = Just "service_unavailable"
      | has (hasStatus 500) e = Just "general_server_error"
      | has (hasStatus 509) e = Just "limit_exceeded"
      | otherwise = Nothing


-- | The region for the trigger target does not match the region for the repository. Triggers must be created in the same region as the target for the trigger.
--
--
_InvalidRepositoryTriggerRegionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerRegionException =
  _MatchServiceError codeCommit "InvalidRepositoryTriggerRegionException"


-- | The specified continuation token is not valid.
--
--
_InvalidContinuationTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidContinuationTokenException =
  _MatchServiceError codeCommit "InvalidContinuationTokenException"


-- | The pull request cannot be merged automatically into the destination branch. You must manually merge the branches and resolve any conflicts.
--
--
_ManualMergeRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ManualMergeRequiredException =
  _MatchServiceError codeCommit "ManualMergeRequiredException"


-- | An array of target objects is required. It cannot be empty or null.
--
--
_TargetsRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetsRequiredException =
  _MatchServiceError codeCommit "TargetsRequiredException"


-- | No encryption key was found.
--
--
_EncryptionKeyNotFoundException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyNotFoundException =
  _MatchServiceError codeCommit "EncryptionKeyNotFoundException"


-- | The divergence between the tips of the provided commit specifiers is too great to determine whether there might be any merge conflicts. Locally compare the specifiers using @git diff@ or a diff tool.
--
--
_TipsDivergenceExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_TipsDivergenceExceededException =
  _MatchServiceError codeCommit "TipsDivergenceExceededException"


-- | One or more branch names specified for the trigger is not valid.
--
--
_InvalidRepositoryTriggerBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerBranchNameException =
  _MatchServiceError codeCommit "InvalidRepositoryTriggerBranchNameException"


-- | The pull request status cannot be updated because it is already closed.
--
--
_PullRequestAlreadyClosedException :: AsError a => Getting (First ServiceError) a ServiceError
_PullRequestAlreadyClosedException =
  _MatchServiceError codeCommit "PullRequestAlreadyClosedException"


-- | The custom data provided for the trigger is not valid.
--
--
_InvalidRepositoryTriggerCustomDataException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerCustomDataException =
  _MatchServiceError codeCommit "InvalidRepositoryTriggerCustomDataException"


-- | A file cannot be added to the repository because the specified path name has the same name as a file that already exists in this repository. Either provide a different name for the file, or specify a different path for the file.
--
--
_DirectoryNameConflictsWithFileNameException :: AsError a => Getting (First ServiceError) a ServiceError
_DirectoryNameConflictsWithFileNameException =
  _MatchServiceError codeCommit "DirectoryNameConflictsWithFileNameException"


-- | The specified reference does not exist. You must provide a full commit ID.
--
--
_ReferenceDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ReferenceDoesNotExistException =
  _MatchServiceError codeCommit "ReferenceDoesNotExistException"


-- | The specified Amazon Resource Name (ARN) does not exist in the AWS account.
--
--
_ActorDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ActorDoesNotExistException =
  _MatchServiceError codeCommit "ActorDoesNotExistException"


-- | A pull request ID is required, but none was provided.
--
--
_PullRequestIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_PullRequestIdRequiredException =
  _MatchServiceError codeCommit "PullRequestIdRequiredException"


-- | The specified email address either contains one or more characters that are not allowed, or it exceeds the maximum number of characters allowed for an email address.
--
--
_InvalidEmailException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidEmailException = _MatchServiceError codeCommit "InvalidEmailException"


-- | The commit message is too long. Provide a shorter string.
--
--
_CommitMessageLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitMessageLengthExceededException =
  _MatchServiceError codeCommit "CommitMessageLengthExceededException"


-- | The specified blob does not exist.
--
--
_BlobIdDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BlobIdDoesNotExistException =
  _MatchServiceError codeCommit "BlobIdDoesNotExistException"


-- | The maximum number of allowed repository names was exceeded. Currently, this number is 25.
--
--
_MaximumRepositoryNamesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryNamesExceededException =
  _MatchServiceError codeCommit "MaximumRepositoryNamesExceededException"


-- | The specified repository description is not valid.
--
--
_InvalidRepositoryDescriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryDescriptionException =
  _MatchServiceError codeCommit "InvalidRepositoryDescriptionException"


-- | The specified repository name already exists.
--
--
_RepositoryNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameExistsException =
  _MatchServiceError codeCommit "RepositoryNameExistsException"


-- | A reference name is required, but none was provided.
--
--
_ReferenceNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ReferenceNameRequiredException =
  _MatchServiceError codeCommit "ReferenceNameRequiredException"


-- | The number of triggers allowed for the repository was exceeded.
--
--
_MaximumRepositoryTriggersExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumRepositoryTriggersExceededException =
  _MatchServiceError codeCommit "MaximumRepositoryTriggersExceededException"


-- | The specified reference name is not valid.
--
--
_InvalidBranchNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBranchNameException =
  _MatchServiceError codeCommit "InvalidBranchNameException"


-- | A branch name is required but was not specified.
--
--
_BranchNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameRequiredException =
  _MatchServiceError codeCommit "BranchNameRequiredException"


-- | A merge option or stategy is required, and none was provided.
--
--
_MergeOptionRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_MergeOptionRequiredException =
  _MatchServiceError codeCommit "MergeOptionRequiredException"


-- | The location of the file is not valid. Make sure that you include the extension of the file as well as the file name.
--
--
_InvalidFileLocationException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFileLocationException =
  _MatchServiceError codeCommit "InvalidFileLocationException"


-- | The before commit ID and the after commit ID are the same, which is not valid. The before commit ID and the after commit ID must be different commit IDs.
--
--
_BeforeCommitIdAndAfterCommitIdAreSameException :: AsError a => Getting (First ServiceError) a ServiceError
_BeforeCommitIdAndAfterCommitIdAreSameException =
  _MatchServiceError codeCommit "BeforeCommitIdAndAfterCommitIdAreSameException"


-- | The list of triggers for the repository is required but was not specified.
--
--
_RepositoryTriggersListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggersListRequiredException =
  _MatchServiceError codeCommit "RepositoryTriggersListRequiredException"


-- | The client request token is not valid. Either the token is not in a valid format, or the token has been used in a previous request and cannot be re-used.
--
--
_IdempotencyParameterMismatchException :: AsError a => Getting (First ServiceError) a ServiceError
_IdempotencyParameterMismatchException =
  _MatchServiceError codeCommit "IdempotencyParameterMismatchException"


-- | The encryption key is not available.
--
--
_EncryptionKeyUnavailableException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyUnavailableException =
  _MatchServiceError codeCommit "EncryptionKeyUnavailableException"


-- | Either the enum is not in a valid format, or the specified file version enum is not valid in respect to the current file version.
--
--
_InvalidRelativeFileVersionEnumException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRelativeFileVersionEnumException =
  _MatchServiceError codeCommit "InvalidRelativeFileVersionEnumException"


-- | The Amazon Resource Name (ARN) for the trigger is not valid for the specified destination. The most common reason for this error is that the ARN does not meet the requirements for the service type.
--
--
_InvalidRepositoryTriggerDestinationARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerDestinationARNException =
  _MatchServiceError
    codeCommit
    "InvalidRepositoryTriggerDestinationArnException"


-- | A blob ID is required but was not specified.
--
--
_BlobIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_BlobIdRequiredException =
  _MatchServiceError codeCommit "BlobIdRequiredException"


-- | A repository names object is required but was not specified.
--
--
_RepositoryNamesRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNamesRequiredException =
  _MatchServiceError codeCommit "RepositoryNamesRequiredException"


-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the user who initiated the change for the pull request, and then try again.
--
--
_InvalidActorARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidActorARNException =
  _MatchServiceError codeCommit "InvalidActorArnException"


-- | The comment ID is not in a valid format. Make sure that you have provided the full comment ID.
--
--
_InvalidCommentIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommentIdException =
  _MatchServiceError codeCommit "InvalidCommentIdException"


-- | The pull request description is not valid. Descriptions are limited to 1,000 characters in length.
--
--
_InvalidDescriptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDescriptionException =
  _MatchServiceError codeCommit "InvalidDescriptionException"


-- | The specified blob is not valid.
--
--
_InvalidBlobIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidBlobIdException = _MatchServiceError codeCommit "InvalidBlobIdException"


-- | The pull request ID could not be found. Make sure that you have specified the correct repository name and pull request ID, and then try again.
--
--
_PullRequestDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_PullRequestDoesNotExistException =
  _MatchServiceError codeCommit "PullRequestDoesNotExistException"


-- | The specified sort order is not valid.
--
--
_InvalidOrderException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidOrderException = _MatchServiceError codeCommit "InvalidOrderException"


-- | The specified branch does not exist.
--
--
_BranchDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchDoesNotExistException =
  _MatchServiceError codeCommit "BranchDoesNotExistException"


-- | The specified branch is the default branch for the repository, and cannot be deleted. To delete this branch, you must first set another branch as the default branch.
--
--
_DefaultBranchCannotBeDeletedException :: AsError a => Getting (First ServiceError) a ServiceError
_DefaultBranchCannotBeDeletedException =
  _MatchServiceError codeCommit "DefaultBranchCannotBeDeletedException"


-- | The specified path is not valid.
--
--
_InvalidPathException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPathException = _MatchServiceError codeCommit "InvalidPathException"


-- | The filePath for a location cannot be empty or null.
--
--
_PathRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_PathRequiredException = _MatchServiceError codeCommit "PathRequiredException"


-- | A name for the trigger is required but was not specified.
--
--
_RepositoryTriggerNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerNameRequiredException =
  _MatchServiceError codeCommit "RepositoryTriggerNameRequiredException"


-- | The specified file mode permission is not valid. For a list of valid file mode permissions, see 'PutFile' .
--
--
_InvalidFileModeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFileModeException =
  _MatchServiceError codeCommit "InvalidFileModeException"


-- | The pull request status is not valid. The only valid values are @OPEN@ and @CLOSED@ .
--
--
_InvalidPullRequestStatusException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPullRequestStatusException =
  _MatchServiceError codeCommit "InvalidPullRequestStatusException"


-- | A parent commit ID is required. To view the full commit ID of a branch in a repository, use 'GetBranch' or a Git command (for example, git pull or git log).
--
--
_ParentCommitIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ParentCommitIdRequiredException =
  _MatchServiceError codeCommit "ParentCommitIdRequiredException"


-- | The source commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.
--
--
_InvalidSourceCommitSpecifierException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSourceCommitSpecifierException =
  _MatchServiceError codeCommit "InvalidSourceCommitSpecifierException"


-- | The specified repository does not exist.
--
--
_RepositoryDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryDoesNotExistException =
  _MatchServiceError codeCommit "RepositoryDoesNotExistException"


-- | The number of branches for the trigger was exceeded.
--
--
_MaximumBranchesExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumBranchesExceededException =
  _MatchServiceError codeCommit "MaximumBranchesExceededException"


-- | The title of the pull request is not valid. Pull request titles cannot exceed 100 characters in length.
--
--
_InvalidTitleException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTitleException = _MatchServiceError codeCommit "InvalidTitleException"


-- | The comment is too large. Comments are limited to 1,000 characters.
--
--
_CommentContentSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentContentSizeLimitExceededException =
  _MatchServiceError codeCommit "CommentContentSizeLimitExceededException"


-- | The parent commit ID is not valid. The commit ID cannot be empty, and must match the head commit ID for the branch of the repository where you want to add or update a file.
--
--
_InvalidParentCommitIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidParentCommitIdException =
  _MatchServiceError codeCommit "InvalidParentCommitIdException"


-- | The pull request event type is not valid.
--
--
_InvalidPullRequestEventTypeException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPullRequestEventTypeException =
  _MatchServiceError codeCommit "InvalidPullRequestEventTypeException"


-- | The file cannot be added because it is empty. Empty files cannot be added to the repository with this API.
--
--
_FileContentRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_FileContentRequiredException =
  _MatchServiceError codeCommit "FileContentRequiredException"


-- | The source branch and the destination branch for the pull request are the same. You must specify different branches for the source and destination.
--
--
_SourceAndDestinationAreSameException :: AsError a => Getting (First ServiceError) a ServiceError
_SourceAndDestinationAreSameException =
  _MatchServiceError codeCommit "SourceAndDestinationAreSameException"


-- | The specified path does not exist.
--
--
_PathDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_PathDoesNotExistException =
  _MatchServiceError codeCommit "PathDoesNotExistException"


-- | An encryption integrity check failed.
--
--
_EncryptionIntegrityChecksFailedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionIntegrityChecksFailedException =
  _MatchServiceError codeCommit "EncryptionIntegrityChecksFailedException"


-- | The file could not be added because the provided parent commit ID is not the current tip of the specified branch. To view the full commit ID of the current head of the branch, use 'GetBranch' .
--
--
_ParentCommitIdOutdatedException :: AsError a => Getting (First ServiceError) a ServiceError
_ParentCommitIdOutdatedException =
  _MatchServiceError codeCommit "ParentCommitIdOutdatedException"


-- | At least one event for the trigger is required but was not specified.
--
--
_RepositoryTriggerEventsListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerEventsListRequiredException =
  _MatchServiceError codeCommit "RepositoryTriggerEventsListRequiredException"


-- | The comment is empty. You must provide some content for a comment. The content cannot be null.
--
--
_CommentContentRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentContentRequiredException =
  _MatchServiceError codeCommit "CommentContentRequiredException"


-- | The targets for the pull request is not valid or not in a valid format. Targets are a list of target objects. Each target object must contain the full values for the repository name, source branch, and destination branch for a pull request.
--
--
_InvalidTargetsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTargetsException =
  _MatchServiceError codeCommit "InvalidTargetsException"


-- | An encryption key could not be accessed.
--
--
_EncryptionKeyAccessDeniedException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyAccessDeniedException =
  _MatchServiceError codeCommit "EncryptionKeyAccessDeniedException"


-- | The specified branch name already exists.
--
--
_BranchNameExistsException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameExistsException =
  _MatchServiceError codeCommit "BranchNameExistsException"


-- | The specified commit is not valid.
--
--
_InvalidCommitException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitException = _MatchServiceError codeCommit "InvalidCommitException"


-- | A pull request target is required. It cannot be empty or null. A pull request target must contain the full values for the repository name, source branch, and destination branch for the pull request.
--
--
_TargetRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TargetRequiredException =
  _MatchServiceError codeCommit "TargetRequiredException"


-- | The destination commit specifier is not valid. You must provide a valid branch name, tag, or full commit ID.
--
--
_InvalidDestinationCommitSpecifierException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidDestinationCommitSpecifierException =
  _MatchServiceError codeCommit "InvalidDestinationCommitSpecifierException"


-- | No comment exists with the provided ID. Verify that you have provided the correct ID, and then try again.
--
--
_CommentDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentDoesNotExistException =
  _MatchServiceError codeCommit "CommentDoesNotExistException"


-- | The specified reference is not a supported type.
--
--
_ReferenceTypeNotSupportedException :: AsError a => Getting (First ServiceError) a ServiceError
_ReferenceTypeNotSupportedException =
  _MatchServiceError codeCommit "ReferenceTypeNotSupportedException"


-- | A file cannot be added to the repository because the specified file name has the same name as a directory in this repository. Either provide another name for the file, or add the file in a directory that does not match the file name.
--
--
_FileNameConflictsWithDirectoryNameException :: AsError a => Getting (First ServiceError) a ServiceError
_FileNameConflictsWithDirectoryNameException =
  _MatchServiceError codeCommit "FileNameConflictsWithDirectoryNameException"


-- | The file name is not valid because it has exceeded the character limit for file names. File names, including the path to the file, cannot exceed the character limit.
--
--
_NameLengthExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_NameLengthExceededException =
  _MatchServiceError codeCommit "NameLengthExceededException"


-- | The specified sort by value is not valid.
--
--
_InvalidSortByException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidSortByException = _MatchServiceError codeCommit "InvalidSortByException"


-- | The encryption key is disabled.
--
--
_EncryptionKeyDisabledException :: AsError a => Getting (First ServiceError) a ServiceError
_EncryptionKeyDisabledException =
  _MatchServiceError codeCommit "EncryptionKeyDisabledException"


-- | A commit was not specified.
--
--
_CommitRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitRequiredException =
  _MatchServiceError codeCommit "CommitRequiredException"


-- | You cannot create the pull request because the repository has too many open pull requests. The maximum number of open pull requests for a repository is 1,000. Close one or more open pull requests, and then try again.
--
--
_MaximumOpenPullRequestsExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_MaximumOpenPullRequestsExceededException =
  _MatchServiceError codeCommit "MaximumOpenPullRequestsExceededException"


-- | The target for the pull request is not valid. A target must contain the full values for the repository name, source branch, and destination branch for the pull request.
--
--
_InvalidTargetException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidTargetException = _MatchServiceError codeCommit "InvalidTargetException"


-- | The pull request ID is not valid. Make sure that you have provided the full ID and that the pull request is in the specified repository, and then try again.
--
--
_InvalidPullRequestIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPullRequestIdException =
  _MatchServiceError codeCommit "InvalidPullRequestIdException"


-- | You cannot modify or delete this comment. Only comment authors can modify or delete their comments.
--
--
_CommentNotCreatedByCallerException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentNotCreatedByCallerException =
  _MatchServiceError codeCommit "CommentNotCreatedByCallerException"


-- | The pull request status update is not valid. The only valid update is from @OPEN@ to @CLOSED@ .
--
--
_InvalidPullRequestStatusUpdateException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidPullRequestStatusUpdateException =
  _MatchServiceError codeCommit "InvalidPullRequestStatusUpdateException"


-- | The specified reference name format is not valid. Reference names must conform to the Git references format, for example refs/heads/master. For more information, see <https://git-scm.com/book/en/v2/Git-Internals-Git-References Git Internals - Git References> or consult your Git documentation.
--
--
_InvalidReferenceNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidReferenceNameException =
  _MatchServiceError codeCommit "InvalidReferenceNameException"


-- | The file was not added or updated because the content of the file is exactly the same as the content of that file in the repository and branch that you specified.
--
--
_SameFileContentException :: AsError a => Getting (First ServiceError) a ServiceError
_SameFileContentException =
  _MatchServiceError codeCommit "SameFileContentException"


-- | A commit ID was not specified.
--
--
_CommitIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdRequiredException =
  _MatchServiceError codeCommit "CommitIdRequiredException"


-- | The specified commit ID is not valid.
--
--
_InvalidCommitIdException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidCommitIdException =
  _MatchServiceError codeCommit "InvalidCommitIdException"


-- | The tip of the source branch in the destination repository does not match the tip of the source branch specified in your request. The pull request might have been updated. Make sure that you have the latest changes.
--
--
_TipOfSourceReferenceIsDifferentException :: AsError a => Getting (First ServiceError) a ServiceError
_TipOfSourceReferenceIsDifferentException =
  _MatchServiceError codeCommit "TipOfSourceReferenceIsDifferentException"


-- | A destination ARN for the target service for the trigger is required but was not specified.
--
--
_RepositoryTriggerDestinationARNRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerDestinationARNRequiredException =
  _MatchServiceError
    codeCommit
    "RepositoryTriggerDestinationArnRequiredException"


-- | The client request token is not valid.
--
--
_InvalidClientRequestTokenException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidClientRequestTokenException =
  _MatchServiceError codeCommit "InvalidClientRequestTokenException"


-- | The specified commit does not exist or no commit was specified, and the specified repository has no default branch.
--
--
_CommitDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitDoesNotExistException =
  _MatchServiceError codeCommit "CommitDoesNotExistException"


-- | At least one branch name is required but was not specified in the trigger configuration.
--
--
_RepositoryTriggerBranchNameListRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryTriggerBranchNameListRequiredException =
  _MatchServiceError
    codeCommit
    "RepositoryTriggerBranchNameListRequiredException"


-- | A client request token is required. A client request token is an unique, client-generated idempotency token that when provided in a request, ensures the request cannot be repeated with a changed parameter. If a request is received with the same parameters and a token is included, the request will return information about the initial request that used that token.
--
--
_ClientRequestTokenRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_ClientRequestTokenRequiredException =
  _MatchServiceError codeCommit "ClientRequestTokenRequiredException"


-- | The specified merge option is not valid. The only valid value is FAST_FORWARD_MERGE.
--
--
_InvalidMergeOptionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMergeOptionException =
  _MatchServiceError codeCommit "InvalidMergeOptionException"


-- | The comment ID is missing or null. A comment ID is required.
--
--
_CommentIdRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentIdRequiredException =
  _MatchServiceError codeCommit "CommentIdRequiredException"


-- | The specified number of maximum results is not valid.
--
--
_InvalidMaxResultsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidMaxResultsException =
  _MatchServiceError codeCommit "InvalidMaxResultsException"


-- | The specified file exceeds the file size limit for AWS CodeCommit. For more information about limits in AWS CodeCommit, see <http://docs.aws.amazon.com/codecommit/latest/userguide/limits.html AWS CodeCommit User Guide> .
--
--
_FileTooLargeException :: AsError a => Getting (First ServiceError) a ServiceError
_FileTooLargeException = _MatchServiceError codeCommit "FileTooLargeException"


-- | The specified commit ID does not exist.
--
--
_CommitIdDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_CommitIdDoesNotExistException =
  _MatchServiceError codeCommit "CommitIdDoesNotExistException"


-- | You cannot include more than one repository in a pull request. Make sure you have specified only one repository name in your request, and then try again.
--
--
_MultipleRepositoriesInPullRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_MultipleRepositoriesInPullRequestException =
  _MatchServiceError codeCommit "MultipleRepositoriesInPullRequestException"


-- | The file cannot be added because it is too large. The maximum file size that can be added using PutFile is 6 MB. For files larger than 6 MB but smaller than 2 GB, add them using a Git client.
--
--
_FileContentSizeLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_FileContentSizeLimitExceededException =
  _MatchServiceError codeCommit "FileContentSizeLimitExceededException"


-- | The name of the trigger is not valid.
--
--
_InvalidRepositoryTriggerNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerNameException =
  _MatchServiceError codeCommit "InvalidRepositoryTriggerNameException"


-- | A repository name is required but was not specified.
--
--
_RepositoryNameRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNameRequiredException =
  _MatchServiceError codeCommit "RepositoryNameRequiredException"


-- | A repository resource limit was exceeded.
--
--
_RepositoryLimitExceededException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryLimitExceededException =
  _MatchServiceError codeCommit "RepositoryLimitExceededException"


-- | One or more events specified for the trigger is not valid. Check to make sure that all events specified match the requirements for allowed events.
--
--
_InvalidRepositoryTriggerEventsException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryTriggerEventsException =
  _MatchServiceError codeCommit "InvalidRepositoryTriggerEventsException"


-- | The specified branch name is not valid because it is a tag name. Type the name of a current branch in the repository. For a list of valid branch names, use 'ListBranches' .
--
--
_BranchNameIsTagNameException :: AsError a => Getting (First ServiceError) a ServiceError
_BranchNameIsTagNameException =
  _MatchServiceError codeCommit "BranchNameIsTagNameException"


-- | At least one specified repository name is not valid.
--
--
_InvalidRepositoryNameException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidRepositoryNameException =
  _MatchServiceError codeCommit "InvalidRepositoryNameException"


-- | The Amazon Resource Name (ARN) is not valid. Make sure that you have provided the full ARN for the author of the pull request, and then try again.
--
--
_InvalidAuthorARNException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidAuthorARNException =
  _MatchServiceError codeCommit "InvalidAuthorArnException"


-- | A pull request status is required, but none was provided.
--
--
_PullRequestStatusRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_PullRequestStatusRequiredException =
  _MatchServiceError codeCommit "PullRequestStatusRequiredException"


-- | The repository does not contain any pull requests with that pull request ID. Check to make sure you have provided the correct repository name for the pull request.
--
--
_RepositoryNotAssociatedWithPullRequestException :: AsError a => Getting (First ServiceError) a ServiceError
_RepositoryNotAssociatedWithPullRequestException =
  _MatchServiceError
    codeCommit
    "RepositoryNotAssociatedWithPullRequestException"


-- | A pull request title is required. It cannot be empty or null.
--
--
_TitleRequiredException :: AsError a => Getting (First ServiceError) a ServiceError
_TitleRequiredException = _MatchServiceError codeCommit "TitleRequiredException"


-- | The position is not valid. Make sure that the line number exists in the version of the file you want to comment on.
--
--
_InvalidFilePositionException :: AsError a => Getting (First ServiceError) a ServiceError
_InvalidFilePositionException =
  _MatchServiceError codeCommit "InvalidFilePositionException"


-- | This comment has already been deleted. You cannot edit or delete a deleted comment.
--
--
_CommentDeletedException :: AsError a => Getting (First ServiceError) a ServiceError
_CommentDeletedException =
  _MatchServiceError codeCommit "CommentDeletedException"


-- | The parent commit ID is not valid. The specified parent commit ID does not exist in the specified branch of the repository.
--
--
_ParentCommitDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_ParentCommitDoesNotExistException =
  _MatchServiceError codeCommit "ParentCommitDoesNotExistException"


-- | The specified Amazon Resource Name (ARN) does not exist in the AWS account.
--
--
_AuthorDoesNotExistException :: AsError a => Getting (First ServiceError) a ServiceError
_AuthorDoesNotExistException =
  _MatchServiceError codeCommit "AuthorDoesNotExistException"

