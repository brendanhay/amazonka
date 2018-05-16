{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeCommit__
--
-- This is the /AWS CodeCommit API Reference/ . This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.
--
-- You can use the AWS CodeCommit API to work with the following objects:
--
-- Repositories, by calling the following:
--
--     * 'BatchGetRepositories' , which returns information about one or more repositories associated with your AWS account.
--
--     * 'CreateRepository' , which creates an AWS CodeCommit repository.
--
--     * 'DeleteRepository' , which deletes an AWS CodeCommit repository.
--
--     * 'GetRepository' , which returns information about a specified repository.
--
--     * 'ListRepositories' , which lists all AWS CodeCommit repositories associated with your AWS account.
--
--     * 'UpdateRepositoryDescription' , which sets or updates the description of the repository.
--
--     * 'UpdateRepositoryName' , which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.
--
--
--
-- Branches, by calling the following:
--
--     * 'CreateBranch' , which creates a new branch in a specified repository.
--
--     * 'DeleteBranch' , which deletes the specified branch in a repository unless it is the default branch.
--
--     * 'GetBranch' , which returns information about a specified branch.
--
--     * 'ListBranches' , which lists all branches for a specified repository.
--
--     * 'UpdateDefaultBranch' , which changes the default branch for a repository.
--
--
--
-- Files, by calling the following:
--
--     * 'PutFile' , which adds or modifies a file in a specified repository and branch.
--
--
--
-- Information about committed code in a repository, by calling the following:
--
--     * 'GetBlob' , which returns the base-64 encoded content of an individual Git blob object within a repository.
--
--     * 'GetCommit' , which returns information about a commit, including commit messages and author and committer information.
--
--     * 'GetDifferences' , which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID or other fully qualified reference).
--
--
--
-- Pull requests, by calling the following:
--
--     * 'CreatePullRequest' , which creates a pull request in a specified repository.
--
--     * 'DescribePullRequestEvents' , which returns information about one or more pull request events.
--
--     * 'GetCommentsForPullRequest' , which returns information about comments on a specified pull request.
--
--     * 'GetMergeConflicts' , which returns information about merge conflicts between the source and destination branch in a pull request.
--
--     * 'GetPullRequest' , which returns information about a specified pull request.
--
--     * 'ListPullRequests' , which lists all pull requests for a repository.
--
--     * 'MergePullRequestByFastForward' , which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.
--
--     * 'PostCommentForPullRequest' , which posts a comment to a pull request at the specified line, file, or request.
--
--     * 'UpdatePullRequestDescription' , which updates the description of a pull request.
--
--     * 'UpdatePullRequestStatus' , which updates the status of a pull request.
--
--     * 'UpdatePullRequestTitle' , which updates the title of a pull request.
--
--
--
-- Information about comments in a repository, by calling the following:
--
--     * 'DeleteCommentContent' , which deletes the content of a comment on a commit in a repository.
--
--     * 'GetComment' , which returns information about a comment on a commit.
--
--     * 'GetCommentsForComparedCommit' , which returns information about comments on the comparison between two commit specifiers in a repository.
--
--     * 'PostCommentForComparedCommit' , which creates a comment on the comparison between two commit specifiers in a repository.
--
--     * 'PostCommentReply' , which creates a reply to a comment.
--
--     * 'UpdateComment' , which updates the content of a comment on a commit in a repository.
--
--
--
-- Triggers, by calling the following:
--
--     * 'GetRepositoryTriggers' , which returns information about triggers configured for a repository.
--
--     * 'PutRepositoryTriggers' , which replaces all triggers for a repository and can be used to create or delete triggers.
--
--     * 'TestRepositoryTriggers' , which tests the functionality of a repository trigger by sending data to the trigger target.
--
--
--
-- For information about how to use AWS CodeCommit, see the <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide> .
--
module Network.AWS.CodeCommit
    (
    -- * Service Configuration
      codeCommit

    -- * Errors
    -- $errors

    -- ** InvalidRepositoryTriggerRegionException
    , _InvalidRepositoryTriggerRegionException

    -- ** InvalidContinuationTokenException
    , _InvalidContinuationTokenException

    -- ** ManualMergeRequiredException
    , _ManualMergeRequiredException

    -- ** TargetsRequiredException
    , _TargetsRequiredException

    -- ** EncryptionKeyNotFoundException
    , _EncryptionKeyNotFoundException

    -- ** TipsDivergenceExceededException
    , _TipsDivergenceExceededException

    -- ** InvalidRepositoryTriggerBranchNameException
    , _InvalidRepositoryTriggerBranchNameException

    -- ** PullRequestAlreadyClosedException
    , _PullRequestAlreadyClosedException

    -- ** InvalidRepositoryTriggerCustomDataException
    , _InvalidRepositoryTriggerCustomDataException

    -- ** DirectoryNameConflictsWithFileNameException
    , _DirectoryNameConflictsWithFileNameException

    -- ** ReferenceDoesNotExistException
    , _ReferenceDoesNotExistException

    -- ** ActorDoesNotExistException
    , _ActorDoesNotExistException

    -- ** PullRequestIdRequiredException
    , _PullRequestIdRequiredException

    -- ** InvalidEmailException
    , _InvalidEmailException

    -- ** CommitMessageLengthExceededException
    , _CommitMessageLengthExceededException

    -- ** BlobIdDoesNotExistException
    , _BlobIdDoesNotExistException

    -- ** MaximumRepositoryNamesExceededException
    , _MaximumRepositoryNamesExceededException

    -- ** InvalidRepositoryDescriptionException
    , _InvalidRepositoryDescriptionException

    -- ** RepositoryNameExistsException
    , _RepositoryNameExistsException

    -- ** ReferenceNameRequiredException
    , _ReferenceNameRequiredException

    -- ** MaximumRepositoryTriggersExceededException
    , _MaximumRepositoryTriggersExceededException

    -- ** InvalidBranchNameException
    , _InvalidBranchNameException

    -- ** BranchNameRequiredException
    , _BranchNameRequiredException

    -- ** MergeOptionRequiredException
    , _MergeOptionRequiredException

    -- ** InvalidFileLocationException
    , _InvalidFileLocationException

    -- ** BeforeCommitIdAndAfterCommitIdAreSameException
    , _BeforeCommitIdAndAfterCommitIdAreSameException

    -- ** RepositoryTriggersListRequiredException
    , _RepositoryTriggersListRequiredException

    -- ** IdempotencyParameterMismatchException
    , _IdempotencyParameterMismatchException

    -- ** EncryptionKeyUnavailableException
    , _EncryptionKeyUnavailableException

    -- ** InvalidRelativeFileVersionEnumException
    , _InvalidRelativeFileVersionEnumException

    -- ** InvalidRepositoryTriggerDestinationARNException
    , _InvalidRepositoryTriggerDestinationARNException

    -- ** BlobIdRequiredException
    , _BlobIdRequiredException

    -- ** RepositoryNamesRequiredException
    , _RepositoryNamesRequiredException

    -- ** InvalidActorARNException
    , _InvalidActorARNException

    -- ** InvalidCommentIdException
    , _InvalidCommentIdException

    -- ** InvalidDescriptionException
    , _InvalidDescriptionException

    -- ** InvalidBlobIdException
    , _InvalidBlobIdException

    -- ** PullRequestDoesNotExistException
    , _PullRequestDoesNotExistException

    -- ** InvalidOrderException
    , _InvalidOrderException

    -- ** BranchDoesNotExistException
    , _BranchDoesNotExistException

    -- ** DefaultBranchCannotBeDeletedException
    , _DefaultBranchCannotBeDeletedException

    -- ** InvalidPathException
    , _InvalidPathException

    -- ** PathRequiredException
    , _PathRequiredException

    -- ** RepositoryTriggerNameRequiredException
    , _RepositoryTriggerNameRequiredException

    -- ** InvalidFileModeException
    , _InvalidFileModeException

    -- ** InvalidPullRequestStatusException
    , _InvalidPullRequestStatusException

    -- ** ParentCommitIdRequiredException
    , _ParentCommitIdRequiredException

    -- ** InvalidSourceCommitSpecifierException
    , _InvalidSourceCommitSpecifierException

    -- ** RepositoryDoesNotExistException
    , _RepositoryDoesNotExistException

    -- ** MaximumBranchesExceededException
    , _MaximumBranchesExceededException

    -- ** InvalidTitleException
    , _InvalidTitleException

    -- ** CommentContentSizeLimitExceededException
    , _CommentContentSizeLimitExceededException

    -- ** InvalidParentCommitIdException
    , _InvalidParentCommitIdException

    -- ** InvalidPullRequestEventTypeException
    , _InvalidPullRequestEventTypeException

    -- ** FileContentRequiredException
    , _FileContentRequiredException

    -- ** SourceAndDestinationAreSameException
    , _SourceAndDestinationAreSameException

    -- ** PathDoesNotExistException
    , _PathDoesNotExistException

    -- ** EncryptionIntegrityChecksFailedException
    , _EncryptionIntegrityChecksFailedException

    -- ** ParentCommitIdOutdatedException
    , _ParentCommitIdOutdatedException

    -- ** RepositoryTriggerEventsListRequiredException
    , _RepositoryTriggerEventsListRequiredException

    -- ** CommentContentRequiredException
    , _CommentContentRequiredException

    -- ** InvalidTargetsException
    , _InvalidTargetsException

    -- ** EncryptionKeyAccessDeniedException
    , _EncryptionKeyAccessDeniedException

    -- ** BranchNameExistsException
    , _BranchNameExistsException

    -- ** InvalidCommitException
    , _InvalidCommitException

    -- ** TargetRequiredException
    , _TargetRequiredException

    -- ** InvalidDestinationCommitSpecifierException
    , _InvalidDestinationCommitSpecifierException

    -- ** CommentDoesNotExistException
    , _CommentDoesNotExistException

    -- ** ReferenceTypeNotSupportedException
    , _ReferenceTypeNotSupportedException

    -- ** FileNameConflictsWithDirectoryNameException
    , _FileNameConflictsWithDirectoryNameException

    -- ** NameLengthExceededException
    , _NameLengthExceededException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** EncryptionKeyDisabledException
    , _EncryptionKeyDisabledException

    -- ** CommitRequiredException
    , _CommitRequiredException

    -- ** MaximumOpenPullRequestsExceededException
    , _MaximumOpenPullRequestsExceededException

    -- ** InvalidTargetException
    , _InvalidTargetException

    -- ** InvalidPullRequestIdException
    , _InvalidPullRequestIdException

    -- ** CommentNotCreatedByCallerException
    , _CommentNotCreatedByCallerException

    -- ** InvalidPullRequestStatusUpdateException
    , _InvalidPullRequestStatusUpdateException

    -- ** InvalidReferenceNameException
    , _InvalidReferenceNameException

    -- ** SameFileContentException
    , _SameFileContentException

    -- ** CommitIdRequiredException
    , _CommitIdRequiredException

    -- ** InvalidCommitIdException
    , _InvalidCommitIdException

    -- ** TipOfSourceReferenceIsDifferentException
    , _TipOfSourceReferenceIsDifferentException

    -- ** RepositoryTriggerDestinationARNRequiredException
    , _RepositoryTriggerDestinationARNRequiredException

    -- ** InvalidClientRequestTokenException
    , _InvalidClientRequestTokenException

    -- ** CommitDoesNotExistException
    , _CommitDoesNotExistException

    -- ** RepositoryTriggerBranchNameListRequiredException
    , _RepositoryTriggerBranchNameListRequiredException

    -- ** ClientRequestTokenRequiredException
    , _ClientRequestTokenRequiredException

    -- ** InvalidMergeOptionException
    , _InvalidMergeOptionException

    -- ** CommentIdRequiredException
    , _CommentIdRequiredException

    -- ** InvalidMaxResultsException
    , _InvalidMaxResultsException

    -- ** FileTooLargeException
    , _FileTooLargeException

    -- ** CommitIdDoesNotExistException
    , _CommitIdDoesNotExistException

    -- ** MultipleRepositoriesInPullRequestException
    , _MultipleRepositoriesInPullRequestException

    -- ** FileContentSizeLimitExceededException
    , _FileContentSizeLimitExceededException

    -- ** InvalidRepositoryTriggerNameException
    , _InvalidRepositoryTriggerNameException

    -- ** RepositoryNameRequiredException
    , _RepositoryNameRequiredException

    -- ** RepositoryLimitExceededException
    , _RepositoryLimitExceededException

    -- ** InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryTriggerEventsException

    -- ** BranchNameIsTagNameException
    , _BranchNameIsTagNameException

    -- ** InvalidRepositoryNameException
    , _InvalidRepositoryNameException

    -- ** InvalidAuthorARNException
    , _InvalidAuthorARNException

    -- ** PullRequestStatusRequiredException
    , _PullRequestStatusRequiredException

    -- ** RepositoryNotAssociatedWithPullRequestException
    , _RepositoryNotAssociatedWithPullRequestException

    -- ** TitleRequiredException
    , _TitleRequiredException

    -- ** InvalidFilePositionException
    , _InvalidFilePositionException

    -- ** CommentDeletedException
    , _CommentDeletedException

    -- ** ParentCommitDoesNotExistException
    , _ParentCommitDoesNotExistException

    -- ** AuthorDoesNotExistException
    , _AuthorDoesNotExistException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** MergePullRequestByFastForward
    , module Network.AWS.CodeCommit.MergePullRequestByFastForward

    -- ** UpdateRepositoryName
    , module Network.AWS.CodeCommit.UpdateRepositoryName

    -- ** PostCommentForPullRequest
    , module Network.AWS.CodeCommit.PostCommentForPullRequest

    -- ** GetCommit
    , module Network.AWS.CodeCommit.GetCommit

    -- ** GetBranch
    , module Network.AWS.CodeCommit.GetBranch

    -- ** GetDifferences (Paginated)
    , module Network.AWS.CodeCommit.GetDifferences

    -- ** GetPullRequest
    , module Network.AWS.CodeCommit.GetPullRequest

    -- ** ListPullRequests (Paginated)
    , module Network.AWS.CodeCommit.ListPullRequests

    -- ** GetComment
    , module Network.AWS.CodeCommit.GetComment

    -- ** DeleteBranch
    , module Network.AWS.CodeCommit.DeleteBranch

    -- ** UpdateRepositoryDescription
    , module Network.AWS.CodeCommit.UpdateRepositoryDescription

    -- ** CreateBranch
    , module Network.AWS.CodeCommit.CreateBranch

    -- ** CreatePullRequest
    , module Network.AWS.CodeCommit.CreatePullRequest

    -- ** ListBranches (Paginated)
    , module Network.AWS.CodeCommit.ListBranches

    -- ** UpdatePullRequestDescription
    , module Network.AWS.CodeCommit.UpdatePullRequestDescription

    -- ** ListRepositories (Paginated)
    , module Network.AWS.CodeCommit.ListRepositories

    -- ** CreateRepository
    , module Network.AWS.CodeCommit.CreateRepository

    -- ** UpdateDefaultBranch
    , module Network.AWS.CodeCommit.UpdateDefaultBranch

    -- ** PostCommentReply
    , module Network.AWS.CodeCommit.PostCommentReply

    -- ** GetRepository
    , module Network.AWS.CodeCommit.GetRepository

    -- ** GetRepositoryTriggers
    , module Network.AWS.CodeCommit.GetRepositoryTriggers

    -- ** PutFile
    , module Network.AWS.CodeCommit.PutFile

    -- ** GetCommentsForComparedCommit (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForComparedCommit

    -- ** TestRepositoryTriggers
    , module Network.AWS.CodeCommit.TestRepositoryTriggers

    -- ** UpdateComment
    , module Network.AWS.CodeCommit.UpdateComment

    -- ** PostCommentForComparedCommit
    , module Network.AWS.CodeCommit.PostCommentForComparedCommit

    -- ** UpdatePullRequestTitle
    , module Network.AWS.CodeCommit.UpdatePullRequestTitle

    -- ** GetBlob
    , module Network.AWS.CodeCommit.GetBlob

    -- ** PutRepositoryTriggers
    , module Network.AWS.CodeCommit.PutRepositoryTriggers

    -- ** GetMergeConflicts
    , module Network.AWS.CodeCommit.GetMergeConflicts

    -- ** DeleteRepository
    , module Network.AWS.CodeCommit.DeleteRepository

    -- ** DeleteCommentContent
    , module Network.AWS.CodeCommit.DeleteCommentContent

    -- ** DescribePullRequestEvents (Paginated)
    , module Network.AWS.CodeCommit.DescribePullRequestEvents

    -- ** BatchGetRepositories
    , module Network.AWS.CodeCommit.BatchGetRepositories

    -- ** GetCommentsForPullRequest (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForPullRequest

    -- ** UpdatePullRequestStatus
    , module Network.AWS.CodeCommit.UpdatePullRequestStatus

    -- * Types

    -- ** ChangeTypeEnum
    , ChangeTypeEnum (..)

    -- ** FileModeTypeEnum
    , FileModeTypeEnum (..)

    -- ** MergeOptionTypeEnum
    , MergeOptionTypeEnum (..)

    -- ** OrderEnum
    , OrderEnum (..)

    -- ** PullRequestEventType
    , PullRequestEventType (..)

    -- ** PullRequestStatusEnum
    , PullRequestStatusEnum (..)

    -- ** RelativeFileVersionEnum
    , RelativeFileVersionEnum (..)

    -- ** RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- ** SortByEnum
    , SortByEnum (..)

    -- ** BlobMetadata
    , BlobMetadata
    , blobMetadata
    , bmPath
    , bmMode
    , bmBlobId

    -- ** BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- ** Comment
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

    -- ** CommentsForComparedCommit
    , CommentsForComparedCommit
    , commentsForComparedCommit
    , cfccBeforeBlobId
    , cfccLocation
    , cfccAfterCommitId
    , cfccAfterBlobId
    , cfccBeforeCommitId
    , cfccRepositoryName
    , cfccComments

    -- ** CommentsForPullRequest
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

    -- ** Commit
    , Commit
    , commit
    , cCommitId
    , cCommitter
    , cTreeId
    , cAdditionalData
    , cParents
    , cAuthor
    , cMessage

    -- ** Difference
    , Difference
    , difference
    , dAfterBlob
    , dBeforeBlob
    , dChangeType

    -- ** Location
    , Location
    , location
    , lRelativeFileVersion
    , lFilePath
    , lFilePosition

    -- ** MergeMetadata
    , MergeMetadata
    , mergeMetadata
    , mmMergedBy
    , mmIsMerged

    -- ** PullRequest
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

    -- ** PullRequestEvent
    , PullRequestEvent
    , pullRequestEvent
    , prePullRequestMergedStateChangedEventMetadata
    , prePullRequestEventType
    , prePullRequestStatusChangedEventMetadata
    , preActorARN
    , prePullRequestId
    , preEventDate
    , prePullRequestSourceReferenceUpdatedEventMetadata

    -- ** PullRequestMergedStateChangedEventMetadata
    , PullRequestMergedStateChangedEventMetadata
    , pullRequestMergedStateChangedEventMetadata
    , prmscemDestinationReference
    , prmscemMergeMetadata
    , prmscemRepositoryName

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    , PullRequestSourceReferenceUpdatedEventMetadata
    , pullRequestSourceReferenceUpdatedEventMetadata
    , prsruemAfterCommitId
    , prsruemBeforeCommitId
    , prsruemRepositoryName

    -- ** PullRequestStatusChangedEventMetadata
    , PullRequestStatusChangedEventMetadata
    , pullRequestStatusChangedEventMetadata
    , prscemPullRequestStatus

    -- ** PullRequestTarget
    , PullRequestTarget
    , pullRequestTarget
    , prtSourceCommit
    , prtDestinationReference
    , prtMergeMetadata
    , prtDestinationCommit
    , prtRepositoryName
    , prtSourceReference

    -- ** RepositoryMetadata
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

    -- ** RepositoryNameIdPair
    , RepositoryNameIdPair
    , repositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName

    -- ** RepositoryTrigger
    , RepositoryTrigger
    , repositoryTrigger
    , rtBranches
    , rtCustomData
    , rtName
    , rtDestinationARN
    , rtEvents

    -- ** RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure
    , repositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- ** Target
    , Target
    , target
    , tDestinationReference
    , tRepositoryName
    , tSourceReference

    -- ** UserInfo
    , UserInfo
    , userInfo
    , uiEmail
    , uiDate
    , uiName
    ) where

import Network.AWS.CodeCommit.BatchGetRepositories
import Network.AWS.CodeCommit.CreateBranch
import Network.AWS.CodeCommit.CreatePullRequest
import Network.AWS.CodeCommit.CreateRepository
import Network.AWS.CodeCommit.DeleteBranch
import Network.AWS.CodeCommit.DeleteCommentContent
import Network.AWS.CodeCommit.DeleteRepository
import Network.AWS.CodeCommit.DescribePullRequestEvents
import Network.AWS.CodeCommit.GetBlob
import Network.AWS.CodeCommit.GetBranch
import Network.AWS.CodeCommit.GetComment
import Network.AWS.CodeCommit.GetCommentsForComparedCommit
import Network.AWS.CodeCommit.GetCommentsForPullRequest
import Network.AWS.CodeCommit.GetCommit
import Network.AWS.CodeCommit.GetDifferences
import Network.AWS.CodeCommit.GetMergeConflicts
import Network.AWS.CodeCommit.GetPullRequest
import Network.AWS.CodeCommit.GetRepository
import Network.AWS.CodeCommit.GetRepositoryTriggers
import Network.AWS.CodeCommit.ListBranches
import Network.AWS.CodeCommit.ListPullRequests
import Network.AWS.CodeCommit.ListRepositories
import Network.AWS.CodeCommit.MergePullRequestByFastForward
import Network.AWS.CodeCommit.PostCommentForComparedCommit
import Network.AWS.CodeCommit.PostCommentForPullRequest
import Network.AWS.CodeCommit.PostCommentReply
import Network.AWS.CodeCommit.PutFile
import Network.AWS.CodeCommit.PutRepositoryTriggers
import Network.AWS.CodeCommit.TestRepositoryTriggers
import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.UpdateComment
import Network.AWS.CodeCommit.UpdateDefaultBranch
import Network.AWS.CodeCommit.UpdatePullRequestDescription
import Network.AWS.CodeCommit.UpdatePullRequestStatus
import Network.AWS.CodeCommit.UpdatePullRequestTitle
import Network.AWS.CodeCommit.UpdateRepositoryDescription
import Network.AWS.CodeCommit.UpdateRepositoryName
import Network.AWS.CodeCommit.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeCommit'.
-}

{- $operations
Some AWS operations return results that are incomplete and require subsequent
requests in order to obtain the entire result set. The process of sending
subsequent requests to continue where a previous request left off is called
pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
1000 objects at a time, and you must send subsequent requests with the
appropriate Marker in order to retrieve the next page of results.

Operations that have an 'AWSPager' instance can transparently perform subsequent
requests, correctly setting Markers and other request facets to iterate through
the entire result set of a truncated API operation. Operations which support
this have an additional note in the documentation.

Many operations have the ability to filter results on the server side. See the
individual operation parameters for details.
-}

{- $waiters
Waiters poll by repeatedly sending a request until some remote success condition
configured by the 'Wait' specification is fulfilled. The 'Wait' specification
determines how many attempts should be made, in addition to delay and retry strategies.
-}
