{-# OPTIONS_GHC -fno-warn-unused-imports    #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeCommit__ 
--
-- This is the /AWS CodeCommit API Reference/ . This reference provides descriptions of the operations and data types for AWS CodeCommit API along with usage examples.
-- You can use the AWS CodeCommit API to work with the following objects:
-- Repositories, by calling the following:
--
--     * 'BatchGetRepositories' , which returns information about one or more repositories associated with your AWS account.
--
--
--     * 'CreateRepository' , which creates an AWS CodeCommit repository.
--
--
--     * 'DeleteRepository' , which deletes an AWS CodeCommit repository.
--
--
--     * 'GetRepository' , which returns information about a specified repository.
--
--
--     * 'ListRepositories' , which lists all AWS CodeCommit repositories associated with your AWS account.
--
--
--     * 'UpdateRepositoryDescription' , which sets or updates the description of the repository.
--
--
--     * 'UpdateRepositoryName' , which changes the name of the repository. If you change the name of a repository, no other users of that repository can access it until you send them the new HTTPS or SSH URL to use.
--
--
-- Branches, by calling the following:
--
--     * 'CreateBranch' , which creates a branch in a specified repository.
--
--
--     * 'DeleteBranch' , which deletes the specified branch in a repository unless it is the default branch.
--
--
--     * 'GetBranch' , which returns information about a specified branch.
--
--
--     * 'ListBranches' , which lists all branches for a specified repository.
--
--
--     * 'UpdateDefaultBranch' , which changes the default branch for a repository.
--
--
-- Files, by calling the following:
--
--     * 'DeleteFile' , which deletes the content of a specified file from a specified branch.
--
--
--     * 'GetBlob' , which returns the base-64 encoded content of an individual Git blob object in a repository.
--
--
--     * 'GetFile' , which returns the base-64 encoded content of a specified file.
--
--
--     * 'GetFolder' , which returns the contents of a specified folder or directory.
--
--
--     * 'PutFile' , which adds or modifies a single file in a specified repository and branch.
--
--
-- Commits, by calling the following:
--
--     * 'BatchGetCommits' , which returns information about one or more commits in a repository.
--
--
--     * 'CreateCommit' , which creates a commit for changes to a repository.
--
--
--     * 'GetCommit' , which returns information about a commit, including commit messages and author and committer information.
--
--
--     * 'GetDifferences' , which returns information about the differences in a valid commit specifier (such as a branch, tag, HEAD, commit ID, or other fully qualified reference).
--
--
-- Merges, by calling the following:
--
--     * 'BatchDescribeMergeConflicts' , which returns information about conflicts in a merge between commits in a repository.
--
--
--     * 'CreateUnreferencedMergeCommit' , which creates an unreferenced commit between two branches or commits for the purpose of comparing them and identifying any potential conflicts.
--
--
--     * 'DescribeMergeConflicts' , which returns information about merge conflicts between the base, source, and destination versions of a file in a potential merge.
--
--
--     * 'GetMergeCommit' , which returns information about the merge between a source and destination commit. 
--
--
--     * 'GetMergeConflicts' , which returns information about merge conflicts between the source and destination branch in a pull request.
--
--
--     * 'GetMergeOptions' , which returns information about the available merge options between two branches or commit specifiers.
--
--
--     * 'MergeBranchesByFastForward' , which merges two branches using the fast-forward merge option.
--
--
--     * 'MergeBranchesBySquash' , which merges two branches using the squash merge option.
--
--
--     * 'MergeBranchesByThreeWay' , which merges two branches using the three-way merge option.
--
--
-- Pull requests, by calling the following:
--
--     * 'CreatePullRequest' , which creates a pull request in a specified repository.
--
--
--     * 'CreatePullRequestApprovalRule' , which creates an approval rule for a specified pull request.
--
--
--     * 'DeletePullRequestApprovalRule' , which deletes an approval rule for a specified pull request.
--
--
--     * 'DescribePullRequestEvents' , which returns information about one or more pull request events.
--
--
--     * 'EvaluatePullRequestApprovalRules' , which evaluates whether a pull request has met all the conditions specified in its associated approval rules.
--
--
--     * 'GetCommentsForPullRequest' , which returns information about comments on a specified pull request.
--
--
--     * 'GetPullRequest' , which returns information about a specified pull request.
--
--
--     * 'GetPullRequestApprovalStates' , which returns information about the approval states for a specified pull request.
--
--
--     * 'GetPullRequestOverrideState' , which returns information about whether approval rules have been set aside (overriden) for a pull request, and if so, the Amazon Resource Name (ARN) of the user or identity that overrode the rules and their requirements for the pull request.
--
--
--     * 'ListPullRequests' , which lists all pull requests for a repository.
--
--
--     * 'MergePullRequestByFastForward' , which merges the source destination branch of a pull request into the specified destination branch for that pull request using the fast-forward merge option.
--
--
--     * 'MergePullRequestBySquash' , which merges the source destination branch of a pull request into the specified destination branch for that pull request using the squash merge option.
--
--
--     * 'MergePullRequestByThreeWay' . which merges the source destination branch of a pull request into the specified destination branch for that pull request using the three-way merge option.
--
--
--     * 'OverridePullRequestApprovalRules' , which sets aside all approval rule requirements for a pull request.
--
--
--     * 'PostCommentForPullRequest' , which posts a comment to a pull request at the specified line, file, or request.
--
--
--     * 'UpdatePullRequestApprovalRuleContent' , which updates the structure of an approval rule for a pull request.
--
--
--     * 'UpdatePullRequestApprovalState' , which updates the state of an approval on a pull request.
--
--
--     * 'UpdatePullRequestDescription' , which updates the description of a pull request.
--
--
--     * 'UpdatePullRequestStatus' , which updates the status of a pull request.
--
--
--     * 'UpdatePullRequestTitle' , which updates the title of a pull request.
--
--
-- Approval rule templates, by calling the following:
--
--     * 'AssociateApprovalRuleTemplateWithRepository' , which associates a template with a specified repository. After the template is associated with a repository, AWS CodeCommit creates approval rules that match the template conditions on every pull request created in the specified repository.
--
--
--     * 'BatchAssociateApprovalRuleTemplateWithRepositories' , which associates a template with one or more specified repositories. After the template is associated with a repository, AWS CodeCommit creates approval rules that match the template conditions on every pull request created in the specified repositories.
--
--
--     * 'BatchDisassociateApprovalRuleTemplateFromRepositories' , which removes the association between a template and specified repositories so that approval rules based on the template are not automatically created when pull requests are created in those repositories.
--
--
--     * 'CreateApprovalRuleTemplate' , which creates a template for approval rules that can then be associated with one or more repositories in your AWS account.
--
--
--     * 'DeleteApprovalRuleTemplate' , which deletes the specified template. It does not remove approval rules on pull requests already created with the template.
--
--
--     * 'DisassociateApprovalRuleTemplateFromRepository' , which removes the association between a template and a repository so that approval rules based on the template are not automatically created when pull requests are created in the specified repository.
--
--
--     * 'GetApprovalRuleTemplate' , which returns information about an approval rule template.
--
--
--     * 'ListApprovalRuleTemplates' , which lists all approval rule templates in the AWS Region in your AWS account.
--
--
--     * 'ListAssociatedApprovalRuleTemplatesForRepository' , which lists all approval rule templates that are associated with a specified repository.
--
--
--     * 'ListRepositoriesForApprovalRuleTemplate' , which lists all repositories associated with the specified approval rule template.
--
--
--     * 'UpdateApprovalRuleTemplateDescription' , which updates the description of an approval rule template.
--
--
--     * 'UpdateApprovalRuleTemplateName' , which updates the name of an approval rule template.
--
--
--     * 'UpdateApprovalRuleTemplateContent' , which updates the content of an approval rule template.
--
--
-- Comments in a repository, by calling the following:
--
--     * 'DeleteCommentContent' , which deletes the content of a comment on a commit in a repository.
--
--
--     * 'GetComment' , which returns information about a comment on a commit.
--
--
--     * 'GetCommentReactions' , which returns information about emoji reactions to comments.
--
--
--     * 'GetCommentsForComparedCommit' , which returns information about comments on the comparison between two commit specifiers in a repository.
--
--
--     * 'PostCommentForComparedCommit' , which creates a comment on the comparison between two commit specifiers in a repository.
--
--
--     * 'PostCommentReply' , which creates a reply to a comment.
--
--
--     * 'PutCommentReaction' , which creates or updates an emoji reaction to a comment.
--
--
--     * 'UpdateComment' , which updates the content of a comment on a commit in a repository.
--
--
-- Tags used to tag resources in AWS CodeCommit (not Git tags), by calling the following:
--
--     * 'ListTagsForResource' , which gets information about AWS tags for a specified Amazon Resource Name (ARN) in AWS CodeCommit.
--
--
--     * 'TagResource' , which adds or updates tags for a resource in AWS CodeCommit.
--
--
--     * 'UntagResource' , which removes tags for a resource in AWS CodeCommit.
--
--
-- Triggers, by calling the following:
--
--     * 'GetRepositoryTriggers' , which returns information about triggers configured for a repository.
--
--
--     * 'PutRepositoryTriggers' , which replaces all triggers for a repository and can be used to create or delete triggers.
--
--
--     * 'TestRepositoryTriggers' , which tests the functionality of a repository trigger by sending data to the trigger target.
--
--
-- For information about how to use AWS CodeCommit, see the <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide> .
module Network.AWS.CodeCommit
    (
    -- * Service configuration
      mkServiceConfig

    -- * Errors
    -- $errors

    -- ** InvalidReactionValueException
    , _InvalidReactionValueException

    -- ** InvalidRepositoryTriggerRegionException
    , _InvalidRepositoryTriggerRegionException

    -- ** InvalidContinuationTokenException
    , _InvalidContinuationTokenException

    -- ** ManualMergeRequiredException
    , _ManualMergeRequiredException

    -- ** TargetsRequiredException
    , _TargetsRequiredException

    -- ** InvalidSystemTagUsageException
    , _InvalidSystemTagUsageException

    -- ** FileEntryRequiredException
    , _FileEntryRequiredException

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

    -- ** ApprovalRuleNameAlreadyExistsException
    , _ApprovalRuleNameAlreadyExistsException

    -- ** ActorDoesNotExistException
    , _ActorDoesNotExistException

    -- ** PullRequestIdRequiredException
    , _PullRequestIdRequiredException

    -- ** OverrideAlreadySetException
    , _OverrideAlreadySetException

    -- ** InvalidRuleContentSha256Exception
    , _InvalidRuleContentSha256Exception

    -- ** InvalidEmailException
    , _InvalidEmailException

    -- ** CommitMessageLengthExceededException
    , _CommitMessageLengthExceededException

    -- ** BlobIdDoesNotExistException
    , _BlobIdDoesNotExistException

    -- ** MaximumRepositoryNamesExceededException
    , _MaximumRepositoryNamesExceededException

    -- ** TagKeysListRequiredException
    , _TagKeysListRequiredException

    -- ** PutFileEntryConflictException
    , _PutFileEntryConflictException

    -- ** FolderDoesNotExistException
    , _FolderDoesNotExistException

    -- ** InvalidRepositoryDescriptionException
    , _InvalidRepositoryDescriptionException

    -- ** RepositoryNameExistsException
    , _RepositoryNameExistsException

    -- ** ReferenceNameRequiredException
    , _ReferenceNameRequiredException

    -- ** MaximumRepositoryTriggersExceededException
    , _MaximumRepositoryTriggersExceededException

    -- ** ApprovalRuleDoesNotExistException
    , _ApprovalRuleDoesNotExistException

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

    -- ** InvalidRepositoryTriggerDestinationArnException
    , _InvalidRepositoryTriggerDestinationArnException

    -- ** ReactionLimitExceededException
    , _ReactionLimitExceededException

    -- ** BlobIdRequiredException
    , _BlobIdRequiredException

    -- ** RepositoryNamesRequiredException
    , _RepositoryNamesRequiredException

    -- ** ReplacementTypeRequiredException
    , _ReplacementTypeRequiredException

    -- ** InvalidActorArnException
    , _InvalidActorArnException

    -- ** InvalidCommentIdException
    , _InvalidCommentIdException

    -- ** FilePathConflictsWithSubmodulePathException
    , _FilePathConflictsWithSubmodulePathException

    -- ** InvalidDescriptionException
    , _InvalidDescriptionException

    -- ** ApprovalRuleNameRequiredException
    , _ApprovalRuleNameRequiredException

    -- ** InvalidBlobIdException
    , _InvalidBlobIdException

    -- ** PullRequestDoesNotExistException
    , _PullRequestDoesNotExistException

    -- ** NoChangeException
    , _NoChangeException

    -- ** InvalidOrderException
    , _InvalidOrderException

    -- ** InvalidApprovalRuleNameException
    , _InvalidApprovalRuleNameException

    -- ** BranchDoesNotExistException
    , _BranchDoesNotExistException

    -- ** DefaultBranchCannotBeDeletedException
    , _DefaultBranchCannotBeDeletedException

    -- ** FolderContentSizeLimitExceededException
    , _FolderContentSizeLimitExceededException

    -- ** InvalidDeletionParameterException
    , _InvalidDeletionParameterException

    -- ** InvalidReactionUserArnException
    , _InvalidReactionUserArnException

    -- ** InvalidTagsMapException
    , _InvalidTagsMapException

    -- ** InvalidPathException
    , _InvalidPathException

    -- ** PathRequiredException
    , _PathRequiredException

    -- ** InvalidTargetBranchException
    , _InvalidTargetBranchException

    -- ** RepositoryTriggerNameRequiredException
    , _RepositoryTriggerNameRequiredException

    -- ** InvalidFileModeException
    , _InvalidFileModeException

    -- ** NumberOfRuleTemplatesExceededException
    , _NumberOfRuleTemplatesExceededException

    -- ** FileModeRequiredException
    , _FileModeRequiredException

    -- ** InvalidPullRequestStatusException
    , _InvalidPullRequestStatusException

    -- ** ApprovalRuleTemplateContentRequiredException
    , _ApprovalRuleTemplateContentRequiredException

    -- ** ApprovalStateRequiredException
    , _ApprovalStateRequiredException

    -- ** ConcurrentReferenceUpdateException
    , _ConcurrentReferenceUpdateException

    -- ** ParentCommitIdRequiredException
    , _ParentCommitIdRequiredException

    -- ** InvalidSourceCommitSpecifierException
    , _InvalidSourceCommitSpecifierException

    -- ** RepositoryDoesNotExistException
    , _RepositoryDoesNotExistException

    -- ** InvalidApprovalRuleContentException
    , _InvalidApprovalRuleContentException

    -- ** MaximumBranchesExceededException
    , _MaximumBranchesExceededException

    -- ** InvalidTitleException
    , _InvalidTitleException

    -- ** CommentContentSizeLimitExceededException
    , _CommentContentSizeLimitExceededException

    -- ** PullRequestApprovalRulesNotSatisfiedException
    , _PullRequestApprovalRulesNotSatisfiedException

    -- ** InvalidParentCommitIdException
    , _InvalidParentCommitIdException

    -- ** InvalidPullRequestEventTypeException
    , _InvalidPullRequestEventTypeException

    -- ** FileContentRequiredException
    , _FileContentRequiredException

    -- ** SourceAndDestinationAreSameException
    , _SourceAndDestinationAreSameException

    -- ** ReplacementContentRequiredException
    , _ReplacementContentRequiredException

    -- ** RestrictedSourceFileException
    , _RestrictedSourceFileException

    -- ** PathDoesNotExistException
    , _PathDoesNotExistException

    -- ** InvalidResourceArnException
    , _InvalidResourceArnException

    -- ** TooManyTagsException
    , _TooManyTagsException

    -- ** EncryptionIntegrityChecksFailedException
    , _EncryptionIntegrityChecksFailedException

    -- ** SamePathRequestException
    , _SamePathRequestException

    -- ** SourceFileOrContentRequiredException
    , _SourceFileOrContentRequiredException

    -- ** InvalidMaxMergeHunksException
    , _InvalidMaxMergeHunksException

    -- ** CannotModifyApprovalRuleFromTemplateException
    , _CannotModifyApprovalRuleFromTemplateException

    -- ** InvalidReplacementContentException
    , _InvalidReplacementContentException

    -- ** ParentCommitIdOutdatedException
    , _ParentCommitIdOutdatedException

    -- ** RepositoryTriggerEventsListRequiredException
    , _RepositoryTriggerEventsListRequiredException

    -- ** CommentContentRequiredException
    , _CommentContentRequiredException

    -- ** ReactionValueRequiredException
    , _ReactionValueRequiredException

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

    -- ** InvalidConflictDetailLevelException
    , _InvalidConflictDetailLevelException

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

    -- ** ApprovalRuleTemplateNameAlreadyExistsException
    , _ApprovalRuleTemplateNameAlreadyExistsException

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

    -- ** MaximumRuleTemplatesAssociatedWithRepositoryException
    , _MaximumRuleTemplatesAssociatedWithRepositoryException

    -- ** SameFileContentException
    , _SameFileContentException

    -- ** ApprovalRuleTemplateInUseException
    , _ApprovalRuleTemplateInUseException

    -- ** MaximumNumberOfApprovalsExceededException
    , _MaximumNumberOfApprovalsExceededException

    -- ** CommitIdRequiredException
    , _CommitIdRequiredException

    -- ** FileDoesNotExistException
    , _FileDoesNotExistException

    -- ** InvalidCommitIdException
    , _InvalidCommitIdException

    -- ** InvalidTagKeysListException
    , _InvalidTagKeysListException

    -- ** FileContentAndSourceFileSpecifiedException
    , _FileContentAndSourceFileSpecifiedException

    -- ** TipOfSourceReferenceIsDifferentException
    , _TipOfSourceReferenceIsDifferentException

    -- ** RepositoryTriggerDestinationArnRequiredException
    , _RepositoryTriggerDestinationArnRequiredException

    -- ** InvalidConflictResolutionStrategyException
    , _InvalidConflictResolutionStrategyException

    -- ** InvalidClientRequestTokenException
    , _InvalidClientRequestTokenException

    -- ** MultipleConflictResolutionEntriesException
    , _MultipleConflictResolutionEntriesException

    -- ** CommitDoesNotExistException
    , _CommitDoesNotExistException

    -- ** RepositoryTriggerBranchNameListRequiredException
    , _RepositoryTriggerBranchNameListRequiredException

    -- ** ClientRequestTokenRequiredException
    , _ClientRequestTokenRequiredException

    -- ** ApprovalRuleTemplateDoesNotExistException
    , _ApprovalRuleTemplateDoesNotExistException

    -- ** TagPolicyException
    , _TagPolicyException

    -- ** InvalidMergeOptionException
    , _InvalidMergeOptionException

    -- ** CannotDeleteApprovalRuleFromTemplateException
    , _CannotDeleteApprovalRuleFromTemplateException

    -- ** CommentIdRequiredException
    , _CommentIdRequiredException

    -- ** InvalidMaxResultsException
    , _InvalidMaxResultsException

    -- ** FileTooLargeException
    , _FileTooLargeException

    -- ** ApprovalRuleTemplateNameRequiredException
    , _ApprovalRuleTemplateNameRequiredException

    -- ** MaximumFileEntriesExceededException
    , _MaximumFileEntriesExceededException

    -- ** CommitIdDoesNotExistException
    , _CommitIdDoesNotExistException

    -- ** InvalidReplacementTypeException
    , _InvalidReplacementTypeException

    -- ** InvalidRevisionIdException
    , _InvalidRevisionIdException

    -- ** RevisionNotCurrentException
    , _RevisionNotCurrentException

    -- ** InvalidApprovalRuleTemplateNameException
    , _InvalidApprovalRuleTemplateNameException

    -- ** PullRequestCannotBeApprovedByAuthorException
    , _PullRequestCannotBeApprovedByAuthorException

    -- ** MultipleRepositoriesInPullRequestException
    , _MultipleRepositoriesInPullRequestException

    -- ** RevisionIdRequiredException
    , _RevisionIdRequiredException

    -- ** FileContentSizeLimitExceededException
    , _FileContentSizeLimitExceededException

    -- ** InvalidRepositoryTriggerNameException
    , _InvalidRepositoryTriggerNameException

    -- ** RepositoryNameRequiredException
    , _RepositoryNameRequiredException

    -- ** RepositoryLimitExceededException
    , _RepositoryLimitExceededException

    -- ** TagsMapRequiredException
    , _TagsMapRequiredException

    -- ** InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryTriggerEventsException

    -- ** NumberOfRulesExceededException
    , _NumberOfRulesExceededException

    -- ** BranchNameIsTagNameException
    , _BranchNameIsTagNameException

    -- ** InvalidRepositoryNameException
    , _InvalidRepositoryNameException

    -- ** CommitIdsListRequiredException
    , _CommitIdsListRequiredException

    -- ** CommitIdsLimitExceededException
    , _CommitIdsLimitExceededException

    -- ** InvalidAuthorArnException
    , _InvalidAuthorArnException

    -- ** MaximumItemsToCompareExceededException
    , _MaximumItemsToCompareExceededException

    -- ** OverrideStatusRequiredException
    , _OverrideStatusRequiredException

    -- ** ApprovalRuleContentRequiredException
    , _ApprovalRuleContentRequiredException

    -- ** MaximumConflictResolutionEntriesExceededException
    , _MaximumConflictResolutionEntriesExceededException

    -- ** PullRequestStatusRequiredException
    , _PullRequestStatusRequiredException

    -- ** InvalidConflictResolutionException
    , _InvalidConflictResolutionException

    -- ** InvalidApprovalRuleTemplateContentException
    , _InvalidApprovalRuleTemplateContentException

    -- ** InvalidApprovalStateException
    , _InvalidApprovalStateException

    -- ** RepositoryNotAssociatedWithPullRequestException
    , _RepositoryNotAssociatedWithPullRequestException

    -- ** MaximumFileContentToLoadExceededException
    , _MaximumFileContentToLoadExceededException

    -- ** TitleRequiredException
    , _TitleRequiredException

    -- ** InvalidOverrideStatusException
    , _InvalidOverrideStatusException

    -- ** InvalidFilePositionException
    , _InvalidFilePositionException

    -- ** CommentDeletedException
    , _CommentDeletedException

    -- ** ParentCommitDoesNotExistException
    , _ParentCommitDoesNotExistException

    -- ** InvalidApprovalRuleTemplateDescriptionException
    , _InvalidApprovalRuleTemplateDescriptionException

    -- ** ResourceArnRequiredException
    , _ResourceArnRequiredException

    -- ** InvalidMaxConflictFilesException
    , _InvalidMaxConflictFilesException

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

    -- ** MergeBranchesBySquash 
    , module Network.AWS.CodeCommit.MergeBranchesBySquash

    -- ** GetCommit 
    , module Network.AWS.CodeCommit.GetCommit

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories 
    , module Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories

    -- ** GetCommentReactions 
    , module Network.AWS.CodeCommit.GetCommentReactions

    -- ** GetApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.GetApprovalRuleTemplate

    -- ** DisassociateApprovalRuleTemplateFromRepository 
    , module Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository

    -- ** GetBranch 
    , module Network.AWS.CodeCommit.GetBranch

    -- ** GetDifferences (Paginated)
    , module Network.AWS.CodeCommit.GetDifferences

    -- ** ListTagsForResource 
    , module Network.AWS.CodeCommit.ListTagsForResource

    -- ** GetPullRequest 
    , module Network.AWS.CodeCommit.GetPullRequest

    -- ** OverridePullRequestApprovalRules 
    , module Network.AWS.CodeCommit.OverridePullRequestApprovalRules

    -- ** ListPullRequests (Paginated)
    , module Network.AWS.CodeCommit.ListPullRequests

    -- ** CreateCommit 
    , module Network.AWS.CodeCommit.CreateCommit

    -- ** UpdatePullRequestApprovalState 
    , module Network.AWS.CodeCommit.UpdatePullRequestApprovalState

    -- ** EvaluatePullRequestApprovalRules 
    , module Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules

    -- ** GetComment 
    , module Network.AWS.CodeCommit.GetComment

    -- ** CreateApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.CreateApprovalRuleTemplate

    -- ** DeleteBranch 
    , module Network.AWS.CodeCommit.DeleteBranch

    -- ** UpdateRepositoryDescription 
    , module Network.AWS.CodeCommit.UpdateRepositoryDescription

    -- ** CreateBranch 
    , module Network.AWS.CodeCommit.CreateBranch

    -- ** GetFolder 
    , module Network.AWS.CodeCommit.GetFolder

    -- ** CreatePullRequest 
    , module Network.AWS.CodeCommit.CreatePullRequest

    -- ** DeleteApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.DeleteApprovalRuleTemplate

    -- ** ListBranches (Paginated)
    , module Network.AWS.CodeCommit.ListBranches

    -- ** BatchGetCommits 
    , module Network.AWS.CodeCommit.BatchGetCommits

    -- ** PutCommentReaction 
    , module Network.AWS.CodeCommit.PutCommentReaction

    -- ** UpdatePullRequestDescription 
    , module Network.AWS.CodeCommit.UpdatePullRequestDescription

    -- ** ListRepositories (Paginated)
    , module Network.AWS.CodeCommit.ListRepositories

    -- ** CreateRepository 
    , module Network.AWS.CodeCommit.CreateRepository

    -- ** UpdateDefaultBranch 
    , module Network.AWS.CodeCommit.UpdateDefaultBranch

    -- ** GetMergeOptions 
    , module Network.AWS.CodeCommit.GetMergeOptions

    -- ** CreatePullRequestApprovalRule 
    , module Network.AWS.CodeCommit.CreatePullRequestApprovalRule

    -- ** PostCommentReply 
    , module Network.AWS.CodeCommit.PostCommentReply

    -- ** UpdateApprovalRuleTemplateContent 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent

    -- ** CreateUnreferencedMergeCommit 
    , module Network.AWS.CodeCommit.CreateUnreferencedMergeCommit

    -- ** ListRepositoriesForApprovalRuleTemplate 
    , module Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate

    -- ** GetRepository 
    , module Network.AWS.CodeCommit.GetRepository

    -- ** BatchDescribeMergeConflicts 
    , module Network.AWS.CodeCommit.BatchDescribeMergeConflicts

    -- ** DeletePullRequestApprovalRule 
    , module Network.AWS.CodeCommit.DeletePullRequestApprovalRule

    -- ** GetRepositoryTriggers 
    , module Network.AWS.CodeCommit.GetRepositoryTriggers

    -- ** UpdateApprovalRuleTemplateName 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName

    -- ** PutFile 
    , module Network.AWS.CodeCommit.PutFile

    -- ** DeleteFile 
    , module Network.AWS.CodeCommit.DeleteFile

    -- ** GetCommentsForComparedCommit (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForComparedCommit

    -- ** GetMergeCommit 
    , module Network.AWS.CodeCommit.GetMergeCommit

    -- ** TestRepositoryTriggers 
    , module Network.AWS.CodeCommit.TestRepositoryTriggers

    -- ** MergePullRequestBySquash 
    , module Network.AWS.CodeCommit.MergePullRequestBySquash

    -- ** UpdateComment 
    , module Network.AWS.CodeCommit.UpdateComment

    -- ** PostCommentForComparedCommit 
    , module Network.AWS.CodeCommit.PostCommentForComparedCommit

    -- ** MergeBranchesByFastForward 
    , module Network.AWS.CodeCommit.MergeBranchesByFastForward

    -- ** UpdatePullRequestTitle 
    , module Network.AWS.CodeCommit.UpdatePullRequestTitle

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories 
    , module Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories

    -- ** UpdatePullRequestApprovalRuleContent 
    , module Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent

    -- ** GetBlob 
    , module Network.AWS.CodeCommit.GetBlob

    -- ** AssociateApprovalRuleTemplateWithRepository 
    , module Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository

    -- ** PutRepositoryTriggers 
    , module Network.AWS.CodeCommit.PutRepositoryTriggers

    -- ** ListApprovalRuleTemplates 
    , module Network.AWS.CodeCommit.ListApprovalRuleTemplates

    -- ** DescribeMergeConflicts 
    , module Network.AWS.CodeCommit.DescribeMergeConflicts

    -- ** TagResource 
    , module Network.AWS.CodeCommit.TagResource

    -- ** MergeBranchesByThreeWay 
    , module Network.AWS.CodeCommit.MergeBranchesByThreeWay

    -- ** GetFile 
    , module Network.AWS.CodeCommit.GetFile

    -- ** UntagResource 
    , module Network.AWS.CodeCommit.UntagResource

    -- ** GetMergeConflicts 
    , module Network.AWS.CodeCommit.GetMergeConflicts

    -- ** DeleteRepository 
    , module Network.AWS.CodeCommit.DeleteRepository

    -- ** DeleteCommentContent 
    , module Network.AWS.CodeCommit.DeleteCommentContent

    -- ** MergePullRequestByThreeWay 
    , module Network.AWS.CodeCommit.MergePullRequestByThreeWay

    -- ** DescribePullRequestEvents (Paginated)
    , module Network.AWS.CodeCommit.DescribePullRequestEvents

    -- ** BatchGetRepositories 
    , module Network.AWS.CodeCommit.BatchGetRepositories

    -- ** UpdateApprovalRuleTemplateDescription 
    , module Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription

    -- ** GetPullRequestOverrideState 
    , module Network.AWS.CodeCommit.GetPullRequestOverrideState

    -- ** GetPullRequestApprovalStates 
    , module Network.AWS.CodeCommit.GetPullRequestApprovalStates

    -- ** GetCommentsForPullRequest (Paginated)
    , module Network.AWS.CodeCommit.GetCommentsForPullRequest

    -- ** UpdatePullRequestStatus 
    , module Network.AWS.CodeCommit.UpdatePullRequestStatus

    -- ** ListAssociatedApprovalRuleTemplatesForRepository 
    , module Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository

    -- * Types

    -- ** CommitId
    , CommitId (..)

    -- ** SymbolicLink
    , SymbolicLink (..)
    , mkSymbolicLink
    , slAbsolutePath
    , slBlobId
    , slFileMode
    , slRelativePath

    -- ** Email
    , Email (..)

    -- ** PullRequestMergedStateChangedEventMetadata
    , PullRequestMergedStateChangedEventMetadata (..)
    , mkPullRequestMergedStateChangedEventMetadata
    , prmscemDestinationReference
    , prmscemMergeMetadata
    , prmscemRepositoryName

    -- ** RuleContentSha256
    , RuleContentSha256 (..)

    -- ** RepositoryDescription
    , RepositoryDescription (..)

    -- ** ApprovalRuleTemplateId
    , ApprovalRuleTemplateId (..)

    -- ** CommentsForPullRequest
    , CommentsForPullRequest (..)
    , mkCommentsForPullRequest
    , cfprAfterBlobId
    , cfprAfterCommitId
    , cfprBeforeBlobId
    , cfprBeforeCommitId
    , cfprComments
    , cfprLocation
    , cfprPullRequestId
    , cfprRepositoryName

    -- ** ApprovalRuleTemplateDescription
    , ApprovalRuleTemplateDescription (..)

    -- ** UserInfo
    , UserInfo (..)
    , mkUserInfo
    , uiDate
    , uiEmail
    , uiName

    -- ** ReactionValue
    , ReactionValue (..)

    -- ** ReactionValueFormats
    , ReactionValueFormats (..)
    , mkReactionValueFormats
    , rvfEmoji
    , rvfShortCode
    , rvfUnicode

    -- ** SortByEnum
    , SortByEnum (..)

    -- ** ConflictDetailLevelTypeEnum
    , ConflictDetailLevelTypeEnum (..)

    -- ** Location
    , Location (..)
    , mkLocation
    , lFilePath
    , lFilePosition
    , lRelativeFileVersion

    -- ** PullRequestTarget
    , PullRequestTarget (..)
    , mkPullRequestTarget
    , prtDestinationCommit
    , prtDestinationReference
    , prtMergeBase
    , prtMergeMetadata
    , prtRepositoryName
    , prtSourceCommit
    , prtSourceReference

    -- ** PullRequestCreatedEventMetadata
    , PullRequestCreatedEventMetadata (..)
    , mkPullRequestCreatedEventMetadata
    , prcemDestinationCommitId
    , prcemMergeBase
    , prcemRepositoryName
    , prcemSourceCommitId

    -- ** MergeMetadata
    , MergeMetadata (..)
    , mkMergeMetadata
    , mmIsMerged
    , mmMergeCommitId
    , mmMergeOption
    , mmMergedBy

    -- ** ReplacementTypeEnum
    , ReplacementTypeEnum (..)

    -- ** Arn
    , Arn (..)

    -- ** RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- ** RepositoryTriggerName
    , RepositoryTriggerName (..)

    -- ** Path
    , Path (..)

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    , BatchDisassociateApprovalRuleTemplateFromRepositoriesError (..)
    , mkBatchDisassociateApprovalRuleTemplateFromRepositoriesError
    , bdartfreErrorCode
    , bdartfreErrorMessage
    , bdartfreRepositoryName

    -- ** CloneUrlHttp
    , CloneUrlHttp (..)

    -- ** PullRequestEventType
    , PullRequestEventType (..)

    -- ** ApprovalRuleEventMetadata
    , ApprovalRuleEventMetadata (..)
    , mkApprovalRuleEventMetadata
    , aremApprovalRuleContent
    , aremApprovalRuleId
    , aremApprovalRuleName

    -- ** ApprovalRuleTemplateContent
    , ApprovalRuleTemplateContent (..)

    -- ** ApprovalState
    , ApprovalState (..)

    -- ** FileSizes
    , FileSizes (..)
    , mkFileSizes
    , fsBase
    , fsDestination
    , fsSource

    -- ** MergeHunk
    , MergeHunk (..)
    , mkMergeHunk
    , mhBase
    , mhDestination
    , mhIsConflict
    , mhSource

    -- ** BatchGetCommitsError
    , BatchGetCommitsError (..)
    , mkBatchGetCommitsError
    , bgceCommitId
    , bgceErrorCode
    , bgceErrorMessage

    -- ** MergeOptionTypeEnum
    , MergeOptionTypeEnum (..)

    -- ** ObjectId
    , ObjectId (..)

    -- ** PullRequestStatusEnum
    , PullRequestStatusEnum (..)

    -- ** BranchName
    , BranchName (..)

    -- ** PullRequestStatusChangedEventMetadata
    , PullRequestStatusChangedEventMetadata (..)
    , mkPullRequestStatusChangedEventMetadata
    , prscemPullRequestStatus

    -- ** RepositoryTriggerExecutionFailureMessage
    , RepositoryTriggerExecutionFailureMessage (..)

    -- ** SetFileModeEntry
    , SetFileModeEntry (..)
    , mkSetFileModeEntry
    , sfmeFilePath
    , sfmeFileMode

    -- ** IsBinaryFile
    , IsBinaryFile (..)
    , mkIsBinaryFile
    , ibfBase
    , ibfDestination
    , ibfSource

    -- ** Mode
    , Mode (..)

    -- ** MergeOperations
    , MergeOperations (..)
    , mkMergeOperations
    , moDestination
    , moSource

    -- ** BlobMetadata
    , BlobMetadata (..)
    , mkBlobMetadata
    , bmBlobId
    , bmMode
    , bmPath

    -- ** SubModule
    , SubModule (..)
    , mkSubModule
    , smAbsolutePath
    , smCommitId
    , smRelativePath

    -- ** Folder
    , Folder (..)
    , mkFolder
    , ffAbsolutePath
    , ffRelativePath
    , ffTreeId

    -- ** ObjectTypeEnum
    , ObjectTypeEnum (..)

    -- ** ApprovalRuleName
    , ApprovalRuleName (..)

    -- ** CommentsForComparedCommit
    , CommentsForComparedCommit (..)
    , mkCommentsForComparedCommit
    , cfccAfterBlobId
    , cfccAfterCommitId
    , cfccBeforeBlobId
    , cfccBeforeCommitId
    , cfccComments
    , cfccLocation
    , cfccRepositoryName

    -- ** ApprovalRule
    , ApprovalRule (..)
    , mkApprovalRule
    , arApprovalRuleContent
    , arApprovalRuleId
    , arApprovalRuleName
    , arCreationDate
    , arLastModifiedDate
    , arLastModifiedUser
    , arOriginApprovalRuleTemplate
    , arRuleContentSha256

    -- ** TagValue
    , TagValue (..)

    -- ** Content
    , Content (..)

    -- ** AdditionalData
    , AdditionalData (..)

    -- ** ReactionForComment
    , ReactionForComment (..)
    , mkReactionForComment
    , rfcReaction
    , rfcReactionUsers
    , rfcReactionsFromDeletedUsersCount

    -- ** ReplaceContentEntry
    , ReplaceContentEntry (..)
    , mkReplaceContentEntry
    , rceFilePath
    , rceReplacementType
    , rceContent
    , rceFileMode

    -- ** PullRequestId
    , PullRequestId (..)

    -- ** AccountId
    , AccountId (..)

    -- ** NextToken
    , NextToken (..)

    -- ** BatchDescribeMergeConflictsError
    , BatchDescribeMergeConflictsError (..)
    , mkBatchDescribeMergeConflictsError
    , bdmceFilePath
    , bdmceExceptionName
    , bdmceMessage

    -- ** PullRequest
    , PullRequest (..)
    , mkPullRequest
    , prApprovalRules
    , prAuthorArn
    , prClientRequestToken
    , prCreationDate
    , prDescription
    , prLastActivityDate
    , prPullRequestId
    , prPullRequestStatus
    , prPullRequestTargets
    , prRevisionId
    , prTitle

    -- ** ReferenceName
    , ReferenceName (..)

    -- ** Date
    , Date (..)

    -- ** ApprovalRuleId
    , ApprovalRuleId (..)

    -- ** Difference
    , Difference (..)
    , mkDifference
    , dAfterBlob
    , dBeforeBlob
    , dChangeType

    -- ** RepositoryMetadata
    , RepositoryMetadata (..)
    , mkRepositoryMetadata
    , rmArn
    , rmAccountId
    , rmCloneUrlHttp
    , rmCloneUrlSsh
    , rmCreationDate
    , rmDefaultBranch
    , rmLastModifiedDate
    , rmRepositoryDescription
    , rmRepositoryId
    , rmRepositoryName

    -- ** ResourceArn
    , ResourceArn (..)

    -- ** Approval
    , Approval (..)
    , mkApproval
    , aApprovalState
    , aUserArn

    -- ** RepositoryId
    , RepositoryId (..)

    -- ** SourceFileSpecifier
    , SourceFileSpecifier (..)
    , mkSourceFileSpecifier
    , sfsFilePath
    , sfsIsMove

    -- ** Name
    , Name (..)

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    , BatchAssociateApprovalRuleTemplateWithRepositoriesError (..)
    , mkBatchAssociateApprovalRuleTemplateWithRepositoriesError
    , baartwreErrorCode
    , baartwreErrorMessage
    , baartwreRepositoryName

    -- ** RepositoryTrigger
    , RepositoryTrigger (..)
    , mkRepositoryTrigger
    , rtName
    , rtDestinationArn
    , rtEvents
    , rtBranches
    , rtCustomData

    -- ** RepositoryNameIdPair
    , RepositoryNameIdPair (..)
    , mkRepositoryNameIdPair
    , rnipRepositoryId
    , rnipRepositoryName

    -- ** BranchInfo
    , BranchInfo (..)
    , mkBranchInfo
    , biBranchName
    , biCommitId

    -- ** ErrorCode
    , ErrorCode (..)

    -- ** OrderEnum
    , OrderEnum (..)

    -- ** OriginApprovalRuleTemplate
    , OriginApprovalRuleTemplate (..)
    , mkOriginApprovalRuleTemplate
    , oartApprovalRuleTemplateId
    , oartApprovalRuleTemplateName

    -- ** RepositoryName
    , RepositoryName (..)

    -- ** ChangeTypeEnum
    , ChangeTypeEnum (..)

    -- ** TagKey
    , TagKey (..)

    -- ** ConflictResolution
    , ConflictResolution (..)
    , mkConflictResolution
    , crDeleteFiles
    , crReplaceContents
    , crSetFileModes

    -- ** Title
    , Title (..)

    -- ** CloneUrlSsh
    , CloneUrlSsh (..)

    -- ** ConflictResolutionStrategyTypeEnum
    , ConflictResolutionStrategyTypeEnum (..)

    -- ** RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure (..)
    , mkRepositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- ** ApprovalRuleContent
    , ApprovalRuleContent (..)

    -- ** PullRequestEvent
    , PullRequestEvent (..)
    , mkPullRequestEvent
    , preActorArn
    , preApprovalRuleEventMetadata
    , preApprovalRuleOverriddenEventMetadata
    , preApprovalStateChangedEventMetadata
    , preEventDate
    , prePullRequestCreatedEventMetadata
    , prePullRequestEventType
    , prePullRequestId
    , prePullRequestMergedStateChangedEventMetadata
    , prePullRequestSourceReferenceUpdatedEventMetadata
    , prePullRequestStatusChangedEventMetadata

    -- ** ExceptionName
    , ExceptionName (..)

    -- ** OverrideStatus
    , OverrideStatus (..)

    -- ** DeleteFileEntry
    , DeleteFileEntry (..)
    , mkDeleteFileEntry
    , dfeFilePath

    -- ** ApprovalStateChangedEventMetadata
    , ApprovalStateChangedEventMetadata (..)
    , mkApprovalStateChangedEventMetadata
    , ascemApprovalStatus
    , ascemRevisionId

    -- ** FileMetadata
    , FileMetadata (..)
    , mkFileMetadata
    , fmAbsolutePath
    , fmBlobId
    , fmFileMode

    -- ** ConflictMetadata
    , ConflictMetadata (..)
    , mkConflictMetadata
    , cmContentConflict
    , cmFileModeConflict
    , cmFileModes
    , cmFilePath
    , cmFileSizes
    , cmIsBinaryFile
    , cmMergeOperations
    , cmNumberOfConflicts
    , cmObjectTypeConflict
    , cmObjectTypes

    -- ** PutFileEntry
    , PutFileEntry (..)
    , mkPutFileEntry
    , pfeFilePath
    , pfeFileContent
    , pfeFileMode
    , pfeSourceFile

    -- ** ErrorMessage
    , ErrorMessage (..)

    -- ** Message
    , Message (..)

    -- ** RelativeFileVersionEnum
    , RelativeFileVersionEnum (..)

    -- ** ClientRequestToken
    , ClientRequestToken (..)

    -- ** Comment
    , Comment (..)
    , mkComment
    , cAuthorArn
    , cCallerReactions
    , cClientRequestToken
    , cCommentId
    , cContent
    , cCreationDate
    , cDeleted
    , cInReplyTo
    , cLastModifiedDate
    , cReactionCounts

    -- ** CommitName
    , CommitName (..)

    -- ** CommentId
    , CommentId (..)

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    , PullRequestSourceReferenceUpdatedEventMetadata (..)
    , mkPullRequestSourceReferenceUpdatedEventMetadata
    , prsruemAfterCommitId
    , prsruemBeforeCommitId
    , prsruemMergeBase
    , prsruemRepositoryName

    -- ** ApprovalRuleOverriddenEventMetadata
    , ApprovalRuleOverriddenEventMetadata (..)
    , mkApprovalRuleOverriddenEventMetadata
    , aroemOverrideStatus
    , aroemRevisionId

    -- ** Description
    , Description (..)

    -- ** Conflict
    , Conflict (..)
    , mkConflict
    , cConflictMetadata
    , cMergeHunks

    -- ** Evaluation
    , Evaluation (..)
    , mkEvaluation
    , eApprovalRulesNotSatisfied
    , eApprovalRulesSatisfied
    , eApproved
    , eOverridden

    -- ** ObjectTypes
    , ObjectTypes (..)
    , mkObjectTypes
    , otBase
    , otDestination
    , otSource

    -- ** ApprovalRuleTemplateName
    , ApprovalRuleTemplateName (..)

    -- ** FileModes
    , FileModes (..)
    , mkFileModes
    , fmBase
    , fmDestination
    , fmSource

    -- ** File
    , File (..)
    , mkFile
    , fAbsolutePath
    , fBlobId
    , fFileMode
    , fRelativePath

    -- ** RevisionId
    , RevisionId (..)

    -- ** FileModeTypeEnum
    , FileModeTypeEnum (..)

    -- ** HunkContent
    , HunkContent (..)

    -- ** MergeHunkDetail
    , MergeHunkDetail (..)
    , mkMergeHunkDetail
    , mhdEndLine
    , mhdHunkContent
    , mhdStartLine

    -- ** ApprovalRuleTemplate
    , ApprovalRuleTemplate (..)
    , mkApprovalRuleTemplate
    , artApprovalRuleTemplateContent
    , artApprovalRuleTemplateDescription
    , artApprovalRuleTemplateId
    , artApprovalRuleTemplateName
    , artCreationDate
    , artLastModifiedDate
    , artLastModifiedUser
    , artRuleContentSha256

    -- ** Commit
    , Commit (..)
    , mkCommit
    , cAdditionalData
    , cAuthor
    , cCommitId
    , cCommitter
    , cMessage
    , cParents
    , cTreeId

    -- ** Target
    , Target (..)
    , mkTarget
    , tRepositoryName
    , tSourceReference
    , tDestinationReference

    -- ** SourceCommitSpecifier
    , SourceCommitSpecifier (..)

    -- ** DestinationCommitSpecifier
    , DestinationCommitSpecifier (..)

    -- ** TargetBranch
    , TargetBranch (..)

    -- ** DestinationCommitId
    , DestinationCommitId (..)

    -- ** SourceCommitId
    , SourceCommitId (..)

    -- ** BaseCommitId
    , BaseCommitId (..)

    -- ** AuthorName
    , AuthorName (..)

    -- ** CommitMessage
    , CommitMessage (..)

    -- ** AbsolutePath
    , AbsolutePath (..)

    -- ** BlobId
    , BlobId (..)

    -- ** RelativePath
    , RelativePath (..)

    -- ** DestinationReference
    , DestinationReference (..)

    -- ** ConfigurationId
    , ConfigurationId (..)

    -- ** NewRuleContent
    , NewRuleContent (..)

    -- ** AfterBlobId
    , AfterBlobId (..)

    -- ** BeforeBlobId
    , BeforeBlobId (..)

    -- ** TreeId
    , TreeId (..)

    -- ** FilePath
    , FilePath (..)

    -- ** InReplyTo
    , InReplyTo (..)

    -- ** Emoji
    , Emoji (..)

    -- ** ShortCode
    , ShortCode (..)

    -- ** Unicode
    , Unicode (..)

    -- ** SourceReference
    , SourceReference (..)

    -- ** MergedBy
    , MergedBy (..)

    -- ** DefaultBranchName
    , DefaultBranchName (..)

    -- ** MergedCommitId
    , MergedCommitId (..)

    -- ** CommitSpecifier
    , CommitSpecifier (..)

    -- ** AfterCommitSpecifier
    , AfterCommitSpecifier (..)

    -- ** BeforeCommitSpecifier
    , BeforeCommitSpecifier (..)

    -- ** OldName
    , OldName (..)

    -- ** NewName
    , NewName (..)

    -- ** CustomData
    , CustomData (..)

    -- * Serialization types
    , Lude.Base64 (..)
    , Lude._Base64
    , Lude.Sensitive (..)
    , Lude._Sensitive
    , Lude.UTCTime
    , Lude.NominalDiffTime
    ) where

import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.Waiters
import Network.AWS.CodeCommit.MergePullRequestByFastForward
import Network.AWS.CodeCommit.UpdateRepositoryName
import Network.AWS.CodeCommit.PostCommentForPullRequest
import Network.AWS.CodeCommit.MergeBranchesBySquash
import Network.AWS.CodeCommit.GetCommit
import Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Network.AWS.CodeCommit.GetCommentReactions
import Network.AWS.CodeCommit.GetApprovalRuleTemplate
import Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Network.AWS.CodeCommit.GetBranch
import Network.AWS.CodeCommit.GetDifferences
import Network.AWS.CodeCommit.ListTagsForResource
import Network.AWS.CodeCommit.GetPullRequest
import Network.AWS.CodeCommit.OverridePullRequestApprovalRules
import Network.AWS.CodeCommit.ListPullRequests
import Network.AWS.CodeCommit.CreateCommit
import Network.AWS.CodeCommit.UpdatePullRequestApprovalState
import Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
import Network.AWS.CodeCommit.GetComment
import Network.AWS.CodeCommit.CreateApprovalRuleTemplate
import Network.AWS.CodeCommit.DeleteBranch
import Network.AWS.CodeCommit.UpdateRepositoryDescription
import Network.AWS.CodeCommit.CreateBranch
import Network.AWS.CodeCommit.GetFolder
import Network.AWS.CodeCommit.CreatePullRequest
import Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
import Network.AWS.CodeCommit.ListBranches
import Network.AWS.CodeCommit.BatchGetCommits
import Network.AWS.CodeCommit.PutCommentReaction
import Network.AWS.CodeCommit.UpdatePullRequestDescription
import Network.AWS.CodeCommit.ListRepositories
import Network.AWS.CodeCommit.CreateRepository
import Network.AWS.CodeCommit.UpdateDefaultBranch
import Network.AWS.CodeCommit.GetMergeOptions
import Network.AWS.CodeCommit.CreatePullRequestApprovalRule
import Network.AWS.CodeCommit.PostCommentReply
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
import Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
import Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Network.AWS.CodeCommit.GetRepository
import Network.AWS.CodeCommit.BatchDescribeMergeConflicts
import Network.AWS.CodeCommit.DeletePullRequestApprovalRule
import Network.AWS.CodeCommit.GetRepositoryTriggers
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
import Network.AWS.CodeCommit.PutFile
import Network.AWS.CodeCommit.DeleteFile
import Network.AWS.CodeCommit.GetCommentsForComparedCommit
import Network.AWS.CodeCommit.GetMergeCommit
import Network.AWS.CodeCommit.TestRepositoryTriggers
import Network.AWS.CodeCommit.MergePullRequestBySquash
import Network.AWS.CodeCommit.UpdateComment
import Network.AWS.CodeCommit.PostCommentForComparedCommit
import Network.AWS.CodeCommit.MergeBranchesByFastForward
import Network.AWS.CodeCommit.UpdatePullRequestTitle
import Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
import Network.AWS.CodeCommit.GetBlob
import Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Network.AWS.CodeCommit.PutRepositoryTriggers
import Network.AWS.CodeCommit.ListApprovalRuleTemplates
import Network.AWS.CodeCommit.DescribeMergeConflicts
import Network.AWS.CodeCommit.TagResource
import Network.AWS.CodeCommit.MergeBranchesByThreeWay
import Network.AWS.CodeCommit.GetFile
import Network.AWS.CodeCommit.UntagResource
import Network.AWS.CodeCommit.GetMergeConflicts
import Network.AWS.CodeCommit.DeleteRepository
import Network.AWS.CodeCommit.DeleteCommentContent
import Network.AWS.CodeCommit.MergePullRequestByThreeWay
import Network.AWS.CodeCommit.DescribePullRequestEvents
import Network.AWS.CodeCommit.BatchGetRepositories
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
import Network.AWS.CodeCommit.GetPullRequestOverrideState
import Network.AWS.CodeCommit.GetPullRequestApprovalStates
import Network.AWS.CodeCommit.GetCommentsForPullRequest
import Network.AWS.CodeCommit.UpdatePullRequestStatus
import Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import qualified Network.AWS.Prelude as Lude

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
