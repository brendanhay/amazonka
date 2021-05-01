{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeCommit
--
-- This is the /AWS CodeCommit API Reference/. This reference provides
-- descriptions of the operations and data types for AWS CodeCommit API
-- along with usage examples.
--
-- You can use the AWS CodeCommit API to work with the following objects:
--
-- Repositories, by calling the following:
--
-- -   BatchGetRepositories, which returns information about one or more
--     repositories associated with your AWS account.
--
-- -   CreateRepository, which creates an AWS CodeCommit repository.
--
-- -   DeleteRepository, which deletes an AWS CodeCommit repository.
--
-- -   GetRepository, which returns information about a specified
--     repository.
--
-- -   ListRepositories, which lists all AWS CodeCommit repositories
--     associated with your AWS account.
--
-- -   UpdateRepositoryDescription, which sets or updates the description
--     of the repository.
--
-- -   UpdateRepositoryName, which changes the name of the repository. If
--     you change the name of a repository, no other users of that
--     repository can access it until you send them the new HTTPS or SSH
--     URL to use.
--
-- Branches, by calling the following:
--
-- -   CreateBranch, which creates a branch in a specified repository.
--
-- -   DeleteBranch, which deletes the specified branch in a repository
--     unless it is the default branch.
--
-- -   GetBranch, which returns information about a specified branch.
--
-- -   ListBranches, which lists all branches for a specified repository.
--
-- -   UpdateDefaultBranch, which changes the default branch for a
--     repository.
--
-- Files, by calling the following:
--
-- -   DeleteFile, which deletes the content of a specified file from a
--     specified branch.
--
-- -   GetBlob, which returns the base-64 encoded content of an individual
--     Git blob object in a repository.
--
-- -   GetFile, which returns the base-64 encoded content of a specified
--     file.
--
-- -   GetFolder, which returns the contents of a specified folder or
--     directory.
--
-- -   PutFile, which adds or modifies a single file in a specified
--     repository and branch.
--
-- Commits, by calling the following:
--
-- -   BatchGetCommits, which returns information about one or more commits
--     in a repository.
--
-- -   CreateCommit, which creates a commit for changes to a repository.
--
-- -   GetCommit, which returns information about a commit, including
--     commit messages and author and committer information.
--
-- -   GetDifferences, which returns information about the differences in a
--     valid commit specifier (such as a branch, tag, HEAD, commit ID, or
--     other fully qualified reference).
--
-- Merges, by calling the following:
--
-- -   BatchDescribeMergeConflicts, which returns information about
--     conflicts in a merge between commits in a repository.
--
-- -   CreateUnreferencedMergeCommit, which creates an unreferenced commit
--     between two branches or commits for the purpose of comparing them
--     and identifying any potential conflicts.
--
-- -   DescribeMergeConflicts, which returns information about merge
--     conflicts between the base, source, and destination versions of a
--     file in a potential merge.
--
-- -   GetMergeCommit, which returns information about the merge between a
--     source and destination commit.
--
-- -   GetMergeConflicts, which returns information about merge conflicts
--     between the source and destination branch in a pull request.
--
-- -   GetMergeOptions, which returns information about the available merge
--     options between two branches or commit specifiers.
--
-- -   MergeBranchesByFastForward, which merges two branches using the
--     fast-forward merge option.
--
-- -   MergeBranchesBySquash, which merges two branches using the squash
--     merge option.
--
-- -   MergeBranchesByThreeWay, which merges two branches using the
--     three-way merge option.
--
-- Pull requests, by calling the following:
--
-- -   CreatePullRequest, which creates a pull request in a specified
--     repository.
--
-- -   CreatePullRequestApprovalRule, which creates an approval rule for a
--     specified pull request.
--
-- -   DeletePullRequestApprovalRule, which deletes an approval rule for a
--     specified pull request.
--
-- -   DescribePullRequestEvents, which returns information about one or
--     more pull request events.
--
-- -   EvaluatePullRequestApprovalRules, which evaluates whether a pull
--     request has met all the conditions specified in its associated
--     approval rules.
--
-- -   GetCommentsForPullRequest, which returns information about comments
--     on a specified pull request.
--
-- -   GetPullRequest, which returns information about a specified pull
--     request.
--
-- -   GetPullRequestApprovalStates, which returns information about the
--     approval states for a specified pull request.
--
-- -   GetPullRequestOverrideState, which returns information about whether
--     approval rules have been set aside (overriden) for a pull request,
--     and if so, the Amazon Resource Name (ARN) of the user or identity
--     that overrode the rules and their requirements for the pull request.
--
-- -   ListPullRequests, which lists all pull requests for a repository.
--
-- -   MergePullRequestByFastForward, which merges the source destination
--     branch of a pull request into the specified destination branch for
--     that pull request using the fast-forward merge option.
--
-- -   MergePullRequestBySquash, which merges the source destination branch
--     of a pull request into the specified destination branch for that
--     pull request using the squash merge option.
--
-- -   MergePullRequestByThreeWay. which merges the source destination
--     branch of a pull request into the specified destination branch for
--     that pull request using the three-way merge option.
--
-- -   OverridePullRequestApprovalRules, which sets aside all approval rule
--     requirements for a pull request.
--
-- -   PostCommentForPullRequest, which posts a comment to a pull request
--     at the specified line, file, or request.
--
-- -   UpdatePullRequestApprovalRuleContent, which updates the structure of
--     an approval rule for a pull request.
--
-- -   UpdatePullRequestApprovalState, which updates the state of an
--     approval on a pull request.
--
-- -   UpdatePullRequestDescription, which updates the description of a
--     pull request.
--
-- -   UpdatePullRequestStatus, which updates the status of a pull request.
--
-- -   UpdatePullRequestTitle, which updates the title of a pull request.
--
-- Approval rule templates, by calling the following:
--
-- -   AssociateApprovalRuleTemplateWithRepository, which associates a
--     template with a specified repository. After the template is
--     associated with a repository, AWS CodeCommit creates approval rules
--     that match the template conditions on every pull request created in
--     the specified repository.
--
-- -   BatchAssociateApprovalRuleTemplateWithRepositories, which associates
--     a template with one or more specified repositories. After the
--     template is associated with a repository, AWS CodeCommit creates
--     approval rules that match the template conditions on every pull
--     request created in the specified repositories.
--
-- -   BatchDisassociateApprovalRuleTemplateFromRepositories, which removes
--     the association between a template and specified repositories so
--     that approval rules based on the template are not automatically
--     created when pull requests are created in those repositories.
--
-- -   CreateApprovalRuleTemplate, which creates a template for approval
--     rules that can then be associated with one or more repositories in
--     your AWS account.
--
-- -   DeleteApprovalRuleTemplate, which deletes the specified template. It
--     does not remove approval rules on pull requests already created with
--     the template.
--
-- -   DisassociateApprovalRuleTemplateFromRepository, which removes the
--     association between a template and a repository so that approval
--     rules based on the template are not automatically created when pull
--     requests are created in the specified repository.
--
-- -   GetApprovalRuleTemplate, which returns information about an approval
--     rule template.
--
-- -   ListApprovalRuleTemplates, which lists all approval rule templates
--     in the AWS Region in your AWS account.
--
-- -   ListAssociatedApprovalRuleTemplatesForRepository, which lists all
--     approval rule templates that are associated with a specified
--     repository.
--
-- -   ListRepositoriesForApprovalRuleTemplate, which lists all
--     repositories associated with the specified approval rule template.
--
-- -   UpdateApprovalRuleTemplateDescription, which updates the description
--     of an approval rule template.
--
-- -   UpdateApprovalRuleTemplateName, which updates the name of an
--     approval rule template.
--
-- -   UpdateApprovalRuleTemplateContent, which updates the content of an
--     approval rule template.
--
-- Comments in a repository, by calling the following:
--
-- -   DeleteCommentContent, which deletes the content of a comment on a
--     commit in a repository.
--
-- -   GetComment, which returns information about a comment on a commit.
--
-- -   GetCommentReactions, which returns information about emoji reactions
--     to comments.
--
-- -   GetCommentsForComparedCommit, which returns information about
--     comments on the comparison between two commit specifiers in a
--     repository.
--
-- -   PostCommentForComparedCommit, which creates a comment on the
--     comparison between two commit specifiers in a repository.
--
-- -   PostCommentReply, which creates a reply to a comment.
--
-- -   PutCommentReaction, which creates or updates an emoji reaction to a
--     comment.
--
-- -   UpdateComment, which updates the content of a comment on a commit in
--     a repository.
--
-- Tags used to tag resources in AWS CodeCommit (not Git tags), by calling
-- the following:
--
-- -   ListTagsForResource, which gets information about AWS tags for a
--     specified Amazon Resource Name (ARN) in AWS CodeCommit.
--
-- -   TagResource, which adds or updates tags for a resource in AWS
--     CodeCommit.
--
-- -   UntagResource, which removes tags for a resource in AWS CodeCommit.
--
-- Triggers, by calling the following:
--
-- -   GetRepositoryTriggers, which returns information about triggers
--     configured for a repository.
--
-- -   PutRepositoryTriggers, which replaces all triggers for a repository
--     and can be used to create or delete triggers.
--
-- -   TestRepositoryTriggers, which tests the functionality of a
--     repository trigger by sending data to the trigger target.
--
-- For information about how to use AWS CodeCommit, see the
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.
module Network.AWS.CodeCommit
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ActorDoesNotExistException
    _ActorDoesNotExistException,

    -- ** InvalidPullRequestStatusUpdateException
    _InvalidPullRequestStatusUpdateException,

    -- ** NameLengthExceededException
    _NameLengthExceededException,

    -- ** ApprovalRuleNameAlreadyExistsException
    _ApprovalRuleNameAlreadyExistsException,

    -- ** PullRequestIdRequiredException
    _PullRequestIdRequiredException,

    -- ** InvalidConflictDetailLevelException
    _InvalidConflictDetailLevelException,

    -- ** InvalidPullRequestIdException
    _InvalidPullRequestIdException,

    -- ** TargetRequiredException
    _TargetRequiredException,

    -- ** CommitRequiredException
    _CommitRequiredException,

    -- ** FileNameConflictsWithDirectoryNameException
    _FileNameConflictsWithDirectoryNameException,

    -- ** ReferenceDoesNotExistException
    _ReferenceDoesNotExistException,

    -- ** EncryptionKeyNotFoundException
    _EncryptionKeyNotFoundException,

    -- ** TargetsRequiredException
    _TargetsRequiredException,

    -- ** CommentContentRequiredException
    _CommentContentRequiredException,

    -- ** InvalidSystemTagUsageException
    _InvalidSystemTagUsageException,

    -- ** FileEntryRequiredException
    _FileEntryRequiredException,

    -- ** InvalidTargetsException
    _InvalidTargetsException,

    -- ** ManualMergeRequiredException
    _ManualMergeRequiredException,

    -- ** BranchNameExistsException
    _BranchNameExistsException,

    -- ** ParentCommitDoesNotExistException
    _ParentCommitDoesNotExistException,

    -- ** SamePathRequestException
    _SamePathRequestException,

    -- ** AuthorDoesNotExistException
    _AuthorDoesNotExistException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** SourceAndDestinationAreSameException
    _SourceAndDestinationAreSameException,

    -- ** EncryptionIntegrityChecksFailedException
    _EncryptionIntegrityChecksFailedException,

    -- ** InvalidApprovalRuleTemplateDescriptionException
    _InvalidApprovalRuleTemplateDescriptionException,

    -- ** CommentDeletedException
    _CommentDeletedException,

    -- ** InvalidReplacementContentException
    _InvalidReplacementContentException,

    -- ** RepositoryTriggerEventsListRequiredException
    _RepositoryTriggerEventsListRequiredException,

    -- ** FileContentRequiredException
    _FileContentRequiredException,

    -- ** SourceFileOrContentRequiredException
    _SourceFileOrContentRequiredException,

    -- ** InvalidTitleException
    _InvalidTitleException,

    -- ** InvalidApprovalRuleContentException
    _InvalidApprovalRuleContentException,

    -- ** InvalidSourceCommitSpecifierException
    _InvalidSourceCommitSpecifierException,

    -- ** InvalidConflictResolutionException
    _InvalidConflictResolutionException,

    -- ** InvalidOverrideStatusException
    _InvalidOverrideStatusException,

    -- ** PullRequestStatusRequiredException
    _PullRequestStatusRequiredException,

    -- ** MaximumItemsToCompareExceededException
    _MaximumItemsToCompareExceededException,

    -- ** InvalidPullRequestStatusException
    _InvalidPullRequestStatusException,

    -- ** ParentCommitIdRequiredException
    _ParentCommitIdRequiredException,

    -- ** RepositoryDoesNotExistException
    _RepositoryDoesNotExistException,

    -- ** PullRequestApprovalRulesNotSatisfiedException
    _PullRequestApprovalRulesNotSatisfiedException,

    -- ** ApprovalRuleContentRequiredException
    _ApprovalRuleContentRequiredException,

    -- ** TitleRequiredException
    _TitleRequiredException,

    -- ** FileContentSizeLimitExceededException
    _FileContentSizeLimitExceededException,

    -- ** InvalidRepositoryTriggerNameException
    _InvalidRepositoryTriggerNameException,

    -- ** RepositoryTriggerNameRequiredException
    _RepositoryTriggerNameRequiredException,

    -- ** NumberOfRuleTemplatesExceededException
    _NumberOfRuleTemplatesExceededException,

    -- ** RepositoryNameRequiredException
    _RepositoryNameRequiredException,

    -- ** InvalidFileModeException
    _InvalidFileModeException,

    -- ** InvalidRepositoryTriggerEventsException
    _InvalidRepositoryTriggerEventsException,

    -- ** InvalidPathException
    _InvalidPathException,

    -- ** InvalidTargetBranchException
    _InvalidTargetBranchException,

    -- ** InvalidActorArnException
    _InvalidActorArnException,

    -- ** RevisionIdRequiredException
    _RevisionIdRequiredException,

    -- ** InvalidBlobIdException
    _InvalidBlobIdException,

    -- ** MaximumFileEntriesExceededException
    _MaximumFileEntriesExceededException,

    -- ** ApprovalRuleTemplateNameRequiredException
    _ApprovalRuleTemplateNameRequiredException,

    -- ** RepositoryNamesRequiredException
    _RepositoryNamesRequiredException,

    -- ** PullRequestDoesNotExistException
    _PullRequestDoesNotExistException,

    -- ** ReplacementTypeRequiredException
    _ReplacementTypeRequiredException,

    -- ** InvalidReplacementTypeException
    _InvalidReplacementTypeException,

    -- ** FileTooLargeException
    _FileTooLargeException,

    -- ** CannotDeleteApprovalRuleFromTemplateException
    _CannotDeleteApprovalRuleFromTemplateException,

    -- ** ReactionLimitExceededException
    _ReactionLimitExceededException,

    -- ** RevisionNotCurrentException
    _RevisionNotCurrentException,

    -- ** InvalidApprovalRuleNameException
    _InvalidApprovalRuleNameException,

    -- ** CommitIdDoesNotExistException
    _CommitIdDoesNotExistException,

    -- ** RepositoryTriggersListRequiredException
    _RepositoryTriggersListRequiredException,

    -- ** InvalidRelativeFileVersionEnumException
    _InvalidRelativeFileVersionEnumException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** ApprovalRuleDoesNotExistException
    _ApprovalRuleDoesNotExistException,

    -- ** RepositoryTriggerBranchNameListRequiredException
    _RepositoryTriggerBranchNameListRequiredException,

    -- ** InvalidClientRequestTokenException
    _InvalidClientRequestTokenException,

    -- ** IdempotencyParameterMismatchException
    _IdempotencyParameterMismatchException,

    -- ** InvalidRepositoryTriggerDestinationArnException
    _InvalidRepositoryTriggerDestinationArnException,

    -- ** BranchNameRequiredException
    _BranchNameRequiredException,

    -- ** RepositoryTriggerDestinationArnRequiredException
    _RepositoryTriggerDestinationArnRequiredException,

    -- ** EncryptionKeyUnavailableException
    _EncryptionKeyUnavailableException,

    -- ** InvalidConflictResolutionStrategyException
    _InvalidConflictResolutionStrategyException,

    -- ** MultipleConflictResolutionEntriesException
    _MultipleConflictResolutionEntriesException,

    -- ** FileDoesNotExistException
    _FileDoesNotExistException,

    -- ** TagKeysListRequiredException
    _TagKeysListRequiredException,

    -- ** InvalidReferenceNameException
    _InvalidReferenceNameException,

    -- ** CommitIdRequiredException
    _CommitIdRequiredException,

    -- ** ReferenceNameRequiredException
    _ReferenceNameRequiredException,

    -- ** MaximumRuleTemplatesAssociatedWithRepositoryException
    _MaximumRuleTemplatesAssociatedWithRepositoryException,

    -- ** CommitMessageLengthExceededException
    _CommitMessageLengthExceededException,

    -- ** MaximumNumberOfApprovalsExceededException
    _MaximumNumberOfApprovalsExceededException,

    -- ** InvalidTagKeysListException
    _InvalidTagKeysListException,

    -- ** EncryptionKeyDisabledException
    _EncryptionKeyDisabledException,

    -- ** DirectoryNameConflictsWithFileNameException
    _DirectoryNameConflictsWithFileNameException,

    -- ** InvalidSortByException
    _InvalidSortByException,

    -- ** InvalidDestinationCommitSpecifierException
    _InvalidDestinationCommitSpecifierException,

    -- ** CommentDoesNotExistException
    _CommentDoesNotExistException,

    -- ** ApprovalRuleTemplateNameAlreadyExistsException
    _ApprovalRuleTemplateNameAlreadyExistsException,

    -- ** InvalidRepositoryTriggerBranchNameException
    _InvalidRepositoryTriggerBranchNameException,

    -- ** MaximumOpenPullRequestsExceededException
    _MaximumOpenPullRequestsExceededException,

    -- ** PullRequestAlreadyClosedException
    _PullRequestAlreadyClosedException,

    -- ** InvalidCommitException
    _InvalidCommitException,

    -- ** ReferenceTypeNotSupportedException
    _ReferenceTypeNotSupportedException,

    -- ** CommentNotCreatedByCallerException
    _CommentNotCreatedByCallerException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** InvalidRepositoryTriggerCustomDataException
    _InvalidRepositoryTriggerCustomDataException,

    -- ** OverrideAlreadySetException
    _OverrideAlreadySetException,

    -- ** InvalidContinuationTokenException
    _InvalidContinuationTokenException,

    -- ** InvalidRepositoryTriggerRegionException
    _InvalidRepositoryTriggerRegionException,

    -- ** InvalidReactionValueException
    _InvalidReactionValueException,

    -- ** TipsDivergenceExceededException
    _TipsDivergenceExceededException,

    -- ** EncryptionKeyAccessDeniedException
    _EncryptionKeyAccessDeniedException,

    -- ** ReactionValueRequiredException
    _ReactionValueRequiredException,

    -- ** ResourceArnRequiredException
    _ResourceArnRequiredException,

    -- ** PathDoesNotExistException
    _PathDoesNotExistException,

    -- ** ReplacementContentRequiredException
    _ReplacementContentRequiredException,

    -- ** InvalidResourceArnException
    _InvalidResourceArnException,

    -- ** ParentCommitIdOutdatedException
    _ParentCommitIdOutdatedException,

    -- ** InvalidMaxMergeHunksException
    _InvalidMaxMergeHunksException,

    -- ** RestrictedSourceFileException
    _RestrictedSourceFileException,

    -- ** InvalidFilePositionException
    _InvalidFilePositionException,

    -- ** CannotModifyApprovalRuleFromTemplateException
    _CannotModifyApprovalRuleFromTemplateException,

    -- ** InvalidMaxConflictFilesException
    _InvalidMaxConflictFilesException,

    -- ** CommentContentSizeLimitExceededException
    _CommentContentSizeLimitExceededException,

    -- ** InvalidApprovalStateException
    _InvalidApprovalStateException,

    -- ** MaximumBranchesExceededException
    _MaximumBranchesExceededException,

    -- ** OverrideStatusRequiredException
    _OverrideStatusRequiredException,

    -- ** InvalidPullRequestEventTypeException
    _InvalidPullRequestEventTypeException,

    -- ** InvalidApprovalRuleTemplateContentException
    _InvalidApprovalRuleTemplateContentException,

    -- ** CommitIdsLimitExceededException
    _CommitIdsLimitExceededException,

    -- ** ApprovalStateRequiredException
    _ApprovalStateRequiredException,

    -- ** InvalidAuthorArnException
    _InvalidAuthorArnException,

    -- ** MaximumConflictResolutionEntriesExceededException
    _MaximumConflictResolutionEntriesExceededException,

    -- ** InvalidParentCommitIdException
    _InvalidParentCommitIdException,

    -- ** RepositoryNotAssociatedWithPullRequestException
    _RepositoryNotAssociatedWithPullRequestException,

    -- ** ApprovalRuleTemplateContentRequiredException
    _ApprovalRuleTemplateContentRequiredException,

    -- ** ConcurrentReferenceUpdateException
    _ConcurrentReferenceUpdateException,

    -- ** MaximumFileContentToLoadExceededException
    _MaximumFileContentToLoadExceededException,

    -- ** TagsMapRequiredException
    _TagsMapRequiredException,

    -- ** InvalidRepositoryNameException
    _InvalidRepositoryNameException,

    -- ** FolderContentSizeLimitExceededException
    _FolderContentSizeLimitExceededException,

    -- ** CommitIdsListRequiredException
    _CommitIdsListRequiredException,

    -- ** FileModeRequiredException
    _FileModeRequiredException,

    -- ** InvalidTagsMapException
    _InvalidTagsMapException,

    -- ** RepositoryLimitExceededException
    _RepositoryLimitExceededException,

    -- ** NumberOfRulesExceededException
    _NumberOfRulesExceededException,

    -- ** InvalidDeletionParameterException
    _InvalidDeletionParameterException,

    -- ** InvalidReactionUserArnException
    _InvalidReactionUserArnException,

    -- ** DefaultBranchCannotBeDeletedException
    _DefaultBranchCannotBeDeletedException,

    -- ** BranchNameIsTagNameException
    _BranchNameIsTagNameException,

    -- ** PathRequiredException
    _PathRequiredException,

    -- ** FilePathConflictsWithSubmodulePathException
    _FilePathConflictsWithSubmodulePathException,

    -- ** BranchDoesNotExistException
    _BranchDoesNotExistException,

    -- ** InvalidCommentIdException
    _InvalidCommentIdException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** InvalidRevisionIdException
    _InvalidRevisionIdException,

    -- ** PullRequestCannotBeApprovedByAuthorException
    _PullRequestCannotBeApprovedByAuthorException,

    -- ** ApprovalRuleNameRequiredException
    _ApprovalRuleNameRequiredException,

    -- ** BlobIdRequiredException
    _BlobIdRequiredException,

    -- ** InvalidApprovalRuleTemplateNameException
    _InvalidApprovalRuleTemplateNameException,

    -- ** InvalidDescriptionException
    _InvalidDescriptionException,

    -- ** NoChangeException
    _NoChangeException,

    -- ** CommentIdRequiredException
    _CommentIdRequiredException,

    -- ** MultipleRepositoriesInPullRequestException
    _MultipleRepositoriesInPullRequestException,

    -- ** InvalidOrderException
    _InvalidOrderException,

    -- ** ClientRequestTokenRequiredException
    _ClientRequestTokenRequiredException,

    -- ** InvalidMergeOptionException
    _InvalidMergeOptionException,

    -- ** MergeOptionRequiredException
    _MergeOptionRequiredException,

    -- ** InvalidBranchNameException
    _InvalidBranchNameException,

    -- ** InvalidFileLocationException
    _InvalidFileLocationException,

    -- ** ApprovalRuleTemplateDoesNotExistException
    _ApprovalRuleTemplateDoesNotExistException,

    -- ** MaximumRepositoryTriggersExceededException
    _MaximumRepositoryTriggersExceededException,

    -- ** CommitDoesNotExistException
    _CommitDoesNotExistException,

    -- ** BeforeCommitIdAndAfterCommitIdAreSameException
    _BeforeCommitIdAndAfterCommitIdAreSameException,

    -- ** RepositoryNameExistsException
    _RepositoryNameExistsException,

    -- ** InvalidCommitIdException
    _InvalidCommitIdException,

    -- ** InvalidRepositoryDescriptionException
    _InvalidRepositoryDescriptionException,

    -- ** MaximumRepositoryNamesExceededException
    _MaximumRepositoryNamesExceededException,

    -- ** SameFileContentException
    _SameFileContentException,

    -- ** ApprovalRuleTemplateInUseException
    _ApprovalRuleTemplateInUseException,

    -- ** InvalidEmailException
    _InvalidEmailException,

    -- ** TipOfSourceReferenceIsDifferentException
    _TipOfSourceReferenceIsDifferentException,

    -- ** FolderDoesNotExistException
    _FolderDoesNotExistException,

    -- ** InvalidRuleContentSha256Exception
    _InvalidRuleContentSha256Exception,

    -- ** BlobIdDoesNotExistException
    _BlobIdDoesNotExistException,

    -- ** PutFileEntryConflictException
    _PutFileEntryConflictException,

    -- ** FileContentAndSourceFileSpecifiedException
    _FileContentAndSourceFileSpecifiedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories
    BatchAssociateApprovalRuleTemplateWithRepositories (BatchAssociateApprovalRuleTemplateWithRepositories'),
    newBatchAssociateApprovalRuleTemplateWithRepositories,
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse (BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse,

    -- ** PutFile
    PutFile (PutFile'),
    newPutFile,
    PutFileResponse (PutFileResponse'),
    newPutFileResponse,

    -- ** GetRepositoryTriggers
    GetRepositoryTriggers (GetRepositoryTriggers'),
    newGetRepositoryTriggers,
    GetRepositoryTriggersResponse (GetRepositoryTriggersResponse'),
    newGetRepositoryTriggersResponse,

    -- ** DisassociateApprovalRuleTemplateFromRepository
    DisassociateApprovalRuleTemplateFromRepository (DisassociateApprovalRuleTemplateFromRepository'),
    newDisassociateApprovalRuleTemplateFromRepository,
    DisassociateApprovalRuleTemplateFromRepositoryResponse (DisassociateApprovalRuleTemplateFromRepositoryResponse'),
    newDisassociateApprovalRuleTemplateFromRepositoryResponse,

    -- ** DeletePullRequestApprovalRule
    DeletePullRequestApprovalRule (DeletePullRequestApprovalRule'),
    newDeletePullRequestApprovalRule,
    DeletePullRequestApprovalRuleResponse (DeletePullRequestApprovalRuleResponse'),
    newDeletePullRequestApprovalRuleResponse,

    -- ** ListRepositoriesForApprovalRuleTemplate
    ListRepositoriesForApprovalRuleTemplate (ListRepositoriesForApprovalRuleTemplate'),
    newListRepositoriesForApprovalRuleTemplate,
    ListRepositoriesForApprovalRuleTemplateResponse (ListRepositoriesForApprovalRuleTemplateResponse'),
    newListRepositoriesForApprovalRuleTemplateResponse,

    -- ** UpdateRepositoryName
    UpdateRepositoryName (UpdateRepositoryName'),
    newUpdateRepositoryName,
    UpdateRepositoryNameResponse (UpdateRepositoryNameResponse'),
    newUpdateRepositoryNameResponse,

    -- ** BatchDescribeMergeConflicts
    BatchDescribeMergeConflicts (BatchDescribeMergeConflicts'),
    newBatchDescribeMergeConflicts,
    BatchDescribeMergeConflictsResponse (BatchDescribeMergeConflictsResponse'),
    newBatchDescribeMergeConflictsResponse,

    -- ** GetRepository
    GetRepository (GetRepository'),
    newGetRepository,
    GetRepositoryResponse (GetRepositoryResponse'),
    newGetRepositoryResponse,

    -- ** UpdatePullRequestStatus
    UpdatePullRequestStatus (UpdatePullRequestStatus'),
    newUpdatePullRequestStatus,
    UpdatePullRequestStatusResponse (UpdatePullRequestStatusResponse'),
    newUpdatePullRequestStatusResponse,

    -- ** PostCommentReply
    PostCommentReply (PostCommentReply'),
    newPostCommentReply,
    PostCommentReplyResponse (PostCommentReplyResponse'),
    newPostCommentReplyResponse,

    -- ** GetPullRequestOverrideState
    GetPullRequestOverrideState (GetPullRequestOverrideState'),
    newGetPullRequestOverrideState,
    GetPullRequestOverrideStateResponse (GetPullRequestOverrideStateResponse'),
    newGetPullRequestOverrideStateResponse,

    -- ** GetCommentsForPullRequest (Paginated)
    GetCommentsForPullRequest (GetCommentsForPullRequest'),
    newGetCommentsForPullRequest,
    GetCommentsForPullRequestResponse (GetCommentsForPullRequestResponse'),
    newGetCommentsForPullRequestResponse,

    -- ** UpdateDefaultBranch
    UpdateDefaultBranch (UpdateDefaultBranch'),
    newUpdateDefaultBranch,
    UpdateDefaultBranchResponse (UpdateDefaultBranchResponse'),
    newUpdateDefaultBranchResponse,

    -- ** BatchGetRepositories
    BatchGetRepositories (BatchGetRepositories'),
    newBatchGetRepositories,
    BatchGetRepositoriesResponse (BatchGetRepositoriesResponse'),
    newBatchGetRepositoriesResponse,

    -- ** GetMergeOptions
    GetMergeOptions (GetMergeOptions'),
    newGetMergeOptions,
    GetMergeOptionsResponse (GetMergeOptionsResponse'),
    newGetMergeOptionsResponse,

    -- ** MergePullRequestByThreeWay
    MergePullRequestByThreeWay (MergePullRequestByThreeWay'),
    newMergePullRequestByThreeWay,
    MergePullRequestByThreeWayResponse (MergePullRequestByThreeWayResponse'),
    newMergePullRequestByThreeWayResponse,

    -- ** UpdatePullRequestDescription
    UpdatePullRequestDescription (UpdatePullRequestDescription'),
    newUpdatePullRequestDescription,
    UpdatePullRequestDescriptionResponse (UpdatePullRequestDescriptionResponse'),
    newUpdatePullRequestDescriptionResponse,

    -- ** GetMergeConflicts
    GetMergeConflicts (GetMergeConflicts'),
    newGetMergeConflicts,
    GetMergeConflictsResponse (GetMergeConflictsResponse'),
    newGetMergeConflictsResponse,

    -- ** PutCommentReaction
    PutCommentReaction (PutCommentReaction'),
    newPutCommentReaction,
    PutCommentReactionResponse (PutCommentReactionResponse'),
    newPutCommentReactionResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** DeleteApprovalRuleTemplate
    DeleteApprovalRuleTemplate (DeleteApprovalRuleTemplate'),
    newDeleteApprovalRuleTemplate,
    DeleteApprovalRuleTemplateResponse (DeleteApprovalRuleTemplateResponse'),
    newDeleteApprovalRuleTemplateResponse,

    -- ** ListBranches (Paginated)
    ListBranches (ListBranches'),
    newListBranches,
    ListBranchesResponse (ListBranchesResponse'),
    newListBranchesResponse,

    -- ** ListApprovalRuleTemplates
    ListApprovalRuleTemplates (ListApprovalRuleTemplates'),
    newListApprovalRuleTemplates,
    ListApprovalRuleTemplatesResponse (ListApprovalRuleTemplatesResponse'),
    newListApprovalRuleTemplatesResponse,

    -- ** PutRepositoryTriggers
    PutRepositoryTriggers (PutRepositoryTriggers'),
    newPutRepositoryTriggers,
    PutRepositoryTriggersResponse (PutRepositoryTriggersResponse'),
    newPutRepositoryTriggersResponse,

    -- ** CreateBranch
    CreateBranch (CreateBranch'),
    newCreateBranch,
    CreateBranchResponse (CreateBranchResponse'),
    newCreateBranchResponse,

    -- ** MergeBranchesByThreeWay
    MergeBranchesByThreeWay (MergeBranchesByThreeWay'),
    newMergeBranchesByThreeWay,
    MergeBranchesByThreeWayResponse (MergeBranchesByThreeWayResponse'),
    newMergeBranchesByThreeWayResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** GetFile
    GetFile (GetFile'),
    newGetFile,
    GetFileResponse (GetFileResponse'),
    newGetFileResponse,

    -- ** UpdatePullRequestApprovalRuleContent
    UpdatePullRequestApprovalRuleContent (UpdatePullRequestApprovalRuleContent'),
    newUpdatePullRequestApprovalRuleContent,
    UpdatePullRequestApprovalRuleContentResponse (UpdatePullRequestApprovalRuleContentResponse'),
    newUpdatePullRequestApprovalRuleContentResponse,

    -- ** UpdatePullRequestTitle
    UpdatePullRequestTitle (UpdatePullRequestTitle'),
    newUpdatePullRequestTitle,
    UpdatePullRequestTitleResponse (UpdatePullRequestTitleResponse'),
    newUpdatePullRequestTitleResponse,

    -- ** UpdateRepositoryDescription
    UpdateRepositoryDescription (UpdateRepositoryDescription'),
    newUpdateRepositoryDescription,
    UpdateRepositoryDescriptionResponse (UpdateRepositoryDescriptionResponse'),
    newUpdateRepositoryDescriptionResponse,

    -- ** OverridePullRequestApprovalRules
    OverridePullRequestApprovalRules (OverridePullRequestApprovalRules'),
    newOverridePullRequestApprovalRules,
    OverridePullRequestApprovalRulesResponse (OverridePullRequestApprovalRulesResponse'),
    newOverridePullRequestApprovalRulesResponse,

    -- ** GetPullRequest
    GetPullRequest (GetPullRequest'),
    newGetPullRequest,
    GetPullRequestResponse (GetPullRequestResponse'),
    newGetPullRequestResponse,

    -- ** UpdateComment
    UpdateComment (UpdateComment'),
    newUpdateComment,
    UpdateCommentResponse (UpdateCommentResponse'),
    newUpdateCommentResponse,

    -- ** GetDifferences (Paginated)
    GetDifferences (GetDifferences'),
    newGetDifferences,
    GetDifferencesResponse (GetDifferencesResponse'),
    newGetDifferencesResponse,

    -- ** GetMergeCommit
    GetMergeCommit (GetMergeCommit'),
    newGetMergeCommit,
    GetMergeCommitResponse (GetMergeCommitResponse'),
    newGetMergeCommitResponse,

    -- ** GetApprovalRuleTemplate
    GetApprovalRuleTemplate (GetApprovalRuleTemplate'),
    newGetApprovalRuleTemplate,
    GetApprovalRuleTemplateResponse (GetApprovalRuleTemplateResponse'),
    newGetApprovalRuleTemplateResponse,

    -- ** GetCommit
    GetCommit (GetCommit'),
    newGetCommit,
    GetCommitResponse (GetCommitResponse'),
    newGetCommitResponse,

    -- ** UpdateApprovalRuleTemplateName
    UpdateApprovalRuleTemplateName (UpdateApprovalRuleTemplateName'),
    newUpdateApprovalRuleTemplateName,
    UpdateApprovalRuleTemplateNameResponse (UpdateApprovalRuleTemplateNameResponse'),
    newUpdateApprovalRuleTemplateNameResponse,

    -- ** GetCommentReactions
    GetCommentReactions (GetCommentReactions'),
    newGetCommentReactions,
    GetCommentReactionsResponse (GetCommentReactionsResponse'),
    newGetCommentReactionsResponse,

    -- ** TestRepositoryTriggers
    TestRepositoryTriggers (TestRepositoryTriggers'),
    newTestRepositoryTriggers,
    TestRepositoryTriggersResponse (TestRepositoryTriggersResponse'),
    newTestRepositoryTriggersResponse,

    -- ** DeleteFile
    DeleteFile (DeleteFile'),
    newDeleteFile,
    DeleteFileResponse (DeleteFileResponse'),
    newDeleteFileResponse,

    -- ** GetCommentsForComparedCommit (Paginated)
    GetCommentsForComparedCommit (GetCommentsForComparedCommit'),
    newGetCommentsForComparedCommit,
    GetCommentsForComparedCommitResponse (GetCommentsForComparedCommitResponse'),
    newGetCommentsForComparedCommitResponse,

    -- ** MergeBranchesBySquash
    MergeBranchesBySquash (MergeBranchesBySquash'),
    newMergeBranchesBySquash,
    MergeBranchesBySquashResponse (MergeBranchesBySquashResponse'),
    newMergeBranchesBySquashResponse,

    -- ** PostCommentForPullRequest
    PostCommentForPullRequest (PostCommentForPullRequest'),
    newPostCommentForPullRequest,
    PostCommentForPullRequestResponse (PostCommentForPullRequestResponse'),
    newPostCommentForPullRequestResponse,

    -- ** MergePullRequestByFastForward
    MergePullRequestByFastForward (MergePullRequestByFastForward'),
    newMergePullRequestByFastForward,
    MergePullRequestByFastForwardResponse (MergePullRequestByFastForwardResponse'),
    newMergePullRequestByFastForwardResponse,

    -- ** CreatePullRequestApprovalRule
    CreatePullRequestApprovalRule (CreatePullRequestApprovalRule'),
    newCreatePullRequestApprovalRule,
    CreatePullRequestApprovalRuleResponse (CreatePullRequestApprovalRuleResponse'),
    newCreatePullRequestApprovalRuleResponse,

    -- ** CreateUnreferencedMergeCommit
    CreateUnreferencedMergeCommit (CreateUnreferencedMergeCommit'),
    newCreateUnreferencedMergeCommit,
    CreateUnreferencedMergeCommitResponse (CreateUnreferencedMergeCommitResponse'),
    newCreateUnreferencedMergeCommitResponse,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    ListAssociatedApprovalRuleTemplatesForRepository (ListAssociatedApprovalRuleTemplatesForRepository'),
    newListAssociatedApprovalRuleTemplatesForRepository,
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse (ListAssociatedApprovalRuleTemplatesForRepositoryResponse'),
    newListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- ** GetPullRequestApprovalStates
    GetPullRequestApprovalStates (GetPullRequestApprovalStates'),
    newGetPullRequestApprovalStates,
    GetPullRequestApprovalStatesResponse (GetPullRequestApprovalStatesResponse'),
    newGetPullRequestApprovalStatesResponse,

    -- ** UpdateApprovalRuleTemplateContent
    UpdateApprovalRuleTemplateContent (UpdateApprovalRuleTemplateContent'),
    newUpdateApprovalRuleTemplateContent,
    UpdateApprovalRuleTemplateContentResponse (UpdateApprovalRuleTemplateContentResponse'),
    newUpdateApprovalRuleTemplateContentResponse,

    -- ** ListRepositories (Paginated)
    ListRepositories (ListRepositories'),
    newListRepositories,
    ListRepositoriesResponse (ListRepositoriesResponse'),
    newListRepositoriesResponse,

    -- ** UpdateApprovalRuleTemplateDescription
    UpdateApprovalRuleTemplateDescription (UpdateApprovalRuleTemplateDescription'),
    newUpdateApprovalRuleTemplateDescription,
    UpdateApprovalRuleTemplateDescriptionResponse (UpdateApprovalRuleTemplateDescriptionResponse'),
    newUpdateApprovalRuleTemplateDescriptionResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** DescribePullRequestEvents (Paginated)
    DescribePullRequestEvents (DescribePullRequestEvents'),
    newDescribePullRequestEvents,
    DescribePullRequestEventsResponse (DescribePullRequestEventsResponse'),
    newDescribePullRequestEventsResponse,

    -- ** DeleteCommentContent
    DeleteCommentContent (DeleteCommentContent'),
    newDeleteCommentContent,
    DeleteCommentContentResponse (DeleteCommentContentResponse'),
    newDeleteCommentContentResponse,

    -- ** DeleteRepository
    DeleteRepository (DeleteRepository'),
    newDeleteRepository,
    DeleteRepositoryResponse (DeleteRepositoryResponse'),
    newDeleteRepositoryResponse,

    -- ** DescribeMergeConflicts
    DescribeMergeConflicts (DescribeMergeConflicts'),
    newDescribeMergeConflicts,
    DescribeMergeConflictsResponse (DescribeMergeConflictsResponse'),
    newDescribeMergeConflictsResponse,

    -- ** BatchGetCommits
    BatchGetCommits (BatchGetCommits'),
    newBatchGetCommits,
    BatchGetCommitsResponse (BatchGetCommitsResponse'),
    newBatchGetCommitsResponse,

    -- ** GetFolder
    GetFolder (GetFolder'),
    newGetFolder,
    GetFolderResponse (GetFolderResponse'),
    newGetFolderResponse,

    -- ** CreatePullRequest
    CreatePullRequest (CreatePullRequest'),
    newCreatePullRequest,
    CreatePullRequestResponse (CreatePullRequestResponse'),
    newCreatePullRequestResponse,

    -- ** EvaluatePullRequestApprovalRules
    EvaluatePullRequestApprovalRules (EvaluatePullRequestApprovalRules'),
    newEvaluatePullRequestApprovalRules,
    EvaluatePullRequestApprovalRulesResponse (EvaluatePullRequestApprovalRulesResponse'),
    newEvaluatePullRequestApprovalRulesResponse,

    -- ** UpdatePullRequestApprovalState
    UpdatePullRequestApprovalState (UpdatePullRequestApprovalState'),
    newUpdatePullRequestApprovalState,
    UpdatePullRequestApprovalStateResponse (UpdatePullRequestApprovalStateResponse'),
    newUpdatePullRequestApprovalStateResponse,

    -- ** CreateCommit
    CreateCommit (CreateCommit'),
    newCreateCommit,
    CreateCommitResponse (CreateCommitResponse'),
    newCreateCommitResponse,

    -- ** AssociateApprovalRuleTemplateWithRepository
    AssociateApprovalRuleTemplateWithRepository (AssociateApprovalRuleTemplateWithRepository'),
    newAssociateApprovalRuleTemplateWithRepository,
    AssociateApprovalRuleTemplateWithRepositoryResponse (AssociateApprovalRuleTemplateWithRepositoryResponse'),
    newAssociateApprovalRuleTemplateWithRepositoryResponse,

    -- ** GetBlob
    GetBlob (GetBlob'),
    newGetBlob,
    GetBlobResponse (GetBlobResponse'),
    newGetBlobResponse,

    -- ** CreateApprovalRuleTemplate
    CreateApprovalRuleTemplate (CreateApprovalRuleTemplate'),
    newCreateApprovalRuleTemplate,
    CreateApprovalRuleTemplateResponse (CreateApprovalRuleTemplateResponse'),
    newCreateApprovalRuleTemplateResponse,

    -- ** ListPullRequests (Paginated)
    ListPullRequests (ListPullRequests'),
    newListPullRequests,
    ListPullRequestsResponse (ListPullRequestsResponse'),
    newListPullRequestsResponse,

    -- ** DeleteBranch
    DeleteBranch (DeleteBranch'),
    newDeleteBranch,
    DeleteBranchResponse (DeleteBranchResponse'),
    newDeleteBranchResponse,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories
    BatchDisassociateApprovalRuleTemplateFromRepositories (BatchDisassociateApprovalRuleTemplateFromRepositories'),
    newBatchDisassociateApprovalRuleTemplateFromRepositories,
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'),
    newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- ** GetComment
    GetComment (GetComment'),
    newGetComment,
    GetCommentResponse (GetCommentResponse'),
    newGetCommentResponse,

    -- ** GetBranch
    GetBranch (GetBranch'),
    newGetBranch,
    GetBranchResponse (GetBranchResponse'),
    newGetBranchResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** MergeBranchesByFastForward
    MergeBranchesByFastForward (MergeBranchesByFastForward'),
    newMergeBranchesByFastForward,
    MergeBranchesByFastForwardResponse (MergeBranchesByFastForwardResponse'),
    newMergeBranchesByFastForwardResponse,

    -- ** PostCommentForComparedCommit
    PostCommentForComparedCommit (PostCommentForComparedCommit'),
    newPostCommentForComparedCommit,
    PostCommentForComparedCommitResponse (PostCommentForComparedCommitResponse'),
    newPostCommentForComparedCommitResponse,

    -- ** MergePullRequestBySquash
    MergePullRequestBySquash (MergePullRequestBySquash'),
    newMergePullRequestBySquash,
    MergePullRequestBySquashResponse (MergePullRequestBySquashResponse'),
    newMergePullRequestBySquashResponse,

    -- * Types

    -- ** ApprovalState
    ApprovalState (..),

    -- ** ChangeTypeEnum
    ChangeTypeEnum (..),

    -- ** ConflictDetailLevelTypeEnum
    ConflictDetailLevelTypeEnum (..),

    -- ** ConflictResolutionStrategyTypeEnum
    ConflictResolutionStrategyTypeEnum (..),

    -- ** FileModeTypeEnum
    FileModeTypeEnum (..),

    -- ** MergeOptionTypeEnum
    MergeOptionTypeEnum (..),

    -- ** ObjectTypeEnum
    ObjectTypeEnum (..),

    -- ** OrderEnum
    OrderEnum (..),

    -- ** OverrideStatus
    OverrideStatus (..),

    -- ** PullRequestEventType
    PullRequestEventType (..),

    -- ** PullRequestStatusEnum
    PullRequestStatusEnum (..),

    -- ** RelativeFileVersionEnum
    RelativeFileVersionEnum (..),

    -- ** ReplacementTypeEnum
    ReplacementTypeEnum (..),

    -- ** RepositoryTriggerEventEnum
    RepositoryTriggerEventEnum (..),

    -- ** SortByEnum
    SortByEnum (..),

    -- ** Approval
    Approval (Approval'),
    newApproval,

    -- ** ApprovalRule
    ApprovalRule (ApprovalRule'),
    newApprovalRule,

    -- ** ApprovalRuleEventMetadata
    ApprovalRuleEventMetadata (ApprovalRuleEventMetadata'),
    newApprovalRuleEventMetadata,

    -- ** ApprovalRuleOverriddenEventMetadata
    ApprovalRuleOverriddenEventMetadata (ApprovalRuleOverriddenEventMetadata'),
    newApprovalRuleOverriddenEventMetadata,

    -- ** ApprovalRuleTemplate
    ApprovalRuleTemplate (ApprovalRuleTemplate'),
    newApprovalRuleTemplate,

    -- ** ApprovalStateChangedEventMetadata
    ApprovalStateChangedEventMetadata (ApprovalStateChangedEventMetadata'),
    newApprovalStateChangedEventMetadata,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositoriesError
    BatchAssociateApprovalRuleTemplateWithRepositoriesError (BatchAssociateApprovalRuleTemplateWithRepositoriesError'),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesError,

    -- ** BatchDescribeMergeConflictsError
    BatchDescribeMergeConflictsError (BatchDescribeMergeConflictsError'),
    newBatchDescribeMergeConflictsError,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositoriesError
    BatchDisassociateApprovalRuleTemplateFromRepositoriesError (BatchDisassociateApprovalRuleTemplateFromRepositoriesError'),
    newBatchDisassociateApprovalRuleTemplateFromRepositoriesError,

    -- ** BatchGetCommitsError
    BatchGetCommitsError (BatchGetCommitsError'),
    newBatchGetCommitsError,

    -- ** BlobMetadata
    BlobMetadata (BlobMetadata'),
    newBlobMetadata,

    -- ** BranchInfo
    BranchInfo (BranchInfo'),
    newBranchInfo,

    -- ** Comment
    Comment (Comment'),
    newComment,

    -- ** CommentsForComparedCommit
    CommentsForComparedCommit (CommentsForComparedCommit'),
    newCommentsForComparedCommit,

    -- ** CommentsForPullRequest
    CommentsForPullRequest (CommentsForPullRequest'),
    newCommentsForPullRequest,

    -- ** Commit
    Commit (Commit'),
    newCommit,

    -- ** Conflict
    Conflict (Conflict'),
    newConflict,

    -- ** ConflictMetadata
    ConflictMetadata (ConflictMetadata'),
    newConflictMetadata,

    -- ** ConflictResolution
    ConflictResolution (ConflictResolution'),
    newConflictResolution,

    -- ** DeleteFileEntry
    DeleteFileEntry (DeleteFileEntry'),
    newDeleteFileEntry,

    -- ** Difference
    Difference (Difference'),
    newDifference,

    -- ** Evaluation
    Evaluation (Evaluation'),
    newEvaluation,

    -- ** File
    File (File'),
    newFile,

    -- ** FileMetadata
    FileMetadata (FileMetadata'),
    newFileMetadata,

    -- ** FileModes
    FileModes (FileModes'),
    newFileModes,

    -- ** FileSizes
    FileSizes (FileSizes'),
    newFileSizes,

    -- ** Folder
    Folder (Folder'),
    newFolder,

    -- ** IsBinaryFile
    IsBinaryFile (IsBinaryFile'),
    newIsBinaryFile,

    -- ** Location
    Location (Location'),
    newLocation,

    -- ** MergeHunk
    MergeHunk (MergeHunk'),
    newMergeHunk,

    -- ** MergeHunkDetail
    MergeHunkDetail (MergeHunkDetail'),
    newMergeHunkDetail,

    -- ** MergeMetadata
    MergeMetadata (MergeMetadata'),
    newMergeMetadata,

    -- ** MergeOperations
    MergeOperations (MergeOperations'),
    newMergeOperations,

    -- ** ObjectTypes
    ObjectTypes (ObjectTypes'),
    newObjectTypes,

    -- ** OriginApprovalRuleTemplate
    OriginApprovalRuleTemplate (OriginApprovalRuleTemplate'),
    newOriginApprovalRuleTemplate,

    -- ** PullRequest
    PullRequest (PullRequest'),
    newPullRequest,

    -- ** PullRequestCreatedEventMetadata
    PullRequestCreatedEventMetadata (PullRequestCreatedEventMetadata'),
    newPullRequestCreatedEventMetadata,

    -- ** PullRequestEvent
    PullRequestEvent (PullRequestEvent'),
    newPullRequestEvent,

    -- ** PullRequestMergedStateChangedEventMetadata
    PullRequestMergedStateChangedEventMetadata (PullRequestMergedStateChangedEventMetadata'),
    newPullRequestMergedStateChangedEventMetadata,

    -- ** PullRequestSourceReferenceUpdatedEventMetadata
    PullRequestSourceReferenceUpdatedEventMetadata (PullRequestSourceReferenceUpdatedEventMetadata'),
    newPullRequestSourceReferenceUpdatedEventMetadata,

    -- ** PullRequestStatusChangedEventMetadata
    PullRequestStatusChangedEventMetadata (PullRequestStatusChangedEventMetadata'),
    newPullRequestStatusChangedEventMetadata,

    -- ** PullRequestTarget
    PullRequestTarget (PullRequestTarget'),
    newPullRequestTarget,

    -- ** PutFileEntry
    PutFileEntry (PutFileEntry'),
    newPutFileEntry,

    -- ** ReactionForComment
    ReactionForComment (ReactionForComment'),
    newReactionForComment,

    -- ** ReactionValueFormats
    ReactionValueFormats (ReactionValueFormats'),
    newReactionValueFormats,

    -- ** ReplaceContentEntry
    ReplaceContentEntry (ReplaceContentEntry'),
    newReplaceContentEntry,

    -- ** RepositoryMetadata
    RepositoryMetadata (RepositoryMetadata'),
    newRepositoryMetadata,

    -- ** RepositoryNameIdPair
    RepositoryNameIdPair (RepositoryNameIdPair'),
    newRepositoryNameIdPair,

    -- ** RepositoryTrigger
    RepositoryTrigger (RepositoryTrigger'),
    newRepositoryTrigger,

    -- ** RepositoryTriggerExecutionFailure
    RepositoryTriggerExecutionFailure (RepositoryTriggerExecutionFailure'),
    newRepositoryTriggerExecutionFailure,

    -- ** SetFileModeEntry
    SetFileModeEntry (SetFileModeEntry'),
    newSetFileModeEntry,

    -- ** SourceFileSpecifier
    SourceFileSpecifier (SourceFileSpecifier'),
    newSourceFileSpecifier,

    -- ** SubModule
    SubModule (SubModule'),
    newSubModule,

    -- ** SymbolicLink
    SymbolicLink (SymbolicLink'),
    newSymbolicLink,

    -- ** Target
    Target (Target'),
    newTarget,

    -- ** UserInfo
    UserInfo (UserInfo'),
    newUserInfo,
  )
where

import Network.AWS.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Network.AWS.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Network.AWS.CodeCommit.BatchDescribeMergeConflicts
import Network.AWS.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Network.AWS.CodeCommit.BatchGetCommits
import Network.AWS.CodeCommit.BatchGetRepositories
import Network.AWS.CodeCommit.CreateApprovalRuleTemplate
import Network.AWS.CodeCommit.CreateBranch
import Network.AWS.CodeCommit.CreateCommit
import Network.AWS.CodeCommit.CreatePullRequest
import Network.AWS.CodeCommit.CreatePullRequestApprovalRule
import Network.AWS.CodeCommit.CreateRepository
import Network.AWS.CodeCommit.CreateUnreferencedMergeCommit
import Network.AWS.CodeCommit.DeleteApprovalRuleTemplate
import Network.AWS.CodeCommit.DeleteBranch
import Network.AWS.CodeCommit.DeleteCommentContent
import Network.AWS.CodeCommit.DeleteFile
import Network.AWS.CodeCommit.DeletePullRequestApprovalRule
import Network.AWS.CodeCommit.DeleteRepository
import Network.AWS.CodeCommit.DescribeMergeConflicts
import Network.AWS.CodeCommit.DescribePullRequestEvents
import Network.AWS.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Network.AWS.CodeCommit.EvaluatePullRequestApprovalRules
import Network.AWS.CodeCommit.GetApprovalRuleTemplate
import Network.AWS.CodeCommit.GetBlob
import Network.AWS.CodeCommit.GetBranch
import Network.AWS.CodeCommit.GetComment
import Network.AWS.CodeCommit.GetCommentReactions
import Network.AWS.CodeCommit.GetCommentsForComparedCommit
import Network.AWS.CodeCommit.GetCommentsForPullRequest
import Network.AWS.CodeCommit.GetCommit
import Network.AWS.CodeCommit.GetDifferences
import Network.AWS.CodeCommit.GetFile
import Network.AWS.CodeCommit.GetFolder
import Network.AWS.CodeCommit.GetMergeCommit
import Network.AWS.CodeCommit.GetMergeConflicts
import Network.AWS.CodeCommit.GetMergeOptions
import Network.AWS.CodeCommit.GetPullRequest
import Network.AWS.CodeCommit.GetPullRequestApprovalStates
import Network.AWS.CodeCommit.GetPullRequestOverrideState
import Network.AWS.CodeCommit.GetRepository
import Network.AWS.CodeCommit.GetRepositoryTriggers
import Network.AWS.CodeCommit.Lens
import Network.AWS.CodeCommit.ListApprovalRuleTemplates
import Network.AWS.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import Network.AWS.CodeCommit.ListBranches
import Network.AWS.CodeCommit.ListPullRequests
import Network.AWS.CodeCommit.ListRepositories
import Network.AWS.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Network.AWS.CodeCommit.ListTagsForResource
import Network.AWS.CodeCommit.MergeBranchesByFastForward
import Network.AWS.CodeCommit.MergeBranchesBySquash
import Network.AWS.CodeCommit.MergeBranchesByThreeWay
import Network.AWS.CodeCommit.MergePullRequestByFastForward
import Network.AWS.CodeCommit.MergePullRequestBySquash
import Network.AWS.CodeCommit.MergePullRequestByThreeWay
import Network.AWS.CodeCommit.OverridePullRequestApprovalRules
import Network.AWS.CodeCommit.PostCommentForComparedCommit
import Network.AWS.CodeCommit.PostCommentForPullRequest
import Network.AWS.CodeCommit.PostCommentReply
import Network.AWS.CodeCommit.PutCommentReaction
import Network.AWS.CodeCommit.PutFile
import Network.AWS.CodeCommit.PutRepositoryTriggers
import Network.AWS.CodeCommit.TagResource
import Network.AWS.CodeCommit.TestRepositoryTriggers
import Network.AWS.CodeCommit.Types
import Network.AWS.CodeCommit.UntagResource
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateContent
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateDescription
import Network.AWS.CodeCommit.UpdateApprovalRuleTemplateName
import Network.AWS.CodeCommit.UpdateComment
import Network.AWS.CodeCommit.UpdateDefaultBranch
import Network.AWS.CodeCommit.UpdatePullRequestApprovalRuleContent
import Network.AWS.CodeCommit.UpdatePullRequestApprovalState
import Network.AWS.CodeCommit.UpdatePullRequestDescription
import Network.AWS.CodeCommit.UpdatePullRequestStatus
import Network.AWS.CodeCommit.UpdatePullRequestTitle
import Network.AWS.CodeCommit.UpdateRepositoryDescription
import Network.AWS.CodeCommit.UpdateRepositoryName
import Network.AWS.CodeCommit.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeCommit'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
