{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeCommit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2015-04-13@ of the AWS service descriptions, licensed under Apache 2.0.
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
module Amazonka.CodeCommit
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** RepositoryTriggerDestinationArnRequiredException
    _RepositoryTriggerDestinationArnRequiredException,

    -- ** InvalidTitleException
    _InvalidTitleException,

    -- ** InvalidRuleContentSha256Exception
    _InvalidRuleContentSha256Exception,

    -- ** ActorDoesNotExistException
    _ActorDoesNotExistException,

    -- ** CommitIdRequiredException
    _CommitIdRequiredException,

    -- ** InvalidSourceCommitSpecifierException
    _InvalidSourceCommitSpecifierException,

    -- ** FileEntryRequiredException
    _FileEntryRequiredException,

    -- ** InvalidRepositoryTriggerEventsException
    _InvalidRepositoryTriggerEventsException,

    -- ** InvalidRepositoryTriggerCustomDataException
    _InvalidRepositoryTriggerCustomDataException,

    -- ** TagKeysListRequiredException
    _TagKeysListRequiredException,

    -- ** OverrideAlreadySetException
    _OverrideAlreadySetException,

    -- ** InvalidReactionUserArnException
    _InvalidReactionUserArnException,

    -- ** EncryptionKeyAccessDeniedException
    _EncryptionKeyAccessDeniedException,

    -- ** DirectoryNameConflictsWithFileNameException
    _DirectoryNameConflictsWithFileNameException,

    -- ** OverrideStatusRequiredException
    _OverrideStatusRequiredException,

    -- ** CommitIdsLimitExceededException
    _CommitIdsLimitExceededException,

    -- ** BranchNameExistsException
    _BranchNameExistsException,

    -- ** FileTooLargeException
    _FileTooLargeException,

    -- ** InvalidFileModeException
    _InvalidFileModeException,

    -- ** RepositoryNotAssociatedWithPullRequestException
    _RepositoryNotAssociatedWithPullRequestException,

    -- ** InvalidContinuationTokenException
    _InvalidContinuationTokenException,

    -- ** ParentCommitIdOutdatedException
    _ParentCommitIdOutdatedException,

    -- ** ReferenceDoesNotExistException
    _ReferenceDoesNotExistException,

    -- ** MaximumFileEntriesExceededException
    _MaximumFileEntriesExceededException,

    -- ** InvalidFileLocationException
    _InvalidFileLocationException,

    -- ** TipsDivergenceExceededException
    _TipsDivergenceExceededException,

    -- ** MaximumRuleTemplatesAssociatedWithRepositoryException
    _MaximumRuleTemplatesAssociatedWithRepositoryException,

    -- ** EncryptionIntegrityChecksFailedException
    _EncryptionIntegrityChecksFailedException,

    -- ** InvalidTargetsException
    _InvalidTargetsException,

    -- ** BranchDoesNotExistException
    _BranchDoesNotExistException,

    -- ** InvalidDescriptionException
    _InvalidDescriptionException,

    -- ** RepositoryNameExistsException
    _RepositoryNameExistsException,

    -- ** FilePathConflictsWithSubmodulePathException
    _FilePathConflictsWithSubmodulePathException,

    -- ** ApprovalRuleTemplateDoesNotExistException
    _ApprovalRuleTemplateDoesNotExistException,

    -- ** InvalidPullRequestStatusUpdateException
    _InvalidPullRequestStatusUpdateException,

    -- ** TargetRequiredException
    _TargetRequiredException,

    -- ** CannotModifyApprovalRuleFromTemplateException
    _CannotModifyApprovalRuleFromTemplateException,

    -- ** CommitRequiredException
    _CommitRequiredException,

    -- ** InvalidMaxMergeHunksException
    _InvalidMaxMergeHunksException,

    -- ** RestrictedSourceFileException
    _RestrictedSourceFileException,

    -- ** MultipleConflictResolutionEntriesException
    _MultipleConflictResolutionEntriesException,

    -- ** BranchNameIsTagNameException
    _BranchNameIsTagNameException,

    -- ** InvalidCommentIdException
    _InvalidCommentIdException,

    -- ** InvalidActorArnException
    _InvalidActorArnException,

    -- ** PullRequestAlreadyClosedException
    _PullRequestAlreadyClosedException,

    -- ** ApprovalRuleTemplateInUseException
    _ApprovalRuleTemplateInUseException,

    -- ** InvalidRepositoryTriggerNameException
    _InvalidRepositoryTriggerNameException,

    -- ** ResourceArnRequiredException
    _ResourceArnRequiredException,

    -- ** SourceFileOrContentRequiredException
    _SourceFileOrContentRequiredException,

    -- ** InvalidReplacementTypeException
    _InvalidReplacementTypeException,

    -- ** InvalidRepositoryNameException
    _InvalidRepositoryNameException,

    -- ** TooManyTagsException
    _TooManyTagsException,

    -- ** SourceAndDestinationAreSameException
    _SourceAndDestinationAreSameException,

    -- ** InvalidDestinationCommitSpecifierException
    _InvalidDestinationCommitSpecifierException,

    -- ** InvalidFilePositionException
    _InvalidFilePositionException,

    -- ** NameLengthExceededException
    _NameLengthExceededException,

    -- ** InvalidBlobIdException
    _InvalidBlobIdException,

    -- ** PathDoesNotExistException
    _PathDoesNotExistException,

    -- ** ParentCommitDoesNotExistException
    _ParentCommitDoesNotExistException,

    -- ** InvalidTagsMapException
    _InvalidTagsMapException,

    -- ** CommitMessageLengthExceededException
    _CommitMessageLengthExceededException,

    -- ** RepositoryDoesNotExistException
    _RepositoryDoesNotExistException,

    -- ** InvalidReplacementContentException
    _InvalidReplacementContentException,

    -- ** InvalidOrderException
    _InvalidOrderException,

    -- ** InvalidTagKeysListException
    _InvalidTagKeysListException,

    -- ** PullRequestStatusRequiredException
    _PullRequestStatusRequiredException,

    -- ** InvalidPullRequestIdException
    _InvalidPullRequestIdException,

    -- ** InvalidApprovalRuleTemplateDescriptionException
    _InvalidApprovalRuleTemplateDescriptionException,

    -- ** PullRequestCannotBeApprovedByAuthorException
    _PullRequestCannotBeApprovedByAuthorException,

    -- ** EncryptionKeyDisabledException
    _EncryptionKeyDisabledException,

    -- ** RevisionIdRequiredException
    _RevisionIdRequiredException,

    -- ** CommentContentRequiredException
    _CommentContentRequiredException,

    -- ** InvalidEmailException
    _InvalidEmailException,

    -- ** RevisionNotCurrentException
    _RevisionNotCurrentException,

    -- ** MaximumFileContentToLoadExceededException
    _MaximumFileContentToLoadExceededException,

    -- ** ApprovalRuleNameAlreadyExistsException
    _ApprovalRuleNameAlreadyExistsException,

    -- ** InvalidRepositoryTriggerRegionException
    _InvalidRepositoryTriggerRegionException,

    -- ** ApprovalRuleTemplateNameAlreadyExistsException
    _ApprovalRuleTemplateNameAlreadyExistsException,

    -- ** InvalidReferenceNameException
    _InvalidReferenceNameException,

    -- ** CommentIdRequiredException
    _CommentIdRequiredException,

    -- ** ReactionValueRequiredException
    _ReactionValueRequiredException,

    -- ** SameFileContentException
    _SameFileContentException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** InvalidRevisionIdException
    _InvalidRevisionIdException,

    -- ** InvalidBranchNameException
    _InvalidBranchNameException,

    -- ** InvalidReactionValueException
    _InvalidReactionValueException,

    -- ** InvalidApprovalRuleContentException
    _InvalidApprovalRuleContentException,

    -- ** ApprovalRuleTemplateNameRequiredException
    _ApprovalRuleTemplateNameRequiredException,

    -- ** InvalidApprovalRuleTemplateContentException
    _InvalidApprovalRuleTemplateContentException,

    -- ** InvalidClientRequestTokenException
    _InvalidClientRequestTokenException,

    -- ** InvalidTargetBranchException
    _InvalidTargetBranchException,

    -- ** NoChangeException
    _NoChangeException,

    -- ** MaximumRepositoryTriggersExceededException
    _MaximumRepositoryTriggersExceededException,

    -- ** PullRequestDoesNotExistException
    _PullRequestDoesNotExistException,

    -- ** ReplacementTypeRequiredException
    _ReplacementTypeRequiredException,

    -- ** MaximumItemsToCompareExceededException
    _MaximumItemsToCompareExceededException,

    -- ** FileContentAndSourceFileSpecifiedException
    _FileContentAndSourceFileSpecifiedException,

    -- ** RepositoryNamesRequiredException
    _RepositoryNamesRequiredException,

    -- ** TitleRequiredException
    _TitleRequiredException,

    -- ** PathRequiredException
    _PathRequiredException,

    -- ** ReplacementContentRequiredException
    _ReplacementContentRequiredException,

    -- ** InvalidApprovalRuleTemplateNameException
    _InvalidApprovalRuleTemplateNameException,

    -- ** FileContentRequiredException
    _FileContentRequiredException,

    -- ** ReferenceTypeNotSupportedException
    _ReferenceTypeNotSupportedException,

    -- ** RepositoryTriggerBranchNameListRequiredException
    _RepositoryTriggerBranchNameListRequiredException,

    -- ** MaximumNumberOfApprovalsExceededException
    _MaximumNumberOfApprovalsExceededException,

    -- ** RepositoryTriggerEventsListRequiredException
    _RepositoryTriggerEventsListRequiredException,

    -- ** FileDoesNotExistException
    _FileDoesNotExistException,

    -- ** InvalidApprovalStateException
    _InvalidApprovalStateException,

    -- ** CommentNotCreatedByCallerException
    _CommentNotCreatedByCallerException,

    -- ** ReactionLimitExceededException
    _ReactionLimitExceededException,

    -- ** MaximumBranchesExceededException
    _MaximumBranchesExceededException,

    -- ** InvalidConflictDetailLevelException
    _InvalidConflictDetailLevelException,

    -- ** InvalidRepositoryTriggerBranchNameException
    _InvalidRepositoryTriggerBranchNameException,

    -- ** FolderDoesNotExistException
    _FolderDoesNotExistException,

    -- ** BeforeCommitIdAndAfterCommitIdAreSameException
    _BeforeCommitIdAndAfterCommitIdAreSameException,

    -- ** BlobIdDoesNotExistException
    _BlobIdDoesNotExistException,

    -- ** RepositoryLimitExceededException
    _RepositoryLimitExceededException,

    -- ** CommentDeletedException
    _CommentDeletedException,

    -- ** ReferenceNameRequiredException
    _ReferenceNameRequiredException,

    -- ** ConcurrentReferenceUpdateException
    _ConcurrentReferenceUpdateException,

    -- ** TargetsRequiredException
    _TargetsRequiredException,

    -- ** ClientRequestTokenRequiredException
    _ClientRequestTokenRequiredException,

    -- ** BranchNameRequiredException
    _BranchNameRequiredException,

    -- ** InvalidDeletionParameterException
    _InvalidDeletionParameterException,

    -- ** InvalidCommitException
    _InvalidCommitException,

    -- ** MaximumConflictResolutionEntriesExceededException
    _MaximumConflictResolutionEntriesExceededException,

    -- ** InvalidSortByException
    _InvalidSortByException,

    -- ** PutFileEntryConflictException
    _PutFileEntryConflictException,

    -- ** SamePathRequestException
    _SamePathRequestException,

    -- ** InvalidMaxConflictFilesException
    _InvalidMaxConflictFilesException,

    -- ** InvalidTargetException
    _InvalidTargetException,

    -- ** MergeOptionRequiredException
    _MergeOptionRequiredException,

    -- ** PullRequestIdRequiredException
    _PullRequestIdRequiredException,

    -- ** InvalidSystemTagUsageException
    _InvalidSystemTagUsageException,

    -- ** InvalidPullRequestEventTypeException
    _InvalidPullRequestEventTypeException,

    -- ** EncryptionKeyUnavailableException
    _EncryptionKeyUnavailableException,

    -- ** EncryptionKeyNotFoundException
    _EncryptionKeyNotFoundException,

    -- ** ApprovalRuleDoesNotExistException
    _ApprovalRuleDoesNotExistException,

    -- ** MaximumRepositoryNamesExceededException
    _MaximumRepositoryNamesExceededException,

    -- ** MaximumOpenPullRequestsExceededException
    _MaximumOpenPullRequestsExceededException,

    -- ** InvalidConflictResolutionStrategyException
    _InvalidConflictResolutionStrategyException,

    -- ** InvalidPathException
    _InvalidPathException,

    -- ** InvalidRepositoryTriggerDestinationArnException
    _InvalidRepositoryTriggerDestinationArnException,

    -- ** FileContentSizeLimitExceededException
    _FileContentSizeLimitExceededException,

    -- ** InvalidResourceArnException
    _InvalidResourceArnException,

    -- ** ManualMergeRequiredException
    _ManualMergeRequiredException,

    -- ** InvalidOverrideStatusException
    _InvalidOverrideStatusException,

    -- ** NumberOfRuleTemplatesExceededException
    _NumberOfRuleTemplatesExceededException,

    -- ** BlobIdRequiredException
    _BlobIdRequiredException,

    -- ** RepositoryTriggerNameRequiredException
    _RepositoryTriggerNameRequiredException,

    -- ** NumberOfRulesExceededException
    _NumberOfRulesExceededException,

    -- ** PullRequestApprovalRulesNotSatisfiedException
    _PullRequestApprovalRulesNotSatisfiedException,

    -- ** CommitIdsListRequiredException
    _CommitIdsListRequiredException,

    -- ** FolderContentSizeLimitExceededException
    _FolderContentSizeLimitExceededException,

    -- ** RepositoryNameRequiredException
    _RepositoryNameRequiredException,

    -- ** IdempotencyParameterMismatchException
    _IdempotencyParameterMismatchException,

    -- ** FileNameConflictsWithDirectoryNameException
    _FileNameConflictsWithDirectoryNameException,

    -- ** CommentContentSizeLimitExceededException
    _CommentContentSizeLimitExceededException,

    -- ** CommitIdDoesNotExistException
    _CommitIdDoesNotExistException,

    -- ** CannotDeleteApprovalRuleFromTemplateException
    _CannotDeleteApprovalRuleFromTemplateException,

    -- ** ParentCommitIdRequiredException
    _ParentCommitIdRequiredException,

    -- ** CommentDoesNotExistException
    _CommentDoesNotExistException,

    -- ** TagsMapRequiredException
    _TagsMapRequiredException,

    -- ** CommitDoesNotExistException
    _CommitDoesNotExistException,

    -- ** InvalidConflictResolutionException
    _InvalidConflictResolutionException,

    -- ** FileModeRequiredException
    _FileModeRequiredException,

    -- ** InvalidMergeOptionException
    _InvalidMergeOptionException,

    -- ** InvalidAuthorArnException
    _InvalidAuthorArnException,

    -- ** InvalidPullRequestStatusException
    _InvalidPullRequestStatusException,

    -- ** InvalidRepositoryDescriptionException
    _InvalidRepositoryDescriptionException,

    -- ** ApprovalRuleContentRequiredException
    _ApprovalRuleContentRequiredException,

    -- ** MultipleRepositoriesInPullRequestException
    _MultipleRepositoriesInPullRequestException,

    -- ** InvalidCommitIdException
    _InvalidCommitIdException,

    -- ** TipOfSourceReferenceIsDifferentException
    _TipOfSourceReferenceIsDifferentException,

    -- ** ApprovalRuleTemplateContentRequiredException
    _ApprovalRuleTemplateContentRequiredException,

    -- ** ApprovalRuleNameRequiredException
    _ApprovalRuleNameRequiredException,

    -- ** TagPolicyException
    _TagPolicyException,

    -- ** InvalidApprovalRuleNameException
    _InvalidApprovalRuleNameException,

    -- ** AuthorDoesNotExistException
    _AuthorDoesNotExistException,

    -- ** InvalidRelativeFileVersionEnumException
    _InvalidRelativeFileVersionEnumException,

    -- ** RepositoryTriggersListRequiredException
    _RepositoryTriggersListRequiredException,

    -- ** ApprovalStateRequiredException
    _ApprovalStateRequiredException,

    -- ** DefaultBranchCannotBeDeletedException
    _DefaultBranchCannotBeDeletedException,

    -- ** InvalidParentCommitIdException
    _InvalidParentCommitIdException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateApprovalRuleTemplateWithRepository
    AssociateApprovalRuleTemplateWithRepository (AssociateApprovalRuleTemplateWithRepository'),
    newAssociateApprovalRuleTemplateWithRepository,
    AssociateApprovalRuleTemplateWithRepositoryResponse (AssociateApprovalRuleTemplateWithRepositoryResponse'),
    newAssociateApprovalRuleTemplateWithRepositoryResponse,

    -- ** BatchAssociateApprovalRuleTemplateWithRepositories
    BatchAssociateApprovalRuleTemplateWithRepositories (BatchAssociateApprovalRuleTemplateWithRepositories'),
    newBatchAssociateApprovalRuleTemplateWithRepositories,
    BatchAssociateApprovalRuleTemplateWithRepositoriesResponse (BatchAssociateApprovalRuleTemplateWithRepositoriesResponse'),
    newBatchAssociateApprovalRuleTemplateWithRepositoriesResponse,

    -- ** BatchDescribeMergeConflicts
    BatchDescribeMergeConflicts (BatchDescribeMergeConflicts'),
    newBatchDescribeMergeConflicts,
    BatchDescribeMergeConflictsResponse (BatchDescribeMergeConflictsResponse'),
    newBatchDescribeMergeConflictsResponse,

    -- ** BatchDisassociateApprovalRuleTemplateFromRepositories
    BatchDisassociateApprovalRuleTemplateFromRepositories (BatchDisassociateApprovalRuleTemplateFromRepositories'),
    newBatchDisassociateApprovalRuleTemplateFromRepositories,
    BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse (BatchDisassociateApprovalRuleTemplateFromRepositoriesResponse'),
    newBatchDisassociateApprovalRuleTemplateFromRepositoriesResponse,

    -- ** BatchGetCommits
    BatchGetCommits (BatchGetCommits'),
    newBatchGetCommits,
    BatchGetCommitsResponse (BatchGetCommitsResponse'),
    newBatchGetCommitsResponse,

    -- ** BatchGetRepositories
    BatchGetRepositories (BatchGetRepositories'),
    newBatchGetRepositories,
    BatchGetRepositoriesResponse (BatchGetRepositoriesResponse'),
    newBatchGetRepositoriesResponse,

    -- ** CreateApprovalRuleTemplate
    CreateApprovalRuleTemplate (CreateApprovalRuleTemplate'),
    newCreateApprovalRuleTemplate,
    CreateApprovalRuleTemplateResponse (CreateApprovalRuleTemplateResponse'),
    newCreateApprovalRuleTemplateResponse,

    -- ** CreateBranch
    CreateBranch (CreateBranch'),
    newCreateBranch,
    CreateBranchResponse (CreateBranchResponse'),
    newCreateBranchResponse,

    -- ** CreateCommit
    CreateCommit (CreateCommit'),
    newCreateCommit,
    CreateCommitResponse (CreateCommitResponse'),
    newCreateCommitResponse,

    -- ** CreatePullRequest
    CreatePullRequest (CreatePullRequest'),
    newCreatePullRequest,
    CreatePullRequestResponse (CreatePullRequestResponse'),
    newCreatePullRequestResponse,

    -- ** CreatePullRequestApprovalRule
    CreatePullRequestApprovalRule (CreatePullRequestApprovalRule'),
    newCreatePullRequestApprovalRule,
    CreatePullRequestApprovalRuleResponse (CreatePullRequestApprovalRuleResponse'),
    newCreatePullRequestApprovalRuleResponse,

    -- ** CreateRepository
    CreateRepository (CreateRepository'),
    newCreateRepository,
    CreateRepositoryResponse (CreateRepositoryResponse'),
    newCreateRepositoryResponse,

    -- ** CreateUnreferencedMergeCommit
    CreateUnreferencedMergeCommit (CreateUnreferencedMergeCommit'),
    newCreateUnreferencedMergeCommit,
    CreateUnreferencedMergeCommitResponse (CreateUnreferencedMergeCommitResponse'),
    newCreateUnreferencedMergeCommitResponse,

    -- ** DeleteApprovalRuleTemplate
    DeleteApprovalRuleTemplate (DeleteApprovalRuleTemplate'),
    newDeleteApprovalRuleTemplate,
    DeleteApprovalRuleTemplateResponse (DeleteApprovalRuleTemplateResponse'),
    newDeleteApprovalRuleTemplateResponse,

    -- ** DeleteBranch
    DeleteBranch (DeleteBranch'),
    newDeleteBranch,
    DeleteBranchResponse (DeleteBranchResponse'),
    newDeleteBranchResponse,

    -- ** DeleteCommentContent
    DeleteCommentContent (DeleteCommentContent'),
    newDeleteCommentContent,
    DeleteCommentContentResponse (DeleteCommentContentResponse'),
    newDeleteCommentContentResponse,

    -- ** DeleteFile
    DeleteFile (DeleteFile'),
    newDeleteFile,
    DeleteFileResponse (DeleteFileResponse'),
    newDeleteFileResponse,

    -- ** DeletePullRequestApprovalRule
    DeletePullRequestApprovalRule (DeletePullRequestApprovalRule'),
    newDeletePullRequestApprovalRule,
    DeletePullRequestApprovalRuleResponse (DeletePullRequestApprovalRuleResponse'),
    newDeletePullRequestApprovalRuleResponse,

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

    -- ** DescribePullRequestEvents (Paginated)
    DescribePullRequestEvents (DescribePullRequestEvents'),
    newDescribePullRequestEvents,
    DescribePullRequestEventsResponse (DescribePullRequestEventsResponse'),
    newDescribePullRequestEventsResponse,

    -- ** DisassociateApprovalRuleTemplateFromRepository
    DisassociateApprovalRuleTemplateFromRepository (DisassociateApprovalRuleTemplateFromRepository'),
    newDisassociateApprovalRuleTemplateFromRepository,
    DisassociateApprovalRuleTemplateFromRepositoryResponse (DisassociateApprovalRuleTemplateFromRepositoryResponse'),
    newDisassociateApprovalRuleTemplateFromRepositoryResponse,

    -- ** EvaluatePullRequestApprovalRules
    EvaluatePullRequestApprovalRules (EvaluatePullRequestApprovalRules'),
    newEvaluatePullRequestApprovalRules,
    EvaluatePullRequestApprovalRulesResponse (EvaluatePullRequestApprovalRulesResponse'),
    newEvaluatePullRequestApprovalRulesResponse,

    -- ** GetApprovalRuleTemplate
    GetApprovalRuleTemplate (GetApprovalRuleTemplate'),
    newGetApprovalRuleTemplate,
    GetApprovalRuleTemplateResponse (GetApprovalRuleTemplateResponse'),
    newGetApprovalRuleTemplateResponse,

    -- ** GetBlob
    GetBlob (GetBlob'),
    newGetBlob,
    GetBlobResponse (GetBlobResponse'),
    newGetBlobResponse,

    -- ** GetBranch
    GetBranch (GetBranch'),
    newGetBranch,
    GetBranchResponse (GetBranchResponse'),
    newGetBranchResponse,

    -- ** GetComment
    GetComment (GetComment'),
    newGetComment,
    GetCommentResponse (GetCommentResponse'),
    newGetCommentResponse,

    -- ** GetCommentReactions
    GetCommentReactions (GetCommentReactions'),
    newGetCommentReactions,
    GetCommentReactionsResponse (GetCommentReactionsResponse'),
    newGetCommentReactionsResponse,

    -- ** GetCommentsForComparedCommit (Paginated)
    GetCommentsForComparedCommit (GetCommentsForComparedCommit'),
    newGetCommentsForComparedCommit,
    GetCommentsForComparedCommitResponse (GetCommentsForComparedCommitResponse'),
    newGetCommentsForComparedCommitResponse,

    -- ** GetCommentsForPullRequest (Paginated)
    GetCommentsForPullRequest (GetCommentsForPullRequest'),
    newGetCommentsForPullRequest,
    GetCommentsForPullRequestResponse (GetCommentsForPullRequestResponse'),
    newGetCommentsForPullRequestResponse,

    -- ** GetCommit
    GetCommit (GetCommit'),
    newGetCommit,
    GetCommitResponse (GetCommitResponse'),
    newGetCommitResponse,

    -- ** GetDifferences (Paginated)
    GetDifferences (GetDifferences'),
    newGetDifferences,
    GetDifferencesResponse (GetDifferencesResponse'),
    newGetDifferencesResponse,

    -- ** GetFile
    GetFile (GetFile'),
    newGetFile,
    GetFileResponse (GetFileResponse'),
    newGetFileResponse,

    -- ** GetFolder
    GetFolder (GetFolder'),
    newGetFolder,
    GetFolderResponse (GetFolderResponse'),
    newGetFolderResponse,

    -- ** GetMergeCommit
    GetMergeCommit (GetMergeCommit'),
    newGetMergeCommit,
    GetMergeCommitResponse (GetMergeCommitResponse'),
    newGetMergeCommitResponse,

    -- ** GetMergeConflicts
    GetMergeConflicts (GetMergeConflicts'),
    newGetMergeConflicts,
    GetMergeConflictsResponse (GetMergeConflictsResponse'),
    newGetMergeConflictsResponse,

    -- ** GetMergeOptions
    GetMergeOptions (GetMergeOptions'),
    newGetMergeOptions,
    GetMergeOptionsResponse (GetMergeOptionsResponse'),
    newGetMergeOptionsResponse,

    -- ** GetPullRequest
    GetPullRequest (GetPullRequest'),
    newGetPullRequest,
    GetPullRequestResponse (GetPullRequestResponse'),
    newGetPullRequestResponse,

    -- ** GetPullRequestApprovalStates
    GetPullRequestApprovalStates (GetPullRequestApprovalStates'),
    newGetPullRequestApprovalStates,
    GetPullRequestApprovalStatesResponse (GetPullRequestApprovalStatesResponse'),
    newGetPullRequestApprovalStatesResponse,

    -- ** GetPullRequestOverrideState
    GetPullRequestOverrideState (GetPullRequestOverrideState'),
    newGetPullRequestOverrideState,
    GetPullRequestOverrideStateResponse (GetPullRequestOverrideStateResponse'),
    newGetPullRequestOverrideStateResponse,

    -- ** GetRepository
    GetRepository (GetRepository'),
    newGetRepository,
    GetRepositoryResponse (GetRepositoryResponse'),
    newGetRepositoryResponse,

    -- ** GetRepositoryTriggers
    GetRepositoryTriggers (GetRepositoryTriggers'),
    newGetRepositoryTriggers,
    GetRepositoryTriggersResponse (GetRepositoryTriggersResponse'),
    newGetRepositoryTriggersResponse,

    -- ** ListApprovalRuleTemplates
    ListApprovalRuleTemplates (ListApprovalRuleTemplates'),
    newListApprovalRuleTemplates,
    ListApprovalRuleTemplatesResponse (ListApprovalRuleTemplatesResponse'),
    newListApprovalRuleTemplatesResponse,

    -- ** ListAssociatedApprovalRuleTemplatesForRepository
    ListAssociatedApprovalRuleTemplatesForRepository (ListAssociatedApprovalRuleTemplatesForRepository'),
    newListAssociatedApprovalRuleTemplatesForRepository,
    ListAssociatedApprovalRuleTemplatesForRepositoryResponse (ListAssociatedApprovalRuleTemplatesForRepositoryResponse'),
    newListAssociatedApprovalRuleTemplatesForRepositoryResponse,

    -- ** ListBranches (Paginated)
    ListBranches (ListBranches'),
    newListBranches,
    ListBranchesResponse (ListBranchesResponse'),
    newListBranchesResponse,

    -- ** ListPullRequests (Paginated)
    ListPullRequests (ListPullRequests'),
    newListPullRequests,
    ListPullRequestsResponse (ListPullRequestsResponse'),
    newListPullRequestsResponse,

    -- ** ListRepositories (Paginated)
    ListRepositories (ListRepositories'),
    newListRepositories,
    ListRepositoriesResponse (ListRepositoriesResponse'),
    newListRepositoriesResponse,

    -- ** ListRepositoriesForApprovalRuleTemplate
    ListRepositoriesForApprovalRuleTemplate (ListRepositoriesForApprovalRuleTemplate'),
    newListRepositoriesForApprovalRuleTemplate,
    ListRepositoriesForApprovalRuleTemplateResponse (ListRepositoriesForApprovalRuleTemplateResponse'),
    newListRepositoriesForApprovalRuleTemplateResponse,

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

    -- ** MergeBranchesBySquash
    MergeBranchesBySquash (MergeBranchesBySquash'),
    newMergeBranchesBySquash,
    MergeBranchesBySquashResponse (MergeBranchesBySquashResponse'),
    newMergeBranchesBySquashResponse,

    -- ** MergeBranchesByThreeWay
    MergeBranchesByThreeWay (MergeBranchesByThreeWay'),
    newMergeBranchesByThreeWay,
    MergeBranchesByThreeWayResponse (MergeBranchesByThreeWayResponse'),
    newMergeBranchesByThreeWayResponse,

    -- ** MergePullRequestByFastForward
    MergePullRequestByFastForward (MergePullRequestByFastForward'),
    newMergePullRequestByFastForward,
    MergePullRequestByFastForwardResponse (MergePullRequestByFastForwardResponse'),
    newMergePullRequestByFastForwardResponse,

    -- ** MergePullRequestBySquash
    MergePullRequestBySquash (MergePullRequestBySquash'),
    newMergePullRequestBySquash,
    MergePullRequestBySquashResponse (MergePullRequestBySquashResponse'),
    newMergePullRequestBySquashResponse,

    -- ** MergePullRequestByThreeWay
    MergePullRequestByThreeWay (MergePullRequestByThreeWay'),
    newMergePullRequestByThreeWay,
    MergePullRequestByThreeWayResponse (MergePullRequestByThreeWayResponse'),
    newMergePullRequestByThreeWayResponse,

    -- ** OverridePullRequestApprovalRules
    OverridePullRequestApprovalRules (OverridePullRequestApprovalRules'),
    newOverridePullRequestApprovalRules,
    OverridePullRequestApprovalRulesResponse (OverridePullRequestApprovalRulesResponse'),
    newOverridePullRequestApprovalRulesResponse,

    -- ** PostCommentForComparedCommit
    PostCommentForComparedCommit (PostCommentForComparedCommit'),
    newPostCommentForComparedCommit,
    PostCommentForComparedCommitResponse (PostCommentForComparedCommitResponse'),
    newPostCommentForComparedCommitResponse,

    -- ** PostCommentForPullRequest
    PostCommentForPullRequest (PostCommentForPullRequest'),
    newPostCommentForPullRequest,
    PostCommentForPullRequestResponse (PostCommentForPullRequestResponse'),
    newPostCommentForPullRequestResponse,

    -- ** PostCommentReply
    PostCommentReply (PostCommentReply'),
    newPostCommentReply,
    PostCommentReplyResponse (PostCommentReplyResponse'),
    newPostCommentReplyResponse,

    -- ** PutCommentReaction
    PutCommentReaction (PutCommentReaction'),
    newPutCommentReaction,
    PutCommentReactionResponse (PutCommentReactionResponse'),
    newPutCommentReactionResponse,

    -- ** PutFile
    PutFile (PutFile'),
    newPutFile,
    PutFileResponse (PutFileResponse'),
    newPutFileResponse,

    -- ** PutRepositoryTriggers
    PutRepositoryTriggers (PutRepositoryTriggers'),
    newPutRepositoryTriggers,
    PutRepositoryTriggersResponse (PutRepositoryTriggersResponse'),
    newPutRepositoryTriggersResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** TestRepositoryTriggers
    TestRepositoryTriggers (TestRepositoryTriggers'),
    newTestRepositoryTriggers,
    TestRepositoryTriggersResponse (TestRepositoryTriggersResponse'),
    newTestRepositoryTriggersResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateApprovalRuleTemplateContent
    UpdateApprovalRuleTemplateContent (UpdateApprovalRuleTemplateContent'),
    newUpdateApprovalRuleTemplateContent,
    UpdateApprovalRuleTemplateContentResponse (UpdateApprovalRuleTemplateContentResponse'),
    newUpdateApprovalRuleTemplateContentResponse,

    -- ** UpdateApprovalRuleTemplateDescription
    UpdateApprovalRuleTemplateDescription (UpdateApprovalRuleTemplateDescription'),
    newUpdateApprovalRuleTemplateDescription,
    UpdateApprovalRuleTemplateDescriptionResponse (UpdateApprovalRuleTemplateDescriptionResponse'),
    newUpdateApprovalRuleTemplateDescriptionResponse,

    -- ** UpdateApprovalRuleTemplateName
    UpdateApprovalRuleTemplateName (UpdateApprovalRuleTemplateName'),
    newUpdateApprovalRuleTemplateName,
    UpdateApprovalRuleTemplateNameResponse (UpdateApprovalRuleTemplateNameResponse'),
    newUpdateApprovalRuleTemplateNameResponse,

    -- ** UpdateComment
    UpdateComment (UpdateComment'),
    newUpdateComment,
    UpdateCommentResponse (UpdateCommentResponse'),
    newUpdateCommentResponse,

    -- ** UpdateDefaultBranch
    UpdateDefaultBranch (UpdateDefaultBranch'),
    newUpdateDefaultBranch,
    UpdateDefaultBranchResponse (UpdateDefaultBranchResponse'),
    newUpdateDefaultBranchResponse,

    -- ** UpdatePullRequestApprovalRuleContent
    UpdatePullRequestApprovalRuleContent (UpdatePullRequestApprovalRuleContent'),
    newUpdatePullRequestApprovalRuleContent,
    UpdatePullRequestApprovalRuleContentResponse (UpdatePullRequestApprovalRuleContentResponse'),
    newUpdatePullRequestApprovalRuleContentResponse,

    -- ** UpdatePullRequestApprovalState
    UpdatePullRequestApprovalState (UpdatePullRequestApprovalState'),
    newUpdatePullRequestApprovalState,
    UpdatePullRequestApprovalStateResponse (UpdatePullRequestApprovalStateResponse'),
    newUpdatePullRequestApprovalStateResponse,

    -- ** UpdatePullRequestDescription
    UpdatePullRequestDescription (UpdatePullRequestDescription'),
    newUpdatePullRequestDescription,
    UpdatePullRequestDescriptionResponse (UpdatePullRequestDescriptionResponse'),
    newUpdatePullRequestDescriptionResponse,

    -- ** UpdatePullRequestStatus
    UpdatePullRequestStatus (UpdatePullRequestStatus'),
    newUpdatePullRequestStatus,
    UpdatePullRequestStatusResponse (UpdatePullRequestStatusResponse'),
    newUpdatePullRequestStatusResponse,

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

    -- ** UpdateRepositoryName
    UpdateRepositoryName (UpdateRepositoryName'),
    newUpdateRepositoryName,
    UpdateRepositoryNameResponse (UpdateRepositoryNameResponse'),
    newUpdateRepositoryNameResponse,

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

import Amazonka.CodeCommit.AssociateApprovalRuleTemplateWithRepository
import Amazonka.CodeCommit.BatchAssociateApprovalRuleTemplateWithRepositories
import Amazonka.CodeCommit.BatchDescribeMergeConflicts
import Amazonka.CodeCommit.BatchDisassociateApprovalRuleTemplateFromRepositories
import Amazonka.CodeCommit.BatchGetCommits
import Amazonka.CodeCommit.BatchGetRepositories
import Amazonka.CodeCommit.CreateApprovalRuleTemplate
import Amazonka.CodeCommit.CreateBranch
import Amazonka.CodeCommit.CreateCommit
import Amazonka.CodeCommit.CreatePullRequest
import Amazonka.CodeCommit.CreatePullRequestApprovalRule
import Amazonka.CodeCommit.CreateRepository
import Amazonka.CodeCommit.CreateUnreferencedMergeCommit
import Amazonka.CodeCommit.DeleteApprovalRuleTemplate
import Amazonka.CodeCommit.DeleteBranch
import Amazonka.CodeCommit.DeleteCommentContent
import Amazonka.CodeCommit.DeleteFile
import Amazonka.CodeCommit.DeletePullRequestApprovalRule
import Amazonka.CodeCommit.DeleteRepository
import Amazonka.CodeCommit.DescribeMergeConflicts
import Amazonka.CodeCommit.DescribePullRequestEvents
import Amazonka.CodeCommit.DisassociateApprovalRuleTemplateFromRepository
import Amazonka.CodeCommit.EvaluatePullRequestApprovalRules
import Amazonka.CodeCommit.GetApprovalRuleTemplate
import Amazonka.CodeCommit.GetBlob
import Amazonka.CodeCommit.GetBranch
import Amazonka.CodeCommit.GetComment
import Amazonka.CodeCommit.GetCommentReactions
import Amazonka.CodeCommit.GetCommentsForComparedCommit
import Amazonka.CodeCommit.GetCommentsForPullRequest
import Amazonka.CodeCommit.GetCommit
import Amazonka.CodeCommit.GetDifferences
import Amazonka.CodeCommit.GetFile
import Amazonka.CodeCommit.GetFolder
import Amazonka.CodeCommit.GetMergeCommit
import Amazonka.CodeCommit.GetMergeConflicts
import Amazonka.CodeCommit.GetMergeOptions
import Amazonka.CodeCommit.GetPullRequest
import Amazonka.CodeCommit.GetPullRequestApprovalStates
import Amazonka.CodeCommit.GetPullRequestOverrideState
import Amazonka.CodeCommit.GetRepository
import Amazonka.CodeCommit.GetRepositoryTriggers
import Amazonka.CodeCommit.Lens
import Amazonka.CodeCommit.ListApprovalRuleTemplates
import Amazonka.CodeCommit.ListAssociatedApprovalRuleTemplatesForRepository
import Amazonka.CodeCommit.ListBranches
import Amazonka.CodeCommit.ListPullRequests
import Amazonka.CodeCommit.ListRepositories
import Amazonka.CodeCommit.ListRepositoriesForApprovalRuleTemplate
import Amazonka.CodeCommit.ListTagsForResource
import Amazonka.CodeCommit.MergeBranchesByFastForward
import Amazonka.CodeCommit.MergeBranchesBySquash
import Amazonka.CodeCommit.MergeBranchesByThreeWay
import Amazonka.CodeCommit.MergePullRequestByFastForward
import Amazonka.CodeCommit.MergePullRequestBySquash
import Amazonka.CodeCommit.MergePullRequestByThreeWay
import Amazonka.CodeCommit.OverridePullRequestApprovalRules
import Amazonka.CodeCommit.PostCommentForComparedCommit
import Amazonka.CodeCommit.PostCommentForPullRequest
import Amazonka.CodeCommit.PostCommentReply
import Amazonka.CodeCommit.PutCommentReaction
import Amazonka.CodeCommit.PutFile
import Amazonka.CodeCommit.PutRepositoryTriggers
import Amazonka.CodeCommit.TagResource
import Amazonka.CodeCommit.TestRepositoryTriggers
import Amazonka.CodeCommit.Types
import Amazonka.CodeCommit.UntagResource
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateContent
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateDescription
import Amazonka.CodeCommit.UpdateApprovalRuleTemplateName
import Amazonka.CodeCommit.UpdateComment
import Amazonka.CodeCommit.UpdateDefaultBranch
import Amazonka.CodeCommit.UpdatePullRequestApprovalRuleContent
import Amazonka.CodeCommit.UpdatePullRequestApprovalState
import Amazonka.CodeCommit.UpdatePullRequestDescription
import Amazonka.CodeCommit.UpdatePullRequestStatus
import Amazonka.CodeCommit.UpdatePullRequestTitle
import Amazonka.CodeCommit.UpdateRepositoryDescription
import Amazonka.CodeCommit.UpdateRepositoryName
import Amazonka.CodeCommit.Waiters

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
