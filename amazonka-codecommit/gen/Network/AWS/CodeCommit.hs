{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS CodeCommit
--
-- This is the /AWS CodeCommit API Reference/. This reference provides descriptions of the operations and data types for AWS CodeCommit API.
--
-- You can use the AWS CodeCommit API to work with the following objects:
--
-- -   Repositories, by calling the following:
--     -   < BatchGetRepositories>, which returns information about one or more repositories associated with your AWS account
--     -   < CreateRepository>, which creates an AWS CodeCommit repository
--     -   < DeleteRepository>, which deletes an AWS CodeCommit repository
--     -   < GetRepository>, which returns information about a specified repository
--     -   < ListRepositories>, which lists all AWS CodeCommit repositories associated with your AWS account
--     -   < UpdateRepositoryDescription>, which sets or updates the description of the repository
--     -   < UpdateRepositoryName>, which changes the name of the repository. If you change the name of a repository, no other users of that repository will be able to access it until you send them the new HTTPS or SSH URL to use.
-- -   Branches, by calling the following:
--     -   < CreateBranch>, which creates a new branch in a specified repository
--     -   < GetBranch>, which returns information about a specified branch
--     -   < ListBranches>, which lists all branches for a specified repository
--     -   < UpdateDefaultBranch>, which changes the default branch for a repository
-- -   Information about committed code in a repository, by calling the following:
--     -   < GetCommit>, which returns information about a commit, including commit messages and committer information.
-- -   Triggers, by calling the following:
--     -   < GetRepositoryTriggers>, which returns information about triggers configured for a repository
--     -   < PutRepositoryTriggers>, which replaces all triggers for a repository and can be used to create or delete triggers
--     -   < TestRepositoryTriggers>, which tests the functionality of a repository trigger by sending data to the trigger target
--
-- For information about how to use AWS CodeCommit, see the <http://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit User Guide>.
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

    -- ** EncryptionKeyNotFoundException
    , _EncryptionKeyNotFoundException

    -- ** InvalidRepositoryTriggerBranchNameException
    , _InvalidRepositoryTriggerBranchNameException

    -- ** InvalidRepositoryTriggerCustomDataException
    , _InvalidRepositoryTriggerCustomDataException

    -- ** MaximumRepositoryNamesExceededException
    , _MaximumRepositoryNamesExceededException

    -- ** InvalidRepositoryDescriptionException
    , _InvalidRepositoryDescriptionException

    -- ** RepositoryNameExistsException
    , _RepositoryNameExistsException

    -- ** MaximumRepositoryTriggersExceededException
    , _MaximumRepositoryTriggersExceededException

    -- ** InvalidBranchNameException
    , _InvalidBranchNameException

    -- ** BranchNameRequiredException
    , _BranchNameRequiredException

    -- ** RepositoryTriggersListRequiredException
    , _RepositoryTriggersListRequiredException

    -- ** EncryptionKeyUnavailableException
    , _EncryptionKeyUnavailableException

    -- ** InvalidRepositoryTriggerDestinationARNException
    , _InvalidRepositoryTriggerDestinationARNException

    -- ** RepositoryNamesRequiredException
    , _RepositoryNamesRequiredException

    -- ** InvalidOrderException
    , _InvalidOrderException

    -- ** BranchDoesNotExistException
    , _BranchDoesNotExistException

    -- ** RepositoryTriggerNameRequiredException
    , _RepositoryTriggerNameRequiredException

    -- ** RepositoryDoesNotExistException
    , _RepositoryDoesNotExistException

    -- ** MaximumBranchesExceededException
    , _MaximumBranchesExceededException

    -- ** EncryptionIntegrityChecksFailedException
    , _EncryptionIntegrityChecksFailedException

    -- ** RepositoryTriggerEventsListRequiredException
    , _RepositoryTriggerEventsListRequiredException

    -- ** EncryptionKeyAccessDeniedException
    , _EncryptionKeyAccessDeniedException

    -- ** BranchNameExistsException
    , _BranchNameExistsException

    -- ** InvalidSortByException
    , _InvalidSortByException

    -- ** EncryptionKeyDisabledException
    , _EncryptionKeyDisabledException

    -- ** CommitIdRequiredException
    , _CommitIdRequiredException

    -- ** InvalidCommitIdException
    , _InvalidCommitIdException

    -- ** RepositoryTriggerDestinationARNRequiredException
    , _RepositoryTriggerDestinationARNRequiredException

    -- ** CommitDoesNotExistException
    , _CommitDoesNotExistException

    -- ** RepositoryTriggerBranchNameListRequiredException
    , _RepositoryTriggerBranchNameListRequiredException

    -- ** CommitIdDoesNotExistException
    , _CommitIdDoesNotExistException

    -- ** InvalidRepositoryTriggerNameException
    , _InvalidRepositoryTriggerNameException

    -- ** RepositoryNameRequiredException
    , _RepositoryNameRequiredException

    -- ** RepositoryLimitExceededException
    , _RepositoryLimitExceededException

    -- ** InvalidRepositoryTriggerEventsException
    , _InvalidRepositoryTriggerEventsException

    -- ** InvalidRepositoryNameException
    , _InvalidRepositoryNameException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** UpdateRepositoryName
    , module Network.AWS.CodeCommit.UpdateRepositoryName

    -- ** GetCommit
    , module Network.AWS.CodeCommit.GetCommit

    -- ** GetBranch
    , module Network.AWS.CodeCommit.GetBranch

    -- ** UpdateRepositoryDescription
    , module Network.AWS.CodeCommit.UpdateRepositoryDescription

    -- ** CreateBranch
    , module Network.AWS.CodeCommit.CreateBranch

    -- ** ListBranches (Paginated)
    , module Network.AWS.CodeCommit.ListBranches

    -- ** ListRepositories (Paginated)
    , module Network.AWS.CodeCommit.ListRepositories

    -- ** CreateRepository
    , module Network.AWS.CodeCommit.CreateRepository

    -- ** UpdateDefaultBranch
    , module Network.AWS.CodeCommit.UpdateDefaultBranch

    -- ** GetRepository
    , module Network.AWS.CodeCommit.GetRepository

    -- ** GetRepositoryTriggers
    , module Network.AWS.CodeCommit.GetRepositoryTriggers

    -- ** TestRepositoryTriggers
    , module Network.AWS.CodeCommit.TestRepositoryTriggers

    -- ** PutRepositoryTriggers
    , module Network.AWS.CodeCommit.PutRepositoryTriggers

    -- ** DeleteRepository
    , module Network.AWS.CodeCommit.DeleteRepository

    -- ** BatchGetRepositories
    , module Network.AWS.CodeCommit.BatchGetRepositories

    -- * Types

    -- ** OrderEnum
    , OrderEnum (..)

    -- ** RepositoryTriggerEventEnum
    , RepositoryTriggerEventEnum (..)

    -- ** SortByEnum
    , SortByEnum (..)

    -- ** BranchInfo
    , BranchInfo
    , branchInfo
    , biCommitId
    , biBranchName

    -- ** Commit
    , Commit
    , commit
    , cCommitter
    , cTreeId
    , cAdditionalData
    , cParents
    , cAuthor
    , cMessage

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
    , rtDestinationARN
    , rtName
    , rtEvents

    -- ** RepositoryTriggerExecutionFailure
    , RepositoryTriggerExecutionFailure
    , repositoryTriggerExecutionFailure
    , rtefFailureMessage
    , rtefTrigger

    -- ** UserInfo
    , UserInfo
    , userInfo
    , uiEmail
    , uiDate
    , uiName
    ) where

import           Network.AWS.CodeCommit.BatchGetRepositories
import           Network.AWS.CodeCommit.CreateBranch
import           Network.AWS.CodeCommit.CreateRepository
import           Network.AWS.CodeCommit.DeleteRepository
import           Network.AWS.CodeCommit.GetBranch
import           Network.AWS.CodeCommit.GetCommit
import           Network.AWS.CodeCommit.GetRepository
import           Network.AWS.CodeCommit.GetRepositoryTriggers
import           Network.AWS.CodeCommit.ListBranches
import           Network.AWS.CodeCommit.ListRepositories
import           Network.AWS.CodeCommit.PutRepositoryTriggers
import           Network.AWS.CodeCommit.TestRepositoryTriggers
import           Network.AWS.CodeCommit.Types
import           Network.AWS.CodeCommit.UpdateDefaultBranch
import           Network.AWS.CodeCommit.UpdateRepositoryDescription
import           Network.AWS.CodeCommit.UpdateRepositoryName
import           Network.AWS.CodeCommit.Waiters

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
