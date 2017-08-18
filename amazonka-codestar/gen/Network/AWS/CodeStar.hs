{-# OPTIONS_GHC -fno-warn-unused-imports    #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __AWS CodeStar__
--
-- This is the API reference for AWS CodeStar. This reference provides descriptions of the operations and data types for the AWS CodeStar API along with usage examples.
--
-- You can use the AWS CodeStar API to work with:
--
-- Projects and their resources, by calling the following:
--
--     * 'DeleteProject' , which deletes a project in AWS CodeStar.
--
--     * 'DescribeProject' , which lists the attributes of a project.
--
--     * 'ListProjects' , which lists all AWS CodeStar projects associated with your AWS account.
--
--     * 'ListResources' , which lists the resources associated with an AWS CodeStar project.
--
--     * 'UpdateProject' , which updates the attributes of an AWS CodeStar project.
--
--
--
-- Teams and team members, by calling the following:
--
--     * 'AssociateTeamMember' , which adds an IAM user to the team for an AWS CodeStar project.
--
--     * 'DisassociateTeamMember' , which removes an IAM user from the team for an AWS CodeStar project.
--
--     * 'ListTeamMembers' , which lists all the IAM users in the team for an AWS CodeStar project, including their roles and attributes.
--
--
--
-- Users, by calling the following:
--
--     * 'CreateUserProfile' , which creates a user profile that contains data associated with the user across all AWS CodeStar projects.
--
--     * 'DeleteUserProfile' , which deletes all user profile information across all AWS CodeStar projects.
--
--     * 'DescribeUserProfile' , which describes the profile of a user.
--
--     * 'ListUserProfiles' , which lists all AWS CodeStar user profiles.
--
--     * 'UpdateUserProfile' , which updates the profile for an AWS CodeStar user.
--
--
--
module Network.AWS.CodeStar
    (
    -- * Service Configuration
      codeStar

    -- * Errors
    -- $errors

    -- ** TeamMemberAlreadyAssociatedException
    , _TeamMemberAlreadyAssociatedException

    -- ** ValidationException
    , _ValidationException

    -- ** InvalidServiceRoleException
    , _InvalidServiceRoleException

    -- ** ProjectCreationFailedException
    , _ProjectCreationFailedException

    -- ** UserProfileAlreadyExistsException
    , _UserProfileAlreadyExistsException

    -- ** ProjectNotFoundException
    , _ProjectNotFoundException

    -- ** TeamMemberNotFoundException
    , _TeamMemberNotFoundException

    -- ** ProjectAlreadyExistsException
    , _ProjectAlreadyExistsException

    -- ** ProjectConfigurationException
    , _ProjectConfigurationException

    -- ** ConcurrentModificationException
    , _ConcurrentModificationException

    -- ** InvalidNextTokenException
    , _InvalidNextTokenException

    -- ** UserProfileNotFoundException
    , _UserProfileNotFoundException

    -- ** LimitExceededException
    , _LimitExceededException

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** ListProjects
    , module Network.AWS.CodeStar.ListProjects

    -- ** ListTeamMembers
    , module Network.AWS.CodeStar.ListTeamMembers

    -- ** DeleteProject
    , module Network.AWS.CodeStar.DeleteProject

    -- ** UpdateProject
    , module Network.AWS.CodeStar.UpdateProject

    -- ** DisassociateTeamMember
    , module Network.AWS.CodeStar.DisassociateTeamMember

    -- ** DescribeProject
    , module Network.AWS.CodeStar.DescribeProject

    -- ** ListUserProfiles
    , module Network.AWS.CodeStar.ListUserProfiles

    -- ** ListResources
    , module Network.AWS.CodeStar.ListResources

    -- ** AssociateTeamMember
    , module Network.AWS.CodeStar.AssociateTeamMember

    -- ** UpdateTeamMember
    , module Network.AWS.CodeStar.UpdateTeamMember

    -- ** DescribeUserProfile
    , module Network.AWS.CodeStar.DescribeUserProfile

    -- ** DeleteUserProfile
    , module Network.AWS.CodeStar.DeleteUserProfile

    -- ** UpdateUserProfile
    , module Network.AWS.CodeStar.UpdateUserProfile

    -- ** CreateUserProfile
    , module Network.AWS.CodeStar.CreateUserProfile

    -- ** CreateProject
    , module Network.AWS.CodeStar.CreateProject

    -- * Types

    -- ** ProjectSummary
    , ProjectSummary
    , projectSummary
    , psProjectARN
    , psProjectId

    -- ** Resource
    , Resource
    , resource
    , rId

    -- ** TeamMember
    , TeamMember
    , teamMember
    , tmRemoteAccessAllowed
    , tmUserARN
    , tmProjectRole

    -- ** UserProfileSummary
    , UserProfileSummary
    , userProfileSummary
    , upsSshPublicKey
    , upsUserARN
    , upsEmailAddress
    , upsDisplayName
    ) where

import           Network.AWS.CodeStar.AssociateTeamMember
import           Network.AWS.CodeStar.CreateProject
import           Network.AWS.CodeStar.CreateUserProfile
import           Network.AWS.CodeStar.DeleteProject
import           Network.AWS.CodeStar.DeleteUserProfile
import           Network.AWS.CodeStar.DescribeProject
import           Network.AWS.CodeStar.DescribeUserProfile
import           Network.AWS.CodeStar.DisassociateTeamMember
import           Network.AWS.CodeStar.ListProjects
import           Network.AWS.CodeStar.ListResources
import           Network.AWS.CodeStar.ListTeamMembers
import           Network.AWS.CodeStar.ListUserProfiles
import           Network.AWS.CodeStar.Types
import           Network.AWS.CodeStar.UpdateProject
import           Network.AWS.CodeStar.UpdateTeamMember
import           Network.AWS.CodeStar.UpdateUserProfile
import           Network.AWS.CodeStar.Waiters

{- $errors
Error matchers are designed for use with the functions provided by
<http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
This allows catching (and rethrowing) service specific errors returned
by 'CodeStar'.
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
