{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.CodeStar
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2017-04-19@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS CodeStar
--
-- This is the API reference for AWS CodeStar. This reference provides
-- descriptions of the operations and data types for the AWS CodeStar API
-- along with usage examples.
--
-- You can use the AWS CodeStar API to work with:
--
-- Projects and their resources, by calling the following:
--
-- -   @DeleteProject@, which deletes a project.
--
-- -   @DescribeProject@, which lists the attributes of a project.
--
-- -   @ListProjects@, which lists all projects associated with your AWS
--     account.
--
-- -   @ListResources@, which lists the resources associated with a
--     project.
--
-- -   @ListTagsForProject@, which lists the tags associated with a
--     project.
--
-- -   @TagProject@, which adds tags to a project.
--
-- -   @UntagProject@, which removes tags from a project.
--
-- -   @UpdateProject@, which updates the attributes of a project.
--
-- Teams and team members, by calling the following:
--
-- -   @AssociateTeamMember@, which adds an IAM user to the team for a
--     project.
--
-- -   @DisassociateTeamMember@, which removes an IAM user from the team
--     for a project.
--
-- -   @ListTeamMembers@, which lists all the IAM users in the team for a
--     project, including their roles and attributes.
--
-- -   @UpdateTeamMember@, which updates a team member\'s attributes in a
--     project.
--
-- Users, by calling the following:
--
-- -   @CreateUserProfile@, which creates a user profile that contains data
--     associated with the user across all projects.
--
-- -   @DeleteUserProfile@, which deletes all user profile information
--     across all projects.
--
-- -   @DescribeUserProfile@, which describes the profile of a user.
--
-- -   @ListUserProfiles@, which lists all user profiles.
--
-- -   @UpdateUserProfile@, which updates the profile for a user.
module Amazonka.CodeStar
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ConcurrentModificationException
    _ConcurrentModificationException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidServiceRoleException
    _InvalidServiceRoleException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** ProjectAlreadyExistsException
    _ProjectAlreadyExistsException,

    -- ** ProjectConfigurationException
    _ProjectConfigurationException,

    -- ** ProjectCreationFailedException
    _ProjectCreationFailedException,

    -- ** ProjectNotFoundException
    _ProjectNotFoundException,

    -- ** TeamMemberAlreadyAssociatedException
    _TeamMemberAlreadyAssociatedException,

    -- ** TeamMemberNotFoundException
    _TeamMemberNotFoundException,

    -- ** UserProfileAlreadyExistsException
    _UserProfileAlreadyExistsException,

    -- ** UserProfileNotFoundException
    _UserProfileNotFoundException,

    -- ** ValidationException
    _ValidationException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateTeamMember
    AssociateTeamMember (AssociateTeamMember'),
    newAssociateTeamMember,
    AssociateTeamMemberResponse (AssociateTeamMemberResponse'),
    newAssociateTeamMemberResponse,

    -- ** CreateProject
    CreateProject (CreateProject'),
    newCreateProject,
    CreateProjectResponse (CreateProjectResponse'),
    newCreateProjectResponse,

    -- ** CreateUserProfile
    CreateUserProfile (CreateUserProfile'),
    newCreateUserProfile,
    CreateUserProfileResponse (CreateUserProfileResponse'),
    newCreateUserProfileResponse,

    -- ** DeleteProject
    DeleteProject (DeleteProject'),
    newDeleteProject,
    DeleteProjectResponse (DeleteProjectResponse'),
    newDeleteProjectResponse,

    -- ** DeleteUserProfile
    DeleteUserProfile (DeleteUserProfile'),
    newDeleteUserProfile,
    DeleteUserProfileResponse (DeleteUserProfileResponse'),
    newDeleteUserProfileResponse,

    -- ** DescribeProject
    DescribeProject (DescribeProject'),
    newDescribeProject,
    DescribeProjectResponse (DescribeProjectResponse'),
    newDescribeProjectResponse,

    -- ** DescribeUserProfile
    DescribeUserProfile (DescribeUserProfile'),
    newDescribeUserProfile,
    DescribeUserProfileResponse (DescribeUserProfileResponse'),
    newDescribeUserProfileResponse,

    -- ** DisassociateTeamMember
    DisassociateTeamMember (DisassociateTeamMember'),
    newDisassociateTeamMember,
    DisassociateTeamMemberResponse (DisassociateTeamMemberResponse'),
    newDisassociateTeamMemberResponse,

    -- ** ListProjects (Paginated)
    ListProjects (ListProjects'),
    newListProjects,
    ListProjectsResponse (ListProjectsResponse'),
    newListProjectsResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** ListTagsForProject
    ListTagsForProject (ListTagsForProject'),
    newListTagsForProject,
    ListTagsForProjectResponse (ListTagsForProjectResponse'),
    newListTagsForProjectResponse,

    -- ** ListTeamMembers (Paginated)
    ListTeamMembers (ListTeamMembers'),
    newListTeamMembers,
    ListTeamMembersResponse (ListTeamMembersResponse'),
    newListTeamMembersResponse,

    -- ** ListUserProfiles (Paginated)
    ListUserProfiles (ListUserProfiles'),
    newListUserProfiles,
    ListUserProfilesResponse (ListUserProfilesResponse'),
    newListUserProfilesResponse,

    -- ** TagProject
    TagProject (TagProject'),
    newTagProject,
    TagProjectResponse (TagProjectResponse'),
    newTagProjectResponse,

    -- ** UntagProject
    UntagProject (UntagProject'),
    newUntagProject,
    UntagProjectResponse (UntagProjectResponse'),
    newUntagProjectResponse,

    -- ** UpdateProject
    UpdateProject (UpdateProject'),
    newUpdateProject,
    UpdateProjectResponse (UpdateProjectResponse'),
    newUpdateProjectResponse,

    -- ** UpdateTeamMember
    UpdateTeamMember (UpdateTeamMember'),
    newUpdateTeamMember,
    UpdateTeamMemberResponse (UpdateTeamMemberResponse'),
    newUpdateTeamMemberResponse,

    -- ** UpdateUserProfile
    UpdateUserProfile (UpdateUserProfile'),
    newUpdateUserProfile,
    UpdateUserProfileResponse (UpdateUserProfileResponse'),
    newUpdateUserProfileResponse,

    -- * Types

    -- ** Code
    Code (Code'),
    newCode,

    -- ** CodeCommitCodeDestination
    CodeCommitCodeDestination (CodeCommitCodeDestination'),
    newCodeCommitCodeDestination,

    -- ** CodeDestination
    CodeDestination (CodeDestination'),
    newCodeDestination,

    -- ** CodeSource
    CodeSource (CodeSource'),
    newCodeSource,

    -- ** GitHubCodeDestination
    GitHubCodeDestination (GitHubCodeDestination'),
    newGitHubCodeDestination,

    -- ** ProjectStatus
    ProjectStatus (ProjectStatus'),
    newProjectStatus,

    -- ** ProjectSummary
    ProjectSummary (ProjectSummary'),
    newProjectSummary,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** S3Location
    S3Location (S3Location'),
    newS3Location,

    -- ** TeamMember
    TeamMember (TeamMember'),
    newTeamMember,

    -- ** Toolchain
    Toolchain (Toolchain'),
    newToolchain,

    -- ** ToolchainSource
    ToolchainSource (ToolchainSource'),
    newToolchainSource,

    -- ** UserProfileSummary
    UserProfileSummary (UserProfileSummary'),
    newUserProfileSummary,
  )
where

import Amazonka.CodeStar.AssociateTeamMember
import Amazonka.CodeStar.CreateProject
import Amazonka.CodeStar.CreateUserProfile
import Amazonka.CodeStar.DeleteProject
import Amazonka.CodeStar.DeleteUserProfile
import Amazonka.CodeStar.DescribeProject
import Amazonka.CodeStar.DescribeUserProfile
import Amazonka.CodeStar.DisassociateTeamMember
import Amazonka.CodeStar.Lens
import Amazonka.CodeStar.ListProjects
import Amazonka.CodeStar.ListResources
import Amazonka.CodeStar.ListTagsForProject
import Amazonka.CodeStar.ListTeamMembers
import Amazonka.CodeStar.ListUserProfiles
import Amazonka.CodeStar.TagProject
import Amazonka.CodeStar.Types
import Amazonka.CodeStar.UntagProject
import Amazonka.CodeStar.UpdateProject
import Amazonka.CodeStar.UpdateTeamMember
import Amazonka.CodeStar.UpdateUserProfile
import Amazonka.CodeStar.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'CodeStar'.

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
