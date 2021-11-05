{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeStar.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeStar.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,

    -- ** ListTeamMembers
    listTeamMembers_nextToken,
    listTeamMembers_maxResults,
    listTeamMembers_projectId,
    listTeamMembersResponse_nextToken,
    listTeamMembersResponse_httpStatus,
    listTeamMembersResponse_teamMembers,

    -- ** DeleteProject
    deleteProject_deleteStack,
    deleteProject_clientRequestToken,
    deleteProject_id,
    deleteProjectResponse_projectArn,
    deleteProjectResponse_stackId,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_name,
    updateProject_description,
    updateProject_id,
    updateProjectResponse_httpStatus,

    -- ** DisassociateTeamMember
    disassociateTeamMember_projectId,
    disassociateTeamMember_userArn,
    disassociateTeamMemberResponse_httpStatus,

    -- ** TagProject
    tagProject_id,
    tagProject_tags,
    tagProjectResponse_tags,
    tagProjectResponse_httpStatus,

    -- ** DescribeProject
    describeProject_id,
    describeProjectResponse_status,
    describeProjectResponse_arn,
    describeProjectResponse_projectTemplateId,
    describeProjectResponse_name,
    describeProjectResponse_id,
    describeProjectResponse_stackId,
    describeProjectResponse_clientRequestToken,
    describeProjectResponse_createdTimeStamp,
    describeProjectResponse_description,
    describeProjectResponse_httpStatus,

    -- ** ListUserProfiles
    listUserProfiles_nextToken,
    listUserProfiles_maxResults,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_httpStatus,
    listUserProfilesResponse_userProfiles,

    -- ** ListResources
    listResources_nextToken,
    listResources_maxResults,
    listResources_projectId,
    listResourcesResponse_resources,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,

    -- ** AssociateTeamMember
    associateTeamMember_remoteAccessAllowed,
    associateTeamMember_clientRequestToken,
    associateTeamMember_projectId,
    associateTeamMember_userArn,
    associateTeamMember_projectRole,
    associateTeamMemberResponse_clientRequestToken,
    associateTeamMemberResponse_httpStatus,

    -- ** UntagProject
    untagProject_id,
    untagProject_tags,
    untagProjectResponse_httpStatus,

    -- ** UpdateTeamMember
    updateTeamMember_remoteAccessAllowed,
    updateTeamMember_projectRole,
    updateTeamMember_projectId,
    updateTeamMember_userArn,
    updateTeamMemberResponse_userArn,
    updateTeamMemberResponse_remoteAccessAllowed,
    updateTeamMemberResponse_projectRole,
    updateTeamMemberResponse_httpStatus,

    -- ** DescribeUserProfile
    describeUserProfile_userArn,
    describeUserProfileResponse_sshPublicKey,
    describeUserProfileResponse_emailAddress,
    describeUserProfileResponse_displayName,
    describeUserProfileResponse_httpStatus,
    describeUserProfileResponse_userArn,
    describeUserProfileResponse_createdTimestamp,
    describeUserProfileResponse_lastModifiedTimestamp,

    -- ** ListTagsForProject
    listTagsForProject_nextToken,
    listTagsForProject_maxResults,
    listTagsForProject_id,
    listTagsForProjectResponse_nextToken,
    listTagsForProjectResponse_tags,
    listTagsForProjectResponse_httpStatus,

    -- ** DeleteUserProfile
    deleteUserProfile_userArn,
    deleteUserProfileResponse_httpStatus,
    deleteUserProfileResponse_userArn,

    -- ** UpdateUserProfile
    updateUserProfile_sshPublicKey,
    updateUserProfile_emailAddress,
    updateUserProfile_displayName,
    updateUserProfile_userArn,
    updateUserProfileResponse_lastModifiedTimestamp,
    updateUserProfileResponse_sshPublicKey,
    updateUserProfileResponse_emailAddress,
    updateUserProfileResponse_displayName,
    updateUserProfileResponse_createdTimestamp,
    updateUserProfileResponse_httpStatus,
    updateUserProfileResponse_userArn,

    -- ** CreateUserProfile
    createUserProfile_sshPublicKey,
    createUserProfile_userArn,
    createUserProfile_displayName,
    createUserProfile_emailAddress,
    createUserProfileResponse_lastModifiedTimestamp,
    createUserProfileResponse_sshPublicKey,
    createUserProfileResponse_emailAddress,
    createUserProfileResponse_displayName,
    createUserProfileResponse_createdTimestamp,
    createUserProfileResponse_httpStatus,
    createUserProfileResponse_userArn,

    -- ** CreateProject
    createProject_sourceCode,
    createProject_toolchain,
    createProject_clientRequestToken,
    createProject_description,
    createProject_tags,
    createProject_name,
    createProject_id,
    createProjectResponse_projectTemplateId,
    createProjectResponse_clientRequestToken,
    createProjectResponse_httpStatus,
    createProjectResponse_id,
    createProjectResponse_arn,

    -- * Types

    -- ** Code
    code_source,
    code_destination,

    -- ** CodeCommitCodeDestination
    codeCommitCodeDestination_name,

    -- ** CodeDestination
    codeDestination_codeCommit,
    codeDestination_gitHub,

    -- ** CodeSource
    codeSource_s3,

    -- ** GitHubCodeDestination
    gitHubCodeDestination_description,
    gitHubCodeDestination_name,
    gitHubCodeDestination_type,
    gitHubCodeDestination_owner,
    gitHubCodeDestination_privateRepository,
    gitHubCodeDestination_issuesEnabled,
    gitHubCodeDestination_token,

    -- ** ProjectStatus
    projectStatus_reason,
    projectStatus_state,

    -- ** ProjectSummary
    projectSummary_projectArn,
    projectSummary_projectId,

    -- ** Resource
    resource_id,

    -- ** S3Location
    s3Location_bucketKey,
    s3Location_bucketName,

    -- ** TeamMember
    teamMember_remoteAccessAllowed,
    teamMember_userArn,
    teamMember_projectRole,

    -- ** Toolchain
    toolchain_stackParameters,
    toolchain_roleArn,
    toolchain_source,

    -- ** ToolchainSource
    toolchainSource_s3,

    -- ** UserProfileSummary
    userProfileSummary_sshPublicKey,
    userProfileSummary_userArn,
    userProfileSummary_emailAddress,
    userProfileSummary_displayName,
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
import Amazonka.CodeStar.ListProjects
import Amazonka.CodeStar.ListResources
import Amazonka.CodeStar.ListTagsForProject
import Amazonka.CodeStar.ListTeamMembers
import Amazonka.CodeStar.ListUserProfiles
import Amazonka.CodeStar.TagProject
import Amazonka.CodeStar.Types.Code
import Amazonka.CodeStar.Types.CodeCommitCodeDestination
import Amazonka.CodeStar.Types.CodeDestination
import Amazonka.CodeStar.Types.CodeSource
import Amazonka.CodeStar.Types.GitHubCodeDestination
import Amazonka.CodeStar.Types.ProjectStatus
import Amazonka.CodeStar.Types.ProjectSummary
import Amazonka.CodeStar.Types.Resource
import Amazonka.CodeStar.Types.S3Location
import Amazonka.CodeStar.Types.TeamMember
import Amazonka.CodeStar.Types.Toolchain
import Amazonka.CodeStar.Types.ToolchainSource
import Amazonka.CodeStar.Types.UserProfileSummary
import Amazonka.CodeStar.UntagProject
import Amazonka.CodeStar.UpdateProject
import Amazonka.CodeStar.UpdateTeamMember
import Amazonka.CodeStar.UpdateUserProfile
