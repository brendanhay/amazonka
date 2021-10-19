{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Lens
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

import Network.AWS.CodeStar.AssociateTeamMember
import Network.AWS.CodeStar.CreateProject
import Network.AWS.CodeStar.CreateUserProfile
import Network.AWS.CodeStar.DeleteProject
import Network.AWS.CodeStar.DeleteUserProfile
import Network.AWS.CodeStar.DescribeProject
import Network.AWS.CodeStar.DescribeUserProfile
import Network.AWS.CodeStar.DisassociateTeamMember
import Network.AWS.CodeStar.ListProjects
import Network.AWS.CodeStar.ListResources
import Network.AWS.CodeStar.ListTagsForProject
import Network.AWS.CodeStar.ListTeamMembers
import Network.AWS.CodeStar.ListUserProfiles
import Network.AWS.CodeStar.TagProject
import Network.AWS.CodeStar.Types.Code
import Network.AWS.CodeStar.Types.CodeCommitCodeDestination
import Network.AWS.CodeStar.Types.CodeDestination
import Network.AWS.CodeStar.Types.CodeSource
import Network.AWS.CodeStar.Types.GitHubCodeDestination
import Network.AWS.CodeStar.Types.ProjectStatus
import Network.AWS.CodeStar.Types.ProjectSummary
import Network.AWS.CodeStar.Types.Resource
import Network.AWS.CodeStar.Types.S3Location
import Network.AWS.CodeStar.Types.TeamMember
import Network.AWS.CodeStar.Types.Toolchain
import Network.AWS.CodeStar.Types.ToolchainSource
import Network.AWS.CodeStar.Types.UserProfileSummary
import Network.AWS.CodeStar.UntagProject
import Network.AWS.CodeStar.UpdateProject
import Network.AWS.CodeStar.UpdateTeamMember
import Network.AWS.CodeStar.UpdateUserProfile
