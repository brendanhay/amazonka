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

    -- ** AssociateTeamMember
    associateTeamMember_remoteAccessAllowed,
    associateTeamMember_clientRequestToken,
    associateTeamMember_projectId,
    associateTeamMember_userArn,
    associateTeamMember_projectRole,
    associateTeamMemberResponse_clientRequestToken,
    associateTeamMemberResponse_httpStatus,

    -- ** CreateProject
    createProject_tags,
    createProject_clientRequestToken,
    createProject_sourceCode,
    createProject_description,
    createProject_toolchain,
    createProject_name,
    createProject_id,
    createProjectResponse_clientRequestToken,
    createProjectResponse_projectTemplateId,
    createProjectResponse_httpStatus,
    createProjectResponse_id,
    createProjectResponse_arn,

    -- ** CreateUserProfile
    createUserProfile_sshPublicKey,
    createUserProfile_userArn,
    createUserProfile_displayName,
    createUserProfile_emailAddress,
    createUserProfileResponse_lastModifiedTimestamp,
    createUserProfileResponse_createdTimestamp,
    createUserProfileResponse_displayName,
    createUserProfileResponse_sshPublicKey,
    createUserProfileResponse_emailAddress,
    createUserProfileResponse_httpStatus,
    createUserProfileResponse_userArn,

    -- ** DeleteProject
    deleteProject_clientRequestToken,
    deleteProject_deleteStack,
    deleteProject_id,
    deleteProjectResponse_stackId,
    deleteProjectResponse_projectArn,
    deleteProjectResponse_httpStatus,

    -- ** DeleteUserProfile
    deleteUserProfile_userArn,
    deleteUserProfileResponse_httpStatus,
    deleteUserProfileResponse_userArn,

    -- ** DescribeProject
    describeProject_id,
    describeProjectResponse_stackId,
    describeProjectResponse_name,
    describeProjectResponse_clientRequestToken,
    describeProjectResponse_createdTimeStamp,
    describeProjectResponse_arn,
    describeProjectResponse_status,
    describeProjectResponse_description,
    describeProjectResponse_id,
    describeProjectResponse_projectTemplateId,
    describeProjectResponse_httpStatus,

    -- ** DescribeUserProfile
    describeUserProfile_userArn,
    describeUserProfileResponse_displayName,
    describeUserProfileResponse_sshPublicKey,
    describeUserProfileResponse_emailAddress,
    describeUserProfileResponse_httpStatus,
    describeUserProfileResponse_userArn,
    describeUserProfileResponse_createdTimestamp,
    describeUserProfileResponse_lastModifiedTimestamp,

    -- ** DisassociateTeamMember
    disassociateTeamMember_projectId,
    disassociateTeamMember_userArn,
    disassociateTeamMemberResponse_httpStatus,

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_maxResults,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,

    -- ** ListResources
    listResources_nextToken,
    listResources_maxResults,
    listResources_projectId,
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,

    -- ** ListTagsForProject
    listTagsForProject_nextToken,
    listTagsForProject_maxResults,
    listTagsForProject_id,
    listTagsForProjectResponse_tags,
    listTagsForProjectResponse_nextToken,
    listTagsForProjectResponse_httpStatus,

    -- ** ListTeamMembers
    listTeamMembers_nextToken,
    listTeamMembers_maxResults,
    listTeamMembers_projectId,
    listTeamMembersResponse_nextToken,
    listTeamMembersResponse_httpStatus,
    listTeamMembersResponse_teamMembers,

    -- ** ListUserProfiles
    listUserProfiles_nextToken,
    listUserProfiles_maxResults,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_httpStatus,
    listUserProfilesResponse_userProfiles,

    -- ** TagProject
    tagProject_id,
    tagProject_tags,
    tagProjectResponse_tags,
    tagProjectResponse_httpStatus,

    -- ** UntagProject
    untagProject_id,
    untagProject_tags,
    untagProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_name,
    updateProject_description,
    updateProject_id,
    updateProjectResponse_httpStatus,

    -- ** UpdateTeamMember
    updateTeamMember_remoteAccessAllowed,
    updateTeamMember_projectRole,
    updateTeamMember_projectId,
    updateTeamMember_userArn,
    updateTeamMemberResponse_remoteAccessAllowed,
    updateTeamMemberResponse_projectRole,
    updateTeamMemberResponse_userArn,
    updateTeamMemberResponse_httpStatus,

    -- ** UpdateUserProfile
    updateUserProfile_displayName,
    updateUserProfile_sshPublicKey,
    updateUserProfile_emailAddress,
    updateUserProfile_userArn,
    updateUserProfileResponse_lastModifiedTimestamp,
    updateUserProfileResponse_createdTimestamp,
    updateUserProfileResponse_displayName,
    updateUserProfileResponse_sshPublicKey,
    updateUserProfileResponse_emailAddress,
    updateUserProfileResponse_httpStatus,
    updateUserProfileResponse_userArn,

    -- * Types

    -- ** Code
    code_source,
    code_destination,

    -- ** CodeCommitCodeDestination
    codeCommitCodeDestination_name,

    -- ** CodeDestination
    codeDestination_gitHub,
    codeDestination_codeCommit,

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
    projectSummary_projectId,
    projectSummary_projectArn,

    -- ** Resource
    resource_id,

    -- ** S3Location
    s3Location_bucketName,
    s3Location_bucketKey,

    -- ** TeamMember
    teamMember_remoteAccessAllowed,
    teamMember_userArn,
    teamMember_projectRole,

    -- ** Toolchain
    toolchain_roleArn,
    toolchain_stackParameters,
    toolchain_source,

    -- ** ToolchainSource
    toolchainSource_s3,

    -- ** UserProfileSummary
    userProfileSummary_displayName,
    userProfileSummary_sshPublicKey,
    userProfileSummary_userArn,
    userProfileSummary_emailAddress,
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
