{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeStar
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CodeStar where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CodeStar
import Test.AWS.CodeStar.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             listProjects
--
--         , requestListTeamMembers $
--             listTeamMembers
--
--         , requestDeleteProject $
--             deleteProject
--
--         , requestUpdateProject $
--             updateProject
--
--         , requestDisassociateTeamMember $
--             disassociateTeamMember
--
--         , requestDescribeProject $
--             describeProject
--
--         , requestListUserProfiles $
--             listUserProfiles
--
--         , requestListResources $
--             listResources
--
--         , requestAssociateTeamMember $
--             associateTeamMember
--
--         , requestUpdateTeamMember $
--             updateTeamMember
--
--         , requestDescribeUserProfile $
--             describeUserProfile
--
--         , requestDeleteUserProfile $
--             deleteUserProfile
--
--         , requestUpdateUserProfile $
--             updateUserProfile
--
--         , requestCreateUserProfile $
--             createUserProfile
--
--         , requestCreateProject $
--             createProject
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             listProjectsResponse
--
--         , responseListTeamMembers $
--             listTeamMembersResponse
--
--         , responseDeleteProject $
--             deleteProjectResponse
--
--         , responseUpdateProject $
--             updateProjectResponse
--
--         , responseDisassociateTeamMember $
--             disassociateTeamMemberResponse
--
--         , responseDescribeProject $
--             describeProjectResponse
--
--         , responseListUserProfiles $
--             listUserProfilesResponse
--
--         , responseListResources $
--             listResourcesResponse
--
--         , responseAssociateTeamMember $
--             associateTeamMemberResponse
--
--         , responseUpdateTeamMember $
--             updateTeamMemberResponse
--
--         , responseDescribeUserProfile $
--             describeUserProfileResponse
--
--         , responseDeleteUserProfile $
--             deleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             updateUserProfileResponse
--
--         , responseCreateUserProfile $
--             createUserProfileResponse
--
--         , responseCreateProject $
--             createProjectResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects = req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListTeamMembers :: ListTeamMembers -> TestTree
requestListTeamMembers = req
    "ListTeamMembers"
    "fixture/ListTeamMembers.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject = req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject = req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestDisassociateTeamMember :: DisassociateTeamMember -> TestTree
requestDisassociateTeamMember = req
    "DisassociateTeamMember"
    "fixture/DisassociateTeamMember.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject = req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles = req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestListResources :: ListResources -> TestTree
requestListResources = req
    "ListResources"
    "fixture/ListResources.yaml"

requestAssociateTeamMember :: AssociateTeamMember -> TestTree
requestAssociateTeamMember = req
    "AssociateTeamMember"
    "fixture/AssociateTeamMember.yaml"

requestUpdateTeamMember :: UpdateTeamMember -> TestTree
requestUpdateTeamMember = req
    "UpdateTeamMember"
    "fixture/UpdateTeamMember.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile = req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile = req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile = req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile = req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject = req
    "CreateProject"
    "fixture/CreateProject.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects = res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    codeStar
    (Proxy :: Proxy ListProjects)

responseListTeamMembers :: ListTeamMembersResponse -> TestTree
responseListTeamMembers = res
    "ListTeamMembersResponse"
    "fixture/ListTeamMembersResponse.proto"
    codeStar
    (Proxy :: Proxy ListTeamMembers)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject = res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    codeStar
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject = res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    codeStar
    (Proxy :: Proxy UpdateProject)

responseDisassociateTeamMember :: DisassociateTeamMemberResponse -> TestTree
responseDisassociateTeamMember = res
    "DisassociateTeamMemberResponse"
    "fixture/DisassociateTeamMemberResponse.proto"
    codeStar
    (Proxy :: Proxy DisassociateTeamMember)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject = res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    codeStar
    (Proxy :: Proxy DescribeProject)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles = res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    codeStar
    (Proxy :: Proxy ListUserProfiles)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources = res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    codeStar
    (Proxy :: Proxy ListResources)

responseAssociateTeamMember :: AssociateTeamMemberResponse -> TestTree
responseAssociateTeamMember = res
    "AssociateTeamMemberResponse"
    "fixture/AssociateTeamMemberResponse.proto"
    codeStar
    (Proxy :: Proxy AssociateTeamMember)

responseUpdateTeamMember :: UpdateTeamMemberResponse -> TestTree
responseUpdateTeamMember = res
    "UpdateTeamMemberResponse"
    "fixture/UpdateTeamMemberResponse.proto"
    codeStar
    (Proxy :: Proxy UpdateTeamMember)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile = res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    codeStar
    (Proxy :: Proxy DescribeUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile = res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    codeStar
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile = res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    codeStar
    (Proxy :: Proxy UpdateUserProfile)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile = res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    codeStar
    (Proxy :: Proxy CreateUserProfile)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject = res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    codeStar
    (Proxy :: Proxy CreateProject)
