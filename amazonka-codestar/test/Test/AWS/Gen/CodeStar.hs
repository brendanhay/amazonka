{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeStar
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CodeStar where

import Data.Proxy
import Network.AWS.CodeStar
import Test.AWS.CodeStar.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDisassociateTeamMember $
--             newDisassociateTeamMember
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestTagProject $
--             newTagProject
--
--         , requestListProjects $
--             newListProjects
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestListTagsForProject $
--             newListTagsForProject
--
--         , requestUpdateTeamMember $
--             newUpdateTeamMember
--
--         , requestUntagProject $
--             newUntagProject
--
--         , requestListTeamMembers $
--             newListTeamMembers
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestAssociateTeamMember $
--             newAssociateTeamMember
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestListResources $
--             newListResources
--
--         , requestDescribeProject $
--             newDescribeProject
--
--           ]

--     , testGroup "response"
--         [ responseDisassociateTeamMember $
--             newDisassociateTeamMemberResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseTagProject $
--             newTagProjectResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseListTagsForProject $
--             newListTagsForProjectResponse
--
--         , responseUpdateTeamMember $
--             newUpdateTeamMemberResponse
--
--         , responseUntagProject $
--             newUntagProjectResponse
--
--         , responseListTeamMembers $
--             newListTeamMembersResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseAssociateTeamMember $
--             newAssociateTeamMemberResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--           ]
--     ]

-- Requests

requestDisassociateTeamMember :: DisassociateTeamMember -> TestTree
requestDisassociateTeamMember =
  req
    "DisassociateTeamMember"
    "fixture/DisassociateTeamMember.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestTagProject :: TagProject -> TestTree
requestTagProject =
  req
    "TagProject"
    "fixture/TagProject.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestListTagsForProject :: ListTagsForProject -> TestTree
requestListTagsForProject =
  req
    "ListTagsForProject"
    "fixture/ListTagsForProject.yaml"

requestUpdateTeamMember :: UpdateTeamMember -> TestTree
requestUpdateTeamMember =
  req
    "UpdateTeamMember"
    "fixture/UpdateTeamMember.yaml"

requestUntagProject :: UntagProject -> TestTree
requestUntagProject =
  req
    "UntagProject"
    "fixture/UntagProject.yaml"

requestListTeamMembers :: ListTeamMembers -> TestTree
requestListTeamMembers =
  req
    "ListTeamMembers"
    "fixture/ListTeamMembers.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestAssociateTeamMember :: AssociateTeamMember -> TestTree
requestAssociateTeamMember =
  req
    "AssociateTeamMember"
    "fixture/AssociateTeamMember.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

-- Responses

responseDisassociateTeamMember :: DisassociateTeamMemberResponse -> TestTree
responseDisassociateTeamMember =
  res
    "DisassociateTeamMemberResponse"
    "fixture/DisassociateTeamMemberResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateTeamMember)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserProfile)

responseTagProject :: TagProjectResponse -> TestTree
responseTagProject =
  res
    "TagProjectResponse"
    "fixture/TagProjectResponse.proto"
    defaultService
    (Proxy :: Proxy TagProject)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy :: Proxy ListProjects)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy :: Proxy ListUserProfiles)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateUserProfile)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUserProfile)

responseListTagsForProject :: ListTagsForProjectResponse -> TestTree
responseListTagsForProject =
  res
    "ListTagsForProjectResponse"
    "fixture/ListTagsForProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForProject)

responseUpdateTeamMember :: UpdateTeamMemberResponse -> TestTree
responseUpdateTeamMember =
  res
    "UpdateTeamMemberResponse"
    "fixture/UpdateTeamMemberResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTeamMember)

responseUntagProject :: UntagProjectResponse -> TestTree
responseUntagProject =
  res
    "UntagProjectResponse"
    "fixture/UntagProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UntagProject)

responseListTeamMembers :: ListTeamMembersResponse -> TestTree
responseListTeamMembers =
  res
    "ListTeamMembersResponse"
    "fixture/ListTeamMembersResponse.proto"
    defaultService
    (Proxy :: Proxy ListTeamMembers)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseAssociateTeamMember :: AssociateTeamMemberResponse -> TestTree
responseAssociateTeamMember =
  res
    "AssociateTeamMemberResponse"
    "fixture/AssociateTeamMemberResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateTeamMember)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUserProfile)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResources)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)
