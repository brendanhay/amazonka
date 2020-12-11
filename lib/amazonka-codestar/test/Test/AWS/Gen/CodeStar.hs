{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CodeStar
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestListProjects $
--             mkListProjects
--
--         , requestListTeamMembers $
--             mkListTeamMembers
--
--         , requestDeleteProject $
--             mkDeleteProject
--
--         , requestUpdateProject $
--             mkUpdateProject
--
--         , requestDisassociateTeamMember $
--             mkDisassociateTeamMember
--
--         , requestTagProject $
--             mkTagProject
--
--         , requestDescribeProject $
--             mkDescribeProject
--
--         , requestListUserProfiles $
--             mkListUserProfiles
--
--         , requestListResources $
--             mkListResources
--
--         , requestAssociateTeamMember $
--             mkAssociateTeamMember
--
--         , requestUntagProject $
--             mkUntagProject
--
--         , requestUpdateTeamMember $
--             mkUpdateTeamMember
--
--         , requestDescribeUserProfile $
--             mkDescribeUserProfile
--
--         , requestListTagsForProject $
--             mkListTagsForProject
--
--         , requestDeleteUserProfile $
--             mkDeleteUserProfile
--
--         , requestUpdateUserProfile $
--             mkUpdateUserProfile
--
--         , requestCreateUserProfile $
--             mkCreateUserProfile
--
--         , requestCreateProject $
--             mkCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             mkListProjectsResponse
--
--         , responseListTeamMembers $
--             mkListTeamMembersResponse
--
--         , responseDeleteProject $
--             mkDeleteProjectResponse
--
--         , responseUpdateProject $
--             mkUpdateProjectResponse
--
--         , responseDisassociateTeamMember $
--             mkDisassociateTeamMemberResponse
--
--         , responseTagProject $
--             mkTagProjectResponse
--
--         , responseDescribeProject $
--             mkDescribeProjectResponse
--
--         , responseListUserProfiles $
--             mkListUserProfilesResponse
--
--         , responseListResources $
--             mkListResourcesResponse
--
--         , responseAssociateTeamMember $
--             mkAssociateTeamMemberResponse
--
--         , responseUntagProject $
--             mkUntagProjectResponse
--
--         , responseUpdateTeamMember $
--             mkUpdateTeamMemberResponse
--
--         , responseDescribeUserProfile $
--             mkDescribeUserProfileResponse
--
--         , responseListTagsForProject $
--             mkListTagsForProjectResponse
--
--         , responseDeleteUserProfile $
--             mkDeleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             mkUpdateUserProfileResponse
--
--         , responseCreateUserProfile $
--             mkCreateUserProfileResponse
--
--         , responseCreateProject $
--             mkCreateProjectResponse
--
--           ]
--     ]

-- Requests

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

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

requestDisassociateTeamMember :: DisassociateTeamMember -> TestTree
requestDisassociateTeamMember =
  req
    "DisassociateTeamMember"
    "fixture/DisassociateTeamMember.yaml"

requestTagProject :: TagProject -> TestTree
requestTagProject =
  req
    "TagProject"
    "fixture/TagProject.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestListUserProfiles :: ListUserProfiles -> TestTree
requestListUserProfiles =
  req
    "ListUserProfiles"
    "fixture/ListUserProfiles.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestAssociateTeamMember :: AssociateTeamMember -> TestTree
requestAssociateTeamMember =
  req
    "AssociateTeamMember"
    "fixture/AssociateTeamMember.yaml"

requestUntagProject :: UntagProject -> TestTree
requestUntagProject =
  req
    "UntagProject"
    "fixture/UntagProject.yaml"

requestUpdateTeamMember :: UpdateTeamMember -> TestTree
requestUpdateTeamMember =
  req
    "UpdateTeamMember"
    "fixture/UpdateTeamMember.yaml"

requestDescribeUserProfile :: DescribeUserProfile -> TestTree
requestDescribeUserProfile =
  req
    "DescribeUserProfile"
    "fixture/DescribeUserProfile.yaml"

requestListTagsForProject :: ListTagsForProject -> TestTree
requestListTagsForProject =
  req
    "ListTagsForProject"
    "fixture/ListTagsForProject.yaml"

requestDeleteUserProfile :: DeleteUserProfile -> TestTree
requestDeleteUserProfile =
  req
    "DeleteUserProfile"
    "fixture/DeleteUserProfile.yaml"

requestUpdateUserProfile :: UpdateUserProfile -> TestTree
requestUpdateUserProfile =
  req
    "UpdateUserProfile"
    "fixture/UpdateUserProfile.yaml"

requestCreateUserProfile :: CreateUserProfile -> TestTree
requestCreateUserProfile =
  req
    "CreateUserProfile"
    "fixture/CreateUserProfile.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

-- Responses

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    codeStarService
    (Proxy :: Proxy ListProjects)

responseListTeamMembers :: ListTeamMembersResponse -> TestTree
responseListTeamMembers =
  res
    "ListTeamMembersResponse"
    "fixture/ListTeamMembersResponse.proto"
    codeStarService
    (Proxy :: Proxy ListTeamMembers)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy UpdateProject)

responseDisassociateTeamMember :: DisassociateTeamMemberResponse -> TestTree
responseDisassociateTeamMember =
  res
    "DisassociateTeamMemberResponse"
    "fixture/DisassociateTeamMemberResponse.proto"
    codeStarService
    (Proxy :: Proxy DisassociateTeamMember)

responseTagProject :: TagProjectResponse -> TestTree
responseTagProject =
  res
    "TagProjectResponse"
    "fixture/TagProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy TagProject)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy DescribeProject)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    codeStarService
    (Proxy :: Proxy ListUserProfiles)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    codeStarService
    (Proxy :: Proxy ListResources)

responseAssociateTeamMember :: AssociateTeamMemberResponse -> TestTree
responseAssociateTeamMember =
  res
    "AssociateTeamMemberResponse"
    "fixture/AssociateTeamMemberResponse.proto"
    codeStarService
    (Proxy :: Proxy AssociateTeamMember)

responseUntagProject :: UntagProjectResponse -> TestTree
responseUntagProject =
  res
    "UntagProjectResponse"
    "fixture/UntagProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy UntagProject)

responseUpdateTeamMember :: UpdateTeamMemberResponse -> TestTree
responseUpdateTeamMember =
  res
    "UpdateTeamMemberResponse"
    "fixture/UpdateTeamMemberResponse.proto"
    codeStarService
    (Proxy :: Proxy UpdateTeamMember)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    codeStarService
    (Proxy :: Proxy DescribeUserProfile)

responseListTagsForProject :: ListTagsForProjectResponse -> TestTree
responseListTagsForProject =
  res
    "ListTagsForProjectResponse"
    "fixture/ListTagsForProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy ListTagsForProject)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    codeStarService
    (Proxy :: Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    codeStarService
    (Proxy :: Proxy UpdateUserProfile)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    codeStarService
    (Proxy :: Proxy CreateUserProfile)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    codeStarService
    (Proxy :: Proxy CreateProject)
