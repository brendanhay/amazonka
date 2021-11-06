{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CodeStar
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CodeStar where

import Amazonka.CodeStar
import qualified Data.Proxy as Proxy
import Test.Amazonka.CodeStar.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListProjects $
--             newListProjects
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
--         , requestDisassociateTeamMember $
--             newDisassociateTeamMember
--
--         , requestTagProject $
--             newTagProject
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestListUserProfiles $
--             newListUserProfiles
--
--         , requestListResources $
--             newListResources
--
--         , requestAssociateTeamMember $
--             newAssociateTeamMember
--
--         , requestUntagProject $
--             newUntagProject
--
--         , requestUpdateTeamMember $
--             newUpdateTeamMember
--
--         , requestDescribeUserProfile $
--             newDescribeUserProfile
--
--         , requestListTagsForProject $
--             newListTagsForProject
--
--         , requestDeleteUserProfile $
--             newDeleteUserProfile
--
--         , requestUpdateUserProfile $
--             newUpdateUserProfile
--
--         , requestCreateUserProfile $
--             newCreateUserProfile
--
--         , requestCreateProject $
--             newCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
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
--         , responseDisassociateTeamMember $
--             newDisassociateTeamMemberResponse
--
--         , responseTagProject $
--             newTagProjectResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseListUserProfiles $
--             newListUserProfilesResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseAssociateTeamMember $
--             newAssociateTeamMemberResponse
--
--         , responseUntagProject $
--             newUntagProjectResponse
--
--         , responseUpdateTeamMember $
--             newUpdateTeamMemberResponse
--
--         , responseDescribeUserProfile $
--             newDescribeUserProfileResponse
--
--         , responseListTagsForProject $
--             newListTagsForProjectResponse
--
--         , responseDeleteUserProfile $
--             newDeleteUserProfileResponse
--
--         , responseUpdateUserProfile $
--             newUpdateUserProfileResponse
--
--         , responseCreateUserProfile $
--             newCreateUserProfileResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListTeamMembers :: ListTeamMembersResponse -> TestTree
responseListTeamMembers =
  res
    "ListTeamMembersResponse"
    "fixture/ListTeamMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTeamMembers)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)

responseDisassociateTeamMember :: DisassociateTeamMemberResponse -> TestTree
responseDisassociateTeamMember =
  res
    "DisassociateTeamMemberResponse"
    "fixture/DisassociateTeamMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateTeamMember)

responseTagProject :: TagProjectResponse -> TestTree
responseTagProject =
  res
    "TagProjectResponse"
    "fixture/TagProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagProject)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseListUserProfiles :: ListUserProfilesResponse -> TestTree
responseListUserProfiles =
  res
    "ListUserProfilesResponse"
    "fixture/ListUserProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUserProfiles)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseAssociateTeamMember :: AssociateTeamMemberResponse -> TestTree
responseAssociateTeamMember =
  res
    "AssociateTeamMemberResponse"
    "fixture/AssociateTeamMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateTeamMember)

responseUntagProject :: UntagProjectResponse -> TestTree
responseUntagProject =
  res
    "UntagProjectResponse"
    "fixture/UntagProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagProject)

responseUpdateTeamMember :: UpdateTeamMemberResponse -> TestTree
responseUpdateTeamMember =
  res
    "UpdateTeamMemberResponse"
    "fixture/UpdateTeamMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTeamMember)

responseDescribeUserProfile :: DescribeUserProfileResponse -> TestTree
responseDescribeUserProfile =
  res
    "DescribeUserProfileResponse"
    "fixture/DescribeUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserProfile)

responseListTagsForProject :: ListTagsForProjectResponse -> TestTree
responseListTagsForProject =
  res
    "ListTagsForProjectResponse"
    "fixture/ListTagsForProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForProject)

responseDeleteUserProfile :: DeleteUserProfileResponse -> TestTree
responseDeleteUserProfile =
  res
    "DeleteUserProfileResponse"
    "fixture/DeleteUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUserProfile)

responseUpdateUserProfile :: UpdateUserProfileResponse -> TestTree
responseUpdateUserProfile =
  res
    "UpdateUserProfileResponse"
    "fixture/UpdateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateUserProfile)

responseCreateUserProfile :: CreateUserProfileResponse -> TestTree
responseCreateUserProfile =
  res
    "CreateUserProfileResponse"
    "fixture/CreateUserProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUserProfile)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)
