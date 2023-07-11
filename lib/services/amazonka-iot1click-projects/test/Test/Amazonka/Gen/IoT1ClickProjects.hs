{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoT1ClickProjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoT1ClickProjects where

import Amazonka.IoT1ClickProjects
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoT1ClickProjects.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateDeviceWithPlacement $
--             newAssociateDeviceWithPlacement
--
--         , requestCreatePlacement $
--             newCreatePlacement
--
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDeletePlacement $
--             newDeletePlacement
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribePlacement $
--             newDescribePlacement
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestDisassociateDeviceFromPlacement $
--             newDisassociateDeviceFromPlacement
--
--         , requestGetDevicesInPlacement $
--             newGetDevicesInPlacement
--
--         , requestListPlacements $
--             newListPlacements
--
--         , requestListProjects $
--             newListProjects
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdatePlacement $
--             newUpdatePlacement
--
--         , requestUpdateProject $
--             newUpdateProject
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDeviceWithPlacement $
--             newAssociateDeviceWithPlacementResponse
--
--         , responseCreatePlacement $
--             newCreatePlacementResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDeletePlacement $
--             newDeletePlacementResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribePlacement $
--             newDescribePlacementResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseDisassociateDeviceFromPlacement $
--             newDisassociateDeviceFromPlacementResponse
--
--         , responseGetDevicesInPlacement $
--             newGetDevicesInPlacementResponse
--
--         , responseListPlacements $
--             newListPlacementsResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdatePlacement $
--             newUpdatePlacementResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--           ]
--     ]

-- Requests

requestAssociateDeviceWithPlacement :: AssociateDeviceWithPlacement -> TestTree
requestAssociateDeviceWithPlacement =
  req
    "AssociateDeviceWithPlacement"
    "fixture/AssociateDeviceWithPlacement.yaml"

requestCreatePlacement :: CreatePlacement -> TestTree
requestCreatePlacement =
  req
    "CreatePlacement"
    "fixture/CreatePlacement.yaml"

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDeletePlacement :: DeletePlacement -> TestTree
requestDeletePlacement =
  req
    "DeletePlacement"
    "fixture/DeletePlacement.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDescribePlacement :: DescribePlacement -> TestTree
requestDescribePlacement =
  req
    "DescribePlacement"
    "fixture/DescribePlacement.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestDisassociateDeviceFromPlacement :: DisassociateDeviceFromPlacement -> TestTree
requestDisassociateDeviceFromPlacement =
  req
    "DisassociateDeviceFromPlacement"
    "fixture/DisassociateDeviceFromPlacement.yaml"

requestGetDevicesInPlacement :: GetDevicesInPlacement -> TestTree
requestGetDevicesInPlacement =
  req
    "GetDevicesInPlacement"
    "fixture/GetDevicesInPlacement.yaml"

requestListPlacements :: ListPlacements -> TestTree
requestListPlacements =
  req
    "ListPlacements"
    "fixture/ListPlacements.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdatePlacement :: UpdatePlacement -> TestTree
requestUpdatePlacement =
  req
    "UpdatePlacement"
    "fixture/UpdatePlacement.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

-- Responses

responseAssociateDeviceWithPlacement :: AssociateDeviceWithPlacementResponse -> TestTree
responseAssociateDeviceWithPlacement =
  res
    "AssociateDeviceWithPlacementResponse"
    "fixture/AssociateDeviceWithPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDeviceWithPlacement)

responseCreatePlacement :: CreatePlacementResponse -> TestTree
responseCreatePlacement =
  res
    "CreatePlacementResponse"
    "fixture/CreatePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlacement)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDeletePlacement :: DeletePlacementResponse -> TestTree
responseDeletePlacement =
  res
    "DeletePlacementResponse"
    "fixture/DeletePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlacement)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDescribePlacement :: DescribePlacementResponse -> TestTree
responseDescribePlacement =
  res
    "DescribePlacementResponse"
    "fixture/DescribePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlacement)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseDisassociateDeviceFromPlacement :: DisassociateDeviceFromPlacementResponse -> TestTree
responseDisassociateDeviceFromPlacement =
  res
    "DisassociateDeviceFromPlacementResponse"
    "fixture/DisassociateDeviceFromPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDeviceFromPlacement)

responseGetDevicesInPlacement :: GetDevicesInPlacementResponse -> TestTree
responseGetDevicesInPlacement =
  res
    "GetDevicesInPlacementResponse"
    "fixture/GetDevicesInPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicesInPlacement)

responseListPlacements :: ListPlacementsResponse -> TestTree
responseListPlacements =
  res
    "ListPlacementsResponse"
    "fixture/ListPlacementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlacements)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdatePlacement :: UpdatePlacementResponse -> TestTree
responseUpdatePlacement =
  res
    "UpdatePlacementResponse"
    "fixture/UpdatePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePlacement)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)
