{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoT1ClickProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
--         [ requestListProjects $
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDisassociateDeviceFromPlacement $
--             newDisassociateDeviceFromPlacement
--
--         , requestCreatePlacement $
--             newCreatePlacement
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestGetDevicesInPlacement $
--             newGetDevicesInPlacement
--
--         , requestDescribePlacement $
--             newDescribePlacement
--
--         , requestAssociateDeviceWithPlacement $
--             newAssociateDeviceWithPlacement
--
--         , requestDeletePlacement $
--             newDeletePlacement
--
--         , requestUpdatePlacement $
--             newUpdatePlacement
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListPlacements $
--             newListPlacements
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateProject $
--             newCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDisassociateDeviceFromPlacement $
--             newDisassociateDeviceFromPlacementResponse
--
--         , responseCreatePlacement $
--             newCreatePlacementResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseGetDevicesInPlacement $
--             newGetDevicesInPlacementResponse
--
--         , responseDescribePlacement $
--             newDescribePlacementResponse
--
--         , responseAssociateDeviceWithPlacement $
--             newAssociateDeviceWithPlacementResponse
--
--         , responseDeletePlacement $
--             newDeletePlacementResponse
--
--         , responseUpdatePlacement $
--             newUpdatePlacementResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListPlacements $
--             newListPlacementsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDisassociateDeviceFromPlacement :: DisassociateDeviceFromPlacement -> TestTree
requestDisassociateDeviceFromPlacement =
  req
    "DisassociateDeviceFromPlacement"
    "fixture/DisassociateDeviceFromPlacement.yaml"

requestCreatePlacement :: CreatePlacement -> TestTree
requestCreatePlacement =
  req
    "CreatePlacement"
    "fixture/CreatePlacement.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestGetDevicesInPlacement :: GetDevicesInPlacement -> TestTree
requestGetDevicesInPlacement =
  req
    "GetDevicesInPlacement"
    "fixture/GetDevicesInPlacement.yaml"

requestDescribePlacement :: DescribePlacement -> TestTree
requestDescribePlacement =
  req
    "DescribePlacement"
    "fixture/DescribePlacement.yaml"

requestAssociateDeviceWithPlacement :: AssociateDeviceWithPlacement -> TestTree
requestAssociateDeviceWithPlacement =
  req
    "AssociateDeviceWithPlacement"
    "fixture/AssociateDeviceWithPlacement.yaml"

requestDeletePlacement :: DeletePlacement -> TestTree
requestDeletePlacement =
  req
    "DeletePlacement"
    "fixture/DeletePlacement.yaml"

requestUpdatePlacement :: UpdatePlacement -> TestTree
requestUpdatePlacement =
  req
    "UpdatePlacement"
    "fixture/UpdatePlacement.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListPlacements :: ListPlacements -> TestTree
requestListPlacements =
  req
    "ListPlacements"
    "fixture/ListPlacements.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDisassociateDeviceFromPlacement :: DisassociateDeviceFromPlacementResponse -> TestTree
responseDisassociateDeviceFromPlacement =
  res
    "DisassociateDeviceFromPlacementResponse"
    "fixture/DisassociateDeviceFromPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDeviceFromPlacement)

responseCreatePlacement :: CreatePlacementResponse -> TestTree
responseCreatePlacement =
  res
    "CreatePlacementResponse"
    "fixture/CreatePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlacement)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseGetDevicesInPlacement :: GetDevicesInPlacementResponse -> TestTree
responseGetDevicesInPlacement =
  res
    "GetDevicesInPlacementResponse"
    "fixture/GetDevicesInPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDevicesInPlacement)

responseDescribePlacement :: DescribePlacementResponse -> TestTree
responseDescribePlacement =
  res
    "DescribePlacementResponse"
    "fixture/DescribePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlacement)

responseAssociateDeviceWithPlacement :: AssociateDeviceWithPlacementResponse -> TestTree
responseAssociateDeviceWithPlacement =
  res
    "AssociateDeviceWithPlacementResponse"
    "fixture/AssociateDeviceWithPlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDeviceWithPlacement)

responseDeletePlacement :: DeletePlacementResponse -> TestTree
responseDeletePlacement =
  res
    "DeletePlacementResponse"
    "fixture/DeletePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlacement)

responseUpdatePlacement :: UpdatePlacementResponse -> TestTree
responseUpdatePlacement =
  res
    "UpdatePlacementResponse"
    "fixture/UpdatePlacementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePlacement)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListPlacements :: ListPlacementsResponse -> TestTree
responseListPlacements =
  res
    "ListPlacementsResponse"
    "fixture/ListPlacementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlacements)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)
