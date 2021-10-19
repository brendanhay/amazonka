{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Mobile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Mobile where

import Data.Proxy
import Network.AWS.Mobile
import Test.AWS.Fixture
import Test.AWS.Mobile.Internal
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
--             newListProjects
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestListBundles $
--             newListBundles
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestExportProject $
--             newExportProject
--
--         , requestDescribeBundle $
--             newDescribeBundle
--
--         , requestExportBundle $
--             newExportBundle
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
--         , responseListBundles $
--             newListBundlesResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseExportProject $
--             newExportProjectResponse
--
--         , responseDescribeBundle $
--             newDescribeBundleResponse
--
--         , responseExportBundle $
--             newExportBundleResponse
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

requestListBundles :: ListBundles -> TestTree
requestListBundles =
  req
    "ListBundles"
    "fixture/ListBundles.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestExportProject :: ExportProject -> TestTree
requestExportProject =
  req
    "ExportProject"
    "fixture/ExportProject.yaml"

requestDescribeBundle :: DescribeBundle -> TestTree
requestDescribeBundle =
  req
    "DescribeBundle"
    "fixture/DescribeBundle.yaml"

requestExportBundle :: ExportBundle -> TestTree
requestExportBundle =
  req
    "ExportBundle"
    "fixture/ExportBundle.yaml"

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
    (Proxy :: Proxy ListProjects)

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

responseListBundles :: ListBundlesResponse -> TestTree
responseListBundles =
  res
    "ListBundlesResponse"
    "fixture/ListBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBundles)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)

responseExportProject :: ExportProjectResponse -> TestTree
responseExportProject =
  res
    "ExportProjectResponse"
    "fixture/ExportProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ExportProject)

responseDescribeBundle :: DescribeBundleResponse -> TestTree
responseDescribeBundle =
  res
    "DescribeBundleResponse"
    "fixture/DescribeBundleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBundle)

responseExportBundle :: ExportBundleResponse -> TestTree
responseExportBundle =
  res
    "ExportBundleResponse"
    "fixture/ExportBundleResponse.proto"
    defaultService
    (Proxy :: Proxy ExportBundle)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy CreateProject)
