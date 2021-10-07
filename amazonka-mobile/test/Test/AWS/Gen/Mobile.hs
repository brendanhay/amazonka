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
--         , requestCreateProject $
--             newCreateProject
--
--         , requestDescribeBundle $
--             newDescribeBundle
--
--         , requestListBundles $
--             newListBundles
--
--         , requestUpdateProject $
--             newUpdateProject
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestExportProject $
--             newExportProject
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestExportBundle $
--             newExportBundle
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             newListProjectsResponse
--
--         , responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDescribeBundle $
--             newDescribeBundleResponse
--
--         , responseListBundles $
--             newListBundlesResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseExportProject $
--             newExportProjectResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseExportBundle $
--             newExportBundleResponse
--
--           ]
--     ]

-- Requests

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

requestDescribeBundle :: DescribeBundle -> TestTree
requestDescribeBundle =
  req
    "DescribeBundle"
    "fixture/DescribeBundle.yaml"

requestListBundles :: ListBundles -> TestTree
requestListBundles =
  req
    "ListBundles"
    "fixture/ListBundles.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestExportProject :: ExportProject -> TestTree
requestExportProject =
  req
    "ExportProject"
    "fixture/ExportProject.yaml"

requestDescribeProject :: DescribeProject -> TestTree
requestDescribeProject =
  req
    "DescribeProject"
    "fixture/DescribeProject.yaml"

requestExportBundle :: ExportBundle -> TestTree
requestExportBundle =
  req
    "ExportBundle"
    "fixture/ExportBundle.yaml"

-- Responses

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

responseDescribeBundle :: DescribeBundleResponse -> TestTree
responseDescribeBundle =
  res
    "DescribeBundleResponse"
    "fixture/DescribeBundleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBundle)

responseListBundles :: ListBundlesResponse -> TestTree
responseListBundles =
  res
    "ListBundlesResponse"
    "fixture/ListBundlesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBundles)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateProject)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteProject)

responseExportProject :: ExportProjectResponse -> TestTree
responseExportProject =
  res
    "ExportProjectResponse"
    "fixture/ExportProjectResponse.proto"
    defaultService
    (Proxy :: Proxy ExportProject)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeProject)

responseExportBundle :: ExportBundleResponse -> TestTree
responseExportBundle =
  res
    "ExportBundleResponse"
    "fixture/ExportBundleResponse.proto"
    defaultService
    (Proxy :: Proxy ExportBundle)
