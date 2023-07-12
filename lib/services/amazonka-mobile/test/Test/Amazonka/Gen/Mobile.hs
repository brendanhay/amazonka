{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Mobile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Mobile where

import Amazonka.Mobile
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Mobile.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateProject $
--             newCreateProject
--
--         , requestDeleteProject $
--             newDeleteProject
--
--         , requestDescribeBundle $
--             newDescribeBundle
--
--         , requestDescribeProject $
--             newDescribeProject
--
--         , requestExportBundle $
--             newExportBundle
--
--         , requestExportProject $
--             newExportProject
--
--         , requestListBundles $
--             newListBundles
--
--         , requestListProjects $
--             newListProjects
--
--         , requestUpdateProject $
--             newUpdateProject
--
--           ]

--     , testGroup "response"
--         [ responseCreateProject $
--             newCreateProjectResponse
--
--         , responseDeleteProject $
--             newDeleteProjectResponse
--
--         , responseDescribeBundle $
--             newDescribeBundleResponse
--
--         , responseDescribeProject $
--             newDescribeProjectResponse
--
--         , responseExportBundle $
--             newExportBundleResponse
--
--         , responseExportProject $
--             newExportProjectResponse
--
--         , responseListBundles $
--             newListBundlesResponse
--
--         , responseListProjects $
--             newListProjectsResponse
--
--         , responseUpdateProject $
--             newUpdateProjectResponse
--
--           ]
--     ]

-- Requests

requestCreateProject :: CreateProject -> TestTree
requestCreateProject =
  req
    "CreateProject"
    "fixture/CreateProject.yaml"

requestDeleteProject :: DeleteProject -> TestTree
requestDeleteProject =
  req
    "DeleteProject"
    "fixture/DeleteProject.yaml"

requestDescribeBundle :: DescribeBundle -> TestTree
requestDescribeBundle =
  req
    "DescribeBundle"
    "fixture/DescribeBundle.yaml"

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

requestExportProject :: ExportProject -> TestTree
requestExportProject =
  req
    "ExportProject"
    "fixture/ExportProject.yaml"

requestListBundles :: ListBundles -> TestTree
requestListBundles =
  req
    "ListBundles"
    "fixture/ListBundles.yaml"

requestListProjects :: ListProjects -> TestTree
requestListProjects =
  req
    "ListProjects"
    "fixture/ListProjects.yaml"

requestUpdateProject :: UpdateProject -> TestTree
requestUpdateProject =
  req
    "UpdateProject"
    "fixture/UpdateProject.yaml"

-- Responses

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProject)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProject)

responseDescribeBundle :: DescribeBundleResponse -> TestTree
responseDescribeBundle =
  res
    "DescribeBundleResponse"
    "fixture/DescribeBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBundle)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeProject)

responseExportBundle :: ExportBundleResponse -> TestTree
responseExportBundle =
  res
    "ExportBundleResponse"
    "fixture/ExportBundleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportBundle)

responseExportProject :: ExportProjectResponse -> TestTree
responseExportProject =
  res
    "ExportProjectResponse"
    "fixture/ExportProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportProject)

responseListBundles :: ListBundlesResponse -> TestTree
responseListBundles =
  res
    "ListBundlesResponse"
    "fixture/ListBundlesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBundles)

responseListProjects :: ListProjectsResponse -> TestTree
responseListProjects =
  res
    "ListProjectsResponse"
    "fixture/ListProjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProjects)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProject)
