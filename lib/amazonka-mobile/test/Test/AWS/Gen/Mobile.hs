{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Mobile
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--             mkListProjects
--
--         , requestDeleteProject $
--             mkDeleteProject
--
--         , requestUpdateProject $
--             mkUpdateProject
--
--         , requestListBundles $
--             mkListBundles
--
--         , requestDescribeProject $
--             mkDescribeProject
--
--         , requestExportProject $
--             mkExportProject
--
--         , requestDescribeBundle $
--             mkDescribeBundle
--
--         , requestExportBundle $
--             mkExportBundle
--
--         , requestCreateProject $
--             mkCreateProject
--
--           ]

--     , testGroup "response"
--         [ responseListProjects $
--             mkListProjectsResponse
--
--         , responseDeleteProject $
--             mkDeleteProjectResponse
--
--         , responseUpdateProject $
--             mkUpdateProjectResponse
--
--         , responseListBundles $
--             mkListBundlesResponse
--
--         , responseDescribeProject $
--             mkDescribeProjectResponse
--
--         , responseExportProject $
--             mkExportProjectResponse
--
--         , responseDescribeBundle $
--             mkDescribeBundleResponse
--
--         , responseExportBundle $
--             mkExportBundleResponse
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
    mobileService
    (Proxy :: Proxy ListProjects)

responseDeleteProject :: DeleteProjectResponse -> TestTree
responseDeleteProject =
  res
    "DeleteProjectResponse"
    "fixture/DeleteProjectResponse.proto"
    mobileService
    (Proxy :: Proxy DeleteProject)

responseUpdateProject :: UpdateProjectResponse -> TestTree
responseUpdateProject =
  res
    "UpdateProjectResponse"
    "fixture/UpdateProjectResponse.proto"
    mobileService
    (Proxy :: Proxy UpdateProject)

responseListBundles :: ListBundlesResponse -> TestTree
responseListBundles =
  res
    "ListBundlesResponse"
    "fixture/ListBundlesResponse.proto"
    mobileService
    (Proxy :: Proxy ListBundles)

responseDescribeProject :: DescribeProjectResponse -> TestTree
responseDescribeProject =
  res
    "DescribeProjectResponse"
    "fixture/DescribeProjectResponse.proto"
    mobileService
    (Proxy :: Proxy DescribeProject)

responseExportProject :: ExportProjectResponse -> TestTree
responseExportProject =
  res
    "ExportProjectResponse"
    "fixture/ExportProjectResponse.proto"
    mobileService
    (Proxy :: Proxy ExportProject)

responseDescribeBundle :: DescribeBundleResponse -> TestTree
responseDescribeBundle =
  res
    "DescribeBundleResponse"
    "fixture/DescribeBundleResponse.proto"
    mobileService
    (Proxy :: Proxy DescribeBundle)

responseExportBundle :: ExportBundleResponse -> TestTree
responseExportBundle =
  res
    "ExportBundleResponse"
    "fixture/ExportBundleResponse.proto"
    mobileService
    (Proxy :: Proxy ExportBundle)

responseCreateProject :: CreateProjectResponse -> TestTree
responseCreateProject =
  res
    "CreateProjectResponse"
    "fixture/CreateProjectResponse.proto"
    mobileService
    (Proxy :: Proxy CreateProject)
