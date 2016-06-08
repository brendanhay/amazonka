{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.WorkSpaces
import Test.AWS.WorkSpaces.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeWorkspaceDirectories $
--             describeWorkspaceDirectories
--
--         , requestDescribeWorkspaceBundles $
--             describeWorkspaceBundles
--
--         , requestRebuildWorkspaces $
--             rebuildWorkspaces
--
--         , requestRebootWorkspaces $
--             rebootWorkspaces
--
--         , requestTerminateWorkspaces $
--             terminateWorkspaces
--
--         , requestCreateWorkspaces $
--             createWorkspaces
--
--         , requestDescribeWorkspaces $
--             describeWorkspaces
--
--           ]

--     , testGroup "response"
--         [ responseDescribeWorkspaceDirectories $
--             describeWorkspaceDirectoriesResponse
--
--         , responseDescribeWorkspaceBundles $
--             describeWorkspaceBundlesResponse
--
--         , responseRebuildWorkspaces $
--             rebuildWorkspacesResponse
--
--         , responseRebootWorkspaces $
--             rebootWorkspacesResponse
--
--         , responseTerminateWorkspaces $
--             terminateWorkspacesResponse
--
--         , responseCreateWorkspaces $
--             createWorkspacesResponse
--
--         , responseDescribeWorkspaces $
--             describeWorkspacesResponse
--
--           ]
--     ]

-- Requests

requestDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
requestDescribeWorkspaceDirectories = req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles = req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces = req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces = req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces = req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces = req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces = req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

-- Responses

responseDescribeWorkspaceDirectories :: DescribeWorkspaceDirectoriesResponse -> TestTree
responseDescribeWorkspaceDirectories = res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceDirectories)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles = res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces = res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy RebuildWorkspaces)

responseRebootWorkspaces :: RebootWorkspacesResponse -> TestTree
responseRebootWorkspaces = res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy RebootWorkspaces)

responseTerminateWorkspaces :: TerminateWorkspacesResponse -> TestTree
responseTerminateWorkspaces = res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy TerminateWorkspaces)

responseCreateWorkspaces :: CreateWorkspacesResponse -> TestTree
responseCreateWorkspaces = res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateWorkspaces)

responseDescribeWorkspaces :: DescribeWorkspacesResponse -> TestTree
responseDescribeWorkspaces = res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaces)
