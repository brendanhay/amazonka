{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.WorkSpaces

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeWorkspaceDirectories $
--             describeWorkspaceDirectories
--
--         , testDescribeWorkspaceBundles $
--             describeWorkspaceBundles
--
--         , testRebuildWorkspaces $
--             rebuildWorkspaces
--
--         , testRebootWorkspaces $
--             rebootWorkspaces
--
--         , testTerminateWorkspaces $
--             terminateWorkspaces
--
--         , testCreateWorkspaces $
--             createWorkspaces
--
--         , testDescribeWorkspaces $
--             describeWorkspaces
--
--           ]

--     , testGroup "response"
--         [ testDescribeWorkspaceDirectoriesResponse $
--             describeWorkspaceDirectoriesResponse
--
--         , testDescribeWorkspaceBundlesResponse $
--             describeWorkspaceBundlesResponse
--
--         , testRebuildWorkspacesResponse $
--             rebuildWorkspacesResponse
--
--         , testRebootWorkspacesResponse $
--             rebootWorkspacesResponse
--
--         , testTerminateWorkspacesResponse $
--             terminateWorkspacesResponse
--
--         , testCreateWorkspacesResponse $
--             createWorkspacesResponse
--
--         , testDescribeWorkspacesResponse $
--             describeWorkspacesResponse
--
--           ]
--     ]

-- Requests

testDescribeWorkspaceDirectories :: DescribeWorkspaceDirectories -> TestTree
testDescribeWorkspaceDirectories = undefined

testDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
testDescribeWorkspaceBundles = undefined

testRebuildWorkspaces :: RebuildWorkspaces -> TestTree
testRebuildWorkspaces = undefined

testRebootWorkspaces :: RebootWorkspaces -> TestTree
testRebootWorkspaces = undefined

testTerminateWorkspaces :: TerminateWorkspaces -> TestTree
testTerminateWorkspaces = undefined

testCreateWorkspaces :: CreateWorkspaces -> TestTree
testCreateWorkspaces = undefined

testDescribeWorkspaces :: DescribeWorkspaces -> TestTree
testDescribeWorkspaces = undefined

-- Responses

testDescribeWorkspaceDirectoriesResponse :: DescribeWorkspaceDirectoriesResponse -> TestTree
testDescribeWorkspaceDirectoriesResponse = resp
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse"
    (Proxy :: Proxy DescribeWorkspaceDirectories)

testDescribeWorkspaceBundlesResponse :: DescribeWorkspaceBundlesResponse -> TestTree
testDescribeWorkspaceBundlesResponse = resp
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse"
    (Proxy :: Proxy DescribeWorkspaceBundles)

testRebuildWorkspacesResponse :: RebuildWorkspacesResponse -> TestTree
testRebuildWorkspacesResponse = resp
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse"
    (Proxy :: Proxy RebuildWorkspaces)

testRebootWorkspacesResponse :: RebootWorkspacesResponse -> TestTree
testRebootWorkspacesResponse = resp
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse"
    (Proxy :: Proxy RebootWorkspaces)

testTerminateWorkspacesResponse :: TerminateWorkspacesResponse -> TestTree
testTerminateWorkspacesResponse = resp
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse"
    (Proxy :: Proxy TerminateWorkspaces)

testCreateWorkspacesResponse :: CreateWorkspacesResponse -> TestTree
testCreateWorkspacesResponse = resp
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse"
    (Proxy :: Proxy CreateWorkspaces)

testDescribeWorkspacesResponse :: DescribeWorkspacesResponse -> TestTree
testDescribeWorkspacesResponse = resp
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse"
    (Proxy :: Proxy DescribeWorkspaces)

instance Out Compute
instance Out ComputeType
instance Out CreateWorkspaces
instance Out CreateWorkspacesResponse
instance Out DefaultWorkspaceCreationProperties
instance Out DescribeWorkspaceBundles
instance Out DescribeWorkspaceBundlesResponse
instance Out DescribeWorkspaceDirectories
instance Out DescribeWorkspaceDirectoriesResponse
instance Out DescribeWorkspaces
instance Out DescribeWorkspacesResponse
instance Out FailedCreateWorkspaceRequest
instance Out FailedWorkspaceChangeRequest
instance Out RebootRequest
instance Out RebootWorkspaces
instance Out RebootWorkspacesResponse
instance Out RebuildRequest
instance Out RebuildWorkspaces
instance Out RebuildWorkspacesResponse
instance Out TerminateRequest
instance Out TerminateWorkspaces
instance Out TerminateWorkspacesResponse
instance Out UserStorage
instance Out Workspace
instance Out WorkspaceBundle
instance Out WorkspaceDirectory
instance Out WorkspaceDirectoryState
instance Out WorkspaceDirectoryType
instance Out WorkspaceRequest
instance Out WorkspaceState
