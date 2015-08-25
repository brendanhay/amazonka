{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2015 Brendan Hay
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
testDescribeWorkspaceDirectories = req
    "DescribeWorkspaceDirectories"
    "fixture/DescribeWorkspaceDirectories"

testDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
testDescribeWorkspaceBundles = req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles"

testRebuildWorkspaces :: RebuildWorkspaces -> TestTree
testRebuildWorkspaces = req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces"

testRebootWorkspaces :: RebootWorkspaces -> TestTree
testRebootWorkspaces = req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces"

testTerminateWorkspaces :: TerminateWorkspaces -> TestTree
testTerminateWorkspaces = req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces"

testCreateWorkspaces :: CreateWorkspaces -> TestTree
testCreateWorkspaces = req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces"

testDescribeWorkspaces :: DescribeWorkspaces -> TestTree
testDescribeWorkspaces = req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces"

-- Responses

testDescribeWorkspaceDirectoriesResponse :: DescribeWorkspaceDirectoriesResponse -> TestTree
testDescribeWorkspaceDirectoriesResponse = res
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceDirectories)

testDescribeWorkspaceBundlesResponse :: DescribeWorkspaceBundlesResponse -> TestTree
testDescribeWorkspaceBundlesResponse = res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceBundles)

testRebuildWorkspacesResponse :: RebuildWorkspacesResponse -> TestTree
testRebuildWorkspacesResponse = res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse"
    workSpaces
    (Proxy :: Proxy RebuildWorkspaces)

testRebootWorkspacesResponse :: RebootWorkspacesResponse -> TestTree
testRebootWorkspacesResponse = res
    "RebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse"
    workSpaces
    (Proxy :: Proxy RebootWorkspaces)

testTerminateWorkspacesResponse :: TerminateWorkspacesResponse -> TestTree
testTerminateWorkspacesResponse = res
    "TerminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse"
    workSpaces
    (Proxy :: Proxy TerminateWorkspaces)

testCreateWorkspacesResponse :: CreateWorkspacesResponse -> TestTree
testCreateWorkspacesResponse = res
    "CreateWorkspacesResponse"
    "fixture/CreateWorkspacesResponse"
    workSpaces
    (Proxy :: Proxy CreateWorkspaces)

testDescribeWorkspacesResponse :: DescribeWorkspacesResponse -> TestTree
testDescribeWorkspacesResponse = res
    "DescribeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaces)
