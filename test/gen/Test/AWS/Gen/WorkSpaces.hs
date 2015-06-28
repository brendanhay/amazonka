-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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

import           Data.Proxy
import           Network.AWS.WorkSpaces
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeWorkspaceDirectoriesTest $
--             describeWorkspaceDirectories
--
--         , describeWorkspaceBundlesTest $
--             describeWorkspaceBundles
--
--         , rebuildWorkspacesTest $
--             rebuildWorkspaces
--
--         , rebootWorkspacesTest $
--             rebootWorkspaces
--
--         , terminateWorkspacesTest $
--             terminateWorkspaces
--
--         , createWorkspacesTest $
--             createWorkspaces
--
--         , describeWorkspacesTest $
--             describeWorkspaces
--
--           ]

--     , testGroup "response"
--         [ describeWorkspaceDirectoriesResponseTest $
--             describeWorkspaceDirectoriesResponse
--
--         , describeWorkspaceBundlesResponseTest $
--             describeWorkspaceBundlesResponse
--
--         , rebuildWorkspacesResponseTest $
--             rebuildWorkspacesResponse
--
--         , rebootWorkspacesResponseTest $
--             rebootWorkspacesResponse
--
--         , terminateWorkspacesResponseTest $
--             terminateWorkspacesResponse
--
--         , createWorkspacesResponseTest $
--             createWorkspacesResponse
--
--         , describeWorkspacesResponseTest $
--             describeWorkspacesResponse
--
--           ]
--     ]

-- Requests

describeWorkspaceDirectoriesTest :: DescribeWorkspaceDirectories -> TestTree
describeWorkspaceDirectoriesTest = undefined

describeWorkspaceBundlesTest :: DescribeWorkspaceBundles -> TestTree
describeWorkspaceBundlesTest = undefined

rebuildWorkspacesTest :: RebuildWorkspaces -> TestTree
rebuildWorkspacesTest = undefined

rebootWorkspacesTest :: RebootWorkspaces -> TestTree
rebootWorkspacesTest = undefined

terminateWorkspacesTest :: TerminateWorkspaces -> TestTree
terminateWorkspacesTest = undefined

createWorkspacesTest :: CreateWorkspaces -> TestTree
createWorkspacesTest = undefined

describeWorkspacesTest :: DescribeWorkspaces -> TestTree
describeWorkspacesTest = undefined

-- Responses

describeWorkspaceDirectoriesResponseTest :: DescribeWorkspaceDirectoriesResponse -> TestTree
describeWorkspaceDirectoriesResponseTest = resp
    "DescribeWorkspaceDirectoriesResponse"
    "fixture/WorkSpaces/DescribeWorkspaceDirectoriesResponse"
    (Proxy :: Proxy DescribeWorkspaceDirectories)

describeWorkspaceBundlesResponseTest :: DescribeWorkspaceBundlesResponse -> TestTree
describeWorkspaceBundlesResponseTest = resp
    "DescribeWorkspaceBundlesResponse"
    "fixture/WorkSpaces/DescribeWorkspaceBundlesResponse"
    (Proxy :: Proxy DescribeWorkspaceBundles)

rebuildWorkspacesResponseTest :: RebuildWorkspacesResponse -> TestTree
rebuildWorkspacesResponseTest = resp
    "RebuildWorkspacesResponse"
    "fixture/WorkSpaces/RebuildWorkspacesResponse"
    (Proxy :: Proxy RebuildWorkspaces)

rebootWorkspacesResponseTest :: RebootWorkspacesResponse -> TestTree
rebootWorkspacesResponseTest = resp
    "RebootWorkspacesResponse"
    "fixture/WorkSpaces/RebootWorkspacesResponse"
    (Proxy :: Proxy RebootWorkspaces)

terminateWorkspacesResponseTest :: TerminateWorkspacesResponse -> TestTree
terminateWorkspacesResponseTest = resp
    "TerminateWorkspacesResponse"
    "fixture/WorkSpaces/TerminateWorkspacesResponse"
    (Proxy :: Proxy TerminateWorkspaces)

createWorkspacesResponseTest :: CreateWorkspacesResponse -> TestTree
createWorkspacesResponseTest = resp
    "CreateWorkspacesResponse"
    "fixture/WorkSpaces/CreateWorkspacesResponse"
    (Proxy :: Proxy CreateWorkspaces)

describeWorkspacesResponseTest :: DescribeWorkspacesResponse -> TestTree
describeWorkspacesResponseTest = resp
    "DescribeWorkspacesResponse"
    "fixture/WorkSpaces/DescribeWorkspacesResponse"
    (Proxy :: Proxy DescribeWorkspaces)
