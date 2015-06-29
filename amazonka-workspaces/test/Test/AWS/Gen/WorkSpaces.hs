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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.WorkSpaces

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ createWorkspacesTest $
--             createWorkspaces
--
--         , describeWorkspaceBundlesTest $
--             describeWorkspaceBundles
--
--         , describeWorkspaceDirectoriesTest $
--             describeWorkspaceDirectories
--
--         , describeWorkspacesTest $
--             describeWorkspaces
--
--         , rebootWorkspacesTest $
--             rebootWorkspaces
--
--         , rebuildWorkspacesTest $
--             rebuildWorkspaces
--
--         , terminateWorkspacesTest $
--             terminateWorkspaces
--
--           ]

--     , testGroup "response"
--         [ createWorkspacesResponseTest $
--             createWorkspacesResponse
--
--         , describeWorkspaceBundlesResponseTest $
--             describeWorkspaceBundlesResponse
--
--         , describeWorkspaceDirectoriesResponseTest $
--             describeWorkspaceDirectoriesResponse
--
--         , describeWorkspacesResponseTest $
--             describeWorkspacesResponse
--
--         , rebootWorkspacesResponseTest $
--             rebootWorkspacesResponse
--
--         , rebuildWorkspacesResponseTest $
--             rebuildWorkspacesResponse
--
--         , terminateWorkspacesResponseTest $
--             terminateWorkspacesResponse
--
--           ]
--     ]

-- Requests

createWorkspacesTest :: CreateWorkspaces -> TestTree
createWorkspacesTest = undefined

describeWorkspaceBundlesTest :: DescribeWorkspaceBundles -> TestTree
describeWorkspaceBundlesTest = undefined

describeWorkspaceDirectoriesTest :: DescribeWorkspaceDirectories -> TestTree
describeWorkspaceDirectoriesTest = undefined

describeWorkspacesTest :: DescribeWorkspaces -> TestTree
describeWorkspacesTest = undefined

rebootWorkspacesTest :: RebootWorkspaces -> TestTree
rebootWorkspacesTest = undefined

rebuildWorkspacesTest :: RebuildWorkspaces -> TestTree
rebuildWorkspacesTest = undefined

terminateWorkspacesTest :: TerminateWorkspaces -> TestTree
terminateWorkspacesTest = undefined

-- Responses

createWorkspacesResponseTest :: CreateWorkspacesResponse -> TestTree
createWorkspacesResponseTest = resp
    "createWorkspacesResponse"
    "fixture/CreateWorkspacesResponse"
    (Proxy :: Proxy CreateWorkspaces)

describeWorkspaceBundlesResponseTest :: DescribeWorkspaceBundlesResponse -> TestTree
describeWorkspaceBundlesResponseTest = resp
    "describeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse"
    (Proxy :: Proxy DescribeWorkspaceBundles)

describeWorkspaceDirectoriesResponseTest :: DescribeWorkspaceDirectoriesResponse -> TestTree
describeWorkspaceDirectoriesResponseTest = resp
    "describeWorkspaceDirectoriesResponse"
    "fixture/DescribeWorkspaceDirectoriesResponse"
    (Proxy :: Proxy DescribeWorkspaceDirectories)

describeWorkspacesResponseTest :: DescribeWorkspacesResponse -> TestTree
describeWorkspacesResponseTest = resp
    "describeWorkspacesResponse"
    "fixture/DescribeWorkspacesResponse"
    (Proxy :: Proxy DescribeWorkspaces)

rebootWorkspacesResponseTest :: RebootWorkspacesResponse -> TestTree
rebootWorkspacesResponseTest = resp
    "rebootWorkspacesResponse"
    "fixture/RebootWorkspacesResponse"
    (Proxy :: Proxy RebootWorkspaces)

rebuildWorkspacesResponseTest :: RebuildWorkspacesResponse -> TestTree
rebuildWorkspacesResponseTest = resp
    "rebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse"
    (Proxy :: Proxy RebuildWorkspaces)

terminateWorkspacesResponseTest :: TerminateWorkspacesResponse -> TestTree
terminateWorkspacesResponseTest = resp
    "terminateWorkspacesResponse"
    "fixture/TerminateWorkspacesResponse"
    (Proxy :: Proxy TerminateWorkspaces)
