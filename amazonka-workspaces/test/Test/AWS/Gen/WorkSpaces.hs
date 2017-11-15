{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkSpaces where

import Data.Proxy
import Network.AWS.WorkSpaces
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WorkSpaces.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestModifyWorkspaceProperties $
--             modifyWorkspaceProperties
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDescribeWorkspaceDirectories $
--             describeWorkspaceDirectories
--
--         , requestDescribeWorkspaceBundles $
--             describeWorkspaceBundles
--
--         , requestRebuildWorkspaces $
--             rebuildWorkspaces
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestStopWorkspaces $
--             stopWorkspaces
--
--         , requestDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatus
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
--         , requestStartWorkspaces $
--             startWorkspaces
--
--           ]

--     , testGroup "response"
--         [ responseModifyWorkspaceProperties $
--             modifyWorkspacePropertiesResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDescribeWorkspaceDirectories $
--             describeWorkspaceDirectoriesResponse
--
--         , responseDescribeWorkspaceBundles $
--             describeWorkspaceBundlesResponse
--
--         , responseRebuildWorkspaces $
--             rebuildWorkspacesResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseStopWorkspaces $
--             stopWorkspacesResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatusResponse
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
--         , responseStartWorkspaces $
--             startWorkspacesResponse
--
--           ]
--     ]

-- Requests

requestModifyWorkspaceProperties :: ModifyWorkspaceProperties -> TestTree
requestModifyWorkspaceProperties = req
    "ModifyWorkspaceProperties"
    "fixture/ModifyWorkspaceProperties.yaml"

requestDescribeTags :: DescribeTags -> TestTree
requestDescribeTags = req
    "DescribeTags"
    "fixture/DescribeTags.yaml"

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

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces = req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus = req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

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

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces = req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

-- Responses

responseModifyWorkspaceProperties :: ModifyWorkspacePropertiesResponse -> TestTree
responseModifyWorkspaceProperties = res
    "ModifyWorkspacePropertiesResponse"
    "fixture/ModifyWorkspacePropertiesResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceProperties)

responseDescribeTags :: DescribeTagsResponse -> TestTree
responseDescribeTags = res
    "DescribeTagsResponse"
    "fixture/DescribeTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeTags)

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

responseCreateTags :: CreateTagsResponse -> TestTree
responseCreateTags = res
    "CreateTagsResponse"
    "fixture/CreateTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateTags)

responseDeleteTags :: DeleteTagsResponse -> TestTree
responseDeleteTags = res
    "DeleteTagsResponse"
    "fixture/DeleteTagsResponse.proto"
    workSpaces
    (Proxy :: Proxy DeleteTags)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces = res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy StopWorkspaces)

responseDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatusResponse -> TestTree
responseDescribeWorkspacesConnectionStatus = res
    "DescribeWorkspacesConnectionStatusResponse"
    "fixture/DescribeWorkspacesConnectionStatusResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspacesConnectionStatus)

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

responseStartWorkspaces :: StartWorkspacesResponse -> TestTree
responseStartWorkspaces = res
    "StartWorkspacesResponse"
    "fixture/StartWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy StartWorkspaces)
