{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkSpaces
-- Copyright   : (c) 2013-2018 Brendan Hay
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
--         [ requestRevokeIPRules $
--             revokeIPRules
--
--         , requestModifyWorkspaceProperties $
--             modifyWorkspaceProperties
--
--         , requestDescribeTags $
--             describeTags
--
--         , requestDescribeWorkspaceDirectories $
--             describeWorkspaceDirectories
--
--         , requestDisassociateIPGroups $
--             disassociateIPGroups
--
--         , requestDescribeWorkspaceBundles $
--             describeWorkspaceBundles
--
--         , requestAuthorizeIPRules $
--             authorizeIPRules
--
--         , requestRebuildWorkspaces $
--             rebuildWorkspaces
--
--         , requestModifyWorkspaceState $
--             modifyWorkspaceState
--
--         , requestCreateIPGroup $
--             createIPGroup
--
--         , requestCreateTags $
--             createTags
--
--         , requestDeleteTags $
--             deleteTags
--
--         , requestUpdateRulesOfIPGroup $
--             updateRulesOfIPGroup
--
--         , requestStopWorkspaces $
--             stopWorkspaces
--
--         , requestAssociateIPGroups $
--             associateIPGroups
--
--         , requestDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatus
--
--         , requestRebootWorkspaces $
--             rebootWorkspaces
--
--         , requestDeleteIPGroup $
--             deleteIPGroup
--
--         , requestTerminateWorkspaces $
--             terminateWorkspaces
--
--         , requestCreateWorkspaces $
--             createWorkspaces
--
--         , requestDescribeIPGroups $
--             describeIPGroups
--
--         , requestDescribeWorkspaces $
--             describeWorkspaces
--
--         , requestStartWorkspaces $
--             startWorkspaces
--
--           ]

--     , testGroup "response"
--         [ responseRevokeIPRules $
--             revokeIPRulesResponse
--
--         , responseModifyWorkspaceProperties $
--             modifyWorkspacePropertiesResponse
--
--         , responseDescribeTags $
--             describeTagsResponse
--
--         , responseDescribeWorkspaceDirectories $
--             describeWorkspaceDirectoriesResponse
--
--         , responseDisassociateIPGroups $
--             disassociateIPGroupsResponse
--
--         , responseDescribeWorkspaceBundles $
--             describeWorkspaceBundlesResponse
--
--         , responseAuthorizeIPRules $
--             authorizeIPRulesResponse
--
--         , responseRebuildWorkspaces $
--             rebuildWorkspacesResponse
--
--         , responseModifyWorkspaceState $
--             modifyWorkspaceStateResponse
--
--         , responseCreateIPGroup $
--             createIPGroupResponse
--
--         , responseCreateTags $
--             createTagsResponse
--
--         , responseDeleteTags $
--             deleteTagsResponse
--
--         , responseUpdateRulesOfIPGroup $
--             updateRulesOfIPGroupResponse
--
--         , responseStopWorkspaces $
--             stopWorkspacesResponse
--
--         , responseAssociateIPGroups $
--             associateIPGroupsResponse
--
--         , responseDescribeWorkspacesConnectionStatus $
--             describeWorkspacesConnectionStatusResponse
--
--         , responseRebootWorkspaces $
--             rebootWorkspacesResponse
--
--         , responseDeleteIPGroup $
--             deleteIPGroupResponse
--
--         , responseTerminateWorkspaces $
--             terminateWorkspacesResponse
--
--         , responseCreateWorkspaces $
--             createWorkspacesResponse
--
--         , responseDescribeIPGroups $
--             describeIPGroupsResponse
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

requestRevokeIPRules :: RevokeIPRules -> TestTree
requestRevokeIPRules = req
    "RevokeIPRules"
    "fixture/RevokeIPRules.yaml"

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

requestDisassociateIPGroups :: DisassociateIPGroups -> TestTree
requestDisassociateIPGroups = req
    "DisassociateIPGroups"
    "fixture/DisassociateIPGroups.yaml"

requestDescribeWorkspaceBundles :: DescribeWorkspaceBundles -> TestTree
requestDescribeWorkspaceBundles = req
    "DescribeWorkspaceBundles"
    "fixture/DescribeWorkspaceBundles.yaml"

requestAuthorizeIPRules :: AuthorizeIPRules -> TestTree
requestAuthorizeIPRules = req
    "AuthorizeIPRules"
    "fixture/AuthorizeIPRules.yaml"

requestRebuildWorkspaces :: RebuildWorkspaces -> TestTree
requestRebuildWorkspaces = req
    "RebuildWorkspaces"
    "fixture/RebuildWorkspaces.yaml"

requestModifyWorkspaceState :: ModifyWorkspaceState -> TestTree
requestModifyWorkspaceState = req
    "ModifyWorkspaceState"
    "fixture/ModifyWorkspaceState.yaml"

requestCreateIPGroup :: CreateIPGroup -> TestTree
requestCreateIPGroup = req
    "CreateIPGroup"
    "fixture/CreateIPGroup.yaml"

requestCreateTags :: CreateTags -> TestTree
requestCreateTags = req
    "CreateTags"
    "fixture/CreateTags.yaml"

requestDeleteTags :: DeleteTags -> TestTree
requestDeleteTags = req
    "DeleteTags"
    "fixture/DeleteTags.yaml"

requestUpdateRulesOfIPGroup :: UpdateRulesOfIPGroup -> TestTree
requestUpdateRulesOfIPGroup = req
    "UpdateRulesOfIPGroup"
    "fixture/UpdateRulesOfIPGroup.yaml"

requestStopWorkspaces :: StopWorkspaces -> TestTree
requestStopWorkspaces = req
    "StopWorkspaces"
    "fixture/StopWorkspaces.yaml"

requestAssociateIPGroups :: AssociateIPGroups -> TestTree
requestAssociateIPGroups = req
    "AssociateIPGroups"
    "fixture/AssociateIPGroups.yaml"

requestDescribeWorkspacesConnectionStatus :: DescribeWorkspacesConnectionStatus -> TestTree
requestDescribeWorkspacesConnectionStatus = req
    "DescribeWorkspacesConnectionStatus"
    "fixture/DescribeWorkspacesConnectionStatus.yaml"

requestRebootWorkspaces :: RebootWorkspaces -> TestTree
requestRebootWorkspaces = req
    "RebootWorkspaces"
    "fixture/RebootWorkspaces.yaml"

requestDeleteIPGroup :: DeleteIPGroup -> TestTree
requestDeleteIPGroup = req
    "DeleteIPGroup"
    "fixture/DeleteIPGroup.yaml"

requestTerminateWorkspaces :: TerminateWorkspaces -> TestTree
requestTerminateWorkspaces = req
    "TerminateWorkspaces"
    "fixture/TerminateWorkspaces.yaml"

requestCreateWorkspaces :: CreateWorkspaces -> TestTree
requestCreateWorkspaces = req
    "CreateWorkspaces"
    "fixture/CreateWorkspaces.yaml"

requestDescribeIPGroups :: DescribeIPGroups -> TestTree
requestDescribeIPGroups = req
    "DescribeIPGroups"
    "fixture/DescribeIPGroups.yaml"

requestDescribeWorkspaces :: DescribeWorkspaces -> TestTree
requestDescribeWorkspaces = req
    "DescribeWorkspaces"
    "fixture/DescribeWorkspaces.yaml"

requestStartWorkspaces :: StartWorkspaces -> TestTree
requestStartWorkspaces = req
    "StartWorkspaces"
    "fixture/StartWorkspaces.yaml"

-- Responses

responseRevokeIPRules :: RevokeIPRulesResponse -> TestTree
responseRevokeIPRules = res
    "RevokeIPRulesResponse"
    "fixture/RevokeIPRulesResponse.proto"
    workSpaces
    (Proxy :: Proxy RevokeIPRules)

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

responseDisassociateIPGroups :: DisassociateIPGroupsResponse -> TestTree
responseDisassociateIPGroups = res
    "DisassociateIPGroupsResponse"
    "fixture/DisassociateIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy DisassociateIPGroups)

responseDescribeWorkspaceBundles :: DescribeWorkspaceBundlesResponse -> TestTree
responseDescribeWorkspaceBundles = res
    "DescribeWorkspaceBundlesResponse"
    "fixture/DescribeWorkspaceBundlesResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeWorkspaceBundles)

responseAuthorizeIPRules :: AuthorizeIPRulesResponse -> TestTree
responseAuthorizeIPRules = res
    "AuthorizeIPRulesResponse"
    "fixture/AuthorizeIPRulesResponse.proto"
    workSpaces
    (Proxy :: Proxy AuthorizeIPRules)

responseRebuildWorkspaces :: RebuildWorkspacesResponse -> TestTree
responseRebuildWorkspaces = res
    "RebuildWorkspacesResponse"
    "fixture/RebuildWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy RebuildWorkspaces)

responseModifyWorkspaceState :: ModifyWorkspaceStateResponse -> TestTree
responseModifyWorkspaceState = res
    "ModifyWorkspaceStateResponse"
    "fixture/ModifyWorkspaceStateResponse.proto"
    workSpaces
    (Proxy :: Proxy ModifyWorkspaceState)

responseCreateIPGroup :: CreateIPGroupResponse -> TestTree
responseCreateIPGroup = res
    "CreateIPGroupResponse"
    "fixture/CreateIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy CreateIPGroup)

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

responseUpdateRulesOfIPGroup :: UpdateRulesOfIPGroupResponse -> TestTree
responseUpdateRulesOfIPGroup = res
    "UpdateRulesOfIPGroupResponse"
    "fixture/UpdateRulesOfIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy UpdateRulesOfIPGroup)

responseStopWorkspaces :: StopWorkspacesResponse -> TestTree
responseStopWorkspaces = res
    "StopWorkspacesResponse"
    "fixture/StopWorkspacesResponse.proto"
    workSpaces
    (Proxy :: Proxy StopWorkspaces)

responseAssociateIPGroups :: AssociateIPGroupsResponse -> TestTree
responseAssociateIPGroups = res
    "AssociateIPGroupsResponse"
    "fixture/AssociateIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy AssociateIPGroups)

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

responseDeleteIPGroup :: DeleteIPGroupResponse -> TestTree
responseDeleteIPGroup = res
    "DeleteIPGroupResponse"
    "fixture/DeleteIPGroupResponse.proto"
    workSpaces
    (Proxy :: Proxy DeleteIPGroup)

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

responseDescribeIPGroups :: DescribeIPGroupsResponse -> TestTree
responseDescribeIPGroups = res
    "DescribeIPGroupsResponse"
    "fixture/DescribeIPGroupsResponse.proto"
    workSpaces
    (Proxy :: Proxy DescribeIPGroups)

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
