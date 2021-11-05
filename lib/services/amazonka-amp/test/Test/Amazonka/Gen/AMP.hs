{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AMP
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AMP where

import Amazonka.AMP
import qualified Data.Proxy as Proxy
import Test.Amazonka.AMP.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListWorkspaces $
--             newListWorkspaces
--
--         , requestCreateAlertManagerDefinition $
--             newCreateAlertManagerDefinition
--
--         , requestDeleteWorkspace $
--             newDeleteWorkspace
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateRuleGroupsNamespace $
--             newCreateRuleGroupsNamespace
--
--         , requestDescribeAlertManagerDefinition $
--             newDescribeAlertManagerDefinition
--
--         , requestDescribeWorkspace $
--             newDescribeWorkspace
--
--         , requestPutAlertManagerDefinition $
--             newPutAlertManagerDefinition
--
--         , requestDeleteAlertManagerDefinition $
--             newDeleteAlertManagerDefinition
--
--         , requestDescribeRuleGroupsNamespace $
--             newDescribeRuleGroupsNamespace
--
--         , requestUpdateWorkspaceAlias $
--             newUpdateWorkspaceAlias
--
--         , requestDeleteRuleGroupsNamespace $
--             newDeleteRuleGroupsNamespace
--
--         , requestPutRuleGroupsNamespace $
--             newPutRuleGroupsNamespace
--
--         , requestListRuleGroupsNamespaces $
--             newListRuleGroupsNamespaces
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateWorkspace $
--             newCreateWorkspace
--
--           ]

--     , testGroup "response"
--         [ responseListWorkspaces $
--             newListWorkspacesResponse
--
--         , responseCreateAlertManagerDefinition $
--             newCreateAlertManagerDefinitionResponse
--
--         , responseDeleteWorkspace $
--             newDeleteWorkspaceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateRuleGroupsNamespace $
--             newCreateRuleGroupsNamespaceResponse
--
--         , responseDescribeAlertManagerDefinition $
--             newDescribeAlertManagerDefinitionResponse
--
--         , responseDescribeWorkspace $
--             newDescribeWorkspaceResponse
--
--         , responsePutAlertManagerDefinition $
--             newPutAlertManagerDefinitionResponse
--
--         , responseDeleteAlertManagerDefinition $
--             newDeleteAlertManagerDefinitionResponse
--
--         , responseDescribeRuleGroupsNamespace $
--             newDescribeRuleGroupsNamespaceResponse
--
--         , responseUpdateWorkspaceAlias $
--             newUpdateWorkspaceAliasResponse
--
--         , responseDeleteRuleGroupsNamespace $
--             newDeleteRuleGroupsNamespaceResponse
--
--         , responsePutRuleGroupsNamespace $
--             newPutRuleGroupsNamespaceResponse
--
--         , responseListRuleGroupsNamespaces $
--             newListRuleGroupsNamespacesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateWorkspace $
--             newCreateWorkspaceResponse
--
--           ]
--     ]

-- Requests

requestListWorkspaces :: ListWorkspaces -> TestTree
requestListWorkspaces =
  req
    "ListWorkspaces"
    "fixture/ListWorkspaces.yaml"

requestCreateAlertManagerDefinition :: CreateAlertManagerDefinition -> TestTree
requestCreateAlertManagerDefinition =
  req
    "CreateAlertManagerDefinition"
    "fixture/CreateAlertManagerDefinition.yaml"

requestDeleteWorkspace :: DeleteWorkspace -> TestTree
requestDeleteWorkspace =
  req
    "DeleteWorkspace"
    "fixture/DeleteWorkspace.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateRuleGroupsNamespace :: CreateRuleGroupsNamespace -> TestTree
requestCreateRuleGroupsNamespace =
  req
    "CreateRuleGroupsNamespace"
    "fixture/CreateRuleGroupsNamespace.yaml"

requestDescribeAlertManagerDefinition :: DescribeAlertManagerDefinition -> TestTree
requestDescribeAlertManagerDefinition =
  req
    "DescribeAlertManagerDefinition"
    "fixture/DescribeAlertManagerDefinition.yaml"

requestDescribeWorkspace :: DescribeWorkspace -> TestTree
requestDescribeWorkspace =
  req
    "DescribeWorkspace"
    "fixture/DescribeWorkspace.yaml"

requestPutAlertManagerDefinition :: PutAlertManagerDefinition -> TestTree
requestPutAlertManagerDefinition =
  req
    "PutAlertManagerDefinition"
    "fixture/PutAlertManagerDefinition.yaml"

requestDeleteAlertManagerDefinition :: DeleteAlertManagerDefinition -> TestTree
requestDeleteAlertManagerDefinition =
  req
    "DeleteAlertManagerDefinition"
    "fixture/DeleteAlertManagerDefinition.yaml"

requestDescribeRuleGroupsNamespace :: DescribeRuleGroupsNamespace -> TestTree
requestDescribeRuleGroupsNamespace =
  req
    "DescribeRuleGroupsNamespace"
    "fixture/DescribeRuleGroupsNamespace.yaml"

requestUpdateWorkspaceAlias :: UpdateWorkspaceAlias -> TestTree
requestUpdateWorkspaceAlias =
  req
    "UpdateWorkspaceAlias"
    "fixture/UpdateWorkspaceAlias.yaml"

requestDeleteRuleGroupsNamespace :: DeleteRuleGroupsNamespace -> TestTree
requestDeleteRuleGroupsNamespace =
  req
    "DeleteRuleGroupsNamespace"
    "fixture/DeleteRuleGroupsNamespace.yaml"

requestPutRuleGroupsNamespace :: PutRuleGroupsNamespace -> TestTree
requestPutRuleGroupsNamespace =
  req
    "PutRuleGroupsNamespace"
    "fixture/PutRuleGroupsNamespace.yaml"

requestListRuleGroupsNamespaces :: ListRuleGroupsNamespaces -> TestTree
requestListRuleGroupsNamespaces =
  req
    "ListRuleGroupsNamespaces"
    "fixture/ListRuleGroupsNamespaces.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateWorkspace :: CreateWorkspace -> TestTree
requestCreateWorkspace =
  req
    "CreateWorkspace"
    "fixture/CreateWorkspace.yaml"

-- Responses

responseListWorkspaces :: ListWorkspacesResponse -> TestTree
responseListWorkspaces =
  res
    "ListWorkspacesResponse"
    "fixture/ListWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkspaces)

responseCreateAlertManagerDefinition :: CreateAlertManagerDefinitionResponse -> TestTree
responseCreateAlertManagerDefinition =
  res
    "CreateAlertManagerDefinitionResponse"
    "fixture/CreateAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlertManagerDefinition)

responseDeleteWorkspace :: DeleteWorkspaceResponse -> TestTree
responseDeleteWorkspace =
  res
    "DeleteWorkspaceResponse"
    "fixture/DeleteWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspace)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseCreateRuleGroupsNamespace :: CreateRuleGroupsNamespaceResponse -> TestTree
responseCreateRuleGroupsNamespace =
  res
    "CreateRuleGroupsNamespaceResponse"
    "fixture/CreateRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroupsNamespace)

responseDescribeAlertManagerDefinition :: DescribeAlertManagerDefinitionResponse -> TestTree
responseDescribeAlertManagerDefinition =
  res
    "DescribeAlertManagerDefinitionResponse"
    "fixture/DescribeAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlertManagerDefinition)

responseDescribeWorkspace :: DescribeWorkspaceResponse -> TestTree
responseDescribeWorkspace =
  res
    "DescribeWorkspaceResponse"
    "fixture/DescribeWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspace)

responsePutAlertManagerDefinition :: PutAlertManagerDefinitionResponse -> TestTree
responsePutAlertManagerDefinition =
  res
    "PutAlertManagerDefinitionResponse"
    "fixture/PutAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlertManagerDefinition)

responseDeleteAlertManagerDefinition :: DeleteAlertManagerDefinitionResponse -> TestTree
responseDeleteAlertManagerDefinition =
  res
    "DeleteAlertManagerDefinitionResponse"
    "fixture/DeleteAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlertManagerDefinition)

responseDescribeRuleGroupsNamespace :: DescribeRuleGroupsNamespaceResponse -> TestTree
responseDescribeRuleGroupsNamespace =
  res
    "DescribeRuleGroupsNamespaceResponse"
    "fixture/DescribeRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleGroupsNamespace)

responseUpdateWorkspaceAlias :: UpdateWorkspaceAliasResponse -> TestTree
responseUpdateWorkspaceAlias =
  res
    "UpdateWorkspaceAliasResponse"
    "fixture/UpdateWorkspaceAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceAlias)

responseDeleteRuleGroupsNamespace :: DeleteRuleGroupsNamespaceResponse -> TestTree
responseDeleteRuleGroupsNamespace =
  res
    "DeleteRuleGroupsNamespaceResponse"
    "fixture/DeleteRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroupsNamespace)

responsePutRuleGroupsNamespace :: PutRuleGroupsNamespaceResponse -> TestTree
responsePutRuleGroupsNamespace =
  res
    "PutRuleGroupsNamespaceResponse"
    "fixture/PutRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRuleGroupsNamespace)

responseListRuleGroupsNamespaces :: ListRuleGroupsNamespacesResponse -> TestTree
responseListRuleGroupsNamespaces =
  res
    "ListRuleGroupsNamespacesResponse"
    "fixture/ListRuleGroupsNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroupsNamespaces)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateWorkspace :: CreateWorkspaceResponse -> TestTree
responseCreateWorkspace =
  res
    "CreateWorkspaceResponse"
    "fixture/CreateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspace)
