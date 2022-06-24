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
--         [ requestCreateAlertManagerDefinition $
--             newCreateAlertManagerDefinition
--
--         , requestCreateRuleGroupsNamespace $
--             newCreateRuleGroupsNamespace
--
--         , requestCreateWorkspace $
--             newCreateWorkspace
--
--         , requestDeleteAlertManagerDefinition $
--             newDeleteAlertManagerDefinition
--
--         , requestDeleteRuleGroupsNamespace $
--             newDeleteRuleGroupsNamespace
--
--         , requestDeleteWorkspace $
--             newDeleteWorkspace
--
--         , requestDescribeAlertManagerDefinition $
--             newDescribeAlertManagerDefinition
--
--         , requestDescribeRuleGroupsNamespace $
--             newDescribeRuleGroupsNamespace
--
--         , requestDescribeWorkspace $
--             newDescribeWorkspace
--
--         , requestListRuleGroupsNamespaces $
--             newListRuleGroupsNamespaces
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListWorkspaces $
--             newListWorkspaces
--
--         , requestPutAlertManagerDefinition $
--             newPutAlertManagerDefinition
--
--         , requestPutRuleGroupsNamespace $
--             newPutRuleGroupsNamespace
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateWorkspaceAlias $
--             newUpdateWorkspaceAlias
--
--           ]

--     , testGroup "response"
--         [ responseCreateAlertManagerDefinition $
--             newCreateAlertManagerDefinitionResponse
--
--         , responseCreateRuleGroupsNamespace $
--             newCreateRuleGroupsNamespaceResponse
--
--         , responseCreateWorkspace $
--             newCreateWorkspaceResponse
--
--         , responseDeleteAlertManagerDefinition $
--             newDeleteAlertManagerDefinitionResponse
--
--         , responseDeleteRuleGroupsNamespace $
--             newDeleteRuleGroupsNamespaceResponse
--
--         , responseDeleteWorkspace $
--             newDeleteWorkspaceResponse
--
--         , responseDescribeAlertManagerDefinition $
--             newDescribeAlertManagerDefinitionResponse
--
--         , responseDescribeRuleGroupsNamespace $
--             newDescribeRuleGroupsNamespaceResponse
--
--         , responseDescribeWorkspace $
--             newDescribeWorkspaceResponse
--
--         , responseListRuleGroupsNamespaces $
--             newListRuleGroupsNamespacesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListWorkspaces $
--             newListWorkspacesResponse
--
--         , responsePutAlertManagerDefinition $
--             newPutAlertManagerDefinitionResponse
--
--         , responsePutRuleGroupsNamespace $
--             newPutRuleGroupsNamespaceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateWorkspaceAlias $
--             newUpdateWorkspaceAliasResponse
--
--           ]
--     ]

-- Requests

requestCreateAlertManagerDefinition :: CreateAlertManagerDefinition -> TestTree
requestCreateAlertManagerDefinition =
  req
    "CreateAlertManagerDefinition"
    "fixture/CreateAlertManagerDefinition.yaml"

requestCreateRuleGroupsNamespace :: CreateRuleGroupsNamespace -> TestTree
requestCreateRuleGroupsNamespace =
  req
    "CreateRuleGroupsNamespace"
    "fixture/CreateRuleGroupsNamespace.yaml"

requestCreateWorkspace :: CreateWorkspace -> TestTree
requestCreateWorkspace =
  req
    "CreateWorkspace"
    "fixture/CreateWorkspace.yaml"

requestDeleteAlertManagerDefinition :: DeleteAlertManagerDefinition -> TestTree
requestDeleteAlertManagerDefinition =
  req
    "DeleteAlertManagerDefinition"
    "fixture/DeleteAlertManagerDefinition.yaml"

requestDeleteRuleGroupsNamespace :: DeleteRuleGroupsNamespace -> TestTree
requestDeleteRuleGroupsNamespace =
  req
    "DeleteRuleGroupsNamespace"
    "fixture/DeleteRuleGroupsNamespace.yaml"

requestDeleteWorkspace :: DeleteWorkspace -> TestTree
requestDeleteWorkspace =
  req
    "DeleteWorkspace"
    "fixture/DeleteWorkspace.yaml"

requestDescribeAlertManagerDefinition :: DescribeAlertManagerDefinition -> TestTree
requestDescribeAlertManagerDefinition =
  req
    "DescribeAlertManagerDefinition"
    "fixture/DescribeAlertManagerDefinition.yaml"

requestDescribeRuleGroupsNamespace :: DescribeRuleGroupsNamespace -> TestTree
requestDescribeRuleGroupsNamespace =
  req
    "DescribeRuleGroupsNamespace"
    "fixture/DescribeRuleGroupsNamespace.yaml"

requestDescribeWorkspace :: DescribeWorkspace -> TestTree
requestDescribeWorkspace =
  req
    "DescribeWorkspace"
    "fixture/DescribeWorkspace.yaml"

requestListRuleGroupsNamespaces :: ListRuleGroupsNamespaces -> TestTree
requestListRuleGroupsNamespaces =
  req
    "ListRuleGroupsNamespaces"
    "fixture/ListRuleGroupsNamespaces.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListWorkspaces :: ListWorkspaces -> TestTree
requestListWorkspaces =
  req
    "ListWorkspaces"
    "fixture/ListWorkspaces.yaml"

requestPutAlertManagerDefinition :: PutAlertManagerDefinition -> TestTree
requestPutAlertManagerDefinition =
  req
    "PutAlertManagerDefinition"
    "fixture/PutAlertManagerDefinition.yaml"

requestPutRuleGroupsNamespace :: PutRuleGroupsNamespace -> TestTree
requestPutRuleGroupsNamespace =
  req
    "PutRuleGroupsNamespace"
    "fixture/PutRuleGroupsNamespace.yaml"

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

requestUpdateWorkspaceAlias :: UpdateWorkspaceAlias -> TestTree
requestUpdateWorkspaceAlias =
  req
    "UpdateWorkspaceAlias"
    "fixture/UpdateWorkspaceAlias.yaml"

-- Responses

responseCreateAlertManagerDefinition :: CreateAlertManagerDefinitionResponse -> TestTree
responseCreateAlertManagerDefinition =
  res
    "CreateAlertManagerDefinitionResponse"
    "fixture/CreateAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlertManagerDefinition)

responseCreateRuleGroupsNamespace :: CreateRuleGroupsNamespaceResponse -> TestTree
responseCreateRuleGroupsNamespace =
  res
    "CreateRuleGroupsNamespaceResponse"
    "fixture/CreateRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRuleGroupsNamespace)

responseCreateWorkspace :: CreateWorkspaceResponse -> TestTree
responseCreateWorkspace =
  res
    "CreateWorkspaceResponse"
    "fixture/CreateWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateWorkspace)

responseDeleteAlertManagerDefinition :: DeleteAlertManagerDefinitionResponse -> TestTree
responseDeleteAlertManagerDefinition =
  res
    "DeleteAlertManagerDefinitionResponse"
    "fixture/DeleteAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlertManagerDefinition)

responseDeleteRuleGroupsNamespace :: DeleteRuleGroupsNamespaceResponse -> TestTree
responseDeleteRuleGroupsNamespace =
  res
    "DeleteRuleGroupsNamespaceResponse"
    "fixture/DeleteRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRuleGroupsNamespace)

responseDeleteWorkspace :: DeleteWorkspaceResponse -> TestTree
responseDeleteWorkspace =
  res
    "DeleteWorkspaceResponse"
    "fixture/DeleteWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkspace)

responseDescribeAlertManagerDefinition :: DescribeAlertManagerDefinitionResponse -> TestTree
responseDescribeAlertManagerDefinition =
  res
    "DescribeAlertManagerDefinitionResponse"
    "fixture/DescribeAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAlertManagerDefinition)

responseDescribeRuleGroupsNamespace :: DescribeRuleGroupsNamespaceResponse -> TestTree
responseDescribeRuleGroupsNamespace =
  res
    "DescribeRuleGroupsNamespaceResponse"
    "fixture/DescribeRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRuleGroupsNamespace)

responseDescribeWorkspace :: DescribeWorkspaceResponse -> TestTree
responseDescribeWorkspace =
  res
    "DescribeWorkspaceResponse"
    "fixture/DescribeWorkspaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeWorkspace)

responseListRuleGroupsNamespaces :: ListRuleGroupsNamespacesResponse -> TestTree
responseListRuleGroupsNamespaces =
  res
    "ListRuleGroupsNamespacesResponse"
    "fixture/ListRuleGroupsNamespacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRuleGroupsNamespaces)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListWorkspaces :: ListWorkspacesResponse -> TestTree
responseListWorkspaces =
  res
    "ListWorkspacesResponse"
    "fixture/ListWorkspacesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkspaces)

responsePutAlertManagerDefinition :: PutAlertManagerDefinitionResponse -> TestTree
responsePutAlertManagerDefinition =
  res
    "PutAlertManagerDefinitionResponse"
    "fixture/PutAlertManagerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAlertManagerDefinition)

responsePutRuleGroupsNamespace :: PutRuleGroupsNamespaceResponse -> TestTree
responsePutRuleGroupsNamespace =
  res
    "PutRuleGroupsNamespaceResponse"
    "fixture/PutRuleGroupsNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRuleGroupsNamespace)

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

responseUpdateWorkspaceAlias :: UpdateWorkspaceAliasResponse -> TestTree
responseUpdateWorkspaceAlias =
  res
    "UpdateWorkspaceAliasResponse"
    "fixture/UpdateWorkspaceAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateWorkspaceAlias)
