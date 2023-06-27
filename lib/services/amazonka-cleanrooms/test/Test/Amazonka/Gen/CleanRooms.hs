{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CleanRooms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CleanRooms where

import Amazonka.CleanRooms
import qualified Data.Proxy as Proxy
import Test.Amazonka.CleanRooms.Internal
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
--         [ requestBatchGetSchema $
--             newBatchGetSchema
--
--         , requestCreateCollaboration $
--             newCreateCollaboration
--
--         , requestCreateConfiguredTable $
--             newCreateConfiguredTable
--
--         , requestCreateConfiguredTableAnalysisRule $
--             newCreateConfiguredTableAnalysisRule
--
--         , requestCreateConfiguredTableAssociation $
--             newCreateConfiguredTableAssociation
--
--         , requestCreateMembership $
--             newCreateMembership
--
--         , requestDeleteCollaboration $
--             newDeleteCollaboration
--
--         , requestDeleteConfiguredTable $
--             newDeleteConfiguredTable
--
--         , requestDeleteConfiguredTableAnalysisRule $
--             newDeleteConfiguredTableAnalysisRule
--
--         , requestDeleteConfiguredTableAssociation $
--             newDeleteConfiguredTableAssociation
--
--         , requestDeleteMember $
--             newDeleteMember
--
--         , requestDeleteMembership $
--             newDeleteMembership
--
--         , requestGetCollaboration $
--             newGetCollaboration
--
--         , requestGetConfiguredTable $
--             newGetConfiguredTable
--
--         , requestGetConfiguredTableAnalysisRule $
--             newGetConfiguredTableAnalysisRule
--
--         , requestGetConfiguredTableAssociation $
--             newGetConfiguredTableAssociation
--
--         , requestGetMembership $
--             newGetMembership
--
--         , requestGetProtectedQuery $
--             newGetProtectedQuery
--
--         , requestGetSchema $
--             newGetSchema
--
--         , requestGetSchemaAnalysisRule $
--             newGetSchemaAnalysisRule
--
--         , requestListCollaborations $
--             newListCollaborations
--
--         , requestListConfiguredTableAssociations $
--             newListConfiguredTableAssociations
--
--         , requestListConfiguredTables $
--             newListConfiguredTables
--
--         , requestListMembers $
--             newListMembers
--
--         , requestListMemberships $
--             newListMemberships
--
--         , requestListProtectedQueries $
--             newListProtectedQueries
--
--         , requestListSchemas $
--             newListSchemas
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartProtectedQuery $
--             newStartProtectedQuery
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCollaboration $
--             newUpdateCollaboration
--
--         , requestUpdateConfiguredTable $
--             newUpdateConfiguredTable
--
--         , requestUpdateConfiguredTableAnalysisRule $
--             newUpdateConfiguredTableAnalysisRule
--
--         , requestUpdateConfiguredTableAssociation $
--             newUpdateConfiguredTableAssociation
--
--         , requestUpdateMembership $
--             newUpdateMembership
--
--         , requestUpdateProtectedQuery $
--             newUpdateProtectedQuery
--
--           ]

--     , testGroup "response"
--         [ responseBatchGetSchema $
--             newBatchGetSchemaResponse
--
--         , responseCreateCollaboration $
--             newCreateCollaborationResponse
--
--         , responseCreateConfiguredTable $
--             newCreateConfiguredTableResponse
--
--         , responseCreateConfiguredTableAnalysisRule $
--             newCreateConfiguredTableAnalysisRuleResponse
--
--         , responseCreateConfiguredTableAssociation $
--             newCreateConfiguredTableAssociationResponse
--
--         , responseCreateMembership $
--             newCreateMembershipResponse
--
--         , responseDeleteCollaboration $
--             newDeleteCollaborationResponse
--
--         , responseDeleteConfiguredTable $
--             newDeleteConfiguredTableResponse
--
--         , responseDeleteConfiguredTableAnalysisRule $
--             newDeleteConfiguredTableAnalysisRuleResponse
--
--         , responseDeleteConfiguredTableAssociation $
--             newDeleteConfiguredTableAssociationResponse
--
--         , responseDeleteMember $
--             newDeleteMemberResponse
--
--         , responseDeleteMembership $
--             newDeleteMembershipResponse
--
--         , responseGetCollaboration $
--             newGetCollaborationResponse
--
--         , responseGetConfiguredTable $
--             newGetConfiguredTableResponse
--
--         , responseGetConfiguredTableAnalysisRule $
--             newGetConfiguredTableAnalysisRuleResponse
--
--         , responseGetConfiguredTableAssociation $
--             newGetConfiguredTableAssociationResponse
--
--         , responseGetMembership $
--             newGetMembershipResponse
--
--         , responseGetProtectedQuery $
--             newGetProtectedQueryResponse
--
--         , responseGetSchema $
--             newGetSchemaResponse
--
--         , responseGetSchemaAnalysisRule $
--             newGetSchemaAnalysisRuleResponse
--
--         , responseListCollaborations $
--             newListCollaborationsResponse
--
--         , responseListConfiguredTableAssociations $
--             newListConfiguredTableAssociationsResponse
--
--         , responseListConfiguredTables $
--             newListConfiguredTablesResponse
--
--         , responseListMembers $
--             newListMembersResponse
--
--         , responseListMemberships $
--             newListMembershipsResponse
--
--         , responseListProtectedQueries $
--             newListProtectedQueriesResponse
--
--         , responseListSchemas $
--             newListSchemasResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartProtectedQuery $
--             newStartProtectedQueryResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCollaboration $
--             newUpdateCollaborationResponse
--
--         , responseUpdateConfiguredTable $
--             newUpdateConfiguredTableResponse
--
--         , responseUpdateConfiguredTableAnalysisRule $
--             newUpdateConfiguredTableAnalysisRuleResponse
--
--         , responseUpdateConfiguredTableAssociation $
--             newUpdateConfiguredTableAssociationResponse
--
--         , responseUpdateMembership $
--             newUpdateMembershipResponse
--
--         , responseUpdateProtectedQuery $
--             newUpdateProtectedQueryResponse
--
--           ]
--     ]

-- Requests

requestBatchGetSchema :: BatchGetSchema -> TestTree
requestBatchGetSchema =
  req
    "BatchGetSchema"
    "fixture/BatchGetSchema.yaml"

requestCreateCollaboration :: CreateCollaboration -> TestTree
requestCreateCollaboration =
  req
    "CreateCollaboration"
    "fixture/CreateCollaboration.yaml"

requestCreateConfiguredTable :: CreateConfiguredTable -> TestTree
requestCreateConfiguredTable =
  req
    "CreateConfiguredTable"
    "fixture/CreateConfiguredTable.yaml"

requestCreateConfiguredTableAnalysisRule :: CreateConfiguredTableAnalysisRule -> TestTree
requestCreateConfiguredTableAnalysisRule =
  req
    "CreateConfiguredTableAnalysisRule"
    "fixture/CreateConfiguredTableAnalysisRule.yaml"

requestCreateConfiguredTableAssociation :: CreateConfiguredTableAssociation -> TestTree
requestCreateConfiguredTableAssociation =
  req
    "CreateConfiguredTableAssociation"
    "fixture/CreateConfiguredTableAssociation.yaml"

requestCreateMembership :: CreateMembership -> TestTree
requestCreateMembership =
  req
    "CreateMembership"
    "fixture/CreateMembership.yaml"

requestDeleteCollaboration :: DeleteCollaboration -> TestTree
requestDeleteCollaboration =
  req
    "DeleteCollaboration"
    "fixture/DeleteCollaboration.yaml"

requestDeleteConfiguredTable :: DeleteConfiguredTable -> TestTree
requestDeleteConfiguredTable =
  req
    "DeleteConfiguredTable"
    "fixture/DeleteConfiguredTable.yaml"

requestDeleteConfiguredTableAnalysisRule :: DeleteConfiguredTableAnalysisRule -> TestTree
requestDeleteConfiguredTableAnalysisRule =
  req
    "DeleteConfiguredTableAnalysisRule"
    "fixture/DeleteConfiguredTableAnalysisRule.yaml"

requestDeleteConfiguredTableAssociation :: DeleteConfiguredTableAssociation -> TestTree
requestDeleteConfiguredTableAssociation =
  req
    "DeleteConfiguredTableAssociation"
    "fixture/DeleteConfiguredTableAssociation.yaml"

requestDeleteMember :: DeleteMember -> TestTree
requestDeleteMember =
  req
    "DeleteMember"
    "fixture/DeleteMember.yaml"

requestDeleteMembership :: DeleteMembership -> TestTree
requestDeleteMembership =
  req
    "DeleteMembership"
    "fixture/DeleteMembership.yaml"

requestGetCollaboration :: GetCollaboration -> TestTree
requestGetCollaboration =
  req
    "GetCollaboration"
    "fixture/GetCollaboration.yaml"

requestGetConfiguredTable :: GetConfiguredTable -> TestTree
requestGetConfiguredTable =
  req
    "GetConfiguredTable"
    "fixture/GetConfiguredTable.yaml"

requestGetConfiguredTableAnalysisRule :: GetConfiguredTableAnalysisRule -> TestTree
requestGetConfiguredTableAnalysisRule =
  req
    "GetConfiguredTableAnalysisRule"
    "fixture/GetConfiguredTableAnalysisRule.yaml"

requestGetConfiguredTableAssociation :: GetConfiguredTableAssociation -> TestTree
requestGetConfiguredTableAssociation =
  req
    "GetConfiguredTableAssociation"
    "fixture/GetConfiguredTableAssociation.yaml"

requestGetMembership :: GetMembership -> TestTree
requestGetMembership =
  req
    "GetMembership"
    "fixture/GetMembership.yaml"

requestGetProtectedQuery :: GetProtectedQuery -> TestTree
requestGetProtectedQuery =
  req
    "GetProtectedQuery"
    "fixture/GetProtectedQuery.yaml"

requestGetSchema :: GetSchema -> TestTree
requestGetSchema =
  req
    "GetSchema"
    "fixture/GetSchema.yaml"

requestGetSchemaAnalysisRule :: GetSchemaAnalysisRule -> TestTree
requestGetSchemaAnalysisRule =
  req
    "GetSchemaAnalysisRule"
    "fixture/GetSchemaAnalysisRule.yaml"

requestListCollaborations :: ListCollaborations -> TestTree
requestListCollaborations =
  req
    "ListCollaborations"
    "fixture/ListCollaborations.yaml"

requestListConfiguredTableAssociations :: ListConfiguredTableAssociations -> TestTree
requestListConfiguredTableAssociations =
  req
    "ListConfiguredTableAssociations"
    "fixture/ListConfiguredTableAssociations.yaml"

requestListConfiguredTables :: ListConfiguredTables -> TestTree
requestListConfiguredTables =
  req
    "ListConfiguredTables"
    "fixture/ListConfiguredTables.yaml"

requestListMembers :: ListMembers -> TestTree
requestListMembers =
  req
    "ListMembers"
    "fixture/ListMembers.yaml"

requestListMemberships :: ListMemberships -> TestTree
requestListMemberships =
  req
    "ListMemberships"
    "fixture/ListMemberships.yaml"

requestListProtectedQueries :: ListProtectedQueries -> TestTree
requestListProtectedQueries =
  req
    "ListProtectedQueries"
    "fixture/ListProtectedQueries.yaml"

requestListSchemas :: ListSchemas -> TestTree
requestListSchemas =
  req
    "ListSchemas"
    "fixture/ListSchemas.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartProtectedQuery :: StartProtectedQuery -> TestTree
requestStartProtectedQuery =
  req
    "StartProtectedQuery"
    "fixture/StartProtectedQuery.yaml"

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

requestUpdateCollaboration :: UpdateCollaboration -> TestTree
requestUpdateCollaboration =
  req
    "UpdateCollaboration"
    "fixture/UpdateCollaboration.yaml"

requestUpdateConfiguredTable :: UpdateConfiguredTable -> TestTree
requestUpdateConfiguredTable =
  req
    "UpdateConfiguredTable"
    "fixture/UpdateConfiguredTable.yaml"

requestUpdateConfiguredTableAnalysisRule :: UpdateConfiguredTableAnalysisRule -> TestTree
requestUpdateConfiguredTableAnalysisRule =
  req
    "UpdateConfiguredTableAnalysisRule"
    "fixture/UpdateConfiguredTableAnalysisRule.yaml"

requestUpdateConfiguredTableAssociation :: UpdateConfiguredTableAssociation -> TestTree
requestUpdateConfiguredTableAssociation =
  req
    "UpdateConfiguredTableAssociation"
    "fixture/UpdateConfiguredTableAssociation.yaml"

requestUpdateMembership :: UpdateMembership -> TestTree
requestUpdateMembership =
  req
    "UpdateMembership"
    "fixture/UpdateMembership.yaml"

requestUpdateProtectedQuery :: UpdateProtectedQuery -> TestTree
requestUpdateProtectedQuery =
  req
    "UpdateProtectedQuery"
    "fixture/UpdateProtectedQuery.yaml"

-- Responses

responseBatchGetSchema :: BatchGetSchemaResponse -> TestTree
responseBatchGetSchema =
  res
    "BatchGetSchemaResponse"
    "fixture/BatchGetSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchGetSchema)

responseCreateCollaboration :: CreateCollaborationResponse -> TestTree
responseCreateCollaboration =
  res
    "CreateCollaborationResponse"
    "fixture/CreateCollaborationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCollaboration)

responseCreateConfiguredTable :: CreateConfiguredTableResponse -> TestTree
responseCreateConfiguredTable =
  res
    "CreateConfiguredTableResponse"
    "fixture/CreateConfiguredTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguredTable)

responseCreateConfiguredTableAnalysisRule :: CreateConfiguredTableAnalysisRuleResponse -> TestTree
responseCreateConfiguredTableAnalysisRule =
  res
    "CreateConfiguredTableAnalysisRuleResponse"
    "fixture/CreateConfiguredTableAnalysisRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguredTableAnalysisRule)

responseCreateConfiguredTableAssociation :: CreateConfiguredTableAssociationResponse -> TestTree
responseCreateConfiguredTableAssociation =
  res
    "CreateConfiguredTableAssociationResponse"
    "fixture/CreateConfiguredTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfiguredTableAssociation)

responseCreateMembership :: CreateMembershipResponse -> TestTree
responseCreateMembership =
  res
    "CreateMembershipResponse"
    "fixture/CreateMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMembership)

responseDeleteCollaboration :: DeleteCollaborationResponse -> TestTree
responseDeleteCollaboration =
  res
    "DeleteCollaborationResponse"
    "fixture/DeleteCollaborationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCollaboration)

responseDeleteConfiguredTable :: DeleteConfiguredTableResponse -> TestTree
responseDeleteConfiguredTable =
  res
    "DeleteConfiguredTableResponse"
    "fixture/DeleteConfiguredTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfiguredTable)

responseDeleteConfiguredTableAnalysisRule :: DeleteConfiguredTableAnalysisRuleResponse -> TestTree
responseDeleteConfiguredTableAnalysisRule =
  res
    "DeleteConfiguredTableAnalysisRuleResponse"
    "fixture/DeleteConfiguredTableAnalysisRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfiguredTableAnalysisRule)

responseDeleteConfiguredTableAssociation :: DeleteConfiguredTableAssociationResponse -> TestTree
responseDeleteConfiguredTableAssociation =
  res
    "DeleteConfiguredTableAssociationResponse"
    "fixture/DeleteConfiguredTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfiguredTableAssociation)

responseDeleteMember :: DeleteMemberResponse -> TestTree
responseDeleteMember =
  res
    "DeleteMemberResponse"
    "fixture/DeleteMemberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMember)

responseDeleteMembership :: DeleteMembershipResponse -> TestTree
responseDeleteMembership =
  res
    "DeleteMembershipResponse"
    "fixture/DeleteMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMembership)

responseGetCollaboration :: GetCollaborationResponse -> TestTree
responseGetCollaboration =
  res
    "GetCollaborationResponse"
    "fixture/GetCollaborationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCollaboration)

responseGetConfiguredTable :: GetConfiguredTableResponse -> TestTree
responseGetConfiguredTable =
  res
    "GetConfiguredTableResponse"
    "fixture/GetConfiguredTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfiguredTable)

responseGetConfiguredTableAnalysisRule :: GetConfiguredTableAnalysisRuleResponse -> TestTree
responseGetConfiguredTableAnalysisRule =
  res
    "GetConfiguredTableAnalysisRuleResponse"
    "fixture/GetConfiguredTableAnalysisRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfiguredTableAnalysisRule)

responseGetConfiguredTableAssociation :: GetConfiguredTableAssociationResponse -> TestTree
responseGetConfiguredTableAssociation =
  res
    "GetConfiguredTableAssociationResponse"
    "fixture/GetConfiguredTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfiguredTableAssociation)

responseGetMembership :: GetMembershipResponse -> TestTree
responseGetMembership =
  res
    "GetMembershipResponse"
    "fixture/GetMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMembership)

responseGetProtectedQuery :: GetProtectedQueryResponse -> TestTree
responseGetProtectedQuery =
  res
    "GetProtectedQueryResponse"
    "fixture/GetProtectedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProtectedQuery)

responseGetSchema :: GetSchemaResponse -> TestTree
responseGetSchema =
  res
    "GetSchemaResponse"
    "fixture/GetSchemaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchema)

responseGetSchemaAnalysisRule :: GetSchemaAnalysisRuleResponse -> TestTree
responseGetSchemaAnalysisRule =
  res
    "GetSchemaAnalysisRuleResponse"
    "fixture/GetSchemaAnalysisRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSchemaAnalysisRule)

responseListCollaborations :: ListCollaborationsResponse -> TestTree
responseListCollaborations =
  res
    "ListCollaborationsResponse"
    "fixture/ListCollaborationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCollaborations)

responseListConfiguredTableAssociations :: ListConfiguredTableAssociationsResponse -> TestTree
responseListConfiguredTableAssociations =
  res
    "ListConfiguredTableAssociationsResponse"
    "fixture/ListConfiguredTableAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfiguredTableAssociations)

responseListConfiguredTables :: ListConfiguredTablesResponse -> TestTree
responseListConfiguredTables =
  res
    "ListConfiguredTablesResponse"
    "fixture/ListConfiguredTablesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConfiguredTables)

responseListMembers :: ListMembersResponse -> TestTree
responseListMembers =
  res
    "ListMembersResponse"
    "fixture/ListMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMembers)

responseListMemberships :: ListMembershipsResponse -> TestTree
responseListMemberships =
  res
    "ListMembershipsResponse"
    "fixture/ListMembershipsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMemberships)

responseListProtectedQueries :: ListProtectedQueriesResponse -> TestTree
responseListProtectedQueries =
  res
    "ListProtectedQueriesResponse"
    "fixture/ListProtectedQueriesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProtectedQueries)

responseListSchemas :: ListSchemasResponse -> TestTree
responseListSchemas =
  res
    "ListSchemasResponse"
    "fixture/ListSchemasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSchemas)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartProtectedQuery :: StartProtectedQueryResponse -> TestTree
responseStartProtectedQuery =
  res
    "StartProtectedQueryResponse"
    "fixture/StartProtectedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartProtectedQuery)

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

responseUpdateCollaboration :: UpdateCollaborationResponse -> TestTree
responseUpdateCollaboration =
  res
    "UpdateCollaborationResponse"
    "fixture/UpdateCollaborationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCollaboration)

responseUpdateConfiguredTable :: UpdateConfiguredTableResponse -> TestTree
responseUpdateConfiguredTable =
  res
    "UpdateConfiguredTableResponse"
    "fixture/UpdateConfiguredTableResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguredTable)

responseUpdateConfiguredTableAnalysisRule :: UpdateConfiguredTableAnalysisRuleResponse -> TestTree
responseUpdateConfiguredTableAnalysisRule =
  res
    "UpdateConfiguredTableAnalysisRuleResponse"
    "fixture/UpdateConfiguredTableAnalysisRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguredTableAnalysisRule)

responseUpdateConfiguredTableAssociation :: UpdateConfiguredTableAssociationResponse -> TestTree
responseUpdateConfiguredTableAssociation =
  res
    "UpdateConfiguredTableAssociationResponse"
    "fixture/UpdateConfiguredTableAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfiguredTableAssociation)

responseUpdateMembership :: UpdateMembershipResponse -> TestTree
responseUpdateMembership =
  res
    "UpdateMembershipResponse"
    "fixture/UpdateMembershipResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMembership)

responseUpdateProtectedQuery :: UpdateProtectedQueryResponse -> TestTree
responseUpdateProtectedQuery =
  res
    "UpdateProtectedQueryResponse"
    "fixture/UpdateProtectedQueryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProtectedQuery)
