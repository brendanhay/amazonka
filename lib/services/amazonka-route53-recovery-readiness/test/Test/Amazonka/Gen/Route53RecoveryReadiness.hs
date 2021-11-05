{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53RecoveryReadiness
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Route53RecoveryReadiness where

import Amazonka.Route53RecoveryReadiness
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.Route53RecoveryReadiness.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateResourceSet $
--             newCreateResourceSet
--
--         , requestGetReadinessCheckStatus $
--             newGetReadinessCheckStatus
--
--         , requestGetCellReadinessSummary $
--             newGetCellReadinessSummary
--
--         , requestUpdateCell $
--             newUpdateCell
--
--         , requestDeleteCell $
--             newDeleteCell
--
--         , requestUpdateReadinessCheck $
--             newUpdateReadinessCheck
--
--         , requestDeleteReadinessCheck $
--             newDeleteReadinessCheck
--
--         , requestListCells $
--             newListCells
--
--         , requestListReadinessChecks $
--             newListReadinessChecks
--
--         , requestListRules $
--             newListRules
--
--         , requestCreateReadinessCheck $
--             newCreateReadinessCheck
--
--         , requestCreateCell $
--             newCreateCell
--
--         , requestGetRecoveryGroup $
--             newGetRecoveryGroup
--
--         , requestListRecoveryGroups $
--             newListRecoveryGroups
--
--         , requestListCrossAccountAuthorizations $
--             newListCrossAccountAuthorizations
--
--         , requestGetCell $
--             newGetCell
--
--         , requestCreateCrossAccountAuthorization $
--             newCreateCrossAccountAuthorization
--
--         , requestCreateRecoveryGroup $
--             newCreateRecoveryGroup
--
--         , requestGetReadinessCheck $
--             newGetReadinessCheck
--
--         , requestGetReadinessCheckResourceStatus $
--             newGetReadinessCheckResourceStatus
--
--         , requestListResourceSets $
--             newListResourceSets
--
--         , requestGetArchitectureRecommendations $
--             newGetArchitectureRecommendations
--
--         , requestDeleteCrossAccountAuthorization $
--             newDeleteCrossAccountAuthorization
--
--         , requestDeleteRecoveryGroup $
--             newDeleteRecoveryGroup
--
--         , requestUpdateRecoveryGroup $
--             newUpdateRecoveryGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetRecoveryGroupReadinessSummary $
--             newGetRecoveryGroupReadinessSummary
--
--         , requestGetResourceSet $
--             newGetResourceSet
--
--         , requestListTagsForResources $
--             newListTagsForResources
--
--         , requestUpdateResourceSet $
--             newUpdateResourceSet
--
--         , requestDeleteResourceSet $
--             newDeleteResourceSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateResourceSet $
--             newCreateResourceSetResponse
--
--         , responseGetReadinessCheckStatus $
--             newGetReadinessCheckStatusResponse
--
--         , responseGetCellReadinessSummary $
--             newGetCellReadinessSummaryResponse
--
--         , responseUpdateCell $
--             newUpdateCellResponse
--
--         , responseDeleteCell $
--             newDeleteCellResponse
--
--         , responseUpdateReadinessCheck $
--             newUpdateReadinessCheckResponse
--
--         , responseDeleteReadinessCheck $
--             newDeleteReadinessCheckResponse
--
--         , responseListCells $
--             newListCellsResponse
--
--         , responseListReadinessChecks $
--             newListReadinessChecksResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseCreateReadinessCheck $
--             newCreateReadinessCheckResponse
--
--         , responseCreateCell $
--             newCreateCellResponse
--
--         , responseGetRecoveryGroup $
--             newGetRecoveryGroupResponse
--
--         , responseListRecoveryGroups $
--             newListRecoveryGroupsResponse
--
--         , responseListCrossAccountAuthorizations $
--             newListCrossAccountAuthorizationsResponse
--
--         , responseGetCell $
--             newGetCellResponse
--
--         , responseCreateCrossAccountAuthorization $
--             newCreateCrossAccountAuthorizationResponse
--
--         , responseCreateRecoveryGroup $
--             newCreateRecoveryGroupResponse
--
--         , responseGetReadinessCheck $
--             newGetReadinessCheckResponse
--
--         , responseGetReadinessCheckResourceStatus $
--             newGetReadinessCheckResourceStatusResponse
--
--         , responseListResourceSets $
--             newListResourceSetsResponse
--
--         , responseGetArchitectureRecommendations $
--             newGetArchitectureRecommendationsResponse
--
--         , responseDeleteCrossAccountAuthorization $
--             newDeleteCrossAccountAuthorizationResponse
--
--         , responseDeleteRecoveryGroup $
--             newDeleteRecoveryGroupResponse
--
--         , responseUpdateRecoveryGroup $
--             newUpdateRecoveryGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetRecoveryGroupReadinessSummary $
--             newGetRecoveryGroupReadinessSummaryResponse
--
--         , responseGetResourceSet $
--             newGetResourceSetResponse
--
--         , responseListTagsForResources $
--             newListTagsForResourcesResponse
--
--         , responseUpdateResourceSet $
--             newUpdateResourceSetResponse
--
--         , responseDeleteResourceSet $
--             newDeleteResourceSetResponse
--
--           ]
--     ]

-- Requests

requestCreateResourceSet :: CreateResourceSet -> TestTree
requestCreateResourceSet =
  req
    "CreateResourceSet"
    "fixture/CreateResourceSet.yaml"

requestGetReadinessCheckStatus :: GetReadinessCheckStatus -> TestTree
requestGetReadinessCheckStatus =
  req
    "GetReadinessCheckStatus"
    "fixture/GetReadinessCheckStatus.yaml"

requestGetCellReadinessSummary :: GetCellReadinessSummary -> TestTree
requestGetCellReadinessSummary =
  req
    "GetCellReadinessSummary"
    "fixture/GetCellReadinessSummary.yaml"

requestUpdateCell :: UpdateCell -> TestTree
requestUpdateCell =
  req
    "UpdateCell"
    "fixture/UpdateCell.yaml"

requestDeleteCell :: DeleteCell -> TestTree
requestDeleteCell =
  req
    "DeleteCell"
    "fixture/DeleteCell.yaml"

requestUpdateReadinessCheck :: UpdateReadinessCheck -> TestTree
requestUpdateReadinessCheck =
  req
    "UpdateReadinessCheck"
    "fixture/UpdateReadinessCheck.yaml"

requestDeleteReadinessCheck :: DeleteReadinessCheck -> TestTree
requestDeleteReadinessCheck =
  req
    "DeleteReadinessCheck"
    "fixture/DeleteReadinessCheck.yaml"

requestListCells :: ListCells -> TestTree
requestListCells =
  req
    "ListCells"
    "fixture/ListCells.yaml"

requestListReadinessChecks :: ListReadinessChecks -> TestTree
requestListReadinessChecks =
  req
    "ListReadinessChecks"
    "fixture/ListReadinessChecks.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestCreateReadinessCheck :: CreateReadinessCheck -> TestTree
requestCreateReadinessCheck =
  req
    "CreateReadinessCheck"
    "fixture/CreateReadinessCheck.yaml"

requestCreateCell :: CreateCell -> TestTree
requestCreateCell =
  req
    "CreateCell"
    "fixture/CreateCell.yaml"

requestGetRecoveryGroup :: GetRecoveryGroup -> TestTree
requestGetRecoveryGroup =
  req
    "GetRecoveryGroup"
    "fixture/GetRecoveryGroup.yaml"

requestListRecoveryGroups :: ListRecoveryGroups -> TestTree
requestListRecoveryGroups =
  req
    "ListRecoveryGroups"
    "fixture/ListRecoveryGroups.yaml"

requestListCrossAccountAuthorizations :: ListCrossAccountAuthorizations -> TestTree
requestListCrossAccountAuthorizations =
  req
    "ListCrossAccountAuthorizations"
    "fixture/ListCrossAccountAuthorizations.yaml"

requestGetCell :: GetCell -> TestTree
requestGetCell =
  req
    "GetCell"
    "fixture/GetCell.yaml"

requestCreateCrossAccountAuthorization :: CreateCrossAccountAuthorization -> TestTree
requestCreateCrossAccountAuthorization =
  req
    "CreateCrossAccountAuthorization"
    "fixture/CreateCrossAccountAuthorization.yaml"

requestCreateRecoveryGroup :: CreateRecoveryGroup -> TestTree
requestCreateRecoveryGroup =
  req
    "CreateRecoveryGroup"
    "fixture/CreateRecoveryGroup.yaml"

requestGetReadinessCheck :: GetReadinessCheck -> TestTree
requestGetReadinessCheck =
  req
    "GetReadinessCheck"
    "fixture/GetReadinessCheck.yaml"

requestGetReadinessCheckResourceStatus :: GetReadinessCheckResourceStatus -> TestTree
requestGetReadinessCheckResourceStatus =
  req
    "GetReadinessCheckResourceStatus"
    "fixture/GetReadinessCheckResourceStatus.yaml"

requestListResourceSets :: ListResourceSets -> TestTree
requestListResourceSets =
  req
    "ListResourceSets"
    "fixture/ListResourceSets.yaml"

requestGetArchitectureRecommendations :: GetArchitectureRecommendations -> TestTree
requestGetArchitectureRecommendations =
  req
    "GetArchitectureRecommendations"
    "fixture/GetArchitectureRecommendations.yaml"

requestDeleteCrossAccountAuthorization :: DeleteCrossAccountAuthorization -> TestTree
requestDeleteCrossAccountAuthorization =
  req
    "DeleteCrossAccountAuthorization"
    "fixture/DeleteCrossAccountAuthorization.yaml"

requestDeleteRecoveryGroup :: DeleteRecoveryGroup -> TestTree
requestDeleteRecoveryGroup =
  req
    "DeleteRecoveryGroup"
    "fixture/DeleteRecoveryGroup.yaml"

requestUpdateRecoveryGroup :: UpdateRecoveryGroup -> TestTree
requestUpdateRecoveryGroup =
  req
    "UpdateRecoveryGroup"
    "fixture/UpdateRecoveryGroup.yaml"

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

requestGetRecoveryGroupReadinessSummary :: GetRecoveryGroupReadinessSummary -> TestTree
requestGetRecoveryGroupReadinessSummary =
  req
    "GetRecoveryGroupReadinessSummary"
    "fixture/GetRecoveryGroupReadinessSummary.yaml"

requestGetResourceSet :: GetResourceSet -> TestTree
requestGetResourceSet =
  req
    "GetResourceSet"
    "fixture/GetResourceSet.yaml"

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources =
  req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

requestUpdateResourceSet :: UpdateResourceSet -> TestTree
requestUpdateResourceSet =
  req
    "UpdateResourceSet"
    "fixture/UpdateResourceSet.yaml"

requestDeleteResourceSet :: DeleteResourceSet -> TestTree
requestDeleteResourceSet =
  req
    "DeleteResourceSet"
    "fixture/DeleteResourceSet.yaml"

-- Responses

responseCreateResourceSet :: CreateResourceSetResponse -> TestTree
responseCreateResourceSet =
  res
    "CreateResourceSetResponse"
    "fixture/CreateResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceSet)

responseGetReadinessCheckStatus :: GetReadinessCheckStatusResponse -> TestTree
responseGetReadinessCheckStatus =
  res
    "GetReadinessCheckStatusResponse"
    "fixture/GetReadinessCheckStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadinessCheckStatus)

responseGetCellReadinessSummary :: GetCellReadinessSummaryResponse -> TestTree
responseGetCellReadinessSummary =
  res
    "GetCellReadinessSummaryResponse"
    "fixture/GetCellReadinessSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCellReadinessSummary)

responseUpdateCell :: UpdateCellResponse -> TestTree
responseUpdateCell =
  res
    "UpdateCellResponse"
    "fixture/UpdateCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCell)

responseDeleteCell :: DeleteCellResponse -> TestTree
responseDeleteCell =
  res
    "DeleteCellResponse"
    "fixture/DeleteCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCell)

responseUpdateReadinessCheck :: UpdateReadinessCheckResponse -> TestTree
responseUpdateReadinessCheck =
  res
    "UpdateReadinessCheckResponse"
    "fixture/UpdateReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReadinessCheck)

responseDeleteReadinessCheck :: DeleteReadinessCheckResponse -> TestTree
responseDeleteReadinessCheck =
  res
    "DeleteReadinessCheckResponse"
    "fixture/DeleteReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReadinessCheck)

responseListCells :: ListCellsResponse -> TestTree
responseListCells =
  res
    "ListCellsResponse"
    "fixture/ListCellsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCells)

responseListReadinessChecks :: ListReadinessChecksResponse -> TestTree
responseListReadinessChecks =
  res
    "ListReadinessChecksResponse"
    "fixture/ListReadinessChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadinessChecks)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseCreateReadinessCheck :: CreateReadinessCheckResponse -> TestTree
responseCreateReadinessCheck =
  res
    "CreateReadinessCheckResponse"
    "fixture/CreateReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReadinessCheck)

responseCreateCell :: CreateCellResponse -> TestTree
responseCreateCell =
  res
    "CreateCellResponse"
    "fixture/CreateCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCell)

responseGetRecoveryGroup :: GetRecoveryGroupResponse -> TestTree
responseGetRecoveryGroup =
  res
    "GetRecoveryGroupResponse"
    "fixture/GetRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryGroup)

responseListRecoveryGroups :: ListRecoveryGroupsResponse -> TestTree
responseListRecoveryGroups =
  res
    "ListRecoveryGroupsResponse"
    "fixture/ListRecoveryGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryGroups)

responseListCrossAccountAuthorizations :: ListCrossAccountAuthorizationsResponse -> TestTree
responseListCrossAccountAuthorizations =
  res
    "ListCrossAccountAuthorizationsResponse"
    "fixture/ListCrossAccountAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrossAccountAuthorizations)

responseGetCell :: GetCellResponse -> TestTree
responseGetCell =
  res
    "GetCellResponse"
    "fixture/GetCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCell)

responseCreateCrossAccountAuthorization :: CreateCrossAccountAuthorizationResponse -> TestTree
responseCreateCrossAccountAuthorization =
  res
    "CreateCrossAccountAuthorizationResponse"
    "fixture/CreateCrossAccountAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCrossAccountAuthorization)

responseCreateRecoveryGroup :: CreateRecoveryGroupResponse -> TestTree
responseCreateRecoveryGroup =
  res
    "CreateRecoveryGroupResponse"
    "fixture/CreateRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecoveryGroup)

responseGetReadinessCheck :: GetReadinessCheckResponse -> TestTree
responseGetReadinessCheck =
  res
    "GetReadinessCheckResponse"
    "fixture/GetReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadinessCheck)

responseGetReadinessCheckResourceStatus :: GetReadinessCheckResourceStatusResponse -> TestTree
responseGetReadinessCheckResourceStatus =
  res
    "GetReadinessCheckResourceStatusResponse"
    "fixture/GetReadinessCheckResourceStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadinessCheckResourceStatus)

responseListResourceSets :: ListResourceSetsResponse -> TestTree
responseListResourceSets =
  res
    "ListResourceSetsResponse"
    "fixture/ListResourceSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSets)

responseGetArchitectureRecommendations :: GetArchitectureRecommendationsResponse -> TestTree
responseGetArchitectureRecommendations =
  res
    "GetArchitectureRecommendationsResponse"
    "fixture/GetArchitectureRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetArchitectureRecommendations)

responseDeleteCrossAccountAuthorization :: DeleteCrossAccountAuthorizationResponse -> TestTree
responseDeleteCrossAccountAuthorization =
  res
    "DeleteCrossAccountAuthorizationResponse"
    "fixture/DeleteCrossAccountAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCrossAccountAuthorization)

responseDeleteRecoveryGroup :: DeleteRecoveryGroupResponse -> TestTree
responseDeleteRecoveryGroup =
  res
    "DeleteRecoveryGroupResponse"
    "fixture/DeleteRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecoveryGroup)

responseUpdateRecoveryGroup :: UpdateRecoveryGroupResponse -> TestTree
responseUpdateRecoveryGroup =
  res
    "UpdateRecoveryGroupResponse"
    "fixture/UpdateRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecoveryGroup)

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

responseGetRecoveryGroupReadinessSummary :: GetRecoveryGroupReadinessSummaryResponse -> TestTree
responseGetRecoveryGroupReadinessSummary =
  res
    "GetRecoveryGroupReadinessSummaryResponse"
    "fixture/GetRecoveryGroupReadinessSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryGroupReadinessSummary)

responseGetResourceSet :: GetResourceSetResponse -> TestTree
responseGetResourceSet =
  res
    "GetResourceSetResponse"
    "fixture/GetResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceSet)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResources)

responseUpdateResourceSet :: UpdateResourceSetResponse -> TestTree
responseUpdateResourceSet =
  res
    "UpdateResourceSetResponse"
    "fixture/UpdateResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceSet)

responseDeleteResourceSet :: DeleteResourceSetResponse -> TestTree
responseDeleteResourceSet =
  res
    "DeleteResourceSetResponse"
    "fixture/DeleteResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceSet)
