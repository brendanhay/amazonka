{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Route53RecoveryReadiness
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCreateCell $
--             newCreateCell
--
--         , requestCreateCrossAccountAuthorization $
--             newCreateCrossAccountAuthorization
--
--         , requestCreateReadinessCheck $
--             newCreateReadinessCheck
--
--         , requestCreateRecoveryGroup $
--             newCreateRecoveryGroup
--
--         , requestCreateResourceSet $
--             newCreateResourceSet
--
--         , requestDeleteCell $
--             newDeleteCell
--
--         , requestDeleteCrossAccountAuthorization $
--             newDeleteCrossAccountAuthorization
--
--         , requestDeleteReadinessCheck $
--             newDeleteReadinessCheck
--
--         , requestDeleteRecoveryGroup $
--             newDeleteRecoveryGroup
--
--         , requestDeleteResourceSet $
--             newDeleteResourceSet
--
--         , requestGetArchitectureRecommendations $
--             newGetArchitectureRecommendations
--
--         , requestGetCell $
--             newGetCell
--
--         , requestGetCellReadinessSummary $
--             newGetCellReadinessSummary
--
--         , requestGetReadinessCheck $
--             newGetReadinessCheck
--
--         , requestGetReadinessCheckResourceStatus $
--             newGetReadinessCheckResourceStatus
--
--         , requestGetReadinessCheckStatus $
--             newGetReadinessCheckStatus
--
--         , requestGetRecoveryGroup $
--             newGetRecoveryGroup
--
--         , requestGetRecoveryGroupReadinessSummary $
--             newGetRecoveryGroupReadinessSummary
--
--         , requestGetResourceSet $
--             newGetResourceSet
--
--         , requestListCells $
--             newListCells
--
--         , requestListCrossAccountAuthorizations $
--             newListCrossAccountAuthorizations
--
--         , requestListReadinessChecks $
--             newListReadinessChecks
--
--         , requestListRecoveryGroups $
--             newListRecoveryGroups
--
--         , requestListResourceSets $
--             newListResourceSets
--
--         , requestListRules $
--             newListRules
--
--         , requestListTagsForResources $
--             newListTagsForResources
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateCell $
--             newUpdateCell
--
--         , requestUpdateReadinessCheck $
--             newUpdateReadinessCheck
--
--         , requestUpdateRecoveryGroup $
--             newUpdateRecoveryGroup
--
--         , requestUpdateResourceSet $
--             newUpdateResourceSet
--
--           ]

--     , testGroup "response"
--         [ responseCreateCell $
--             newCreateCellResponse
--
--         , responseCreateCrossAccountAuthorization $
--             newCreateCrossAccountAuthorizationResponse
--
--         , responseCreateReadinessCheck $
--             newCreateReadinessCheckResponse
--
--         , responseCreateRecoveryGroup $
--             newCreateRecoveryGroupResponse
--
--         , responseCreateResourceSet $
--             newCreateResourceSetResponse
--
--         , responseDeleteCell $
--             newDeleteCellResponse
--
--         , responseDeleteCrossAccountAuthorization $
--             newDeleteCrossAccountAuthorizationResponse
--
--         , responseDeleteReadinessCheck $
--             newDeleteReadinessCheckResponse
--
--         , responseDeleteRecoveryGroup $
--             newDeleteRecoveryGroupResponse
--
--         , responseDeleteResourceSet $
--             newDeleteResourceSetResponse
--
--         , responseGetArchitectureRecommendations $
--             newGetArchitectureRecommendationsResponse
--
--         , responseGetCell $
--             newGetCellResponse
--
--         , responseGetCellReadinessSummary $
--             newGetCellReadinessSummaryResponse
--
--         , responseGetReadinessCheck $
--             newGetReadinessCheckResponse
--
--         , responseGetReadinessCheckResourceStatus $
--             newGetReadinessCheckResourceStatusResponse
--
--         , responseGetReadinessCheckStatus $
--             newGetReadinessCheckStatusResponse
--
--         , responseGetRecoveryGroup $
--             newGetRecoveryGroupResponse
--
--         , responseGetRecoveryGroupReadinessSummary $
--             newGetRecoveryGroupReadinessSummaryResponse
--
--         , responseGetResourceSet $
--             newGetResourceSetResponse
--
--         , responseListCells $
--             newListCellsResponse
--
--         , responseListCrossAccountAuthorizations $
--             newListCrossAccountAuthorizationsResponse
--
--         , responseListReadinessChecks $
--             newListReadinessChecksResponse
--
--         , responseListRecoveryGroups $
--             newListRecoveryGroupsResponse
--
--         , responseListResourceSets $
--             newListResourceSetsResponse
--
--         , responseListRules $
--             newListRulesResponse
--
--         , responseListTagsForResources $
--             newListTagsForResourcesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateCell $
--             newUpdateCellResponse
--
--         , responseUpdateReadinessCheck $
--             newUpdateReadinessCheckResponse
--
--         , responseUpdateRecoveryGroup $
--             newUpdateRecoveryGroupResponse
--
--         , responseUpdateResourceSet $
--             newUpdateResourceSetResponse
--
--           ]
--     ]

-- Requests

requestCreateCell :: CreateCell -> TestTree
requestCreateCell =
  req
    "CreateCell"
    "fixture/CreateCell.yaml"

requestCreateCrossAccountAuthorization :: CreateCrossAccountAuthorization -> TestTree
requestCreateCrossAccountAuthorization =
  req
    "CreateCrossAccountAuthorization"
    "fixture/CreateCrossAccountAuthorization.yaml"

requestCreateReadinessCheck :: CreateReadinessCheck -> TestTree
requestCreateReadinessCheck =
  req
    "CreateReadinessCheck"
    "fixture/CreateReadinessCheck.yaml"

requestCreateRecoveryGroup :: CreateRecoveryGroup -> TestTree
requestCreateRecoveryGroup =
  req
    "CreateRecoveryGroup"
    "fixture/CreateRecoveryGroup.yaml"

requestCreateResourceSet :: CreateResourceSet -> TestTree
requestCreateResourceSet =
  req
    "CreateResourceSet"
    "fixture/CreateResourceSet.yaml"

requestDeleteCell :: DeleteCell -> TestTree
requestDeleteCell =
  req
    "DeleteCell"
    "fixture/DeleteCell.yaml"

requestDeleteCrossAccountAuthorization :: DeleteCrossAccountAuthorization -> TestTree
requestDeleteCrossAccountAuthorization =
  req
    "DeleteCrossAccountAuthorization"
    "fixture/DeleteCrossAccountAuthorization.yaml"

requestDeleteReadinessCheck :: DeleteReadinessCheck -> TestTree
requestDeleteReadinessCheck =
  req
    "DeleteReadinessCheck"
    "fixture/DeleteReadinessCheck.yaml"

requestDeleteRecoveryGroup :: DeleteRecoveryGroup -> TestTree
requestDeleteRecoveryGroup =
  req
    "DeleteRecoveryGroup"
    "fixture/DeleteRecoveryGroup.yaml"

requestDeleteResourceSet :: DeleteResourceSet -> TestTree
requestDeleteResourceSet =
  req
    "DeleteResourceSet"
    "fixture/DeleteResourceSet.yaml"

requestGetArchitectureRecommendations :: GetArchitectureRecommendations -> TestTree
requestGetArchitectureRecommendations =
  req
    "GetArchitectureRecommendations"
    "fixture/GetArchitectureRecommendations.yaml"

requestGetCell :: GetCell -> TestTree
requestGetCell =
  req
    "GetCell"
    "fixture/GetCell.yaml"

requestGetCellReadinessSummary :: GetCellReadinessSummary -> TestTree
requestGetCellReadinessSummary =
  req
    "GetCellReadinessSummary"
    "fixture/GetCellReadinessSummary.yaml"

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

requestGetReadinessCheckStatus :: GetReadinessCheckStatus -> TestTree
requestGetReadinessCheckStatus =
  req
    "GetReadinessCheckStatus"
    "fixture/GetReadinessCheckStatus.yaml"

requestGetRecoveryGroup :: GetRecoveryGroup -> TestTree
requestGetRecoveryGroup =
  req
    "GetRecoveryGroup"
    "fixture/GetRecoveryGroup.yaml"

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

requestListCells :: ListCells -> TestTree
requestListCells =
  req
    "ListCells"
    "fixture/ListCells.yaml"

requestListCrossAccountAuthorizations :: ListCrossAccountAuthorizations -> TestTree
requestListCrossAccountAuthorizations =
  req
    "ListCrossAccountAuthorizations"
    "fixture/ListCrossAccountAuthorizations.yaml"

requestListReadinessChecks :: ListReadinessChecks -> TestTree
requestListReadinessChecks =
  req
    "ListReadinessChecks"
    "fixture/ListReadinessChecks.yaml"

requestListRecoveryGroups :: ListRecoveryGroups -> TestTree
requestListRecoveryGroups =
  req
    "ListRecoveryGroups"
    "fixture/ListRecoveryGroups.yaml"

requestListResourceSets :: ListResourceSets -> TestTree
requestListResourceSets =
  req
    "ListResourceSets"
    "fixture/ListResourceSets.yaml"

requestListRules :: ListRules -> TestTree
requestListRules =
  req
    "ListRules"
    "fixture/ListRules.yaml"

requestListTagsForResources :: ListTagsForResources -> TestTree
requestListTagsForResources =
  req
    "ListTagsForResources"
    "fixture/ListTagsForResources.yaml"

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

requestUpdateCell :: UpdateCell -> TestTree
requestUpdateCell =
  req
    "UpdateCell"
    "fixture/UpdateCell.yaml"

requestUpdateReadinessCheck :: UpdateReadinessCheck -> TestTree
requestUpdateReadinessCheck =
  req
    "UpdateReadinessCheck"
    "fixture/UpdateReadinessCheck.yaml"

requestUpdateRecoveryGroup :: UpdateRecoveryGroup -> TestTree
requestUpdateRecoveryGroup =
  req
    "UpdateRecoveryGroup"
    "fixture/UpdateRecoveryGroup.yaml"

requestUpdateResourceSet :: UpdateResourceSet -> TestTree
requestUpdateResourceSet =
  req
    "UpdateResourceSet"
    "fixture/UpdateResourceSet.yaml"

-- Responses

responseCreateCell :: CreateCellResponse -> TestTree
responseCreateCell =
  res
    "CreateCellResponse"
    "fixture/CreateCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCell)

responseCreateCrossAccountAuthorization :: CreateCrossAccountAuthorizationResponse -> TestTree
responseCreateCrossAccountAuthorization =
  res
    "CreateCrossAccountAuthorizationResponse"
    "fixture/CreateCrossAccountAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCrossAccountAuthorization)

responseCreateReadinessCheck :: CreateReadinessCheckResponse -> TestTree
responseCreateReadinessCheck =
  res
    "CreateReadinessCheckResponse"
    "fixture/CreateReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateReadinessCheck)

responseCreateRecoveryGroup :: CreateRecoveryGroupResponse -> TestTree
responseCreateRecoveryGroup =
  res
    "CreateRecoveryGroupResponse"
    "fixture/CreateRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateRecoveryGroup)

responseCreateResourceSet :: CreateResourceSetResponse -> TestTree
responseCreateResourceSet =
  res
    "CreateResourceSetResponse"
    "fixture/CreateResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceSet)

responseDeleteCell :: DeleteCellResponse -> TestTree
responseDeleteCell =
  res
    "DeleteCellResponse"
    "fixture/DeleteCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCell)

responseDeleteCrossAccountAuthorization :: DeleteCrossAccountAuthorizationResponse -> TestTree
responseDeleteCrossAccountAuthorization =
  res
    "DeleteCrossAccountAuthorizationResponse"
    "fixture/DeleteCrossAccountAuthorizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCrossAccountAuthorization)

responseDeleteReadinessCheck :: DeleteReadinessCheckResponse -> TestTree
responseDeleteReadinessCheck =
  res
    "DeleteReadinessCheckResponse"
    "fixture/DeleteReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteReadinessCheck)

responseDeleteRecoveryGroup :: DeleteRecoveryGroupResponse -> TestTree
responseDeleteRecoveryGroup =
  res
    "DeleteRecoveryGroupResponse"
    "fixture/DeleteRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRecoveryGroup)

responseDeleteResourceSet :: DeleteResourceSetResponse -> TestTree
responseDeleteResourceSet =
  res
    "DeleteResourceSetResponse"
    "fixture/DeleteResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceSet)

responseGetArchitectureRecommendations :: GetArchitectureRecommendationsResponse -> TestTree
responseGetArchitectureRecommendations =
  res
    "GetArchitectureRecommendationsResponse"
    "fixture/GetArchitectureRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetArchitectureRecommendations)

responseGetCell :: GetCellResponse -> TestTree
responseGetCell =
  res
    "GetCellResponse"
    "fixture/GetCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCell)

responseGetCellReadinessSummary :: GetCellReadinessSummaryResponse -> TestTree
responseGetCellReadinessSummary =
  res
    "GetCellReadinessSummaryResponse"
    "fixture/GetCellReadinessSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCellReadinessSummary)

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

responseGetReadinessCheckStatus :: GetReadinessCheckStatusResponse -> TestTree
responseGetReadinessCheckStatus =
  res
    "GetReadinessCheckStatusResponse"
    "fixture/GetReadinessCheckStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetReadinessCheckStatus)

responseGetRecoveryGroup :: GetRecoveryGroupResponse -> TestTree
responseGetRecoveryGroup =
  res
    "GetRecoveryGroupResponse"
    "fixture/GetRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecoveryGroup)

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

responseListCells :: ListCellsResponse -> TestTree
responseListCells =
  res
    "ListCellsResponse"
    "fixture/ListCellsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCells)

responseListCrossAccountAuthorizations :: ListCrossAccountAuthorizationsResponse -> TestTree
responseListCrossAccountAuthorizations =
  res
    "ListCrossAccountAuthorizationsResponse"
    "fixture/ListCrossAccountAuthorizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCrossAccountAuthorizations)

responseListReadinessChecks :: ListReadinessChecksResponse -> TestTree
responseListReadinessChecks =
  res
    "ListReadinessChecksResponse"
    "fixture/ListReadinessChecksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListReadinessChecks)

responseListRecoveryGroups :: ListRecoveryGroupsResponse -> TestTree
responseListRecoveryGroups =
  res
    "ListRecoveryGroupsResponse"
    "fixture/ListRecoveryGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecoveryGroups)

responseListResourceSets :: ListResourceSetsResponse -> TestTree
responseListResourceSets =
  res
    "ListResourceSetsResponse"
    "fixture/ListResourceSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSets)

responseListRules :: ListRulesResponse -> TestTree
responseListRules =
  res
    "ListRulesResponse"
    "fixture/ListRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRules)

responseListTagsForResources :: ListTagsForResourcesResponse -> TestTree
responseListTagsForResources =
  res
    "ListTagsForResourcesResponse"
    "fixture/ListTagsForResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResources)

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

responseUpdateCell :: UpdateCellResponse -> TestTree
responseUpdateCell =
  res
    "UpdateCellResponse"
    "fixture/UpdateCellResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCell)

responseUpdateReadinessCheck :: UpdateReadinessCheckResponse -> TestTree
responseUpdateReadinessCheck =
  res
    "UpdateReadinessCheckResponse"
    "fixture/UpdateReadinessCheckResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateReadinessCheck)

responseUpdateRecoveryGroup :: UpdateRecoveryGroupResponse -> TestTree
responseUpdateRecoveryGroup =
  res
    "UpdateRecoveryGroupResponse"
    "fixture/UpdateRecoveryGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecoveryGroup)

responseUpdateResourceSet :: UpdateResourceSetResponse -> TestTree
responseUpdateResourceSet =
  res
    "UpdateResourceSetResponse"
    "fixture/UpdateResourceSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceSet)
