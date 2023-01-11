{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LexV2Models
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LexV2Models where

import Amazonka.LexV2Models
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LexV2Models.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBatchCreateCustomVocabularyItem $
--             newBatchCreateCustomVocabularyItem
--
--         , requestBatchDeleteCustomVocabularyItem $
--             newBatchDeleteCustomVocabularyItem
--
--         , requestBatchUpdateCustomVocabularyItem $
--             newBatchUpdateCustomVocabularyItem
--
--         , requestBuildBotLocale $
--             newBuildBotLocale
--
--         , requestCreateBot $
--             newCreateBot
--
--         , requestCreateBotAlias $
--             newCreateBotAlias
--
--         , requestCreateBotLocale $
--             newCreateBotLocale
--
--         , requestCreateBotVersion $
--             newCreateBotVersion
--
--         , requestCreateExport $
--             newCreateExport
--
--         , requestCreateIntent $
--             newCreateIntent
--
--         , requestCreateResourcePolicy $
--             newCreateResourcePolicy
--
--         , requestCreateResourcePolicyStatement $
--             newCreateResourcePolicyStatement
--
--         , requestCreateSlot $
--             newCreateSlot
--
--         , requestCreateSlotType $
--             newCreateSlotType
--
--         , requestCreateUploadUrl $
--             newCreateUploadUrl
--
--         , requestDeleteBot $
--             newDeleteBot
--
--         , requestDeleteBotAlias $
--             newDeleteBotAlias
--
--         , requestDeleteBotLocale $
--             newDeleteBotLocale
--
--         , requestDeleteBotVersion $
--             newDeleteBotVersion
--
--         , requestDeleteCustomVocabulary $
--             newDeleteCustomVocabulary
--
--         , requestDeleteExport $
--             newDeleteExport
--
--         , requestDeleteImport $
--             newDeleteImport
--
--         , requestDeleteIntent $
--             newDeleteIntent
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeleteResourcePolicyStatement $
--             newDeleteResourcePolicyStatement
--
--         , requestDeleteSlot $
--             newDeleteSlot
--
--         , requestDeleteSlotType $
--             newDeleteSlotType
--
--         , requestDeleteUtterances $
--             newDeleteUtterances
--
--         , requestDescribeBot $
--             newDescribeBot
--
--         , requestDescribeBotAlias $
--             newDescribeBotAlias
--
--         , requestDescribeBotLocale $
--             newDescribeBotLocale
--
--         , requestDescribeBotRecommendation $
--             newDescribeBotRecommendation
--
--         , requestDescribeBotVersion $
--             newDescribeBotVersion
--
--         , requestDescribeCustomVocabularyMetadata $
--             newDescribeCustomVocabularyMetadata
--
--         , requestDescribeExport $
--             newDescribeExport
--
--         , requestDescribeImport $
--             newDescribeImport
--
--         , requestDescribeIntent $
--             newDescribeIntent
--
--         , requestDescribeResourcePolicy $
--             newDescribeResourcePolicy
--
--         , requestDescribeSlot $
--             newDescribeSlot
--
--         , requestDescribeSlotType $
--             newDescribeSlotType
--
--         , requestListAggregatedUtterances $
--             newListAggregatedUtterances
--
--         , requestListBotAliases $
--             newListBotAliases
--
--         , requestListBotLocales $
--             newListBotLocales
--
--         , requestListBotRecommendations $
--             newListBotRecommendations
--
--         , requestListBotVersions $
--             newListBotVersions
--
--         , requestListBots $
--             newListBots
--
--         , requestListBuiltInIntents $
--             newListBuiltInIntents
--
--         , requestListBuiltInSlotTypes $
--             newListBuiltInSlotTypes
--
--         , requestListCustomVocabularyItems $
--             newListCustomVocabularyItems
--
--         , requestListExports $
--             newListExports
--
--         , requestListImports $
--             newListImports
--
--         , requestListIntents $
--             newListIntents
--
--         , requestListRecommendedIntents $
--             newListRecommendedIntents
--
--         , requestListSlotTypes $
--             newListSlotTypes
--
--         , requestListSlots $
--             newListSlots
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSearchAssociatedTranscripts $
--             newSearchAssociatedTranscripts
--
--         , requestStartBotRecommendation $
--             newStartBotRecommendation
--
--         , requestStartImport $
--             newStartImport
--
--         , requestStopBotRecommendation $
--             newStopBotRecommendation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateBot $
--             newUpdateBot
--
--         , requestUpdateBotAlias $
--             newUpdateBotAlias
--
--         , requestUpdateBotLocale $
--             newUpdateBotLocale
--
--         , requestUpdateBotRecommendation $
--             newUpdateBotRecommendation
--
--         , requestUpdateExport $
--             newUpdateExport
--
--         , requestUpdateIntent $
--             newUpdateIntent
--
--         , requestUpdateResourcePolicy $
--             newUpdateResourcePolicy
--
--         , requestUpdateSlot $
--             newUpdateSlot
--
--         , requestUpdateSlotType $
--             newUpdateSlotType
--
--           ]

--     , testGroup "response"
--         [ responseBatchCreateCustomVocabularyItem $
--             newBatchCreateCustomVocabularyItemResponse
--
--         , responseBatchDeleteCustomVocabularyItem $
--             newBatchDeleteCustomVocabularyItemResponse
--
--         , responseBatchUpdateCustomVocabularyItem $
--             newBatchUpdateCustomVocabularyItemResponse
--
--         , responseBuildBotLocale $
--             newBuildBotLocaleResponse
--
--         , responseCreateBot $
--             newCreateBotResponse
--
--         , responseCreateBotAlias $
--             newCreateBotAliasResponse
--
--         , responseCreateBotLocale $
--             newCreateBotLocaleResponse
--
--         , responseCreateBotVersion $
--             newCreateBotVersionResponse
--
--         , responseCreateExport $
--             newCreateExportResponse
--
--         , responseCreateIntent $
--             newCreateIntentResponse
--
--         , responseCreateResourcePolicy $
--             newCreateResourcePolicyResponse
--
--         , responseCreateResourcePolicyStatement $
--             newCreateResourcePolicyStatementResponse
--
--         , responseCreateSlot $
--             newCreateSlotResponse
--
--         , responseCreateSlotType $
--             newCreateSlotTypeResponse
--
--         , responseCreateUploadUrl $
--             newCreateUploadUrlResponse
--
--         , responseDeleteBot $
--             newDeleteBotResponse
--
--         , responseDeleteBotAlias $
--             newDeleteBotAliasResponse
--
--         , responseDeleteBotLocale $
--             newDeleteBotLocaleResponse
--
--         , responseDeleteBotVersion $
--             newDeleteBotVersionResponse
--
--         , responseDeleteCustomVocabulary $
--             newDeleteCustomVocabularyResponse
--
--         , responseDeleteExport $
--             newDeleteExportResponse
--
--         , responseDeleteImport $
--             newDeleteImportResponse
--
--         , responseDeleteIntent $
--             newDeleteIntentResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeleteResourcePolicyStatement $
--             newDeleteResourcePolicyStatementResponse
--
--         , responseDeleteSlot $
--             newDeleteSlotResponse
--
--         , responseDeleteSlotType $
--             newDeleteSlotTypeResponse
--
--         , responseDeleteUtterances $
--             newDeleteUtterancesResponse
--
--         , responseDescribeBot $
--             newDescribeBotResponse
--
--         , responseDescribeBotAlias $
--             newDescribeBotAliasResponse
--
--         , responseDescribeBotLocale $
--             newDescribeBotLocaleResponse
--
--         , responseDescribeBotRecommendation $
--             newDescribeBotRecommendationResponse
--
--         , responseDescribeBotVersion $
--             newDescribeBotVersionResponse
--
--         , responseDescribeCustomVocabularyMetadata $
--             newDescribeCustomVocabularyMetadataResponse
--
--         , responseDescribeExport $
--             newDescribeExportResponse
--
--         , responseDescribeImport $
--             newDescribeImportResponse
--
--         , responseDescribeIntent $
--             newDescribeIntentResponse
--
--         , responseDescribeResourcePolicy $
--             newDescribeResourcePolicyResponse
--
--         , responseDescribeSlot $
--             newDescribeSlotResponse
--
--         , responseDescribeSlotType $
--             newDescribeSlotTypeResponse
--
--         , responseListAggregatedUtterances $
--             newListAggregatedUtterancesResponse
--
--         , responseListBotAliases $
--             newListBotAliasesResponse
--
--         , responseListBotLocales $
--             newListBotLocalesResponse
--
--         , responseListBotRecommendations $
--             newListBotRecommendationsResponse
--
--         , responseListBotVersions $
--             newListBotVersionsResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseListBuiltInIntents $
--             newListBuiltInIntentsResponse
--
--         , responseListBuiltInSlotTypes $
--             newListBuiltInSlotTypesResponse
--
--         , responseListCustomVocabularyItems $
--             newListCustomVocabularyItemsResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseListIntents $
--             newListIntentsResponse
--
--         , responseListRecommendedIntents $
--             newListRecommendedIntentsResponse
--
--         , responseListSlotTypes $
--             newListSlotTypesResponse
--
--         , responseListSlots $
--             newListSlotsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSearchAssociatedTranscripts $
--             newSearchAssociatedTranscriptsResponse
--
--         , responseStartBotRecommendation $
--             newStartBotRecommendationResponse
--
--         , responseStartImport $
--             newStartImportResponse
--
--         , responseStopBotRecommendation $
--             newStopBotRecommendationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateBot $
--             newUpdateBotResponse
--
--         , responseUpdateBotAlias $
--             newUpdateBotAliasResponse
--
--         , responseUpdateBotLocale $
--             newUpdateBotLocaleResponse
--
--         , responseUpdateBotRecommendation $
--             newUpdateBotRecommendationResponse
--
--         , responseUpdateExport $
--             newUpdateExportResponse
--
--         , responseUpdateIntent $
--             newUpdateIntentResponse
--
--         , responseUpdateResourcePolicy $
--             newUpdateResourcePolicyResponse
--
--         , responseUpdateSlot $
--             newUpdateSlotResponse
--
--         , responseUpdateSlotType $
--             newUpdateSlotTypeResponse
--
--           ]
--     ]

-- Requests

requestBatchCreateCustomVocabularyItem :: BatchCreateCustomVocabularyItem -> TestTree
requestBatchCreateCustomVocabularyItem =
  req
    "BatchCreateCustomVocabularyItem"
    "fixture/BatchCreateCustomVocabularyItem.yaml"

requestBatchDeleteCustomVocabularyItem :: BatchDeleteCustomVocabularyItem -> TestTree
requestBatchDeleteCustomVocabularyItem =
  req
    "BatchDeleteCustomVocabularyItem"
    "fixture/BatchDeleteCustomVocabularyItem.yaml"

requestBatchUpdateCustomVocabularyItem :: BatchUpdateCustomVocabularyItem -> TestTree
requestBatchUpdateCustomVocabularyItem =
  req
    "BatchUpdateCustomVocabularyItem"
    "fixture/BatchUpdateCustomVocabularyItem.yaml"

requestBuildBotLocale :: BuildBotLocale -> TestTree
requestBuildBotLocale =
  req
    "BuildBotLocale"
    "fixture/BuildBotLocale.yaml"

requestCreateBot :: CreateBot -> TestTree
requestCreateBot =
  req
    "CreateBot"
    "fixture/CreateBot.yaml"

requestCreateBotAlias :: CreateBotAlias -> TestTree
requestCreateBotAlias =
  req
    "CreateBotAlias"
    "fixture/CreateBotAlias.yaml"

requestCreateBotLocale :: CreateBotLocale -> TestTree
requestCreateBotLocale =
  req
    "CreateBotLocale"
    "fixture/CreateBotLocale.yaml"

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion =
  req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestCreateExport :: CreateExport -> TestTree
requestCreateExport =
  req
    "CreateExport"
    "fixture/CreateExport.yaml"

requestCreateIntent :: CreateIntent -> TestTree
requestCreateIntent =
  req
    "CreateIntent"
    "fixture/CreateIntent.yaml"

requestCreateResourcePolicy :: CreateResourcePolicy -> TestTree
requestCreateResourcePolicy =
  req
    "CreateResourcePolicy"
    "fixture/CreateResourcePolicy.yaml"

requestCreateResourcePolicyStatement :: CreateResourcePolicyStatement -> TestTree
requestCreateResourcePolicyStatement =
  req
    "CreateResourcePolicyStatement"
    "fixture/CreateResourcePolicyStatement.yaml"

requestCreateSlot :: CreateSlot -> TestTree
requestCreateSlot =
  req
    "CreateSlot"
    "fixture/CreateSlot.yaml"

requestCreateSlotType :: CreateSlotType -> TestTree
requestCreateSlotType =
  req
    "CreateSlotType"
    "fixture/CreateSlotType.yaml"

requestCreateUploadUrl :: CreateUploadUrl -> TestTree
requestCreateUploadUrl =
  req
    "CreateUploadUrl"
    "fixture/CreateUploadUrl.yaml"

requestDeleteBot :: DeleteBot -> TestTree
requestDeleteBot =
  req
    "DeleteBot"
    "fixture/DeleteBot.yaml"

requestDeleteBotAlias :: DeleteBotAlias -> TestTree
requestDeleteBotAlias =
  req
    "DeleteBotAlias"
    "fixture/DeleteBotAlias.yaml"

requestDeleteBotLocale :: DeleteBotLocale -> TestTree
requestDeleteBotLocale =
  req
    "DeleteBotLocale"
    "fixture/DeleteBotLocale.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion =
  req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestDeleteCustomVocabulary :: DeleteCustomVocabulary -> TestTree
requestDeleteCustomVocabulary =
  req
    "DeleteCustomVocabulary"
    "fixture/DeleteCustomVocabulary.yaml"

requestDeleteExport :: DeleteExport -> TestTree
requestDeleteExport =
  req
    "DeleteExport"
    "fixture/DeleteExport.yaml"

requestDeleteImport :: DeleteImport -> TestTree
requestDeleteImport =
  req
    "DeleteImport"
    "fixture/DeleteImport.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent =
  req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeleteResourcePolicyStatement :: DeleteResourcePolicyStatement -> TestTree
requestDeleteResourcePolicyStatement =
  req
    "DeleteResourcePolicyStatement"
    "fixture/DeleteResourcePolicyStatement.yaml"

requestDeleteSlot :: DeleteSlot -> TestTree
requestDeleteSlot =
  req
    "DeleteSlot"
    "fixture/DeleteSlot.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType =
  req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestDeleteUtterances :: DeleteUtterances -> TestTree
requestDeleteUtterances =
  req
    "DeleteUtterances"
    "fixture/DeleteUtterances.yaml"

requestDescribeBot :: DescribeBot -> TestTree
requestDescribeBot =
  req
    "DescribeBot"
    "fixture/DescribeBot.yaml"

requestDescribeBotAlias :: DescribeBotAlias -> TestTree
requestDescribeBotAlias =
  req
    "DescribeBotAlias"
    "fixture/DescribeBotAlias.yaml"

requestDescribeBotLocale :: DescribeBotLocale -> TestTree
requestDescribeBotLocale =
  req
    "DescribeBotLocale"
    "fixture/DescribeBotLocale.yaml"

requestDescribeBotRecommendation :: DescribeBotRecommendation -> TestTree
requestDescribeBotRecommendation =
  req
    "DescribeBotRecommendation"
    "fixture/DescribeBotRecommendation.yaml"

requestDescribeBotVersion :: DescribeBotVersion -> TestTree
requestDescribeBotVersion =
  req
    "DescribeBotVersion"
    "fixture/DescribeBotVersion.yaml"

requestDescribeCustomVocabularyMetadata :: DescribeCustomVocabularyMetadata -> TestTree
requestDescribeCustomVocabularyMetadata =
  req
    "DescribeCustomVocabularyMetadata"
    "fixture/DescribeCustomVocabularyMetadata.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport =
  req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

requestDescribeImport :: DescribeImport -> TestTree
requestDescribeImport =
  req
    "DescribeImport"
    "fixture/DescribeImport.yaml"

requestDescribeIntent :: DescribeIntent -> TestTree
requestDescribeIntent =
  req
    "DescribeIntent"
    "fixture/DescribeIntent.yaml"

requestDescribeResourcePolicy :: DescribeResourcePolicy -> TestTree
requestDescribeResourcePolicy =
  req
    "DescribeResourcePolicy"
    "fixture/DescribeResourcePolicy.yaml"

requestDescribeSlot :: DescribeSlot -> TestTree
requestDescribeSlot =
  req
    "DescribeSlot"
    "fixture/DescribeSlot.yaml"

requestDescribeSlotType :: DescribeSlotType -> TestTree
requestDescribeSlotType =
  req
    "DescribeSlotType"
    "fixture/DescribeSlotType.yaml"

requestListAggregatedUtterances :: ListAggregatedUtterances -> TestTree
requestListAggregatedUtterances =
  req
    "ListAggregatedUtterances"
    "fixture/ListAggregatedUtterances.yaml"

requestListBotAliases :: ListBotAliases -> TestTree
requestListBotAliases =
  req
    "ListBotAliases"
    "fixture/ListBotAliases.yaml"

requestListBotLocales :: ListBotLocales -> TestTree
requestListBotLocales =
  req
    "ListBotLocales"
    "fixture/ListBotLocales.yaml"

requestListBotRecommendations :: ListBotRecommendations -> TestTree
requestListBotRecommendations =
  req
    "ListBotRecommendations"
    "fixture/ListBotRecommendations.yaml"

requestListBotVersions :: ListBotVersions -> TestTree
requestListBotVersions =
  req
    "ListBotVersions"
    "fixture/ListBotVersions.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestListBuiltInIntents :: ListBuiltInIntents -> TestTree
requestListBuiltInIntents =
  req
    "ListBuiltInIntents"
    "fixture/ListBuiltInIntents.yaml"

requestListBuiltInSlotTypes :: ListBuiltInSlotTypes -> TestTree
requestListBuiltInSlotTypes =
  req
    "ListBuiltInSlotTypes"
    "fixture/ListBuiltInSlotTypes.yaml"

requestListCustomVocabularyItems :: ListCustomVocabularyItems -> TestTree
requestListCustomVocabularyItems =
  req
    "ListCustomVocabularyItems"
    "fixture/ListCustomVocabularyItems.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestListIntents :: ListIntents -> TestTree
requestListIntents =
  req
    "ListIntents"
    "fixture/ListIntents.yaml"

requestListRecommendedIntents :: ListRecommendedIntents -> TestTree
requestListRecommendedIntents =
  req
    "ListRecommendedIntents"
    "fixture/ListRecommendedIntents.yaml"

requestListSlotTypes :: ListSlotTypes -> TestTree
requestListSlotTypes =
  req
    "ListSlotTypes"
    "fixture/ListSlotTypes.yaml"

requestListSlots :: ListSlots -> TestTree
requestListSlots =
  req
    "ListSlots"
    "fixture/ListSlots.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSearchAssociatedTranscripts :: SearchAssociatedTranscripts -> TestTree
requestSearchAssociatedTranscripts =
  req
    "SearchAssociatedTranscripts"
    "fixture/SearchAssociatedTranscripts.yaml"

requestStartBotRecommendation :: StartBotRecommendation -> TestTree
requestStartBotRecommendation =
  req
    "StartBotRecommendation"
    "fixture/StartBotRecommendation.yaml"

requestStartImport :: StartImport -> TestTree
requestStartImport =
  req
    "StartImport"
    "fixture/StartImport.yaml"

requestStopBotRecommendation :: StopBotRecommendation -> TestTree
requestStopBotRecommendation =
  req
    "StopBotRecommendation"
    "fixture/StopBotRecommendation.yaml"

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

requestUpdateBot :: UpdateBot -> TestTree
requestUpdateBot =
  req
    "UpdateBot"
    "fixture/UpdateBot.yaml"

requestUpdateBotAlias :: UpdateBotAlias -> TestTree
requestUpdateBotAlias =
  req
    "UpdateBotAlias"
    "fixture/UpdateBotAlias.yaml"

requestUpdateBotLocale :: UpdateBotLocale -> TestTree
requestUpdateBotLocale =
  req
    "UpdateBotLocale"
    "fixture/UpdateBotLocale.yaml"

requestUpdateBotRecommendation :: UpdateBotRecommendation -> TestTree
requestUpdateBotRecommendation =
  req
    "UpdateBotRecommendation"
    "fixture/UpdateBotRecommendation.yaml"

requestUpdateExport :: UpdateExport -> TestTree
requestUpdateExport =
  req
    "UpdateExport"
    "fixture/UpdateExport.yaml"

requestUpdateIntent :: UpdateIntent -> TestTree
requestUpdateIntent =
  req
    "UpdateIntent"
    "fixture/UpdateIntent.yaml"

requestUpdateResourcePolicy :: UpdateResourcePolicy -> TestTree
requestUpdateResourcePolicy =
  req
    "UpdateResourcePolicy"
    "fixture/UpdateResourcePolicy.yaml"

requestUpdateSlot :: UpdateSlot -> TestTree
requestUpdateSlot =
  req
    "UpdateSlot"
    "fixture/UpdateSlot.yaml"

requestUpdateSlotType :: UpdateSlotType -> TestTree
requestUpdateSlotType =
  req
    "UpdateSlotType"
    "fixture/UpdateSlotType.yaml"

-- Responses

responseBatchCreateCustomVocabularyItem :: BatchCreateCustomVocabularyItemResponse -> TestTree
responseBatchCreateCustomVocabularyItem =
  res
    "BatchCreateCustomVocabularyItemResponse"
    "fixture/BatchCreateCustomVocabularyItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchCreateCustomVocabularyItem)

responseBatchDeleteCustomVocabularyItem :: BatchDeleteCustomVocabularyItemResponse -> TestTree
responseBatchDeleteCustomVocabularyItem =
  res
    "BatchDeleteCustomVocabularyItemResponse"
    "fixture/BatchDeleteCustomVocabularyItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDeleteCustomVocabularyItem)

responseBatchUpdateCustomVocabularyItem :: BatchUpdateCustomVocabularyItemResponse -> TestTree
responseBatchUpdateCustomVocabularyItem =
  res
    "BatchUpdateCustomVocabularyItemResponse"
    "fixture/BatchUpdateCustomVocabularyItemResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchUpdateCustomVocabularyItem)

responseBuildBotLocale :: BuildBotLocaleResponse -> TestTree
responseBuildBotLocale =
  res
    "BuildBotLocaleResponse"
    "fixture/BuildBotLocaleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BuildBotLocale)

responseCreateBot :: CreateBotResponse -> TestTree
responseCreateBot =
  res
    "CreateBotResponse"
    "fixture/CreateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBot)

responseCreateBotAlias :: CreateBotAliasResponse -> TestTree
responseCreateBotAlias =
  res
    "CreateBotAliasResponse"
    "fixture/CreateBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBotAlias)

responseCreateBotLocale :: CreateBotLocaleResponse -> TestTree
responseCreateBotLocale =
  res
    "CreateBotLocaleResponse"
    "fixture/CreateBotLocaleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBotLocale)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBotVersion)

responseCreateExport :: CreateExportResponse -> TestTree
responseCreateExport =
  res
    "CreateExportResponse"
    "fixture/CreateExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateExport)

responseCreateIntent :: CreateIntentResponse -> TestTree
responseCreateIntent =
  res
    "CreateIntentResponse"
    "fixture/CreateIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntent)

responseCreateResourcePolicy :: CreateResourcePolicyResponse -> TestTree
responseCreateResourcePolicy =
  res
    "CreateResourcePolicyResponse"
    "fixture/CreateResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourcePolicy)

responseCreateResourcePolicyStatement :: CreateResourcePolicyStatementResponse -> TestTree
responseCreateResourcePolicyStatement =
  res
    "CreateResourcePolicyStatementResponse"
    "fixture/CreateResourcePolicyStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourcePolicyStatement)

responseCreateSlot :: CreateSlotResponse -> TestTree
responseCreateSlot =
  res
    "CreateSlotResponse"
    "fixture/CreateSlotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSlot)

responseCreateSlotType :: CreateSlotTypeResponse -> TestTree
responseCreateSlotType =
  res
    "CreateSlotTypeResponse"
    "fixture/CreateSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSlotType)

responseCreateUploadUrl :: CreateUploadUrlResponse -> TestTree
responseCreateUploadUrl =
  res
    "CreateUploadUrlResponse"
    "fixture/CreateUploadUrlResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUploadUrl)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot =
  res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBot)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias =
  res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotAlias)

responseDeleteBotLocale :: DeleteBotLocaleResponse -> TestTree
responseDeleteBotLocale =
  res
    "DeleteBotLocaleResponse"
    "fixture/DeleteBotLocaleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotLocale)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotVersion)

responseDeleteCustomVocabulary :: DeleteCustomVocabularyResponse -> TestTree
responseDeleteCustomVocabulary =
  res
    "DeleteCustomVocabularyResponse"
    "fixture/DeleteCustomVocabularyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCustomVocabulary)

responseDeleteExport :: DeleteExportResponse -> TestTree
responseDeleteExport =
  res
    "DeleteExportResponse"
    "fixture/DeleteExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteExport)

responseDeleteImport :: DeleteImportResponse -> TestTree
responseDeleteImport =
  res
    "DeleteImportResponse"
    "fixture/DeleteImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImport)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntent)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeleteResourcePolicyStatement :: DeleteResourcePolicyStatementResponse -> TestTree
responseDeleteResourcePolicyStatement =
  res
    "DeleteResourcePolicyStatementResponse"
    "fixture/DeleteResourcePolicyStatementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicyStatement)

responseDeleteSlot :: DeleteSlotResponse -> TestTree
responseDeleteSlot =
  res
    "DeleteSlotResponse"
    "fixture/DeleteSlotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlot)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlotType)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUtterances)

responseDescribeBot :: DescribeBotResponse -> TestTree
responseDescribeBot =
  res
    "DescribeBotResponse"
    "fixture/DescribeBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBot)

responseDescribeBotAlias :: DescribeBotAliasResponse -> TestTree
responseDescribeBotAlias =
  res
    "DescribeBotAliasResponse"
    "fixture/DescribeBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBotAlias)

responseDescribeBotLocale :: DescribeBotLocaleResponse -> TestTree
responseDescribeBotLocale =
  res
    "DescribeBotLocaleResponse"
    "fixture/DescribeBotLocaleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBotLocale)

responseDescribeBotRecommendation :: DescribeBotRecommendationResponse -> TestTree
responseDescribeBotRecommendation =
  res
    "DescribeBotRecommendationResponse"
    "fixture/DescribeBotRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBotRecommendation)

responseDescribeBotVersion :: DescribeBotVersionResponse -> TestTree
responseDescribeBotVersion =
  res
    "DescribeBotVersionResponse"
    "fixture/DescribeBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeBotVersion)

responseDescribeCustomVocabularyMetadata :: DescribeCustomVocabularyMetadataResponse -> TestTree
responseDescribeCustomVocabularyMetadata =
  res
    "DescribeCustomVocabularyMetadataResponse"
    "fixture/DescribeCustomVocabularyMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCustomVocabularyMetadata)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeExport)

responseDescribeImport :: DescribeImportResponse -> TestTree
responseDescribeImport =
  res
    "DescribeImportResponse"
    "fixture/DescribeImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImport)

responseDescribeIntent :: DescribeIntentResponse -> TestTree
responseDescribeIntent =
  res
    "DescribeIntentResponse"
    "fixture/DescribeIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIntent)

responseDescribeResourcePolicy :: DescribeResourcePolicyResponse -> TestTree
responseDescribeResourcePolicy =
  res
    "DescribeResourcePolicyResponse"
    "fixture/DescribeResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicy)

responseDescribeSlot :: DescribeSlotResponse -> TestTree
responseDescribeSlot =
  res
    "DescribeSlotResponse"
    "fixture/DescribeSlotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSlot)

responseDescribeSlotType :: DescribeSlotTypeResponse -> TestTree
responseDescribeSlotType =
  res
    "DescribeSlotTypeResponse"
    "fixture/DescribeSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSlotType)

responseListAggregatedUtterances :: ListAggregatedUtterancesResponse -> TestTree
responseListAggregatedUtterances =
  res
    "ListAggregatedUtterancesResponse"
    "fixture/ListAggregatedUtterancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAggregatedUtterances)

responseListBotAliases :: ListBotAliasesResponse -> TestTree
responseListBotAliases =
  res
    "ListBotAliasesResponse"
    "fixture/ListBotAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBotAliases)

responseListBotLocales :: ListBotLocalesResponse -> TestTree
responseListBotLocales =
  res
    "ListBotLocalesResponse"
    "fixture/ListBotLocalesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBotLocales)

responseListBotRecommendations :: ListBotRecommendationsResponse -> TestTree
responseListBotRecommendations =
  res
    "ListBotRecommendationsResponse"
    "fixture/ListBotRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBotRecommendations)

responseListBotVersions :: ListBotVersionsResponse -> TestTree
responseListBotVersions =
  res
    "ListBotVersionsResponse"
    "fixture/ListBotVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBotVersions)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBots)

responseListBuiltInIntents :: ListBuiltInIntentsResponse -> TestTree
responseListBuiltInIntents =
  res
    "ListBuiltInIntentsResponse"
    "fixture/ListBuiltInIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuiltInIntents)

responseListBuiltInSlotTypes :: ListBuiltInSlotTypesResponse -> TestTree
responseListBuiltInSlotTypes =
  res
    "ListBuiltInSlotTypesResponse"
    "fixture/ListBuiltInSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBuiltInSlotTypes)

responseListCustomVocabularyItems :: ListCustomVocabularyItemsResponse -> TestTree
responseListCustomVocabularyItems =
  res
    "ListCustomVocabularyItemsResponse"
    "fixture/ListCustomVocabularyItemsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCustomVocabularyItems)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExports)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImports)

responseListIntents :: ListIntentsResponse -> TestTree
responseListIntents =
  res
    "ListIntentsResponse"
    "fixture/ListIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIntents)

responseListRecommendedIntents :: ListRecommendedIntentsResponse -> TestTree
responseListRecommendedIntents =
  res
    "ListRecommendedIntentsResponse"
    "fixture/ListRecommendedIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecommendedIntents)

responseListSlotTypes :: ListSlotTypesResponse -> TestTree
responseListSlotTypes =
  res
    "ListSlotTypesResponse"
    "fixture/ListSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSlotTypes)

responseListSlots :: ListSlotsResponse -> TestTree
responseListSlots =
  res
    "ListSlotsResponse"
    "fixture/ListSlotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSlots)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSearchAssociatedTranscripts :: SearchAssociatedTranscriptsResponse -> TestTree
responseSearchAssociatedTranscripts =
  res
    "SearchAssociatedTranscriptsResponse"
    "fixture/SearchAssociatedTranscriptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchAssociatedTranscripts)

responseStartBotRecommendation :: StartBotRecommendationResponse -> TestTree
responseStartBotRecommendation =
  res
    "StartBotRecommendationResponse"
    "fixture/StartBotRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBotRecommendation)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImport)

responseStopBotRecommendation :: StopBotRecommendationResponse -> TestTree
responseStopBotRecommendation =
  res
    "StopBotRecommendationResponse"
    "fixture/StopBotRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBotRecommendation)

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

responseUpdateBot :: UpdateBotResponse -> TestTree
responseUpdateBot =
  res
    "UpdateBotResponse"
    "fixture/UpdateBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBot)

responseUpdateBotAlias :: UpdateBotAliasResponse -> TestTree
responseUpdateBotAlias =
  res
    "UpdateBotAliasResponse"
    "fixture/UpdateBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBotAlias)

responseUpdateBotLocale :: UpdateBotLocaleResponse -> TestTree
responseUpdateBotLocale =
  res
    "UpdateBotLocaleResponse"
    "fixture/UpdateBotLocaleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBotLocale)

responseUpdateBotRecommendation :: UpdateBotRecommendationResponse -> TestTree
responseUpdateBotRecommendation =
  res
    "UpdateBotRecommendationResponse"
    "fixture/UpdateBotRecommendationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateBotRecommendation)

responseUpdateExport :: UpdateExportResponse -> TestTree
responseUpdateExport =
  res
    "UpdateExportResponse"
    "fixture/UpdateExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateExport)

responseUpdateIntent :: UpdateIntentResponse -> TestTree
responseUpdateIntent =
  res
    "UpdateIntentResponse"
    "fixture/UpdateIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateIntent)

responseUpdateResourcePolicy :: UpdateResourcePolicyResponse -> TestTree
responseUpdateResourcePolicy =
  res
    "UpdateResourcePolicyResponse"
    "fixture/UpdateResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourcePolicy)

responseUpdateSlot :: UpdateSlotResponse -> TestTree
responseUpdateSlot =
  res
    "UpdateSlotResponse"
    "fixture/UpdateSlotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSlot)

responseUpdateSlotType :: UpdateSlotTypeResponse -> TestTree
responseUpdateSlotType =
  res
    "UpdateSlotTypeResponse"
    "fixture/UpdateSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSlotType)
