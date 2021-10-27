{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexV2Models
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LexV2Models where

import Data.Proxy
import Network.AWS.LexV2Models
import Test.AWS.Fixture
import Test.AWS.LexV2Models.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteImport $
--             newDeleteImport
--
--         , requestDescribeBot $
--             newDescribeBot
--
--         , requestDescribeBotLocale $
--             newDescribeBotLocale
--
--         , requestListBuiltInSlotTypes $
--             newListBuiltInSlotTypes
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListBotVersions $
--             newListBotVersions
--
--         , requestListIntents $
--             newListIntents
--
--         , requestListSlots $
--             newListSlots
--
--         , requestDeleteIntent $
--             newDeleteIntent
--
--         , requestDeleteSlot $
--             newDeleteSlot
--
--         , requestUpdateIntent $
--             newUpdateIntent
--
--         , requestUpdateSlot $
--             newUpdateSlot
--
--         , requestCreateSlot $
--             newCreateSlot
--
--         , requestListBots $
--             newListBots
--
--         , requestDeleteBotLocale $
--             newDeleteBotLocale
--
--         , requestUpdateBotLocale $
--             newUpdateBotLocale
--
--         , requestCreateIntent $
--             newCreateIntent
--
--         , requestDescribeImport $
--             newDescribeImport
--
--         , requestDeleteBot $
--             newDeleteBot
--
--         , requestUpdateBot $
--             newUpdateBot
--
--         , requestListBotLocales $
--             newListBotLocales
--
--         , requestCreateResourcePolicy $
--             newCreateResourcePolicy
--
--         , requestDeleteBotAlias $
--             newDeleteBotAlias
--
--         , requestUpdateBotAlias $
--             newUpdateBotAlias
--
--         , requestDescribeBotVersion $
--             newDescribeBotVersion
--
--         , requestDescribeSlot $
--             newDescribeSlot
--
--         , requestDescribeIntent $
--             newDescribeIntent
--
--         , requestDeleteUtterances $
--             newDeleteUtterances
--
--         , requestCreateUploadUrl $
--             newCreateUploadUrl
--
--         , requestListBuiltInIntents $
--             newListBuiltInIntents
--
--         , requestListImports $
--             newListImports
--
--         , requestListAggregatedUtterances $
--             newListAggregatedUtterances
--
--         , requestCreateBotVersion $
--             newCreateBotVersion
--
--         , requestBuildBotLocale $
--             newBuildBotLocale
--
--         , requestDescribeResourcePolicy $
--             newDescribeResourcePolicy
--
--         , requestDeleteBotVersion $
--             newDeleteBotVersion
--
--         , requestDescribeBotAlias $
--             newDescribeBotAlias
--
--         , requestUpdateSlotType $
--             newUpdateSlotType
--
--         , requestDeleteSlotType $
--             newDeleteSlotType
--
--         , requestCreateBotLocale $
--             newCreateBotLocale
--
--         , requestListSlotTypes $
--             newListSlotTypes
--
--         , requestDeleteExport $
--             newDeleteExport
--
--         , requestStartImport $
--             newStartImport
--
--         , requestUpdateExport $
--             newUpdateExport
--
--         , requestCreateBot $
--             newCreateBot
--
--         , requestListExports $
--             newListExports
--
--         , requestCreateSlotType $
--             newCreateSlotType
--
--         , requestCreateExport $
--             newCreateExport
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListBotAliases $
--             newListBotAliases
--
--         , requestCreateBotAlias $
--             newCreateBotAlias
--
--         , requestCreateResourcePolicyStatement $
--             newCreateResourcePolicyStatement
--
--         , requestUpdateResourcePolicy $
--             newUpdateResourcePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteResourcePolicyStatement $
--             newDeleteResourcePolicyStatement
--
--         , requestDescribeSlotType $
--             newDescribeSlotType
--
--         , requestDescribeExport $
--             newDescribeExport
--
--           ]

--     , testGroup "response"
--         [ responseDeleteImport $
--             newDeleteImportResponse
--
--         , responseDescribeBot $
--             newDescribeBotResponse
--
--         , responseDescribeBotLocale $
--             newDescribeBotLocaleResponse
--
--         , responseListBuiltInSlotTypes $
--             newListBuiltInSlotTypesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListBotVersions $
--             newListBotVersionsResponse
--
--         , responseListIntents $
--             newListIntentsResponse
--
--         , responseListSlots $
--             newListSlotsResponse
--
--         , responseDeleteIntent $
--             newDeleteIntentResponse
--
--         , responseDeleteSlot $
--             newDeleteSlotResponse
--
--         , responseUpdateIntent $
--             newUpdateIntentResponse
--
--         , responseUpdateSlot $
--             newUpdateSlotResponse
--
--         , responseCreateSlot $
--             newCreateSlotResponse
--
--         , responseListBots $
--             newListBotsResponse
--
--         , responseDeleteBotLocale $
--             newDeleteBotLocaleResponse
--
--         , responseUpdateBotLocale $
--             newUpdateBotLocaleResponse
--
--         , responseCreateIntent $
--             newCreateIntentResponse
--
--         , responseDescribeImport $
--             newDescribeImportResponse
--
--         , responseDeleteBot $
--             newDeleteBotResponse
--
--         , responseUpdateBot $
--             newUpdateBotResponse
--
--         , responseListBotLocales $
--             newListBotLocalesResponse
--
--         , responseCreateResourcePolicy $
--             newCreateResourcePolicyResponse
--
--         , responseDeleteBotAlias $
--             newDeleteBotAliasResponse
--
--         , responseUpdateBotAlias $
--             newUpdateBotAliasResponse
--
--         , responseDescribeBotVersion $
--             newDescribeBotVersionResponse
--
--         , responseDescribeSlot $
--             newDescribeSlotResponse
--
--         , responseDescribeIntent $
--             newDescribeIntentResponse
--
--         , responseDeleteUtterances $
--             newDeleteUtterancesResponse
--
--         , responseCreateUploadUrl $
--             newCreateUploadUrlResponse
--
--         , responseListBuiltInIntents $
--             newListBuiltInIntentsResponse
--
--         , responseListImports $
--             newListImportsResponse
--
--         , responseListAggregatedUtterances $
--             newListAggregatedUtterancesResponse
--
--         , responseCreateBotVersion $
--             newCreateBotVersionResponse
--
--         , responseBuildBotLocale $
--             newBuildBotLocaleResponse
--
--         , responseDescribeResourcePolicy $
--             newDescribeResourcePolicyResponse
--
--         , responseDeleteBotVersion $
--             newDeleteBotVersionResponse
--
--         , responseDescribeBotAlias $
--             newDescribeBotAliasResponse
--
--         , responseUpdateSlotType $
--             newUpdateSlotTypeResponse
--
--         , responseDeleteSlotType $
--             newDeleteSlotTypeResponse
--
--         , responseCreateBotLocale $
--             newCreateBotLocaleResponse
--
--         , responseListSlotTypes $
--             newListSlotTypesResponse
--
--         , responseDeleteExport $
--             newDeleteExportResponse
--
--         , responseStartImport $
--             newStartImportResponse
--
--         , responseUpdateExport $
--             newUpdateExportResponse
--
--         , responseCreateBot $
--             newCreateBotResponse
--
--         , responseListExports $
--             newListExportsResponse
--
--         , responseCreateSlotType $
--             newCreateSlotTypeResponse
--
--         , responseCreateExport $
--             newCreateExportResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListBotAliases $
--             newListBotAliasesResponse
--
--         , responseCreateBotAlias $
--             newCreateBotAliasResponse
--
--         , responseCreateResourcePolicyStatement $
--             newCreateResourcePolicyStatementResponse
--
--         , responseUpdateResourcePolicy $
--             newUpdateResourcePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteResourcePolicyStatement $
--             newDeleteResourcePolicyStatementResponse
--
--         , responseDescribeSlotType $
--             newDescribeSlotTypeResponse
--
--         , responseDescribeExport $
--             newDescribeExportResponse
--
--           ]
--     ]

-- Requests

requestDeleteImport :: DeleteImport -> TestTree
requestDeleteImport =
  req
    "DeleteImport"
    "fixture/DeleteImport.yaml"

requestDescribeBot :: DescribeBot -> TestTree
requestDescribeBot =
  req
    "DescribeBot"
    "fixture/DescribeBot.yaml"

requestDescribeBotLocale :: DescribeBotLocale -> TestTree
requestDescribeBotLocale =
  req
    "DescribeBotLocale"
    "fixture/DescribeBotLocale.yaml"

requestListBuiltInSlotTypes :: ListBuiltInSlotTypes -> TestTree
requestListBuiltInSlotTypes =
  req
    "ListBuiltInSlotTypes"
    "fixture/ListBuiltInSlotTypes.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListBotVersions :: ListBotVersions -> TestTree
requestListBotVersions =
  req
    "ListBotVersions"
    "fixture/ListBotVersions.yaml"

requestListIntents :: ListIntents -> TestTree
requestListIntents =
  req
    "ListIntents"
    "fixture/ListIntents.yaml"

requestListSlots :: ListSlots -> TestTree
requestListSlots =
  req
    "ListSlots"
    "fixture/ListSlots.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent =
  req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestDeleteSlot :: DeleteSlot -> TestTree
requestDeleteSlot =
  req
    "DeleteSlot"
    "fixture/DeleteSlot.yaml"

requestUpdateIntent :: UpdateIntent -> TestTree
requestUpdateIntent =
  req
    "UpdateIntent"
    "fixture/UpdateIntent.yaml"

requestUpdateSlot :: UpdateSlot -> TestTree
requestUpdateSlot =
  req
    "UpdateSlot"
    "fixture/UpdateSlot.yaml"

requestCreateSlot :: CreateSlot -> TestTree
requestCreateSlot =
  req
    "CreateSlot"
    "fixture/CreateSlot.yaml"

requestListBots :: ListBots -> TestTree
requestListBots =
  req
    "ListBots"
    "fixture/ListBots.yaml"

requestDeleteBotLocale :: DeleteBotLocale -> TestTree
requestDeleteBotLocale =
  req
    "DeleteBotLocale"
    "fixture/DeleteBotLocale.yaml"

requestUpdateBotLocale :: UpdateBotLocale -> TestTree
requestUpdateBotLocale =
  req
    "UpdateBotLocale"
    "fixture/UpdateBotLocale.yaml"

requestCreateIntent :: CreateIntent -> TestTree
requestCreateIntent =
  req
    "CreateIntent"
    "fixture/CreateIntent.yaml"

requestDescribeImport :: DescribeImport -> TestTree
requestDescribeImport =
  req
    "DescribeImport"
    "fixture/DescribeImport.yaml"

requestDeleteBot :: DeleteBot -> TestTree
requestDeleteBot =
  req
    "DeleteBot"
    "fixture/DeleteBot.yaml"

requestUpdateBot :: UpdateBot -> TestTree
requestUpdateBot =
  req
    "UpdateBot"
    "fixture/UpdateBot.yaml"

requestListBotLocales :: ListBotLocales -> TestTree
requestListBotLocales =
  req
    "ListBotLocales"
    "fixture/ListBotLocales.yaml"

requestCreateResourcePolicy :: CreateResourcePolicy -> TestTree
requestCreateResourcePolicy =
  req
    "CreateResourcePolicy"
    "fixture/CreateResourcePolicy.yaml"

requestDeleteBotAlias :: DeleteBotAlias -> TestTree
requestDeleteBotAlias =
  req
    "DeleteBotAlias"
    "fixture/DeleteBotAlias.yaml"

requestUpdateBotAlias :: UpdateBotAlias -> TestTree
requestUpdateBotAlias =
  req
    "UpdateBotAlias"
    "fixture/UpdateBotAlias.yaml"

requestDescribeBotVersion :: DescribeBotVersion -> TestTree
requestDescribeBotVersion =
  req
    "DescribeBotVersion"
    "fixture/DescribeBotVersion.yaml"

requestDescribeSlot :: DescribeSlot -> TestTree
requestDescribeSlot =
  req
    "DescribeSlot"
    "fixture/DescribeSlot.yaml"

requestDescribeIntent :: DescribeIntent -> TestTree
requestDescribeIntent =
  req
    "DescribeIntent"
    "fixture/DescribeIntent.yaml"

requestDeleteUtterances :: DeleteUtterances -> TestTree
requestDeleteUtterances =
  req
    "DeleteUtterances"
    "fixture/DeleteUtterances.yaml"

requestCreateUploadUrl :: CreateUploadUrl -> TestTree
requestCreateUploadUrl =
  req
    "CreateUploadUrl"
    "fixture/CreateUploadUrl.yaml"

requestListBuiltInIntents :: ListBuiltInIntents -> TestTree
requestListBuiltInIntents =
  req
    "ListBuiltInIntents"
    "fixture/ListBuiltInIntents.yaml"

requestListImports :: ListImports -> TestTree
requestListImports =
  req
    "ListImports"
    "fixture/ListImports.yaml"

requestListAggregatedUtterances :: ListAggregatedUtterances -> TestTree
requestListAggregatedUtterances =
  req
    "ListAggregatedUtterances"
    "fixture/ListAggregatedUtterances.yaml"

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion =
  req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestBuildBotLocale :: BuildBotLocale -> TestTree
requestBuildBotLocale =
  req
    "BuildBotLocale"
    "fixture/BuildBotLocale.yaml"

requestDescribeResourcePolicy :: DescribeResourcePolicy -> TestTree
requestDescribeResourcePolicy =
  req
    "DescribeResourcePolicy"
    "fixture/DescribeResourcePolicy.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion =
  req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestDescribeBotAlias :: DescribeBotAlias -> TestTree
requestDescribeBotAlias =
  req
    "DescribeBotAlias"
    "fixture/DescribeBotAlias.yaml"

requestUpdateSlotType :: UpdateSlotType -> TestTree
requestUpdateSlotType =
  req
    "UpdateSlotType"
    "fixture/UpdateSlotType.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType =
  req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestCreateBotLocale :: CreateBotLocale -> TestTree
requestCreateBotLocale =
  req
    "CreateBotLocale"
    "fixture/CreateBotLocale.yaml"

requestListSlotTypes :: ListSlotTypes -> TestTree
requestListSlotTypes =
  req
    "ListSlotTypes"
    "fixture/ListSlotTypes.yaml"

requestDeleteExport :: DeleteExport -> TestTree
requestDeleteExport =
  req
    "DeleteExport"
    "fixture/DeleteExport.yaml"

requestStartImport :: StartImport -> TestTree
requestStartImport =
  req
    "StartImport"
    "fixture/StartImport.yaml"

requestUpdateExport :: UpdateExport -> TestTree
requestUpdateExport =
  req
    "UpdateExport"
    "fixture/UpdateExport.yaml"

requestCreateBot :: CreateBot -> TestTree
requestCreateBot =
  req
    "CreateBot"
    "fixture/CreateBot.yaml"

requestListExports :: ListExports -> TestTree
requestListExports =
  req
    "ListExports"
    "fixture/ListExports.yaml"

requestCreateSlotType :: CreateSlotType -> TestTree
requestCreateSlotType =
  req
    "CreateSlotType"
    "fixture/CreateSlotType.yaml"

requestCreateExport :: CreateExport -> TestTree
requestCreateExport =
  req
    "CreateExport"
    "fixture/CreateExport.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListBotAliases :: ListBotAliases -> TestTree
requestListBotAliases =
  req
    "ListBotAliases"
    "fixture/ListBotAliases.yaml"

requestCreateBotAlias :: CreateBotAlias -> TestTree
requestCreateBotAlias =
  req
    "CreateBotAlias"
    "fixture/CreateBotAlias.yaml"

requestCreateResourcePolicyStatement :: CreateResourcePolicyStatement -> TestTree
requestCreateResourcePolicyStatement =
  req
    "CreateResourcePolicyStatement"
    "fixture/CreateResourcePolicyStatement.yaml"

requestUpdateResourcePolicy :: UpdateResourcePolicy -> TestTree
requestUpdateResourcePolicy =
  req
    "UpdateResourcePolicy"
    "fixture/UpdateResourcePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteResourcePolicyStatement :: DeleteResourcePolicyStatement -> TestTree
requestDeleteResourcePolicyStatement =
  req
    "DeleteResourcePolicyStatement"
    "fixture/DeleteResourcePolicyStatement.yaml"

requestDescribeSlotType :: DescribeSlotType -> TestTree
requestDescribeSlotType =
  req
    "DescribeSlotType"
    "fixture/DescribeSlotType.yaml"

requestDescribeExport :: DescribeExport -> TestTree
requestDescribeExport =
  req
    "DescribeExport"
    "fixture/DescribeExport.yaml"

-- Responses

responseDeleteImport :: DeleteImportResponse -> TestTree
responseDeleteImport =
  res
    "DeleteImportResponse"
    "fixture/DeleteImportResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImport)

responseDescribeBot :: DescribeBotResponse -> TestTree
responseDescribeBot =
  res
    "DescribeBotResponse"
    "fixture/DescribeBotResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBot)

responseDescribeBotLocale :: DescribeBotLocaleResponse -> TestTree
responseDescribeBotLocale =
  res
    "DescribeBotLocaleResponse"
    "fixture/DescribeBotLocaleResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBotLocale)

responseListBuiltInSlotTypes :: ListBuiltInSlotTypesResponse -> TestTree
responseListBuiltInSlotTypes =
  res
    "ListBuiltInSlotTypesResponse"
    "fixture/ListBuiltInSlotTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuiltInSlotTypes)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseListBotVersions :: ListBotVersionsResponse -> TestTree
responseListBotVersions =
  res
    "ListBotVersionsResponse"
    "fixture/ListBotVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBotVersions)

responseListIntents :: ListIntentsResponse -> TestTree
responseListIntents =
  res
    "ListIntentsResponse"
    "fixture/ListIntentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListIntents)

responseListSlots :: ListSlotsResponse -> TestTree
responseListSlots =
  res
    "ListSlotsResponse"
    "fixture/ListSlotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSlots)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntent)

responseDeleteSlot :: DeleteSlotResponse -> TestTree
responseDeleteSlot =
  res
    "DeleteSlotResponse"
    "fixture/DeleteSlotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSlot)

responseUpdateIntent :: UpdateIntentResponse -> TestTree
responseUpdateIntent =
  res
    "UpdateIntentResponse"
    "fixture/UpdateIntentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateIntent)

responseUpdateSlot :: UpdateSlotResponse -> TestTree
responseUpdateSlot =
  res
    "UpdateSlotResponse"
    "fixture/UpdateSlotResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSlot)

responseCreateSlot :: CreateSlotResponse -> TestTree
responseCreateSlot =
  res
    "CreateSlotResponse"
    "fixture/CreateSlotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSlot)

responseListBots :: ListBotsResponse -> TestTree
responseListBots =
  res
    "ListBotsResponse"
    "fixture/ListBotsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBots)

responseDeleteBotLocale :: DeleteBotLocaleResponse -> TestTree
responseDeleteBotLocale =
  res
    "DeleteBotLocaleResponse"
    "fixture/DeleteBotLocaleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotLocale)

responseUpdateBotLocale :: UpdateBotLocaleResponse -> TestTree
responseUpdateBotLocale =
  res
    "UpdateBotLocaleResponse"
    "fixture/UpdateBotLocaleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBotLocale)

responseCreateIntent :: CreateIntentResponse -> TestTree
responseCreateIntent =
  res
    "CreateIntentResponse"
    "fixture/CreateIntentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIntent)

responseDescribeImport :: DescribeImportResponse -> TestTree
responseDescribeImport =
  res
    "DescribeImportResponse"
    "fixture/DescribeImportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImport)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot =
  res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBot)

responseUpdateBot :: UpdateBotResponse -> TestTree
responseUpdateBot =
  res
    "UpdateBotResponse"
    "fixture/UpdateBotResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBot)

responseListBotLocales :: ListBotLocalesResponse -> TestTree
responseListBotLocales =
  res
    "ListBotLocalesResponse"
    "fixture/ListBotLocalesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBotLocales)

responseCreateResourcePolicy :: CreateResourcePolicyResponse -> TestTree
responseCreateResourcePolicy =
  res
    "CreateResourcePolicyResponse"
    "fixture/CreateResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourcePolicy)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias =
  res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotAlias)

responseUpdateBotAlias :: UpdateBotAliasResponse -> TestTree
responseUpdateBotAlias =
  res
    "UpdateBotAliasResponse"
    "fixture/UpdateBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateBotAlias)

responseDescribeBotVersion :: DescribeBotVersionResponse -> TestTree
responseDescribeBotVersion =
  res
    "DescribeBotVersionResponse"
    "fixture/DescribeBotVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBotVersion)

responseDescribeSlot :: DescribeSlotResponse -> TestTree
responseDescribeSlot =
  res
    "DescribeSlotResponse"
    "fixture/DescribeSlotResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSlot)

responseDescribeIntent :: DescribeIntentResponse -> TestTree
responseDescribeIntent =
  res
    "DescribeIntentResponse"
    "fixture/DescribeIntentResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIntent)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUtterances)

responseCreateUploadUrl :: CreateUploadUrlResponse -> TestTree
responseCreateUploadUrl =
  res
    "CreateUploadUrlResponse"
    "fixture/CreateUploadUrlResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUploadUrl)

responseListBuiltInIntents :: ListBuiltInIntentsResponse -> TestTree
responseListBuiltInIntents =
  res
    "ListBuiltInIntentsResponse"
    "fixture/ListBuiltInIntentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBuiltInIntents)

responseListImports :: ListImportsResponse -> TestTree
responseListImports =
  res
    "ListImportsResponse"
    "fixture/ListImportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListImports)

responseListAggregatedUtterances :: ListAggregatedUtterancesResponse -> TestTree
responseListAggregatedUtterances =
  res
    "ListAggregatedUtterancesResponse"
    "fixture/ListAggregatedUtterancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAggregatedUtterances)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBotVersion)

responseBuildBotLocale :: BuildBotLocaleResponse -> TestTree
responseBuildBotLocale =
  res
    "BuildBotLocaleResponse"
    "fixture/BuildBotLocaleResponse.proto"
    defaultService
    (Proxy :: Proxy BuildBotLocale)

responseDescribeResourcePolicy :: DescribeResourcePolicyResponse -> TestTree
responseDescribeResourcePolicy =
  res
    "DescribeResourcePolicyResponse"
    "fixture/DescribeResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResourcePolicy)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotVersion)

responseDescribeBotAlias :: DescribeBotAliasResponse -> TestTree
responseDescribeBotAlias =
  res
    "DescribeBotAliasResponse"
    "fixture/DescribeBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeBotAlias)

responseUpdateSlotType :: UpdateSlotTypeResponse -> TestTree
responseUpdateSlotType =
  res
    "UpdateSlotTypeResponse"
    "fixture/UpdateSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSlotType)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSlotType)

responseCreateBotLocale :: CreateBotLocaleResponse -> TestTree
responseCreateBotLocale =
  res
    "CreateBotLocaleResponse"
    "fixture/CreateBotLocaleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBotLocale)

responseListSlotTypes :: ListSlotTypesResponse -> TestTree
responseListSlotTypes =
  res
    "ListSlotTypesResponse"
    "fixture/ListSlotTypesResponse.proto"
    defaultService
    (Proxy :: Proxy ListSlotTypes)

responseDeleteExport :: DeleteExportResponse -> TestTree
responseDeleteExport =
  res
    "DeleteExportResponse"
    "fixture/DeleteExportResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteExport)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    defaultService
    (Proxy :: Proxy StartImport)

responseUpdateExport :: UpdateExportResponse -> TestTree
responseUpdateExport =
  res
    "UpdateExportResponse"
    "fixture/UpdateExportResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateExport)

responseCreateBot :: CreateBotResponse -> TestTree
responseCreateBot =
  res
    "CreateBotResponse"
    "fixture/CreateBotResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBot)

responseListExports :: ListExportsResponse -> TestTree
responseListExports =
  res
    "ListExportsResponse"
    "fixture/ListExportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListExports)

responseCreateSlotType :: CreateSlotTypeResponse -> TestTree
responseCreateSlotType =
  res
    "CreateSlotTypeResponse"
    "fixture/CreateSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSlotType)

responseCreateExport :: CreateExportResponse -> TestTree
responseCreateExport =
  res
    "CreateExportResponse"
    "fixture/CreateExportResponse.proto"
    defaultService
    (Proxy :: Proxy CreateExport)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListBotAliases :: ListBotAliasesResponse -> TestTree
responseListBotAliases =
  res
    "ListBotAliasesResponse"
    "fixture/ListBotAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListBotAliases)

responseCreateBotAlias :: CreateBotAliasResponse -> TestTree
responseCreateBotAlias =
  res
    "CreateBotAliasResponse"
    "fixture/CreateBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBotAlias)

responseCreateResourcePolicyStatement :: CreateResourcePolicyStatementResponse -> TestTree
responseCreateResourcePolicyStatement =
  res
    "CreateResourcePolicyStatementResponse"
    "fixture/CreateResourcePolicyStatementResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourcePolicyStatement)

responseUpdateResourcePolicy :: UpdateResourcePolicyResponse -> TestTree
responseUpdateResourcePolicy =
  res
    "UpdateResourcePolicyResponse"
    "fixture/UpdateResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourcePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicy)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteResourcePolicyStatement :: DeleteResourcePolicyStatementResponse -> TestTree
responseDeleteResourcePolicyStatement =
  res
    "DeleteResourcePolicyStatementResponse"
    "fixture/DeleteResourcePolicyStatementResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourcePolicyStatement)

responseDescribeSlotType :: DescribeSlotTypeResponse -> TestTree
responseDescribeSlotType =
  res
    "DescribeSlotTypeResponse"
    "fixture/DescribeSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSlotType)

responseDescribeExport :: DescribeExportResponse -> TestTree
responseDescribeExport =
  res
    "DescribeExportResponse"
    "fixture/DescribeExportResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeExport)
