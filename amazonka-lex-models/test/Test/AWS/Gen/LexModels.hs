{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.LexModels where

import Data.Proxy
import Network.AWS.LexModels
import Test.AWS.Fixture
import Test.AWS.LexModels.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersion
--
--         , requestGetBots $
--             newGetBots
--
--         , requestGetSlotTypes $
--             newGetSlotTypes
--
--         , requestDeleteUtterances $
--             newDeleteUtterances
--
--         , requestGetBotAlias $
--             newGetBotAlias
--
--         , requestGetBotChannelAssociations $
--             newGetBotChannelAssociations
--
--         , requestPutBotAlias $
--             newPutBotAlias
--
--         , requestGetUtterancesView $
--             newGetUtterancesView
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetBuiltinIntent $
--             newGetBuiltinIntent
--
--         , requestGetSlotTypeVersions $
--             newGetSlotTypeVersions
--
--         , requestGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypes
--
--         , requestPutBot $
--             newPutBot
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDeleteSlotType $
--             newDeleteSlotType
--
--         , requestPutIntent $
--             newPutIntent
--
--         , requestGetBotChannelAssociation $
--             newGetBotChannelAssociation
--
--         , requestCreateIntentVersion $
--             newCreateIntentVersion
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetSlotType $
--             newGetSlotType
--
--         , requestDeleteIntentVersion $
--             newDeleteIntentVersion
--
--         , requestCreateBotVersion $
--             newCreateBotVersion
--
--         , requestGetBot $
--             newGetBot
--
--         , requestGetBotAliases $
--             newGetBotAliases
--
--         , requestGetIntents $
--             newGetIntents
--
--         , requestGetBotVersions $
--             newGetBotVersions
--
--         , requestDeleteBotAlias $
--             newDeleteBotAlias
--
--         , requestGetImport $
--             newGetImport
--
--         , requestGetIntentVersions $
--             newGetIntentVersions
--
--         , requestGetBuiltinIntents $
--             newGetBuiltinIntents
--
--         , requestDeleteBot $
--             newDeleteBot
--
--         , requestPutSlotType $
--             newPutSlotType
--
--         , requestStartImport $
--             newStartImport
--
--         , requestDeleteIntent $
--             newDeleteIntent
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateSlotTypeVersion $
--             newCreateSlotTypeVersion
--
--         , requestGetIntent $
--             newGetIntent
--
--         , requestDeleteBotVersion $
--             newDeleteBotVersion
--
--         , requestDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociation
--
--           ]

--     , testGroup "response"
--         [ responseDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersionResponse
--
--         , responseGetBots $
--             newGetBotsResponse
--
--         , responseGetSlotTypes $
--             newGetSlotTypesResponse
--
--         , responseDeleteUtterances $
--             newDeleteUtterancesResponse
--
--         , responseGetBotAlias $
--             newGetBotAliasResponse
--
--         , responseGetBotChannelAssociations $
--             newGetBotChannelAssociationsResponse
--
--         , responsePutBotAlias $
--             newPutBotAliasResponse
--
--         , responseGetUtterancesView $
--             newGetUtterancesViewResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetBuiltinIntent $
--             newGetBuiltinIntentResponse
--
--         , responseGetSlotTypeVersions $
--             newGetSlotTypeVersionsResponse
--
--         , responseGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypesResponse
--
--         , responsePutBot $
--             newPutBotResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDeleteSlotType $
--             newDeleteSlotTypeResponse
--
--         , responsePutIntent $
--             newPutIntentResponse
--
--         , responseGetBotChannelAssociation $
--             newGetBotChannelAssociationResponse
--
--         , responseCreateIntentVersion $
--             newCreateIntentVersionResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetSlotType $
--             newGetSlotTypeResponse
--
--         , responseDeleteIntentVersion $
--             newDeleteIntentVersionResponse
--
--         , responseCreateBotVersion $
--             newCreateBotVersionResponse
--
--         , responseGetBot $
--             newGetBotResponse
--
--         , responseGetBotAliases $
--             newGetBotAliasesResponse
--
--         , responseGetIntents $
--             newGetIntentsResponse
--
--         , responseGetBotVersions $
--             newGetBotVersionsResponse
--
--         , responseDeleteBotAlias $
--             newDeleteBotAliasResponse
--
--         , responseGetImport $
--             newGetImportResponse
--
--         , responseGetIntentVersions $
--             newGetIntentVersionsResponse
--
--         , responseGetBuiltinIntents $
--             newGetBuiltinIntentsResponse
--
--         , responseDeleteBot $
--             newDeleteBotResponse
--
--         , responsePutSlotType $
--             newPutSlotTypeResponse
--
--         , responseStartImport $
--             newStartImportResponse
--
--         , responseDeleteIntent $
--             newDeleteIntentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateSlotTypeVersion $
--             newCreateSlotTypeVersionResponse
--
--         , responseGetIntent $
--             newGetIntentResponse
--
--         , responseDeleteBotVersion $
--             newDeleteBotVersionResponse
--
--         , responseDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociationResponse
--
--           ]
--     ]

-- Requests

requestDeleteSlotTypeVersion :: DeleteSlotTypeVersion -> TestTree
requestDeleteSlotTypeVersion =
  req
    "DeleteSlotTypeVersion"
    "fixture/DeleteSlotTypeVersion.yaml"

requestGetBots :: GetBots -> TestTree
requestGetBots =
  req
    "GetBots"
    "fixture/GetBots.yaml"

requestGetSlotTypes :: GetSlotTypes -> TestTree
requestGetSlotTypes =
  req
    "GetSlotTypes"
    "fixture/GetSlotTypes.yaml"

requestDeleteUtterances :: DeleteUtterances -> TestTree
requestDeleteUtterances =
  req
    "DeleteUtterances"
    "fixture/DeleteUtterances.yaml"

requestGetBotAlias :: GetBotAlias -> TestTree
requestGetBotAlias =
  req
    "GetBotAlias"
    "fixture/GetBotAlias.yaml"

requestGetBotChannelAssociations :: GetBotChannelAssociations -> TestTree
requestGetBotChannelAssociations =
  req
    "GetBotChannelAssociations"
    "fixture/GetBotChannelAssociations.yaml"

requestPutBotAlias :: PutBotAlias -> TestTree
requestPutBotAlias =
  req
    "PutBotAlias"
    "fixture/PutBotAlias.yaml"

requestGetUtterancesView :: GetUtterancesView -> TestTree
requestGetUtterancesView =
  req
    "GetUtterancesView"
    "fixture/GetUtterancesView.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetBuiltinIntent :: GetBuiltinIntent -> TestTree
requestGetBuiltinIntent =
  req
    "GetBuiltinIntent"
    "fixture/GetBuiltinIntent.yaml"

requestGetSlotTypeVersions :: GetSlotTypeVersions -> TestTree
requestGetSlotTypeVersions =
  req
    "GetSlotTypeVersions"
    "fixture/GetSlotTypeVersions.yaml"

requestGetBuiltinSlotTypes :: GetBuiltinSlotTypes -> TestTree
requestGetBuiltinSlotTypes =
  req
    "GetBuiltinSlotTypes"
    "fixture/GetBuiltinSlotTypes.yaml"

requestPutBot :: PutBot -> TestTree
requestPutBot =
  req
    "PutBot"
    "fixture/PutBot.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType =
  req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestPutIntent :: PutIntent -> TestTree
requestPutIntent =
  req
    "PutIntent"
    "fixture/PutIntent.yaml"

requestGetBotChannelAssociation :: GetBotChannelAssociation -> TestTree
requestGetBotChannelAssociation =
  req
    "GetBotChannelAssociation"
    "fixture/GetBotChannelAssociation.yaml"

requestCreateIntentVersion :: CreateIntentVersion -> TestTree
requestCreateIntentVersion =
  req
    "CreateIntentVersion"
    "fixture/CreateIntentVersion.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetSlotType :: GetSlotType -> TestTree
requestGetSlotType =
  req
    "GetSlotType"
    "fixture/GetSlotType.yaml"

requestDeleteIntentVersion :: DeleteIntentVersion -> TestTree
requestDeleteIntentVersion =
  req
    "DeleteIntentVersion"
    "fixture/DeleteIntentVersion.yaml"

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion =
  req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot =
  req
    "GetBot"
    "fixture/GetBot.yaml"

requestGetBotAliases :: GetBotAliases -> TestTree
requestGetBotAliases =
  req
    "GetBotAliases"
    "fixture/GetBotAliases.yaml"

requestGetIntents :: GetIntents -> TestTree
requestGetIntents =
  req
    "GetIntents"
    "fixture/GetIntents.yaml"

requestGetBotVersions :: GetBotVersions -> TestTree
requestGetBotVersions =
  req
    "GetBotVersions"
    "fixture/GetBotVersions.yaml"

requestDeleteBotAlias :: DeleteBotAlias -> TestTree
requestDeleteBotAlias =
  req
    "DeleteBotAlias"
    "fixture/DeleteBotAlias.yaml"

requestGetImport :: GetImport -> TestTree
requestGetImport =
  req
    "GetImport"
    "fixture/GetImport.yaml"

requestGetIntentVersions :: GetIntentVersions -> TestTree
requestGetIntentVersions =
  req
    "GetIntentVersions"
    "fixture/GetIntentVersions.yaml"

requestGetBuiltinIntents :: GetBuiltinIntents -> TestTree
requestGetBuiltinIntents =
  req
    "GetBuiltinIntents"
    "fixture/GetBuiltinIntents.yaml"

requestDeleteBot :: DeleteBot -> TestTree
requestDeleteBot =
  req
    "DeleteBot"
    "fixture/DeleteBot.yaml"

requestPutSlotType :: PutSlotType -> TestTree
requestPutSlotType =
  req
    "PutSlotType"
    "fixture/PutSlotType.yaml"

requestStartImport :: StartImport -> TestTree
requestStartImport =
  req
    "StartImport"
    "fixture/StartImport.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent =
  req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateSlotTypeVersion :: CreateSlotTypeVersion -> TestTree
requestCreateSlotTypeVersion =
  req
    "CreateSlotTypeVersion"
    "fixture/CreateSlotTypeVersion.yaml"

requestGetIntent :: GetIntent -> TestTree
requestGetIntent =
  req
    "GetIntent"
    "fixture/GetIntent.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion =
  req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestDeleteBotChannelAssociation :: DeleteBotChannelAssociation -> TestTree
requestDeleteBotChannelAssociation =
  req
    "DeleteBotChannelAssociation"
    "fixture/DeleteBotChannelAssociation.yaml"

-- Responses

responseDeleteSlotTypeVersion :: DeleteSlotTypeVersionResponse -> TestTree
responseDeleteSlotTypeVersion =
  res
    "DeleteSlotTypeVersionResponse"
    "fixture/DeleteSlotTypeVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSlotTypeVersion)

responseGetBots :: GetBotsResponse -> TestTree
responseGetBots =
  res
    "GetBotsResponse"
    "fixture/GetBotsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBots)

responseGetSlotTypes :: GetSlotTypesResponse -> TestTree
responseGetSlotTypes =
  res
    "GetSlotTypesResponse"
    "fixture/GetSlotTypesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSlotTypes)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUtterances)

responseGetBotAlias :: GetBotAliasResponse -> TestTree
responseGetBotAlias =
  res
    "GetBotAliasResponse"
    "fixture/GetBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy GetBotAlias)

responseGetBotChannelAssociations :: GetBotChannelAssociationsResponse -> TestTree
responseGetBotChannelAssociations =
  res
    "GetBotChannelAssociationsResponse"
    "fixture/GetBotChannelAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBotChannelAssociations)

responsePutBotAlias :: PutBotAliasResponse -> TestTree
responsePutBotAlias =
  res
    "PutBotAliasResponse"
    "fixture/PutBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy PutBotAlias)

responseGetUtterancesView :: GetUtterancesViewResponse -> TestTree
responseGetUtterancesView =
  res
    "GetUtterancesViewResponse"
    "fixture/GetUtterancesViewResponse.proto"
    defaultService
    (Proxy :: Proxy GetUtterancesView)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetBuiltinIntent :: GetBuiltinIntentResponse -> TestTree
responseGetBuiltinIntent =
  res
    "GetBuiltinIntentResponse"
    "fixture/GetBuiltinIntentResponse.proto"
    defaultService
    (Proxy :: Proxy GetBuiltinIntent)

responseGetSlotTypeVersions :: GetSlotTypeVersionsResponse -> TestTree
responseGetSlotTypeVersions =
  res
    "GetSlotTypeVersionsResponse"
    "fixture/GetSlotTypeVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetSlotTypeVersions)

responseGetBuiltinSlotTypes :: GetBuiltinSlotTypesResponse -> TestTree
responseGetBuiltinSlotTypes =
  res
    "GetBuiltinSlotTypesResponse"
    "fixture/GetBuiltinSlotTypesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBuiltinSlotTypes)

responsePutBot :: PutBotResponse -> TestTree
responsePutBot =
  res
    "PutBotResponse"
    "fixture/PutBotResponse.proto"
    defaultService
    (Proxy :: Proxy PutBot)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSlotType)

responsePutIntent :: PutIntentResponse -> TestTree
responsePutIntent =
  res
    "PutIntentResponse"
    "fixture/PutIntentResponse.proto"
    defaultService
    (Proxy :: Proxy PutIntent)

responseGetBotChannelAssociation :: GetBotChannelAssociationResponse -> TestTree
responseGetBotChannelAssociation =
  res
    "GetBotChannelAssociationResponse"
    "fixture/GetBotChannelAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy GetBotChannelAssociation)

responseCreateIntentVersion :: CreateIntentVersionResponse -> TestTree
responseCreateIntentVersion =
  res
    "CreateIntentVersionResponse"
    "fixture/CreateIntentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateIntentVersion)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy :: Proxy GetExport)

responseGetSlotType :: GetSlotTypeResponse -> TestTree
responseGetSlotType =
  res
    "GetSlotTypeResponse"
    "fixture/GetSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy GetSlotType)

responseDeleteIntentVersion :: DeleteIntentVersionResponse -> TestTree
responseDeleteIntentVersion =
  res
    "DeleteIntentVersionResponse"
    "fixture/DeleteIntentVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntentVersion)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateBotVersion)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    defaultService
    (Proxy :: Proxy GetBot)

responseGetBotAliases :: GetBotAliasesResponse -> TestTree
responseGetBotAliases =
  res
    "GetBotAliasesResponse"
    "fixture/GetBotAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy GetBotAliases)

responseGetIntents :: GetIntentsResponse -> TestTree
responseGetIntents =
  res
    "GetIntentsResponse"
    "fixture/GetIntentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntents)

responseGetBotVersions :: GetBotVersionsResponse -> TestTree
responseGetBotVersions =
  res
    "GetBotVersionsResponse"
    "fixture/GetBotVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBotVersions)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias =
  res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotAlias)

responseGetImport :: GetImportResponse -> TestTree
responseGetImport =
  res
    "GetImportResponse"
    "fixture/GetImportResponse.proto"
    defaultService
    (Proxy :: Proxy GetImport)

responseGetIntentVersions :: GetIntentVersionsResponse -> TestTree
responseGetIntentVersions =
  res
    "GetIntentVersionsResponse"
    "fixture/GetIntentVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntentVersions)

responseGetBuiltinIntents :: GetBuiltinIntentsResponse -> TestTree
responseGetBuiltinIntents =
  res
    "GetBuiltinIntentsResponse"
    "fixture/GetBuiltinIntentsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBuiltinIntents)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot =
  res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBot)

responsePutSlotType :: PutSlotTypeResponse -> TestTree
responsePutSlotType =
  res
    "PutSlotTypeResponse"
    "fixture/PutSlotTypeResponse.proto"
    defaultService
    (Proxy :: Proxy PutSlotType)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    defaultService
    (Proxy :: Proxy StartImport)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteIntent)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateSlotTypeVersion :: CreateSlotTypeVersionResponse -> TestTree
responseCreateSlotTypeVersion =
  res
    "CreateSlotTypeVersionResponse"
    "fixture/CreateSlotTypeVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSlotTypeVersion)

responseGetIntent :: GetIntentResponse -> TestTree
responseGetIntent =
  res
    "GetIntentResponse"
    "fixture/GetIntentResponse.proto"
    defaultService
    (Proxy :: Proxy GetIntent)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotVersion)

responseDeleteBotChannelAssociation :: DeleteBotChannelAssociationResponse -> TestTree
responseDeleteBotChannelAssociation =
  res
    "DeleteBotChannelAssociationResponse"
    "fixture/DeleteBotChannelAssociationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteBotChannelAssociation)
