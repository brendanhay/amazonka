{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexModels
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestDeleteIntentVersion $
--             deleteIntentVersion
--
--         , requestGetBotAliases $
--             getBotAliases
--
--         , requestDeleteBotChannelAssociation $
--             deleteBotChannelAssociation
--
--         , requestCreateSlotTypeVersion $
--             createSlotTypeVersion
--
--         , requestGetIntent $
--             getIntent
--
--         , requestPutIntent $
--             putIntent
--
--         , requestDeleteIntent $
--             deleteIntent
--
--         , requestGetBuiltinIntents $
--             getBuiltinIntents
--
--         , requestPutBot $
--             putBot
--
--         , requestDeleteBot $
--             deleteBot
--
--         , requestGetImport $
--             getImport
--
--         , requestGetIntentVersions $
--             getIntentVersions
--
--         , requestGetBuiltinIntent $
--             getBuiltinIntent
--
--         , requestPutBotAlias $
--             putBotAlias
--
--         , requestGetBotVersions $
--             getBotVersions
--
--         , requestGetBotChannelAssociations $
--             getBotChannelAssociations
--
--         , requestDeleteBotAlias $
--             deleteBotAlias
--
--         , requestGetSlotTypes $
--             getSlotTypes
--
--         , requestDeleteUtterances $
--             deleteUtterances
--
--         , requestGetBots $
--             getBots
--
--         , requestGetBot $
--             getBot
--
--         , requestCreateBotVersion $
--             createBotVersion
--
--         , requestDeleteSlotTypeVersion $
--             deleteSlotTypeVersion
--
--         , requestDeleteBotVersion $
--             deleteBotVersion
--
--         , requestGetSlotType $
--             getSlotType
--
--         , requestGetExport $
--             getExport
--
--         , requestCreateIntentVersion $
--             createIntentVersion
--
--         , requestDeleteSlotType $
--             deleteSlotType
--
--         , requestStartImport $
--             startImport
--
--         , requestGetBotChannelAssociation $
--             getBotChannelAssociation
--
--         , requestPutSlotType $
--             putSlotType
--
--         , requestGetBuiltinSlotTypes $
--             getBuiltinSlotTypes
--
--         , requestGetUtterancesView $
--             getUtterancesView
--
--         , requestGetSlotTypeVersions $
--             getSlotTypeVersions
--
--         , requestGetIntents $
--             getIntents
--
--         , requestGetBotAlias $
--             getBotAlias
--
--           ]

--     , testGroup "response"
--         [ responseDeleteIntentVersion $
--             deleteIntentVersionResponse
--
--         , responseGetBotAliases $
--             getBotAliasesResponse
--
--         , responseDeleteBotChannelAssociation $
--             deleteBotChannelAssociationResponse
--
--         , responseCreateSlotTypeVersion $
--             createSlotTypeVersionResponse
--
--         , responseGetIntent $
--             getIntentResponse
--
--         , responsePutIntent $
--             putIntentResponse
--
--         , responseDeleteIntent $
--             deleteIntentResponse
--
--         , responseGetBuiltinIntents $
--             getBuiltinIntentsResponse
--
--         , responsePutBot $
--             putBotResponse
--
--         , responseDeleteBot $
--             deleteBotResponse
--
--         , responseGetImport $
--             getImportResponse
--
--         , responseGetIntentVersions $
--             getIntentVersionsResponse
--
--         , responseGetBuiltinIntent $
--             getBuiltinIntentResponse
--
--         , responsePutBotAlias $
--             putBotAliasResponse
--
--         , responseGetBotVersions $
--             getBotVersionsResponse
--
--         , responseGetBotChannelAssociations $
--             getBotChannelAssociationsResponse
--
--         , responseDeleteBotAlias $
--             deleteBotAliasResponse
--
--         , responseGetSlotTypes $
--             getSlotTypesResponse
--
--         , responseDeleteUtterances $
--             deleteUtterancesResponse
--
--         , responseGetBots $
--             getBotsResponse
--
--         , responseGetBot $
--             getBotResponse
--
--         , responseCreateBotVersion $
--             createBotVersionResponse
--
--         , responseDeleteSlotTypeVersion $
--             deleteSlotTypeVersionResponse
--
--         , responseDeleteBotVersion $
--             deleteBotVersionResponse
--
--         , responseGetSlotType $
--             getSlotTypeResponse
--
--         , responseGetExport $
--             getExportResponse
--
--         , responseCreateIntentVersion $
--             createIntentVersionResponse
--
--         , responseDeleteSlotType $
--             deleteSlotTypeResponse
--
--         , responseStartImport $
--             startImportResponse
--
--         , responseGetBotChannelAssociation $
--             getBotChannelAssociationResponse
--
--         , responsePutSlotType $
--             putSlotTypeResponse
--
--         , responseGetBuiltinSlotTypes $
--             getBuiltinSlotTypesResponse
--
--         , responseGetUtterancesView $
--             getUtterancesViewResponse
--
--         , responseGetSlotTypeVersions $
--             getSlotTypeVersionsResponse
--
--         , responseGetIntents $
--             getIntentsResponse
--
--         , responseGetBotAlias $
--             getBotAliasResponse
--
--           ]
--     ]

-- Requests

requestDeleteIntentVersion :: DeleteIntentVersion -> TestTree
requestDeleteIntentVersion = req
    "DeleteIntentVersion"
    "fixture/DeleteIntentVersion.yaml"

requestGetBotAliases :: GetBotAliases -> TestTree
requestGetBotAliases = req
    "GetBotAliases"
    "fixture/GetBotAliases.yaml"

requestDeleteBotChannelAssociation :: DeleteBotChannelAssociation -> TestTree
requestDeleteBotChannelAssociation = req
    "DeleteBotChannelAssociation"
    "fixture/DeleteBotChannelAssociation.yaml"

requestCreateSlotTypeVersion :: CreateSlotTypeVersion -> TestTree
requestCreateSlotTypeVersion = req
    "CreateSlotTypeVersion"
    "fixture/CreateSlotTypeVersion.yaml"

requestGetIntent :: GetIntent -> TestTree
requestGetIntent = req
    "GetIntent"
    "fixture/GetIntent.yaml"

requestPutIntent :: PutIntent -> TestTree
requestPutIntent = req
    "PutIntent"
    "fixture/PutIntent.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent = req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestGetBuiltinIntents :: GetBuiltinIntents -> TestTree
requestGetBuiltinIntents = req
    "GetBuiltinIntents"
    "fixture/GetBuiltinIntents.yaml"

requestPutBot :: PutBot -> TestTree
requestPutBot = req
    "PutBot"
    "fixture/PutBot.yaml"

requestDeleteBot :: DeleteBot -> TestTree
requestDeleteBot = req
    "DeleteBot"
    "fixture/DeleteBot.yaml"

requestGetImport :: GetImport -> TestTree
requestGetImport = req
    "GetImport"
    "fixture/GetImport.yaml"

requestGetIntentVersions :: GetIntentVersions -> TestTree
requestGetIntentVersions = req
    "GetIntentVersions"
    "fixture/GetIntentVersions.yaml"

requestGetBuiltinIntent :: GetBuiltinIntent -> TestTree
requestGetBuiltinIntent = req
    "GetBuiltinIntent"
    "fixture/GetBuiltinIntent.yaml"

requestPutBotAlias :: PutBotAlias -> TestTree
requestPutBotAlias = req
    "PutBotAlias"
    "fixture/PutBotAlias.yaml"

requestGetBotVersions :: GetBotVersions -> TestTree
requestGetBotVersions = req
    "GetBotVersions"
    "fixture/GetBotVersions.yaml"

requestGetBotChannelAssociations :: GetBotChannelAssociations -> TestTree
requestGetBotChannelAssociations = req
    "GetBotChannelAssociations"
    "fixture/GetBotChannelAssociations.yaml"

requestDeleteBotAlias :: DeleteBotAlias -> TestTree
requestDeleteBotAlias = req
    "DeleteBotAlias"
    "fixture/DeleteBotAlias.yaml"

requestGetSlotTypes :: GetSlotTypes -> TestTree
requestGetSlotTypes = req
    "GetSlotTypes"
    "fixture/GetSlotTypes.yaml"

requestDeleteUtterances :: DeleteUtterances -> TestTree
requestDeleteUtterances = req
    "DeleteUtterances"
    "fixture/DeleteUtterances.yaml"

requestGetBots :: GetBots -> TestTree
requestGetBots = req
    "GetBots"
    "fixture/GetBots.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot = req
    "GetBot"
    "fixture/GetBot.yaml"

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion = req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestDeleteSlotTypeVersion :: DeleteSlotTypeVersion -> TestTree
requestDeleteSlotTypeVersion = req
    "DeleteSlotTypeVersion"
    "fixture/DeleteSlotTypeVersion.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion = req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestGetSlotType :: GetSlotType -> TestTree
requestGetSlotType = req
    "GetSlotType"
    "fixture/GetSlotType.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport = req
    "GetExport"
    "fixture/GetExport.yaml"

requestCreateIntentVersion :: CreateIntentVersion -> TestTree
requestCreateIntentVersion = req
    "CreateIntentVersion"
    "fixture/CreateIntentVersion.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType = req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestStartImport :: StartImport -> TestTree
requestStartImport = req
    "StartImport"
    "fixture/StartImport.yaml"

requestGetBotChannelAssociation :: GetBotChannelAssociation -> TestTree
requestGetBotChannelAssociation = req
    "GetBotChannelAssociation"
    "fixture/GetBotChannelAssociation.yaml"

requestPutSlotType :: PutSlotType -> TestTree
requestPutSlotType = req
    "PutSlotType"
    "fixture/PutSlotType.yaml"

requestGetBuiltinSlotTypes :: GetBuiltinSlotTypes -> TestTree
requestGetBuiltinSlotTypes = req
    "GetBuiltinSlotTypes"
    "fixture/GetBuiltinSlotTypes.yaml"

requestGetUtterancesView :: GetUtterancesView -> TestTree
requestGetUtterancesView = req
    "GetUtterancesView"
    "fixture/GetUtterancesView.yaml"

requestGetSlotTypeVersions :: GetSlotTypeVersions -> TestTree
requestGetSlotTypeVersions = req
    "GetSlotTypeVersions"
    "fixture/GetSlotTypeVersions.yaml"

requestGetIntents :: GetIntents -> TestTree
requestGetIntents = req
    "GetIntents"
    "fixture/GetIntents.yaml"

requestGetBotAlias :: GetBotAlias -> TestTree
requestGetBotAlias = req
    "GetBotAlias"
    "fixture/GetBotAlias.yaml"

-- Responses

responseDeleteIntentVersion :: DeleteIntentVersionResponse -> TestTree
responseDeleteIntentVersion = res
    "DeleteIntentVersionResponse"
    "fixture/DeleteIntentVersionResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteIntentVersion)

responseGetBotAliases :: GetBotAliasesResponse -> TestTree
responseGetBotAliases = res
    "GetBotAliasesResponse"
    "fixture/GetBotAliasesResponse.proto"
    lexModels
    (Proxy :: Proxy GetBotAliases)

responseDeleteBotChannelAssociation :: DeleteBotChannelAssociationResponse -> TestTree
responseDeleteBotChannelAssociation = res
    "DeleteBotChannelAssociationResponse"
    "fixture/DeleteBotChannelAssociationResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteBotChannelAssociation)

responseCreateSlotTypeVersion :: CreateSlotTypeVersionResponse -> TestTree
responseCreateSlotTypeVersion = res
    "CreateSlotTypeVersionResponse"
    "fixture/CreateSlotTypeVersionResponse.proto"
    lexModels
    (Proxy :: Proxy CreateSlotTypeVersion)

responseGetIntent :: GetIntentResponse -> TestTree
responseGetIntent = res
    "GetIntentResponse"
    "fixture/GetIntentResponse.proto"
    lexModels
    (Proxy :: Proxy GetIntent)

responsePutIntent :: PutIntentResponse -> TestTree
responsePutIntent = res
    "PutIntentResponse"
    "fixture/PutIntentResponse.proto"
    lexModels
    (Proxy :: Proxy PutIntent)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent = res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteIntent)

responseGetBuiltinIntents :: GetBuiltinIntentsResponse -> TestTree
responseGetBuiltinIntents = res
    "GetBuiltinIntentsResponse"
    "fixture/GetBuiltinIntentsResponse.proto"
    lexModels
    (Proxy :: Proxy GetBuiltinIntents)

responsePutBot :: PutBotResponse -> TestTree
responsePutBot = res
    "PutBotResponse"
    "fixture/PutBotResponse.proto"
    lexModels
    (Proxy :: Proxy PutBot)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot = res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteBot)

responseGetImport :: GetImportResponse -> TestTree
responseGetImport = res
    "GetImportResponse"
    "fixture/GetImportResponse.proto"
    lexModels
    (Proxy :: Proxy GetImport)

responseGetIntentVersions :: GetIntentVersionsResponse -> TestTree
responseGetIntentVersions = res
    "GetIntentVersionsResponse"
    "fixture/GetIntentVersionsResponse.proto"
    lexModels
    (Proxy :: Proxy GetIntentVersions)

responseGetBuiltinIntent :: GetBuiltinIntentResponse -> TestTree
responseGetBuiltinIntent = res
    "GetBuiltinIntentResponse"
    "fixture/GetBuiltinIntentResponse.proto"
    lexModels
    (Proxy :: Proxy GetBuiltinIntent)

responsePutBotAlias :: PutBotAliasResponse -> TestTree
responsePutBotAlias = res
    "PutBotAliasResponse"
    "fixture/PutBotAliasResponse.proto"
    lexModels
    (Proxy :: Proxy PutBotAlias)

responseGetBotVersions :: GetBotVersionsResponse -> TestTree
responseGetBotVersions = res
    "GetBotVersionsResponse"
    "fixture/GetBotVersionsResponse.proto"
    lexModels
    (Proxy :: Proxy GetBotVersions)

responseGetBotChannelAssociations :: GetBotChannelAssociationsResponse -> TestTree
responseGetBotChannelAssociations = res
    "GetBotChannelAssociationsResponse"
    "fixture/GetBotChannelAssociationsResponse.proto"
    lexModels
    (Proxy :: Proxy GetBotChannelAssociations)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias = res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteBotAlias)

responseGetSlotTypes :: GetSlotTypesResponse -> TestTree
responseGetSlotTypes = res
    "GetSlotTypesResponse"
    "fixture/GetSlotTypesResponse.proto"
    lexModels
    (Proxy :: Proxy GetSlotTypes)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances = res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteUtterances)

responseGetBots :: GetBotsResponse -> TestTree
responseGetBots = res
    "GetBotsResponse"
    "fixture/GetBotsResponse.proto"
    lexModels
    (Proxy :: Proxy GetBots)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot = res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    lexModels
    (Proxy :: Proxy GetBot)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion = res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    lexModels
    (Proxy :: Proxy CreateBotVersion)

responseDeleteSlotTypeVersion :: DeleteSlotTypeVersionResponse -> TestTree
responseDeleteSlotTypeVersion = res
    "DeleteSlotTypeVersionResponse"
    "fixture/DeleteSlotTypeVersionResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteSlotTypeVersion)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion = res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteBotVersion)

responseGetSlotType :: GetSlotTypeResponse -> TestTree
responseGetSlotType = res
    "GetSlotTypeResponse"
    "fixture/GetSlotTypeResponse.proto"
    lexModels
    (Proxy :: Proxy GetSlotType)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport = res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    lexModels
    (Proxy :: Proxy GetExport)

responseCreateIntentVersion :: CreateIntentVersionResponse -> TestTree
responseCreateIntentVersion = res
    "CreateIntentVersionResponse"
    "fixture/CreateIntentVersionResponse.proto"
    lexModels
    (Proxy :: Proxy CreateIntentVersion)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType = res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    lexModels
    (Proxy :: Proxy DeleteSlotType)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport = res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    lexModels
    (Proxy :: Proxy StartImport)

responseGetBotChannelAssociation :: GetBotChannelAssociationResponse -> TestTree
responseGetBotChannelAssociation = res
    "GetBotChannelAssociationResponse"
    "fixture/GetBotChannelAssociationResponse.proto"
    lexModels
    (Proxy :: Proxy GetBotChannelAssociation)

responsePutSlotType :: PutSlotTypeResponse -> TestTree
responsePutSlotType = res
    "PutSlotTypeResponse"
    "fixture/PutSlotTypeResponse.proto"
    lexModels
    (Proxy :: Proxy PutSlotType)

responseGetBuiltinSlotTypes :: GetBuiltinSlotTypesResponse -> TestTree
responseGetBuiltinSlotTypes = res
    "GetBuiltinSlotTypesResponse"
    "fixture/GetBuiltinSlotTypesResponse.proto"
    lexModels
    (Proxy :: Proxy GetBuiltinSlotTypes)

responseGetUtterancesView :: GetUtterancesViewResponse -> TestTree
responseGetUtterancesView = res
    "GetUtterancesViewResponse"
    "fixture/GetUtterancesViewResponse.proto"
    lexModels
    (Proxy :: Proxy GetUtterancesView)

responseGetSlotTypeVersions :: GetSlotTypeVersionsResponse -> TestTree
responseGetSlotTypeVersions = res
    "GetSlotTypeVersionsResponse"
    "fixture/GetSlotTypeVersionsResponse.proto"
    lexModels
    (Proxy :: Proxy GetSlotTypeVersions)

responseGetIntents :: GetIntentsResponse -> TestTree
responseGetIntents = res
    "GetIntentsResponse"
    "fixture/GetIntentsResponse.proto"
    lexModels
    (Proxy :: Proxy GetIntents)

responseGetBotAlias :: GetBotAliasResponse -> TestTree
responseGetBotAlias = res
    "GetBotAliasResponse"
    "fixture/GetBotAliasResponse.proto"
    lexModels
    (Proxy :: Proxy GetBotAlias)
