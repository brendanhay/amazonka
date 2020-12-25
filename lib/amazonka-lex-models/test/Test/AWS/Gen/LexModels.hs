{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.LexModels
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestDeleteIntentVersion $
--             mkDeleteIntentVersion
--
--         , requestGetBotAliases $
--             mkGetBotAliases
--
--         , requestDeleteBotChannelAssociation $
--             mkDeleteBotChannelAssociation
--
--         , requestCreateSlotTypeVersion $
--             mkCreateSlotTypeVersion
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestGetIntent $
--             mkGetIntent
--
--         , requestPutIntent $
--             mkPutIntent
--
--         , requestDeleteIntent $
--             mkDeleteIntent
--
--         , requestGetBuiltinIntents $
--             mkGetBuiltinIntents
--
--         , requestPutBot $
--             mkPutBot
--
--         , requestDeleteBot $
--             mkDeleteBot
--
--         , requestGetImport $
--             mkGetImport
--
--         , requestGetIntentVersions $
--             mkGetIntentVersions
--
--         , requestGetBuiltinIntent $
--             mkGetBuiltinIntent
--
--         , requestPutBotAlias $
--             mkPutBotAlias
--
--         , requestGetBotVersions $
--             mkGetBotVersions
--
--         , requestGetBotChannelAssociations $
--             mkGetBotChannelAssociations
--
--         , requestDeleteBotAlias $
--             mkDeleteBotAlias
--
--         , requestGetSlotTypes $
--             mkGetSlotTypes
--
--         , requestDeleteUtterances $
--             mkDeleteUtterances
--
--         , requestGetBots $
--             mkGetBots
--
--         , requestGetBot $
--             mkGetBot
--
--         , requestCreateBotVersion $
--             mkCreateBotVersion
--
--         , requestDeleteSlotTypeVersion $
--             mkDeleteSlotTypeVersion
--
--         , requestDeleteBotVersion $
--             mkDeleteBotVersion
--
--         , requestGetSlotType $
--             mkGetSlotType
--
--         , requestGetExport $
--             mkGetExport
--
--         , requestCreateIntentVersion $
--             mkCreateIntentVersion
--
--         , requestDeleteSlotType $
--             mkDeleteSlotType
--
--         , requestStartImport $
--             mkStartImport
--
--         , requestGetBotChannelAssociation $
--             mkGetBotChannelAssociation
--
--         , requestPutSlotType $
--             mkPutSlotType
--
--         , requestGetBuiltinSlotTypes $
--             mkGetBuiltinSlotTypes
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestGetUtterancesView $
--             mkGetUtterancesView
--
--         , requestGetSlotTypeVersions $
--             mkGetSlotTypeVersions
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestGetIntents $
--             mkGetIntents
--
--         , requestGetBotAlias $
--             mkGetBotAlias
--
--           ]

--     , testGroup "response"
--         [ responseDeleteIntentVersion $
--             mkDeleteIntentVersionResponse
--
--         , responseGetBotAliases $
--             mkGetBotAliasesResponse
--
--         , responseDeleteBotChannelAssociation $
--             mkDeleteBotChannelAssociationResponse
--
--         , responseCreateSlotTypeVersion $
--             mkCreateSlotTypeVersionResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseGetIntent $
--             mkGetIntentResponse
--
--         , responsePutIntent $
--             mkPutIntentResponse
--
--         , responseDeleteIntent $
--             mkDeleteIntentResponse
--
--         , responseGetBuiltinIntents $
--             mkGetBuiltinIntentsResponse
--
--         , responsePutBot $
--             mkPutBotResponse
--
--         , responseDeleteBot $
--             mkDeleteBotResponse
--
--         , responseGetImport $
--             mkGetImportResponse
--
--         , responseGetIntentVersions $
--             mkGetIntentVersionsResponse
--
--         , responseGetBuiltinIntent $
--             mkGetBuiltinIntentResponse
--
--         , responsePutBotAlias $
--             mkPutBotAliasResponse
--
--         , responseGetBotVersions $
--             mkGetBotVersionsResponse
--
--         , responseGetBotChannelAssociations $
--             mkGetBotChannelAssociationsResponse
--
--         , responseDeleteBotAlias $
--             mkDeleteBotAliasResponse
--
--         , responseGetSlotTypes $
--             mkGetSlotTypesResponse
--
--         , responseDeleteUtterances $
--             mkDeleteUtterancesResponse
--
--         , responseGetBots $
--             mkGetBotsResponse
--
--         , responseGetBot $
--             mkGetBotResponse
--
--         , responseCreateBotVersion $
--             mkCreateBotVersionResponse
--
--         , responseDeleteSlotTypeVersion $
--             mkDeleteSlotTypeVersionResponse
--
--         , responseDeleteBotVersion $
--             mkDeleteBotVersionResponse
--
--         , responseGetSlotType $
--             mkGetSlotTypeResponse
--
--         , responseGetExport $
--             mkGetExportResponse
--
--         , responseCreateIntentVersion $
--             mkCreateIntentVersionResponse
--
--         , responseDeleteSlotType $
--             mkDeleteSlotTypeResponse
--
--         , responseStartImport $
--             mkStartImportResponse
--
--         , responseGetBotChannelAssociation $
--             mkGetBotChannelAssociationResponse
--
--         , responsePutSlotType $
--             mkPutSlotTypeResponse
--
--         , responseGetBuiltinSlotTypes $
--             mkGetBuiltinSlotTypesResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseGetUtterancesView $
--             mkGetUtterancesViewResponse
--
--         , responseGetSlotTypeVersions $
--             mkGetSlotTypeVersionsResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseGetIntents $
--             mkGetIntentsResponse
--
--         , responseGetBotAlias $
--             mkGetBotAliasResponse
--
--           ]
--     ]

-- Requests

requestDeleteIntentVersion :: DeleteIntentVersion -> TestTree
requestDeleteIntentVersion =
  req
    "DeleteIntentVersion"
    "fixture/DeleteIntentVersion.yaml"

requestGetBotAliases :: GetBotAliases -> TestTree
requestGetBotAliases =
  req
    "GetBotAliases"
    "fixture/GetBotAliases.yaml"

requestDeleteBotChannelAssociation :: DeleteBotChannelAssociation -> TestTree
requestDeleteBotChannelAssociation =
  req
    "DeleteBotChannelAssociation"
    "fixture/DeleteBotChannelAssociation.yaml"

requestCreateSlotTypeVersion :: CreateSlotTypeVersion -> TestTree
requestCreateSlotTypeVersion =
  req
    "CreateSlotTypeVersion"
    "fixture/CreateSlotTypeVersion.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetIntent :: GetIntent -> TestTree
requestGetIntent =
  req
    "GetIntent"
    "fixture/GetIntent.yaml"

requestPutIntent :: PutIntent -> TestTree
requestPutIntent =
  req
    "PutIntent"
    "fixture/PutIntent.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent =
  req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestGetBuiltinIntents :: GetBuiltinIntents -> TestTree
requestGetBuiltinIntents =
  req
    "GetBuiltinIntents"
    "fixture/GetBuiltinIntents.yaml"

requestPutBot :: PutBot -> TestTree
requestPutBot =
  req
    "PutBot"
    "fixture/PutBot.yaml"

requestDeleteBot :: DeleteBot -> TestTree
requestDeleteBot =
  req
    "DeleteBot"
    "fixture/DeleteBot.yaml"

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

requestGetBuiltinIntent :: GetBuiltinIntent -> TestTree
requestGetBuiltinIntent =
  req
    "GetBuiltinIntent"
    "fixture/GetBuiltinIntent.yaml"

requestPutBotAlias :: PutBotAlias -> TestTree
requestPutBotAlias =
  req
    "PutBotAlias"
    "fixture/PutBotAlias.yaml"

requestGetBotVersions :: GetBotVersions -> TestTree
requestGetBotVersions =
  req
    "GetBotVersions"
    "fixture/GetBotVersions.yaml"

requestGetBotChannelAssociations :: GetBotChannelAssociations -> TestTree
requestGetBotChannelAssociations =
  req
    "GetBotChannelAssociations"
    "fixture/GetBotChannelAssociations.yaml"

requestDeleteBotAlias :: DeleteBotAlias -> TestTree
requestDeleteBotAlias =
  req
    "DeleteBotAlias"
    "fixture/DeleteBotAlias.yaml"

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

requestGetBots :: GetBots -> TestTree
requestGetBots =
  req
    "GetBots"
    "fixture/GetBots.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot =
  req
    "GetBot"
    "fixture/GetBot.yaml"

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion =
  req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestDeleteSlotTypeVersion :: DeleteSlotTypeVersion -> TestTree
requestDeleteSlotTypeVersion =
  req
    "DeleteSlotTypeVersion"
    "fixture/DeleteSlotTypeVersion.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion =
  req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestGetSlotType :: GetSlotType -> TestTree
requestGetSlotType =
  req
    "GetSlotType"
    "fixture/GetSlotType.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestCreateIntentVersion :: CreateIntentVersion -> TestTree
requestCreateIntentVersion =
  req
    "CreateIntentVersion"
    "fixture/CreateIntentVersion.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType =
  req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestStartImport :: StartImport -> TestTree
requestStartImport =
  req
    "StartImport"
    "fixture/StartImport.yaml"

requestGetBotChannelAssociation :: GetBotChannelAssociation -> TestTree
requestGetBotChannelAssociation =
  req
    "GetBotChannelAssociation"
    "fixture/GetBotChannelAssociation.yaml"

requestPutSlotType :: PutSlotType -> TestTree
requestPutSlotType =
  req
    "PutSlotType"
    "fixture/PutSlotType.yaml"

requestGetBuiltinSlotTypes :: GetBuiltinSlotTypes -> TestTree
requestGetBuiltinSlotTypes =
  req
    "GetBuiltinSlotTypes"
    "fixture/GetBuiltinSlotTypes.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetUtterancesView :: GetUtterancesView -> TestTree
requestGetUtterancesView =
  req
    "GetUtterancesView"
    "fixture/GetUtterancesView.yaml"

requestGetSlotTypeVersions :: GetSlotTypeVersions -> TestTree
requestGetSlotTypeVersions =
  req
    "GetSlotTypeVersions"
    "fixture/GetSlotTypeVersions.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetIntents :: GetIntents -> TestTree
requestGetIntents =
  req
    "GetIntents"
    "fixture/GetIntents.yaml"

requestGetBotAlias :: GetBotAlias -> TestTree
requestGetBotAlias =
  req
    "GetBotAlias"
    "fixture/GetBotAlias.yaml"

-- Responses

responseDeleteIntentVersion :: DeleteIntentVersionResponse -> TestTree
responseDeleteIntentVersion =
  res
    "DeleteIntentVersionResponse"
    "fixture/DeleteIntentVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIntentVersion)

responseGetBotAliases :: GetBotAliasesResponse -> TestTree
responseGetBotAliases =
  res
    "GetBotAliasesResponse"
    "fixture/GetBotAliasesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBotAliases)

responseDeleteBotChannelAssociation :: DeleteBotChannelAssociationResponse -> TestTree
responseDeleteBotChannelAssociation =
  res
    "DeleteBotChannelAssociationResponse"
    "fixture/DeleteBotChannelAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBotChannelAssociation)

responseCreateSlotTypeVersion :: CreateSlotTypeVersionResponse -> TestTree
responseCreateSlotTypeVersion =
  res
    "CreateSlotTypeVersionResponse"
    "fixture/CreateSlotTypeVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateSlotTypeVersion)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseGetIntent :: GetIntentResponse -> TestTree
responseGetIntent =
  res
    "GetIntentResponse"
    "fixture/GetIntentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntent)

responsePutIntent :: PutIntentResponse -> TestTree
responsePutIntent =
  res
    "PutIntentResponse"
    "fixture/PutIntentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutIntent)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteIntent)

responseGetBuiltinIntents :: GetBuiltinIntentsResponse -> TestTree
responseGetBuiltinIntents =
  res
    "GetBuiltinIntentsResponse"
    "fixture/GetBuiltinIntentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBuiltinIntents)

responsePutBot :: PutBotResponse -> TestTree
responsePutBot =
  res
    "PutBotResponse"
    "fixture/PutBotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBot)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot =
  res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBot)

responseGetImport :: GetImportResponse -> TestTree
responseGetImport =
  res
    "GetImportResponse"
    "fixture/GetImportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetImport)

responseGetIntentVersions :: GetIntentVersionsResponse -> TestTree
responseGetIntentVersions =
  res
    "GetIntentVersionsResponse"
    "fixture/GetIntentVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntentVersions)

responseGetBuiltinIntent :: GetBuiltinIntentResponse -> TestTree
responseGetBuiltinIntent =
  res
    "GetBuiltinIntentResponse"
    "fixture/GetBuiltinIntentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBuiltinIntent)

responsePutBotAlias :: PutBotAliasResponse -> TestTree
responsePutBotAlias =
  res
    "PutBotAliasResponse"
    "fixture/PutBotAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutBotAlias)

responseGetBotVersions :: GetBotVersionsResponse -> TestTree
responseGetBotVersions =
  res
    "GetBotVersionsResponse"
    "fixture/GetBotVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBotVersions)

responseGetBotChannelAssociations :: GetBotChannelAssociationsResponse -> TestTree
responseGetBotChannelAssociations =
  res
    "GetBotChannelAssociationsResponse"
    "fixture/GetBotChannelAssociationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBotChannelAssociations)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias =
  res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBotAlias)

responseGetSlotTypes :: GetSlotTypesResponse -> TestTree
responseGetSlotTypes =
  res
    "GetSlotTypesResponse"
    "fixture/GetSlotTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSlotTypes)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteUtterances)

responseGetBots :: GetBotsResponse -> TestTree
responseGetBots =
  res
    "GetBotsResponse"
    "fixture/GetBotsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBots)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBot)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateBotVersion)

responseDeleteSlotTypeVersion :: DeleteSlotTypeVersionResponse -> TestTree
responseDeleteSlotTypeVersion =
  res
    "DeleteSlotTypeVersionResponse"
    "fixture/DeleteSlotTypeVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSlotTypeVersion)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteBotVersion)

responseGetSlotType :: GetSlotTypeResponse -> TestTree
responseGetSlotType =
  res
    "GetSlotTypeResponse"
    "fixture/GetSlotTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSlotType)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetExport)

responseCreateIntentVersion :: CreateIntentVersionResponse -> TestTree
responseCreateIntentVersion =
  res
    "CreateIntentVersionResponse"
    "fixture/CreateIntentVersionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateIntentVersion)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteSlotType)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy StartImport)

responseGetBotChannelAssociation :: GetBotChannelAssociationResponse -> TestTree
responseGetBotChannelAssociation =
  res
    "GetBotChannelAssociationResponse"
    "fixture/GetBotChannelAssociationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBotChannelAssociation)

responsePutSlotType :: PutSlotTypeResponse -> TestTree
responsePutSlotType =
  res
    "PutSlotTypeResponse"
    "fixture/PutSlotTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy PutSlotType)

responseGetBuiltinSlotTypes :: GetBuiltinSlotTypesResponse -> TestTree
responseGetBuiltinSlotTypes =
  res
    "GetBuiltinSlotTypesResponse"
    "fixture/GetBuiltinSlotTypesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBuiltinSlotTypes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseGetUtterancesView :: GetUtterancesViewResponse -> TestTree
responseGetUtterancesView =
  res
    "GetUtterancesViewResponse"
    "fixture/GetUtterancesViewResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetUtterancesView)

responseGetSlotTypeVersions :: GetSlotTypeVersionsResponse -> TestTree
responseGetSlotTypeVersions =
  res
    "GetSlotTypeVersionsResponse"
    "fixture/GetSlotTypeVersionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSlotTypeVersions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseGetIntents :: GetIntentsResponse -> TestTree
responseGetIntents =
  res
    "GetIntentsResponse"
    "fixture/GetIntentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIntents)

responseGetBotAlias :: GetBotAliasResponse -> TestTree
responseGetBotAlias =
  res
    "GetBotAliasResponse"
    "fixture/GetBotAliasResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBotAlias)
