{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LexModels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.LexModels where

import Amazonka.LexModels
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.LexModels.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestStartMigration $
--             newStartMigration
--
--         , requestDeleteIntentVersion $
--             newDeleteIntentVersion
--
--         , requestGetBotAliases $
--             newGetBotAliases
--
--         , requestDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociation
--
--         , requestCreateSlotTypeVersion $
--             newCreateSlotTypeVersion
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetIntent $
--             newGetIntent
--
--         , requestPutIntent $
--             newPutIntent
--
--         , requestDeleteIntent $
--             newDeleteIntent
--
--         , requestGetBuiltinIntents $
--             newGetBuiltinIntents
--
--         , requestPutBot $
--             newPutBot
--
--         , requestDeleteBot $
--             newDeleteBot
--
--         , requestGetImport $
--             newGetImport
--
--         , requestGetIntentVersions $
--             newGetIntentVersions
--
--         , requestGetBuiltinIntent $
--             newGetBuiltinIntent
--
--         , requestPutBotAlias $
--             newPutBotAlias
--
--         , requestGetBotVersions $
--             newGetBotVersions
--
--         , requestGetBotChannelAssociations $
--             newGetBotChannelAssociations
--
--         , requestDeleteBotAlias $
--             newDeleteBotAlias
--
--         , requestGetSlotTypes $
--             newGetSlotTypes
--
--         , requestGetMigrations $
--             newGetMigrations
--
--         , requestDeleteUtterances $
--             newDeleteUtterances
--
--         , requestGetBots $
--             newGetBots
--
--         , requestGetBot $
--             newGetBot
--
--         , requestCreateBotVersion $
--             newCreateBotVersion
--
--         , requestDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersion
--
--         , requestDeleteBotVersion $
--             newDeleteBotVersion
--
--         , requestGetSlotType $
--             newGetSlotType
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetMigration $
--             newGetMigration
--
--         , requestCreateIntentVersion $
--             newCreateIntentVersion
--
--         , requestDeleteSlotType $
--             newDeleteSlotType
--
--         , requestStartImport $
--             newStartImport
--
--         , requestGetBotChannelAssociation $
--             newGetBotChannelAssociation
--
--         , requestPutSlotType $
--             newPutSlotType
--
--         , requestGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypes
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetUtterancesView $
--             newGetUtterancesView
--
--         , requestGetSlotTypeVersions $
--             newGetSlotTypeVersions
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetIntents $
--             newGetIntents
--
--         , requestGetBotAlias $
--             newGetBotAlias
--
--           ]

--     , testGroup "response"
--         [ responseStartMigration $
--             newStartMigrationResponse
--
--         , responseDeleteIntentVersion $
--             newDeleteIntentVersionResponse
--
--         , responseGetBotAliases $
--             newGetBotAliasesResponse
--
--         , responseDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociationResponse
--
--         , responseCreateSlotTypeVersion $
--             newCreateSlotTypeVersionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetIntent $
--             newGetIntentResponse
--
--         , responsePutIntent $
--             newPutIntentResponse
--
--         , responseDeleteIntent $
--             newDeleteIntentResponse
--
--         , responseGetBuiltinIntents $
--             newGetBuiltinIntentsResponse
--
--         , responsePutBot $
--             newPutBotResponse
--
--         , responseDeleteBot $
--             newDeleteBotResponse
--
--         , responseGetImport $
--             newGetImportResponse
--
--         , responseGetIntentVersions $
--             newGetIntentVersionsResponse
--
--         , responseGetBuiltinIntent $
--             newGetBuiltinIntentResponse
--
--         , responsePutBotAlias $
--             newPutBotAliasResponse
--
--         , responseGetBotVersions $
--             newGetBotVersionsResponse
--
--         , responseGetBotChannelAssociations $
--             newGetBotChannelAssociationsResponse
--
--         , responseDeleteBotAlias $
--             newDeleteBotAliasResponse
--
--         , responseGetSlotTypes $
--             newGetSlotTypesResponse
--
--         , responseGetMigrations $
--             newGetMigrationsResponse
--
--         , responseDeleteUtterances $
--             newDeleteUtterancesResponse
--
--         , responseGetBots $
--             newGetBotsResponse
--
--         , responseGetBot $
--             newGetBotResponse
--
--         , responseCreateBotVersion $
--             newCreateBotVersionResponse
--
--         , responseDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersionResponse
--
--         , responseDeleteBotVersion $
--             newDeleteBotVersionResponse
--
--         , responseGetSlotType $
--             newGetSlotTypeResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetMigration $
--             newGetMigrationResponse
--
--         , responseCreateIntentVersion $
--             newCreateIntentVersionResponse
--
--         , responseDeleteSlotType $
--             newDeleteSlotTypeResponse
--
--         , responseStartImport $
--             newStartImportResponse
--
--         , responseGetBotChannelAssociation $
--             newGetBotChannelAssociationResponse
--
--         , responsePutSlotType $
--             newPutSlotTypeResponse
--
--         , responseGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetUtterancesView $
--             newGetUtterancesViewResponse
--
--         , responseGetSlotTypeVersions $
--             newGetSlotTypeVersionsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetIntents $
--             newGetIntentsResponse
--
--         , responseGetBotAlias $
--             newGetBotAliasResponse
--
--           ]
--     ]

-- Requests

requestStartMigration :: StartMigration -> TestTree
requestStartMigration =
  req
    "StartMigration"
    "fixture/StartMigration.yaml"

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

requestGetMigrations :: GetMigrations -> TestTree
requestGetMigrations =
  req
    "GetMigrations"
    "fixture/GetMigrations.yaml"

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

requestGetMigration :: GetMigration -> TestTree
requestGetMigration =
  req
    "GetMigration"
    "fixture/GetMigration.yaml"

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

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMigration)

responseDeleteIntentVersion :: DeleteIntentVersionResponse -> TestTree
responseDeleteIntentVersion =
  res
    "DeleteIntentVersionResponse"
    "fixture/DeleteIntentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntentVersion)

responseGetBotAliases :: GetBotAliasesResponse -> TestTree
responseGetBotAliases =
  res
    "GetBotAliasesResponse"
    "fixture/GetBotAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotAliases)

responseDeleteBotChannelAssociation :: DeleteBotChannelAssociationResponse -> TestTree
responseDeleteBotChannelAssociation =
  res
    "DeleteBotChannelAssociationResponse"
    "fixture/DeleteBotChannelAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotChannelAssociation)

responseCreateSlotTypeVersion :: CreateSlotTypeVersionResponse -> TestTree
responseCreateSlotTypeVersion =
  res
    "CreateSlotTypeVersionResponse"
    "fixture/CreateSlotTypeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSlotTypeVersion)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetIntent :: GetIntentResponse -> TestTree
responseGetIntent =
  res
    "GetIntentResponse"
    "fixture/GetIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntent)

responsePutIntent :: PutIntentResponse -> TestTree
responsePutIntent =
  res
    "PutIntentResponse"
    "fixture/PutIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntent)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntent)

responseGetBuiltinIntents :: GetBuiltinIntentsResponse -> TestTree
responseGetBuiltinIntents =
  res
    "GetBuiltinIntentsResponse"
    "fixture/GetBuiltinIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinIntents)

responsePutBot :: PutBotResponse -> TestTree
responsePutBot =
  res
    "PutBotResponse"
    "fixture/PutBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBot)

responseDeleteBot :: DeleteBotResponse -> TestTree
responseDeleteBot =
  res
    "DeleteBotResponse"
    "fixture/DeleteBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBot)

responseGetImport :: GetImportResponse -> TestTree
responseGetImport =
  res
    "GetImportResponse"
    "fixture/GetImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImport)

responseGetIntentVersions :: GetIntentVersionsResponse -> TestTree
responseGetIntentVersions =
  res
    "GetIntentVersionsResponse"
    "fixture/GetIntentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntentVersions)

responseGetBuiltinIntent :: GetBuiltinIntentResponse -> TestTree
responseGetBuiltinIntent =
  res
    "GetBuiltinIntentResponse"
    "fixture/GetBuiltinIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinIntent)

responsePutBotAlias :: PutBotAliasResponse -> TestTree
responsePutBotAlias =
  res
    "PutBotAliasResponse"
    "fixture/PutBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBotAlias)

responseGetBotVersions :: GetBotVersionsResponse -> TestTree
responseGetBotVersions =
  res
    "GetBotVersionsResponse"
    "fixture/GetBotVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotVersions)

responseGetBotChannelAssociations :: GetBotChannelAssociationsResponse -> TestTree
responseGetBotChannelAssociations =
  res
    "GetBotChannelAssociationsResponse"
    "fixture/GetBotChannelAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotChannelAssociations)

responseDeleteBotAlias :: DeleteBotAliasResponse -> TestTree
responseDeleteBotAlias =
  res
    "DeleteBotAliasResponse"
    "fixture/DeleteBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotAlias)

responseGetSlotTypes :: GetSlotTypesResponse -> TestTree
responseGetSlotTypes =
  res
    "GetSlotTypesResponse"
    "fixture/GetSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotTypes)

responseGetMigrations :: GetMigrationsResponse -> TestTree
responseGetMigrations =
  res
    "GetMigrationsResponse"
    "fixture/GetMigrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMigrations)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUtterances)

responseGetBots :: GetBotsResponse -> TestTree
responseGetBots =
  res
    "GetBotsResponse"
    "fixture/GetBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBots)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBot)

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBotVersion)

responseDeleteSlotTypeVersion :: DeleteSlotTypeVersionResponse -> TestTree
responseDeleteSlotTypeVersion =
  res
    "DeleteSlotTypeVersionResponse"
    "fixture/DeleteSlotTypeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlotTypeVersion)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotVersion)

responseGetSlotType :: GetSlotTypeResponse -> TestTree
responseGetSlotType =
  res
    "GetSlotTypeResponse"
    "fixture/GetSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotType)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExport)

responseGetMigration :: GetMigrationResponse -> TestTree
responseGetMigration =
  res
    "GetMigrationResponse"
    "fixture/GetMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMigration)

responseCreateIntentVersion :: CreateIntentVersionResponse -> TestTree
responseCreateIntentVersion =
  res
    "CreateIntentVersionResponse"
    "fixture/CreateIntentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntentVersion)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlotType)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImport)

responseGetBotChannelAssociation :: GetBotChannelAssociationResponse -> TestTree
responseGetBotChannelAssociation =
  res
    "GetBotChannelAssociationResponse"
    "fixture/GetBotChannelAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotChannelAssociation)

responsePutSlotType :: PutSlotTypeResponse -> TestTree
responsePutSlotType =
  res
    "PutSlotTypeResponse"
    "fixture/PutSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSlotType)

responseGetBuiltinSlotTypes :: GetBuiltinSlotTypesResponse -> TestTree
responseGetBuiltinSlotTypes =
  res
    "GetBuiltinSlotTypesResponse"
    "fixture/GetBuiltinSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinSlotTypes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetUtterancesView :: GetUtterancesViewResponse -> TestTree
responseGetUtterancesView =
  res
    "GetUtterancesViewResponse"
    "fixture/GetUtterancesViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUtterancesView)

responseGetSlotTypeVersions :: GetSlotTypeVersionsResponse -> TestTree
responseGetSlotTypeVersions =
  res
    "GetSlotTypeVersionsResponse"
    "fixture/GetSlotTypeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotTypeVersions)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetIntents :: GetIntentsResponse -> TestTree
responseGetIntents =
  res
    "GetIntentsResponse"
    "fixture/GetIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntents)

responseGetBotAlias :: GetBotAliasResponse -> TestTree
responseGetBotAlias =
  res
    "GetBotAliasResponse"
    "fixture/GetBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotAlias)
