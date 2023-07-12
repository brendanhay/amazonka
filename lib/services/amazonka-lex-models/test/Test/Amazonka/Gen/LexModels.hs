{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.LexModels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
--         [ requestCreateBotVersion $
--             newCreateBotVersion
--
--         , requestCreateIntentVersion $
--             newCreateIntentVersion
--
--         , requestCreateSlotTypeVersion $
--             newCreateSlotTypeVersion
--
--         , requestDeleteBot $
--             newDeleteBot
--
--         , requestDeleteBotAlias $
--             newDeleteBotAlias
--
--         , requestDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociation
--
--         , requestDeleteBotVersion $
--             newDeleteBotVersion
--
--         , requestDeleteIntent $
--             newDeleteIntent
--
--         , requestDeleteIntentVersion $
--             newDeleteIntentVersion
--
--         , requestDeleteSlotType $
--             newDeleteSlotType
--
--         , requestDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersion
--
--         , requestDeleteUtterances $
--             newDeleteUtterances
--
--         , requestGetBot $
--             newGetBot
--
--         , requestGetBotAlias $
--             newGetBotAlias
--
--         , requestGetBotAliases $
--             newGetBotAliases
--
--         , requestGetBotChannelAssociation $
--             newGetBotChannelAssociation
--
--         , requestGetBotChannelAssociations $
--             newGetBotChannelAssociations
--
--         , requestGetBotVersions $
--             newGetBotVersions
--
--         , requestGetBots $
--             newGetBots
--
--         , requestGetBuiltinIntent $
--             newGetBuiltinIntent
--
--         , requestGetBuiltinIntents $
--             newGetBuiltinIntents
--
--         , requestGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypes
--
--         , requestGetExport $
--             newGetExport
--
--         , requestGetImport $
--             newGetImport
--
--         , requestGetIntent $
--             newGetIntent
--
--         , requestGetIntentVersions $
--             newGetIntentVersions
--
--         , requestGetIntents $
--             newGetIntents
--
--         , requestGetMigration $
--             newGetMigration
--
--         , requestGetMigrations $
--             newGetMigrations
--
--         , requestGetSlotType $
--             newGetSlotType
--
--         , requestGetSlotTypeVersions $
--             newGetSlotTypeVersions
--
--         , requestGetSlotTypes $
--             newGetSlotTypes
--
--         , requestGetUtterancesView $
--             newGetUtterancesView
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutBot $
--             newPutBot
--
--         , requestPutBotAlias $
--             newPutBotAlias
--
--         , requestPutIntent $
--             newPutIntent
--
--         , requestPutSlotType $
--             newPutSlotType
--
--         , requestStartImport $
--             newStartImport
--
--         , requestStartMigration $
--             newStartMigration
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseCreateBotVersion $
--             newCreateBotVersionResponse
--
--         , responseCreateIntentVersion $
--             newCreateIntentVersionResponse
--
--         , responseCreateSlotTypeVersion $
--             newCreateSlotTypeVersionResponse
--
--         , responseDeleteBot $
--             newDeleteBotResponse
--
--         , responseDeleteBotAlias $
--             newDeleteBotAliasResponse
--
--         , responseDeleteBotChannelAssociation $
--             newDeleteBotChannelAssociationResponse
--
--         , responseDeleteBotVersion $
--             newDeleteBotVersionResponse
--
--         , responseDeleteIntent $
--             newDeleteIntentResponse
--
--         , responseDeleteIntentVersion $
--             newDeleteIntentVersionResponse
--
--         , responseDeleteSlotType $
--             newDeleteSlotTypeResponse
--
--         , responseDeleteSlotTypeVersion $
--             newDeleteSlotTypeVersionResponse
--
--         , responseDeleteUtterances $
--             newDeleteUtterancesResponse
--
--         , responseGetBot $
--             newGetBotResponse
--
--         , responseGetBotAlias $
--             newGetBotAliasResponse
--
--         , responseGetBotAliases $
--             newGetBotAliasesResponse
--
--         , responseGetBotChannelAssociation $
--             newGetBotChannelAssociationResponse
--
--         , responseGetBotChannelAssociations $
--             newGetBotChannelAssociationsResponse
--
--         , responseGetBotVersions $
--             newGetBotVersionsResponse
--
--         , responseGetBots $
--             newGetBotsResponse
--
--         , responseGetBuiltinIntent $
--             newGetBuiltinIntentResponse
--
--         , responseGetBuiltinIntents $
--             newGetBuiltinIntentsResponse
--
--         , responseGetBuiltinSlotTypes $
--             newGetBuiltinSlotTypesResponse
--
--         , responseGetExport $
--             newGetExportResponse
--
--         , responseGetImport $
--             newGetImportResponse
--
--         , responseGetIntent $
--             newGetIntentResponse
--
--         , responseGetIntentVersions $
--             newGetIntentVersionsResponse
--
--         , responseGetIntents $
--             newGetIntentsResponse
--
--         , responseGetMigration $
--             newGetMigrationResponse
--
--         , responseGetMigrations $
--             newGetMigrationsResponse
--
--         , responseGetSlotType $
--             newGetSlotTypeResponse
--
--         , responseGetSlotTypeVersions $
--             newGetSlotTypeVersionsResponse
--
--         , responseGetSlotTypes $
--             newGetSlotTypesResponse
--
--         , responseGetUtterancesView $
--             newGetUtterancesViewResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutBot $
--             newPutBotResponse
--
--         , responsePutBotAlias $
--             newPutBotAliasResponse
--
--         , responsePutIntent $
--             newPutIntentResponse
--
--         , responsePutSlotType $
--             newPutSlotTypeResponse
--
--         , responseStartImport $
--             newStartImportResponse
--
--         , responseStartMigration $
--             newStartMigrationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestCreateBotVersion :: CreateBotVersion -> TestTree
requestCreateBotVersion =
  req
    "CreateBotVersion"
    "fixture/CreateBotVersion.yaml"

requestCreateIntentVersion :: CreateIntentVersion -> TestTree
requestCreateIntentVersion =
  req
    "CreateIntentVersion"
    "fixture/CreateIntentVersion.yaml"

requestCreateSlotTypeVersion :: CreateSlotTypeVersion -> TestTree
requestCreateSlotTypeVersion =
  req
    "CreateSlotTypeVersion"
    "fixture/CreateSlotTypeVersion.yaml"

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

requestDeleteBotChannelAssociation :: DeleteBotChannelAssociation -> TestTree
requestDeleteBotChannelAssociation =
  req
    "DeleteBotChannelAssociation"
    "fixture/DeleteBotChannelAssociation.yaml"

requestDeleteBotVersion :: DeleteBotVersion -> TestTree
requestDeleteBotVersion =
  req
    "DeleteBotVersion"
    "fixture/DeleteBotVersion.yaml"

requestDeleteIntent :: DeleteIntent -> TestTree
requestDeleteIntent =
  req
    "DeleteIntent"
    "fixture/DeleteIntent.yaml"

requestDeleteIntentVersion :: DeleteIntentVersion -> TestTree
requestDeleteIntentVersion =
  req
    "DeleteIntentVersion"
    "fixture/DeleteIntentVersion.yaml"

requestDeleteSlotType :: DeleteSlotType -> TestTree
requestDeleteSlotType =
  req
    "DeleteSlotType"
    "fixture/DeleteSlotType.yaml"

requestDeleteSlotTypeVersion :: DeleteSlotTypeVersion -> TestTree
requestDeleteSlotTypeVersion =
  req
    "DeleteSlotTypeVersion"
    "fixture/DeleteSlotTypeVersion.yaml"

requestDeleteUtterances :: DeleteUtterances -> TestTree
requestDeleteUtterances =
  req
    "DeleteUtterances"
    "fixture/DeleteUtterances.yaml"

requestGetBot :: GetBot -> TestTree
requestGetBot =
  req
    "GetBot"
    "fixture/GetBot.yaml"

requestGetBotAlias :: GetBotAlias -> TestTree
requestGetBotAlias =
  req
    "GetBotAlias"
    "fixture/GetBotAlias.yaml"

requestGetBotAliases :: GetBotAliases -> TestTree
requestGetBotAliases =
  req
    "GetBotAliases"
    "fixture/GetBotAliases.yaml"

requestGetBotChannelAssociation :: GetBotChannelAssociation -> TestTree
requestGetBotChannelAssociation =
  req
    "GetBotChannelAssociation"
    "fixture/GetBotChannelAssociation.yaml"

requestGetBotChannelAssociations :: GetBotChannelAssociations -> TestTree
requestGetBotChannelAssociations =
  req
    "GetBotChannelAssociations"
    "fixture/GetBotChannelAssociations.yaml"

requestGetBotVersions :: GetBotVersions -> TestTree
requestGetBotVersions =
  req
    "GetBotVersions"
    "fixture/GetBotVersions.yaml"

requestGetBots :: GetBots -> TestTree
requestGetBots =
  req
    "GetBots"
    "fixture/GetBots.yaml"

requestGetBuiltinIntent :: GetBuiltinIntent -> TestTree
requestGetBuiltinIntent =
  req
    "GetBuiltinIntent"
    "fixture/GetBuiltinIntent.yaml"

requestGetBuiltinIntents :: GetBuiltinIntents -> TestTree
requestGetBuiltinIntents =
  req
    "GetBuiltinIntents"
    "fixture/GetBuiltinIntents.yaml"

requestGetBuiltinSlotTypes :: GetBuiltinSlotTypes -> TestTree
requestGetBuiltinSlotTypes =
  req
    "GetBuiltinSlotTypes"
    "fixture/GetBuiltinSlotTypes.yaml"

requestGetExport :: GetExport -> TestTree
requestGetExport =
  req
    "GetExport"
    "fixture/GetExport.yaml"

requestGetImport :: GetImport -> TestTree
requestGetImport =
  req
    "GetImport"
    "fixture/GetImport.yaml"

requestGetIntent :: GetIntent -> TestTree
requestGetIntent =
  req
    "GetIntent"
    "fixture/GetIntent.yaml"

requestGetIntentVersions :: GetIntentVersions -> TestTree
requestGetIntentVersions =
  req
    "GetIntentVersions"
    "fixture/GetIntentVersions.yaml"

requestGetIntents :: GetIntents -> TestTree
requestGetIntents =
  req
    "GetIntents"
    "fixture/GetIntents.yaml"

requestGetMigration :: GetMigration -> TestTree
requestGetMigration =
  req
    "GetMigration"
    "fixture/GetMigration.yaml"

requestGetMigrations :: GetMigrations -> TestTree
requestGetMigrations =
  req
    "GetMigrations"
    "fixture/GetMigrations.yaml"

requestGetSlotType :: GetSlotType -> TestTree
requestGetSlotType =
  req
    "GetSlotType"
    "fixture/GetSlotType.yaml"

requestGetSlotTypeVersions :: GetSlotTypeVersions -> TestTree
requestGetSlotTypeVersions =
  req
    "GetSlotTypeVersions"
    "fixture/GetSlotTypeVersions.yaml"

requestGetSlotTypes :: GetSlotTypes -> TestTree
requestGetSlotTypes =
  req
    "GetSlotTypes"
    "fixture/GetSlotTypes.yaml"

requestGetUtterancesView :: GetUtterancesView -> TestTree
requestGetUtterancesView =
  req
    "GetUtterancesView"
    "fixture/GetUtterancesView.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutBot :: PutBot -> TestTree
requestPutBot =
  req
    "PutBot"
    "fixture/PutBot.yaml"

requestPutBotAlias :: PutBotAlias -> TestTree
requestPutBotAlias =
  req
    "PutBotAlias"
    "fixture/PutBotAlias.yaml"

requestPutIntent :: PutIntent -> TestTree
requestPutIntent =
  req
    "PutIntent"
    "fixture/PutIntent.yaml"

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

requestStartMigration :: StartMigration -> TestTree
requestStartMigration =
  req
    "StartMigration"
    "fixture/StartMigration.yaml"

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

-- Responses

responseCreateBotVersion :: CreateBotVersionResponse -> TestTree
responseCreateBotVersion =
  res
    "CreateBotVersionResponse"
    "fixture/CreateBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateBotVersion)

responseCreateIntentVersion :: CreateIntentVersionResponse -> TestTree
responseCreateIntentVersion =
  res
    "CreateIntentVersionResponse"
    "fixture/CreateIntentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntentVersion)

responseCreateSlotTypeVersion :: CreateSlotTypeVersionResponse -> TestTree
responseCreateSlotTypeVersion =
  res
    "CreateSlotTypeVersionResponse"
    "fixture/CreateSlotTypeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSlotTypeVersion)

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

responseDeleteBotChannelAssociation :: DeleteBotChannelAssociationResponse -> TestTree
responseDeleteBotChannelAssociation =
  res
    "DeleteBotChannelAssociationResponse"
    "fixture/DeleteBotChannelAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotChannelAssociation)

responseDeleteBotVersion :: DeleteBotVersionResponse -> TestTree
responseDeleteBotVersion =
  res
    "DeleteBotVersionResponse"
    "fixture/DeleteBotVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteBotVersion)

responseDeleteIntent :: DeleteIntentResponse -> TestTree
responseDeleteIntent =
  res
    "DeleteIntentResponse"
    "fixture/DeleteIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntent)

responseDeleteIntentVersion :: DeleteIntentVersionResponse -> TestTree
responseDeleteIntentVersion =
  res
    "DeleteIntentVersionResponse"
    "fixture/DeleteIntentVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntentVersion)

responseDeleteSlotType :: DeleteSlotTypeResponse -> TestTree
responseDeleteSlotType =
  res
    "DeleteSlotTypeResponse"
    "fixture/DeleteSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlotType)

responseDeleteSlotTypeVersion :: DeleteSlotTypeVersionResponse -> TestTree
responseDeleteSlotTypeVersion =
  res
    "DeleteSlotTypeVersionResponse"
    "fixture/DeleteSlotTypeVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSlotTypeVersion)

responseDeleteUtterances :: DeleteUtterancesResponse -> TestTree
responseDeleteUtterances =
  res
    "DeleteUtterancesResponse"
    "fixture/DeleteUtterancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUtterances)

responseGetBot :: GetBotResponse -> TestTree
responseGetBot =
  res
    "GetBotResponse"
    "fixture/GetBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBot)

responseGetBotAlias :: GetBotAliasResponse -> TestTree
responseGetBotAlias =
  res
    "GetBotAliasResponse"
    "fixture/GetBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotAlias)

responseGetBotAliases :: GetBotAliasesResponse -> TestTree
responseGetBotAliases =
  res
    "GetBotAliasesResponse"
    "fixture/GetBotAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotAliases)

responseGetBotChannelAssociation :: GetBotChannelAssociationResponse -> TestTree
responseGetBotChannelAssociation =
  res
    "GetBotChannelAssociationResponse"
    "fixture/GetBotChannelAssociationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotChannelAssociation)

responseGetBotChannelAssociations :: GetBotChannelAssociationsResponse -> TestTree
responseGetBotChannelAssociations =
  res
    "GetBotChannelAssociationsResponse"
    "fixture/GetBotChannelAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotChannelAssociations)

responseGetBotVersions :: GetBotVersionsResponse -> TestTree
responseGetBotVersions =
  res
    "GetBotVersionsResponse"
    "fixture/GetBotVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBotVersions)

responseGetBots :: GetBotsResponse -> TestTree
responseGetBots =
  res
    "GetBotsResponse"
    "fixture/GetBotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBots)

responseGetBuiltinIntent :: GetBuiltinIntentResponse -> TestTree
responseGetBuiltinIntent =
  res
    "GetBuiltinIntentResponse"
    "fixture/GetBuiltinIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinIntent)

responseGetBuiltinIntents :: GetBuiltinIntentsResponse -> TestTree
responseGetBuiltinIntents =
  res
    "GetBuiltinIntentsResponse"
    "fixture/GetBuiltinIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinIntents)

responseGetBuiltinSlotTypes :: GetBuiltinSlotTypesResponse -> TestTree
responseGetBuiltinSlotTypes =
  res
    "GetBuiltinSlotTypesResponse"
    "fixture/GetBuiltinSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBuiltinSlotTypes)

responseGetExport :: GetExportResponse -> TestTree
responseGetExport =
  res
    "GetExportResponse"
    "fixture/GetExportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExport)

responseGetImport :: GetImportResponse -> TestTree
responseGetImport =
  res
    "GetImportResponse"
    "fixture/GetImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImport)

responseGetIntent :: GetIntentResponse -> TestTree
responseGetIntent =
  res
    "GetIntentResponse"
    "fixture/GetIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntent)

responseGetIntentVersions :: GetIntentVersionsResponse -> TestTree
responseGetIntentVersions =
  res
    "GetIntentVersionsResponse"
    "fixture/GetIntentVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntentVersions)

responseGetIntents :: GetIntentsResponse -> TestTree
responseGetIntents =
  res
    "GetIntentsResponse"
    "fixture/GetIntentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntents)

responseGetMigration :: GetMigrationResponse -> TestTree
responseGetMigration =
  res
    "GetMigrationResponse"
    "fixture/GetMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMigration)

responseGetMigrations :: GetMigrationsResponse -> TestTree
responseGetMigrations =
  res
    "GetMigrationsResponse"
    "fixture/GetMigrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMigrations)

responseGetSlotType :: GetSlotTypeResponse -> TestTree
responseGetSlotType =
  res
    "GetSlotTypeResponse"
    "fixture/GetSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotType)

responseGetSlotTypeVersions :: GetSlotTypeVersionsResponse -> TestTree
responseGetSlotTypeVersions =
  res
    "GetSlotTypeVersionsResponse"
    "fixture/GetSlotTypeVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotTypeVersions)

responseGetSlotTypes :: GetSlotTypesResponse -> TestTree
responseGetSlotTypes =
  res
    "GetSlotTypesResponse"
    "fixture/GetSlotTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSlotTypes)

responseGetUtterancesView :: GetUtterancesViewResponse -> TestTree
responseGetUtterancesView =
  res
    "GetUtterancesViewResponse"
    "fixture/GetUtterancesViewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUtterancesView)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutBot :: PutBotResponse -> TestTree
responsePutBot =
  res
    "PutBotResponse"
    "fixture/PutBotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBot)

responsePutBotAlias :: PutBotAliasResponse -> TestTree
responsePutBotAlias =
  res
    "PutBotAliasResponse"
    "fixture/PutBotAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutBotAlias)

responsePutIntent :: PutIntentResponse -> TestTree
responsePutIntent =
  res
    "PutIntentResponse"
    "fixture/PutIntentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntent)

responsePutSlotType :: PutSlotTypeResponse -> TestTree
responsePutSlotType =
  res
    "PutSlotTypeResponse"
    "fixture/PutSlotTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutSlotType)

responseStartImport :: StartImportResponse -> TestTree
responseStartImport =
  res
    "StartImportResponse"
    "fixture/StartImportResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImport)

responseStartMigration :: StartMigrationResponse -> TestTree
responseStartMigration =
  res
    "StartMigrationResponse"
    "fixture/StartMigrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMigration)

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
