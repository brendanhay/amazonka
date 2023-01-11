{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.GamesParks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.GamesParks where

import Amazonka.GamesParks
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.GamesParks.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateGame $
--             newCreateGame
--
--         , requestCreateSnapshot $
--             newCreateSnapshot
--
--         , requestCreateStage $
--             newCreateStage
--
--         , requestDeleteGame $
--             newDeleteGame
--
--         , requestDeleteStage $
--             newDeleteStage
--
--         , requestDisconnectPlayer $
--             newDisconnectPlayer
--
--         , requestExportSnapshot $
--             newExportSnapshot
--
--         , requestGetExtension $
--             newGetExtension
--
--         , requestGetExtensionVersion $
--             newGetExtensionVersion
--
--         , requestGetGame $
--             newGetGame
--
--         , requestGetGameConfiguration $
--             newGetGameConfiguration
--
--         , requestGetGeneratedCodeJob $
--             newGetGeneratedCodeJob
--
--         , requestGetPlayerConnectionStatus $
--             newGetPlayerConnectionStatus
--
--         , requestGetSnapshot $
--             newGetSnapshot
--
--         , requestGetStage $
--             newGetStage
--
--         , requestGetStageDeployment $
--             newGetStageDeployment
--
--         , requestImportGameConfiguration $
--             newImportGameConfiguration
--
--         , requestListExtensionVersions $
--             newListExtensionVersions
--
--         , requestListExtensions $
--             newListExtensions
--
--         , requestListGames $
--             newListGames
--
--         , requestListGeneratedCodeJobs $
--             newListGeneratedCodeJobs
--
--         , requestListSnapshots $
--             newListSnapshots
--
--         , requestListStageDeployments $
--             newListStageDeployments
--
--         , requestListStages $
--             newListStages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartGeneratedCodeJob $
--             newStartGeneratedCodeJob
--
--         , requestStartStageDeployment $
--             newStartStageDeployment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateGame $
--             newUpdateGame
--
--         , requestUpdateGameConfiguration $
--             newUpdateGameConfiguration
--
--         , requestUpdateSnapshot $
--             newUpdateSnapshot
--
--         , requestUpdateStage $
--             newUpdateStage
--
--           ]

--     , testGroup "response"
--         [ responseCreateGame $
--             newCreateGameResponse
--
--         , responseCreateSnapshot $
--             newCreateSnapshotResponse
--
--         , responseCreateStage $
--             newCreateStageResponse
--
--         , responseDeleteGame $
--             newDeleteGameResponse
--
--         , responseDeleteStage $
--             newDeleteStageResponse
--
--         , responseDisconnectPlayer $
--             newDisconnectPlayerResponse
--
--         , responseExportSnapshot $
--             newExportSnapshotResponse
--
--         , responseGetExtension $
--             newGetExtensionResponse
--
--         , responseGetExtensionVersion $
--             newGetExtensionVersionResponse
--
--         , responseGetGame $
--             newGetGameResponse
--
--         , responseGetGameConfiguration $
--             newGetGameConfigurationResponse
--
--         , responseGetGeneratedCodeJob $
--             newGetGeneratedCodeJobResponse
--
--         , responseGetPlayerConnectionStatus $
--             newGetPlayerConnectionStatusResponse
--
--         , responseGetSnapshot $
--             newGetSnapshotResponse
--
--         , responseGetStage $
--             newGetStageResponse
--
--         , responseGetStageDeployment $
--             newGetStageDeploymentResponse
--
--         , responseImportGameConfiguration $
--             newImportGameConfigurationResponse
--
--         , responseListExtensionVersions $
--             newListExtensionVersionsResponse
--
--         , responseListExtensions $
--             newListExtensionsResponse
--
--         , responseListGames $
--             newListGamesResponse
--
--         , responseListGeneratedCodeJobs $
--             newListGeneratedCodeJobsResponse
--
--         , responseListSnapshots $
--             newListSnapshotsResponse
--
--         , responseListStageDeployments $
--             newListStageDeploymentsResponse
--
--         , responseListStages $
--             newListStagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartGeneratedCodeJob $
--             newStartGeneratedCodeJobResponse
--
--         , responseStartStageDeployment $
--             newStartStageDeploymentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateGame $
--             newUpdateGameResponse
--
--         , responseUpdateGameConfiguration $
--             newUpdateGameConfigurationResponse
--
--         , responseUpdateSnapshot $
--             newUpdateSnapshotResponse
--
--         , responseUpdateStage $
--             newUpdateStageResponse
--
--           ]
--     ]

-- Requests

requestCreateGame :: CreateGame -> TestTree
requestCreateGame =
  req
    "CreateGame"
    "fixture/CreateGame.yaml"

requestCreateSnapshot :: CreateSnapshot -> TestTree
requestCreateSnapshot =
  req
    "CreateSnapshot"
    "fixture/CreateSnapshot.yaml"

requestCreateStage :: CreateStage -> TestTree
requestCreateStage =
  req
    "CreateStage"
    "fixture/CreateStage.yaml"

requestDeleteGame :: DeleteGame -> TestTree
requestDeleteGame =
  req
    "DeleteGame"
    "fixture/DeleteGame.yaml"

requestDeleteStage :: DeleteStage -> TestTree
requestDeleteStage =
  req
    "DeleteStage"
    "fixture/DeleteStage.yaml"

requestDisconnectPlayer :: DisconnectPlayer -> TestTree
requestDisconnectPlayer =
  req
    "DisconnectPlayer"
    "fixture/DisconnectPlayer.yaml"

requestExportSnapshot :: ExportSnapshot -> TestTree
requestExportSnapshot =
  req
    "ExportSnapshot"
    "fixture/ExportSnapshot.yaml"

requestGetExtension :: GetExtension -> TestTree
requestGetExtension =
  req
    "GetExtension"
    "fixture/GetExtension.yaml"

requestGetExtensionVersion :: GetExtensionVersion -> TestTree
requestGetExtensionVersion =
  req
    "GetExtensionVersion"
    "fixture/GetExtensionVersion.yaml"

requestGetGame :: GetGame -> TestTree
requestGetGame =
  req
    "GetGame"
    "fixture/GetGame.yaml"

requestGetGameConfiguration :: GetGameConfiguration -> TestTree
requestGetGameConfiguration =
  req
    "GetGameConfiguration"
    "fixture/GetGameConfiguration.yaml"

requestGetGeneratedCodeJob :: GetGeneratedCodeJob -> TestTree
requestGetGeneratedCodeJob =
  req
    "GetGeneratedCodeJob"
    "fixture/GetGeneratedCodeJob.yaml"

requestGetPlayerConnectionStatus :: GetPlayerConnectionStatus -> TestTree
requestGetPlayerConnectionStatus =
  req
    "GetPlayerConnectionStatus"
    "fixture/GetPlayerConnectionStatus.yaml"

requestGetSnapshot :: GetSnapshot -> TestTree
requestGetSnapshot =
  req
    "GetSnapshot"
    "fixture/GetSnapshot.yaml"

requestGetStage :: GetStage -> TestTree
requestGetStage =
  req
    "GetStage"
    "fixture/GetStage.yaml"

requestGetStageDeployment :: GetStageDeployment -> TestTree
requestGetStageDeployment =
  req
    "GetStageDeployment"
    "fixture/GetStageDeployment.yaml"

requestImportGameConfiguration :: ImportGameConfiguration -> TestTree
requestImportGameConfiguration =
  req
    "ImportGameConfiguration"
    "fixture/ImportGameConfiguration.yaml"

requestListExtensionVersions :: ListExtensionVersions -> TestTree
requestListExtensionVersions =
  req
    "ListExtensionVersions"
    "fixture/ListExtensionVersions.yaml"

requestListExtensions :: ListExtensions -> TestTree
requestListExtensions =
  req
    "ListExtensions"
    "fixture/ListExtensions.yaml"

requestListGames :: ListGames -> TestTree
requestListGames =
  req
    "ListGames"
    "fixture/ListGames.yaml"

requestListGeneratedCodeJobs :: ListGeneratedCodeJobs -> TestTree
requestListGeneratedCodeJobs =
  req
    "ListGeneratedCodeJobs"
    "fixture/ListGeneratedCodeJobs.yaml"

requestListSnapshots :: ListSnapshots -> TestTree
requestListSnapshots =
  req
    "ListSnapshots"
    "fixture/ListSnapshots.yaml"

requestListStageDeployments :: ListStageDeployments -> TestTree
requestListStageDeployments =
  req
    "ListStageDeployments"
    "fixture/ListStageDeployments.yaml"

requestListStages :: ListStages -> TestTree
requestListStages =
  req
    "ListStages"
    "fixture/ListStages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartGeneratedCodeJob :: StartGeneratedCodeJob -> TestTree
requestStartGeneratedCodeJob =
  req
    "StartGeneratedCodeJob"
    "fixture/StartGeneratedCodeJob.yaml"

requestStartStageDeployment :: StartStageDeployment -> TestTree
requestStartStageDeployment =
  req
    "StartStageDeployment"
    "fixture/StartStageDeployment.yaml"

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

requestUpdateGame :: UpdateGame -> TestTree
requestUpdateGame =
  req
    "UpdateGame"
    "fixture/UpdateGame.yaml"

requestUpdateGameConfiguration :: UpdateGameConfiguration -> TestTree
requestUpdateGameConfiguration =
  req
    "UpdateGameConfiguration"
    "fixture/UpdateGameConfiguration.yaml"

requestUpdateSnapshot :: UpdateSnapshot -> TestTree
requestUpdateSnapshot =
  req
    "UpdateSnapshot"
    "fixture/UpdateSnapshot.yaml"

requestUpdateStage :: UpdateStage -> TestTree
requestUpdateStage =
  req
    "UpdateStage"
    "fixture/UpdateStage.yaml"

-- Responses

responseCreateGame :: CreateGameResponse -> TestTree
responseCreateGame =
  res
    "CreateGameResponse"
    "fixture/CreateGameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGame)

responseCreateSnapshot :: CreateSnapshotResponse -> TestTree
responseCreateSnapshot =
  res
    "CreateSnapshotResponse"
    "fixture/CreateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSnapshot)

responseCreateStage :: CreateStageResponse -> TestTree
responseCreateStage =
  res
    "CreateStageResponse"
    "fixture/CreateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStage)

responseDeleteGame :: DeleteGameResponse -> TestTree
responseDeleteGame =
  res
    "DeleteGameResponse"
    "fixture/DeleteGameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGame)

responseDeleteStage :: DeleteStageResponse -> TestTree
responseDeleteStage =
  res
    "DeleteStageResponse"
    "fixture/DeleteStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStage)

responseDisconnectPlayer :: DisconnectPlayerResponse -> TestTree
responseDisconnectPlayer =
  res
    "DisconnectPlayerResponse"
    "fixture/DisconnectPlayerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisconnectPlayer)

responseExportSnapshot :: ExportSnapshotResponse -> TestTree
responseExportSnapshot =
  res
    "ExportSnapshotResponse"
    "fixture/ExportSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportSnapshot)

responseGetExtension :: GetExtensionResponse -> TestTree
responseGetExtension =
  res
    "GetExtensionResponse"
    "fixture/GetExtensionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExtension)

responseGetExtensionVersion :: GetExtensionVersionResponse -> TestTree
responseGetExtensionVersion =
  res
    "GetExtensionVersionResponse"
    "fixture/GetExtensionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetExtensionVersion)

responseGetGame :: GetGameResponse -> TestTree
responseGetGame =
  res
    "GetGameResponse"
    "fixture/GetGameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGame)

responseGetGameConfiguration :: GetGameConfigurationResponse -> TestTree
responseGetGameConfiguration =
  res
    "GetGameConfigurationResponse"
    "fixture/GetGameConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGameConfiguration)

responseGetGeneratedCodeJob :: GetGeneratedCodeJobResponse -> TestTree
responseGetGeneratedCodeJob =
  res
    "GetGeneratedCodeJobResponse"
    "fixture/GetGeneratedCodeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGeneratedCodeJob)

responseGetPlayerConnectionStatus :: GetPlayerConnectionStatusResponse -> TestTree
responseGetPlayerConnectionStatus =
  res
    "GetPlayerConnectionStatusResponse"
    "fixture/GetPlayerConnectionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlayerConnectionStatus)

responseGetSnapshot :: GetSnapshotResponse -> TestTree
responseGetSnapshot =
  res
    "GetSnapshotResponse"
    "fixture/GetSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSnapshot)

responseGetStage :: GetStageResponse -> TestTree
responseGetStage =
  res
    "GetStageResponse"
    "fixture/GetStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStage)

responseGetStageDeployment :: GetStageDeploymentResponse -> TestTree
responseGetStageDeployment =
  res
    "GetStageDeploymentResponse"
    "fixture/GetStageDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetStageDeployment)

responseImportGameConfiguration :: ImportGameConfigurationResponse -> TestTree
responseImportGameConfiguration =
  res
    "ImportGameConfigurationResponse"
    "fixture/ImportGameConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ImportGameConfiguration)

responseListExtensionVersions :: ListExtensionVersionsResponse -> TestTree
responseListExtensionVersions =
  res
    "ListExtensionVersionsResponse"
    "fixture/ListExtensionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExtensionVersions)

responseListExtensions :: ListExtensionsResponse -> TestTree
responseListExtensions =
  res
    "ListExtensionsResponse"
    "fixture/ListExtensionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListExtensions)

responseListGames :: ListGamesResponse -> TestTree
responseListGames =
  res
    "ListGamesResponse"
    "fixture/ListGamesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGames)

responseListGeneratedCodeJobs :: ListGeneratedCodeJobsResponse -> TestTree
responseListGeneratedCodeJobs =
  res
    "ListGeneratedCodeJobsResponse"
    "fixture/ListGeneratedCodeJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGeneratedCodeJobs)

responseListSnapshots :: ListSnapshotsResponse -> TestTree
responseListSnapshots =
  res
    "ListSnapshotsResponse"
    "fixture/ListSnapshotsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSnapshots)

responseListStageDeployments :: ListStageDeploymentsResponse -> TestTree
responseListStageDeployments =
  res
    "ListStageDeploymentsResponse"
    "fixture/ListStageDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStageDeployments)

responseListStages :: ListStagesResponse -> TestTree
responseListStages =
  res
    "ListStagesResponse"
    "fixture/ListStagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListStages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartGeneratedCodeJob :: StartGeneratedCodeJobResponse -> TestTree
responseStartGeneratedCodeJob =
  res
    "StartGeneratedCodeJobResponse"
    "fixture/StartGeneratedCodeJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartGeneratedCodeJob)

responseStartStageDeployment :: StartStageDeploymentResponse -> TestTree
responseStartStageDeployment =
  res
    "StartStageDeploymentResponse"
    "fixture/StartStageDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartStageDeployment)

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

responseUpdateGame :: UpdateGameResponse -> TestTree
responseUpdateGame =
  res
    "UpdateGameResponse"
    "fixture/UpdateGameResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGame)

responseUpdateGameConfiguration :: UpdateGameConfigurationResponse -> TestTree
responseUpdateGameConfiguration =
  res
    "UpdateGameConfigurationResponse"
    "fixture/UpdateGameConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGameConfiguration)

responseUpdateSnapshot :: UpdateSnapshotResponse -> TestTree
responseUpdateSnapshot =
  res
    "UpdateSnapshotResponse"
    "fixture/UpdateSnapshotResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSnapshot)

responseUpdateStage :: UpdateStageResponse -> TestTree
responseUpdateStage =
  res
    "UpdateStageResponse"
    "fixture/UpdateStageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStage)
