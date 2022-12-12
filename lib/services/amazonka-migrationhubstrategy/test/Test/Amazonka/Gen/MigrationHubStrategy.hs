{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.MigrationHubStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.MigrationHubStrategy where

import Amazonka.MigrationHubStrategy
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.MigrationHubStrategy.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetApplicationComponentDetails $
--             newGetApplicationComponentDetails
--
--         , requestGetApplicationComponentStrategies $
--             newGetApplicationComponentStrategies
--
--         , requestGetAssessment $
--             newGetAssessment
--
--         , requestGetImportFileTask $
--             newGetImportFileTask
--
--         , requestGetLatestAssessmentId $
--             newGetLatestAssessmentId
--
--         , requestGetPortfolioPreferences $
--             newGetPortfolioPreferences
--
--         , requestGetPortfolioSummary $
--             newGetPortfolioSummary
--
--         , requestGetRecommendationReportDetails $
--             newGetRecommendationReportDetails
--
--         , requestGetServerDetails $
--             newGetServerDetails
--
--         , requestGetServerStrategies $
--             newGetServerStrategies
--
--         , requestListApplicationComponents $
--             newListApplicationComponents
--
--         , requestListCollectors $
--             newListCollectors
--
--         , requestListImportFileTask $
--             newListImportFileTask
--
--         , requestListServers $
--             newListServers
--
--         , requestPutPortfolioPreferences $
--             newPutPortfolioPreferences
--
--         , requestStartAssessment $
--             newStartAssessment
--
--         , requestStartImportFileTask $
--             newStartImportFileTask
--
--         , requestStartRecommendationReportGeneration $
--             newStartRecommendationReportGeneration
--
--         , requestStopAssessment $
--             newStopAssessment
--
--         , requestUpdateApplicationComponentConfig $
--             newUpdateApplicationComponentConfig
--
--         , requestUpdateServerConfig $
--             newUpdateServerConfig
--
--           ]

--     , testGroup "response"
--         [ responseGetApplicationComponentDetails $
--             newGetApplicationComponentDetailsResponse
--
--         , responseGetApplicationComponentStrategies $
--             newGetApplicationComponentStrategiesResponse
--
--         , responseGetAssessment $
--             newGetAssessmentResponse
--
--         , responseGetImportFileTask $
--             newGetImportFileTaskResponse
--
--         , responseGetLatestAssessmentId $
--             newGetLatestAssessmentIdResponse
--
--         , responseGetPortfolioPreferences $
--             newGetPortfolioPreferencesResponse
--
--         , responseGetPortfolioSummary $
--             newGetPortfolioSummaryResponse
--
--         , responseGetRecommendationReportDetails $
--             newGetRecommendationReportDetailsResponse
--
--         , responseGetServerDetails $
--             newGetServerDetailsResponse
--
--         , responseGetServerStrategies $
--             newGetServerStrategiesResponse
--
--         , responseListApplicationComponents $
--             newListApplicationComponentsResponse
--
--         , responseListCollectors $
--             newListCollectorsResponse
--
--         , responseListImportFileTask $
--             newListImportFileTaskResponse
--
--         , responseListServers $
--             newListServersResponse
--
--         , responsePutPortfolioPreferences $
--             newPutPortfolioPreferencesResponse
--
--         , responseStartAssessment $
--             newStartAssessmentResponse
--
--         , responseStartImportFileTask $
--             newStartImportFileTaskResponse
--
--         , responseStartRecommendationReportGeneration $
--             newStartRecommendationReportGenerationResponse
--
--         , responseStopAssessment $
--             newStopAssessmentResponse
--
--         , responseUpdateApplicationComponentConfig $
--             newUpdateApplicationComponentConfigResponse
--
--         , responseUpdateServerConfig $
--             newUpdateServerConfigResponse
--
--           ]
--     ]

-- Requests

requestGetApplicationComponentDetails :: GetApplicationComponentDetails -> TestTree
requestGetApplicationComponentDetails =
  req
    "GetApplicationComponentDetails"
    "fixture/GetApplicationComponentDetails.yaml"

requestGetApplicationComponentStrategies :: GetApplicationComponentStrategies -> TestTree
requestGetApplicationComponentStrategies =
  req
    "GetApplicationComponentStrategies"
    "fixture/GetApplicationComponentStrategies.yaml"

requestGetAssessment :: GetAssessment -> TestTree
requestGetAssessment =
  req
    "GetAssessment"
    "fixture/GetAssessment.yaml"

requestGetImportFileTask :: GetImportFileTask -> TestTree
requestGetImportFileTask =
  req
    "GetImportFileTask"
    "fixture/GetImportFileTask.yaml"

requestGetLatestAssessmentId :: GetLatestAssessmentId -> TestTree
requestGetLatestAssessmentId =
  req
    "GetLatestAssessmentId"
    "fixture/GetLatestAssessmentId.yaml"

requestGetPortfolioPreferences :: GetPortfolioPreferences -> TestTree
requestGetPortfolioPreferences =
  req
    "GetPortfolioPreferences"
    "fixture/GetPortfolioPreferences.yaml"

requestGetPortfolioSummary :: GetPortfolioSummary -> TestTree
requestGetPortfolioSummary =
  req
    "GetPortfolioSummary"
    "fixture/GetPortfolioSummary.yaml"

requestGetRecommendationReportDetails :: GetRecommendationReportDetails -> TestTree
requestGetRecommendationReportDetails =
  req
    "GetRecommendationReportDetails"
    "fixture/GetRecommendationReportDetails.yaml"

requestGetServerDetails :: GetServerDetails -> TestTree
requestGetServerDetails =
  req
    "GetServerDetails"
    "fixture/GetServerDetails.yaml"

requestGetServerStrategies :: GetServerStrategies -> TestTree
requestGetServerStrategies =
  req
    "GetServerStrategies"
    "fixture/GetServerStrategies.yaml"

requestListApplicationComponents :: ListApplicationComponents -> TestTree
requestListApplicationComponents =
  req
    "ListApplicationComponents"
    "fixture/ListApplicationComponents.yaml"

requestListCollectors :: ListCollectors -> TestTree
requestListCollectors =
  req
    "ListCollectors"
    "fixture/ListCollectors.yaml"

requestListImportFileTask :: ListImportFileTask -> TestTree
requestListImportFileTask =
  req
    "ListImportFileTask"
    "fixture/ListImportFileTask.yaml"

requestListServers :: ListServers -> TestTree
requestListServers =
  req
    "ListServers"
    "fixture/ListServers.yaml"

requestPutPortfolioPreferences :: PutPortfolioPreferences -> TestTree
requestPutPortfolioPreferences =
  req
    "PutPortfolioPreferences"
    "fixture/PutPortfolioPreferences.yaml"

requestStartAssessment :: StartAssessment -> TestTree
requestStartAssessment =
  req
    "StartAssessment"
    "fixture/StartAssessment.yaml"

requestStartImportFileTask :: StartImportFileTask -> TestTree
requestStartImportFileTask =
  req
    "StartImportFileTask"
    "fixture/StartImportFileTask.yaml"

requestStartRecommendationReportGeneration :: StartRecommendationReportGeneration -> TestTree
requestStartRecommendationReportGeneration =
  req
    "StartRecommendationReportGeneration"
    "fixture/StartRecommendationReportGeneration.yaml"

requestStopAssessment :: StopAssessment -> TestTree
requestStopAssessment =
  req
    "StopAssessment"
    "fixture/StopAssessment.yaml"

requestUpdateApplicationComponentConfig :: UpdateApplicationComponentConfig -> TestTree
requestUpdateApplicationComponentConfig =
  req
    "UpdateApplicationComponentConfig"
    "fixture/UpdateApplicationComponentConfig.yaml"

requestUpdateServerConfig :: UpdateServerConfig -> TestTree
requestUpdateServerConfig =
  req
    "UpdateServerConfig"
    "fixture/UpdateServerConfig.yaml"

-- Responses

responseGetApplicationComponentDetails :: GetApplicationComponentDetailsResponse -> TestTree
responseGetApplicationComponentDetails =
  res
    "GetApplicationComponentDetailsResponse"
    "fixture/GetApplicationComponentDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationComponentDetails)

responseGetApplicationComponentStrategies :: GetApplicationComponentStrategiesResponse -> TestTree
responseGetApplicationComponentStrategies =
  res
    "GetApplicationComponentStrategiesResponse"
    "fixture/GetApplicationComponentStrategiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplicationComponentStrategies)

responseGetAssessment :: GetAssessmentResponse -> TestTree
responseGetAssessment =
  res
    "GetAssessmentResponse"
    "fixture/GetAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssessment)

responseGetImportFileTask :: GetImportFileTaskResponse -> TestTree
responseGetImportFileTask =
  res
    "GetImportFileTaskResponse"
    "fixture/GetImportFileTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImportFileTask)

responseGetLatestAssessmentId :: GetLatestAssessmentIdResponse -> TestTree
responseGetLatestAssessmentId =
  res
    "GetLatestAssessmentIdResponse"
    "fixture/GetLatestAssessmentIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLatestAssessmentId)

responseGetPortfolioPreferences :: GetPortfolioPreferencesResponse -> TestTree
responseGetPortfolioPreferences =
  res
    "GetPortfolioPreferencesResponse"
    "fixture/GetPortfolioPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPortfolioPreferences)

responseGetPortfolioSummary :: GetPortfolioSummaryResponse -> TestTree
responseGetPortfolioSummary =
  res
    "GetPortfolioSummaryResponse"
    "fixture/GetPortfolioSummaryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPortfolioSummary)

responseGetRecommendationReportDetails :: GetRecommendationReportDetailsResponse -> TestTree
responseGetRecommendationReportDetails =
  res
    "GetRecommendationReportDetailsResponse"
    "fixture/GetRecommendationReportDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendationReportDetails)

responseGetServerDetails :: GetServerDetailsResponse -> TestTree
responseGetServerDetails =
  res
    "GetServerDetailsResponse"
    "fixture/GetServerDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServerDetails)

responseGetServerStrategies :: GetServerStrategiesResponse -> TestTree
responseGetServerStrategies =
  res
    "GetServerStrategiesResponse"
    "fixture/GetServerStrategiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServerStrategies)

responseListApplicationComponents :: ListApplicationComponentsResponse -> TestTree
responseListApplicationComponents =
  res
    "ListApplicationComponentsResponse"
    "fixture/ListApplicationComponentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationComponents)

responseListCollectors :: ListCollectorsResponse -> TestTree
responseListCollectors =
  res
    "ListCollectorsResponse"
    "fixture/ListCollectorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCollectors)

responseListImportFileTask :: ListImportFileTaskResponse -> TestTree
responseListImportFileTask =
  res
    "ListImportFileTaskResponse"
    "fixture/ListImportFileTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImportFileTask)

responseListServers :: ListServersResponse -> TestTree
responseListServers =
  res
    "ListServersResponse"
    "fixture/ListServersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListServers)

responsePutPortfolioPreferences :: PutPortfolioPreferencesResponse -> TestTree
responsePutPortfolioPreferences =
  res
    "PutPortfolioPreferencesResponse"
    "fixture/PutPortfolioPreferencesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutPortfolioPreferences)

responseStartAssessment :: StartAssessmentResponse -> TestTree
responseStartAssessment =
  res
    "StartAssessmentResponse"
    "fixture/StartAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartAssessment)

responseStartImportFileTask :: StartImportFileTaskResponse -> TestTree
responseStartImportFileTask =
  res
    "StartImportFileTaskResponse"
    "fixture/StartImportFileTaskResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImportFileTask)

responseStartRecommendationReportGeneration :: StartRecommendationReportGenerationResponse -> TestTree
responseStartRecommendationReportGeneration =
  res
    "StartRecommendationReportGenerationResponse"
    "fixture/StartRecommendationReportGenerationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartRecommendationReportGeneration)

responseStopAssessment :: StopAssessmentResponse -> TestTree
responseStopAssessment =
  res
    "StopAssessmentResponse"
    "fixture/StopAssessmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopAssessment)

responseUpdateApplicationComponentConfig :: UpdateApplicationComponentConfigResponse -> TestTree
responseUpdateApplicationComponentConfig =
  res
    "UpdateApplicationComponentConfigResponse"
    "fixture/UpdateApplicationComponentConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationComponentConfig)

responseUpdateServerConfig :: UpdateServerConfigResponse -> TestTree
responseUpdateServerConfig =
  res
    "UpdateServerConfigResponse"
    "fixture/UpdateServerConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateServerConfig)
