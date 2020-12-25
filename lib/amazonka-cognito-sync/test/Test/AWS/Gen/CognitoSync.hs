{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CognitoSync where

import Data.Proxy
import Network.AWS.CognitoSync
import Test.AWS.CognitoSync.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDescribeDataset $
--             mkDescribeDataset
--
--         , requestSetCognitoEvents $
--             mkSetCognitoEvents
--
--         , requestDescribeIdentityPoolUsage $
--             mkDescribeIdentityPoolUsage
--
--         , requestGetBulkPublishDetails $
--             mkGetBulkPublishDetails
--
--         , requestListIdentityPoolUsage $
--             mkListIdentityPoolUsage
--
--         , requestSetIdentityPoolConfiguration $
--             mkSetIdentityPoolConfiguration
--
--         , requestDeleteDataset $
--             mkDeleteDataset
--
--         , requestGetCognitoEvents $
--             mkGetCognitoEvents
--
--         , requestDescribeIdentityUsage $
--             mkDescribeIdentityUsage
--
--         , requestRegisterDevice $
--             mkRegisterDevice
--
--         , requestSubscribeToDataset $
--             mkSubscribeToDataset
--
--         , requestGetIdentityPoolConfiguration $
--             mkGetIdentityPoolConfiguration
--
--         , requestListRecords $
--             mkListRecords
--
--         , requestUnsubscribeFromDataset $
--             mkUnsubscribeFromDataset
--
--         , requestUpdateRecords $
--             mkUpdateRecords
--
--         , requestListDatasets $
--             mkListDatasets
--
--         , requestBulkPublish $
--             mkBulkPublish
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDataset $
--             mkDescribeDatasetResponse
--
--         , responseSetCognitoEvents $
--             mkSetCognitoEventsResponse
--
--         , responseDescribeIdentityPoolUsage $
--             mkDescribeIdentityPoolUsageResponse
--
--         , responseGetBulkPublishDetails $
--             mkGetBulkPublishDetailsResponse
--
--         , responseListIdentityPoolUsage $
--             mkListIdentityPoolUsageResponse
--
--         , responseSetIdentityPoolConfiguration $
--             mkSetIdentityPoolConfigurationResponse
--
--         , responseDeleteDataset $
--             mkDeleteDatasetResponse
--
--         , responseGetCognitoEvents $
--             mkGetCognitoEventsResponse
--
--         , responseDescribeIdentityUsage $
--             mkDescribeIdentityUsageResponse
--
--         , responseRegisterDevice $
--             mkRegisterDeviceResponse
--
--         , responseSubscribeToDataset $
--             mkSubscribeToDatasetResponse
--
--         , responseGetIdentityPoolConfiguration $
--             mkGetIdentityPoolConfigurationResponse
--
--         , responseListRecords $
--             mkListRecordsResponse
--
--         , responseUnsubscribeFromDataset $
--             mkUnsubscribeFromDatasetResponse
--
--         , responseUpdateRecords $
--             mkUpdateRecordsResponse
--
--         , responseListDatasets $
--             mkListDatasetsResponse
--
--         , responseBulkPublish $
--             mkBulkPublishResponse
--
--           ]
--     ]

-- Requests

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestSetCognitoEvents :: SetCognitoEvents -> TestTree
requestSetCognitoEvents =
  req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents.yaml"

requestDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
requestDescribeIdentityPoolUsage =
  req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage.yaml"

requestGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
requestGetBulkPublishDetails =
  req
    "GetBulkPublishDetails"
    "fixture/GetBulkPublishDetails.yaml"

requestListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
requestListIdentityPoolUsage =
  req
    "ListIdentityPoolUsage"
    "fixture/ListIdentityPoolUsage.yaml"

requestSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
requestSetIdentityPoolConfiguration =
  req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestGetCognitoEvents :: GetCognitoEvents -> TestTree
requestGetCognitoEvents =
  req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents.yaml"

requestDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
requestDescribeIdentityUsage =
  req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage.yaml"

requestRegisterDevice :: RegisterDevice -> TestTree
requestRegisterDevice =
  req
    "RegisterDevice"
    "fixture/RegisterDevice.yaml"

requestSubscribeToDataset :: SubscribeToDataset -> TestTree
requestSubscribeToDataset =
  req
    "SubscribeToDataset"
    "fixture/SubscribeToDataset.yaml"

requestGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
requestGetIdentityPoolConfiguration =
  req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration.yaml"

requestListRecords :: ListRecords -> TestTree
requestListRecords =
  req
    "ListRecords"
    "fixture/ListRecords.yaml"

requestUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
requestUnsubscribeFromDataset =
  req
    "UnsubscribeFromDataset"
    "fixture/UnsubscribeFromDataset.yaml"

requestUpdateRecords :: UpdateRecords -> TestTree
requestUpdateRecords =
  req
    "UpdateRecords"
    "fixture/UpdateRecords.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestBulkPublish :: BulkPublish -> TestTree
requestBulkPublish =
  req
    "BulkPublish"
    "fixture/BulkPublish.yaml"

-- Responses

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeDataset)

responseSetCognitoEvents :: SetCognitoEventsResponse -> TestTree
responseSetCognitoEvents =
  res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetCognitoEvents)

responseDescribeIdentityPoolUsage :: DescribeIdentityPoolUsageResponse -> TestTree
responseDescribeIdentityPoolUsage =
  res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIdentityPoolUsage)

responseGetBulkPublishDetails :: GetBulkPublishDetailsResponse -> TestTree
responseGetBulkPublishDetails =
  res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetBulkPublishDetails)

responseListIdentityPoolUsage :: ListIdentityPoolUsageResponse -> TestTree
responseListIdentityPoolUsage =
  res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListIdentityPoolUsage)

responseSetIdentityPoolConfiguration :: SetIdentityPoolConfigurationResponse -> TestTree
responseSetIdentityPoolConfiguration =
  res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetIdentityPoolConfiguration)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteDataset)

responseGetCognitoEvents :: GetCognitoEventsResponse -> TestTree
responseGetCognitoEvents =
  res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetCognitoEvents)

responseDescribeIdentityUsage :: DescribeIdentityUsageResponse -> TestTree
responseDescribeIdentityUsage =
  res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeIdentityUsage)

responseRegisterDevice :: RegisterDeviceResponse -> TestTree
responseRegisterDevice =
  res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterDevice)

responseSubscribeToDataset :: SubscribeToDatasetResponse -> TestTree
responseSubscribeToDataset =
  res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SubscribeToDataset)

responseGetIdentityPoolConfiguration :: GetIdentityPoolConfigurationResponse -> TestTree
responseGetIdentityPoolConfiguration =
  res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetIdentityPoolConfiguration)

responseListRecords :: ListRecordsResponse -> TestTree
responseListRecords =
  res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRecords)

responseUnsubscribeFromDataset :: UnsubscribeFromDatasetResponse -> TestTree
responseUnsubscribeFromDataset =
  res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UnsubscribeFromDataset)

responseUpdateRecords :: UpdateRecordsResponse -> TestTree
responseUpdateRecords =
  res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateRecords)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDatasets)

responseBulkPublish :: BulkPublishResponse -> TestTree
responseBulkPublish =
  res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy BulkPublish)
