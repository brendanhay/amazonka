{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestUpdateRecords $
--             newUpdateRecords
--
--         , requestListRecords $
--             newListRecords
--
--         , requestRegisterDevice $
--             newRegisterDevice
--
--         , requestSubscribeToDataset $
--             newSubscribeToDataset
--
--         , requestBulkPublish $
--             newBulkPublish
--
--         , requestDescribeIdentityUsage $
--             newDescribeIdentityUsage
--
--         , requestSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfiguration
--
--         , requestGetBulkPublishDetails $
--             newGetBulkPublishDetails
--
--         , requestListIdentityPoolUsage $
--             newListIdentityPoolUsage
--
--         , requestSetCognitoEvents $
--             newSetCognitoEvents
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestUnsubscribeFromDataset $
--             newUnsubscribeFromDataset
--
--         , requestGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfiguration
--
--         , requestGetCognitoEvents $
--             newGetCognitoEvents
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsage
--
--           ]

--     , testGroup "response"
--         [ responseUpdateRecords $
--             newUpdateRecordsResponse
--
--         , responseListRecords $
--             newListRecordsResponse
--
--         , responseRegisterDevice $
--             newRegisterDeviceResponse
--
--         , responseSubscribeToDataset $
--             newSubscribeToDatasetResponse
--
--         , responseBulkPublish $
--             newBulkPublishResponse
--
--         , responseDescribeIdentityUsage $
--             newDescribeIdentityUsageResponse
--
--         , responseSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfigurationResponse
--
--         , responseGetBulkPublishDetails $
--             newGetBulkPublishDetailsResponse
--
--         , responseListIdentityPoolUsage $
--             newListIdentityPoolUsageResponse
--
--         , responseSetCognitoEvents $
--             newSetCognitoEventsResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseUnsubscribeFromDataset $
--             newUnsubscribeFromDatasetResponse
--
--         , responseGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfigurationResponse
--
--         , responseGetCognitoEvents $
--             newGetCognitoEventsResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsageResponse
--
--           ]
--     ]

-- Requests

requestUpdateRecords :: UpdateRecords -> TestTree
requestUpdateRecords =
  req
    "UpdateRecords"
    "fixture/UpdateRecords.yaml"

requestListRecords :: ListRecords -> TestTree
requestListRecords =
  req
    "ListRecords"
    "fixture/ListRecords.yaml"

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

requestBulkPublish :: BulkPublish -> TestTree
requestBulkPublish =
  req
    "BulkPublish"
    "fixture/BulkPublish.yaml"

requestDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
requestDescribeIdentityUsage =
  req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage.yaml"

requestSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
requestSetIdentityPoolConfiguration =
  req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration.yaml"

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

requestSetCognitoEvents :: SetCognitoEvents -> TestTree
requestSetCognitoEvents =
  req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
requestUnsubscribeFromDataset =
  req
    "UnsubscribeFromDataset"
    "fixture/UnsubscribeFromDataset.yaml"

requestGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
requestGetIdentityPoolConfiguration =
  req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration.yaml"

requestGetCognitoEvents :: GetCognitoEvents -> TestTree
requestGetCognitoEvents =
  req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
requestDescribeIdentityPoolUsage =
  req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage.yaml"

-- Responses

responseUpdateRecords :: UpdateRecordsResponse -> TestTree
responseUpdateRecords =
  res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateRecords)

responseListRecords :: ListRecordsResponse -> TestTree
responseListRecords =
  res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRecords)

responseRegisterDevice :: RegisterDeviceResponse -> TestTree
responseRegisterDevice =
  res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDevice)

responseSubscribeToDataset :: SubscribeToDatasetResponse -> TestTree
responseSubscribeToDataset =
  res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy SubscribeToDataset)

responseBulkPublish :: BulkPublishResponse -> TestTree
responseBulkPublish =
  res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    defaultService
    (Proxy :: Proxy BulkPublish)

responseDescribeIdentityUsage :: DescribeIdentityUsageResponse -> TestTree
responseDescribeIdentityUsage =
  res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityUsage)

responseSetIdentityPoolConfiguration :: SetIdentityPoolConfigurationResponse -> TestTree
responseSetIdentityPoolConfiguration =
  res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy SetIdentityPoolConfiguration)

responseGetBulkPublishDetails :: GetBulkPublishDetailsResponse -> TestTree
responseGetBulkPublishDetails =
  res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetBulkPublishDetails)

responseListIdentityPoolUsage :: ListIdentityPoolUsageResponse -> TestTree
responseListIdentityPoolUsage =
  res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy :: Proxy ListIdentityPoolUsage)

responseSetCognitoEvents :: SetCognitoEventsResponse -> TestTree
responseSetCognitoEvents =
  res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    defaultService
    (Proxy :: Proxy SetCognitoEvents)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDataset)

responseUnsubscribeFromDataset :: UnsubscribeFromDatasetResponse -> TestTree
responseUnsubscribeFromDataset =
  res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy UnsubscribeFromDataset)

responseGetIdentityPoolConfiguration :: GetIdentityPoolConfigurationResponse -> TestTree
responseGetIdentityPoolConfiguration =
  res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetIdentityPoolConfiguration)

responseGetCognitoEvents :: GetCognitoEventsResponse -> TestTree
responseGetCognitoEvents =
  res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    defaultService
    (Proxy :: Proxy GetCognitoEvents)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDataset)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDatasets)

responseDescribeIdentityPoolUsage :: DescribeIdentityPoolUsageResponse -> TestTree
responseDescribeIdentityPoolUsage =
  res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeIdentityPoolUsage)
