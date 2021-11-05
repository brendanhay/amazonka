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

import Amazonka.CognitoSync
import qualified Data.Proxy as Proxy
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
--             newDescribeDataset
--
--         , requestSetCognitoEvents $
--             newSetCognitoEvents
--
--         , requestDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsage
--
--         , requestGetBulkPublishDetails $
--             newGetBulkPublishDetails
--
--         , requestListIdentityPoolUsage $
--             newListIdentityPoolUsage
--
--         , requestSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfiguration
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestGetCognitoEvents $
--             newGetCognitoEvents
--
--         , requestDescribeIdentityUsage $
--             newDescribeIdentityUsage
--
--         , requestRegisterDevice $
--             newRegisterDevice
--
--         , requestSubscribeToDataset $
--             newSubscribeToDataset
--
--         , requestGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfiguration
--
--         , requestListRecords $
--             newListRecords
--
--         , requestUnsubscribeFromDataset $
--             newUnsubscribeFromDataset
--
--         , requestUpdateRecords $
--             newUpdateRecords
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestBulkPublish $
--             newBulkPublish
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseSetCognitoEvents $
--             newSetCognitoEventsResponse
--
--         , responseDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsageResponse
--
--         , responseGetBulkPublishDetails $
--             newGetBulkPublishDetailsResponse
--
--         , responseListIdentityPoolUsage $
--             newListIdentityPoolUsageResponse
--
--         , responseSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfigurationResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseGetCognitoEvents $
--             newGetCognitoEventsResponse
--
--         , responseDescribeIdentityUsage $
--             newDescribeIdentityUsageResponse
--
--         , responseRegisterDevice $
--             newRegisterDeviceResponse
--
--         , responseSubscribeToDataset $
--             newSubscribeToDatasetResponse
--
--         , responseGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfigurationResponse
--
--         , responseListRecords $
--             newListRecordsResponse
--
--         , responseUnsubscribeFromDataset $
--             newUnsubscribeFromDatasetResponse
--
--         , responseUpdateRecords $
--             newUpdateRecordsResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseBulkPublish $
--             newBulkPublishResponse
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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseSetCognitoEvents :: SetCognitoEventsResponse -> TestTree
responseSetCognitoEvents =
  res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetCognitoEvents)

responseDescribeIdentityPoolUsage :: DescribeIdentityPoolUsageResponse -> TestTree
responseDescribeIdentityPoolUsage =
  res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityPoolUsage)

responseGetBulkPublishDetails :: GetBulkPublishDetailsResponse -> TestTree
responseGetBulkPublishDetails =
  res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBulkPublishDetails)

responseListIdentityPoolUsage :: ListIdentityPoolUsageResponse -> TestTree
responseListIdentityPoolUsage =
  res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityPoolUsage)

responseSetIdentityPoolConfiguration :: SetIdentityPoolConfigurationResponse -> TestTree
responseSetIdentityPoolConfiguration =
  res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityPoolConfiguration)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseGetCognitoEvents :: GetCognitoEventsResponse -> TestTree
responseGetCognitoEvents =
  res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCognitoEvents)

responseDescribeIdentityUsage :: DescribeIdentityUsageResponse -> TestTree
responseDescribeIdentityUsage =
  res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityUsage)

responseRegisterDevice :: RegisterDeviceResponse -> TestTree
responseRegisterDevice =
  res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDevice)

responseSubscribeToDataset :: SubscribeToDatasetResponse -> TestTree
responseSubscribeToDataset =
  res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubscribeToDataset)

responseGetIdentityPoolConfiguration :: GetIdentityPoolConfigurationResponse -> TestTree
responseGetIdentityPoolConfiguration =
  res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityPoolConfiguration)

responseListRecords :: ListRecordsResponse -> TestTree
responseListRecords =
  res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecords)

responseUnsubscribeFromDataset :: UnsubscribeFromDatasetResponse -> TestTree
responseUnsubscribeFromDataset =
  res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UnsubscribeFromDataset)

responseUpdateRecords :: UpdateRecordsResponse -> TestTree
responseUpdateRecords =
  res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateRecords)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseBulkPublish :: BulkPublishResponse -> TestTree
responseBulkPublish =
  res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BulkPublish)
