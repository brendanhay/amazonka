{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CognitoSync
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CognitoSync where

import Amazonka.CognitoSync
import qualified Data.Proxy as Proxy
import Test.Amazonka.CognitoSync.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestBulkPublish $
--             newBulkPublish
--
--         , requestDeleteDataset $
--             newDeleteDataset
--
--         , requestDescribeDataset $
--             newDescribeDataset
--
--         , requestDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsage
--
--         , requestDescribeIdentityUsage $
--             newDescribeIdentityUsage
--
--         , requestGetBulkPublishDetails $
--             newGetBulkPublishDetails
--
--         , requestGetCognitoEvents $
--             newGetCognitoEvents
--
--         , requestGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfiguration
--
--         , requestListDatasets $
--             newListDatasets
--
--         , requestListIdentityPoolUsage $
--             newListIdentityPoolUsage
--
--         , requestListRecords $
--             newListRecords
--
--         , requestRegisterDevice $
--             newRegisterDevice
--
--         , requestSetCognitoEvents $
--             newSetCognitoEvents
--
--         , requestSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfiguration
--
--         , requestSubscribeToDataset $
--             newSubscribeToDataset
--
--         , requestUnsubscribeFromDataset $
--             newUnsubscribeFromDataset
--
--         , requestUpdateRecords $
--             newUpdateRecords
--
--           ]

--     , testGroup "response"
--         [ responseBulkPublish $
--             newBulkPublishResponse
--
--         , responseDeleteDataset $
--             newDeleteDatasetResponse
--
--         , responseDescribeDataset $
--             newDescribeDatasetResponse
--
--         , responseDescribeIdentityPoolUsage $
--             newDescribeIdentityPoolUsageResponse
--
--         , responseDescribeIdentityUsage $
--             newDescribeIdentityUsageResponse
--
--         , responseGetBulkPublishDetails $
--             newGetBulkPublishDetailsResponse
--
--         , responseGetCognitoEvents $
--             newGetCognitoEventsResponse
--
--         , responseGetIdentityPoolConfiguration $
--             newGetIdentityPoolConfigurationResponse
--
--         , responseListDatasets $
--             newListDatasetsResponse
--
--         , responseListIdentityPoolUsage $
--             newListIdentityPoolUsageResponse
--
--         , responseListRecords $
--             newListRecordsResponse
--
--         , responseRegisterDevice $
--             newRegisterDeviceResponse
--
--         , responseSetCognitoEvents $
--             newSetCognitoEventsResponse
--
--         , responseSetIdentityPoolConfiguration $
--             newSetIdentityPoolConfigurationResponse
--
--         , responseSubscribeToDataset $
--             newSubscribeToDatasetResponse
--
--         , responseUnsubscribeFromDataset $
--             newUnsubscribeFromDatasetResponse
--
--         , responseUpdateRecords $
--             newUpdateRecordsResponse
--
--           ]
--     ]

-- Requests

requestBulkPublish :: BulkPublish -> TestTree
requestBulkPublish =
  req
    "BulkPublish"
    "fixture/BulkPublish.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset =
  req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset =
  req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
requestDescribeIdentityPoolUsage =
  req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage.yaml"

requestDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
requestDescribeIdentityUsage =
  req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage.yaml"

requestGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
requestGetBulkPublishDetails =
  req
    "GetBulkPublishDetails"
    "fixture/GetBulkPublishDetails.yaml"

requestGetCognitoEvents :: GetCognitoEvents -> TestTree
requestGetCognitoEvents =
  req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents.yaml"

requestGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
requestGetIdentityPoolConfiguration =
  req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets =
  req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
requestListIdentityPoolUsage =
  req
    "ListIdentityPoolUsage"
    "fixture/ListIdentityPoolUsage.yaml"

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

requestSetCognitoEvents :: SetCognitoEvents -> TestTree
requestSetCognitoEvents =
  req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents.yaml"

requestSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
requestSetIdentityPoolConfiguration =
  req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration.yaml"

requestSubscribeToDataset :: SubscribeToDataset -> TestTree
requestSubscribeToDataset =
  req
    "SubscribeToDataset"
    "fixture/SubscribeToDataset.yaml"

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

-- Responses

responseBulkPublish :: BulkPublishResponse -> TestTree
responseBulkPublish =
  res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BulkPublish)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset =
  res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDataset)

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset =
  res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDataset)

responseDescribeIdentityPoolUsage :: DescribeIdentityPoolUsageResponse -> TestTree
responseDescribeIdentityPoolUsage =
  res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityPoolUsage)

responseDescribeIdentityUsage :: DescribeIdentityUsageResponse -> TestTree
responseDescribeIdentityUsage =
  res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeIdentityUsage)

responseGetBulkPublishDetails :: GetBulkPublishDetailsResponse -> TestTree
responseGetBulkPublishDetails =
  res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBulkPublishDetails)

responseGetCognitoEvents :: GetCognitoEventsResponse -> TestTree
responseGetCognitoEvents =
  res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCognitoEvents)

responseGetIdentityPoolConfiguration :: GetIdentityPoolConfigurationResponse -> TestTree
responseGetIdentityPoolConfiguration =
  res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityPoolConfiguration)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets =
  res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDatasets)

responseListIdentityPoolUsage :: ListIdentityPoolUsageResponse -> TestTree
responseListIdentityPoolUsage =
  res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityPoolUsage)

responseListRecords :: ListRecordsResponse -> TestTree
responseListRecords =
  res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRecords)

responseRegisterDevice :: RegisterDeviceResponse -> TestTree
responseRegisterDevice =
  res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDevice)

responseSetCognitoEvents :: SetCognitoEventsResponse -> TestTree
responseSetCognitoEvents =
  res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetCognitoEvents)

responseSetIdentityPoolConfiguration :: SetIdentityPoolConfigurationResponse -> TestTree
responseSetIdentityPoolConfiguration =
  res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetIdentityPoolConfiguration)

responseSubscribeToDataset :: SubscribeToDatasetResponse -> TestTree
responseSubscribeToDataset =
  res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SubscribeToDataset)

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
