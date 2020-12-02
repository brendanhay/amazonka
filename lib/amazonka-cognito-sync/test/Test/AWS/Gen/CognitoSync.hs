{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--             describeDataset
--
--         , requestSetCognitoEvents $
--             setCognitoEvents
--
--         , requestDescribeIdentityPoolUsage $
--             describeIdentityPoolUsage
--
--         , requestGetBulkPublishDetails $
--             getBulkPublishDetails
--
--         , requestListIdentityPoolUsage $
--             listIdentityPoolUsage
--
--         , requestSetIdentityPoolConfiguration $
--             setIdentityPoolConfiguration
--
--         , requestDeleteDataset $
--             deleteDataset
--
--         , requestGetCognitoEvents $
--             getCognitoEvents
--
--         , requestDescribeIdentityUsage $
--             describeIdentityUsage
--
--         , requestRegisterDevice $
--             registerDevice
--
--         , requestSubscribeToDataset $
--             subscribeToDataset
--
--         , requestGetIdentityPoolConfiguration $
--             getIdentityPoolConfiguration
--
--         , requestListRecords $
--             listRecords
--
--         , requestUnsubscribeFromDataset $
--             unsubscribeFromDataset
--
--         , requestUpdateRecords $
--             updateRecords
--
--         , requestListDatasets $
--             listDatasets
--
--         , requestBulkPublish $
--             bulkPublish
--
--           ]

--     , testGroup "response"
--         [ responseDescribeDataset $
--             describeDatasetResponse
--
--         , responseSetCognitoEvents $
--             setCognitoEventsResponse
--
--         , responseDescribeIdentityPoolUsage $
--             describeIdentityPoolUsageResponse
--
--         , responseGetBulkPublishDetails $
--             getBulkPublishDetailsResponse
--
--         , responseListIdentityPoolUsage $
--             listIdentityPoolUsageResponse
--
--         , responseSetIdentityPoolConfiguration $
--             setIdentityPoolConfigurationResponse
--
--         , responseDeleteDataset $
--             deleteDatasetResponse
--
--         , responseGetCognitoEvents $
--             getCognitoEventsResponse
--
--         , responseDescribeIdentityUsage $
--             describeIdentityUsageResponse
--
--         , responseRegisterDevice $
--             registerDeviceResponse
--
--         , responseSubscribeToDataset $
--             subscribeToDatasetResponse
--
--         , responseGetIdentityPoolConfiguration $
--             getIdentityPoolConfigurationResponse
--
--         , responseListRecords $
--             listRecordsResponse
--
--         , responseUnsubscribeFromDataset $
--             unsubscribeFromDatasetResponse
--
--         , responseUpdateRecords $
--             updateRecordsResponse
--
--         , responseListDatasets $
--             listDatasetsResponse
--
--         , responseBulkPublish $
--             bulkPublishResponse
--
--           ]
--     ]

-- Requests

requestDescribeDataset :: DescribeDataset -> TestTree
requestDescribeDataset = req
    "DescribeDataset"
    "fixture/DescribeDataset.yaml"

requestSetCognitoEvents :: SetCognitoEvents -> TestTree
requestSetCognitoEvents = req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents.yaml"

requestDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
requestDescribeIdentityPoolUsage = req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage.yaml"

requestGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
requestGetBulkPublishDetails = req
    "GetBulkPublishDetails"
    "fixture/GetBulkPublishDetails.yaml"

requestListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
requestListIdentityPoolUsage = req
    "ListIdentityPoolUsage"
    "fixture/ListIdentityPoolUsage.yaml"

requestSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
requestSetIdentityPoolConfiguration = req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration.yaml"

requestDeleteDataset :: DeleteDataset -> TestTree
requestDeleteDataset = req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

requestGetCognitoEvents :: GetCognitoEvents -> TestTree
requestGetCognitoEvents = req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents.yaml"

requestDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
requestDescribeIdentityUsage = req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage.yaml"

requestRegisterDevice :: RegisterDevice -> TestTree
requestRegisterDevice = req
    "RegisterDevice"
    "fixture/RegisterDevice.yaml"

requestSubscribeToDataset :: SubscribeToDataset -> TestTree
requestSubscribeToDataset = req
    "SubscribeToDataset"
    "fixture/SubscribeToDataset.yaml"

requestGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
requestGetIdentityPoolConfiguration = req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration.yaml"

requestListRecords :: ListRecords -> TestTree
requestListRecords = req
    "ListRecords"
    "fixture/ListRecords.yaml"

requestUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
requestUnsubscribeFromDataset = req
    "UnsubscribeFromDataset"
    "fixture/UnsubscribeFromDataset.yaml"

requestUpdateRecords :: UpdateRecords -> TestTree
requestUpdateRecords = req
    "UpdateRecords"
    "fixture/UpdateRecords.yaml"

requestListDatasets :: ListDatasets -> TestTree
requestListDatasets = req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

requestBulkPublish :: BulkPublish -> TestTree
requestBulkPublish = req
    "BulkPublish"
    "fixture/BulkPublish.yaml"

-- Responses

responseDescribeDataset :: DescribeDatasetResponse -> TestTree
responseDescribeDataset = res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeDataset)

responseSetCognitoEvents :: SetCognitoEventsResponse -> TestTree
responseSetCognitoEvents = res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    cognitoSync
    (Proxy :: Proxy SetCognitoEvents)

responseDescribeIdentityPoolUsage :: DescribeIdentityPoolUsageResponse -> TestTree
responseDescribeIdentityPoolUsage = res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeIdentityPoolUsage)

responseGetBulkPublishDetails :: GetBulkPublishDetailsResponse -> TestTree
responseGetBulkPublishDetails = res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetBulkPublishDetails)

responseListIdentityPoolUsage :: ListIdentityPoolUsageResponse -> TestTree
responseListIdentityPoolUsage = res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListIdentityPoolUsage)

responseSetIdentityPoolConfiguration :: SetIdentityPoolConfigurationResponse -> TestTree
responseSetIdentityPoolConfiguration = res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    cognitoSync
    (Proxy :: Proxy SetIdentityPoolConfiguration)

responseDeleteDataset :: DeleteDatasetResponse -> TestTree
responseDeleteDataset = res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy DeleteDataset)

responseGetCognitoEvents :: GetCognitoEventsResponse -> TestTree
responseGetCognitoEvents = res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetCognitoEvents)

responseDescribeIdentityUsage :: DescribeIdentityUsageResponse -> TestTree
responseDescribeIdentityUsage = res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeIdentityUsage)

responseRegisterDevice :: RegisterDeviceResponse -> TestTree
responseRegisterDevice = res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    cognitoSync
    (Proxy :: Proxy RegisterDevice)

responseSubscribeToDataset :: SubscribeToDatasetResponse -> TestTree
responseSubscribeToDataset = res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy SubscribeToDataset)

responseGetIdentityPoolConfiguration :: GetIdentityPoolConfigurationResponse -> TestTree
responseGetIdentityPoolConfiguration = res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetIdentityPoolConfiguration)

responseListRecords :: ListRecordsResponse -> TestTree
responseListRecords = res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListRecords)

responseUnsubscribeFromDataset :: UnsubscribeFromDatasetResponse -> TestTree
responseUnsubscribeFromDataset = res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy UnsubscribeFromDataset)

responseUpdateRecords :: UpdateRecordsResponse -> TestTree
responseUpdateRecords = res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse.proto"
    cognitoSync
    (Proxy :: Proxy UpdateRecords)

responseListDatasets :: ListDatasetsResponse -> TestTree
responseListDatasets = res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListDatasets)

responseBulkPublish :: BulkPublishResponse -> TestTree
responseBulkPublish = res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    cognitoSync
    (Proxy :: Proxy BulkPublish)
