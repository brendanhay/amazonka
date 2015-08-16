{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.CognitoSync where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.CognitoSync
import Test.AWS.CognitoSync.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeDataset $
--             describeDataset
--
--         , testDescribeIdentityPoolUsage $
--             describeIdentityPoolUsage
--
--         , testSetCognitoEvents $
--             setCognitoEvents
--
--         , testListIdentityPoolUsage $
--             listIdentityPoolUsage
--
--         , testGetBulkPublishDetails $
--             getBulkPublishDetails
--
--         , testSetIdentityPoolConfiguration $
--             setIdentityPoolConfiguration
--
--         , testDeleteDataset $
--             deleteDataset
--
--         , testDescribeIdentityUsage $
--             describeIdentityUsage
--
--         , testGetCognitoEvents $
--             getCognitoEvents
--
--         , testRegisterDevice $
--             registerDevice
--
--         , testSubscribeToDataset $
--             subscribeToDataset
--
--         , testGetIdentityPoolConfiguration $
--             getIdentityPoolConfiguration
--
--         , testListRecords $
--             listRecords
--
--         , testUnsubscribeFromDataset $
--             unsubscribeFromDataset
--
--         , testUpdateRecords $
--             updateRecords
--
--         , testListDatasets $
--             listDatasets
--
--         , testBulkPublish $
--             bulkPublish
--
--           ]

--     , testGroup "response"
--         [ testDescribeDatasetResponse $
--             describeDatasetResponse
--
--         , testDescribeIdentityPoolUsageResponse $
--             describeIdentityPoolUsageResponse
--
--         , testSetCognitoEventsResponse $
--             setCognitoEventsResponse
--
--         , testListIdentityPoolUsageResponse $
--             listIdentityPoolUsageResponse
--
--         , testGetBulkPublishDetailsResponse $
--             getBulkPublishDetailsResponse
--
--         , testSetIdentityPoolConfigurationResponse $
--             setIdentityPoolConfigurationResponse
--
--         , testDeleteDatasetResponse $
--             deleteDatasetResponse
--
--         , testDescribeIdentityUsageResponse $
--             describeIdentityUsageResponse
--
--         , testGetCognitoEventsResponse $
--             getCognitoEventsResponse
--
--         , testRegisterDeviceResponse $
--             registerDeviceResponse
--
--         , testSubscribeToDatasetResponse $
--             subscribeToDatasetResponse
--
--         , testGetIdentityPoolConfigurationResponse $
--             getIdentityPoolConfigurationResponse
--
--         , testListRecordsResponse $
--             listRecordsResponse
--
--         , testUnsubscribeFromDatasetResponse $
--             unsubscribeFromDatasetResponse
--
--         , testUpdateRecordsResponse $
--             updateRecordsResponse
--
--         , testListDatasetsResponse $
--             listDatasetsResponse
--
--         , testBulkPublishResponse $
--             bulkPublishResponse
--
--           ]
--     ]

-- Requests

testDescribeDataset :: DescribeDataset -> TestTree
testDescribeDataset = req
    "DescribeDataset"
    "fixture/DescribeDataset"

testDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
testDescribeIdentityPoolUsage = req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage"

testSetCognitoEvents :: SetCognitoEvents -> TestTree
testSetCognitoEvents = req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents"

testListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
testListIdentityPoolUsage = req
    "ListIdentityPoolUsage"
    "fixture/ListIdentityPoolUsage"

testGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
testGetBulkPublishDetails = req
    "GetBulkPublishDetails"
    "fixture/GetBulkPublishDetails"

testSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
testSetIdentityPoolConfiguration = req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration"

testDeleteDataset :: DeleteDataset -> TestTree
testDeleteDataset = req
    "DeleteDataset"
    "fixture/DeleteDataset"

testDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
testDescribeIdentityUsage = req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage"

testGetCognitoEvents :: GetCognitoEvents -> TestTree
testGetCognitoEvents = req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents"

testRegisterDevice :: RegisterDevice -> TestTree
testRegisterDevice = req
    "RegisterDevice"
    "fixture/RegisterDevice"

testSubscribeToDataset :: SubscribeToDataset -> TestTree
testSubscribeToDataset = req
    "SubscribeToDataset"
    "fixture/SubscribeToDataset"

testGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
testGetIdentityPoolConfiguration = req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration"

testListRecords :: ListRecords -> TestTree
testListRecords = req
    "ListRecords"
    "fixture/ListRecords"

testUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
testUnsubscribeFromDataset = req
    "UnsubscribeFromDataset"
    "fixture/UnsubscribeFromDataset"

testUpdateRecords :: UpdateRecords -> TestTree
testUpdateRecords = req
    "UpdateRecords"
    "fixture/UpdateRecords"

testListDatasets :: ListDatasets -> TestTree
testListDatasets = req
    "ListDatasets"
    "fixture/ListDatasets"

testBulkPublish :: BulkPublish -> TestTree
testBulkPublish = req
    "BulkPublish"
    "fixture/BulkPublish"

-- Responses

testDescribeDatasetResponse :: DescribeDatasetResponse -> TestTree
testDescribeDatasetResponse = res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse"
    (Proxy :: Proxy DescribeDataset)

testDescribeIdentityPoolUsageResponse :: DescribeIdentityPoolUsageResponse -> TestTree
testDescribeIdentityPoolUsageResponse = res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse"
    (Proxy :: Proxy DescribeIdentityPoolUsage)

testSetCognitoEventsResponse :: SetCognitoEventsResponse -> TestTree
testSetCognitoEventsResponse = res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse"
    (Proxy :: Proxy SetCognitoEvents)

testListIdentityPoolUsageResponse :: ListIdentityPoolUsageResponse -> TestTree
testListIdentityPoolUsageResponse = res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse"
    (Proxy :: Proxy ListIdentityPoolUsage)

testGetBulkPublishDetailsResponse :: GetBulkPublishDetailsResponse -> TestTree
testGetBulkPublishDetailsResponse = res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse"
    (Proxy :: Proxy GetBulkPublishDetails)

testSetIdentityPoolConfigurationResponse :: SetIdentityPoolConfigurationResponse -> TestTree
testSetIdentityPoolConfigurationResponse = res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy SetIdentityPoolConfiguration)

testDeleteDatasetResponse :: DeleteDatasetResponse -> TestTree
testDeleteDatasetResponse = res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse"
    (Proxy :: Proxy DeleteDataset)

testDescribeIdentityUsageResponse :: DescribeIdentityUsageResponse -> TestTree
testDescribeIdentityUsageResponse = res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse"
    (Proxy :: Proxy DescribeIdentityUsage)

testGetCognitoEventsResponse :: GetCognitoEventsResponse -> TestTree
testGetCognitoEventsResponse = res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse"
    (Proxy :: Proxy GetCognitoEvents)

testRegisterDeviceResponse :: RegisterDeviceResponse -> TestTree
testRegisterDeviceResponse = res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse"
    (Proxy :: Proxy RegisterDevice)

testSubscribeToDatasetResponse :: SubscribeToDatasetResponse -> TestTree
testSubscribeToDatasetResponse = res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse"
    (Proxy :: Proxy SubscribeToDataset)

testGetIdentityPoolConfigurationResponse :: GetIdentityPoolConfigurationResponse -> TestTree
testGetIdentityPoolConfigurationResponse = res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy GetIdentityPoolConfiguration)

testListRecordsResponse :: ListRecordsResponse -> TestTree
testListRecordsResponse = res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse"
    (Proxy :: Proxy ListRecords)

testUnsubscribeFromDatasetResponse :: UnsubscribeFromDatasetResponse -> TestTree
testUnsubscribeFromDatasetResponse = res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse"
    (Proxy :: Proxy UnsubscribeFromDataset)

testUpdateRecordsResponse :: UpdateRecordsResponse -> TestTree
testUpdateRecordsResponse = res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse"
    (Proxy :: Proxy UpdateRecords)

testListDatasetsResponse :: ListDatasetsResponse -> TestTree
testListDatasetsResponse = res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse"
    (Proxy :: Proxy ListDatasets)

testBulkPublishResponse :: BulkPublishResponse -> TestTree
testBulkPublishResponse = res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse"
    (Proxy :: Proxy BulkPublish)
