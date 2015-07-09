{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.CognitoSync where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.CognitoSync

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
testDescribeDataset = undefined

testDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
testDescribeIdentityPoolUsage = undefined

testSetCognitoEvents :: SetCognitoEvents -> TestTree
testSetCognitoEvents = undefined

testListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
testListIdentityPoolUsage = undefined

testGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
testGetBulkPublishDetails = undefined

testSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
testSetIdentityPoolConfiguration = undefined

testDeleteDataset :: DeleteDataset -> TestTree
testDeleteDataset = undefined

testDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
testDescribeIdentityUsage = undefined

testGetCognitoEvents :: GetCognitoEvents -> TestTree
testGetCognitoEvents = undefined

testRegisterDevice :: RegisterDevice -> TestTree
testRegisterDevice = undefined

testSubscribeToDataset :: SubscribeToDataset -> TestTree
testSubscribeToDataset = undefined

testGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
testGetIdentityPoolConfiguration = undefined

testListRecords :: ListRecords -> TestTree
testListRecords = undefined

testUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
testUnsubscribeFromDataset = undefined

testUpdateRecords :: UpdateRecords -> TestTree
testUpdateRecords = undefined

testListDatasets :: ListDatasets -> TestTree
testListDatasets = undefined

testBulkPublish :: BulkPublish -> TestTree
testBulkPublish = undefined

-- Responses

testDescribeDatasetResponse :: DescribeDatasetResponse -> TestTree
testDescribeDatasetResponse = resp
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse"
    (Proxy :: Proxy DescribeDataset)

testDescribeIdentityPoolUsageResponse :: DescribeIdentityPoolUsageResponse -> TestTree
testDescribeIdentityPoolUsageResponse = resp
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse"
    (Proxy :: Proxy DescribeIdentityPoolUsage)

testSetCognitoEventsResponse :: SetCognitoEventsResponse -> TestTree
testSetCognitoEventsResponse = resp
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse"
    (Proxy :: Proxy SetCognitoEvents)

testListIdentityPoolUsageResponse :: ListIdentityPoolUsageResponse -> TestTree
testListIdentityPoolUsageResponse = resp
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse"
    (Proxy :: Proxy ListIdentityPoolUsage)

testGetBulkPublishDetailsResponse :: GetBulkPublishDetailsResponse -> TestTree
testGetBulkPublishDetailsResponse = resp
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse"
    (Proxy :: Proxy GetBulkPublishDetails)

testSetIdentityPoolConfigurationResponse :: SetIdentityPoolConfigurationResponse -> TestTree
testSetIdentityPoolConfigurationResponse = resp
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy SetIdentityPoolConfiguration)

testDeleteDatasetResponse :: DeleteDatasetResponse -> TestTree
testDeleteDatasetResponse = resp
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse"
    (Proxy :: Proxy DeleteDataset)

testDescribeIdentityUsageResponse :: DescribeIdentityUsageResponse -> TestTree
testDescribeIdentityUsageResponse = resp
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse"
    (Proxy :: Proxy DescribeIdentityUsage)

testGetCognitoEventsResponse :: GetCognitoEventsResponse -> TestTree
testGetCognitoEventsResponse = resp
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse"
    (Proxy :: Proxy GetCognitoEvents)

testRegisterDeviceResponse :: RegisterDeviceResponse -> TestTree
testRegisterDeviceResponse = resp
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse"
    (Proxy :: Proxy RegisterDevice)

testSubscribeToDatasetResponse :: SubscribeToDatasetResponse -> TestTree
testSubscribeToDatasetResponse = resp
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse"
    (Proxy :: Proxy SubscribeToDataset)

testGetIdentityPoolConfigurationResponse :: GetIdentityPoolConfigurationResponse -> TestTree
testGetIdentityPoolConfigurationResponse = resp
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy GetIdentityPoolConfiguration)

testListRecordsResponse :: ListRecordsResponse -> TestTree
testListRecordsResponse = resp
    "ListRecordsResponse"
    "fixture/ListRecordsResponse"
    (Proxy :: Proxy ListRecords)

testUnsubscribeFromDatasetResponse :: UnsubscribeFromDatasetResponse -> TestTree
testUnsubscribeFromDatasetResponse = resp
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse"
    (Proxy :: Proxy UnsubscribeFromDataset)

testUpdateRecordsResponse :: UpdateRecordsResponse -> TestTree
testUpdateRecordsResponse = resp
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse"
    (Proxy :: Proxy UpdateRecords)

testListDatasetsResponse :: ListDatasetsResponse -> TestTree
testListDatasetsResponse = resp
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse"
    (Proxy :: Proxy ListDatasets)

testBulkPublishResponse :: BulkPublishResponse -> TestTree
testBulkPublishResponse = resp
    "BulkPublishResponse"
    "fixture/BulkPublishResponse"
    (Proxy :: Proxy BulkPublish)

instance Out BulkPublish
instance Out BulkPublishResponse
instance Out BulkPublishStatus
instance Out CognitoStreams
instance Out Dataset
instance Out DeleteDataset
instance Out DeleteDatasetResponse
instance Out DescribeDataset
instance Out DescribeDatasetResponse
instance Out DescribeIdentityPoolUsage
instance Out DescribeIdentityPoolUsageResponse
instance Out DescribeIdentityUsage
instance Out DescribeIdentityUsageResponse
instance Out GetBulkPublishDetails
instance Out GetBulkPublishDetailsResponse
instance Out GetCognitoEvents
instance Out GetCognitoEventsResponse
instance Out GetIdentityPoolConfiguration
instance Out GetIdentityPoolConfigurationResponse
instance Out IdentityPoolUsage
instance Out IdentityUsage
instance Out ListDatasets
instance Out ListDatasetsResponse
instance Out ListIdentityPoolUsage
instance Out ListIdentityPoolUsageResponse
instance Out ListRecords
instance Out ListRecordsResponse
instance Out Operation
instance Out Platform
instance Out PushSync
instance Out Record
instance Out RecordPatch
instance Out RegisterDevice
instance Out RegisterDeviceResponse
instance Out SetCognitoEvents
instance Out SetCognitoEventsResponse
instance Out SetIdentityPoolConfiguration
instance Out SetIdentityPoolConfigurationResponse
instance Out StreamingStatus
instance Out SubscribeToDataset
instance Out SubscribeToDatasetResponse
instance Out UnsubscribeFromDataset
instance Out UnsubscribeFromDatasetResponse
instance Out UpdateRecords
instance Out UpdateRecordsResponse
