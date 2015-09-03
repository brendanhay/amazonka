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
--         , testSetCognitoEvents $
--             setCognitoEvents
--
--         , testDescribeIdentityPoolUsage $
--             describeIdentityPoolUsage
--
--         , testGetBulkPublishDetails $
--             getBulkPublishDetails
--
--         , testListIdentityPoolUsage $
--             listIdentityPoolUsage
--
--         , testSetIdentityPoolConfiguration $
--             setIdentityPoolConfiguration
--
--         , testDeleteDataset $
--             deleteDataset
--
--         , testGetCognitoEvents $
--             getCognitoEvents
--
--         , testDescribeIdentityUsage $
--             describeIdentityUsage
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
--         , testSetCognitoEventsResponse $
--             setCognitoEventsResponse
--
--         , testDescribeIdentityPoolUsageResponse $
--             describeIdentityPoolUsageResponse
--
--         , testGetBulkPublishDetailsResponse $
--             getBulkPublishDetailsResponse
--
--         , testListIdentityPoolUsageResponse $
--             listIdentityPoolUsageResponse
--
--         , testSetIdentityPoolConfigurationResponse $
--             setIdentityPoolConfigurationResponse
--
--         , testDeleteDatasetResponse $
--             deleteDatasetResponse
--
--         , testGetCognitoEventsResponse $
--             getCognitoEventsResponse
--
--         , testDescribeIdentityUsageResponse $
--             describeIdentityUsageResponse
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
    "fixture/DescribeDataset.yaml"

testSetCognitoEvents :: SetCognitoEvents -> TestTree
testSetCognitoEvents = req
    "SetCognitoEvents"
    "fixture/SetCognitoEvents.yaml"

testDescribeIdentityPoolUsage :: DescribeIdentityPoolUsage -> TestTree
testDescribeIdentityPoolUsage = req
    "DescribeIdentityPoolUsage"
    "fixture/DescribeIdentityPoolUsage.yaml"

testGetBulkPublishDetails :: GetBulkPublishDetails -> TestTree
testGetBulkPublishDetails = req
    "GetBulkPublishDetails"
    "fixture/GetBulkPublishDetails.yaml"

testListIdentityPoolUsage :: ListIdentityPoolUsage -> TestTree
testListIdentityPoolUsage = req
    "ListIdentityPoolUsage"
    "fixture/ListIdentityPoolUsage.yaml"

testSetIdentityPoolConfiguration :: SetIdentityPoolConfiguration -> TestTree
testSetIdentityPoolConfiguration = req
    "SetIdentityPoolConfiguration"
    "fixture/SetIdentityPoolConfiguration.yaml"

testDeleteDataset :: DeleteDataset -> TestTree
testDeleteDataset = req
    "DeleteDataset"
    "fixture/DeleteDataset.yaml"

testGetCognitoEvents :: GetCognitoEvents -> TestTree
testGetCognitoEvents = req
    "GetCognitoEvents"
    "fixture/GetCognitoEvents.yaml"

testDescribeIdentityUsage :: DescribeIdentityUsage -> TestTree
testDescribeIdentityUsage = req
    "DescribeIdentityUsage"
    "fixture/DescribeIdentityUsage.yaml"

testRegisterDevice :: RegisterDevice -> TestTree
testRegisterDevice = req
    "RegisterDevice"
    "fixture/RegisterDevice.yaml"

testSubscribeToDataset :: SubscribeToDataset -> TestTree
testSubscribeToDataset = req
    "SubscribeToDataset"
    "fixture/SubscribeToDataset.yaml"

testGetIdentityPoolConfiguration :: GetIdentityPoolConfiguration -> TestTree
testGetIdentityPoolConfiguration = req
    "GetIdentityPoolConfiguration"
    "fixture/GetIdentityPoolConfiguration.yaml"

testListRecords :: ListRecords -> TestTree
testListRecords = req
    "ListRecords"
    "fixture/ListRecords.yaml"

testUnsubscribeFromDataset :: UnsubscribeFromDataset -> TestTree
testUnsubscribeFromDataset = req
    "UnsubscribeFromDataset"
    "fixture/UnsubscribeFromDataset.yaml"

testUpdateRecords :: UpdateRecords -> TestTree
testUpdateRecords = req
    "UpdateRecords"
    "fixture/UpdateRecords.yaml"

testListDatasets :: ListDatasets -> TestTree
testListDatasets = req
    "ListDatasets"
    "fixture/ListDatasets.yaml"

testBulkPublish :: BulkPublish -> TestTree
testBulkPublish = req
    "BulkPublish"
    "fixture/BulkPublish.yaml"

-- Responses

testDescribeDatasetResponse :: DescribeDatasetResponse -> TestTree
testDescribeDatasetResponse = res
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeDataset)

testSetCognitoEventsResponse :: SetCognitoEventsResponse -> TestTree
testSetCognitoEventsResponse = res
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse.proto"
    cognitoSync
    (Proxy :: Proxy SetCognitoEvents)

testDescribeIdentityPoolUsageResponse :: DescribeIdentityPoolUsageResponse -> TestTree
testDescribeIdentityPoolUsageResponse = res
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeIdentityPoolUsage)

testGetBulkPublishDetailsResponse :: GetBulkPublishDetailsResponse -> TestTree
testGetBulkPublishDetailsResponse = res
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetBulkPublishDetails)

testListIdentityPoolUsageResponse :: ListIdentityPoolUsageResponse -> TestTree
testListIdentityPoolUsageResponse = res
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListIdentityPoolUsage)

testSetIdentityPoolConfigurationResponse :: SetIdentityPoolConfigurationResponse -> TestTree
testSetIdentityPoolConfigurationResponse = res
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse.proto"
    cognitoSync
    (Proxy :: Proxy SetIdentityPoolConfiguration)

testDeleteDatasetResponse :: DeleteDatasetResponse -> TestTree
testDeleteDatasetResponse = res
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy DeleteDataset)

testGetCognitoEventsResponse :: GetCognitoEventsResponse -> TestTree
testGetCognitoEventsResponse = res
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetCognitoEvents)

testDescribeIdentityUsageResponse :: DescribeIdentityUsageResponse -> TestTree
testDescribeIdentityUsageResponse = res
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse.proto"
    cognitoSync
    (Proxy :: Proxy DescribeIdentityUsage)

testRegisterDeviceResponse :: RegisterDeviceResponse -> TestTree
testRegisterDeviceResponse = res
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse.proto"
    cognitoSync
    (Proxy :: Proxy RegisterDevice)

testSubscribeToDatasetResponse :: SubscribeToDatasetResponse -> TestTree
testSubscribeToDatasetResponse = res
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy SubscribeToDataset)

testGetIdentityPoolConfigurationResponse :: GetIdentityPoolConfigurationResponse -> TestTree
testGetIdentityPoolConfigurationResponse = res
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse.proto"
    cognitoSync
    (Proxy :: Proxy GetIdentityPoolConfiguration)

testListRecordsResponse :: ListRecordsResponse -> TestTree
testListRecordsResponse = res
    "ListRecordsResponse"
    "fixture/ListRecordsResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListRecords)

testUnsubscribeFromDatasetResponse :: UnsubscribeFromDatasetResponse -> TestTree
testUnsubscribeFromDatasetResponse = res
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse.proto"
    cognitoSync
    (Proxy :: Proxy UnsubscribeFromDataset)

testUpdateRecordsResponse :: UpdateRecordsResponse -> TestTree
testUpdateRecordsResponse = res
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse.proto"
    cognitoSync
    (Proxy :: Proxy UpdateRecords)

testListDatasetsResponse :: ListDatasetsResponse -> TestTree
testListDatasetsResponse = res
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse.proto"
    cognitoSync
    (Proxy :: Proxy ListDatasets)

testBulkPublishResponse :: BulkPublishResponse -> TestTree
testBulkPublishResponse = res
    "BulkPublishResponse"
    "fixture/BulkPublishResponse.proto"
    cognitoSync
    (Proxy :: Proxy BulkPublish)
