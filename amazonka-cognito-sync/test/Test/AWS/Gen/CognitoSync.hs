-- Module      : Test.AWS.Gen.CognitoSync
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ describeDatasetTest $
--             describeDataset
--
--         , describeIdentityPoolUsageTest $
--             describeIdentityPoolUsage
--
--         , setCognitoEventsTest $
--             setCognitoEvents
--
--         , listIdentityPoolUsageTest $
--             listIdentityPoolUsage
--
--         , getBulkPublishDetailsTest $
--             getBulkPublishDetails
--
--         , setIdentityPoolConfigurationTest $
--             setIdentityPoolConfiguration
--
--         , deleteDatasetTest $
--             deleteDataset
--
--         , describeIdentityUsageTest $
--             describeIdentityUsage
--
--         , getCognitoEventsTest $
--             getCognitoEvents
--
--         , registerDeviceTest $
--             registerDevice
--
--         , subscribeToDatasetTest $
--             subscribeToDataset
--
--         , getIdentityPoolConfigurationTest $
--             getIdentityPoolConfiguration
--
--         , listRecordsTest $
--             listRecords
--
--         , unsubscribeFromDatasetTest $
--             unsubscribeFromDataset
--
--         , updateRecordsTest $
--             updateRecords
--
--         , listDatasetsTest $
--             listDatasets
--
--         , bulkPublishTest $
--             bulkPublish
--
--           ]

--     , testGroup "response"
--         [ describeDatasetResponseTest $
--             describeDatasetResponse
--
--         , describeIdentityPoolUsageResponseTest $
--             describeIdentityPoolUsageResponse
--
--         , setCognitoEventsResponseTest $
--             setCognitoEventsResponse
--
--         , listIdentityPoolUsageResponseTest $
--             listIdentityPoolUsageResponse
--
--         , getBulkPublishDetailsResponseTest $
--             getBulkPublishDetailsResponse
--
--         , setIdentityPoolConfigurationResponseTest $
--             setIdentityPoolConfigurationResponse
--
--         , deleteDatasetResponseTest $
--             deleteDatasetResponse
--
--         , describeIdentityUsageResponseTest $
--             describeIdentityUsageResponse
--
--         , getCognitoEventsResponseTest $
--             getCognitoEventsResponse
--
--         , registerDeviceResponseTest $
--             registerDeviceResponse
--
--         , subscribeToDatasetResponseTest $
--             subscribeToDatasetResponse
--
--         , getIdentityPoolConfigurationResponseTest $
--             getIdentityPoolConfigurationResponse
--
--         , listRecordsResponseTest $
--             listRecordsResponse
--
--         , unsubscribeFromDatasetResponseTest $
--             unsubscribeFromDatasetResponse
--
--         , updateRecordsResponseTest $
--             updateRecordsResponse
--
--         , listDatasetsResponseTest $
--             listDatasetsResponse
--
--         , bulkPublishResponseTest $
--             bulkPublishResponse
--
--           ]
--     ]

-- Requests

describeDatasetTest :: DescribeDataset -> TestTree
describeDatasetTest = undefined

describeIdentityPoolUsageTest :: DescribeIdentityPoolUsage -> TestTree
describeIdentityPoolUsageTest = undefined

setCognitoEventsTest :: SetCognitoEvents -> TestTree
setCognitoEventsTest = undefined

listIdentityPoolUsageTest :: ListIdentityPoolUsage -> TestTree
listIdentityPoolUsageTest = undefined

getBulkPublishDetailsTest :: GetBulkPublishDetails -> TestTree
getBulkPublishDetailsTest = undefined

setIdentityPoolConfigurationTest :: SetIdentityPoolConfiguration -> TestTree
setIdentityPoolConfigurationTest = undefined

deleteDatasetTest :: DeleteDataset -> TestTree
deleteDatasetTest = undefined

describeIdentityUsageTest :: DescribeIdentityUsage -> TestTree
describeIdentityUsageTest = undefined

getCognitoEventsTest :: GetCognitoEvents -> TestTree
getCognitoEventsTest = undefined

registerDeviceTest :: RegisterDevice -> TestTree
registerDeviceTest = undefined

subscribeToDatasetTest :: SubscribeToDataset -> TestTree
subscribeToDatasetTest = undefined

getIdentityPoolConfigurationTest :: GetIdentityPoolConfiguration -> TestTree
getIdentityPoolConfigurationTest = undefined

listRecordsTest :: ListRecords -> TestTree
listRecordsTest = undefined

unsubscribeFromDatasetTest :: UnsubscribeFromDataset -> TestTree
unsubscribeFromDatasetTest = undefined

updateRecordsTest :: UpdateRecords -> TestTree
updateRecordsTest = undefined

listDatasetsTest :: ListDatasets -> TestTree
listDatasetsTest = undefined

bulkPublishTest :: BulkPublish -> TestTree
bulkPublishTest = undefined

-- Responses

describeDatasetResponseTest :: DescribeDatasetResponse -> TestTree
describeDatasetResponseTest = resp
    "DescribeDatasetResponse"
    "fixture/DescribeDatasetResponse"
    (Proxy :: Proxy DescribeDataset)

describeIdentityPoolUsageResponseTest :: DescribeIdentityPoolUsageResponse -> TestTree
describeIdentityPoolUsageResponseTest = resp
    "DescribeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse"
    (Proxy :: Proxy DescribeIdentityPoolUsage)

setCognitoEventsResponseTest :: SetCognitoEventsResponse -> TestTree
setCognitoEventsResponseTest = resp
    "SetCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse"
    (Proxy :: Proxy SetCognitoEvents)

listIdentityPoolUsageResponseTest :: ListIdentityPoolUsageResponse -> TestTree
listIdentityPoolUsageResponseTest = resp
    "ListIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse"
    (Proxy :: Proxy ListIdentityPoolUsage)

getBulkPublishDetailsResponseTest :: GetBulkPublishDetailsResponse -> TestTree
getBulkPublishDetailsResponseTest = resp
    "GetBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse"
    (Proxy :: Proxy GetBulkPublishDetails)

setIdentityPoolConfigurationResponseTest :: SetIdentityPoolConfigurationResponse -> TestTree
setIdentityPoolConfigurationResponseTest = resp
    "SetIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy SetIdentityPoolConfiguration)

deleteDatasetResponseTest :: DeleteDatasetResponse -> TestTree
deleteDatasetResponseTest = resp
    "DeleteDatasetResponse"
    "fixture/DeleteDatasetResponse"
    (Proxy :: Proxy DeleteDataset)

describeIdentityUsageResponseTest :: DescribeIdentityUsageResponse -> TestTree
describeIdentityUsageResponseTest = resp
    "DescribeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse"
    (Proxy :: Proxy DescribeIdentityUsage)

getCognitoEventsResponseTest :: GetCognitoEventsResponse -> TestTree
getCognitoEventsResponseTest = resp
    "GetCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse"
    (Proxy :: Proxy GetCognitoEvents)

registerDeviceResponseTest :: RegisterDeviceResponse -> TestTree
registerDeviceResponseTest = resp
    "RegisterDeviceResponse"
    "fixture/RegisterDeviceResponse"
    (Proxy :: Proxy RegisterDevice)

subscribeToDatasetResponseTest :: SubscribeToDatasetResponse -> TestTree
subscribeToDatasetResponseTest = resp
    "SubscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse"
    (Proxy :: Proxy SubscribeToDataset)

getIdentityPoolConfigurationResponseTest :: GetIdentityPoolConfigurationResponse -> TestTree
getIdentityPoolConfigurationResponseTest = resp
    "GetIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy GetIdentityPoolConfiguration)

listRecordsResponseTest :: ListRecordsResponse -> TestTree
listRecordsResponseTest = resp
    "ListRecordsResponse"
    "fixture/ListRecordsResponse"
    (Proxy :: Proxy ListRecords)

unsubscribeFromDatasetResponseTest :: UnsubscribeFromDatasetResponse -> TestTree
unsubscribeFromDatasetResponseTest = resp
    "UnsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse"
    (Proxy :: Proxy UnsubscribeFromDataset)

updateRecordsResponseTest :: UpdateRecordsResponse -> TestTree
updateRecordsResponseTest = resp
    "UpdateRecordsResponse"
    "fixture/UpdateRecordsResponse"
    (Proxy :: Proxy UpdateRecords)

listDatasetsResponseTest :: ListDatasetsResponse -> TestTree
listDatasetsResponseTest = resp
    "ListDatasetsResponse"
    "fixture/ListDatasetsResponse"
    (Proxy :: Proxy ListDatasets)

bulkPublishResponseTest :: BulkPublishResponse -> TestTree
bulkPublishResponseTest = resp
    "BulkPublishResponse"
    "fixture/BulkPublishResponse"
    (Proxy :: Proxy BulkPublish)
