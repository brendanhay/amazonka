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
--         [ bulkPublishTest $
--             bulkPublish
--
--         , deleteDatasetTest $
--             deleteDataset
--
--         , describeDatasetTest $
--             describeDataset
--
--         , describeIdentityPoolUsageTest $
--             describeIdentityPoolUsage
--
--         , describeIdentityUsageTest $
--             describeIdentityUsage
--
--         , getBulkPublishDetailsTest $
--             getBulkPublishDetails
--
--         , getCognitoEventsTest $
--             getCognitoEvents
--
--         , getIdentityPoolConfigurationTest $
--             getIdentityPoolConfiguration
--
--         , listDatasetsTest $
--             listDatasets
--
--         , listIdentityPoolUsageTest $
--             listIdentityPoolUsage
--
--         , listRecordsTest $
--             listRecords
--
--         , registerDeviceTest $
--             registerDevice
--
--         , setCognitoEventsTest $
--             setCognitoEvents
--
--         , setIdentityPoolConfigurationTest $
--             setIdentityPoolConfiguration
--
--         , subscribeToDatasetTest $
--             subscribeToDataset
--
--         , unsubscribeFromDatasetTest $
--             unsubscribeFromDataset
--
--         , updateRecordsTest $
--             updateRecords
--
--           ]

--     , testGroup "response"
--         [ bulkPublishResponseTest $
--             bulkPublishResponse
--
--         , deleteDatasetResponseTest $
--             deleteDatasetResponse
--
--         , describeDatasetResponseTest $
--             describeDatasetResponse
--
--         , describeIdentityPoolUsageResponseTest $
--             describeIdentityPoolUsageResponse
--
--         , describeIdentityUsageResponseTest $
--             describeIdentityUsageResponse
--
--         , getBulkPublishDetailsResponseTest $
--             getBulkPublishDetailsResponse
--
--         , getCognitoEventsResponseTest $
--             getCognitoEventsResponse
--
--         , getIdentityPoolConfigurationResponseTest $
--             getIdentityPoolConfigurationResponse
--
--         , listDatasetsResponseTest $
--             listDatasetsResponse
--
--         , listIdentityPoolUsageResponseTest $
--             listIdentityPoolUsageResponse
--
--         , listRecordsResponseTest $
--             listRecordsResponse
--
--         , registerDeviceResponseTest $
--             registerDeviceResponse
--
--         , setCognitoEventsResponseTest $
--             setCognitoEventsResponse
--
--         , setIdentityPoolConfigurationResponseTest $
--             setIdentityPoolConfigurationResponse
--
--         , subscribeToDatasetResponseTest $
--             subscribeToDatasetResponse
--
--         , unsubscribeFromDatasetResponseTest $
--             unsubscribeFromDatasetResponse
--
--         , updateRecordsResponseTest $
--             updateRecordsResponse
--
--           ]
--     ]

-- Requests

bulkPublishTest :: BulkPublish -> TestTree
bulkPublishTest = undefined

deleteDatasetTest :: DeleteDataset -> TestTree
deleteDatasetTest = undefined

describeDatasetTest :: DescribeDataset -> TestTree
describeDatasetTest = undefined

describeIdentityPoolUsageTest :: DescribeIdentityPoolUsage -> TestTree
describeIdentityPoolUsageTest = undefined

describeIdentityUsageTest :: DescribeIdentityUsage -> TestTree
describeIdentityUsageTest = undefined

getBulkPublishDetailsTest :: GetBulkPublishDetails -> TestTree
getBulkPublishDetailsTest = undefined

getCognitoEventsTest :: GetCognitoEvents -> TestTree
getCognitoEventsTest = undefined

getIdentityPoolConfigurationTest :: GetIdentityPoolConfiguration -> TestTree
getIdentityPoolConfigurationTest = undefined

listDatasetsTest :: ListDatasets -> TestTree
listDatasetsTest = undefined

listIdentityPoolUsageTest :: ListIdentityPoolUsage -> TestTree
listIdentityPoolUsageTest = undefined

listRecordsTest :: ListRecords -> TestTree
listRecordsTest = undefined

registerDeviceTest :: RegisterDevice -> TestTree
registerDeviceTest = undefined

setCognitoEventsTest :: SetCognitoEvents -> TestTree
setCognitoEventsTest = undefined

setIdentityPoolConfigurationTest :: SetIdentityPoolConfiguration -> TestTree
setIdentityPoolConfigurationTest = undefined

subscribeToDatasetTest :: SubscribeToDataset -> TestTree
subscribeToDatasetTest = undefined

unsubscribeFromDatasetTest :: UnsubscribeFromDataset -> TestTree
unsubscribeFromDatasetTest = undefined

updateRecordsTest :: UpdateRecords -> TestTree
updateRecordsTest = undefined

-- Responses

bulkPublishResponseTest :: BulkPublishResponse -> TestTree
bulkPublishResponseTest = resp
    "bulkPublishResponse"
    "fixture/BulkPublishResponse"
    (Proxy :: Proxy BulkPublish)

deleteDatasetResponseTest :: DeleteDatasetResponse -> TestTree
deleteDatasetResponseTest = resp
    "deleteDatasetResponse"
    "fixture/DeleteDatasetResponse"
    (Proxy :: Proxy DeleteDataset)

describeDatasetResponseTest :: DescribeDatasetResponse -> TestTree
describeDatasetResponseTest = resp
    "describeDatasetResponse"
    "fixture/DescribeDatasetResponse"
    (Proxy :: Proxy DescribeDataset)

describeIdentityPoolUsageResponseTest :: DescribeIdentityPoolUsageResponse -> TestTree
describeIdentityPoolUsageResponseTest = resp
    "describeIdentityPoolUsageResponse"
    "fixture/DescribeIdentityPoolUsageResponse"
    (Proxy :: Proxy DescribeIdentityPoolUsage)

describeIdentityUsageResponseTest :: DescribeIdentityUsageResponse -> TestTree
describeIdentityUsageResponseTest = resp
    "describeIdentityUsageResponse"
    "fixture/DescribeIdentityUsageResponse"
    (Proxy :: Proxy DescribeIdentityUsage)

getBulkPublishDetailsResponseTest :: GetBulkPublishDetailsResponse -> TestTree
getBulkPublishDetailsResponseTest = resp
    "getBulkPublishDetailsResponse"
    "fixture/GetBulkPublishDetailsResponse"
    (Proxy :: Proxy GetBulkPublishDetails)

getCognitoEventsResponseTest :: GetCognitoEventsResponse -> TestTree
getCognitoEventsResponseTest = resp
    "getCognitoEventsResponse"
    "fixture/GetCognitoEventsResponse"
    (Proxy :: Proxy GetCognitoEvents)

getIdentityPoolConfigurationResponseTest :: GetIdentityPoolConfigurationResponse -> TestTree
getIdentityPoolConfigurationResponseTest = resp
    "getIdentityPoolConfigurationResponse"
    "fixture/GetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy GetIdentityPoolConfiguration)

listDatasetsResponseTest :: ListDatasetsResponse -> TestTree
listDatasetsResponseTest = resp
    "listDatasetsResponse"
    "fixture/ListDatasetsResponse"
    (Proxy :: Proxy ListDatasets)

listIdentityPoolUsageResponseTest :: ListIdentityPoolUsageResponse -> TestTree
listIdentityPoolUsageResponseTest = resp
    "listIdentityPoolUsageResponse"
    "fixture/ListIdentityPoolUsageResponse"
    (Proxy :: Proxy ListIdentityPoolUsage)

listRecordsResponseTest :: ListRecordsResponse -> TestTree
listRecordsResponseTest = resp
    "listRecordsResponse"
    "fixture/ListRecordsResponse"
    (Proxy :: Proxy ListRecords)

registerDeviceResponseTest :: RegisterDeviceResponse -> TestTree
registerDeviceResponseTest = resp
    "registerDeviceResponse"
    "fixture/RegisterDeviceResponse"
    (Proxy :: Proxy RegisterDevice)

setCognitoEventsResponseTest :: SetCognitoEventsResponse -> TestTree
setCognitoEventsResponseTest = resp
    "setCognitoEventsResponse"
    "fixture/SetCognitoEventsResponse"
    (Proxy :: Proxy SetCognitoEvents)

setIdentityPoolConfigurationResponseTest :: SetIdentityPoolConfigurationResponse -> TestTree
setIdentityPoolConfigurationResponseTest = resp
    "setIdentityPoolConfigurationResponse"
    "fixture/SetIdentityPoolConfigurationResponse"
    (Proxy :: Proxy SetIdentityPoolConfiguration)

subscribeToDatasetResponseTest :: SubscribeToDatasetResponse -> TestTree
subscribeToDatasetResponseTest = resp
    "subscribeToDatasetResponse"
    "fixture/SubscribeToDatasetResponse"
    (Proxy :: Proxy SubscribeToDataset)

unsubscribeFromDatasetResponseTest :: UnsubscribeFromDatasetResponse -> TestTree
unsubscribeFromDatasetResponseTest = resp
    "unsubscribeFromDatasetResponse"
    "fixture/UnsubscribeFromDatasetResponse"
    (Proxy :: Proxy UnsubscribeFromDataset)

updateRecordsResponseTest :: UpdateRecordsResponse -> TestTree
updateRecordsResponseTest = resp
    "updateRecordsResponse"
    "fixture/UpdateRecordsResponse"
    (Proxy :: Proxy UpdateRecords)
