-- Module      : Test.AWS.Gen.SNS
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

module Test.AWS.Gen.SNS where

import           Data.Proxy
import           Network.AWS.SNS
import           Test.AWS.Fixture
import           Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ deleteEndpointTest $
--             deleteEndpoint
--
--         , removePermissionTest $
--             removePermission
--
--         , setPlatformApplicationAttributesTest $
--             setPlatformApplicationAttributes
--
--         , createPlatformEndpointTest $
--             createPlatformEndpoint
--
--         , listSubscriptionsByTopicTest $
--             listSubscriptionsByTopic
--
--         , getTopicAttributesTest $
--             getTopicAttributes
--
--         , deleteTopicTest $
--             deleteTopic
--
--         , listTopicsTest $
--             listTopics
--
--         , createPlatformApplicationTest $
--             createPlatformApplication
--
--         , listEndpointsByPlatformApplicationTest $
--             listEndpointsByPlatformApplication
--
--         , getPlatformApplicationAttributesTest $
--             getPlatformApplicationAttributes
--
--         , deletePlatformApplicationTest $
--             deletePlatformApplication
--
--         , listPlatformApplicationsTest $
--             listPlatformApplications
--
--         , setTopicAttributesTest $
--             setTopicAttributes
--
--         , getEndpointAttributesTest $
--             getEndpointAttributes
--
--         , addPermissionTest $
--             addPermission
--
--         , getSubscriptionAttributesTest $
--             getSubscriptionAttributes
--
--         , listSubscriptionsTest $
--             listSubscriptions
--
--         , createTopicTest $
--             createTopic
--
--         , subscribeTest $
--             subscribe
--
--         , unsubscribeTest $
--             unsubscribe
--
--         , setEndpointAttributesTest $
--             setEndpointAttributes
--
--         , setSubscriptionAttributesTest $
--             setSubscriptionAttributes
--
--         , confirmSubscriptionTest $
--             confirmSubscription
--
--         , publishTest $
--             publish
--
--           ]

--     , testGroup "response"
--         [ deleteEndpointResponseTest $
--             deleteEndpointResponse
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , setPlatformApplicationAttributesResponseTest $
--             setPlatformApplicationAttributesResponse
--
--         , createPlatformEndpointResponseTest $
--             createPlatformEndpointResponse
--
--         , listSubscriptionsByTopicResponseTest $
--             listSubscriptionsByTopicResponse
--
--         , getTopicAttributesResponseTest $
--             getTopicAttributesResponse
--
--         , deleteTopicResponseTest $
--             deleteTopicResponse
--
--         , listTopicsResponseTest $
--             listTopicsResponse
--
--         , createPlatformApplicationResponseTest $
--             createPlatformApplicationResponse
--
--         , listEndpointsByPlatformApplicationResponseTest $
--             listEndpointsByPlatformApplicationResponse
--
--         , getPlatformApplicationAttributesResponseTest $
--             getPlatformApplicationAttributesResponse
--
--         , deletePlatformApplicationResponseTest $
--             deletePlatformApplicationResponse
--
--         , listPlatformApplicationsResponseTest $
--             listPlatformApplicationsResponse
--
--         , setTopicAttributesResponseTest $
--             setTopicAttributesResponse
--
--         , getEndpointAttributesResponseTest $
--             getEndpointAttributesResponse
--
--         , addPermissionResponseTest $
--             addPermissionResponse
--
--         , getSubscriptionAttributesResponseTest $
--             getSubscriptionAttributesResponse
--
--         , listSubscriptionsResponseTest $
--             listSubscriptionsResponse
--
--         , createTopicResponseTest $
--             createTopicResponse
--
--         , subscribeResponseTest $
--             subscribeResponse
--
--         , unsubscribeResponseTest $
--             unsubscribeResponse
--
--         , setEndpointAttributesResponseTest $
--             setEndpointAttributesResponse
--
--         , setSubscriptionAttributesResponseTest $
--             setSubscriptionAttributesResponse
--
--         , confirmSubscriptionResponseTest $
--             confirmSubscriptionResponse
--
--         , publishResponseTest $
--             publishResponse
--
--           ]
--     ]

-- Requests

deleteEndpointTest :: DeleteEndpoint -> TestTree
deleteEndpointTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

setPlatformApplicationAttributesTest :: SetPlatformApplicationAttributes -> TestTree
setPlatformApplicationAttributesTest = undefined

createPlatformEndpointTest :: CreatePlatformEndpoint -> TestTree
createPlatformEndpointTest = undefined

listSubscriptionsByTopicTest :: ListSubscriptionsByTopic -> TestTree
listSubscriptionsByTopicTest = undefined

getTopicAttributesTest :: GetTopicAttributes -> TestTree
getTopicAttributesTest = undefined

deleteTopicTest :: DeleteTopic -> TestTree
deleteTopicTest = undefined

listTopicsTest :: ListTopics -> TestTree
listTopicsTest = undefined

createPlatformApplicationTest :: CreatePlatformApplication -> TestTree
createPlatformApplicationTest = undefined

listEndpointsByPlatformApplicationTest :: ListEndpointsByPlatformApplication -> TestTree
listEndpointsByPlatformApplicationTest = undefined

getPlatformApplicationAttributesTest :: GetPlatformApplicationAttributes -> TestTree
getPlatformApplicationAttributesTest = undefined

deletePlatformApplicationTest :: DeletePlatformApplication -> TestTree
deletePlatformApplicationTest = undefined

listPlatformApplicationsTest :: ListPlatformApplications -> TestTree
listPlatformApplicationsTest = undefined

setTopicAttributesTest :: SetTopicAttributes -> TestTree
setTopicAttributesTest = undefined

getEndpointAttributesTest :: GetEndpointAttributes -> TestTree
getEndpointAttributesTest = undefined

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

getSubscriptionAttributesTest :: GetSubscriptionAttributes -> TestTree
getSubscriptionAttributesTest = undefined

listSubscriptionsTest :: ListSubscriptions -> TestTree
listSubscriptionsTest = undefined

createTopicTest :: CreateTopic -> TestTree
createTopicTest = undefined

subscribeTest :: Subscribe -> TestTree
subscribeTest = undefined

unsubscribeTest :: Unsubscribe -> TestTree
unsubscribeTest = undefined

setEndpointAttributesTest :: SetEndpointAttributes -> TestTree
setEndpointAttributesTest = undefined

setSubscriptionAttributesTest :: SetSubscriptionAttributes -> TestTree
setSubscriptionAttributesTest = undefined

confirmSubscriptionTest :: ConfirmSubscription -> TestTree
confirmSubscriptionTest = undefined

publishTest :: Publish -> TestTree
publishTest = undefined

-- Responses

deleteEndpointResponseTest :: DeleteEndpointResponse -> TestTree
deleteEndpointResponseTest = resp
    "DeleteEndpointResponse"
    "fixture/SNS/DeleteEndpointResponse"
    (Proxy :: Proxy DeleteEndpoint)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "RemovePermissionResponse"
    "fixture/SNS/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

setPlatformApplicationAttributesResponseTest :: SetPlatformApplicationAttributesResponse -> TestTree
setPlatformApplicationAttributesResponseTest = resp
    "SetPlatformApplicationAttributesResponse"
    "fixture/SNS/SetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy SetPlatformApplicationAttributes)

createPlatformEndpointResponseTest :: CreatePlatformEndpointResponse -> TestTree
createPlatformEndpointResponseTest = resp
    "CreatePlatformEndpointResponse"
    "fixture/SNS/CreatePlatformEndpointResponse"
    (Proxy :: Proxy CreatePlatformEndpoint)

listSubscriptionsByTopicResponseTest :: ListSubscriptionsByTopicResponse -> TestTree
listSubscriptionsByTopicResponseTest = resp
    "ListSubscriptionsByTopicResponse"
    "fixture/SNS/ListSubscriptionsByTopicResponse"
    (Proxy :: Proxy ListSubscriptionsByTopic)

getTopicAttributesResponseTest :: GetTopicAttributesResponse -> TestTree
getTopicAttributesResponseTest = resp
    "GetTopicAttributesResponse"
    "fixture/SNS/GetTopicAttributesResponse"
    (Proxy :: Proxy GetTopicAttributes)

deleteTopicResponseTest :: DeleteTopicResponse -> TestTree
deleteTopicResponseTest = resp
    "DeleteTopicResponse"
    "fixture/SNS/DeleteTopicResponse"
    (Proxy :: Proxy DeleteTopic)

listTopicsResponseTest :: ListTopicsResponse -> TestTree
listTopicsResponseTest = resp
    "ListTopicsResponse"
    "fixture/SNS/ListTopicsResponse"
    (Proxy :: Proxy ListTopics)

createPlatformApplicationResponseTest :: CreatePlatformApplicationResponse -> TestTree
createPlatformApplicationResponseTest = resp
    "CreatePlatformApplicationResponse"
    "fixture/SNS/CreatePlatformApplicationResponse"
    (Proxy :: Proxy CreatePlatformApplication)

listEndpointsByPlatformApplicationResponseTest :: ListEndpointsByPlatformApplicationResponse -> TestTree
listEndpointsByPlatformApplicationResponseTest = resp
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/SNS/ListEndpointsByPlatformApplicationResponse"
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

getPlatformApplicationAttributesResponseTest :: GetPlatformApplicationAttributesResponse -> TestTree
getPlatformApplicationAttributesResponseTest = resp
    "GetPlatformApplicationAttributesResponse"
    "fixture/SNS/GetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy GetPlatformApplicationAttributes)

deletePlatformApplicationResponseTest :: DeletePlatformApplicationResponse -> TestTree
deletePlatformApplicationResponseTest = resp
    "DeletePlatformApplicationResponse"
    "fixture/SNS/DeletePlatformApplicationResponse"
    (Proxy :: Proxy DeletePlatformApplication)

listPlatformApplicationsResponseTest :: ListPlatformApplicationsResponse -> TestTree
listPlatformApplicationsResponseTest = resp
    "ListPlatformApplicationsResponse"
    "fixture/SNS/ListPlatformApplicationsResponse"
    (Proxy :: Proxy ListPlatformApplications)

setTopicAttributesResponseTest :: SetTopicAttributesResponse -> TestTree
setTopicAttributesResponseTest = resp
    "SetTopicAttributesResponse"
    "fixture/SNS/SetTopicAttributesResponse"
    (Proxy :: Proxy SetTopicAttributes)

getEndpointAttributesResponseTest :: GetEndpointAttributesResponse -> TestTree
getEndpointAttributesResponseTest = resp
    "GetEndpointAttributesResponse"
    "fixture/SNS/GetEndpointAttributesResponse"
    (Proxy :: Proxy GetEndpointAttributes)

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "AddPermissionResponse"
    "fixture/SNS/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

getSubscriptionAttributesResponseTest :: GetSubscriptionAttributesResponse -> TestTree
getSubscriptionAttributesResponseTest = resp
    "GetSubscriptionAttributesResponse"
    "fixture/SNS/GetSubscriptionAttributesResponse"
    (Proxy :: Proxy GetSubscriptionAttributes)

listSubscriptionsResponseTest :: ListSubscriptionsResponse -> TestTree
listSubscriptionsResponseTest = resp
    "ListSubscriptionsResponse"
    "fixture/SNS/ListSubscriptionsResponse"
    (Proxy :: Proxy ListSubscriptions)

createTopicResponseTest :: CreateTopicResponse -> TestTree
createTopicResponseTest = resp
    "CreateTopicResponse"
    "fixture/SNS/CreateTopicResponse"
    (Proxy :: Proxy CreateTopic)

subscribeResponseTest :: SubscribeResponse -> TestTree
subscribeResponseTest = resp
    "SubscribeResponse"
    "fixture/SNS/SubscribeResponse"
    (Proxy :: Proxy Subscribe)

unsubscribeResponseTest :: UnsubscribeResponse -> TestTree
unsubscribeResponseTest = resp
    "UnsubscribeResponse"
    "fixture/SNS/UnsubscribeResponse"
    (Proxy :: Proxy Unsubscribe)

setEndpointAttributesResponseTest :: SetEndpointAttributesResponse -> TestTree
setEndpointAttributesResponseTest = resp
    "SetEndpointAttributesResponse"
    "fixture/SNS/SetEndpointAttributesResponse"
    (Proxy :: Proxy SetEndpointAttributes)

setSubscriptionAttributesResponseTest :: SetSubscriptionAttributesResponse -> TestTree
setSubscriptionAttributesResponseTest = resp
    "SetSubscriptionAttributesResponse"
    "fixture/SNS/SetSubscriptionAttributesResponse"
    (Proxy :: Proxy SetSubscriptionAttributes)

confirmSubscriptionResponseTest :: ConfirmSubscriptionResponse -> TestTree
confirmSubscriptionResponseTest = resp
    "ConfirmSubscriptionResponse"
    "fixture/SNS/ConfirmSubscriptionResponse"
    (Proxy :: Proxy ConfirmSubscription)

publishResponseTest :: PublishResponse -> TestTree
publishResponseTest = resp
    "PublishResponse"
    "fixture/SNS/PublishResponse"
    (Proxy :: Proxy Publish)
