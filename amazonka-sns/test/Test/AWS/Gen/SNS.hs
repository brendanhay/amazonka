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

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.SNS

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures = testGroup "SQS"
--     [ testGroup "request"
--         [ addPermissionTest $
--             addPermission
--
--         , confirmSubscriptionTest $
--             confirmSubscription
--
--         , createPlatformApplicationTest $
--             createPlatformApplication
--
--         , createPlatformEndpointTest $
--             createPlatformEndpoint
--
--         , createTopicTest $
--             createTopic
--
--         , deleteEndpointTest $
--             deleteEndpoint
--
--         , deletePlatformApplicationTest $
--             deletePlatformApplication
--
--         , deleteTopicTest $
--             deleteTopic
--
--         , getEndpointAttributesTest $
--             getEndpointAttributes
--
--         , getPlatformApplicationAttributesTest $
--             getPlatformApplicationAttributes
--
--         , getSubscriptionAttributesTest $
--             getSubscriptionAttributes
--
--         , getTopicAttributesTest $
--             getTopicAttributes
--
--         , listEndpointsByPlatformApplicationTest $
--             listEndpointsByPlatformApplication
--
--         , listPlatformApplicationsTest $
--             listPlatformApplications
--
--         , listSubscriptionsTest $
--             listSubscriptions
--
--         , listSubscriptionsByTopicTest $
--             listSubscriptionsByTopic
--
--         , listTopicsTest $
--             listTopics
--
--         , publishTest $
--             publish
--
--         , removePermissionTest $
--             removePermission
--
--         , setEndpointAttributesTest $
--             setEndpointAttributes
--
--         , setPlatformApplicationAttributesTest $
--             setPlatformApplicationAttributes
--
--         , setSubscriptionAttributesTest $
--             setSubscriptionAttributes
--
--         , setTopicAttributesTest $
--             setTopicAttributes
--
--         , subscribeTest $
--             subscribe
--
--         , unsubscribeTest $
--             unsubscribe
--
--           ]

--     , testGroup "response"
--         [ addPermissionResponseTest $
--             addPermissionResponse
--
--         , confirmSubscriptionResponseTest $
--             confirmSubscriptionResponse
--
--         , createPlatformApplicationResponseTest $
--             createPlatformApplicationResponse
--
--         , createPlatformEndpointResponseTest $
--             createPlatformEndpointResponse
--
--         , createTopicResponseTest $
--             createTopicResponse
--
--         , deleteEndpointResponseTest $
--             deleteEndpointResponse
--
--         , deletePlatformApplicationResponseTest $
--             deletePlatformApplicationResponse
--
--         , deleteTopicResponseTest $
--             deleteTopicResponse
--
--         , getEndpointAttributesResponseTest $
--             getEndpointAttributesResponse
--
--         , getPlatformApplicationAttributesResponseTest $
--             getPlatformApplicationAttributesResponse
--
--         , getSubscriptionAttributesResponseTest $
--             getSubscriptionAttributesResponse
--
--         , getTopicAttributesResponseTest $
--             getTopicAttributesResponse
--
--         , listEndpointsByPlatformApplicationResponseTest $
--             listEndpointsByPlatformApplicationResponse
--
--         , listPlatformApplicationsResponseTest $
--             listPlatformApplicationsResponse
--
--         , listSubscriptionsResponseTest $
--             listSubscriptionsResponse
--
--         , listSubscriptionsByTopicResponseTest $
--             listSubscriptionsByTopicResponse
--
--         , listTopicsResponseTest $
--             listTopicsResponse
--
--         , publishResponseTest $
--             publishResponse
--
--         , removePermissionResponseTest $
--             removePermissionResponse
--
--         , setEndpointAttributesResponseTest $
--             setEndpointAttributesResponse
--
--         , setPlatformApplicationAttributesResponseTest $
--             setPlatformApplicationAttributesResponse
--
--         , setSubscriptionAttributesResponseTest $
--             setSubscriptionAttributesResponse
--
--         , setTopicAttributesResponseTest $
--             setTopicAttributesResponse
--
--         , subscribeResponseTest $
--             subscribeResponse
--
--         , unsubscribeResponseTest $
--             unsubscribeResponse
--
--           ]
--     ]

-- Requests

addPermissionTest :: AddPermission -> TestTree
addPermissionTest = undefined

confirmSubscriptionTest :: ConfirmSubscription -> TestTree
confirmSubscriptionTest = undefined

createPlatformApplicationTest :: CreatePlatformApplication -> TestTree
createPlatformApplicationTest = undefined

createPlatformEndpointTest :: CreatePlatformEndpoint -> TestTree
createPlatformEndpointTest = undefined

createTopicTest :: CreateTopic -> TestTree
createTopicTest = undefined

deleteEndpointTest :: DeleteEndpoint -> TestTree
deleteEndpointTest = undefined

deletePlatformApplicationTest :: DeletePlatformApplication -> TestTree
deletePlatformApplicationTest = undefined

deleteTopicTest :: DeleteTopic -> TestTree
deleteTopicTest = undefined

getEndpointAttributesTest :: GetEndpointAttributes -> TestTree
getEndpointAttributesTest = undefined

getPlatformApplicationAttributesTest :: GetPlatformApplicationAttributes -> TestTree
getPlatformApplicationAttributesTest = undefined

getSubscriptionAttributesTest :: GetSubscriptionAttributes -> TestTree
getSubscriptionAttributesTest = undefined

getTopicAttributesTest :: GetTopicAttributes -> TestTree
getTopicAttributesTest = undefined

listEndpointsByPlatformApplicationTest :: ListEndpointsByPlatformApplication -> TestTree
listEndpointsByPlatformApplicationTest = undefined

listPlatformApplicationsTest :: ListPlatformApplications -> TestTree
listPlatformApplicationsTest = undefined

listSubscriptionsTest :: ListSubscriptions -> TestTree
listSubscriptionsTest = undefined

listSubscriptionsByTopicTest :: ListSubscriptionsByTopic -> TestTree
listSubscriptionsByTopicTest = undefined

listTopicsTest :: ListTopics -> TestTree
listTopicsTest = undefined

publishTest :: Publish -> TestTree
publishTest = undefined

removePermissionTest :: RemovePermission -> TestTree
removePermissionTest = undefined

setEndpointAttributesTest :: SetEndpointAttributes -> TestTree
setEndpointAttributesTest = undefined

setPlatformApplicationAttributesTest :: SetPlatformApplicationAttributes -> TestTree
setPlatformApplicationAttributesTest = undefined

setSubscriptionAttributesTest :: SetSubscriptionAttributes -> TestTree
setSubscriptionAttributesTest = undefined

setTopicAttributesTest :: SetTopicAttributes -> TestTree
setTopicAttributesTest = undefined

subscribeTest :: Subscribe -> TestTree
subscribeTest = undefined

unsubscribeTest :: Unsubscribe -> TestTree
unsubscribeTest = undefined

-- Responses

addPermissionResponseTest :: AddPermissionResponse -> TestTree
addPermissionResponseTest = resp
    "addPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

confirmSubscriptionResponseTest :: ConfirmSubscriptionResponse -> TestTree
confirmSubscriptionResponseTest = resp
    "confirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse"
    (Proxy :: Proxy ConfirmSubscription)

createPlatformApplicationResponseTest :: CreatePlatformApplicationResponse -> TestTree
createPlatformApplicationResponseTest = resp
    "createPlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse"
    (Proxy :: Proxy CreatePlatformApplication)

createPlatformEndpointResponseTest :: CreatePlatformEndpointResponse -> TestTree
createPlatformEndpointResponseTest = resp
    "createPlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse"
    (Proxy :: Proxy CreatePlatformEndpoint)

createTopicResponseTest :: CreateTopicResponse -> TestTree
createTopicResponseTest = resp
    "createTopicResponse"
    "fixture/CreateTopicResponse"
    (Proxy :: Proxy CreateTopic)

deleteEndpointResponseTest :: DeleteEndpointResponse -> TestTree
deleteEndpointResponseTest = resp
    "deleteEndpointResponse"
    "fixture/DeleteEndpointResponse"
    (Proxy :: Proxy DeleteEndpoint)

deletePlatformApplicationResponseTest :: DeletePlatformApplicationResponse -> TestTree
deletePlatformApplicationResponseTest = resp
    "deletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse"
    (Proxy :: Proxy DeletePlatformApplication)

deleteTopicResponseTest :: DeleteTopicResponse -> TestTree
deleteTopicResponseTest = resp
    "deleteTopicResponse"
    "fixture/DeleteTopicResponse"
    (Proxy :: Proxy DeleteTopic)

getEndpointAttributesResponseTest :: GetEndpointAttributesResponse -> TestTree
getEndpointAttributesResponseTest = resp
    "getEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse"
    (Proxy :: Proxy GetEndpointAttributes)

getPlatformApplicationAttributesResponseTest :: GetPlatformApplicationAttributesResponse -> TestTree
getPlatformApplicationAttributesResponseTest = resp
    "getPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy GetPlatformApplicationAttributes)

getSubscriptionAttributesResponseTest :: GetSubscriptionAttributesResponse -> TestTree
getSubscriptionAttributesResponseTest = resp
    "getSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse"
    (Proxy :: Proxy GetSubscriptionAttributes)

getTopicAttributesResponseTest :: GetTopicAttributesResponse -> TestTree
getTopicAttributesResponseTest = resp
    "getTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse"
    (Proxy :: Proxy GetTopicAttributes)

listEndpointsByPlatformApplicationResponseTest :: ListEndpointsByPlatformApplicationResponse -> TestTree
listEndpointsByPlatformApplicationResponseTest = resp
    "listEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse"
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

listPlatformApplicationsResponseTest :: ListPlatformApplicationsResponse -> TestTree
listPlatformApplicationsResponseTest = resp
    "listPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse"
    (Proxy :: Proxy ListPlatformApplications)

listSubscriptionsResponseTest :: ListSubscriptionsResponse -> TestTree
listSubscriptionsResponseTest = resp
    "listSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse"
    (Proxy :: Proxy ListSubscriptions)

listSubscriptionsByTopicResponseTest :: ListSubscriptionsByTopicResponse -> TestTree
listSubscriptionsByTopicResponseTest = resp
    "listSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse"
    (Proxy :: Proxy ListSubscriptionsByTopic)

listTopicsResponseTest :: ListTopicsResponse -> TestTree
listTopicsResponseTest = resp
    "listTopicsResponse"
    "fixture/ListTopicsResponse"
    (Proxy :: Proxy ListTopics)

publishResponseTest :: PublishResponse -> TestTree
publishResponseTest = resp
    "publishResponse"
    "fixture/PublishResponse"
    (Proxy :: Proxy Publish)

removePermissionResponseTest :: RemovePermissionResponse -> TestTree
removePermissionResponseTest = resp
    "removePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

setEndpointAttributesResponseTest :: SetEndpointAttributesResponse -> TestTree
setEndpointAttributesResponseTest = resp
    "setEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse"
    (Proxy :: Proxy SetEndpointAttributes)

setPlatformApplicationAttributesResponseTest :: SetPlatformApplicationAttributesResponse -> TestTree
setPlatformApplicationAttributesResponseTest = resp
    "setPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy SetPlatformApplicationAttributes)

setSubscriptionAttributesResponseTest :: SetSubscriptionAttributesResponse -> TestTree
setSubscriptionAttributesResponseTest = resp
    "setSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse"
    (Proxy :: Proxy SetSubscriptionAttributes)

setTopicAttributesResponseTest :: SetTopicAttributesResponse -> TestTree
setTopicAttributesResponseTest = resp
    "setTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse"
    (Proxy :: Proxy SetTopicAttributes)

subscribeResponseTest :: SubscribeResponse -> TestTree
subscribeResponseTest = resp
    "subscribeResponse"
    "fixture/SubscribeResponse"
    (Proxy :: Proxy Subscribe)

unsubscribeResponseTest :: UnsubscribeResponse -> TestTree
unsubscribeResponseTest = resp
    "unsubscribeResponse"
    "fixture/UnsubscribeResponse"
    (Proxy :: Proxy Unsubscribe)
