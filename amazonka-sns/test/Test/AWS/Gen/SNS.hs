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
-- fixtures =
--     [ testGroup "request"
--         [ testDeleteEndpoint $
--             deleteEndpoint
--
--         , testRemovePermission $
--             removePermission
--
--         , testSetPlatformApplicationAttributes $
--             setPlatformApplicationAttributes
--
--         , testCreatePlatformEndpoint $
--             createPlatformEndpoint
--
--         , testListSubscriptionsByTopic $
--             listSubscriptionsByTopic
--
--         , testGetTopicAttributes $
--             getTopicAttributes
--
--         , testDeleteTopic $
--             deleteTopic
--
--         , testListTopics $
--             listTopics
--
--         , testCreatePlatformApplication $
--             createPlatformApplication
--
--         , testListEndpointsByPlatformApplication $
--             listEndpointsByPlatformApplication
--
--         , testGetPlatformApplicationAttributes $
--             getPlatformApplicationAttributes
--
--         , testDeletePlatformApplication $
--             deletePlatformApplication
--
--         , testListPlatformApplications $
--             listPlatformApplications
--
--         , testSetTopicAttributes $
--             setTopicAttributes
--
--         , testGetEndpointAttributes $
--             getEndpointAttributes
--
--         , testAddPermission $
--             addPermission
--
--         , testGetSubscriptionAttributes $
--             getSubscriptionAttributes
--
--         , testListSubscriptions $
--             listSubscriptions
--
--         , testCreateTopic $
--             createTopic
--
--         , testSubscribe $
--             subscribe
--
--         , testUnsubscribe $
--             unsubscribe
--
--         , testSetEndpointAttributes $
--             setEndpointAttributes
--
--         , testSetSubscriptionAttributes $
--             setSubscriptionAttributes
--
--         , testConfirmSubscription $
--             confirmSubscription
--
--         , testPublish $
--             publish
--
--           ]

--     , testGroup "response"
--         [ testDeleteEndpointResponse $
--             deleteEndpointResponse
--
--         , testRemovePermissionResponse $
--             removePermissionResponse
--
--         , testSetPlatformApplicationAttributesResponse $
--             setPlatformApplicationAttributesResponse
--
--         , testCreatePlatformEndpointResponse $
--             createPlatformEndpointResponse
--
--         , testListSubscriptionsByTopicResponse $
--             listSubscriptionsByTopicResponse
--
--         , testGetTopicAttributesResponse $
--             getTopicAttributesResponse
--
--         , testDeleteTopicResponse $
--             deleteTopicResponse
--
--         , testListTopicsResponse $
--             listTopicsResponse
--
--         , testCreatePlatformApplicationResponse $
--             createPlatformApplicationResponse
--
--         , testListEndpointsByPlatformApplicationResponse $
--             listEndpointsByPlatformApplicationResponse
--
--         , testGetPlatformApplicationAttributesResponse $
--             getPlatformApplicationAttributesResponse
--
--         , testDeletePlatformApplicationResponse $
--             deletePlatformApplicationResponse
--
--         , testListPlatformApplicationsResponse $
--             listPlatformApplicationsResponse
--
--         , testSetTopicAttributesResponse $
--             setTopicAttributesResponse
--
--         , testGetEndpointAttributesResponse $
--             getEndpointAttributesResponse
--
--         , testAddPermissionResponse $
--             addPermissionResponse
--
--         , testGetSubscriptionAttributesResponse $
--             getSubscriptionAttributesResponse
--
--         , testListSubscriptionsResponse $
--             listSubscriptionsResponse
--
--         , testCreateTopicResponse $
--             createTopicResponse
--
--         , testSubscribeResponse $
--             subscribeResponse
--
--         , testUnsubscribeResponse $
--             unsubscribeResponse
--
--         , testSetEndpointAttributesResponse $
--             setEndpointAttributesResponse
--
--         , testSetSubscriptionAttributesResponse $
--             setSubscriptionAttributesResponse
--
--         , testConfirmSubscriptionResponse $
--             confirmSubscriptionResponse
--
--         , testPublishResponse $
--             publishResponse
--
--           ]
--     ]

-- Requests

testDeleteEndpoint :: DeleteEndpoint -> TestTree
testDeleteEndpoint = undefined

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = undefined

testSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
testSetPlatformApplicationAttributes = undefined

testCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
testCreatePlatformEndpoint = undefined

testListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
testListSubscriptionsByTopic = undefined

testGetTopicAttributes :: GetTopicAttributes -> TestTree
testGetTopicAttributes = undefined

testDeleteTopic :: DeleteTopic -> TestTree
testDeleteTopic = undefined

testListTopics :: ListTopics -> TestTree
testListTopics = undefined

testCreatePlatformApplication :: CreatePlatformApplication -> TestTree
testCreatePlatformApplication = undefined

testListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
testListEndpointsByPlatformApplication = undefined

testGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
testGetPlatformApplicationAttributes = undefined

testDeletePlatformApplication :: DeletePlatformApplication -> TestTree
testDeletePlatformApplication = undefined

testListPlatformApplications :: ListPlatformApplications -> TestTree
testListPlatformApplications = undefined

testSetTopicAttributes :: SetTopicAttributes -> TestTree
testSetTopicAttributes = undefined

testGetEndpointAttributes :: GetEndpointAttributes -> TestTree
testGetEndpointAttributes = undefined

testAddPermission :: AddPermission -> TestTree
testAddPermission = undefined

testGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
testGetSubscriptionAttributes = undefined

testListSubscriptions :: ListSubscriptions -> TestTree
testListSubscriptions = undefined

testCreateTopic :: CreateTopic -> TestTree
testCreateTopic = undefined

testSubscribe :: Subscribe -> TestTree
testSubscribe = undefined

testUnsubscribe :: Unsubscribe -> TestTree
testUnsubscribe = undefined

testSetEndpointAttributes :: SetEndpointAttributes -> TestTree
testSetEndpointAttributes = undefined

testSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
testSetSubscriptionAttributes = undefined

testConfirmSubscription :: ConfirmSubscription -> TestTree
testConfirmSubscription = undefined

testPublish :: Publish -> TestTree
testPublish = undefined

-- Responses

testDeleteEndpointResponse :: DeleteEndpointResponse -> TestTree
testDeleteEndpointResponse = resp
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse"
    (Proxy :: Proxy DeleteEndpoint)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = resp
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testSetPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse -> TestTree
testSetPlatformApplicationAttributesResponse = resp
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy SetPlatformApplicationAttributes)

testCreatePlatformEndpointResponse :: CreatePlatformEndpointResponse -> TestTree
testCreatePlatformEndpointResponse = resp
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse"
    (Proxy :: Proxy CreatePlatformEndpoint)

testListSubscriptionsByTopicResponse :: ListSubscriptionsByTopicResponse -> TestTree
testListSubscriptionsByTopicResponse = resp
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse"
    (Proxy :: Proxy ListSubscriptionsByTopic)

testGetTopicAttributesResponse :: GetTopicAttributesResponse -> TestTree
testGetTopicAttributesResponse = resp
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse"
    (Proxy :: Proxy GetTopicAttributes)

testDeleteTopicResponse :: DeleteTopicResponse -> TestTree
testDeleteTopicResponse = resp
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse"
    (Proxy :: Proxy DeleteTopic)

testListTopicsResponse :: ListTopicsResponse -> TestTree
testListTopicsResponse = resp
    "ListTopicsResponse"
    "fixture/ListTopicsResponse"
    (Proxy :: Proxy ListTopics)

testCreatePlatformApplicationResponse :: CreatePlatformApplicationResponse -> TestTree
testCreatePlatformApplicationResponse = resp
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse"
    (Proxy :: Proxy CreatePlatformApplication)

testListEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse -> TestTree
testListEndpointsByPlatformApplicationResponse = resp
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse"
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

testGetPlatformApplicationAttributesResponse :: GetPlatformApplicationAttributesResponse -> TestTree
testGetPlatformApplicationAttributesResponse = resp
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy GetPlatformApplicationAttributes)

testDeletePlatformApplicationResponse :: DeletePlatformApplicationResponse -> TestTree
testDeletePlatformApplicationResponse = resp
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse"
    (Proxy :: Proxy DeletePlatformApplication)

testListPlatformApplicationsResponse :: ListPlatformApplicationsResponse -> TestTree
testListPlatformApplicationsResponse = resp
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse"
    (Proxy :: Proxy ListPlatformApplications)

testSetTopicAttributesResponse :: SetTopicAttributesResponse -> TestTree
testSetTopicAttributesResponse = resp
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse"
    (Proxy :: Proxy SetTopicAttributes)

testGetEndpointAttributesResponse :: GetEndpointAttributesResponse -> TestTree
testGetEndpointAttributesResponse = resp
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse"
    (Proxy :: Proxy GetEndpointAttributes)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = resp
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testGetSubscriptionAttributesResponse :: GetSubscriptionAttributesResponse -> TestTree
testGetSubscriptionAttributesResponse = resp
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse"
    (Proxy :: Proxy GetSubscriptionAttributes)

testListSubscriptionsResponse :: ListSubscriptionsResponse -> TestTree
testListSubscriptionsResponse = resp
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse"
    (Proxy :: Proxy ListSubscriptions)

testCreateTopicResponse :: CreateTopicResponse -> TestTree
testCreateTopicResponse = resp
    "CreateTopicResponse"
    "fixture/CreateTopicResponse"
    (Proxy :: Proxy CreateTopic)

testSubscribeResponse :: SubscribeResponse -> TestTree
testSubscribeResponse = resp
    "SubscribeResponse"
    "fixture/SubscribeResponse"
    (Proxy :: Proxy Subscribe)

testUnsubscribeResponse :: UnsubscribeResponse -> TestTree
testUnsubscribeResponse = resp
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse"
    (Proxy :: Proxy Unsubscribe)

testSetEndpointAttributesResponse :: SetEndpointAttributesResponse -> TestTree
testSetEndpointAttributesResponse = resp
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse"
    (Proxy :: Proxy SetEndpointAttributes)

testSetSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse -> TestTree
testSetSubscriptionAttributesResponse = resp
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse"
    (Proxy :: Proxy SetSubscriptionAttributes)

testConfirmSubscriptionResponse :: ConfirmSubscriptionResponse -> TestTree
testConfirmSubscriptionResponse = resp
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse"
    (Proxy :: Proxy ConfirmSubscription)

testPublishResponse :: PublishResponse -> TestTree
testPublishResponse = resp
    "PublishResponse"
    "fixture/PublishResponse"
    (Proxy :: Proxy Publish)
