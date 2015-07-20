{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SNS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.SNS where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.SNS
import Test.AWS.SNS.Internal

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
testDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission"

testSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
testSetPlatformApplicationAttributes = req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes"

testCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
testCreatePlatformEndpoint = req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint"

testListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
testListSubscriptionsByTopic = req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic"

testGetTopicAttributes :: GetTopicAttributes -> TestTree
testGetTopicAttributes = req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes"

testDeleteTopic :: DeleteTopic -> TestTree
testDeleteTopic = req
    "DeleteTopic"
    "fixture/DeleteTopic"

testListTopics :: ListTopics -> TestTree
testListTopics = req
    "ListTopics"
    "fixture/ListTopics"

testCreatePlatformApplication :: CreatePlatformApplication -> TestTree
testCreatePlatformApplication = req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication"

testListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
testListEndpointsByPlatformApplication = req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication"

testGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
testGetPlatformApplicationAttributes = req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes"

testDeletePlatformApplication :: DeletePlatformApplication -> TestTree
testDeletePlatformApplication = req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication"

testListPlatformApplications :: ListPlatformApplications -> TestTree
testListPlatformApplications = req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications"

testSetTopicAttributes :: SetTopicAttributes -> TestTree
testSetTopicAttributes = req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes"

testGetEndpointAttributes :: GetEndpointAttributes -> TestTree
testGetEndpointAttributes = req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission"

testGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
testGetSubscriptionAttributes = req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes"

testListSubscriptions :: ListSubscriptions -> TestTree
testListSubscriptions = req
    "ListSubscriptions"
    "fixture/ListSubscriptions"

testCreateTopic :: CreateTopic -> TestTree
testCreateTopic = req
    "CreateTopic"
    "fixture/CreateTopic"

testSubscribe :: Subscribe -> TestTree
testSubscribe = req
    "Subscribe"
    "fixture/Subscribe"

testUnsubscribe :: Unsubscribe -> TestTree
testUnsubscribe = req
    "Unsubscribe"
    "fixture/Unsubscribe"

testSetEndpointAttributes :: SetEndpointAttributes -> TestTree
testSetEndpointAttributes = req
    "SetEndpointAttributes"
    "fixture/SetEndpointAttributes"

testSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
testSetSubscriptionAttributes = req
    "SetSubscriptionAttributes"
    "fixture/SetSubscriptionAttributes"

testConfirmSubscription :: ConfirmSubscription -> TestTree
testConfirmSubscription = req
    "ConfirmSubscription"
    "fixture/ConfirmSubscription"

testPublish :: Publish -> TestTree
testPublish = req
    "Publish"
    "fixture/Publish"

-- Responses

testDeleteEndpointResponse :: DeleteEndpointResponse -> TestTree
testDeleteEndpointResponse = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse"
    (Proxy :: Proxy DeleteEndpoint)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse"
    (Proxy :: Proxy RemovePermission)

testSetPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse -> TestTree
testSetPlatformApplicationAttributesResponse = res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy SetPlatformApplicationAttributes)

testCreatePlatformEndpointResponse :: CreatePlatformEndpointResponse -> TestTree
testCreatePlatformEndpointResponse = res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse"
    (Proxy :: Proxy CreatePlatformEndpoint)

testListSubscriptionsByTopicResponse :: ListSubscriptionsByTopicResponse -> TestTree
testListSubscriptionsByTopicResponse = res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse"
    (Proxy :: Proxy ListSubscriptionsByTopic)

testGetTopicAttributesResponse :: GetTopicAttributesResponse -> TestTree
testGetTopicAttributesResponse = res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse"
    (Proxy :: Proxy GetTopicAttributes)

testDeleteTopicResponse :: DeleteTopicResponse -> TestTree
testDeleteTopicResponse = res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse"
    (Proxy :: Proxy DeleteTopic)

testListTopicsResponse :: ListTopicsResponse -> TestTree
testListTopicsResponse = res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse"
    (Proxy :: Proxy ListTopics)

testCreatePlatformApplicationResponse :: CreatePlatformApplicationResponse -> TestTree
testCreatePlatformApplicationResponse = res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse"
    (Proxy :: Proxy CreatePlatformApplication)

testListEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse -> TestTree
testListEndpointsByPlatformApplicationResponse = res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse"
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

testGetPlatformApplicationAttributesResponse :: GetPlatformApplicationAttributesResponse -> TestTree
testGetPlatformApplicationAttributesResponse = res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse"
    (Proxy :: Proxy GetPlatformApplicationAttributes)

testDeletePlatformApplicationResponse :: DeletePlatformApplicationResponse -> TestTree
testDeletePlatformApplicationResponse = res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse"
    (Proxy :: Proxy DeletePlatformApplication)

testListPlatformApplicationsResponse :: ListPlatformApplicationsResponse -> TestTree
testListPlatformApplicationsResponse = res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse"
    (Proxy :: Proxy ListPlatformApplications)

testSetTopicAttributesResponse :: SetTopicAttributesResponse -> TestTree
testSetTopicAttributesResponse = res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse"
    (Proxy :: Proxy SetTopicAttributes)

testGetEndpointAttributesResponse :: GetEndpointAttributesResponse -> TestTree
testGetEndpointAttributesResponse = res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse"
    (Proxy :: Proxy GetEndpointAttributes)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse"
    (Proxy :: Proxy AddPermission)

testGetSubscriptionAttributesResponse :: GetSubscriptionAttributesResponse -> TestTree
testGetSubscriptionAttributesResponse = res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse"
    (Proxy :: Proxy GetSubscriptionAttributes)

testListSubscriptionsResponse :: ListSubscriptionsResponse -> TestTree
testListSubscriptionsResponse = res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse"
    (Proxy :: Proxy ListSubscriptions)

testCreateTopicResponse :: CreateTopicResponse -> TestTree
testCreateTopicResponse = res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse"
    (Proxy :: Proxy CreateTopic)

testSubscribeResponse :: SubscribeResponse -> TestTree
testSubscribeResponse = res
    "SubscribeResponse"
    "fixture/SubscribeResponse"
    (Proxy :: Proxy Subscribe)

testUnsubscribeResponse :: UnsubscribeResponse -> TestTree
testUnsubscribeResponse = res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse"
    (Proxy :: Proxy Unsubscribe)

testSetEndpointAttributesResponse :: SetEndpointAttributesResponse -> TestTree
testSetEndpointAttributesResponse = res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse"
    (Proxy :: Proxy SetEndpointAttributes)

testSetSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse -> TestTree
testSetSubscriptionAttributesResponse = res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse"
    (Proxy :: Proxy SetSubscriptionAttributes)

testConfirmSubscriptionResponse :: ConfirmSubscriptionResponse -> TestTree
testConfirmSubscriptionResponse = res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse"
    (Proxy :: Proxy ConfirmSubscription)

testPublishResponse :: PublishResponse -> TestTree
testPublishResponse = res
    "PublishResponse"
    "fixture/PublishResponse"
    (Proxy :: Proxy Publish)
