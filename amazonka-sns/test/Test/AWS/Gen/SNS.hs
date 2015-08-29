{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SNS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
    "fixture/DeleteEndpoint.yaml"

testRemovePermission :: RemovePermission -> TestTree
testRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

testSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
testSetPlatformApplicationAttributes = req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

testCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
testCreatePlatformEndpoint = req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

testListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
testListSubscriptionsByTopic = req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

testGetTopicAttributes :: GetTopicAttributes -> TestTree
testGetTopicAttributes = req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

testDeleteTopic :: DeleteTopic -> TestTree
testDeleteTopic = req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

testListTopics :: ListTopics -> TestTree
testListTopics = req
    "ListTopics"
    "fixture/ListTopics.yaml"

testCreatePlatformApplication :: CreatePlatformApplication -> TestTree
testCreatePlatformApplication = req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

testListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
testListEndpointsByPlatformApplication = req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

testGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
testGetPlatformApplicationAttributes = req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes.yaml"

testDeletePlatformApplication :: DeletePlatformApplication -> TestTree
testDeletePlatformApplication = req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

testListPlatformApplications :: ListPlatformApplications -> TestTree
testListPlatformApplications = req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

testSetTopicAttributes :: SetTopicAttributes -> TestTree
testSetTopicAttributes = req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

testGetEndpointAttributes :: GetEndpointAttributes -> TestTree
testGetEndpointAttributes = req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes.yaml"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

testGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
testGetSubscriptionAttributes = req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes.yaml"

testListSubscriptions :: ListSubscriptions -> TestTree
testListSubscriptions = req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

testCreateTopic :: CreateTopic -> TestTree
testCreateTopic = req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

testSubscribe :: Subscribe -> TestTree
testSubscribe = req
    "Subscribe"
    "fixture/Subscribe.yaml"

testUnsubscribe :: Unsubscribe -> TestTree
testUnsubscribe = req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

testSetEndpointAttributes :: SetEndpointAttributes -> TestTree
testSetEndpointAttributes = req
    "SetEndpointAttributes"
    "fixture/SetEndpointAttributes.yaml"

testSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
testSetSubscriptionAttributes = req
    "SetSubscriptionAttributes"
    "fixture/SetSubscriptionAttributes.yaml"

testConfirmSubscription :: ConfirmSubscription -> TestTree
testConfirmSubscription = req
    "ConfirmSubscription"
    "fixture/ConfirmSubscription.yaml"

testPublish :: Publish -> TestTree
testPublish = req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

testDeleteEndpointResponse :: DeleteEndpointResponse -> TestTree
testDeleteEndpointResponse = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    sNS
    (Proxy :: Proxy DeleteEndpoint)

testRemovePermissionResponse :: RemovePermissionResponse -> TestTree
testRemovePermissionResponse = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    sNS
    (Proxy :: Proxy RemovePermission)

testSetPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse -> TestTree
testSetPlatformApplicationAttributesResponse = res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetPlatformApplicationAttributes)

testCreatePlatformEndpointResponse :: CreatePlatformEndpointResponse -> TestTree
testCreatePlatformEndpointResponse = res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    sNS
    (Proxy :: Proxy CreatePlatformEndpoint)

testListSubscriptionsByTopicResponse :: ListSubscriptionsByTopicResponse -> TestTree
testListSubscriptionsByTopicResponse = res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    sNS
    (Proxy :: Proxy ListSubscriptionsByTopic)

testGetTopicAttributesResponse :: GetTopicAttributesResponse -> TestTree
testGetTopicAttributesResponse = res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetTopicAttributes)

testDeleteTopicResponse :: DeleteTopicResponse -> TestTree
testDeleteTopicResponse = res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    sNS
    (Proxy :: Proxy DeleteTopic)

testListTopicsResponse :: ListTopicsResponse -> TestTree
testListTopicsResponse = res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    sNS
    (Proxy :: Proxy ListTopics)

testCreatePlatformApplicationResponse :: CreatePlatformApplicationResponse -> TestTree
testCreatePlatformApplicationResponse = res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    sNS
    (Proxy :: Proxy CreatePlatformApplication)

testListEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse -> TestTree
testListEndpointsByPlatformApplicationResponse = res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    sNS
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

testGetPlatformApplicationAttributesResponse :: GetPlatformApplicationAttributesResponse -> TestTree
testGetPlatformApplicationAttributesResponse = res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetPlatformApplicationAttributes)

testDeletePlatformApplicationResponse :: DeletePlatformApplicationResponse -> TestTree
testDeletePlatformApplicationResponse = res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    sNS
    (Proxy :: Proxy DeletePlatformApplication)

testListPlatformApplicationsResponse :: ListPlatformApplicationsResponse -> TestTree
testListPlatformApplicationsResponse = res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    sNS
    (Proxy :: Proxy ListPlatformApplications)

testSetTopicAttributesResponse :: SetTopicAttributesResponse -> TestTree
testSetTopicAttributesResponse = res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetTopicAttributes)

testGetEndpointAttributesResponse :: GetEndpointAttributesResponse -> TestTree
testGetEndpointAttributesResponse = res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetEndpointAttributes)

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    sNS
    (Proxy :: Proxy AddPermission)

testGetSubscriptionAttributesResponse :: GetSubscriptionAttributesResponse -> TestTree
testGetSubscriptionAttributesResponse = res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetSubscriptionAttributes)

testListSubscriptionsResponse :: ListSubscriptionsResponse -> TestTree
testListSubscriptionsResponse = res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    sNS
    (Proxy :: Proxy ListSubscriptions)

testCreateTopicResponse :: CreateTopicResponse -> TestTree
testCreateTopicResponse = res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    sNS
    (Proxy :: Proxy CreateTopic)

testSubscribeResponse :: SubscribeResponse -> TestTree
testSubscribeResponse = res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    sNS
    (Proxy :: Proxy Subscribe)

testUnsubscribeResponse :: UnsubscribeResponse -> TestTree
testUnsubscribeResponse = res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    sNS
    (Proxy :: Proxy Unsubscribe)

testSetEndpointAttributesResponse :: SetEndpointAttributesResponse -> TestTree
testSetEndpointAttributesResponse = res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetEndpointAttributes)

testSetSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse -> TestTree
testSetSubscriptionAttributesResponse = res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetSubscriptionAttributes)

testConfirmSubscriptionResponse :: ConfirmSubscriptionResponse -> TestTree
testConfirmSubscriptionResponse = res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    sNS
    (Proxy :: Proxy ConfirmSubscription)

testPublishResponse :: PublishResponse -> TestTree
testPublishResponse = res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    sNS
    (Proxy :: Proxy Publish)
