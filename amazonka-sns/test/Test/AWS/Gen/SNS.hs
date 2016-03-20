{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SNS
-- Copyright   : (c) 2013-2016 Brendan Hay
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
--         , testDeleteTopic $
--             deleteTopic
--
--         , testListTopics $
--             listTopics
--
--         , testCreatePlatformEndpoint $
--             createPlatformEndpoint
--
--         , testSetPlatformApplicationAttributes $
--             setPlatformApplicationAttributes
--
--         , testListSubscriptionsByTopic $
--             listSubscriptionsByTopic
--
--         , testGetTopicAttributes $
--             getTopicAttributes
--
--         , testCreatePlatformApplication $
--             createPlatformApplication
--
--         , testGetPlatformApplicationAttributes $
--             getPlatformApplicationAttributes
--
--         , testListEndpointsByPlatformApplication $
--             listEndpointsByPlatformApplication
--
--         , testSetTopicAttributes $
--             setTopicAttributes
--
--         , testDeletePlatformApplication $
--             deletePlatformApplication
--
--         , testListPlatformApplications $
--             listPlatformApplications
--
--         , testAddPermission $
--             addPermission
--
--         , testGetEndpointAttributes $
--             getEndpointAttributes
--
--         , testListSubscriptions $
--             listSubscriptions
--
--         , testGetSubscriptionAttributes $
--             getSubscriptionAttributes
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
--         , testDeleteTopicResponse $
--             deleteTopicResponse
--
--         , testListTopicsResponse $
--             listTopicsResponse
--
--         , testCreatePlatformEndpointResponse $
--             createPlatformEndpointResponse
--
--         , testSetPlatformApplicationAttributesResponse $
--             setPlatformApplicationAttributesResponse
--
--         , testListSubscriptionsByTopicResponse $
--             listSubscriptionsByTopicResponse
--
--         , testGetTopicAttributesResponse $
--             getTopicAttributesResponse
--
--         , testCreatePlatformApplicationResponse $
--             createPlatformApplicationResponse
--
--         , testGetPlatformApplicationAttributesResponse $
--             getPlatformApplicationAttributesResponse
--
--         , testListEndpointsByPlatformApplicationResponse $
--             listEndpointsByPlatformApplicationResponse
--
--         , testSetTopicAttributesResponse $
--             setTopicAttributesResponse
--
--         , testDeletePlatformApplicationResponse $
--             deletePlatformApplicationResponse
--
--         , testListPlatformApplicationsResponse $
--             listPlatformApplicationsResponse
--
--         , testAddPermissionResponse $
--             addPermissionResponse
--
--         , testGetEndpointAttributesResponse $
--             getEndpointAttributesResponse
--
--         , testListSubscriptionsResponse $
--             listSubscriptionsResponse
--
--         , testGetSubscriptionAttributesResponse $
--             getSubscriptionAttributesResponse
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

testDeleteTopic :: DeleteTopic -> TestTree
testDeleteTopic = req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

testListTopics :: ListTopics -> TestTree
testListTopics = req
    "ListTopics"
    "fixture/ListTopics.yaml"

testCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
testCreatePlatformEndpoint = req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

testSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
testSetPlatformApplicationAttributes = req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

testListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
testListSubscriptionsByTopic = req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

testGetTopicAttributes :: GetTopicAttributes -> TestTree
testGetTopicAttributes = req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

testCreatePlatformApplication :: CreatePlatformApplication -> TestTree
testCreatePlatformApplication = req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

testGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
testGetPlatformApplicationAttributes = req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes.yaml"

testListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
testListEndpointsByPlatformApplication = req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

testSetTopicAttributes :: SetTopicAttributes -> TestTree
testSetTopicAttributes = req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

testDeletePlatformApplication :: DeletePlatformApplication -> TestTree
testDeletePlatformApplication = req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

testListPlatformApplications :: ListPlatformApplications -> TestTree
testListPlatformApplications = req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

testAddPermission :: AddPermission -> TestTree
testAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

testGetEndpointAttributes :: GetEndpointAttributes -> TestTree
testGetEndpointAttributes = req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes.yaml"

testListSubscriptions :: ListSubscriptions -> TestTree
testListSubscriptions = req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

testGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
testGetSubscriptionAttributes = req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes.yaml"

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

testCreatePlatformEndpointResponse :: CreatePlatformEndpointResponse -> TestTree
testCreatePlatformEndpointResponse = res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    sNS
    (Proxy :: Proxy CreatePlatformEndpoint)

testSetPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse -> TestTree
testSetPlatformApplicationAttributesResponse = res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetPlatformApplicationAttributes)

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

testCreatePlatformApplicationResponse :: CreatePlatformApplicationResponse -> TestTree
testCreatePlatformApplicationResponse = res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    sNS
    (Proxy :: Proxy CreatePlatformApplication)

testGetPlatformApplicationAttributesResponse :: GetPlatformApplicationAttributesResponse -> TestTree
testGetPlatformApplicationAttributesResponse = res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetPlatformApplicationAttributes)

testListEndpointsByPlatformApplicationResponse :: ListEndpointsByPlatformApplicationResponse -> TestTree
testListEndpointsByPlatformApplicationResponse = res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    sNS
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

testSetTopicAttributesResponse :: SetTopicAttributesResponse -> TestTree
testSetTopicAttributesResponse = res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    sNS
    (Proxy :: Proxy SetTopicAttributes)

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

testAddPermissionResponse :: AddPermissionResponse -> TestTree
testAddPermissionResponse = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    sNS
    (Proxy :: Proxy AddPermission)

testGetEndpointAttributesResponse :: GetEndpointAttributesResponse -> TestTree
testGetEndpointAttributesResponse = res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetEndpointAttributes)

testListSubscriptionsResponse :: ListSubscriptionsResponse -> TestTree
testListSubscriptionsResponse = res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    sNS
    (Proxy :: Proxy ListSubscriptions)

testGetSubscriptionAttributesResponse :: GetSubscriptionAttributesResponse -> TestTree
testGetSubscriptionAttributesResponse = res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    sNS
    (Proxy :: Proxy GetSubscriptionAttributes)

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
