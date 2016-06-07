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
--         [ requestDeleteEndpoint $
--             deleteEndpoint
--
--         , requestRemovePermission $
--             removePermission
--
--         , requestDeleteTopic $
--             deleteTopic
--
--         , requestListTopics $
--             listTopics
--
--         , requestCreatePlatformEndpoint $
--             createPlatformEndpoint
--
--         , requestSetPlatformApplicationAttributes $
--             setPlatformApplicationAttributes
--
--         , requestListSubscriptionsByTopic $
--             listSubscriptionsByTopic
--
--         , requestGetTopicAttributes $
--             getTopicAttributes
--
--         , requestCreatePlatformApplication $
--             createPlatformApplication
--
--         , requestGetPlatformApplicationAttributes $
--             getPlatformApplicationAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             listEndpointsByPlatformApplication
--
--         , requestSetTopicAttributes $
--             setTopicAttributes
--
--         , requestDeletePlatformApplication $
--             deletePlatformApplication
--
--         , requestListPlatformApplications $
--             listPlatformApplications
--
--         , requestAddPermission $
--             addPermission
--
--         , requestGetEndpointAttributes $
--             getEndpointAttributes
--
--         , requestListSubscriptions $
--             listSubscriptions
--
--         , requestGetSubscriptionAttributes $
--             getSubscriptionAttributes
--
--         , requestCreateTopic $
--             createTopic
--
--         , requestSubscribe $
--             subscribe
--
--         , requestUnsubscribe $
--             unsubscribe
--
--         , requestSetEndpointAttributes $
--             setEndpointAttributes
--
--         , requestSetSubscriptionAttributes $
--             setSubscriptionAttributes
--
--         , requestConfirmSubscription $
--             confirmSubscription
--
--         , requestPublish $
--             publish
--
--           ]

--     , testGroup "response"
--         [ responseDeleteEndpoint $
--             deleteEndpointResponse
--
--         , responseRemovePermission $
--             removePermissionResponse
--
--         , responseDeleteTopic $
--             deleteTopicResponse
--
--         , responseListTopics $
--             listTopicsResponse
--
--         , responseCreatePlatformEndpoint $
--             createPlatformEndpointResponse
--
--         , responseSetPlatformApplicationAttributes $
--             setPlatformApplicationAttributesResponse
--
--         , responseListSubscriptionsByTopic $
--             listSubscriptionsByTopicResponse
--
--         , responseGetTopicAttributes $
--             getTopicAttributesResponse
--
--         , responseCreatePlatformApplication $
--             createPlatformApplicationResponse
--
--         , responseGetPlatformApplicationAttributes $
--             getPlatformApplicationAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             listEndpointsByPlatformApplicationResponse
--
--         , responseSetTopicAttributes $
--             setTopicAttributesResponse
--
--         , responseDeletePlatformApplication $
--             deletePlatformApplicationResponse
--
--         , responseListPlatformApplications $
--             listPlatformApplicationsResponse
--
--         , responseAddPermission $
--             addPermissionResponse
--
--         , responseGetEndpointAttributes $
--             getEndpointAttributesResponse
--
--         , responseListSubscriptions $
--             listSubscriptionsResponse
--
--         , responseGetSubscriptionAttributes $
--             getSubscriptionAttributesResponse
--
--         , responseCreateTopic $
--             createTopicResponse
--
--         , responseSubscribe $
--             subscribeResponse
--
--         , responseUnsubscribe $
--             unsubscribeResponse
--
--         , responseSetEndpointAttributes $
--             setEndpointAttributesResponse
--
--         , responseSetSubscriptionAttributes $
--             setSubscriptionAttributesResponse
--
--         , responseConfirmSubscription $
--             confirmSubscriptionResponse
--
--         , responsePublish $
--             publishResponse
--
--           ]
--     ]

-- Requests

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint = req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission = req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestDeleteTopic :: DeleteTopic -> TestTree
requestDeleteTopic = req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics = req
    "ListTopics"
    "fixture/ListTopics.yaml"

requestCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
requestCreatePlatformEndpoint = req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

requestSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
requestSetPlatformApplicationAttributes = req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

requestListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
requestListSubscriptionsByTopic = req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

requestGetTopicAttributes :: GetTopicAttributes -> TestTree
requestGetTopicAttributes = req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

requestCreatePlatformApplication :: CreatePlatformApplication -> TestTree
requestCreatePlatformApplication = req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

requestGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
requestGetPlatformApplicationAttributes = req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes.yaml"

requestListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
requestListEndpointsByPlatformApplication = req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

requestSetTopicAttributes :: SetTopicAttributes -> TestTree
requestSetTopicAttributes = req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

requestDeletePlatformApplication :: DeletePlatformApplication -> TestTree
requestDeletePlatformApplication = req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

requestListPlatformApplications :: ListPlatformApplications -> TestTree
requestListPlatformApplications = req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission = req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestGetEndpointAttributes :: GetEndpointAttributes -> TestTree
requestGetEndpointAttributes = req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes.yaml"

requestListSubscriptions :: ListSubscriptions -> TestTree
requestListSubscriptions = req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

requestGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
requestGetSubscriptionAttributes = req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes.yaml"

requestCreateTopic :: CreateTopic -> TestTree
requestCreateTopic = req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe = req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestUnsubscribe :: Unsubscribe -> TestTree
requestUnsubscribe = req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

requestSetEndpointAttributes :: SetEndpointAttributes -> TestTree
requestSetEndpointAttributes = req
    "SetEndpointAttributes"
    "fixture/SetEndpointAttributes.yaml"

requestSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
requestSetSubscriptionAttributes = req
    "SetSubscriptionAttributes"
    "fixture/SetSubscriptionAttributes.yaml"

requestConfirmSubscription :: ConfirmSubscription -> TestTree
requestConfirmSubscription = req
    "ConfirmSubscription"
    "fixture/ConfirmSubscription.yaml"

requestPublish :: Publish -> TestTree
requestPublish = req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint = res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    sns
    (Proxy :: Proxy DeleteEndpoint)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission = res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    sns
    (Proxy :: Proxy RemovePermission)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic = res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    sns
    (Proxy :: Proxy DeleteTopic)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics = res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    sns
    (Proxy :: Proxy ListTopics)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint = res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    sns
    (Proxy :: Proxy CreatePlatformEndpoint)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes = res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    sns
    (Proxy :: Proxy SetPlatformApplicationAttributes)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic = res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    sns
    (Proxy :: Proxy ListSubscriptionsByTopic)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes = res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    sns
    (Proxy :: Proxy GetTopicAttributes)

responseCreatePlatformApplication :: CreatePlatformApplicationResponse -> TestTree
responseCreatePlatformApplication = res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    sns
    (Proxy :: Proxy CreatePlatformApplication)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes = res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    sns
    (Proxy :: Proxy GetPlatformApplicationAttributes)

responseListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplicationResponse -> TestTree
responseListEndpointsByPlatformApplication = res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    sns
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes = res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    sns
    (Proxy :: Proxy SetTopicAttributes)

responseDeletePlatformApplication :: DeletePlatformApplicationResponse -> TestTree
responseDeletePlatformApplication = res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    sns
    (Proxy :: Proxy DeletePlatformApplication)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications = res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    sns
    (Proxy :: Proxy ListPlatformApplications)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission = res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    sns
    (Proxy :: Proxy AddPermission)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes = res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    sns
    (Proxy :: Proxy GetEndpointAttributes)

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions = res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    sns
    (Proxy :: Proxy ListSubscriptions)

responseGetSubscriptionAttributes :: GetSubscriptionAttributesResponse -> TestTree
responseGetSubscriptionAttributes = res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    sns
    (Proxy :: Proxy GetSubscriptionAttributes)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic = res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    sns
    (Proxy :: Proxy CreateTopic)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe = res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    sns
    (Proxy :: Proxy Subscribe)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe = res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    sns
    (Proxy :: Proxy Unsubscribe)

responseSetEndpointAttributes :: SetEndpointAttributesResponse -> TestTree
responseSetEndpointAttributes = res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    sns
    (Proxy :: Proxy SetEndpointAttributes)

responseSetSubscriptionAttributes :: SetSubscriptionAttributesResponse -> TestTree
responseSetSubscriptionAttributes = res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    sns
    (Proxy :: Proxy SetSubscriptionAttributes)

responseConfirmSubscription :: ConfirmSubscriptionResponse -> TestTree
responseConfirmSubscription = res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    sns
    (Proxy :: Proxy ConfirmSubscription)

responsePublish :: PublishResponse -> TestTree
responsePublish = res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    sns
    (Proxy :: Proxy Publish)
