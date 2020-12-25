{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SNS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SNS where

import Data.Proxy
import Network.AWS.SNS
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SNS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPhoneNumbersOptedOut $
--             mkListPhoneNumbersOptedOut
--
--         , requestDeleteEndpoint $
--             mkDeleteEndpoint
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRemovePermission $
--             mkRemovePermission
--
--         , requestDeleteTopic $
--             mkDeleteTopic
--
--         , requestSetSMSAttributes $
--             mkSetSMSAttributes
--
--         , requestListTopics $
--             mkListTopics
--
--         , requestCreatePlatformEndpoint $
--             mkCreatePlatformEndpoint
--
--         , requestSetPlatformApplicationAttributes $
--             mkSetPlatformApplicationAttributes
--
--         , requestListSubscriptionsByTopic $
--             mkListSubscriptionsByTopic
--
--         , requestGetTopicAttributes $
--             mkGetTopicAttributes
--
--         , requestOptInPhoneNumber $
--             mkOptInPhoneNumber
--
--         , requestCreatePlatformApplication $
--             mkCreatePlatformApplication
--
--         , requestGetPlatformApplicationAttributes $
--             mkGetPlatformApplicationAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             mkListEndpointsByPlatformApplication
--
--         , requestSetTopicAttributes $
--             mkSetTopicAttributes
--
--         , requestDeletePlatformApplication $
--             mkDeletePlatformApplication
--
--         , requestGetSMSAttributes $
--             mkGetSMSAttributes
--
--         , requestListPlatformApplications $
--             mkListPlatformApplications
--
--         , requestAddPermission $
--             mkAddPermission
--
--         , requestGetEndpointAttributes $
--             mkGetEndpointAttributes
--
--         , requestListSubscriptions $
--             mkListSubscriptions
--
--         , requestGetSubscriptionAttributes $
--             mkGetSubscriptionAttributes
--
--         , requestCreateTopic $
--             mkCreateTopic
--
--         , requestCheckIfPhoneNumberIsOptedOut $
--             mkCheckIfPhoneNumberIsOptedOut
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestSubscribe $
--             mkSubscribe
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestUnsubscribe $
--             mkUnsubscribe
--
--         , requestSetEndpointAttributes $
--             mkSetEndpointAttributes
--
--         , requestSetSubscriptionAttributes $
--             mkSetSubscriptionAttributes
--
--         , requestConfirmSubscription $
--             mkConfirmSubscription
--
--         , requestPublish $
--             mkPublish
--
--           ]

--     , testGroup "response"
--         [ responseListPhoneNumbersOptedOut $
--             mkListPhoneNumbersOptedOutResponse
--
--         , responseDeleteEndpoint $
--             mkDeleteEndpointResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRemovePermission $
--             mkRemovePermissionResponse
--
--         , responseDeleteTopic $
--             mkDeleteTopicResponse
--
--         , responseSetSMSAttributes $
--             mkSetSMSAttributesResponse
--
--         , responseListTopics $
--             mkListTopicsResponse
--
--         , responseCreatePlatformEndpoint $
--             mkCreatePlatformEndpointResponse
--
--         , responseSetPlatformApplicationAttributes $
--             mkSetPlatformApplicationAttributesResponse
--
--         , responseListSubscriptionsByTopic $
--             mkListSubscriptionsByTopicResponse
--
--         , responseGetTopicAttributes $
--             mkGetTopicAttributesResponse
--
--         , responseOptInPhoneNumber $
--             mkOptInPhoneNumberResponse
--
--         , responseCreatePlatformApplication $
--             mkCreatePlatformApplicationResponse
--
--         , responseGetPlatformApplicationAttributes $
--             mkGetPlatformApplicationAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             mkListEndpointsByPlatformApplicationResponse
--
--         , responseSetTopicAttributes $
--             mkSetTopicAttributesResponse
--
--         , responseDeletePlatformApplication $
--             mkDeletePlatformApplicationResponse
--
--         , responseGetSMSAttributes $
--             mkGetSMSAttributesResponse
--
--         , responseListPlatformApplications $
--             mkListPlatformApplicationsResponse
--
--         , responseAddPermission $
--             mkAddPermissionResponse
--
--         , responseGetEndpointAttributes $
--             mkGetEndpointAttributesResponse
--
--         , responseListSubscriptions $
--             mkListSubscriptionsResponse
--
--         , responseGetSubscriptionAttributes $
--             mkGetSubscriptionAttributesResponse
--
--         , responseCreateTopic $
--             mkCreateTopicResponse
--
--         , responseCheckIfPhoneNumberIsOptedOut $
--             mkCheckIfPhoneNumberIsOptedOutResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseSubscribe $
--             mkSubscribeResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseUnsubscribe $
--             mkUnsubscribeResponse
--
--         , responseSetEndpointAttributes $
--             mkSetEndpointAttributesResponse
--
--         , responseSetSubscriptionAttributes $
--             mkSetSubscriptionAttributesResponse
--
--         , responseConfirmSubscription $
--             mkConfirmSubscriptionResponse
--
--         , responsePublish $
--             mkPublishResponse
--
--           ]
--     ]

-- Requests

requestListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOut -> TestTree
requestListPhoneNumbersOptedOut =
  req
    "ListPhoneNumbersOptedOut"
    "fixture/ListPhoneNumbersOptedOut.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestDeleteTopic :: DeleteTopic -> TestTree
requestDeleteTopic =
  req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

requestSetSMSAttributes :: SetSMSAttributes -> TestTree
requestSetSMSAttributes =
  req
    "SetSMSAttributes"
    "fixture/SetSMSAttributes.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics =
  req
    "ListTopics"
    "fixture/ListTopics.yaml"

requestCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
requestCreatePlatformEndpoint =
  req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

requestSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
requestSetPlatformApplicationAttributes =
  req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

requestListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
requestListSubscriptionsByTopic =
  req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

requestGetTopicAttributes :: GetTopicAttributes -> TestTree
requestGetTopicAttributes =
  req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

requestOptInPhoneNumber :: OptInPhoneNumber -> TestTree
requestOptInPhoneNumber =
  req
    "OptInPhoneNumber"
    "fixture/OptInPhoneNumber.yaml"

requestCreatePlatformApplication :: CreatePlatformApplication -> TestTree
requestCreatePlatformApplication =
  req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

requestGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
requestGetPlatformApplicationAttributes =
  req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes.yaml"

requestListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
requestListEndpointsByPlatformApplication =
  req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

requestSetTopicAttributes :: SetTopicAttributes -> TestTree
requestSetTopicAttributes =
  req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

requestDeletePlatformApplication :: DeletePlatformApplication -> TestTree
requestDeletePlatformApplication =
  req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

requestGetSMSAttributes :: GetSMSAttributes -> TestTree
requestGetSMSAttributes =
  req
    "GetSMSAttributes"
    "fixture/GetSMSAttributes.yaml"

requestListPlatformApplications :: ListPlatformApplications -> TestTree
requestListPlatformApplications =
  req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestGetEndpointAttributes :: GetEndpointAttributes -> TestTree
requestGetEndpointAttributes =
  req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes.yaml"

requestListSubscriptions :: ListSubscriptions -> TestTree
requestListSubscriptions =
  req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

requestGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
requestGetSubscriptionAttributes =
  req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes.yaml"

requestCreateTopic :: CreateTopic -> TestTree
requestCreateTopic =
  req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

requestCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOut -> TestTree
requestCheckIfPhoneNumberIsOptedOut =
  req
    "CheckIfPhoneNumberIsOptedOut"
    "fixture/CheckIfPhoneNumberIsOptedOut.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUnsubscribe :: Unsubscribe -> TestTree
requestUnsubscribe =
  req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

requestSetEndpointAttributes :: SetEndpointAttributes -> TestTree
requestSetEndpointAttributes =
  req
    "SetEndpointAttributes"
    "fixture/SetEndpointAttributes.yaml"

requestSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
requestSetSubscriptionAttributes =
  req
    "SetSubscriptionAttributes"
    "fixture/SetSubscriptionAttributes.yaml"

requestConfirmSubscription :: ConfirmSubscription -> TestTree
requestConfirmSubscription =
  req
    "ConfirmSubscription"
    "fixture/ConfirmSubscription.yaml"

requestPublish :: Publish -> TestTree
requestPublish =
  req
    "Publish"
    "fixture/Publish.yaml"

-- Responses

responseListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOutResponse -> TestTree
responseListPhoneNumbersOptedOut =
  res
    "ListPhoneNumbersOptedOutResponse"
    "fixture/ListPhoneNumbersOptedOutResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPhoneNumbersOptedOut)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemovePermission)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic =
  res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteTopic)

responseSetSMSAttributes :: SetSMSAttributesResponse -> TestTree
responseSetSMSAttributes =
  res
    "SetSMSAttributesResponse"
    "fixture/SetSMSAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSMSAttributes)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTopics)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint =
  res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePlatformEndpoint)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes =
  res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetPlatformApplicationAttributes)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic =
  res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSubscriptionsByTopic)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes =
  res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetTopicAttributes)

responseOptInPhoneNumber :: OptInPhoneNumberResponse -> TestTree
responseOptInPhoneNumber =
  res
    "OptInPhoneNumberResponse"
    "fixture/OptInPhoneNumberResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy OptInPhoneNumber)

responseCreatePlatformApplication :: CreatePlatformApplicationResponse -> TestTree
responseCreatePlatformApplication =
  res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePlatformApplication)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes =
  res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetPlatformApplicationAttributes)

responseListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplicationResponse -> TestTree
responseListEndpointsByPlatformApplication =
  res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes =
  res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetTopicAttributes)

responseDeletePlatformApplication :: DeletePlatformApplicationResponse -> TestTree
responseDeletePlatformApplication =
  res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePlatformApplication)

responseGetSMSAttributes :: GetSMSAttributesResponse -> TestTree
responseGetSMSAttributes =
  res
    "GetSMSAttributesResponse"
    "fixture/GetSMSAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSMSAttributes)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications =
  res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPlatformApplications)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AddPermission)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes =
  res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetEndpointAttributes)

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions =
  res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListSubscriptions)

responseGetSubscriptionAttributes :: GetSubscriptionAttributesResponse -> TestTree
responseGetSubscriptionAttributes =
  res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy GetSubscriptionAttributes)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic =
  res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateTopic)

responseCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> TestTree
responseCheckIfPhoneNumberIsOptedOut =
  res
    "CheckIfPhoneNumberIsOptedOutResponse"
    "fixture/CheckIfPhoneNumberIsOptedOutResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CheckIfPhoneNumberIsOptedOut)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Subscribe)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Unsubscribe)

responseSetEndpointAttributes :: SetEndpointAttributesResponse -> TestTree
responseSetEndpointAttributes =
  res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetEndpointAttributes)

responseSetSubscriptionAttributes :: SetSubscriptionAttributesResponse -> TestTree
responseSetSubscriptionAttributes =
  res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy SetSubscriptionAttributes)

responseConfirmSubscription :: ConfirmSubscriptionResponse -> TestTree
responseConfirmSubscription =
  res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ConfirmSubscription)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy Publish)
