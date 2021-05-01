{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SNS
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         [ requestDeletePlatformApplication $
--             newDeletePlatformApplication
--
--         , requestConfirmSubscription $
--             newConfirmSubscription
--
--         , requestPublish $
--             newPublish
--
--         , requestOptInPhoneNumber $
--             newOptInPhoneNumber
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestListSubscriptionsByTopic $
--             newListSubscriptionsByTopic
--
--         , requestGetTopicAttributes $
--             newGetTopicAttributes
--
--         , requestSetSMSAttributes $
--             newSetSMSAttributes
--
--         , requestSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributes
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreatePlatformEndpoint $
--             newCreatePlatformEndpoint
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestListTopics $
--             newListTopics
--
--         , requestListSubscriptions $
--             newListSubscriptions
--
--         , requestGetSubscriptionAttributes $
--             newGetSubscriptionAttributes
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOut
--
--         , requestGetEndpointAttributes $
--             newGetEndpointAttributes
--
--         , requestGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributes
--
--         , requestSetTopicAttributes $
--             newSetTopicAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplication
--
--         , requestGetSMSAttributes $
--             newGetSMSAttributes
--
--         , requestListPlatformApplications $
--             newListPlatformApplications
--
--         , requestCreatePlatformApplication $
--             newCreatePlatformApplication
--
--         , requestSetEndpointAttributes $
--             newSetEndpointAttributes
--
--         , requestSetSubscriptionAttributes $
--             newSetSubscriptionAttributes
--
--         , requestCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOut
--
--         , requestDeleteTopic $
--             newDeleteTopic
--
--         , requestCreateTopic $
--             newCreateTopic
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseDeletePlatformApplication $
--             newDeletePlatformApplicationResponse
--
--         , responseConfirmSubscription $
--             newConfirmSubscriptionResponse
--
--         , responsePublish $
--             newPublishResponse
--
--         , responseOptInPhoneNumber $
--             newOptInPhoneNumberResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseListSubscriptionsByTopic $
--             newListSubscriptionsByTopicResponse
--
--         , responseGetTopicAttributes $
--             newGetTopicAttributesResponse
--
--         , responseSetSMSAttributes $
--             newSetSMSAttributesResponse
--
--         , responseSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreatePlatformEndpoint $
--             newCreatePlatformEndpointResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseListTopics $
--             newListTopicsResponse
--
--         , responseListSubscriptions $
--             newListSubscriptionsResponse
--
--         , responseGetSubscriptionAttributes $
--             newGetSubscriptionAttributesResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOutResponse
--
--         , responseGetEndpointAttributes $
--             newGetEndpointAttributesResponse
--
--         , responseGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributesResponse
--
--         , responseSetTopicAttributes $
--             newSetTopicAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplicationResponse
--
--         , responseGetSMSAttributes $
--             newGetSMSAttributesResponse
--
--         , responseListPlatformApplications $
--             newListPlatformApplicationsResponse
--
--         , responseCreatePlatformApplication $
--             newCreatePlatformApplicationResponse
--
--         , responseSetEndpointAttributes $
--             newSetEndpointAttributesResponse
--
--         , responseSetSubscriptionAttributes $
--             newSetSubscriptionAttributesResponse
--
--         , responseCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOutResponse
--
--         , responseDeleteTopic $
--             newDeleteTopicResponse
--
--         , responseCreateTopic $
--             newCreateTopicResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestDeletePlatformApplication :: DeletePlatformApplication -> TestTree
requestDeletePlatformApplication =
  req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

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

requestOptInPhoneNumber :: OptInPhoneNumber -> TestTree
requestOptInPhoneNumber =
  req
    "OptInPhoneNumber"
    "fixture/OptInPhoneNumber.yaml"

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

requestSetSMSAttributes :: SetSMSAttributes -> TestTree
requestSetSMSAttributes =
  req
    "SetSMSAttributes"
    "fixture/SetSMSAttributes.yaml"

requestSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
requestSetPlatformApplicationAttributes =
  req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
requestCreatePlatformEndpoint =
  req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics =
  req
    "ListTopics"
    "fixture/ListTopics.yaml"

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

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOut -> TestTree
requestListPhoneNumbersOptedOut =
  req
    "ListPhoneNumbersOptedOut"
    "fixture/ListPhoneNumbersOptedOut.yaml"

requestGetEndpointAttributes :: GetEndpointAttributes -> TestTree
requestGetEndpointAttributes =
  req
    "GetEndpointAttributes"
    "fixture/GetEndpointAttributes.yaml"

requestGetPlatformApplicationAttributes :: GetPlatformApplicationAttributes -> TestTree
requestGetPlatformApplicationAttributes =
  req
    "GetPlatformApplicationAttributes"
    "fixture/GetPlatformApplicationAttributes.yaml"

requestSetTopicAttributes :: SetTopicAttributes -> TestTree
requestSetTopicAttributes =
  req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

requestListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
requestListEndpointsByPlatformApplication =
  req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

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

requestCreatePlatformApplication :: CreatePlatformApplication -> TestTree
requestCreatePlatformApplication =
  req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

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

requestCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOut -> TestTree
requestCheckIfPhoneNumberIsOptedOut =
  req
    "CheckIfPhoneNumberIsOptedOut"
    "fixture/CheckIfPhoneNumberIsOptedOut.yaml"

requestDeleteTopic :: DeleteTopic -> TestTree
requestDeleteTopic =
  req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

requestCreateTopic :: CreateTopic -> TestTree
requestCreateTopic =
  req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseDeletePlatformApplication :: DeletePlatformApplicationResponse -> TestTree
responseDeletePlatformApplication =
  res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlatformApplication)

responseConfirmSubscription :: ConfirmSubscriptionResponse -> TestTree
responseConfirmSubscription =
  res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy ConfirmSubscription)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy :: Proxy Publish)

responseOptInPhoneNumber :: OptInPhoneNumberResponse -> TestTree
responseOptInPhoneNumber =
  res
    "OptInPhoneNumberResponse"
    "fixture/OptInPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy OptInPhoneNumber)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy :: Proxy Unsubscribe)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic =
  res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionsByTopic)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes =
  res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicAttributes)

responseSetSMSAttributes :: SetSMSAttributesResponse -> TestTree
responseSetSMSAttributes =
  res
    "SetSMSAttributesResponse"
    "fixture/SetSMSAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetSMSAttributes)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes =
  res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetPlatformApplicationAttributes)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint =
  res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlatformEndpoint)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy :: Proxy Subscribe)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopics)

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions =
  res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptions)

responseGetSubscriptionAttributes :: GetSubscriptionAttributesResponse -> TestTree
responseGetSubscriptionAttributes =
  res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubscriptionAttributes)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEndpoint)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddPermission)

responseListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOutResponse -> TestTree
responseListPhoneNumbersOptedOut =
  res
    "ListPhoneNumbersOptedOutResponse"
    "fixture/ListPhoneNumbersOptedOutResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumbersOptedOut)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes =
  res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEndpointAttributes)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes =
  res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPlatformApplicationAttributes)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes =
  res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetTopicAttributes)

responseListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplicationResponse -> TestTree
responseListEndpointsByPlatformApplication =
  res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy ListEndpointsByPlatformApplication)

responseGetSMSAttributes :: GetSMSAttributesResponse -> TestTree
responseGetSMSAttributes =
  res
    "GetSMSAttributesResponse"
    "fixture/GetSMSAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetSMSAttributes)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications =
  res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformApplications)

responseCreatePlatformApplication :: CreatePlatformApplicationResponse -> TestTree
responseCreatePlatformApplication =
  res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlatformApplication)

responseSetEndpointAttributes :: SetEndpointAttributesResponse -> TestTree
responseSetEndpointAttributes =
  res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetEndpointAttributes)

responseSetSubscriptionAttributes :: SetSubscriptionAttributesResponse -> TestTree
responseSetSubscriptionAttributes =
  res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetSubscriptionAttributes)

responseCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> TestTree
responseCheckIfPhoneNumberIsOptedOut =
  res
    "CheckIfPhoneNumberIsOptedOutResponse"
    "fixture/CheckIfPhoneNumberIsOptedOutResponse.proto"
    defaultService
    (Proxy :: Proxy CheckIfPhoneNumberIsOptedOut)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic =
  res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteTopic)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic =
  res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    defaultService
    (Proxy :: Proxy CreateTopic)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy :: Proxy RemovePermission)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
