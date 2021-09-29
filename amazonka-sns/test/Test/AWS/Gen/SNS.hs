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
--         , requestDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumber
--
--         , requestOptInPhoneNumber $
--             newOptInPhoneNumber
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListSubscriptionsByTopic $
--             newListSubscriptionsByTopic
--
--         , requestSetSMSAttributes $
--             newSetSMSAttributes
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributes
--
--         , requestGetTopicAttributes $
--             newGetTopicAttributes
--
--         , requestCreatePlatformEndpoint $
--             newCreatePlatformEndpoint
--
--         , requestListTopics $
--             newListTopics
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestGetEndpointAttributes $
--             newGetEndpointAttributes
--
--         , requestGetSubscriptionAttributes $
--             newGetSubscriptionAttributes
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestListSubscriptions $
--             newListSubscriptions
--
--         , requestListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOut
--
--         , requestSetTopicAttributes $
--             newSetTopicAttributes
--
--         , requestListPlatformApplications $
--             newListPlatformApplications
--
--         , requestGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplication
--
--         , requestGetSMSAttributes $
--             newGetSMSAttributes
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
--         , requestListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbers
--
--         , requestListOriginationNumbers $
--             newListOriginationNumbers
--
--         , requestCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumber
--
--         , requestGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatus
--
--         , requestCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOut
--
--         , requestVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumber
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
--         , responseDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumberResponse
--
--         , responseOptInPhoneNumber $
--             newOptInPhoneNumberResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListSubscriptionsByTopic $
--             newListSubscriptionsByTopicResponse
--
--         , responseSetSMSAttributes $
--             newSetSMSAttributesResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributesResponse
--
--         , responseGetTopicAttributes $
--             newGetTopicAttributesResponse
--
--         , responseCreatePlatformEndpoint $
--             newCreatePlatformEndpointResponse
--
--         , responseListTopics $
--             newListTopicsResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseGetEndpointAttributes $
--             newGetEndpointAttributesResponse
--
--         , responseGetSubscriptionAttributes $
--             newGetSubscriptionAttributesResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseListSubscriptions $
--             newListSubscriptionsResponse
--
--         , responseListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOutResponse
--
--         , responseSetTopicAttributes $
--             newSetTopicAttributesResponse
--
--         , responseListPlatformApplications $
--             newListPlatformApplicationsResponse
--
--         , responseGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplicationResponse
--
--         , responseGetSMSAttributes $
--             newGetSMSAttributesResponse
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
--         , responseListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbersResponse
--
--         , responseListOriginationNumbers $
--             newListOriginationNumbersResponse
--
--         , responseCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumberResponse
--
--         , responseGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatusResponse
--
--         , responseCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOutResponse
--
--         , responseVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumberResponse
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

requestDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumber -> TestTree
requestDeleteSMSSandboxPhoneNumber =
  req
    "DeleteSMSSandboxPhoneNumber"
    "fixture/DeleteSMSSandboxPhoneNumber.yaml"

requestOptInPhoneNumber :: OptInPhoneNumber -> TestTree
requestOptInPhoneNumber =
  req
    "OptInPhoneNumber"
    "fixture/OptInPhoneNumber.yaml"

requestUnsubscribe :: Unsubscribe -> TestTree
requestUnsubscribe =
  req
    "Unsubscribe"
    "fixture/Unsubscribe.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
requestListSubscriptionsByTopic =
  req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

requestSetSMSAttributes :: SetSMSAttributes -> TestTree
requestSetSMSAttributes =
  req
    "SetSMSAttributes"
    "fixture/SetSMSAttributes.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
requestSetPlatformApplicationAttributes =
  req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

requestGetTopicAttributes :: GetTopicAttributes -> TestTree
requestGetTopicAttributes =
  req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

requestCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
requestCreatePlatformEndpoint =
  req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics =
  req
    "ListTopics"
    "fixture/ListTopics.yaml"

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

requestListSubscriptions :: ListSubscriptions -> TestTree
requestListSubscriptions =
  req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

requestListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOut -> TestTree
requestListPhoneNumbersOptedOut =
  req
    "ListPhoneNumbersOptedOut"
    "fixture/ListPhoneNumbersOptedOut.yaml"

requestSetTopicAttributes :: SetTopicAttributes -> TestTree
requestSetTopicAttributes =
  req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

requestListPlatformApplications :: ListPlatformApplications -> TestTree
requestListPlatformApplications =
  req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

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

requestGetSMSAttributes :: GetSMSAttributes -> TestTree
requestGetSMSAttributes =
  req
    "GetSMSAttributes"
    "fixture/GetSMSAttributes.yaml"

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

requestListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbers -> TestTree
requestListSMSSandboxPhoneNumbers =
  req
    "ListSMSSandboxPhoneNumbers"
    "fixture/ListSMSSandboxPhoneNumbers.yaml"

requestListOriginationNumbers :: ListOriginationNumbers -> TestTree
requestListOriginationNumbers =
  req
    "ListOriginationNumbers"
    "fixture/ListOriginationNumbers.yaml"

requestCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumber -> TestTree
requestCreateSMSSandboxPhoneNumber =
  req
    "CreateSMSSandboxPhoneNumber"
    "fixture/CreateSMSSandboxPhoneNumber.yaml"

requestGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatus -> TestTree
requestGetSMSSandboxAccountStatus =
  req
    "GetSMSSandboxAccountStatus"
    "fixture/GetSMSSandboxAccountStatus.yaml"

requestCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOut -> TestTree
requestCheckIfPhoneNumberIsOptedOut =
  req
    "CheckIfPhoneNumberIsOptedOut"
    "fixture/CheckIfPhoneNumberIsOptedOut.yaml"

requestVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumber -> TestTree
requestVerifySMSSandboxPhoneNumber =
  req
    "VerifySMSSandboxPhoneNumber"
    "fixture/VerifySMSSandboxPhoneNumber.yaml"

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

responseDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumberResponse -> TestTree
responseDeleteSMSSandboxPhoneNumber =
  res
    "DeleteSMSSandboxPhoneNumberResponse"
    "fixture/DeleteSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSMSSandboxPhoneNumber)

responseOptInPhoneNumber :: OptInPhoneNumberResponse -> TestTree
responseOptInPhoneNumber =
  res
    "OptInPhoneNumberResponse"
    "fixture/OptInPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy OptInPhoneNumber)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy :: Proxy Unsubscribe)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic =
  res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionsByTopic)

responseSetSMSAttributes :: SetSMSAttributesResponse -> TestTree
responseSetSMSAttributes =
  res
    "SetSMSAttributesResponse"
    "fixture/SetSMSAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetSMSAttributes)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy :: Proxy Subscribe)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes =
  res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetPlatformApplicationAttributes)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes =
  res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetTopicAttributes)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint =
  res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlatformEndpoint)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    defaultService
    (Proxy :: Proxy ListTopics)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy :: Proxy AddPermission)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes =
  res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetEndpointAttributes)

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

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions =
  res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptions)

responseListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOutResponse -> TestTree
responseListPhoneNumbersOptedOut =
  res
    "ListPhoneNumbersOptedOutResponse"
    "fixture/ListPhoneNumbersOptedOutResponse.proto"
    defaultService
    (Proxy :: Proxy ListPhoneNumbersOptedOut)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes =
  res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy SetTopicAttributes)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications =
  res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformApplications)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes =
  res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy GetPlatformApplicationAttributes)

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

responseListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbersResponse -> TestTree
responseListSMSSandboxPhoneNumbers =
  res
    "ListSMSSandboxPhoneNumbersResponse"
    "fixture/ListSMSSandboxPhoneNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy ListSMSSandboxPhoneNumbers)

responseListOriginationNumbers :: ListOriginationNumbersResponse -> TestTree
responseListOriginationNumbers =
  res
    "ListOriginationNumbersResponse"
    "fixture/ListOriginationNumbersResponse.proto"
    defaultService
    (Proxy :: Proxy ListOriginationNumbers)

responseCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumberResponse -> TestTree
responseCreateSMSSandboxPhoneNumber =
  res
    "CreateSMSSandboxPhoneNumberResponse"
    "fixture/CreateSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSMSSandboxPhoneNumber)

responseGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatusResponse -> TestTree
responseGetSMSSandboxAccountStatus =
  res
    "GetSMSSandboxAccountStatusResponse"
    "fixture/GetSMSSandboxAccountStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetSMSSandboxAccountStatus)

responseCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> TestTree
responseCheckIfPhoneNumberIsOptedOut =
  res
    "CheckIfPhoneNumberIsOptedOutResponse"
    "fixture/CheckIfPhoneNumberIsOptedOutResponse.proto"
    defaultService
    (Proxy :: Proxy CheckIfPhoneNumberIsOptedOut)

responseVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumberResponse -> TestTree
responseVerifySMSSandboxPhoneNumber =
  res
    "VerifySMSSandboxPhoneNumberResponse"
    "fixture/VerifySMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy :: Proxy VerifySMSSandboxPhoneNumber)

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
