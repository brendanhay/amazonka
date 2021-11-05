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

import Amazonka.SNS
import qualified Data.Proxy as Proxy
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
--             newListPhoneNumbersOptedOut
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestDeleteTopic $
--             newDeleteTopic
--
--         , requestSetSMSAttributes $
--             newSetSMSAttributes
--
--         , requestListTopics $
--             newListTopics
--
--         , requestVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumber
--
--         , requestCreatePlatformEndpoint $
--             newCreatePlatformEndpoint
--
--         , requestSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributes
--
--         , requestListSubscriptionsByTopic $
--             newListSubscriptionsByTopic
--
--         , requestGetTopicAttributes $
--             newGetTopicAttributes
--
--         , requestCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumber
--
--         , requestOptInPhoneNumber $
--             newOptInPhoneNumber
--
--         , requestDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumber
--
--         , requestListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbers
--
--         , requestCreatePlatformApplication $
--             newCreatePlatformApplication
--
--         , requestGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplication
--
--         , requestSetTopicAttributes $
--             newSetTopicAttributes
--
--         , requestDeletePlatformApplication $
--             newDeletePlatformApplication
--
--         , requestGetSMSAttributes $
--             newGetSMSAttributes
--
--         , requestListPlatformApplications $
--             newListPlatformApplications
--
--         , requestAddPermission $
--             newAddPermission
--
--         , requestGetEndpointAttributes $
--             newGetEndpointAttributes
--
--         , requestListSubscriptions $
--             newListSubscriptions
--
--         , requestGetSubscriptionAttributes $
--             newGetSubscriptionAttributes
--
--         , requestCreateTopic $
--             newCreateTopic
--
--         , requestCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOut
--
--         , requestTagResource $
--             newTagResource
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestListOriginationNumbers $
--             newListOriginationNumbers
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatus
--
--         , requestSetEndpointAttributes $
--             newSetEndpointAttributes
--
--         , requestSetSubscriptionAttributes $
--             newSetSubscriptionAttributes
--
--         , requestConfirmSubscription $
--             newConfirmSubscription
--
--         , requestPublish $
--             newPublish
--
--           ]

--     , testGroup "response"
--         [ responseListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOutResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseDeleteTopic $
--             newDeleteTopicResponse
--
--         , responseSetSMSAttributes $
--             newSetSMSAttributesResponse
--
--         , responseListTopics $
--             newListTopicsResponse
--
--         , responseVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumberResponse
--
--         , responseCreatePlatformEndpoint $
--             newCreatePlatformEndpointResponse
--
--         , responseSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributesResponse
--
--         , responseListSubscriptionsByTopic $
--             newListSubscriptionsByTopicResponse
--
--         , responseGetTopicAttributes $
--             newGetTopicAttributesResponse
--
--         , responseCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumberResponse
--
--         , responseOptInPhoneNumber $
--             newOptInPhoneNumberResponse
--
--         , responseDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumberResponse
--
--         , responseListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbersResponse
--
--         , responseCreatePlatformApplication $
--             newCreatePlatformApplicationResponse
--
--         , responseGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplicationResponse
--
--         , responseSetTopicAttributes $
--             newSetTopicAttributesResponse
--
--         , responseDeletePlatformApplication $
--             newDeletePlatformApplicationResponse
--
--         , responseGetSMSAttributes $
--             newGetSMSAttributesResponse
--
--         , responseListPlatformApplications $
--             newListPlatformApplicationsResponse
--
--         , responseAddPermission $
--             newAddPermissionResponse
--
--         , responseGetEndpointAttributes $
--             newGetEndpointAttributesResponse
--
--         , responseListSubscriptions $
--             newListSubscriptionsResponse
--
--         , responseGetSubscriptionAttributes $
--             newGetSubscriptionAttributesResponse
--
--         , responseCreateTopic $
--             newCreateTopicResponse
--
--         , responseCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOutResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseListOriginationNumbers $
--             newListOriginationNumbersResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatusResponse
--
--         , responseSetEndpointAttributes $
--             newSetEndpointAttributesResponse
--
--         , responseSetSubscriptionAttributes $
--             newSetSubscriptionAttributesResponse
--
--         , responseConfirmSubscription $
--             newConfirmSubscriptionResponse
--
--         , responsePublish $
--             newPublishResponse
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

requestVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumber -> TestTree
requestVerifySMSSandboxPhoneNumber =
  req
    "VerifySMSSandboxPhoneNumber"
    "fixture/VerifySMSSandboxPhoneNumber.yaml"

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

requestCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumber -> TestTree
requestCreateSMSSandboxPhoneNumber =
  req
    "CreateSMSSandboxPhoneNumber"
    "fixture/CreateSMSSandboxPhoneNumber.yaml"

requestOptInPhoneNumber :: OptInPhoneNumber -> TestTree
requestOptInPhoneNumber =
  req
    "OptInPhoneNumber"
    "fixture/OptInPhoneNumber.yaml"

requestDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumber -> TestTree
requestDeleteSMSSandboxPhoneNumber =
  req
    "DeleteSMSSandboxPhoneNumber"
    "fixture/DeleteSMSSandboxPhoneNumber.yaml"

requestListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbers -> TestTree
requestListSMSSandboxPhoneNumbers =
  req
    "ListSMSSandboxPhoneNumbers"
    "fixture/ListSMSSandboxPhoneNumbers.yaml"

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

requestListOriginationNumbers :: ListOriginationNumbers -> TestTree
requestListOriginationNumbers =
  req
    "ListOriginationNumbers"
    "fixture/ListOriginationNumbers.yaml"

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

requestGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatus -> TestTree
requestGetSMSSandboxAccountStatus =
  req
    "GetSMSSandboxAccountStatus"
    "fixture/GetSMSSandboxAccountStatus.yaml"

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
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbersOptedOut)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic =
  res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopic)

responseSetSMSAttributes :: SetSMSAttributesResponse -> TestTree
responseSetSMSAttributes =
  res
    "SetSMSAttributesResponse"
    "fixture/SetSMSAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSMSAttributes)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopics)

responseVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumberResponse -> TestTree
responseVerifySMSSandboxPhoneNumber =
  res
    "VerifySMSSandboxPhoneNumberResponse"
    "fixture/VerifySMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifySMSSandboxPhoneNumber)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint =
  res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlatformEndpoint)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes =
  res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetPlatformApplicationAttributes)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic =
  res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionsByTopic)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes =
  res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicAttributes)

responseCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumberResponse -> TestTree
responseCreateSMSSandboxPhoneNumber =
  res
    "CreateSMSSandboxPhoneNumberResponse"
    "fixture/CreateSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSMSSandboxPhoneNumber)

responseOptInPhoneNumber :: OptInPhoneNumberResponse -> TestTree
responseOptInPhoneNumber =
  res
    "OptInPhoneNumberResponse"
    "fixture/OptInPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OptInPhoneNumber)

responseDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumberResponse -> TestTree
responseDeleteSMSSandboxPhoneNumber =
  res
    "DeleteSMSSandboxPhoneNumberResponse"
    "fixture/DeleteSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSMSSandboxPhoneNumber)

responseListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbersResponse -> TestTree
responseListSMSSandboxPhoneNumbers =
  res
    "ListSMSSandboxPhoneNumbersResponse"
    "fixture/ListSMSSandboxPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSMSSandboxPhoneNumbers)

responseCreatePlatformApplication :: CreatePlatformApplicationResponse -> TestTree
responseCreatePlatformApplication =
  res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlatformApplication)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes =
  res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlatformApplicationAttributes)

responseListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplicationResponse -> TestTree
responseListEndpointsByPlatformApplication =
  res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointsByPlatformApplication)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes =
  res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTopicAttributes)

responseDeletePlatformApplication :: DeletePlatformApplicationResponse -> TestTree
responseDeletePlatformApplication =
  res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlatformApplication)

responseGetSMSAttributes :: GetSMSAttributesResponse -> TestTree
responseGetSMSAttributes =
  res
    "GetSMSAttributesResponse"
    "fixture/GetSMSAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSMSAttributes)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications =
  res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlatformApplications)

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes =
  res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpointAttributes)

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions =
  res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptions)

responseGetSubscriptionAttributes :: GetSubscriptionAttributesResponse -> TestTree
responseGetSubscriptionAttributes =
  res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionAttributes)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic =
  res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopic)

responseCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> TestTree
responseCheckIfPhoneNumberIsOptedOut =
  res
    "CheckIfPhoneNumberIsOptedOutResponse"
    "fixture/CheckIfPhoneNumberIsOptedOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckIfPhoneNumberIsOptedOut)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Subscribe)

responseListOriginationNumbers :: ListOriginationNumbersResponse -> TestTree
responseListOriginationNumbers =
  res
    "ListOriginationNumbersResponse"
    "fixture/ListOriginationNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOriginationNumbers)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Unsubscribe)

responseGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatusResponse -> TestTree
responseGetSMSSandboxAccountStatus =
  res
    "GetSMSSandboxAccountStatusResponse"
    "fixture/GetSMSSandboxAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSMSSandboxAccountStatus)

responseSetEndpointAttributes :: SetEndpointAttributesResponse -> TestTree
responseSetEndpointAttributes =
  res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetEndpointAttributes)

responseSetSubscriptionAttributes :: SetSubscriptionAttributesResponse -> TestTree
responseSetSubscriptionAttributes =
  res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSubscriptionAttributes)

responseConfirmSubscription :: ConfirmSubscriptionResponse -> TestTree
responseConfirmSubscription =
  res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmSubscription)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Publish)
