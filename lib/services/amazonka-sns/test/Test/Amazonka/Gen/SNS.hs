{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SNS
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SNS where

import Amazonka.SNS
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SNS.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAddPermission $
--             newAddPermission
--
--         , requestCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOut
--
--         , requestConfirmSubscription $
--             newConfirmSubscription
--
--         , requestCreatePlatformApplication $
--             newCreatePlatformApplication
--
--         , requestCreatePlatformEndpoint $
--             newCreatePlatformEndpoint
--
--         , requestCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumber
--
--         , requestCreateTopic $
--             newCreateTopic
--
--         , requestDeleteEndpoint $
--             newDeleteEndpoint
--
--         , requestDeletePlatformApplication $
--             newDeletePlatformApplication
--
--         , requestDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumber
--
--         , requestDeleteTopic $
--             newDeleteTopic
--
--         , requestGetDataProtectionPolicy $
--             newGetDataProtectionPolicy
--
--         , requestGetEndpointAttributes $
--             newGetEndpointAttributes
--
--         , requestGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributes
--
--         , requestGetSMSAttributes $
--             newGetSMSAttributes
--
--         , requestGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatus
--
--         , requestGetSubscriptionAttributes $
--             newGetSubscriptionAttributes
--
--         , requestGetTopicAttributes $
--             newGetTopicAttributes
--
--         , requestListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplication
--
--         , requestListOriginationNumbers $
--             newListOriginationNumbers
--
--         , requestListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOut
--
--         , requestListPlatformApplications $
--             newListPlatformApplications
--
--         , requestListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbers
--
--         , requestListSubscriptions $
--             newListSubscriptions
--
--         , requestListSubscriptionsByTopic $
--             newListSubscriptionsByTopic
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTopics $
--             newListTopics
--
--         , requestOptInPhoneNumber $
--             newOptInPhoneNumber
--
--         , requestPublish $
--             newPublish
--
--         , requestPublishBatch $
--             newPublishBatch
--
--         , requestPutDataProtectionPolicy $
--             newPutDataProtectionPolicy
--
--         , requestRemovePermission $
--             newRemovePermission
--
--         , requestSetEndpointAttributes $
--             newSetEndpointAttributes
--
--         , requestSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributes
--
--         , requestSetSMSAttributes $
--             newSetSMSAttributes
--
--         , requestSetSubscriptionAttributes $
--             newSetSubscriptionAttributes
--
--         , requestSetTopicAttributes $
--             newSetTopicAttributes
--
--         , requestSubscribe $
--             newSubscribe
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUnsubscribe $
--             newUnsubscribe
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumber
--
--           ]

--     , testGroup "response"
--         [ responseAddPermission $
--             newAddPermissionResponse
--
--         , responseCheckIfPhoneNumberIsOptedOut $
--             newCheckIfPhoneNumberIsOptedOutResponse
--
--         , responseConfirmSubscription $
--             newConfirmSubscriptionResponse
--
--         , responseCreatePlatformApplication $
--             newCreatePlatformApplicationResponse
--
--         , responseCreatePlatformEndpoint $
--             newCreatePlatformEndpointResponse
--
--         , responseCreateSMSSandboxPhoneNumber $
--             newCreateSMSSandboxPhoneNumberResponse
--
--         , responseCreateTopic $
--             newCreateTopicResponse
--
--         , responseDeleteEndpoint $
--             newDeleteEndpointResponse
--
--         , responseDeletePlatformApplication $
--             newDeletePlatformApplicationResponse
--
--         , responseDeleteSMSSandboxPhoneNumber $
--             newDeleteSMSSandboxPhoneNumberResponse
--
--         , responseDeleteTopic $
--             newDeleteTopicResponse
--
--         , responseGetDataProtectionPolicy $
--             newGetDataProtectionPolicyResponse
--
--         , responseGetEndpointAttributes $
--             newGetEndpointAttributesResponse
--
--         , responseGetPlatformApplicationAttributes $
--             newGetPlatformApplicationAttributesResponse
--
--         , responseGetSMSAttributes $
--             newGetSMSAttributesResponse
--
--         , responseGetSMSSandboxAccountStatus $
--             newGetSMSSandboxAccountStatusResponse
--
--         , responseGetSubscriptionAttributes $
--             newGetSubscriptionAttributesResponse
--
--         , responseGetTopicAttributes $
--             newGetTopicAttributesResponse
--
--         , responseListEndpointsByPlatformApplication $
--             newListEndpointsByPlatformApplicationResponse
--
--         , responseListOriginationNumbers $
--             newListOriginationNumbersResponse
--
--         , responseListPhoneNumbersOptedOut $
--             newListPhoneNumbersOptedOutResponse
--
--         , responseListPlatformApplications $
--             newListPlatformApplicationsResponse
--
--         , responseListSMSSandboxPhoneNumbers $
--             newListSMSSandboxPhoneNumbersResponse
--
--         , responseListSubscriptions $
--             newListSubscriptionsResponse
--
--         , responseListSubscriptionsByTopic $
--             newListSubscriptionsByTopicResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTopics $
--             newListTopicsResponse
--
--         , responseOptInPhoneNumber $
--             newOptInPhoneNumberResponse
--
--         , responsePublish $
--             newPublishResponse
--
--         , responsePublishBatch $
--             newPublishBatchResponse
--
--         , responsePutDataProtectionPolicy $
--             newPutDataProtectionPolicyResponse
--
--         , responseRemovePermission $
--             newRemovePermissionResponse
--
--         , responseSetEndpointAttributes $
--             newSetEndpointAttributesResponse
--
--         , responseSetPlatformApplicationAttributes $
--             newSetPlatformApplicationAttributesResponse
--
--         , responseSetSMSAttributes $
--             newSetSMSAttributesResponse
--
--         , responseSetSubscriptionAttributes $
--             newSetSubscriptionAttributesResponse
--
--         , responseSetTopicAttributes $
--             newSetTopicAttributesResponse
--
--         , responseSubscribe $
--             newSubscribeResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUnsubscribe $
--             newUnsubscribeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseVerifySMSSandboxPhoneNumber $
--             newVerifySMSSandboxPhoneNumberResponse
--
--           ]
--     ]

-- Requests

requestAddPermission :: AddPermission -> TestTree
requestAddPermission =
  req
    "AddPermission"
    "fixture/AddPermission.yaml"

requestCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOut -> TestTree
requestCheckIfPhoneNumberIsOptedOut =
  req
    "CheckIfPhoneNumberIsOptedOut"
    "fixture/CheckIfPhoneNumberIsOptedOut.yaml"

requestConfirmSubscription :: ConfirmSubscription -> TestTree
requestConfirmSubscription =
  req
    "ConfirmSubscription"
    "fixture/ConfirmSubscription.yaml"

requestCreatePlatformApplication :: CreatePlatformApplication -> TestTree
requestCreatePlatformApplication =
  req
    "CreatePlatformApplication"
    "fixture/CreatePlatformApplication.yaml"

requestCreatePlatformEndpoint :: CreatePlatformEndpoint -> TestTree
requestCreatePlatformEndpoint =
  req
    "CreatePlatformEndpoint"
    "fixture/CreatePlatformEndpoint.yaml"

requestCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumber -> TestTree
requestCreateSMSSandboxPhoneNumber =
  req
    "CreateSMSSandboxPhoneNumber"
    "fixture/CreateSMSSandboxPhoneNumber.yaml"

requestCreateTopic :: CreateTopic -> TestTree
requestCreateTopic =
  req
    "CreateTopic"
    "fixture/CreateTopic.yaml"

requestDeleteEndpoint :: DeleteEndpoint -> TestTree
requestDeleteEndpoint =
  req
    "DeleteEndpoint"
    "fixture/DeleteEndpoint.yaml"

requestDeletePlatformApplication :: DeletePlatformApplication -> TestTree
requestDeletePlatformApplication =
  req
    "DeletePlatformApplication"
    "fixture/DeletePlatformApplication.yaml"

requestDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumber -> TestTree
requestDeleteSMSSandboxPhoneNumber =
  req
    "DeleteSMSSandboxPhoneNumber"
    "fixture/DeleteSMSSandboxPhoneNumber.yaml"

requestDeleteTopic :: DeleteTopic -> TestTree
requestDeleteTopic =
  req
    "DeleteTopic"
    "fixture/DeleteTopic.yaml"

requestGetDataProtectionPolicy :: GetDataProtectionPolicy -> TestTree
requestGetDataProtectionPolicy =
  req
    "GetDataProtectionPolicy"
    "fixture/GetDataProtectionPolicy.yaml"

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

requestGetSMSAttributes :: GetSMSAttributes -> TestTree
requestGetSMSAttributes =
  req
    "GetSMSAttributes"
    "fixture/GetSMSAttributes.yaml"

requestGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatus -> TestTree
requestGetSMSSandboxAccountStatus =
  req
    "GetSMSSandboxAccountStatus"
    "fixture/GetSMSSandboxAccountStatus.yaml"

requestGetSubscriptionAttributes :: GetSubscriptionAttributes -> TestTree
requestGetSubscriptionAttributes =
  req
    "GetSubscriptionAttributes"
    "fixture/GetSubscriptionAttributes.yaml"

requestGetTopicAttributes :: GetTopicAttributes -> TestTree
requestGetTopicAttributes =
  req
    "GetTopicAttributes"
    "fixture/GetTopicAttributes.yaml"

requestListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplication -> TestTree
requestListEndpointsByPlatformApplication =
  req
    "ListEndpointsByPlatformApplication"
    "fixture/ListEndpointsByPlatformApplication.yaml"

requestListOriginationNumbers :: ListOriginationNumbers -> TestTree
requestListOriginationNumbers =
  req
    "ListOriginationNumbers"
    "fixture/ListOriginationNumbers.yaml"

requestListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOut -> TestTree
requestListPhoneNumbersOptedOut =
  req
    "ListPhoneNumbersOptedOut"
    "fixture/ListPhoneNumbersOptedOut.yaml"

requestListPlatformApplications :: ListPlatformApplications -> TestTree
requestListPlatformApplications =
  req
    "ListPlatformApplications"
    "fixture/ListPlatformApplications.yaml"

requestListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbers -> TestTree
requestListSMSSandboxPhoneNumbers =
  req
    "ListSMSSandboxPhoneNumbers"
    "fixture/ListSMSSandboxPhoneNumbers.yaml"

requestListSubscriptions :: ListSubscriptions -> TestTree
requestListSubscriptions =
  req
    "ListSubscriptions"
    "fixture/ListSubscriptions.yaml"

requestListSubscriptionsByTopic :: ListSubscriptionsByTopic -> TestTree
requestListSubscriptionsByTopic =
  req
    "ListSubscriptionsByTopic"
    "fixture/ListSubscriptionsByTopic.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTopics :: ListTopics -> TestTree
requestListTopics =
  req
    "ListTopics"
    "fixture/ListTopics.yaml"

requestOptInPhoneNumber :: OptInPhoneNumber -> TestTree
requestOptInPhoneNumber =
  req
    "OptInPhoneNumber"
    "fixture/OptInPhoneNumber.yaml"

requestPublish :: Publish -> TestTree
requestPublish =
  req
    "Publish"
    "fixture/Publish.yaml"

requestPublishBatch :: PublishBatch -> TestTree
requestPublishBatch =
  req
    "PublishBatch"
    "fixture/PublishBatch.yaml"

requestPutDataProtectionPolicy :: PutDataProtectionPolicy -> TestTree
requestPutDataProtectionPolicy =
  req
    "PutDataProtectionPolicy"
    "fixture/PutDataProtectionPolicy.yaml"

requestRemovePermission :: RemovePermission -> TestTree
requestRemovePermission =
  req
    "RemovePermission"
    "fixture/RemovePermission.yaml"

requestSetEndpointAttributes :: SetEndpointAttributes -> TestTree
requestSetEndpointAttributes =
  req
    "SetEndpointAttributes"
    "fixture/SetEndpointAttributes.yaml"

requestSetPlatformApplicationAttributes :: SetPlatformApplicationAttributes -> TestTree
requestSetPlatformApplicationAttributes =
  req
    "SetPlatformApplicationAttributes"
    "fixture/SetPlatformApplicationAttributes.yaml"

requestSetSMSAttributes :: SetSMSAttributes -> TestTree
requestSetSMSAttributes =
  req
    "SetSMSAttributes"
    "fixture/SetSMSAttributes.yaml"

requestSetSubscriptionAttributes :: SetSubscriptionAttributes -> TestTree
requestSetSubscriptionAttributes =
  req
    "SetSubscriptionAttributes"
    "fixture/SetSubscriptionAttributes.yaml"

requestSetTopicAttributes :: SetTopicAttributes -> TestTree
requestSetTopicAttributes =
  req
    "SetTopicAttributes"
    "fixture/SetTopicAttributes.yaml"

requestSubscribe :: Subscribe -> TestTree
requestSubscribe =
  req
    "Subscribe"
    "fixture/Subscribe.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumber -> TestTree
requestVerifySMSSandboxPhoneNumber =
  req
    "VerifySMSSandboxPhoneNumber"
    "fixture/VerifySMSSandboxPhoneNumber.yaml"

-- Responses

responseAddPermission :: AddPermissionResponse -> TestTree
responseAddPermission =
  res
    "AddPermissionResponse"
    "fixture/AddPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddPermission)

responseCheckIfPhoneNumberIsOptedOut :: CheckIfPhoneNumberIsOptedOutResponse -> TestTree
responseCheckIfPhoneNumberIsOptedOut =
  res
    "CheckIfPhoneNumberIsOptedOutResponse"
    "fixture/CheckIfPhoneNumberIsOptedOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckIfPhoneNumberIsOptedOut)

responseConfirmSubscription :: ConfirmSubscriptionResponse -> TestTree
responseConfirmSubscription =
  res
    "ConfirmSubscriptionResponse"
    "fixture/ConfirmSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ConfirmSubscription)

responseCreatePlatformApplication :: CreatePlatformApplicationResponse -> TestTree
responseCreatePlatformApplication =
  res
    "CreatePlatformApplicationResponse"
    "fixture/CreatePlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlatformApplication)

responseCreatePlatformEndpoint :: CreatePlatformEndpointResponse -> TestTree
responseCreatePlatformEndpoint =
  res
    "CreatePlatformEndpointResponse"
    "fixture/CreatePlatformEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlatformEndpoint)

responseCreateSMSSandboxPhoneNumber :: CreateSMSSandboxPhoneNumberResponse -> TestTree
responseCreateSMSSandboxPhoneNumber =
  res
    "CreateSMSSandboxPhoneNumberResponse"
    "fixture/CreateSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSMSSandboxPhoneNumber)

responseCreateTopic :: CreateTopicResponse -> TestTree
responseCreateTopic =
  res
    "CreateTopicResponse"
    "fixture/CreateTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateTopic)

responseDeleteEndpoint :: DeleteEndpointResponse -> TestTree
responseDeleteEndpoint =
  res
    "DeleteEndpointResponse"
    "fixture/DeleteEndpointResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEndpoint)

responseDeletePlatformApplication :: DeletePlatformApplicationResponse -> TestTree
responseDeletePlatformApplication =
  res
    "DeletePlatformApplicationResponse"
    "fixture/DeletePlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlatformApplication)

responseDeleteSMSSandboxPhoneNumber :: DeleteSMSSandboxPhoneNumberResponse -> TestTree
responseDeleteSMSSandboxPhoneNumber =
  res
    "DeleteSMSSandboxPhoneNumberResponse"
    "fixture/DeleteSMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSMSSandboxPhoneNumber)

responseDeleteTopic :: DeleteTopicResponse -> TestTree
responseDeleteTopic =
  res
    "DeleteTopicResponse"
    "fixture/DeleteTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTopic)

responseGetDataProtectionPolicy :: GetDataProtectionPolicyResponse -> TestTree
responseGetDataProtectionPolicy =
  res
    "GetDataProtectionPolicyResponse"
    "fixture/GetDataProtectionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDataProtectionPolicy)

responseGetEndpointAttributes :: GetEndpointAttributesResponse -> TestTree
responseGetEndpointAttributes =
  res
    "GetEndpointAttributesResponse"
    "fixture/GetEndpointAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEndpointAttributes)

responseGetPlatformApplicationAttributes :: GetPlatformApplicationAttributesResponse -> TestTree
responseGetPlatformApplicationAttributes =
  res
    "GetPlatformApplicationAttributesResponse"
    "fixture/GetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPlatformApplicationAttributes)

responseGetSMSAttributes :: GetSMSAttributesResponse -> TestTree
responseGetSMSAttributes =
  res
    "GetSMSAttributesResponse"
    "fixture/GetSMSAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSMSAttributes)

responseGetSMSSandboxAccountStatus :: GetSMSSandboxAccountStatusResponse -> TestTree
responseGetSMSSandboxAccountStatus =
  res
    "GetSMSSandboxAccountStatusResponse"
    "fixture/GetSMSSandboxAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSMSSandboxAccountStatus)

responseGetSubscriptionAttributes :: GetSubscriptionAttributesResponse -> TestTree
responseGetSubscriptionAttributes =
  res
    "GetSubscriptionAttributesResponse"
    "fixture/GetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionAttributes)

responseGetTopicAttributes :: GetTopicAttributesResponse -> TestTree
responseGetTopicAttributes =
  res
    "GetTopicAttributesResponse"
    "fixture/GetTopicAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetTopicAttributes)

responseListEndpointsByPlatformApplication :: ListEndpointsByPlatformApplicationResponse -> TestTree
responseListEndpointsByPlatformApplication =
  res
    "ListEndpointsByPlatformApplicationResponse"
    "fixture/ListEndpointsByPlatformApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEndpointsByPlatformApplication)

responseListOriginationNumbers :: ListOriginationNumbersResponse -> TestTree
responseListOriginationNumbers =
  res
    "ListOriginationNumbersResponse"
    "fixture/ListOriginationNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOriginationNumbers)

responseListPhoneNumbersOptedOut :: ListPhoneNumbersOptedOutResponse -> TestTree
responseListPhoneNumbersOptedOut =
  res
    "ListPhoneNumbersOptedOutResponse"
    "fixture/ListPhoneNumbersOptedOutResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPhoneNumbersOptedOut)

responseListPlatformApplications :: ListPlatformApplicationsResponse -> TestTree
responseListPlatformApplications =
  res
    "ListPlatformApplicationsResponse"
    "fixture/ListPlatformApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlatformApplications)

responseListSMSSandboxPhoneNumbers :: ListSMSSandboxPhoneNumbersResponse -> TestTree
responseListSMSSandboxPhoneNumbers =
  res
    "ListSMSSandboxPhoneNumbersResponse"
    "fixture/ListSMSSandboxPhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSMSSandboxPhoneNumbers)

responseListSubscriptions :: ListSubscriptionsResponse -> TestTree
responseListSubscriptions =
  res
    "ListSubscriptionsResponse"
    "fixture/ListSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptions)

responseListSubscriptionsByTopic :: ListSubscriptionsByTopicResponse -> TestTree
responseListSubscriptionsByTopic =
  res
    "ListSubscriptionsByTopicResponse"
    "fixture/ListSubscriptionsByTopicResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionsByTopic)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTopics :: ListTopicsResponse -> TestTree
responseListTopics =
  res
    "ListTopicsResponse"
    "fixture/ListTopicsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTopics)

responseOptInPhoneNumber :: OptInPhoneNumberResponse -> TestTree
responseOptInPhoneNumber =
  res
    "OptInPhoneNumberResponse"
    "fixture/OptInPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy OptInPhoneNumber)

responsePublish :: PublishResponse -> TestTree
responsePublish =
  res
    "PublishResponse"
    "fixture/PublishResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Publish)

responsePublishBatch :: PublishBatchResponse -> TestTree
responsePublishBatch =
  res
    "PublishBatchResponse"
    "fixture/PublishBatchResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PublishBatch)

responsePutDataProtectionPolicy :: PutDataProtectionPolicyResponse -> TestTree
responsePutDataProtectionPolicy =
  res
    "PutDataProtectionPolicyResponse"
    "fixture/PutDataProtectionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutDataProtectionPolicy)

responseRemovePermission :: RemovePermissionResponse -> TestTree
responseRemovePermission =
  res
    "RemovePermissionResponse"
    "fixture/RemovePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemovePermission)

responseSetEndpointAttributes :: SetEndpointAttributesResponse -> TestTree
responseSetEndpointAttributes =
  res
    "SetEndpointAttributesResponse"
    "fixture/SetEndpointAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetEndpointAttributes)

responseSetPlatformApplicationAttributes :: SetPlatformApplicationAttributesResponse -> TestTree
responseSetPlatformApplicationAttributes =
  res
    "SetPlatformApplicationAttributesResponse"
    "fixture/SetPlatformApplicationAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetPlatformApplicationAttributes)

responseSetSMSAttributes :: SetSMSAttributesResponse -> TestTree
responseSetSMSAttributes =
  res
    "SetSMSAttributesResponse"
    "fixture/SetSMSAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSMSAttributes)

responseSetSubscriptionAttributes :: SetSubscriptionAttributesResponse -> TestTree
responseSetSubscriptionAttributes =
  res
    "SetSubscriptionAttributesResponse"
    "fixture/SetSubscriptionAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetSubscriptionAttributes)

responseSetTopicAttributes :: SetTopicAttributesResponse -> TestTree
responseSetTopicAttributes =
  res
    "SetTopicAttributesResponse"
    "fixture/SetTopicAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTopicAttributes)

responseSubscribe :: SubscribeResponse -> TestTree
responseSubscribe =
  res
    "SubscribeResponse"
    "fixture/SubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Subscribe)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUnsubscribe :: UnsubscribeResponse -> TestTree
responseUnsubscribe =
  res
    "UnsubscribeResponse"
    "fixture/UnsubscribeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy Unsubscribe)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseVerifySMSSandboxPhoneNumber :: VerifySMSSandboxPhoneNumberResponse -> TestTree
responseVerifySMSSandboxPhoneNumber =
  res
    "VerifySMSSandboxPhoneNumberResponse"
    "fixture/VerifySMSSandboxPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy VerifySMSSandboxPhoneNumber)
