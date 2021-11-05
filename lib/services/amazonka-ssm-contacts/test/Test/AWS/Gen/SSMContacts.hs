{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.SSMContacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.SSMContacts where

import Amazonka.SSMContacts
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.SSMContacts.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListPagesByEngagement $
--             newListPagesByEngagement
--
--         , requestListEngagements $
--             newListEngagements
--
--         , requestListContactChannels $
--             newListContactChannels
--
--         , requestActivateContactChannel $
--             newActivateContactChannel
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestStartEngagement $
--             newStartEngagement
--
--         , requestDeactivateContactChannel $
--             newDeactivateContactChannel
--
--         , requestAcceptPage $
--             newAcceptPage
--
--         , requestListPageReceipts $
--             newListPageReceipts
--
--         , requestGetContact $
--             newGetContact
--
--         , requestDescribePage $
--             newDescribePage
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestCreateContactChannel $
--             newCreateContactChannel
--
--         , requestDeleteContactChannel $
--             newDeleteContactChannel
--
--         , requestUpdateContactChannel $
--             newUpdateContactChannel
--
--         , requestGetContactChannel $
--             newGetContactChannel
--
--         , requestSendActivationCode $
--             newSendActivationCode
--
--         , requestStopEngagement $
--             newStopEngagement
--
--         , requestDescribeEngagement $
--             newDescribeEngagement
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetContactPolicy $
--             newGetContactPolicy
--
--         , requestPutContactPolicy $
--             newPutContactPolicy
--
--         , requestListContacts $
--             newListContacts
--
--         , requestListPagesByContact $
--             newListPagesByContact
--
--           ]

--     , testGroup "response"
--         [ responseListPagesByEngagement $
--             newListPagesByEngagementResponse
--
--         , responseListEngagements $
--             newListEngagementsResponse
--
--         , responseListContactChannels $
--             newListContactChannelsResponse
--
--         , responseActivateContactChannel $
--             newActivateContactChannelResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseStartEngagement $
--             newStartEngagementResponse
--
--         , responseDeactivateContactChannel $
--             newDeactivateContactChannelResponse
--
--         , responseAcceptPage $
--             newAcceptPageResponse
--
--         , responseListPageReceipts $
--             newListPageReceiptsResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseDescribePage $
--             newDescribePageResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseCreateContactChannel $
--             newCreateContactChannelResponse
--
--         , responseDeleteContactChannel $
--             newDeleteContactChannelResponse
--
--         , responseUpdateContactChannel $
--             newUpdateContactChannelResponse
--
--         , responseGetContactChannel $
--             newGetContactChannelResponse
--
--         , responseSendActivationCode $
--             newSendActivationCodeResponse
--
--         , responseStopEngagement $
--             newStopEngagementResponse
--
--         , responseDescribeEngagement $
--             newDescribeEngagementResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetContactPolicy $
--             newGetContactPolicyResponse
--
--         , responsePutContactPolicy $
--             newPutContactPolicyResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--         , responseListPagesByContact $
--             newListPagesByContactResponse
--
--           ]
--     ]

-- Requests

requestListPagesByEngagement :: ListPagesByEngagement -> TestTree
requestListPagesByEngagement =
  req
    "ListPagesByEngagement"
    "fixture/ListPagesByEngagement.yaml"

requestListEngagements :: ListEngagements -> TestTree
requestListEngagements =
  req
    "ListEngagements"
    "fixture/ListEngagements.yaml"

requestListContactChannels :: ListContactChannels -> TestTree
requestListContactChannels =
  req
    "ListContactChannels"
    "fixture/ListContactChannels.yaml"

requestActivateContactChannel :: ActivateContactChannel -> TestTree
requestActivateContactChannel =
  req
    "ActivateContactChannel"
    "fixture/ActivateContactChannel.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestStartEngagement :: StartEngagement -> TestTree
requestStartEngagement =
  req
    "StartEngagement"
    "fixture/StartEngagement.yaml"

requestDeactivateContactChannel :: DeactivateContactChannel -> TestTree
requestDeactivateContactChannel =
  req
    "DeactivateContactChannel"
    "fixture/DeactivateContactChannel.yaml"

requestAcceptPage :: AcceptPage -> TestTree
requestAcceptPage =
  req
    "AcceptPage"
    "fixture/AcceptPage.yaml"

requestListPageReceipts :: ListPageReceipts -> TestTree
requestListPageReceipts =
  req
    "ListPageReceipts"
    "fixture/ListPageReceipts.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestDescribePage :: DescribePage -> TestTree
requestDescribePage =
  req
    "DescribePage"
    "fixture/DescribePage.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestCreateContact :: CreateContact -> TestTree
requestCreateContact =
  req
    "CreateContact"
    "fixture/CreateContact.yaml"

requestCreateContactChannel :: CreateContactChannel -> TestTree
requestCreateContactChannel =
  req
    "CreateContactChannel"
    "fixture/CreateContactChannel.yaml"

requestDeleteContactChannel :: DeleteContactChannel -> TestTree
requestDeleteContactChannel =
  req
    "DeleteContactChannel"
    "fixture/DeleteContactChannel.yaml"

requestUpdateContactChannel :: UpdateContactChannel -> TestTree
requestUpdateContactChannel =
  req
    "UpdateContactChannel"
    "fixture/UpdateContactChannel.yaml"

requestGetContactChannel :: GetContactChannel -> TestTree
requestGetContactChannel =
  req
    "GetContactChannel"
    "fixture/GetContactChannel.yaml"

requestSendActivationCode :: SendActivationCode -> TestTree
requestSendActivationCode =
  req
    "SendActivationCode"
    "fixture/SendActivationCode.yaml"

requestStopEngagement :: StopEngagement -> TestTree
requestStopEngagement =
  req
    "StopEngagement"
    "fixture/StopEngagement.yaml"

requestDescribeEngagement :: DescribeEngagement -> TestTree
requestDescribeEngagement =
  req
    "DescribeEngagement"
    "fixture/DescribeEngagement.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetContactPolicy :: GetContactPolicy -> TestTree
requestGetContactPolicy =
  req
    "GetContactPolicy"
    "fixture/GetContactPolicy.yaml"

requestPutContactPolicy :: PutContactPolicy -> TestTree
requestPutContactPolicy =
  req
    "PutContactPolicy"
    "fixture/PutContactPolicy.yaml"

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

requestListPagesByContact :: ListPagesByContact -> TestTree
requestListPagesByContact =
  req
    "ListPagesByContact"
    "fixture/ListPagesByContact.yaml"

-- Responses

responseListPagesByEngagement :: ListPagesByEngagementResponse -> TestTree
responseListPagesByEngagement =
  res
    "ListPagesByEngagementResponse"
    "fixture/ListPagesByEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPagesByEngagement)

responseListEngagements :: ListEngagementsResponse -> TestTree
responseListEngagements =
  res
    "ListEngagementsResponse"
    "fixture/ListEngagementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEngagements)

responseListContactChannels :: ListContactChannelsResponse -> TestTree
responseListContactChannels =
  res
    "ListContactChannelsResponse"
    "fixture/ListContactChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactChannels)

responseActivateContactChannel :: ActivateContactChannelResponse -> TestTree
responseActivateContactChannel =
  res
    "ActivateContactChannelResponse"
    "fixture/ActivateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateContactChannel)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartEngagement :: StartEngagementResponse -> TestTree
responseStartEngagement =
  res
    "StartEngagementResponse"
    "fixture/StartEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEngagement)

responseDeactivateContactChannel :: DeactivateContactChannelResponse -> TestTree
responseDeactivateContactChannel =
  res
    "DeactivateContactChannelResponse"
    "fixture/DeactivateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateContactChannel)

responseAcceptPage :: AcceptPageResponse -> TestTree
responseAcceptPage =
  res
    "AcceptPageResponse"
    "fixture/AcceptPageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptPage)

responseListPageReceipts :: ListPageReceiptsResponse -> TestTree
responseListPageReceipts =
  res
    "ListPageReceiptsResponse"
    "fixture/ListPageReceiptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPageReceipts)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContact)

responseDescribePage :: DescribePageResponse -> TestTree
responseDescribePage =
  res
    "DescribePageResponse"
    "fixture/DescribePageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePage)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContact)

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContact)

responseCreateContact :: CreateContactResponse -> TestTree
responseCreateContact =
  res
    "CreateContactResponse"
    "fixture/CreateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContact)

responseCreateContactChannel :: CreateContactChannelResponse -> TestTree
responseCreateContactChannel =
  res
    "CreateContactChannelResponse"
    "fixture/CreateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateContactChannel)

responseDeleteContactChannel :: DeleteContactChannelResponse -> TestTree
responseDeleteContactChannel =
  res
    "DeleteContactChannelResponse"
    "fixture/DeleteContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactChannel)

responseUpdateContactChannel :: UpdateContactChannelResponse -> TestTree
responseUpdateContactChannel =
  res
    "UpdateContactChannelResponse"
    "fixture/UpdateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactChannel)

responseGetContactChannel :: GetContactChannelResponse -> TestTree
responseGetContactChannel =
  res
    "GetContactChannelResponse"
    "fixture/GetContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactChannel)

responseSendActivationCode :: SendActivationCodeResponse -> TestTree
responseSendActivationCode =
  res
    "SendActivationCodeResponse"
    "fixture/SendActivationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendActivationCode)

responseStopEngagement :: StopEngagementResponse -> TestTree
responseStopEngagement =
  res
    "StopEngagementResponse"
    "fixture/StopEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEngagement)

responseDescribeEngagement :: DescribeEngagementResponse -> TestTree
responseDescribeEngagement =
  res
    "DescribeEngagementResponse"
    "fixture/DescribeEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngagement)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseGetContactPolicy :: GetContactPolicyResponse -> TestTree
responseGetContactPolicy =
  res
    "GetContactPolicyResponse"
    "fixture/GetContactPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactPolicy)

responsePutContactPolicy :: PutContactPolicyResponse -> TestTree
responsePutContactPolicy =
  res
    "PutContactPolicyResponse"
    "fixture/PutContactPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutContactPolicy)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContacts)

responseListPagesByContact :: ListPagesByContactResponse -> TestTree
responseListPagesByContact =
  res
    "ListPagesByContactResponse"
    "fixture/ListPagesByContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPagesByContact)
