{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.SSMContacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.SSMContacts where

import Amazonka.SSMContacts
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.SSMContacts.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptPage $
--             newAcceptPage
--
--         , requestActivateContactChannel $
--             newActivateContactChannel
--
--         , requestCreateContact $
--             newCreateContact
--
--         , requestCreateContactChannel $
--             newCreateContactChannel
--
--         , requestDeactivateContactChannel $
--             newDeactivateContactChannel
--
--         , requestDeleteContact $
--             newDeleteContact
--
--         , requestDeleteContactChannel $
--             newDeleteContactChannel
--
--         , requestDescribeEngagement $
--             newDescribeEngagement
--
--         , requestDescribePage $
--             newDescribePage
--
--         , requestGetContact $
--             newGetContact
--
--         , requestGetContactChannel $
--             newGetContactChannel
--
--         , requestGetContactPolicy $
--             newGetContactPolicy
--
--         , requestListContactChannels $
--             newListContactChannels
--
--         , requestListContacts $
--             newListContacts
--
--         , requestListEngagements $
--             newListEngagements
--
--         , requestListPageReceipts $
--             newListPageReceipts
--
--         , requestListPagesByContact $
--             newListPagesByContact
--
--         , requestListPagesByEngagement $
--             newListPagesByEngagement
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutContactPolicy $
--             newPutContactPolicy
--
--         , requestSendActivationCode $
--             newSendActivationCode
--
--         , requestStartEngagement $
--             newStartEngagement
--
--         , requestStopEngagement $
--             newStopEngagement
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateContact $
--             newUpdateContact
--
--         , requestUpdateContactChannel $
--             newUpdateContactChannel
--
--           ]

--     , testGroup "response"
--         [ responseAcceptPage $
--             newAcceptPageResponse
--
--         , responseActivateContactChannel $
--             newActivateContactChannelResponse
--
--         , responseCreateContact $
--             newCreateContactResponse
--
--         , responseCreateContactChannel $
--             newCreateContactChannelResponse
--
--         , responseDeactivateContactChannel $
--             newDeactivateContactChannelResponse
--
--         , responseDeleteContact $
--             newDeleteContactResponse
--
--         , responseDeleteContactChannel $
--             newDeleteContactChannelResponse
--
--         , responseDescribeEngagement $
--             newDescribeEngagementResponse
--
--         , responseDescribePage $
--             newDescribePageResponse
--
--         , responseGetContact $
--             newGetContactResponse
--
--         , responseGetContactChannel $
--             newGetContactChannelResponse
--
--         , responseGetContactPolicy $
--             newGetContactPolicyResponse
--
--         , responseListContactChannels $
--             newListContactChannelsResponse
--
--         , responseListContacts $
--             newListContactsResponse
--
--         , responseListEngagements $
--             newListEngagementsResponse
--
--         , responseListPageReceipts $
--             newListPageReceiptsResponse
--
--         , responseListPagesByContact $
--             newListPagesByContactResponse
--
--         , responseListPagesByEngagement $
--             newListPagesByEngagementResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutContactPolicy $
--             newPutContactPolicyResponse
--
--         , responseSendActivationCode $
--             newSendActivationCodeResponse
--
--         , responseStartEngagement $
--             newStartEngagementResponse
--
--         , responseStopEngagement $
--             newStopEngagementResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateContact $
--             newUpdateContactResponse
--
--         , responseUpdateContactChannel $
--             newUpdateContactChannelResponse
--
--           ]
--     ]

-- Requests

requestAcceptPage :: AcceptPage -> TestTree
requestAcceptPage =
  req
    "AcceptPage"
    "fixture/AcceptPage.yaml"

requestActivateContactChannel :: ActivateContactChannel -> TestTree
requestActivateContactChannel =
  req
    "ActivateContactChannel"
    "fixture/ActivateContactChannel.yaml"

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

requestDeactivateContactChannel :: DeactivateContactChannel -> TestTree
requestDeactivateContactChannel =
  req
    "DeactivateContactChannel"
    "fixture/DeactivateContactChannel.yaml"

requestDeleteContact :: DeleteContact -> TestTree
requestDeleteContact =
  req
    "DeleteContact"
    "fixture/DeleteContact.yaml"

requestDeleteContactChannel :: DeleteContactChannel -> TestTree
requestDeleteContactChannel =
  req
    "DeleteContactChannel"
    "fixture/DeleteContactChannel.yaml"

requestDescribeEngagement :: DescribeEngagement -> TestTree
requestDescribeEngagement =
  req
    "DescribeEngagement"
    "fixture/DescribeEngagement.yaml"

requestDescribePage :: DescribePage -> TestTree
requestDescribePage =
  req
    "DescribePage"
    "fixture/DescribePage.yaml"

requestGetContact :: GetContact -> TestTree
requestGetContact =
  req
    "GetContact"
    "fixture/GetContact.yaml"

requestGetContactChannel :: GetContactChannel -> TestTree
requestGetContactChannel =
  req
    "GetContactChannel"
    "fixture/GetContactChannel.yaml"

requestGetContactPolicy :: GetContactPolicy -> TestTree
requestGetContactPolicy =
  req
    "GetContactPolicy"
    "fixture/GetContactPolicy.yaml"

requestListContactChannels :: ListContactChannels -> TestTree
requestListContactChannels =
  req
    "ListContactChannels"
    "fixture/ListContactChannels.yaml"

requestListContacts :: ListContacts -> TestTree
requestListContacts =
  req
    "ListContacts"
    "fixture/ListContacts.yaml"

requestListEngagements :: ListEngagements -> TestTree
requestListEngagements =
  req
    "ListEngagements"
    "fixture/ListEngagements.yaml"

requestListPageReceipts :: ListPageReceipts -> TestTree
requestListPageReceipts =
  req
    "ListPageReceipts"
    "fixture/ListPageReceipts.yaml"

requestListPagesByContact :: ListPagesByContact -> TestTree
requestListPagesByContact =
  req
    "ListPagesByContact"
    "fixture/ListPagesByContact.yaml"

requestListPagesByEngagement :: ListPagesByEngagement -> TestTree
requestListPagesByEngagement =
  req
    "ListPagesByEngagement"
    "fixture/ListPagesByEngagement.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutContactPolicy :: PutContactPolicy -> TestTree
requestPutContactPolicy =
  req
    "PutContactPolicy"
    "fixture/PutContactPolicy.yaml"

requestSendActivationCode :: SendActivationCode -> TestTree
requestSendActivationCode =
  req
    "SendActivationCode"
    "fixture/SendActivationCode.yaml"

requestStartEngagement :: StartEngagement -> TestTree
requestStartEngagement =
  req
    "StartEngagement"
    "fixture/StartEngagement.yaml"

requestStopEngagement :: StopEngagement -> TestTree
requestStopEngagement =
  req
    "StopEngagement"
    "fixture/StopEngagement.yaml"

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

requestUpdateContact :: UpdateContact -> TestTree
requestUpdateContact =
  req
    "UpdateContact"
    "fixture/UpdateContact.yaml"

requestUpdateContactChannel :: UpdateContactChannel -> TestTree
requestUpdateContactChannel =
  req
    "UpdateContactChannel"
    "fixture/UpdateContactChannel.yaml"

-- Responses

responseAcceptPage :: AcceptPageResponse -> TestTree
responseAcceptPage =
  res
    "AcceptPageResponse"
    "fixture/AcceptPageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptPage)

responseActivateContactChannel :: ActivateContactChannelResponse -> TestTree
responseActivateContactChannel =
  res
    "ActivateContactChannelResponse"
    "fixture/ActivateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ActivateContactChannel)

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

responseDeactivateContactChannel :: DeactivateContactChannelResponse -> TestTree
responseDeactivateContactChannel =
  res
    "DeactivateContactChannelResponse"
    "fixture/DeactivateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeactivateContactChannel)

responseDeleteContact :: DeleteContactResponse -> TestTree
responseDeleteContact =
  res
    "DeleteContactResponse"
    "fixture/DeleteContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContact)

responseDeleteContactChannel :: DeleteContactChannelResponse -> TestTree
responseDeleteContactChannel =
  res
    "DeleteContactChannelResponse"
    "fixture/DeleteContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteContactChannel)

responseDescribeEngagement :: DescribeEngagementResponse -> TestTree
responseDescribeEngagement =
  res
    "DescribeEngagementResponse"
    "fixture/DescribeEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEngagement)

responseDescribePage :: DescribePageResponse -> TestTree
responseDescribePage =
  res
    "DescribePageResponse"
    "fixture/DescribePageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePage)

responseGetContact :: GetContactResponse -> TestTree
responseGetContact =
  res
    "GetContactResponse"
    "fixture/GetContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContact)

responseGetContactChannel :: GetContactChannelResponse -> TestTree
responseGetContactChannel =
  res
    "GetContactChannelResponse"
    "fixture/GetContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactChannel)

responseGetContactPolicy :: GetContactPolicyResponse -> TestTree
responseGetContactPolicy =
  res
    "GetContactPolicyResponse"
    "fixture/GetContactPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetContactPolicy)

responseListContactChannels :: ListContactChannelsResponse -> TestTree
responseListContactChannels =
  res
    "ListContactChannelsResponse"
    "fixture/ListContactChannelsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContactChannels)

responseListContacts :: ListContactsResponse -> TestTree
responseListContacts =
  res
    "ListContactsResponse"
    "fixture/ListContactsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListContacts)

responseListEngagements :: ListEngagementsResponse -> TestTree
responseListEngagements =
  res
    "ListEngagementsResponse"
    "fixture/ListEngagementsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEngagements)

responseListPageReceipts :: ListPageReceiptsResponse -> TestTree
responseListPageReceipts =
  res
    "ListPageReceiptsResponse"
    "fixture/ListPageReceiptsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPageReceipts)

responseListPagesByContact :: ListPagesByContactResponse -> TestTree
responseListPagesByContact =
  res
    "ListPagesByContactResponse"
    "fixture/ListPagesByContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPagesByContact)

responseListPagesByEngagement :: ListPagesByEngagementResponse -> TestTree
responseListPagesByEngagement =
  res
    "ListPagesByEngagementResponse"
    "fixture/ListPagesByEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPagesByEngagement)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutContactPolicy :: PutContactPolicyResponse -> TestTree
responsePutContactPolicy =
  res
    "PutContactPolicyResponse"
    "fixture/PutContactPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutContactPolicy)

responseSendActivationCode :: SendActivationCodeResponse -> TestTree
responseSendActivationCode =
  res
    "SendActivationCodeResponse"
    "fixture/SendActivationCodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendActivationCode)

responseStartEngagement :: StartEngagementResponse -> TestTree
responseStartEngagement =
  res
    "StartEngagementResponse"
    "fixture/StartEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartEngagement)

responseStopEngagement :: StopEngagementResponse -> TestTree
responseStopEngagement =
  res
    "StopEngagementResponse"
    "fixture/StopEngagementResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopEngagement)

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

responseUpdateContact :: UpdateContactResponse -> TestTree
responseUpdateContact =
  res
    "UpdateContactResponse"
    "fixture/UpdateContactResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContact)

responseUpdateContactChannel :: UpdateContactChannelResponse -> TestTree
responseUpdateContactChannel =
  res
    "UpdateContactChannelResponse"
    "fixture/UpdateContactChannelResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateContactChannel)
