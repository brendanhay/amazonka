{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.PinpointSmsVoiceV2
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.PinpointSmsVoiceV2 where

import Amazonka.PinpointSmsVoiceV2
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.PinpointSmsVoiceV2.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateOriginationIdentity $
--             newAssociateOriginationIdentity
--
--         , requestCreateConfigurationSet $
--             newCreateConfigurationSet
--
--         , requestCreateEventDestination $
--             newCreateEventDestination
--
--         , requestCreateOptOutList $
--             newCreateOptOutList
--
--         , requestCreatePool $
--             newCreatePool
--
--         , requestDeleteConfigurationSet $
--             newDeleteConfigurationSet
--
--         , requestDeleteDefaultMessageType $
--             newDeleteDefaultMessageType
--
--         , requestDeleteDefaultSenderId $
--             newDeleteDefaultSenderId
--
--         , requestDeleteEventDestination $
--             newDeleteEventDestination
--
--         , requestDeleteKeyword $
--             newDeleteKeyword
--
--         , requestDeleteOptOutList $
--             newDeleteOptOutList
--
--         , requestDeleteOptedOutNumber $
--             newDeleteOptedOutNumber
--
--         , requestDeletePool $
--             newDeletePool
--
--         , requestDeleteTextMessageSpendLimitOverride $
--             newDeleteTextMessageSpendLimitOverride
--
--         , requestDeleteVoiceMessageSpendLimitOverride $
--             newDeleteVoiceMessageSpendLimitOverride
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeAccountLimits $
--             newDescribeAccountLimits
--
--         , requestDescribeConfigurationSets $
--             newDescribeConfigurationSets
--
--         , requestDescribeKeywords $
--             newDescribeKeywords
--
--         , requestDescribeOptOutLists $
--             newDescribeOptOutLists
--
--         , requestDescribeOptedOutNumbers $
--             newDescribeOptedOutNumbers
--
--         , requestDescribePhoneNumbers $
--             newDescribePhoneNumbers
--
--         , requestDescribePools $
--             newDescribePools
--
--         , requestDescribeSenderIds $
--             newDescribeSenderIds
--
--         , requestDescribeSpendLimits $
--             newDescribeSpendLimits
--
--         , requestDisassociateOriginationIdentity $
--             newDisassociateOriginationIdentity
--
--         , requestListPoolOriginationIdentities $
--             newListPoolOriginationIdentities
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutKeyword $
--             newPutKeyword
--
--         , requestPutOptedOutNumber $
--             newPutOptedOutNumber
--
--         , requestReleasePhoneNumber $
--             newReleasePhoneNumber
--
--         , requestRequestPhoneNumber $
--             newRequestPhoneNumber
--
--         , requestSendTextMessage $
--             newSendTextMessage
--
--         , requestSendVoiceMessage $
--             newSendVoiceMessage
--
--         , requestSetDefaultMessageType $
--             newSetDefaultMessageType
--
--         , requestSetDefaultSenderId $
--             newSetDefaultSenderId
--
--         , requestSetTextMessageSpendLimitOverride $
--             newSetTextMessageSpendLimitOverride
--
--         , requestSetVoiceMessageSpendLimitOverride $
--             newSetVoiceMessageSpendLimitOverride
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateEventDestination $
--             newUpdateEventDestination
--
--         , requestUpdatePhoneNumber $
--             newUpdatePhoneNumber
--
--         , requestUpdatePool $
--             newUpdatePool
--
--           ]

--     , testGroup "response"
--         [ responseAssociateOriginationIdentity $
--             newAssociateOriginationIdentityResponse
--
--         , responseCreateConfigurationSet $
--             newCreateConfigurationSetResponse
--
--         , responseCreateEventDestination $
--             newCreateEventDestinationResponse
--
--         , responseCreateOptOutList $
--             newCreateOptOutListResponse
--
--         , responseCreatePool $
--             newCreatePoolResponse
--
--         , responseDeleteConfigurationSet $
--             newDeleteConfigurationSetResponse
--
--         , responseDeleteDefaultMessageType $
--             newDeleteDefaultMessageTypeResponse
--
--         , responseDeleteDefaultSenderId $
--             newDeleteDefaultSenderIdResponse
--
--         , responseDeleteEventDestination $
--             newDeleteEventDestinationResponse
--
--         , responseDeleteKeyword $
--             newDeleteKeywordResponse
--
--         , responseDeleteOptOutList $
--             newDeleteOptOutListResponse
--
--         , responseDeleteOptedOutNumber $
--             newDeleteOptedOutNumberResponse
--
--         , responseDeletePool $
--             newDeletePoolResponse
--
--         , responseDeleteTextMessageSpendLimitOverride $
--             newDeleteTextMessageSpendLimitOverrideResponse
--
--         , responseDeleteVoiceMessageSpendLimitOverride $
--             newDeleteVoiceMessageSpendLimitOverrideResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeAccountLimits $
--             newDescribeAccountLimitsResponse
--
--         , responseDescribeConfigurationSets $
--             newDescribeConfigurationSetsResponse
--
--         , responseDescribeKeywords $
--             newDescribeKeywordsResponse
--
--         , responseDescribeOptOutLists $
--             newDescribeOptOutListsResponse
--
--         , responseDescribeOptedOutNumbers $
--             newDescribeOptedOutNumbersResponse
--
--         , responseDescribePhoneNumbers $
--             newDescribePhoneNumbersResponse
--
--         , responseDescribePools $
--             newDescribePoolsResponse
--
--         , responseDescribeSenderIds $
--             newDescribeSenderIdsResponse
--
--         , responseDescribeSpendLimits $
--             newDescribeSpendLimitsResponse
--
--         , responseDisassociateOriginationIdentity $
--             newDisassociateOriginationIdentityResponse
--
--         , responseListPoolOriginationIdentities $
--             newListPoolOriginationIdentitiesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutKeyword $
--             newPutKeywordResponse
--
--         , responsePutOptedOutNumber $
--             newPutOptedOutNumberResponse
--
--         , responseReleasePhoneNumber $
--             newReleasePhoneNumberResponse
--
--         , responseRequestPhoneNumber $
--             newRequestPhoneNumberResponse
--
--         , responseSendTextMessage $
--             newSendTextMessageResponse
--
--         , responseSendVoiceMessage $
--             newSendVoiceMessageResponse
--
--         , responseSetDefaultMessageType $
--             newSetDefaultMessageTypeResponse
--
--         , responseSetDefaultSenderId $
--             newSetDefaultSenderIdResponse
--
--         , responseSetTextMessageSpendLimitOverride $
--             newSetTextMessageSpendLimitOverrideResponse
--
--         , responseSetVoiceMessageSpendLimitOverride $
--             newSetVoiceMessageSpendLimitOverrideResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateEventDestination $
--             newUpdateEventDestinationResponse
--
--         , responseUpdatePhoneNumber $
--             newUpdatePhoneNumberResponse
--
--         , responseUpdatePool $
--             newUpdatePoolResponse
--
--           ]
--     ]

-- Requests

requestAssociateOriginationIdentity :: AssociateOriginationIdentity -> TestTree
requestAssociateOriginationIdentity =
  req
    "AssociateOriginationIdentity"
    "fixture/AssociateOriginationIdentity.yaml"

requestCreateConfigurationSet :: CreateConfigurationSet -> TestTree
requestCreateConfigurationSet =
  req
    "CreateConfigurationSet"
    "fixture/CreateConfigurationSet.yaml"

requestCreateEventDestination :: CreateEventDestination -> TestTree
requestCreateEventDestination =
  req
    "CreateEventDestination"
    "fixture/CreateEventDestination.yaml"

requestCreateOptOutList :: CreateOptOutList -> TestTree
requestCreateOptOutList =
  req
    "CreateOptOutList"
    "fixture/CreateOptOutList.yaml"

requestCreatePool :: CreatePool -> TestTree
requestCreatePool =
  req
    "CreatePool"
    "fixture/CreatePool.yaml"

requestDeleteConfigurationSet :: DeleteConfigurationSet -> TestTree
requestDeleteConfigurationSet =
  req
    "DeleteConfigurationSet"
    "fixture/DeleteConfigurationSet.yaml"

requestDeleteDefaultMessageType :: DeleteDefaultMessageType -> TestTree
requestDeleteDefaultMessageType =
  req
    "DeleteDefaultMessageType"
    "fixture/DeleteDefaultMessageType.yaml"

requestDeleteDefaultSenderId :: DeleteDefaultSenderId -> TestTree
requestDeleteDefaultSenderId =
  req
    "DeleteDefaultSenderId"
    "fixture/DeleteDefaultSenderId.yaml"

requestDeleteEventDestination :: DeleteEventDestination -> TestTree
requestDeleteEventDestination =
  req
    "DeleteEventDestination"
    "fixture/DeleteEventDestination.yaml"

requestDeleteKeyword :: DeleteKeyword -> TestTree
requestDeleteKeyword =
  req
    "DeleteKeyword"
    "fixture/DeleteKeyword.yaml"

requestDeleteOptOutList :: DeleteOptOutList -> TestTree
requestDeleteOptOutList =
  req
    "DeleteOptOutList"
    "fixture/DeleteOptOutList.yaml"

requestDeleteOptedOutNumber :: DeleteOptedOutNumber -> TestTree
requestDeleteOptedOutNumber =
  req
    "DeleteOptedOutNumber"
    "fixture/DeleteOptedOutNumber.yaml"

requestDeletePool :: DeletePool -> TestTree
requestDeletePool =
  req
    "DeletePool"
    "fixture/DeletePool.yaml"

requestDeleteTextMessageSpendLimitOverride :: DeleteTextMessageSpendLimitOverride -> TestTree
requestDeleteTextMessageSpendLimitOverride =
  req
    "DeleteTextMessageSpendLimitOverride"
    "fixture/DeleteTextMessageSpendLimitOverride.yaml"

requestDeleteVoiceMessageSpendLimitOverride :: DeleteVoiceMessageSpendLimitOverride -> TestTree
requestDeleteVoiceMessageSpendLimitOverride =
  req
    "DeleteVoiceMessageSpendLimitOverride"
    "fixture/DeleteVoiceMessageSpendLimitOverride.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeAccountLimits :: DescribeAccountLimits -> TestTree
requestDescribeAccountLimits =
  req
    "DescribeAccountLimits"
    "fixture/DescribeAccountLimits.yaml"

requestDescribeConfigurationSets :: DescribeConfigurationSets -> TestTree
requestDescribeConfigurationSets =
  req
    "DescribeConfigurationSets"
    "fixture/DescribeConfigurationSets.yaml"

requestDescribeKeywords :: DescribeKeywords -> TestTree
requestDescribeKeywords =
  req
    "DescribeKeywords"
    "fixture/DescribeKeywords.yaml"

requestDescribeOptOutLists :: DescribeOptOutLists -> TestTree
requestDescribeOptOutLists =
  req
    "DescribeOptOutLists"
    "fixture/DescribeOptOutLists.yaml"

requestDescribeOptedOutNumbers :: DescribeOptedOutNumbers -> TestTree
requestDescribeOptedOutNumbers =
  req
    "DescribeOptedOutNumbers"
    "fixture/DescribeOptedOutNumbers.yaml"

requestDescribePhoneNumbers :: DescribePhoneNumbers -> TestTree
requestDescribePhoneNumbers =
  req
    "DescribePhoneNumbers"
    "fixture/DescribePhoneNumbers.yaml"

requestDescribePools :: DescribePools -> TestTree
requestDescribePools =
  req
    "DescribePools"
    "fixture/DescribePools.yaml"

requestDescribeSenderIds :: DescribeSenderIds -> TestTree
requestDescribeSenderIds =
  req
    "DescribeSenderIds"
    "fixture/DescribeSenderIds.yaml"

requestDescribeSpendLimits :: DescribeSpendLimits -> TestTree
requestDescribeSpendLimits =
  req
    "DescribeSpendLimits"
    "fixture/DescribeSpendLimits.yaml"

requestDisassociateOriginationIdentity :: DisassociateOriginationIdentity -> TestTree
requestDisassociateOriginationIdentity =
  req
    "DisassociateOriginationIdentity"
    "fixture/DisassociateOriginationIdentity.yaml"

requestListPoolOriginationIdentities :: ListPoolOriginationIdentities -> TestTree
requestListPoolOriginationIdentities =
  req
    "ListPoolOriginationIdentities"
    "fixture/ListPoolOriginationIdentities.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutKeyword :: PutKeyword -> TestTree
requestPutKeyword =
  req
    "PutKeyword"
    "fixture/PutKeyword.yaml"

requestPutOptedOutNumber :: PutOptedOutNumber -> TestTree
requestPutOptedOutNumber =
  req
    "PutOptedOutNumber"
    "fixture/PutOptedOutNumber.yaml"

requestReleasePhoneNumber :: ReleasePhoneNumber -> TestTree
requestReleasePhoneNumber =
  req
    "ReleasePhoneNumber"
    "fixture/ReleasePhoneNumber.yaml"

requestRequestPhoneNumber :: RequestPhoneNumber -> TestTree
requestRequestPhoneNumber =
  req
    "RequestPhoneNumber"
    "fixture/RequestPhoneNumber.yaml"

requestSendTextMessage :: SendTextMessage -> TestTree
requestSendTextMessage =
  req
    "SendTextMessage"
    "fixture/SendTextMessage.yaml"

requestSendVoiceMessage :: SendVoiceMessage -> TestTree
requestSendVoiceMessage =
  req
    "SendVoiceMessage"
    "fixture/SendVoiceMessage.yaml"

requestSetDefaultMessageType :: SetDefaultMessageType -> TestTree
requestSetDefaultMessageType =
  req
    "SetDefaultMessageType"
    "fixture/SetDefaultMessageType.yaml"

requestSetDefaultSenderId :: SetDefaultSenderId -> TestTree
requestSetDefaultSenderId =
  req
    "SetDefaultSenderId"
    "fixture/SetDefaultSenderId.yaml"

requestSetTextMessageSpendLimitOverride :: SetTextMessageSpendLimitOverride -> TestTree
requestSetTextMessageSpendLimitOverride =
  req
    "SetTextMessageSpendLimitOverride"
    "fixture/SetTextMessageSpendLimitOverride.yaml"

requestSetVoiceMessageSpendLimitOverride :: SetVoiceMessageSpendLimitOverride -> TestTree
requestSetVoiceMessageSpendLimitOverride =
  req
    "SetVoiceMessageSpendLimitOverride"
    "fixture/SetVoiceMessageSpendLimitOverride.yaml"

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

requestUpdateEventDestination :: UpdateEventDestination -> TestTree
requestUpdateEventDestination =
  req
    "UpdateEventDestination"
    "fixture/UpdateEventDestination.yaml"

requestUpdatePhoneNumber :: UpdatePhoneNumber -> TestTree
requestUpdatePhoneNumber =
  req
    "UpdatePhoneNumber"
    "fixture/UpdatePhoneNumber.yaml"

requestUpdatePool :: UpdatePool -> TestTree
requestUpdatePool =
  req
    "UpdatePool"
    "fixture/UpdatePool.yaml"

-- Responses

responseAssociateOriginationIdentity :: AssociateOriginationIdentityResponse -> TestTree
responseAssociateOriginationIdentity =
  res
    "AssociateOriginationIdentityResponse"
    "fixture/AssociateOriginationIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateOriginationIdentity)

responseCreateConfigurationSet :: CreateConfigurationSetResponse -> TestTree
responseCreateConfigurationSet =
  res
    "CreateConfigurationSetResponse"
    "fixture/CreateConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationSet)

responseCreateEventDestination :: CreateEventDestinationResponse -> TestTree
responseCreateEventDestination =
  res
    "CreateEventDestinationResponse"
    "fixture/CreateEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventDestination)

responseCreateOptOutList :: CreateOptOutListResponse -> TestTree
responseCreateOptOutList =
  res
    "CreateOptOutListResponse"
    "fixture/CreateOptOutListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOptOutList)

responseCreatePool :: CreatePoolResponse -> TestTree
responseCreatePool =
  res
    "CreatePoolResponse"
    "fixture/CreatePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePool)

responseDeleteConfigurationSet :: DeleteConfigurationSetResponse -> TestTree
responseDeleteConfigurationSet =
  res
    "DeleteConfigurationSetResponse"
    "fixture/DeleteConfigurationSetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationSet)

responseDeleteDefaultMessageType :: DeleteDefaultMessageTypeResponse -> TestTree
responseDeleteDefaultMessageType =
  res
    "DeleteDefaultMessageTypeResponse"
    "fixture/DeleteDefaultMessageTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDefaultMessageType)

responseDeleteDefaultSenderId :: DeleteDefaultSenderIdResponse -> TestTree
responseDeleteDefaultSenderId =
  res
    "DeleteDefaultSenderIdResponse"
    "fixture/DeleteDefaultSenderIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDefaultSenderId)

responseDeleteEventDestination :: DeleteEventDestinationResponse -> TestTree
responseDeleteEventDestination =
  res
    "DeleteEventDestinationResponse"
    "fixture/DeleteEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventDestination)

responseDeleteKeyword :: DeleteKeywordResponse -> TestTree
responseDeleteKeyword =
  res
    "DeleteKeywordResponse"
    "fixture/DeleteKeywordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteKeyword)

responseDeleteOptOutList :: DeleteOptOutListResponse -> TestTree
responseDeleteOptOutList =
  res
    "DeleteOptOutListResponse"
    "fixture/DeleteOptOutListResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOptOutList)

responseDeleteOptedOutNumber :: DeleteOptedOutNumberResponse -> TestTree
responseDeleteOptedOutNumber =
  res
    "DeleteOptedOutNumberResponse"
    "fixture/DeleteOptedOutNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOptedOutNumber)

responseDeletePool :: DeletePoolResponse -> TestTree
responseDeletePool =
  res
    "DeletePoolResponse"
    "fixture/DeletePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePool)

responseDeleteTextMessageSpendLimitOverride :: DeleteTextMessageSpendLimitOverrideResponse -> TestTree
responseDeleteTextMessageSpendLimitOverride =
  res
    "DeleteTextMessageSpendLimitOverrideResponse"
    "fixture/DeleteTextMessageSpendLimitOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteTextMessageSpendLimitOverride)

responseDeleteVoiceMessageSpendLimitOverride :: DeleteVoiceMessageSpendLimitOverrideResponse -> TestTree
responseDeleteVoiceMessageSpendLimitOverride =
  res
    "DeleteVoiceMessageSpendLimitOverrideResponse"
    "fixture/DeleteVoiceMessageSpendLimitOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteVoiceMessageSpendLimitOverride)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeAccountLimits :: DescribeAccountLimitsResponse -> TestTree
responseDescribeAccountLimits =
  res
    "DescribeAccountLimitsResponse"
    "fixture/DescribeAccountLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountLimits)

responseDescribeConfigurationSets :: DescribeConfigurationSetsResponse -> TestTree
responseDescribeConfigurationSets =
  res
    "DescribeConfigurationSetsResponse"
    "fixture/DescribeConfigurationSetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationSets)

responseDescribeKeywords :: DescribeKeywordsResponse -> TestTree
responseDescribeKeywords =
  res
    "DescribeKeywordsResponse"
    "fixture/DescribeKeywordsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeKeywords)

responseDescribeOptOutLists :: DescribeOptOutListsResponse -> TestTree
responseDescribeOptOutLists =
  res
    "DescribeOptOutListsResponse"
    "fixture/DescribeOptOutListsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOptOutLists)

responseDescribeOptedOutNumbers :: DescribeOptedOutNumbersResponse -> TestTree
responseDescribeOptedOutNumbers =
  res
    "DescribeOptedOutNumbersResponse"
    "fixture/DescribeOptedOutNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOptedOutNumbers)

responseDescribePhoneNumbers :: DescribePhoneNumbersResponse -> TestTree
responseDescribePhoneNumbers =
  res
    "DescribePhoneNumbersResponse"
    "fixture/DescribePhoneNumbersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePhoneNumbers)

responseDescribePools :: DescribePoolsResponse -> TestTree
responseDescribePools =
  res
    "DescribePoolsResponse"
    "fixture/DescribePoolsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePools)

responseDescribeSenderIds :: DescribeSenderIdsResponse -> TestTree
responseDescribeSenderIds =
  res
    "DescribeSenderIdsResponse"
    "fixture/DescribeSenderIdsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSenderIds)

responseDescribeSpendLimits :: DescribeSpendLimitsResponse -> TestTree
responseDescribeSpendLimits =
  res
    "DescribeSpendLimitsResponse"
    "fixture/DescribeSpendLimitsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSpendLimits)

responseDisassociateOriginationIdentity :: DisassociateOriginationIdentityResponse -> TestTree
responseDisassociateOriginationIdentity =
  res
    "DisassociateOriginationIdentityResponse"
    "fixture/DisassociateOriginationIdentityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateOriginationIdentity)

responseListPoolOriginationIdentities :: ListPoolOriginationIdentitiesResponse -> TestTree
responseListPoolOriginationIdentities =
  res
    "ListPoolOriginationIdentitiesResponse"
    "fixture/ListPoolOriginationIdentitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPoolOriginationIdentities)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutKeyword :: PutKeywordResponse -> TestTree
responsePutKeyword =
  res
    "PutKeywordResponse"
    "fixture/PutKeywordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutKeyword)

responsePutOptedOutNumber :: PutOptedOutNumberResponse -> TestTree
responsePutOptedOutNumber =
  res
    "PutOptedOutNumberResponse"
    "fixture/PutOptedOutNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutOptedOutNumber)

responseReleasePhoneNumber :: ReleasePhoneNumberResponse -> TestTree
responseReleasePhoneNumber =
  res
    "ReleasePhoneNumberResponse"
    "fixture/ReleasePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ReleasePhoneNumber)

responseRequestPhoneNumber :: RequestPhoneNumberResponse -> TestTree
responseRequestPhoneNumber =
  res
    "RequestPhoneNumberResponse"
    "fixture/RequestPhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestPhoneNumber)

responseSendTextMessage :: SendTextMessageResponse -> TestTree
responseSendTextMessage =
  res
    "SendTextMessageResponse"
    "fixture/SendTextMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendTextMessage)

responseSendVoiceMessage :: SendVoiceMessageResponse -> TestTree
responseSendVoiceMessage =
  res
    "SendVoiceMessageResponse"
    "fixture/SendVoiceMessageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SendVoiceMessage)

responseSetDefaultMessageType :: SetDefaultMessageTypeResponse -> TestTree
responseSetDefaultMessageType =
  res
    "SetDefaultMessageTypeResponse"
    "fixture/SetDefaultMessageTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultMessageType)

responseSetDefaultSenderId :: SetDefaultSenderIdResponse -> TestTree
responseSetDefaultSenderId =
  res
    "SetDefaultSenderIdResponse"
    "fixture/SetDefaultSenderIdResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetDefaultSenderId)

responseSetTextMessageSpendLimitOverride :: SetTextMessageSpendLimitOverrideResponse -> TestTree
responseSetTextMessageSpendLimitOverride =
  res
    "SetTextMessageSpendLimitOverrideResponse"
    "fixture/SetTextMessageSpendLimitOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetTextMessageSpendLimitOverride)

responseSetVoiceMessageSpendLimitOverride :: SetVoiceMessageSpendLimitOverrideResponse -> TestTree
responseSetVoiceMessageSpendLimitOverride =
  res
    "SetVoiceMessageSpendLimitOverrideResponse"
    "fixture/SetVoiceMessageSpendLimitOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SetVoiceMessageSpendLimitOverride)

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

responseUpdateEventDestination :: UpdateEventDestinationResponse -> TestTree
responseUpdateEventDestination =
  res
    "UpdateEventDestinationResponse"
    "fixture/UpdateEventDestinationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEventDestination)

responseUpdatePhoneNumber :: UpdatePhoneNumberResponse -> TestTree
responseUpdatePhoneNumber =
  res
    "UpdatePhoneNumberResponse"
    "fixture/UpdatePhoneNumberResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePhoneNumber)

responseUpdatePool :: UpdatePoolResponse -> TestTree
responseUpdatePool =
  res
    "UpdatePoolResponse"
    "fixture/UpdatePoolResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePool)
