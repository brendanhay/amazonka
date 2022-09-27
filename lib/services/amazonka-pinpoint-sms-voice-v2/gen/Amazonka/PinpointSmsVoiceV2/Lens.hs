{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Lens
  ( -- * Operations

    -- ** AssociateOriginationIdentity
    associateOriginationIdentity_clientToken,
    associateOriginationIdentity_poolId,
    associateOriginationIdentity_originationIdentity,
    associateOriginationIdentity_isoCountryCode,
    associateOriginationIdentityResponse_isoCountryCode,
    associateOriginationIdentityResponse_originationIdentity,
    associateOriginationIdentityResponse_poolArn,
    associateOriginationIdentityResponse_originationIdentityArn,
    associateOriginationIdentityResponse_poolId,
    associateOriginationIdentityResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_tags,
    createConfigurationSet_clientToken,
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_tags,
    createConfigurationSetResponse_createdTimestamp,
    createConfigurationSetResponse_configurationSetName,
    createConfigurationSetResponse_configurationSetArn,
    createConfigurationSetResponse_httpStatus,

    -- ** CreateEventDestination
    createEventDestination_clientToken,
    createEventDestination_cloudWatchLogsDestination,
    createEventDestination_snsDestination,
    createEventDestination_kinesisFirehoseDestination,
    createEventDestination_configurationSetName,
    createEventDestination_eventDestinationName,
    createEventDestination_matchingEventTypes,
    createEventDestinationResponse_configurationSetName,
    createEventDestinationResponse_eventDestination,
    createEventDestinationResponse_configurationSetArn,
    createEventDestinationResponse_httpStatus,

    -- ** CreateOptOutList
    createOptOutList_tags,
    createOptOutList_clientToken,
    createOptOutList_optOutListName,
    createOptOutListResponse_tags,
    createOptOutListResponse_optOutListArn,
    createOptOutListResponse_createdTimestamp,
    createOptOutListResponse_optOutListName,
    createOptOutListResponse_httpStatus,

    -- ** CreatePool
    createPool_deletionProtectionEnabled,
    createPool_tags,
    createPool_clientToken,
    createPool_originationIdentity,
    createPool_isoCountryCode,
    createPool_messageType,
    createPoolResponse_deletionProtectionEnabled,
    createPoolResponse_tags,
    createPoolResponse_poolArn,
    createPoolResponse_messageType,
    createPoolResponse_selfManagedOptOutsEnabled,
    createPoolResponse_createdTimestamp,
    createPoolResponse_status,
    createPoolResponse_twoWayEnabled,
    createPoolResponse_optOutListName,
    createPoolResponse_poolId,
    createPoolResponse_twoWayChannelArn,
    createPoolResponse_sharedRoutesEnabled,
    createPoolResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_createdTimestamp,
    deleteConfigurationSetResponse_configurationSetName,
    deleteConfigurationSetResponse_defaultSenderId,
    deleteConfigurationSetResponse_configurationSetArn,
    deleteConfigurationSetResponse_defaultMessageType,
    deleteConfigurationSetResponse_eventDestinations,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteDefaultMessageType
    deleteDefaultMessageType_configurationSetName,
    deleteDefaultMessageTypeResponse_messageType,
    deleteDefaultMessageTypeResponse_configurationSetName,
    deleteDefaultMessageTypeResponse_configurationSetArn,
    deleteDefaultMessageTypeResponse_httpStatus,

    -- ** DeleteDefaultSenderId
    deleteDefaultSenderId_configurationSetName,
    deleteDefaultSenderIdResponse_senderId,
    deleteDefaultSenderIdResponse_configurationSetName,
    deleteDefaultSenderIdResponse_configurationSetArn,
    deleteDefaultSenderIdResponse_httpStatus,

    -- ** DeleteEventDestination
    deleteEventDestination_configurationSetName,
    deleteEventDestination_eventDestinationName,
    deleteEventDestinationResponse_configurationSetName,
    deleteEventDestinationResponse_eventDestination,
    deleteEventDestinationResponse_configurationSetArn,
    deleteEventDestinationResponse_httpStatus,

    -- ** DeleteKeyword
    deleteKeyword_originationIdentity,
    deleteKeyword_keyword,
    deleteKeywordResponse_originationIdentity,
    deleteKeywordResponse_keywordAction,
    deleteKeywordResponse_originationIdentityArn,
    deleteKeywordResponse_keyword,
    deleteKeywordResponse_keywordMessage,
    deleteKeywordResponse_httpStatus,

    -- ** DeleteOptOutList
    deleteOptOutList_optOutListName,
    deleteOptOutListResponse_optOutListArn,
    deleteOptOutListResponse_createdTimestamp,
    deleteOptOutListResponse_optOutListName,
    deleteOptOutListResponse_httpStatus,

    -- ** DeleteOptedOutNumber
    deleteOptedOutNumber_optOutListName,
    deleteOptedOutNumber_optedOutNumber,
    deleteOptedOutNumberResponse_optOutListArn,
    deleteOptedOutNumberResponse_optedOutTimestamp,
    deleteOptedOutNumberResponse_optOutListName,
    deleteOptedOutNumberResponse_optedOutNumber,
    deleteOptedOutNumberResponse_endUserOptedOut,
    deleteOptedOutNumberResponse_httpStatus,

    -- ** DeletePool
    deletePool_poolId,
    deletePoolResponse_poolArn,
    deletePoolResponse_messageType,
    deletePoolResponse_selfManagedOptOutsEnabled,
    deletePoolResponse_createdTimestamp,
    deletePoolResponse_status,
    deletePoolResponse_twoWayEnabled,
    deletePoolResponse_optOutListName,
    deletePoolResponse_poolId,
    deletePoolResponse_twoWayChannelArn,
    deletePoolResponse_sharedRoutesEnabled,
    deletePoolResponse_httpStatus,

    -- ** DeleteTextMessageSpendLimitOverride
    deleteTextMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteTextMessageSpendLimitOverrideResponse_httpStatus,

    -- ** DeleteVoiceMessageSpendLimitOverride
    deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteVoiceMessageSpendLimitOverrideResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_nextToken,
    describeAccountAttributes_maxResults,
    describeAccountAttributesResponse_nextToken,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_nextToken,
    describeAccountLimits_maxResults,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeConfigurationSets
    describeConfigurationSets_nextToken,
    describeConfigurationSets_configurationSetNames,
    describeConfigurationSets_filters,
    describeConfigurationSets_maxResults,
    describeConfigurationSetsResponse_nextToken,
    describeConfigurationSetsResponse_configurationSets,
    describeConfigurationSetsResponse_httpStatus,

    -- ** DescribeKeywords
    describeKeywords_nextToken,
    describeKeywords_filters,
    describeKeywords_keywords,
    describeKeywords_maxResults,
    describeKeywords_originationIdentity,
    describeKeywordsResponse_originationIdentity,
    describeKeywordsResponse_nextToken,
    describeKeywordsResponse_originationIdentityArn,
    describeKeywordsResponse_keywords,
    describeKeywordsResponse_httpStatus,

    -- ** DescribeOptOutLists
    describeOptOutLists_nextToken,
    describeOptOutLists_optOutListNames,
    describeOptOutLists_maxResults,
    describeOptOutListsResponse_nextToken,
    describeOptOutListsResponse_optOutLists,
    describeOptOutListsResponse_httpStatus,

    -- ** DescribeOptedOutNumbers
    describeOptedOutNumbers_nextToken,
    describeOptedOutNumbers_optedOutNumbers,
    describeOptedOutNumbers_filters,
    describeOptedOutNumbers_maxResults,
    describeOptedOutNumbers_optOutListName,
    describeOptedOutNumbersResponse_nextToken,
    describeOptedOutNumbersResponse_optOutListArn,
    describeOptedOutNumbersResponse_optedOutNumbers,
    describeOptedOutNumbersResponse_optOutListName,
    describeOptedOutNumbersResponse_httpStatus,

    -- ** DescribePhoneNumbers
    describePhoneNumbers_nextToken,
    describePhoneNumbers_phoneNumberIds,
    describePhoneNumbers_filters,
    describePhoneNumbers_maxResults,
    describePhoneNumbersResponse_nextToken,
    describePhoneNumbersResponse_phoneNumbers,
    describePhoneNumbersResponse_httpStatus,

    -- ** DescribePools
    describePools_nextToken,
    describePools_poolIds,
    describePools_filters,
    describePools_maxResults,
    describePoolsResponse_nextToken,
    describePoolsResponse_pools,
    describePoolsResponse_httpStatus,

    -- ** DescribeSenderIds
    describeSenderIds_nextToken,
    describeSenderIds_filters,
    describeSenderIds_senderIds,
    describeSenderIds_maxResults,
    describeSenderIdsResponse_nextToken,
    describeSenderIdsResponse_senderIds,
    describeSenderIdsResponse_httpStatus,

    -- ** DescribeSpendLimits
    describeSpendLimits_nextToken,
    describeSpendLimits_maxResults,
    describeSpendLimitsResponse_nextToken,
    describeSpendLimitsResponse_spendLimits,
    describeSpendLimitsResponse_httpStatus,

    -- ** DisassociateOriginationIdentity
    disassociateOriginationIdentity_clientToken,
    disassociateOriginationIdentity_poolId,
    disassociateOriginationIdentity_originationIdentity,
    disassociateOriginationIdentity_isoCountryCode,
    disassociateOriginationIdentityResponse_isoCountryCode,
    disassociateOriginationIdentityResponse_originationIdentity,
    disassociateOriginationIdentityResponse_poolArn,
    disassociateOriginationIdentityResponse_originationIdentityArn,
    disassociateOriginationIdentityResponse_poolId,
    disassociateOriginationIdentityResponse_httpStatus,

    -- ** ListPoolOriginationIdentities
    listPoolOriginationIdentities_nextToken,
    listPoolOriginationIdentities_filters,
    listPoolOriginationIdentities_maxResults,
    listPoolOriginationIdentities_poolId,
    listPoolOriginationIdentitiesResponse_nextToken,
    listPoolOriginationIdentitiesResponse_poolArn,
    listPoolOriginationIdentitiesResponse_poolId,
    listPoolOriginationIdentitiesResponse_originationIdentities,
    listPoolOriginationIdentitiesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_httpStatus,

    -- ** PutKeyword
    putKeyword_keywordAction,
    putKeyword_originationIdentity,
    putKeyword_keyword,
    putKeyword_keywordMessage,
    putKeywordResponse_originationIdentity,
    putKeywordResponse_keywordAction,
    putKeywordResponse_originationIdentityArn,
    putKeywordResponse_keyword,
    putKeywordResponse_keywordMessage,
    putKeywordResponse_httpStatus,

    -- ** PutOptedOutNumber
    putOptedOutNumber_optOutListName,
    putOptedOutNumber_optedOutNumber,
    putOptedOutNumberResponse_optOutListArn,
    putOptedOutNumberResponse_optedOutTimestamp,
    putOptedOutNumberResponse_optOutListName,
    putOptedOutNumberResponse_optedOutNumber,
    putOptedOutNumberResponse_endUserOptedOut,
    putOptedOutNumberResponse_httpStatus,

    -- ** ReleasePhoneNumber
    releasePhoneNumber_phoneNumberId,
    releasePhoneNumberResponse_isoCountryCode,
    releasePhoneNumberResponse_phoneNumberArn,
    releasePhoneNumberResponse_messageType,
    releasePhoneNumberResponse_selfManagedOptOutsEnabled,
    releasePhoneNumberResponse_createdTimestamp,
    releasePhoneNumberResponse_status,
    releasePhoneNumberResponse_numberCapabilities,
    releasePhoneNumberResponse_twoWayEnabled,
    releasePhoneNumberResponse_optOutListName,
    releasePhoneNumberResponse_numberType,
    releasePhoneNumberResponse_phoneNumberId,
    releasePhoneNumberResponse_twoWayChannelArn,
    releasePhoneNumberResponse_phoneNumber,
    releasePhoneNumberResponse_monthlyLeasingPrice,
    releasePhoneNumberResponse_httpStatus,

    -- ** RequestPhoneNumber
    requestPhoneNumber_deletionProtectionEnabled,
    requestPhoneNumber_tags,
    requestPhoneNumber_clientToken,
    requestPhoneNumber_registrationId,
    requestPhoneNumber_optOutListName,
    requestPhoneNumber_poolId,
    requestPhoneNumber_isoCountryCode,
    requestPhoneNumber_messageType,
    requestPhoneNumber_numberCapabilities,
    requestPhoneNumber_numberType,
    requestPhoneNumberResponse_deletionProtectionEnabled,
    requestPhoneNumberResponse_tags,
    requestPhoneNumberResponse_isoCountryCode,
    requestPhoneNumberResponse_phoneNumberArn,
    requestPhoneNumberResponse_messageType,
    requestPhoneNumberResponse_selfManagedOptOutsEnabled,
    requestPhoneNumberResponse_createdTimestamp,
    requestPhoneNumberResponse_status,
    requestPhoneNumberResponse_numberCapabilities,
    requestPhoneNumberResponse_twoWayEnabled,
    requestPhoneNumberResponse_optOutListName,
    requestPhoneNumberResponse_numberType,
    requestPhoneNumberResponse_phoneNumberId,
    requestPhoneNumberResponse_poolId,
    requestPhoneNumberResponse_twoWayChannelArn,
    requestPhoneNumberResponse_phoneNumber,
    requestPhoneNumberResponse_monthlyLeasingPrice,
    requestPhoneNumberResponse_httpStatus,

    -- ** SendTextMessage
    sendTextMessage_originationIdentity,
    sendTextMessage_timeToLive,
    sendTextMessage_messageType,
    sendTextMessage_configurationSetName,
    sendTextMessage_maxPrice,
    sendTextMessage_context,
    sendTextMessage_messageBody,
    sendTextMessage_dryRun,
    sendTextMessage_keyword,
    sendTextMessage_destinationCountryParameters,
    sendTextMessage_destinationPhoneNumber,
    sendTextMessageResponse_messageId,
    sendTextMessageResponse_httpStatus,

    -- ** SendVoiceMessage
    sendVoiceMessage_voiceId,
    sendVoiceMessage_timeToLive,
    sendVoiceMessage_maxPricePerMinute,
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_context,
    sendVoiceMessage_messageBody,
    sendVoiceMessage_dryRun,
    sendVoiceMessage_messageBodyTextType,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessage_originationIdentity,
    sendVoiceMessageResponse_messageId,
    sendVoiceMessageResponse_httpStatus,

    -- ** SetDefaultMessageType
    setDefaultMessageType_configurationSetName,
    setDefaultMessageType_messageType,
    setDefaultMessageTypeResponse_messageType,
    setDefaultMessageTypeResponse_configurationSetName,
    setDefaultMessageTypeResponse_configurationSetArn,
    setDefaultMessageTypeResponse_httpStatus,

    -- ** SetDefaultSenderId
    setDefaultSenderId_configurationSetName,
    setDefaultSenderId_senderId,
    setDefaultSenderIdResponse_senderId,
    setDefaultSenderIdResponse_configurationSetName,
    setDefaultSenderIdResponse_configurationSetArn,
    setDefaultSenderIdResponse_httpStatus,

    -- ** SetTextMessageSpendLimitOverride
    setTextMessageSpendLimitOverride_monthlyLimit,
    setTextMessageSpendLimitOverrideResponse_monthlyLimit,
    setTextMessageSpendLimitOverrideResponse_httpStatus,

    -- ** SetVoiceMessageSpendLimitOverride
    setVoiceMessageSpendLimitOverride_monthlyLimit,
    setVoiceMessageSpendLimitOverrideResponse_monthlyLimit,
    setVoiceMessageSpendLimitOverrideResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateEventDestination
    updateEventDestination_cloudWatchLogsDestination,
    updateEventDestination_matchingEventTypes,
    updateEventDestination_snsDestination,
    updateEventDestination_enabled,
    updateEventDestination_kinesisFirehoseDestination,
    updateEventDestination_configurationSetName,
    updateEventDestination_eventDestinationName,
    updateEventDestinationResponse_configurationSetName,
    updateEventDestinationResponse_eventDestination,
    updateEventDestinationResponse_configurationSetArn,
    updateEventDestinationResponse_httpStatus,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_deletionProtectionEnabled,
    updatePhoneNumber_selfManagedOptOutsEnabled,
    updatePhoneNumber_twoWayEnabled,
    updatePhoneNumber_optOutListName,
    updatePhoneNumber_twoWayChannelArn,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumberResponse_deletionProtectionEnabled,
    updatePhoneNumberResponse_isoCountryCode,
    updatePhoneNumberResponse_phoneNumberArn,
    updatePhoneNumberResponse_messageType,
    updatePhoneNumberResponse_selfManagedOptOutsEnabled,
    updatePhoneNumberResponse_createdTimestamp,
    updatePhoneNumberResponse_status,
    updatePhoneNumberResponse_numberCapabilities,
    updatePhoneNumberResponse_twoWayEnabled,
    updatePhoneNumberResponse_optOutListName,
    updatePhoneNumberResponse_numberType,
    updatePhoneNumberResponse_phoneNumberId,
    updatePhoneNumberResponse_twoWayChannelArn,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_monthlyLeasingPrice,
    updatePhoneNumberResponse_httpStatus,

    -- ** UpdatePool
    updatePool_deletionProtectionEnabled,
    updatePool_selfManagedOptOutsEnabled,
    updatePool_twoWayEnabled,
    updatePool_optOutListName,
    updatePool_twoWayChannelArn,
    updatePool_sharedRoutesEnabled,
    updatePool_poolId,
    updatePoolResponse_deletionProtectionEnabled,
    updatePoolResponse_poolArn,
    updatePoolResponse_messageType,
    updatePoolResponse_selfManagedOptOutsEnabled,
    updatePoolResponse_createdTimestamp,
    updatePoolResponse_status,
    updatePoolResponse_twoWayEnabled,
    updatePoolResponse_optOutListName,
    updatePoolResponse_poolId,
    updatePoolResponse_twoWayChannelArn,
    updatePoolResponse_sharedRoutesEnabled,
    updatePoolResponse_httpStatus,

    -- * Types

    -- ** AccountAttribute
    accountAttribute_name,
    accountAttribute_value,

    -- ** AccountLimit
    accountLimit_name,
    accountLimit_used,
    accountLimit_max,

    -- ** CloudWatchLogsDestination
    cloudWatchLogsDestination_iamRoleArn,
    cloudWatchLogsDestination_logGroupArn,

    -- ** ConfigurationSetFilter
    configurationSetFilter_name,
    configurationSetFilter_values,

    -- ** ConfigurationSetInformation
    configurationSetInformation_defaultSenderId,
    configurationSetInformation_defaultMessageType,
    configurationSetInformation_configurationSetArn,
    configurationSetInformation_configurationSetName,
    configurationSetInformation_eventDestinations,
    configurationSetInformation_createdTimestamp,

    -- ** EventDestination
    eventDestination_cloudWatchLogsDestination,
    eventDestination_snsDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_eventDestinationName,
    eventDestination_enabled,
    eventDestination_matchingEventTypes,

    -- ** KeywordFilter
    keywordFilter_name,
    keywordFilter_values,

    -- ** KeywordInformation
    keywordInformation_keyword,
    keywordInformation_keywordMessage,
    keywordInformation_keywordAction,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- ** OptOutListInformation
    optOutListInformation_optOutListArn,
    optOutListInformation_optOutListName,
    optOutListInformation_createdTimestamp,

    -- ** OptedOutFilter
    optedOutFilter_name,
    optedOutFilter_values,

    -- ** OptedOutNumberInformation
    optedOutNumberInformation_optedOutNumber,
    optedOutNumberInformation_optedOutTimestamp,
    optedOutNumberInformation_endUserOptedOut,

    -- ** OriginationIdentityMetadata
    originationIdentityMetadata_originationIdentityArn,
    originationIdentityMetadata_originationIdentity,
    originationIdentityMetadata_isoCountryCode,
    originationIdentityMetadata_numberCapabilities,

    -- ** PhoneNumberFilter
    phoneNumberFilter_name,
    phoneNumberFilter_values,

    -- ** PhoneNumberInformation
    phoneNumberInformation_phoneNumberId,
    phoneNumberInformation_poolId,
    phoneNumberInformation_twoWayChannelArn,
    phoneNumberInformation_phoneNumberArn,
    phoneNumberInformation_phoneNumber,
    phoneNumberInformation_status,
    phoneNumberInformation_isoCountryCode,
    phoneNumberInformation_messageType,
    phoneNumberInformation_numberCapabilities,
    phoneNumberInformation_numberType,
    phoneNumberInformation_monthlyLeasingPrice,
    phoneNumberInformation_twoWayEnabled,
    phoneNumberInformation_selfManagedOptOutsEnabled,
    phoneNumberInformation_optOutListName,
    phoneNumberInformation_deletionProtectionEnabled,
    phoneNumberInformation_createdTimestamp,

    -- ** PoolFilter
    poolFilter_name,
    poolFilter_values,

    -- ** PoolInformation
    poolInformation_twoWayChannelArn,
    poolInformation_poolArn,
    poolInformation_poolId,
    poolInformation_status,
    poolInformation_messageType,
    poolInformation_twoWayEnabled,
    poolInformation_selfManagedOptOutsEnabled,
    poolInformation_optOutListName,
    poolInformation_sharedRoutesEnabled,
    poolInformation_deletionProtectionEnabled,
    poolInformation_createdTimestamp,

    -- ** PoolOriginationIdentitiesFilter
    poolOriginationIdentitiesFilter_name,
    poolOriginationIdentitiesFilter_values,

    -- ** SenderIdAndCountry
    senderIdAndCountry_senderId,
    senderIdAndCountry_isoCountryCode,

    -- ** SenderIdFilter
    senderIdFilter_name,
    senderIdFilter_values,

    -- ** SenderIdInformation
    senderIdInformation_senderIdArn,
    senderIdInformation_senderId,
    senderIdInformation_isoCountryCode,
    senderIdInformation_messageTypes,
    senderIdInformation_monthlyLeasingPrice,

    -- ** SnsDestination
    snsDestination_topicArn,

    -- ** SpendLimit
    spendLimit_name,
    spendLimit_enforcedLimit,
    spendLimit_maxLimit,
    spendLimit_overridden,

    -- ** Tag
    tag_key,
    tag_value,
  )
where

import Amazonka.PinpointSmsVoiceV2.AssociateOriginationIdentity
import Amazonka.PinpointSmsVoiceV2.CreateConfigurationSet
import Amazonka.PinpointSmsVoiceV2.CreateEventDestination
import Amazonka.PinpointSmsVoiceV2.CreateOptOutList
import Amazonka.PinpointSmsVoiceV2.CreatePool
import Amazonka.PinpointSmsVoiceV2.DeleteConfigurationSet
import Amazonka.PinpointSmsVoiceV2.DeleteDefaultMessageType
import Amazonka.PinpointSmsVoiceV2.DeleteDefaultSenderId
import Amazonka.PinpointSmsVoiceV2.DeleteEventDestination
import Amazonka.PinpointSmsVoiceV2.DeleteKeyword
import Amazonka.PinpointSmsVoiceV2.DeleteOptOutList
import Amazonka.PinpointSmsVoiceV2.DeleteOptedOutNumber
import Amazonka.PinpointSmsVoiceV2.DeletePool
import Amazonka.PinpointSmsVoiceV2.DeleteTextMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.DeleteVoiceMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.DescribeAccountAttributes
import Amazonka.PinpointSmsVoiceV2.DescribeAccountLimits
import Amazonka.PinpointSmsVoiceV2.DescribeConfigurationSets
import Amazonka.PinpointSmsVoiceV2.DescribeKeywords
import Amazonka.PinpointSmsVoiceV2.DescribeOptOutLists
import Amazonka.PinpointSmsVoiceV2.DescribeOptedOutNumbers
import Amazonka.PinpointSmsVoiceV2.DescribePhoneNumbers
import Amazonka.PinpointSmsVoiceV2.DescribePools
import Amazonka.PinpointSmsVoiceV2.DescribeSenderIds
import Amazonka.PinpointSmsVoiceV2.DescribeSpendLimits
import Amazonka.PinpointSmsVoiceV2.DisassociateOriginationIdentity
import Amazonka.PinpointSmsVoiceV2.ListPoolOriginationIdentities
import Amazonka.PinpointSmsVoiceV2.ListTagsForResource
import Amazonka.PinpointSmsVoiceV2.PutKeyword
import Amazonka.PinpointSmsVoiceV2.PutOptedOutNumber
import Amazonka.PinpointSmsVoiceV2.ReleasePhoneNumber
import Amazonka.PinpointSmsVoiceV2.RequestPhoneNumber
import Amazonka.PinpointSmsVoiceV2.SendTextMessage
import Amazonka.PinpointSmsVoiceV2.SendVoiceMessage
import Amazonka.PinpointSmsVoiceV2.SetDefaultMessageType
import Amazonka.PinpointSmsVoiceV2.SetDefaultSenderId
import Amazonka.PinpointSmsVoiceV2.SetTextMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.SetVoiceMessageSpendLimitOverride
import Amazonka.PinpointSmsVoiceV2.TagResource
import Amazonka.PinpointSmsVoiceV2.Types.AccountAttribute
import Amazonka.PinpointSmsVoiceV2.Types.AccountLimit
import Amazonka.PinpointSmsVoiceV2.Types.CloudWatchLogsDestination
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetFilter
import Amazonka.PinpointSmsVoiceV2.Types.ConfigurationSetInformation
import Amazonka.PinpointSmsVoiceV2.Types.EventDestination
import Amazonka.PinpointSmsVoiceV2.Types.KeywordFilter
import Amazonka.PinpointSmsVoiceV2.Types.KeywordInformation
import Amazonka.PinpointSmsVoiceV2.Types.KinesisFirehoseDestination
import Amazonka.PinpointSmsVoiceV2.Types.OptOutListInformation
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutFilter
import Amazonka.PinpointSmsVoiceV2.Types.OptedOutNumberInformation
import Amazonka.PinpointSmsVoiceV2.Types.OriginationIdentityMetadata
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberFilter
import Amazonka.PinpointSmsVoiceV2.Types.PhoneNumberInformation
import Amazonka.PinpointSmsVoiceV2.Types.PoolFilter
import Amazonka.PinpointSmsVoiceV2.Types.PoolInformation
import Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilter
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdAndCountry
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdFilter
import Amazonka.PinpointSmsVoiceV2.Types.SenderIdInformation
import Amazonka.PinpointSmsVoiceV2.Types.SnsDestination
import Amazonka.PinpointSmsVoiceV2.Types.SpendLimit
import Amazonka.PinpointSmsVoiceV2.Types.Tag
import Amazonka.PinpointSmsVoiceV2.UntagResource
import Amazonka.PinpointSmsVoiceV2.UpdateEventDestination
import Amazonka.PinpointSmsVoiceV2.UpdatePhoneNumber
import Amazonka.PinpointSmsVoiceV2.UpdatePool
