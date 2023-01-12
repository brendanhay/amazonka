{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSmsVoiceV2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    associateOriginationIdentityResponse_originationIdentityArn,
    associateOriginationIdentityResponse_poolArn,
    associateOriginationIdentityResponse_poolId,
    associateOriginationIdentityResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_clientToken,
    createConfigurationSet_tags,
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_configurationSetArn,
    createConfigurationSetResponse_configurationSetName,
    createConfigurationSetResponse_createdTimestamp,
    createConfigurationSetResponse_tags,
    createConfigurationSetResponse_httpStatus,

    -- ** CreateEventDestination
    createEventDestination_clientToken,
    createEventDestination_cloudWatchLogsDestination,
    createEventDestination_kinesisFirehoseDestination,
    createEventDestination_snsDestination,
    createEventDestination_configurationSetName,
    createEventDestination_eventDestinationName,
    createEventDestination_matchingEventTypes,
    createEventDestinationResponse_configurationSetArn,
    createEventDestinationResponse_configurationSetName,
    createEventDestinationResponse_eventDestination,
    createEventDestinationResponse_httpStatus,

    -- ** CreateOptOutList
    createOptOutList_clientToken,
    createOptOutList_tags,
    createOptOutList_optOutListName,
    createOptOutListResponse_createdTimestamp,
    createOptOutListResponse_optOutListArn,
    createOptOutListResponse_optOutListName,
    createOptOutListResponse_tags,
    createOptOutListResponse_httpStatus,

    -- ** CreatePool
    createPool_clientToken,
    createPool_deletionProtectionEnabled,
    createPool_tags,
    createPool_originationIdentity,
    createPool_isoCountryCode,
    createPool_messageType,
    createPoolResponse_createdTimestamp,
    createPoolResponse_deletionProtectionEnabled,
    createPoolResponse_messageType,
    createPoolResponse_optOutListName,
    createPoolResponse_poolArn,
    createPoolResponse_poolId,
    createPoolResponse_selfManagedOptOutsEnabled,
    createPoolResponse_sharedRoutesEnabled,
    createPoolResponse_status,
    createPoolResponse_tags,
    createPoolResponse_twoWayChannelArn,
    createPoolResponse_twoWayEnabled,
    createPoolResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_configurationSetArn,
    deleteConfigurationSetResponse_configurationSetName,
    deleteConfigurationSetResponse_createdTimestamp,
    deleteConfigurationSetResponse_defaultMessageType,
    deleteConfigurationSetResponse_defaultSenderId,
    deleteConfigurationSetResponse_eventDestinations,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteDefaultMessageType
    deleteDefaultMessageType_configurationSetName,
    deleteDefaultMessageTypeResponse_configurationSetArn,
    deleteDefaultMessageTypeResponse_configurationSetName,
    deleteDefaultMessageTypeResponse_messageType,
    deleteDefaultMessageTypeResponse_httpStatus,

    -- ** DeleteDefaultSenderId
    deleteDefaultSenderId_configurationSetName,
    deleteDefaultSenderIdResponse_configurationSetArn,
    deleteDefaultSenderIdResponse_configurationSetName,
    deleteDefaultSenderIdResponse_senderId,
    deleteDefaultSenderIdResponse_httpStatus,

    -- ** DeleteEventDestination
    deleteEventDestination_configurationSetName,
    deleteEventDestination_eventDestinationName,
    deleteEventDestinationResponse_configurationSetArn,
    deleteEventDestinationResponse_configurationSetName,
    deleteEventDestinationResponse_eventDestination,
    deleteEventDestinationResponse_httpStatus,

    -- ** DeleteKeyword
    deleteKeyword_originationIdentity,
    deleteKeyword_keyword,
    deleteKeywordResponse_keyword,
    deleteKeywordResponse_keywordAction,
    deleteKeywordResponse_keywordMessage,
    deleteKeywordResponse_originationIdentity,
    deleteKeywordResponse_originationIdentityArn,
    deleteKeywordResponse_httpStatus,

    -- ** DeleteOptOutList
    deleteOptOutList_optOutListName,
    deleteOptOutListResponse_createdTimestamp,
    deleteOptOutListResponse_optOutListArn,
    deleteOptOutListResponse_optOutListName,
    deleteOptOutListResponse_httpStatus,

    -- ** DeleteOptedOutNumber
    deleteOptedOutNumber_optOutListName,
    deleteOptedOutNumber_optedOutNumber,
    deleteOptedOutNumberResponse_endUserOptedOut,
    deleteOptedOutNumberResponse_optOutListArn,
    deleteOptedOutNumberResponse_optOutListName,
    deleteOptedOutNumberResponse_optedOutNumber,
    deleteOptedOutNumberResponse_optedOutTimestamp,
    deleteOptedOutNumberResponse_httpStatus,

    -- ** DeletePool
    deletePool_poolId,
    deletePoolResponse_createdTimestamp,
    deletePoolResponse_messageType,
    deletePoolResponse_optOutListName,
    deletePoolResponse_poolArn,
    deletePoolResponse_poolId,
    deletePoolResponse_selfManagedOptOutsEnabled,
    deletePoolResponse_sharedRoutesEnabled,
    deletePoolResponse_status,
    deletePoolResponse_twoWayChannelArn,
    deletePoolResponse_twoWayEnabled,
    deletePoolResponse_httpStatus,

    -- ** DeleteTextMessageSpendLimitOverride
    deleteTextMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteTextMessageSpendLimitOverrideResponse_httpStatus,

    -- ** DeleteVoiceMessageSpendLimitOverride
    deleteVoiceMessageSpendLimitOverrideResponse_monthlyLimit,
    deleteVoiceMessageSpendLimitOverrideResponse_httpStatus,

    -- ** DescribeAccountAttributes
    describeAccountAttributes_maxResults,
    describeAccountAttributes_nextToken,
    describeAccountAttributesResponse_accountAttributes,
    describeAccountAttributesResponse_nextToken,
    describeAccountAttributesResponse_httpStatus,

    -- ** DescribeAccountLimits
    describeAccountLimits_maxResults,
    describeAccountLimits_nextToken,
    describeAccountLimitsResponse_accountLimits,
    describeAccountLimitsResponse_nextToken,
    describeAccountLimitsResponse_httpStatus,

    -- ** DescribeConfigurationSets
    describeConfigurationSets_configurationSetNames,
    describeConfigurationSets_filters,
    describeConfigurationSets_maxResults,
    describeConfigurationSets_nextToken,
    describeConfigurationSetsResponse_configurationSets,
    describeConfigurationSetsResponse_nextToken,
    describeConfigurationSetsResponse_httpStatus,

    -- ** DescribeKeywords
    describeKeywords_filters,
    describeKeywords_keywords,
    describeKeywords_maxResults,
    describeKeywords_nextToken,
    describeKeywords_originationIdentity,
    describeKeywordsResponse_keywords,
    describeKeywordsResponse_nextToken,
    describeKeywordsResponse_originationIdentity,
    describeKeywordsResponse_originationIdentityArn,
    describeKeywordsResponse_httpStatus,

    -- ** DescribeOptOutLists
    describeOptOutLists_maxResults,
    describeOptOutLists_nextToken,
    describeOptOutLists_optOutListNames,
    describeOptOutListsResponse_nextToken,
    describeOptOutListsResponse_optOutLists,
    describeOptOutListsResponse_httpStatus,

    -- ** DescribeOptedOutNumbers
    describeOptedOutNumbers_filters,
    describeOptedOutNumbers_maxResults,
    describeOptedOutNumbers_nextToken,
    describeOptedOutNumbers_optedOutNumbers,
    describeOptedOutNumbers_optOutListName,
    describeOptedOutNumbersResponse_nextToken,
    describeOptedOutNumbersResponse_optOutListArn,
    describeOptedOutNumbersResponse_optOutListName,
    describeOptedOutNumbersResponse_optedOutNumbers,
    describeOptedOutNumbersResponse_httpStatus,

    -- ** DescribePhoneNumbers
    describePhoneNumbers_filters,
    describePhoneNumbers_maxResults,
    describePhoneNumbers_nextToken,
    describePhoneNumbers_phoneNumberIds,
    describePhoneNumbersResponse_nextToken,
    describePhoneNumbersResponse_phoneNumbers,
    describePhoneNumbersResponse_httpStatus,

    -- ** DescribePools
    describePools_filters,
    describePools_maxResults,
    describePools_nextToken,
    describePools_poolIds,
    describePoolsResponse_nextToken,
    describePoolsResponse_pools,
    describePoolsResponse_httpStatus,

    -- ** DescribeSenderIds
    describeSenderIds_filters,
    describeSenderIds_maxResults,
    describeSenderIds_nextToken,
    describeSenderIds_senderIds,
    describeSenderIdsResponse_nextToken,
    describeSenderIdsResponse_senderIds,
    describeSenderIdsResponse_httpStatus,

    -- ** DescribeSpendLimits
    describeSpendLimits_maxResults,
    describeSpendLimits_nextToken,
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
    disassociateOriginationIdentityResponse_originationIdentityArn,
    disassociateOriginationIdentityResponse_poolArn,
    disassociateOriginationIdentityResponse_poolId,
    disassociateOriginationIdentityResponse_httpStatus,

    -- ** ListPoolOriginationIdentities
    listPoolOriginationIdentities_filters,
    listPoolOriginationIdentities_maxResults,
    listPoolOriginationIdentities_nextToken,
    listPoolOriginationIdentities_poolId,
    listPoolOriginationIdentitiesResponse_nextToken,
    listPoolOriginationIdentitiesResponse_originationIdentities,
    listPoolOriginationIdentitiesResponse_poolArn,
    listPoolOriginationIdentitiesResponse_poolId,
    listPoolOriginationIdentitiesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutKeyword
    putKeyword_keywordAction,
    putKeyword_originationIdentity,
    putKeyword_keyword,
    putKeyword_keywordMessage,
    putKeywordResponse_keyword,
    putKeywordResponse_keywordAction,
    putKeywordResponse_keywordMessage,
    putKeywordResponse_originationIdentity,
    putKeywordResponse_originationIdentityArn,
    putKeywordResponse_httpStatus,

    -- ** PutOptedOutNumber
    putOptedOutNumber_optOutListName,
    putOptedOutNumber_optedOutNumber,
    putOptedOutNumberResponse_endUserOptedOut,
    putOptedOutNumberResponse_optOutListArn,
    putOptedOutNumberResponse_optOutListName,
    putOptedOutNumberResponse_optedOutNumber,
    putOptedOutNumberResponse_optedOutTimestamp,
    putOptedOutNumberResponse_httpStatus,

    -- ** ReleasePhoneNumber
    releasePhoneNumber_phoneNumberId,
    releasePhoneNumberResponse_createdTimestamp,
    releasePhoneNumberResponse_isoCountryCode,
    releasePhoneNumberResponse_messageType,
    releasePhoneNumberResponse_monthlyLeasingPrice,
    releasePhoneNumberResponse_numberCapabilities,
    releasePhoneNumberResponse_numberType,
    releasePhoneNumberResponse_optOutListName,
    releasePhoneNumberResponse_phoneNumber,
    releasePhoneNumberResponse_phoneNumberArn,
    releasePhoneNumberResponse_phoneNumberId,
    releasePhoneNumberResponse_selfManagedOptOutsEnabled,
    releasePhoneNumberResponse_status,
    releasePhoneNumberResponse_twoWayChannelArn,
    releasePhoneNumberResponse_twoWayEnabled,
    releasePhoneNumberResponse_httpStatus,

    -- ** RequestPhoneNumber
    requestPhoneNumber_clientToken,
    requestPhoneNumber_deletionProtectionEnabled,
    requestPhoneNumber_optOutListName,
    requestPhoneNumber_poolId,
    requestPhoneNumber_registrationId,
    requestPhoneNumber_tags,
    requestPhoneNumber_isoCountryCode,
    requestPhoneNumber_messageType,
    requestPhoneNumber_numberCapabilities,
    requestPhoneNumber_numberType,
    requestPhoneNumberResponse_createdTimestamp,
    requestPhoneNumberResponse_deletionProtectionEnabled,
    requestPhoneNumberResponse_isoCountryCode,
    requestPhoneNumberResponse_messageType,
    requestPhoneNumberResponse_monthlyLeasingPrice,
    requestPhoneNumberResponse_numberCapabilities,
    requestPhoneNumberResponse_numberType,
    requestPhoneNumberResponse_optOutListName,
    requestPhoneNumberResponse_phoneNumber,
    requestPhoneNumberResponse_phoneNumberArn,
    requestPhoneNumberResponse_phoneNumberId,
    requestPhoneNumberResponse_poolId,
    requestPhoneNumberResponse_selfManagedOptOutsEnabled,
    requestPhoneNumberResponse_status,
    requestPhoneNumberResponse_tags,
    requestPhoneNumberResponse_twoWayChannelArn,
    requestPhoneNumberResponse_twoWayEnabled,
    requestPhoneNumberResponse_httpStatus,

    -- ** SendTextMessage
    sendTextMessage_configurationSetName,
    sendTextMessage_context,
    sendTextMessage_destinationCountryParameters,
    sendTextMessage_dryRun,
    sendTextMessage_keyword,
    sendTextMessage_maxPrice,
    sendTextMessage_messageBody,
    sendTextMessage_messageType,
    sendTextMessage_originationIdentity,
    sendTextMessage_timeToLive,
    sendTextMessage_destinationPhoneNumber,
    sendTextMessageResponse_messageId,
    sendTextMessageResponse_httpStatus,

    -- ** SendVoiceMessage
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_context,
    sendVoiceMessage_dryRun,
    sendVoiceMessage_maxPricePerMinute,
    sendVoiceMessage_messageBody,
    sendVoiceMessage_messageBodyTextType,
    sendVoiceMessage_timeToLive,
    sendVoiceMessage_voiceId,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessage_originationIdentity,
    sendVoiceMessageResponse_messageId,
    sendVoiceMessageResponse_httpStatus,

    -- ** SetDefaultMessageType
    setDefaultMessageType_configurationSetName,
    setDefaultMessageType_messageType,
    setDefaultMessageTypeResponse_configurationSetArn,
    setDefaultMessageTypeResponse_configurationSetName,
    setDefaultMessageTypeResponse_messageType,
    setDefaultMessageTypeResponse_httpStatus,

    -- ** SetDefaultSenderId
    setDefaultSenderId_configurationSetName,
    setDefaultSenderId_senderId,
    setDefaultSenderIdResponse_configurationSetArn,
    setDefaultSenderIdResponse_configurationSetName,
    setDefaultSenderIdResponse_senderId,
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
    updateEventDestination_enabled,
    updateEventDestination_kinesisFirehoseDestination,
    updateEventDestination_matchingEventTypes,
    updateEventDestination_snsDestination,
    updateEventDestination_configurationSetName,
    updateEventDestination_eventDestinationName,
    updateEventDestinationResponse_configurationSetArn,
    updateEventDestinationResponse_configurationSetName,
    updateEventDestinationResponse_eventDestination,
    updateEventDestinationResponse_httpStatus,

    -- ** UpdatePhoneNumber
    updatePhoneNumber_deletionProtectionEnabled,
    updatePhoneNumber_optOutListName,
    updatePhoneNumber_selfManagedOptOutsEnabled,
    updatePhoneNumber_twoWayChannelArn,
    updatePhoneNumber_twoWayEnabled,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumberResponse_createdTimestamp,
    updatePhoneNumberResponse_deletionProtectionEnabled,
    updatePhoneNumberResponse_isoCountryCode,
    updatePhoneNumberResponse_messageType,
    updatePhoneNumberResponse_monthlyLeasingPrice,
    updatePhoneNumberResponse_numberCapabilities,
    updatePhoneNumberResponse_numberType,
    updatePhoneNumberResponse_optOutListName,
    updatePhoneNumberResponse_phoneNumber,
    updatePhoneNumberResponse_phoneNumberArn,
    updatePhoneNumberResponse_phoneNumberId,
    updatePhoneNumberResponse_selfManagedOptOutsEnabled,
    updatePhoneNumberResponse_status,
    updatePhoneNumberResponse_twoWayChannelArn,
    updatePhoneNumberResponse_twoWayEnabled,
    updatePhoneNumberResponse_httpStatus,

    -- ** UpdatePool
    updatePool_deletionProtectionEnabled,
    updatePool_optOutListName,
    updatePool_selfManagedOptOutsEnabled,
    updatePool_sharedRoutesEnabled,
    updatePool_twoWayChannelArn,
    updatePool_twoWayEnabled,
    updatePool_poolId,
    updatePoolResponse_createdTimestamp,
    updatePoolResponse_deletionProtectionEnabled,
    updatePoolResponse_messageType,
    updatePoolResponse_optOutListName,
    updatePoolResponse_poolArn,
    updatePoolResponse_poolId,
    updatePoolResponse_selfManagedOptOutsEnabled,
    updatePoolResponse_sharedRoutesEnabled,
    updatePoolResponse_status,
    updatePoolResponse_twoWayChannelArn,
    updatePoolResponse_twoWayEnabled,
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
    configurationSetInformation_defaultMessageType,
    configurationSetInformation_defaultSenderId,
    configurationSetInformation_configurationSetArn,
    configurationSetInformation_configurationSetName,
    configurationSetInformation_eventDestinations,
    configurationSetInformation_createdTimestamp,

    -- ** EventDestination
    eventDestination_cloudWatchLogsDestination,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_snsDestination,
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
