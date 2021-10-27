{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMSVoice.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMSVoice.Lens
  ( -- * Operations

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** SendVoiceMessage
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_callerId,
    sendVoiceMessage_originationPhoneNumber,
    sendVoiceMessage_content,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessageResponse_messageId,
    sendVoiceMessageResponse_httpStatus,

    -- ** GetConfigurationSetEventDestinations
    getConfigurationSetEventDestinations_configurationSetName,
    getConfigurationSetEventDestinationsResponse_eventDestinations,
    getConfigurationSetEventDestinationsResponse_httpStatus,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** CreateConfigurationSet
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_httpStatus,

    -- * Types

    -- ** CallInstructionsMessageType
    callInstructionsMessageType_text,

    -- ** CloudWatchLogsDestination
    cloudWatchLogsDestination_iamRoleArn,
    cloudWatchLogsDestination_logGroupArn,

    -- ** EventDestination
    eventDestination_matchingEventTypes,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,
    eventDestination_name,
    eventDestination_snsDestination,
    eventDestination_cloudWatchLogsDestination,

    -- ** EventDestinationDefinition
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_kinesisFirehoseDestination,
    eventDestinationDefinition_snsDestination,
    eventDestinationDefinition_cloudWatchLogsDestination,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_iamRoleArn,
    kinesisFirehoseDestination_deliveryStreamArn,

    -- ** PlainTextMessageType
    plainTextMessageType_languageCode,
    plainTextMessageType_text,
    plainTextMessageType_voiceId,

    -- ** SSMLMessageType
    sSMLMessageType_languageCode,
    sSMLMessageType_text,
    sSMLMessageType_voiceId,

    -- ** SnsDestination
    snsDestination_topicArn,

    -- ** VoiceMessageContent
    voiceMessageContent_callInstructionsMessage,
    voiceMessageContent_sSMLMessage,
    voiceMessageContent_plainTextMessage,
  )
where

import Network.AWS.SMSVoice.CreateConfigurationSet
import Network.AWS.SMSVoice.CreateConfigurationSetEventDestination
import Network.AWS.SMSVoice.DeleteConfigurationSet
import Network.AWS.SMSVoice.DeleteConfigurationSetEventDestination
import Network.AWS.SMSVoice.GetConfigurationSetEventDestinations
import Network.AWS.SMSVoice.ListConfigurationSets
import Network.AWS.SMSVoice.SendVoiceMessage
import Network.AWS.SMSVoice.Types.CallInstructionsMessageType
import Network.AWS.SMSVoice.Types.CloudWatchLogsDestination
import Network.AWS.SMSVoice.Types.EventDestination
import Network.AWS.SMSVoice.Types.EventDestinationDefinition
import Network.AWS.SMSVoice.Types.KinesisFirehoseDestination
import Network.AWS.SMSVoice.Types.PlainTextMessageType
import Network.AWS.SMSVoice.Types.SSMLMessageType
import Network.AWS.SMSVoice.Types.SnsDestination
import Network.AWS.SMSVoice.Types.VoiceMessageContent
import Network.AWS.SMSVoice.UpdateConfigurationSetEventDestination
