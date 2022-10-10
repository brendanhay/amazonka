{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SmsVoice.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SmsVoice.Lens
  ( -- * Operations

    -- ** CreateConfigurationSet
    createConfigurationSet_configurationSetName,
    createConfigurationSetResponse_httpStatus,

    -- ** CreateConfigurationSetEventDestination
    createConfigurationSetEventDestination_eventDestination,
    createConfigurationSetEventDestination_eventDestinationName,
    createConfigurationSetEventDestination_configurationSetName,
    createConfigurationSetEventDestinationResponse_httpStatus,

    -- ** DeleteConfigurationSet
    deleteConfigurationSet_configurationSetName,
    deleteConfigurationSetResponse_httpStatus,

    -- ** DeleteConfigurationSetEventDestination
    deleteConfigurationSetEventDestination_eventDestinationName,
    deleteConfigurationSetEventDestination_configurationSetName,
    deleteConfigurationSetEventDestinationResponse_httpStatus,

    -- ** GetConfigurationSetEventDestinations
    getConfigurationSetEventDestinations_configurationSetName,
    getConfigurationSetEventDestinationsResponse_eventDestinations,
    getConfigurationSetEventDestinationsResponse_httpStatus,

    -- ** ListConfigurationSets
    listConfigurationSets_nextToken,
    listConfigurationSets_pageSize,
    listConfigurationSetsResponse_nextToken,
    listConfigurationSetsResponse_configurationSets,
    listConfigurationSetsResponse_httpStatus,

    -- ** SendVoiceMessage
    sendVoiceMessage_callerId,
    sendVoiceMessage_configurationSetName,
    sendVoiceMessage_destinationPhoneNumber,
    sendVoiceMessage_originationPhoneNumber,
    sendVoiceMessage_content,
    sendVoiceMessageResponse_messageId,
    sendVoiceMessageResponse_httpStatus,

    -- ** UpdateConfigurationSetEventDestination
    updateConfigurationSetEventDestination_eventDestination,
    updateConfigurationSetEventDestination_eventDestinationName,
    updateConfigurationSetEventDestination_configurationSetName,
    updateConfigurationSetEventDestinationResponse_httpStatus,

    -- * Types

    -- ** CallInstructionsMessageType
    callInstructionsMessageType_text,

    -- ** CloudWatchLogsDestination
    cloudWatchLogsDestination_logGroupArn,
    cloudWatchLogsDestination_iamRoleArn,

    -- ** EventDestination
    eventDestination_name,
    eventDestination_cloudWatchLogsDestination,
    eventDestination_matchingEventTypes,
    eventDestination_snsDestination,
    eventDestination_enabled,
    eventDestination_kinesisFirehoseDestination,

    -- ** EventDestinationDefinition
    eventDestinationDefinition_cloudWatchLogsDestination,
    eventDestinationDefinition_matchingEventTypes,
    eventDestinationDefinition_snsDestination,
    eventDestinationDefinition_enabled,
    eventDestinationDefinition_kinesisFirehoseDestination,

    -- ** KinesisFirehoseDestination
    kinesisFirehoseDestination_deliveryStreamArn,
    kinesisFirehoseDestination_iamRoleArn,

    -- ** PlainTextMessageType
    plainTextMessageType_voiceId,
    plainTextMessageType_languageCode,
    plainTextMessageType_text,

    -- ** SSMLMessageType
    sSMLMessageType_voiceId,
    sSMLMessageType_languageCode,
    sSMLMessageType_text,

    -- ** SnsDestination
    snsDestination_topicArn,

    -- ** VoiceMessageContent
    voiceMessageContent_sSMLMessage,
    voiceMessageContent_callInstructionsMessage,
    voiceMessageContent_plainTextMessage,
  )
where

import Amazonka.SmsVoice.CreateConfigurationSet
import Amazonka.SmsVoice.CreateConfigurationSetEventDestination
import Amazonka.SmsVoice.DeleteConfigurationSet
import Amazonka.SmsVoice.DeleteConfigurationSetEventDestination
import Amazonka.SmsVoice.GetConfigurationSetEventDestinations
import Amazonka.SmsVoice.ListConfigurationSets
import Amazonka.SmsVoice.SendVoiceMessage
import Amazonka.SmsVoice.Types.CallInstructionsMessageType
import Amazonka.SmsVoice.Types.CloudWatchLogsDestination
import Amazonka.SmsVoice.Types.EventDestination
import Amazonka.SmsVoice.Types.EventDestinationDefinition
import Amazonka.SmsVoice.Types.KinesisFirehoseDestination
import Amazonka.SmsVoice.Types.PlainTextMessageType
import Amazonka.SmsVoice.Types.SSMLMessageType
import Amazonka.SmsVoice.Types.SnsDestination
import Amazonka.SmsVoice.Types.VoiceMessageContent
import Amazonka.SmsVoice.UpdateConfigurationSetEventDestination
