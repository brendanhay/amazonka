{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.PinpointSMSVoice.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSMSVoice.Lens
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

import Amazonka.PinpointSMSVoice.CreateConfigurationSet
import Amazonka.PinpointSMSVoice.CreateConfigurationSetEventDestination
import Amazonka.PinpointSMSVoice.DeleteConfigurationSet
import Amazonka.PinpointSMSVoice.DeleteConfigurationSetEventDestination
import Amazonka.PinpointSMSVoice.GetConfigurationSetEventDestinations
import Amazonka.PinpointSMSVoice.SendVoiceMessage
import Amazonka.PinpointSMSVoice.Types.CallInstructionsMessageType
import Amazonka.PinpointSMSVoice.Types.CloudWatchLogsDestination
import Amazonka.PinpointSMSVoice.Types.EventDestination
import Amazonka.PinpointSMSVoice.Types.EventDestinationDefinition
import Amazonka.PinpointSMSVoice.Types.KinesisFirehoseDestination
import Amazonka.PinpointSMSVoice.Types.PlainTextMessageType
import Amazonka.PinpointSMSVoice.Types.SSMLMessageType
import Amazonka.PinpointSMSVoice.Types.SnsDestination
import Amazonka.PinpointSMSVoice.Types.VoiceMessageContent
import Amazonka.PinpointSMSVoice.UpdateConfigurationSetEventDestination
