{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.PinpointSMSVoice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-09-05@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Pinpoint SMS and Voice Messaging public facing APIs
module Amazonka.PinpointSMSVoice
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** AlreadyExistsException
    _AlreadyExistsException,

    -- ** BadRequestException
    _BadRequestException,

    -- ** InternalServiceErrorException
    _InternalServiceErrorException,

    -- ** LimitExceededException
    _LimitExceededException,

    -- ** NotFoundException
    _NotFoundException,

    -- ** TooManyRequestsException
    _TooManyRequestsException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CreateConfigurationSet
    CreateConfigurationSet (CreateConfigurationSet'),
    newCreateConfigurationSet,
    CreateConfigurationSetResponse (CreateConfigurationSetResponse'),
    newCreateConfigurationSetResponse,

    -- ** CreateConfigurationSetEventDestination
    CreateConfigurationSetEventDestination (CreateConfigurationSetEventDestination'),
    newCreateConfigurationSetEventDestination,
    CreateConfigurationSetEventDestinationResponse (CreateConfigurationSetEventDestinationResponse'),
    newCreateConfigurationSetEventDestinationResponse,

    -- ** DeleteConfigurationSet
    DeleteConfigurationSet (DeleteConfigurationSet'),
    newDeleteConfigurationSet,
    DeleteConfigurationSetResponse (DeleteConfigurationSetResponse'),
    newDeleteConfigurationSetResponse,

    -- ** DeleteConfigurationSetEventDestination
    DeleteConfigurationSetEventDestination (DeleteConfigurationSetEventDestination'),
    newDeleteConfigurationSetEventDestination,
    DeleteConfigurationSetEventDestinationResponse (DeleteConfigurationSetEventDestinationResponse'),
    newDeleteConfigurationSetEventDestinationResponse,

    -- ** GetConfigurationSetEventDestinations
    GetConfigurationSetEventDestinations (GetConfigurationSetEventDestinations'),
    newGetConfigurationSetEventDestinations,
    GetConfigurationSetEventDestinationsResponse (GetConfigurationSetEventDestinationsResponse'),
    newGetConfigurationSetEventDestinationsResponse,

    -- ** SendVoiceMessage
    SendVoiceMessage (SendVoiceMessage'),
    newSendVoiceMessage,
    SendVoiceMessageResponse (SendVoiceMessageResponse'),
    newSendVoiceMessageResponse,

    -- ** UpdateConfigurationSetEventDestination
    UpdateConfigurationSetEventDestination (UpdateConfigurationSetEventDestination'),
    newUpdateConfigurationSetEventDestination,
    UpdateConfigurationSetEventDestinationResponse (UpdateConfigurationSetEventDestinationResponse'),
    newUpdateConfigurationSetEventDestinationResponse,

    -- * Types

    -- ** EventType
    EventType (..),

    -- ** CallInstructionsMessageType
    CallInstructionsMessageType (CallInstructionsMessageType'),
    newCallInstructionsMessageType,

    -- ** CloudWatchLogsDestination
    CloudWatchLogsDestination (CloudWatchLogsDestination'),
    newCloudWatchLogsDestination,

    -- ** EventDestination
    EventDestination (EventDestination'),
    newEventDestination,

    -- ** EventDestinationDefinition
    EventDestinationDefinition (EventDestinationDefinition'),
    newEventDestinationDefinition,

    -- ** KinesisFirehoseDestination
    KinesisFirehoseDestination (KinesisFirehoseDestination'),
    newKinesisFirehoseDestination,

    -- ** PlainTextMessageType
    PlainTextMessageType (PlainTextMessageType'),
    newPlainTextMessageType,

    -- ** SSMLMessageType
    SSMLMessageType (SSMLMessageType'),
    newSSMLMessageType,

    -- ** SnsDestination
    SnsDestination (SnsDestination'),
    newSnsDestination,

    -- ** VoiceMessageContent
    VoiceMessageContent (VoiceMessageContent'),
    newVoiceMessageContent,
  )
where

import Amazonka.PinpointSMSVoice.CreateConfigurationSet
import Amazonka.PinpointSMSVoice.CreateConfigurationSetEventDestination
import Amazonka.PinpointSMSVoice.DeleteConfigurationSet
import Amazonka.PinpointSMSVoice.DeleteConfigurationSetEventDestination
import Amazonka.PinpointSMSVoice.GetConfigurationSetEventDestinations
import Amazonka.PinpointSMSVoice.Lens
import Amazonka.PinpointSMSVoice.SendVoiceMessage
import Amazonka.PinpointSMSVoice.Types
import Amazonka.PinpointSMSVoice.UpdateConfigurationSetEventDestination
import Amazonka.PinpointSMSVoice.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'PinpointSMSVoice'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
