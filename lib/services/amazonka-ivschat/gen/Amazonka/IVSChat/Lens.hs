{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IVSChat.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IVSChat.Lens
  ( -- * Operations

    -- ** CreateChatToken
    createChatToken_capabilities,
    createChatToken_attributes,
    createChatToken_sessionDurationInMinutes,
    createChatToken_roomIdentifier,
    createChatToken_userId,
    createChatTokenResponse_sessionExpirationTime,
    createChatTokenResponse_tokenExpirationTime,
    createChatTokenResponse_token,
    createChatTokenResponse_httpStatus,

    -- ** CreateLoggingConfiguration
    createLoggingConfiguration_tags,
    createLoggingConfiguration_name,
    createLoggingConfiguration_destinationConfiguration,
    createLoggingConfigurationResponse_tags,
    createLoggingConfigurationResponse_name,
    createLoggingConfigurationResponse_arn,
    createLoggingConfigurationResponse_state,
    createLoggingConfigurationResponse_id,
    createLoggingConfigurationResponse_updateTime,
    createLoggingConfigurationResponse_createTime,
    createLoggingConfigurationResponse_destinationConfiguration,
    createLoggingConfigurationResponse_httpStatus,

    -- ** CreateRoom
    createRoom_tags,
    createRoom_loggingConfigurationIdentifiers,
    createRoom_name,
    createRoom_messageReviewHandler,
    createRoom_maximumMessageRatePerSecond,
    createRoom_maximumMessageLength,
    createRoomResponse_tags,
    createRoomResponse_loggingConfigurationIdentifiers,
    createRoomResponse_name,
    createRoomResponse_messageReviewHandler,
    createRoomResponse_arn,
    createRoomResponse_id,
    createRoomResponse_maximumMessageRatePerSecond,
    createRoomResponse_maximumMessageLength,
    createRoomResponse_updateTime,
    createRoomResponse_createTime,
    createRoomResponse_httpStatus,

    -- ** DeleteLoggingConfiguration
    deleteLoggingConfiguration_identifier,

    -- ** DeleteMessage
    deleteMessage_reason,
    deleteMessage_id,
    deleteMessage_roomIdentifier,
    deleteMessageResponse_id,
    deleteMessageResponse_httpStatus,

    -- ** DeleteRoom
    deleteRoom_identifier,

    -- ** DisconnectUser
    disconnectUser_reason,
    disconnectUser_roomIdentifier,
    disconnectUser_userId,
    disconnectUserResponse_httpStatus,

    -- ** GetLoggingConfiguration
    getLoggingConfiguration_identifier,
    getLoggingConfigurationResponse_tags,
    getLoggingConfigurationResponse_name,
    getLoggingConfigurationResponse_arn,
    getLoggingConfigurationResponse_state,
    getLoggingConfigurationResponse_id,
    getLoggingConfigurationResponse_updateTime,
    getLoggingConfigurationResponse_createTime,
    getLoggingConfigurationResponse_destinationConfiguration,
    getLoggingConfigurationResponse_httpStatus,

    -- ** GetRoom
    getRoom_identifier,
    getRoomResponse_tags,
    getRoomResponse_loggingConfigurationIdentifiers,
    getRoomResponse_name,
    getRoomResponse_messageReviewHandler,
    getRoomResponse_arn,
    getRoomResponse_id,
    getRoomResponse_maximumMessageRatePerSecond,
    getRoomResponse_maximumMessageLength,
    getRoomResponse_updateTime,
    getRoomResponse_createTime,
    getRoomResponse_httpStatus,

    -- ** ListLoggingConfigurations
    listLoggingConfigurations_nextToken,
    listLoggingConfigurations_maxResults,
    listLoggingConfigurationsResponse_nextToken,
    listLoggingConfigurationsResponse_httpStatus,
    listLoggingConfigurationsResponse_loggingConfigurations,

    -- ** ListRooms
    listRooms_name,
    listRooms_nextToken,
    listRooms_loggingConfigurationIdentifier,
    listRooms_messageReviewHandlerUri,
    listRooms_maxResults,
    listRoomsResponse_nextToken,
    listRoomsResponse_httpStatus,
    listRoomsResponse_rooms,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_httpStatus,
    listTagsForResourceResponse_tags,

    -- ** SendEvent
    sendEvent_attributes,
    sendEvent_eventName,
    sendEvent_roomIdentifier,
    sendEventResponse_id,
    sendEventResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateLoggingConfiguration
    updateLoggingConfiguration_name,
    updateLoggingConfiguration_destinationConfiguration,
    updateLoggingConfiguration_identifier,
    updateLoggingConfigurationResponse_tags,
    updateLoggingConfigurationResponse_name,
    updateLoggingConfigurationResponse_arn,
    updateLoggingConfigurationResponse_state,
    updateLoggingConfigurationResponse_id,
    updateLoggingConfigurationResponse_updateTime,
    updateLoggingConfigurationResponse_createTime,
    updateLoggingConfigurationResponse_destinationConfiguration,
    updateLoggingConfigurationResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_loggingConfigurationIdentifiers,
    updateRoom_name,
    updateRoom_messageReviewHandler,
    updateRoom_maximumMessageRatePerSecond,
    updateRoom_maximumMessageLength,
    updateRoom_identifier,
    updateRoomResponse_tags,
    updateRoomResponse_loggingConfigurationIdentifiers,
    updateRoomResponse_name,
    updateRoomResponse_messageReviewHandler,
    updateRoomResponse_arn,
    updateRoomResponse_id,
    updateRoomResponse_maximumMessageRatePerSecond,
    updateRoomResponse_maximumMessageLength,
    updateRoomResponse_updateTime,
    updateRoomResponse_createTime,
    updateRoomResponse_httpStatus,

    -- * Types

    -- ** CloudWatchLogsDestinationConfiguration
    cloudWatchLogsDestinationConfiguration_logGroupName,

    -- ** DestinationConfiguration
    destinationConfiguration_cloudWatchLogs,
    destinationConfiguration_firehose,
    destinationConfiguration_s3,

    -- ** FirehoseDestinationConfiguration
    firehoseDestinationConfiguration_deliveryStreamName,

    -- ** LoggingConfigurationSummary
    loggingConfigurationSummary_tags,
    loggingConfigurationSummary_name,
    loggingConfigurationSummary_arn,
    loggingConfigurationSummary_state,
    loggingConfigurationSummary_id,
    loggingConfigurationSummary_updateTime,
    loggingConfigurationSummary_createTime,
    loggingConfigurationSummary_destinationConfiguration,

    -- ** MessageReviewHandler
    messageReviewHandler_uri,
    messageReviewHandler_fallbackResult,

    -- ** RoomSummary
    roomSummary_tags,
    roomSummary_loggingConfigurationIdentifiers,
    roomSummary_name,
    roomSummary_messageReviewHandler,
    roomSummary_arn,
    roomSummary_id,
    roomSummary_updateTime,
    roomSummary_createTime,

    -- ** S3DestinationConfiguration
    s3DestinationConfiguration_bucketName,
  )
where

import Amazonka.IVSChat.CreateChatToken
import Amazonka.IVSChat.CreateLoggingConfiguration
import Amazonka.IVSChat.CreateRoom
import Amazonka.IVSChat.DeleteLoggingConfiguration
import Amazonka.IVSChat.DeleteMessage
import Amazonka.IVSChat.DeleteRoom
import Amazonka.IVSChat.DisconnectUser
import Amazonka.IVSChat.GetLoggingConfiguration
import Amazonka.IVSChat.GetRoom
import Amazonka.IVSChat.ListLoggingConfigurations
import Amazonka.IVSChat.ListRooms
import Amazonka.IVSChat.ListTagsForResource
import Amazonka.IVSChat.SendEvent
import Amazonka.IVSChat.TagResource
import Amazonka.IVSChat.Types.CloudWatchLogsDestinationConfiguration
import Amazonka.IVSChat.Types.DestinationConfiguration
import Amazonka.IVSChat.Types.FirehoseDestinationConfiguration
import Amazonka.IVSChat.Types.LoggingConfigurationSummary
import Amazonka.IVSChat.Types.MessageReviewHandler
import Amazonka.IVSChat.Types.RoomSummary
import Amazonka.IVSChat.Types.S3DestinationConfiguration
import Amazonka.IVSChat.UntagResource
import Amazonka.IVSChat.UpdateLoggingConfiguration
import Amazonka.IVSChat.UpdateRoom
