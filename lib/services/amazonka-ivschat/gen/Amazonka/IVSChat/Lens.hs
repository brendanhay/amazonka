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
    createChatToken_attributes,
    createChatToken_capabilities,
    createChatToken_sessionDurationInMinutes,
    createChatToken_roomIdentifier,
    createChatToken_userId,
    createChatTokenResponse_sessionExpirationTime,
    createChatTokenResponse_token,
    createChatTokenResponse_tokenExpirationTime,
    createChatTokenResponse_httpStatus,

    -- ** CreateLoggingConfiguration
    createLoggingConfiguration_name,
    createLoggingConfiguration_tags,
    createLoggingConfiguration_destinationConfiguration,
    createLoggingConfigurationResponse_arn,
    createLoggingConfigurationResponse_createTime,
    createLoggingConfigurationResponse_destinationConfiguration,
    createLoggingConfigurationResponse_id,
    createLoggingConfigurationResponse_name,
    createLoggingConfigurationResponse_state,
    createLoggingConfigurationResponse_tags,
    createLoggingConfigurationResponse_updateTime,
    createLoggingConfigurationResponse_httpStatus,

    -- ** CreateRoom
    createRoom_loggingConfigurationIdentifiers,
    createRoom_maximumMessageLength,
    createRoom_maximumMessageRatePerSecond,
    createRoom_messageReviewHandler,
    createRoom_name,
    createRoom_tags,
    createRoomResponse_arn,
    createRoomResponse_createTime,
    createRoomResponse_id,
    createRoomResponse_loggingConfigurationIdentifiers,
    createRoomResponse_maximumMessageLength,
    createRoomResponse_maximumMessageRatePerSecond,
    createRoomResponse_messageReviewHandler,
    createRoomResponse_name,
    createRoomResponse_tags,
    createRoomResponse_updateTime,
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
    getLoggingConfigurationResponse_arn,
    getLoggingConfigurationResponse_createTime,
    getLoggingConfigurationResponse_destinationConfiguration,
    getLoggingConfigurationResponse_id,
    getLoggingConfigurationResponse_name,
    getLoggingConfigurationResponse_state,
    getLoggingConfigurationResponse_tags,
    getLoggingConfigurationResponse_updateTime,
    getLoggingConfigurationResponse_httpStatus,

    -- ** GetRoom
    getRoom_identifier,
    getRoomResponse_arn,
    getRoomResponse_createTime,
    getRoomResponse_id,
    getRoomResponse_loggingConfigurationIdentifiers,
    getRoomResponse_maximumMessageLength,
    getRoomResponse_maximumMessageRatePerSecond,
    getRoomResponse_messageReviewHandler,
    getRoomResponse_name,
    getRoomResponse_tags,
    getRoomResponse_updateTime,
    getRoomResponse_httpStatus,

    -- ** ListLoggingConfigurations
    listLoggingConfigurations_maxResults,
    listLoggingConfigurations_nextToken,
    listLoggingConfigurationsResponse_nextToken,
    listLoggingConfigurationsResponse_httpStatus,
    listLoggingConfigurationsResponse_loggingConfigurations,

    -- ** ListRooms
    listRooms_loggingConfigurationIdentifier,
    listRooms_maxResults,
    listRooms_messageReviewHandlerUri,
    listRooms_name,
    listRooms_nextToken,
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
    updateLoggingConfiguration_destinationConfiguration,
    updateLoggingConfiguration_name,
    updateLoggingConfiguration_identifier,
    updateLoggingConfigurationResponse_arn,
    updateLoggingConfigurationResponse_createTime,
    updateLoggingConfigurationResponse_destinationConfiguration,
    updateLoggingConfigurationResponse_id,
    updateLoggingConfigurationResponse_name,
    updateLoggingConfigurationResponse_state,
    updateLoggingConfigurationResponse_tags,
    updateLoggingConfigurationResponse_updateTime,
    updateLoggingConfigurationResponse_httpStatus,

    -- ** UpdateRoom
    updateRoom_loggingConfigurationIdentifiers,
    updateRoom_maximumMessageLength,
    updateRoom_maximumMessageRatePerSecond,
    updateRoom_messageReviewHandler,
    updateRoom_name,
    updateRoom_identifier,
    updateRoomResponse_arn,
    updateRoomResponse_createTime,
    updateRoomResponse_id,
    updateRoomResponse_loggingConfigurationIdentifiers,
    updateRoomResponse_maximumMessageLength,
    updateRoomResponse_maximumMessageRatePerSecond,
    updateRoomResponse_messageReviewHandler,
    updateRoomResponse_name,
    updateRoomResponse_tags,
    updateRoomResponse_updateTime,
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
    loggingConfigurationSummary_arn,
    loggingConfigurationSummary_createTime,
    loggingConfigurationSummary_destinationConfiguration,
    loggingConfigurationSummary_id,
    loggingConfigurationSummary_name,
    loggingConfigurationSummary_state,
    loggingConfigurationSummary_tags,
    loggingConfigurationSummary_updateTime,

    -- ** MessageReviewHandler
    messageReviewHandler_fallbackResult,
    messageReviewHandler_uri,

    -- ** RoomSummary
    roomSummary_arn,
    roomSummary_createTime,
    roomSummary_id,
    roomSummary_loggingConfigurationIdentifiers,
    roomSummary_messageReviewHandler,
    roomSummary_name,
    roomSummary_tags,
    roomSummary_updateTime,

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
