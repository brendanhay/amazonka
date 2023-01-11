{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTData.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTData.Lens
  ( -- * Operations

    -- ** DeleteThingShadow
    deleteThingShadow_shadowName,
    deleteThingShadow_thingName,
    deleteThingShadowResponse_httpStatus,
    deleteThingShadowResponse_payload,

    -- ** GetRetainedMessage
    getRetainedMessage_topic,
    getRetainedMessageResponse_lastModifiedTime,
    getRetainedMessageResponse_payload,
    getRetainedMessageResponse_qos,
    getRetainedMessageResponse_topic,
    getRetainedMessageResponse_httpStatus,

    -- ** GetThingShadow
    getThingShadow_shadowName,
    getThingShadow_thingName,
    getThingShadowResponse_payload,
    getThingShadowResponse_httpStatus,

    -- ** ListNamedShadowsForThing
    listNamedShadowsForThing_nextToken,
    listNamedShadowsForThing_pageSize,
    listNamedShadowsForThing_thingName,
    listNamedShadowsForThingResponse_nextToken,
    listNamedShadowsForThingResponse_results,
    listNamedShadowsForThingResponse_timestamp,
    listNamedShadowsForThingResponse_httpStatus,

    -- ** ListRetainedMessages
    listRetainedMessages_maxResults,
    listRetainedMessages_nextToken,
    listRetainedMessagesResponse_nextToken,
    listRetainedMessagesResponse_retainedTopics,
    listRetainedMessagesResponse_httpStatus,

    -- ** Publish
    publish_contentType,
    publish_correlationData,
    publish_messageExpiry,
    publish_payload,
    publish_payloadFormatIndicator,
    publish_qos,
    publish_responseTopic,
    publish_retain,
    publish_userProperties,
    publish_topic,

    -- ** UpdateThingShadow
    updateThingShadow_shadowName,
    updateThingShadow_thingName,
    updateThingShadow_payload,
    updateThingShadowResponse_payload,
    updateThingShadowResponse_httpStatus,

    -- * Types

    -- ** RetainedMessageSummary
    retainedMessageSummary_lastModifiedTime,
    retainedMessageSummary_payloadSize,
    retainedMessageSummary_qos,
    retainedMessageSummary_topic,
  )
where

import Amazonka.IoTData.DeleteThingShadow
import Amazonka.IoTData.GetRetainedMessage
import Amazonka.IoTData.GetThingShadow
import Amazonka.IoTData.ListNamedShadowsForThing
import Amazonka.IoTData.ListRetainedMessages
import Amazonka.IoTData.Publish
import Amazonka.IoTData.Types.RetainedMessageSummary
import Amazonka.IoTData.UpdateThingShadow
