{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTData.GetRetainedMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the details of a single retained message for the specified topic.
--
-- This action returns the message payload of the retained message, which
-- can incur messaging costs. To list only the topic names of the retained
-- messages, call
-- </iot/latest/developerguide/API_iotdata_ListRetainedMessages.html ListRetainedMessages>.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiotfleethubfordevicemanagement.html#awsiotfleethubfordevicemanagement-actions-as-permissions GetRetainedMessage>
-- action.
--
-- For more information about messaging costs, see
-- <http://aws.amazon.com/iot-core/pricing/#Messaging IoT Core pricing - Messaging>.
module Network.AWS.IoTData.GetRetainedMessage
  ( -- * Creating a Request
    GetRetainedMessage (..),
    newGetRetainedMessage,

    -- * Request Lenses
    getRetainedMessage_topic,

    -- * Destructuring the Response
    GetRetainedMessageResponse (..),
    newGetRetainedMessageResponse,

    -- * Response Lenses
    getRetainedMessageResponse_payload,
    getRetainedMessageResponse_topic,
    getRetainedMessageResponse_lastModifiedTime,
    getRetainedMessageResponse_qos,
    getRetainedMessageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTData.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetRetainedMessage operation.
--
-- /See:/ 'newGetRetainedMessage' smart constructor.
data GetRetainedMessage = GetRetainedMessage'
  { -- | The topic name of the retained message to retrieve.
    topic :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRetainedMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topic', 'getRetainedMessage_topic' - The topic name of the retained message to retrieve.
newGetRetainedMessage ::
  -- | 'topic'
  Prelude.Text ->
  GetRetainedMessage
newGetRetainedMessage pTopic_ =
  GetRetainedMessage' {topic = pTopic_}

-- | The topic name of the retained message to retrieve.
getRetainedMessage_topic :: Lens.Lens' GetRetainedMessage Prelude.Text
getRetainedMessage_topic = Lens.lens (\GetRetainedMessage' {topic} -> topic) (\s@GetRetainedMessage' {} a -> s {topic = a} :: GetRetainedMessage)

instance Core.AWSRequest GetRetainedMessage where
  type
    AWSResponse GetRetainedMessage =
      GetRetainedMessageResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRetainedMessageResponse'
            Prelude.<$> (x Core..?> "payload")
            Prelude.<*> (x Core..?> "topic")
            Prelude.<*> (x Core..?> "lastModifiedTime")
            Prelude.<*> (x Core..?> "qos")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRetainedMessage

instance Prelude.NFData GetRetainedMessage

instance Core.ToHeaders GetRetainedMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetRetainedMessage where
  toPath GetRetainedMessage' {..} =
    Prelude.mconcat
      ["/retainedMessage/", Core.toBS topic]

instance Core.ToQuery GetRetainedMessage where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetRetainedMessage operation.
--
-- /See:/ 'newGetRetainedMessageResponse' smart constructor.
data GetRetainedMessageResponse = GetRetainedMessageResponse'
  { -- | The Base64-encoded message payload of the retained message body.
    payload :: Prelude.Maybe Prelude.ByteString,
    -- | The topic name to which the retained message was published.
    topic :: Prelude.Maybe Prelude.Text,
    -- | The Epoch date and time, in milliseconds, when the retained message was
    -- stored by IoT.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The quality of service (QoS) level used to publish the retained message.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRetainedMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'payload', 'getRetainedMessageResponse_payload' - The Base64-encoded message payload of the retained message body.
--
-- 'topic', 'getRetainedMessageResponse_topic' - The topic name to which the retained message was published.
--
-- 'lastModifiedTime', 'getRetainedMessageResponse_lastModifiedTime' - The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
--
-- 'qos', 'getRetainedMessageResponse_qos' - The quality of service (QoS) level used to publish the retained message.
--
-- 'httpStatus', 'getRetainedMessageResponse_httpStatus' - The response's http status code.
newGetRetainedMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRetainedMessageResponse
newGetRetainedMessageResponse pHttpStatus_ =
  GetRetainedMessageResponse'
    { payload =
        Prelude.Nothing,
      topic = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      qos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Base64-encoded message payload of the retained message body.
getRetainedMessageResponse_payload :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.ByteString)
getRetainedMessageResponse_payload = Lens.lens (\GetRetainedMessageResponse' {payload} -> payload) (\s@GetRetainedMessageResponse' {} a -> s {payload = a} :: GetRetainedMessageResponse)

-- | The topic name to which the retained message was published.
getRetainedMessageResponse_topic :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Text)
getRetainedMessageResponse_topic = Lens.lens (\GetRetainedMessageResponse' {topic} -> topic) (\s@GetRetainedMessageResponse' {} a -> s {topic = a} :: GetRetainedMessageResponse)

-- | The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
getRetainedMessageResponse_lastModifiedTime :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Integer)
getRetainedMessageResponse_lastModifiedTime = Lens.lens (\GetRetainedMessageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetRetainedMessageResponse' {} a -> s {lastModifiedTime = a} :: GetRetainedMessageResponse)

-- | The quality of service (QoS) level used to publish the retained message.
getRetainedMessageResponse_qos :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Natural)
getRetainedMessageResponse_qos = Lens.lens (\GetRetainedMessageResponse' {qos} -> qos) (\s@GetRetainedMessageResponse' {} a -> s {qos = a} :: GetRetainedMessageResponse)

-- | The response's http status code.
getRetainedMessageResponse_httpStatus :: Lens.Lens' GetRetainedMessageResponse Prelude.Int
getRetainedMessageResponse_httpStatus = Lens.lens (\GetRetainedMessageResponse' {httpStatus} -> httpStatus) (\s@GetRetainedMessageResponse' {} a -> s {httpStatus = a} :: GetRetainedMessageResponse)

instance Prelude.NFData GetRetainedMessageResponse
