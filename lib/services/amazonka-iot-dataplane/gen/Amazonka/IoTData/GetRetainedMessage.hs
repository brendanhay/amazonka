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
-- Module      : Amazonka.IoTData.GetRetainedMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <http://aws.amazon.com/iot-core/pricing/#Messaging Amazon Web Services IoT Core pricing - Messaging>.
module Amazonka.IoTData.GetRetainedMessage
  ( -- * Creating a Request
    GetRetainedMessage (..),
    newGetRetainedMessage,

    -- * Request Lenses
    getRetainedMessage_topic,

    -- * Destructuring the Response
    GetRetainedMessageResponse (..),
    newGetRetainedMessageResponse,

    -- * Response Lenses
    getRetainedMessageResponse_lastModifiedTime,
    getRetainedMessageResponse_payload,
    getRetainedMessageResponse_qos,
    getRetainedMessageResponse_topic,
    getRetainedMessageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRetainedMessageResponse'
            Prelude.<$> (x Data..?> "lastModifiedTime")
            Prelude.<*> (x Data..?> "payload")
            Prelude.<*> (x Data..?> "qos")
            Prelude.<*> (x Data..?> "topic")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRetainedMessage where
  hashWithSalt _salt GetRetainedMessage' {..} =
    _salt `Prelude.hashWithSalt` topic

instance Prelude.NFData GetRetainedMessage where
  rnf GetRetainedMessage' {..} = Prelude.rnf topic

instance Data.ToHeaders GetRetainedMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetRetainedMessage where
  toPath GetRetainedMessage' {..} =
    Prelude.mconcat
      ["/retainedMessage/", Data.toBS topic]

instance Data.ToQuery GetRetainedMessage where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetRetainedMessage operation.
--
-- /See:/ 'newGetRetainedMessageResponse' smart constructor.
data GetRetainedMessageResponse = GetRetainedMessageResponse'
  { -- | The Epoch date and time, in milliseconds, when the retained message was
    -- stored by IoT.
    lastModifiedTime :: Prelude.Maybe Prelude.Integer,
    -- | The Base64-encoded message payload of the retained message body.
    payload :: Prelude.Maybe Data.Base64,
    -- | The quality of service (QoS) level used to publish the retained message.
    qos :: Prelude.Maybe Prelude.Natural,
    -- | The topic name to which the retained message was published.
    topic :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRetainedMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'getRetainedMessageResponse_lastModifiedTime' - The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
--
-- 'payload', 'getRetainedMessageResponse_payload' - The Base64-encoded message payload of the retained message body.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'qos', 'getRetainedMessageResponse_qos' - The quality of service (QoS) level used to publish the retained message.
--
-- 'topic', 'getRetainedMessageResponse_topic' - The topic name to which the retained message was published.
--
-- 'httpStatus', 'getRetainedMessageResponse_httpStatus' - The response's http status code.
newGetRetainedMessageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRetainedMessageResponse
newGetRetainedMessageResponse pHttpStatus_ =
  GetRetainedMessageResponse'
    { lastModifiedTime =
        Prelude.Nothing,
      payload = Prelude.Nothing,
      qos = Prelude.Nothing,
      topic = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Epoch date and time, in milliseconds, when the retained message was
-- stored by IoT.
getRetainedMessageResponse_lastModifiedTime :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Integer)
getRetainedMessageResponse_lastModifiedTime = Lens.lens (\GetRetainedMessageResponse' {lastModifiedTime} -> lastModifiedTime) (\s@GetRetainedMessageResponse' {} a -> s {lastModifiedTime = a} :: GetRetainedMessageResponse)

-- | The Base64-encoded message payload of the retained message body.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
getRetainedMessageResponse_payload :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.ByteString)
getRetainedMessageResponse_payload = Lens.lens (\GetRetainedMessageResponse' {payload} -> payload) (\s@GetRetainedMessageResponse' {} a -> s {payload = a} :: GetRetainedMessageResponse) Prelude.. Lens.mapping Data._Base64

-- | The quality of service (QoS) level used to publish the retained message.
getRetainedMessageResponse_qos :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Natural)
getRetainedMessageResponse_qos = Lens.lens (\GetRetainedMessageResponse' {qos} -> qos) (\s@GetRetainedMessageResponse' {} a -> s {qos = a} :: GetRetainedMessageResponse)

-- | The topic name to which the retained message was published.
getRetainedMessageResponse_topic :: Lens.Lens' GetRetainedMessageResponse (Prelude.Maybe Prelude.Text)
getRetainedMessageResponse_topic = Lens.lens (\GetRetainedMessageResponse' {topic} -> topic) (\s@GetRetainedMessageResponse' {} a -> s {topic = a} :: GetRetainedMessageResponse)

-- | The response's http status code.
getRetainedMessageResponse_httpStatus :: Lens.Lens' GetRetainedMessageResponse Prelude.Int
getRetainedMessageResponse_httpStatus = Lens.lens (\GetRetainedMessageResponse' {httpStatus} -> httpStatus) (\s@GetRetainedMessageResponse' {} a -> s {httpStatus = a} :: GetRetainedMessageResponse)

instance Prelude.NFData GetRetainedMessageResponse where
  rnf GetRetainedMessageResponse' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf payload
      `Prelude.seq` Prelude.rnf qos
      `Prelude.seq` Prelude.rnf topic
      `Prelude.seq` Prelude.rnf httpStatus
