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
-- Module      : Network.AWS.IoTAnalytics.BatchPutMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends messages to a channel.
module Network.AWS.IoTAnalytics.BatchPutMessage
  ( -- * Creating a Request
    BatchPutMessage (..),
    newBatchPutMessage,

    -- * Request Lenses
    batchPutMessage_channelName,
    batchPutMessage_messages,

    -- * Destructuring the Response
    BatchPutMessageResponse (..),
    newBatchPutMessageResponse,

    -- * Response Lenses
    batchPutMessageResponse_batchPutMessageErrorEntries,
    batchPutMessageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { -- | The name of the channel where the messages are sent.
    channelName :: Core.Text,
    -- | The list of messages to be sent. Each message has the format: {
    -- \"messageId\": \"string\", \"payload\": \"string\"}.
    --
    -- The field names of message payloads (data) that you send to AWS IoT
    -- Analytics:
    --
    -- -   Must contain only alphanumeric characters and undescores (_). No
    --     other special characters are allowed.
    --
    -- -   Must begin with an alphabetic character or single underscore (_).
    --
    -- -   Cannot contain hyphens (-).
    --
    -- -   In regular expression terms:
    --     \"^[A-Za-z_]([A-Za-z0-9]*|[A-Za-z0-9][A-Za-z0-9_]*)$\".
    --
    -- -   Cannot be more than 255 characters.
    --
    -- -   Are case insensitive. (Fields named foo and FOO in the same payload
    --     are considered duplicates.)
    --
    -- For example, {\"temp_01\": 29} or {\"_temp_01\": 29} are valid, but
    -- {\"temp-01\": 29}, {\"01_temp\": 29} or {\"__temp_01\": 29} are invalid
    -- in message payloads.
    messages :: [Message]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchPutMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelName', 'batchPutMessage_channelName' - The name of the channel where the messages are sent.
--
-- 'messages', 'batchPutMessage_messages' - The list of messages to be sent. Each message has the format: {
-- \"messageId\": \"string\", \"payload\": \"string\"}.
--
-- The field names of message payloads (data) that you send to AWS IoT
-- Analytics:
--
-- -   Must contain only alphanumeric characters and undescores (_). No
--     other special characters are allowed.
--
-- -   Must begin with an alphabetic character or single underscore (_).
--
-- -   Cannot contain hyphens (-).
--
-- -   In regular expression terms:
--     \"^[A-Za-z_]([A-Za-z0-9]*|[A-Za-z0-9][A-Za-z0-9_]*)$\".
--
-- -   Cannot be more than 255 characters.
--
-- -   Are case insensitive. (Fields named foo and FOO in the same payload
--     are considered duplicates.)
--
-- For example, {\"temp_01\": 29} or {\"_temp_01\": 29} are valid, but
-- {\"temp-01\": 29}, {\"01_temp\": 29} or {\"__temp_01\": 29} are invalid
-- in message payloads.
newBatchPutMessage ::
  -- | 'channelName'
  Core.Text ->
  BatchPutMessage
newBatchPutMessage pChannelName_ =
  BatchPutMessage'
    { channelName = pChannelName_,
      messages = Core.mempty
    }

-- | The name of the channel where the messages are sent.
batchPutMessage_channelName :: Lens.Lens' BatchPutMessage Core.Text
batchPutMessage_channelName = Lens.lens (\BatchPutMessage' {channelName} -> channelName) (\s@BatchPutMessage' {} a -> s {channelName = a} :: BatchPutMessage)

-- | The list of messages to be sent. Each message has the format: {
-- \"messageId\": \"string\", \"payload\": \"string\"}.
--
-- The field names of message payloads (data) that you send to AWS IoT
-- Analytics:
--
-- -   Must contain only alphanumeric characters and undescores (_). No
--     other special characters are allowed.
--
-- -   Must begin with an alphabetic character or single underscore (_).
--
-- -   Cannot contain hyphens (-).
--
-- -   In regular expression terms:
--     \"^[A-Za-z_]([A-Za-z0-9]*|[A-Za-z0-9][A-Za-z0-9_]*)$\".
--
-- -   Cannot be more than 255 characters.
--
-- -   Are case insensitive. (Fields named foo and FOO in the same payload
--     are considered duplicates.)
--
-- For example, {\"temp_01\": 29} or {\"_temp_01\": 29} are valid, but
-- {\"temp-01\": 29}, {\"01_temp\": 29} or {\"__temp_01\": 29} are invalid
-- in message payloads.
batchPutMessage_messages :: Lens.Lens' BatchPutMessage [Message]
batchPutMessage_messages = Lens.lens (\BatchPutMessage' {messages} -> messages) (\s@BatchPutMessage' {} a -> s {messages = a} :: BatchPutMessage) Core.. Lens._Coerce

instance Core.AWSRequest BatchPutMessage where
  type
    AWSResponse BatchPutMessage =
      BatchPutMessageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutMessageResponse'
            Core.<$> ( x Core..?> "batchPutMessageErrorEntries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchPutMessage

instance Core.NFData BatchPutMessage

instance Core.ToHeaders BatchPutMessage where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON BatchPutMessage where
  toJSON BatchPutMessage' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("channelName" Core..= channelName),
            Core.Just ("messages" Core..= messages)
          ]
      )

instance Core.ToPath BatchPutMessage where
  toPath = Core.const "/messages/batch"

instance Core.ToQuery BatchPutMessage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { -- | A list of any errors encountered when sending the messages to the
    -- channel.
    batchPutMessageErrorEntries :: Core.Maybe [BatchPutMessageErrorEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchPutMessageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchPutMessageErrorEntries', 'batchPutMessageResponse_batchPutMessageErrorEntries' - A list of any errors encountered when sending the messages to the
-- channel.
--
-- 'httpStatus', 'batchPutMessageResponse_httpStatus' - The response's http status code.
newBatchPutMessageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchPutMessageResponse
newBatchPutMessageResponse pHttpStatus_ =
  BatchPutMessageResponse'
    { batchPutMessageErrorEntries =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of any errors encountered when sending the messages to the
-- channel.
batchPutMessageResponse_batchPutMessageErrorEntries :: Lens.Lens' BatchPutMessageResponse (Core.Maybe [BatchPutMessageErrorEntry])
batchPutMessageResponse_batchPutMessageErrorEntries = Lens.lens (\BatchPutMessageResponse' {batchPutMessageErrorEntries} -> batchPutMessageErrorEntries) (\s@BatchPutMessageResponse' {} a -> s {batchPutMessageErrorEntries = a} :: BatchPutMessageResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchPutMessageResponse_httpStatus :: Lens.Lens' BatchPutMessageResponse Core.Int
batchPutMessageResponse_httpStatus = Lens.lens (\BatchPutMessageResponse' {httpStatus} -> httpStatus) (\s@BatchPutMessageResponse' {} a -> s {httpStatus = a} :: BatchPutMessageResponse)

instance Core.NFData BatchPutMessageResponse
