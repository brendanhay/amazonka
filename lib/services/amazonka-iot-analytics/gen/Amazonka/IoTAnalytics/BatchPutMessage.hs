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
-- Module      : Amazonka.IoTAnalytics.BatchPutMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends messages to a channel.
module Amazonka.IoTAnalytics.BatchPutMessage
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { -- | The name of the channel where the messages are sent.
    channelName :: Prelude.Text,
    -- | The list of messages to be sent. Each message has the format: {
    -- \"messageId\": \"string\", \"payload\": \"string\"}.
    --
    -- The field names of message payloads (data) that you send to IoT
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- The field names of message payloads (data) that you send to IoT
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
  Prelude.Text ->
  BatchPutMessage
newBatchPutMessage pChannelName_ =
  BatchPutMessage'
    { channelName = pChannelName_,
      messages = Prelude.mempty
    }

-- | The name of the channel where the messages are sent.
batchPutMessage_channelName :: Lens.Lens' BatchPutMessage Prelude.Text
batchPutMessage_channelName = Lens.lens (\BatchPutMessage' {channelName} -> channelName) (\s@BatchPutMessage' {} a -> s {channelName = a} :: BatchPutMessage)

-- | The list of messages to be sent. Each message has the format: {
-- \"messageId\": \"string\", \"payload\": \"string\"}.
--
-- The field names of message payloads (data) that you send to IoT
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
batchPutMessage_messages = Lens.lens (\BatchPutMessage' {messages} -> messages) (\s@BatchPutMessage' {} a -> s {messages = a} :: BatchPutMessage) Prelude.. Lens.coerced

instance Core.AWSRequest BatchPutMessage where
  type
    AWSResponse BatchPutMessage =
      BatchPutMessageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchPutMessageResponse'
            Prelude.<$> ( x Data..?> "batchPutMessageErrorEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutMessage where
  hashWithSalt _salt BatchPutMessage' {..} =
    _salt `Prelude.hashWithSalt` channelName
      `Prelude.hashWithSalt` messages

instance Prelude.NFData BatchPutMessage where
  rnf BatchPutMessage' {..} =
    Prelude.rnf channelName
      `Prelude.seq` Prelude.rnf messages

instance Data.ToHeaders BatchPutMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchPutMessage where
  toJSON BatchPutMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("channelName" Data..= channelName),
            Prelude.Just ("messages" Data..= messages)
          ]
      )

instance Data.ToPath BatchPutMessage where
  toPath = Prelude.const "/messages/batch"

instance Data.ToQuery BatchPutMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { -- | A list of any errors encountered when sending the messages to the
    -- channel.
    batchPutMessageErrorEntries :: Prelude.Maybe [BatchPutMessageErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchPutMessageResponse
newBatchPutMessageResponse pHttpStatus_ =
  BatchPutMessageResponse'
    { batchPutMessageErrorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of any errors encountered when sending the messages to the
-- channel.
batchPutMessageResponse_batchPutMessageErrorEntries :: Lens.Lens' BatchPutMessageResponse (Prelude.Maybe [BatchPutMessageErrorEntry])
batchPutMessageResponse_batchPutMessageErrorEntries = Lens.lens (\BatchPutMessageResponse' {batchPutMessageErrorEntries} -> batchPutMessageErrorEntries) (\s@BatchPutMessageResponse' {} a -> s {batchPutMessageErrorEntries = a} :: BatchPutMessageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutMessageResponse_httpStatus :: Lens.Lens' BatchPutMessageResponse Prelude.Int
batchPutMessageResponse_httpStatus = Lens.lens (\BatchPutMessageResponse' {httpStatus} -> httpStatus) (\s@BatchPutMessageResponse' {} a -> s {httpStatus = a} :: BatchPutMessageResponse)

instance Prelude.NFData BatchPutMessageResponse where
  rnf BatchPutMessageResponse' {..} =
    Prelude.rnf batchPutMessageErrorEntries
      `Prelude.seq` Prelude.rnf httpStatus
