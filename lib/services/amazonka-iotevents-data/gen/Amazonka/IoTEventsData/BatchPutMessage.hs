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
-- Module      : Amazonka.IoTEventsData.BatchPutMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sends a set of messages to the IoT Events system. Each message payload
-- is transformed into the input you specify (@\"inputName\"@) and ingested
-- into any detectors that monitor that input. If multiple messages are
-- sent, the order in which the messages are processed isn\'t guaranteed.
-- To guarantee ordering, you must send messages one at a time and wait for
-- a successful response.
module Amazonka.IoTEventsData.BatchPutMessage
  ( -- * Creating a Request
    BatchPutMessage (..),
    newBatchPutMessage,

    -- * Request Lenses
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
import Amazonka.IoTEventsData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchPutMessage' smart constructor.
data BatchPutMessage = BatchPutMessage'
  { -- | The list of messages to send. Each message has the following format:
    -- @\'{ \"messageId\": \"string\", \"inputName\": \"string\", \"payload\": \"string\"}\'@
    messages :: Prelude.NonEmpty Message
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
-- 'messages', 'batchPutMessage_messages' - The list of messages to send. Each message has the following format:
-- @\'{ \"messageId\": \"string\", \"inputName\": \"string\", \"payload\": \"string\"}\'@
newBatchPutMessage ::
  -- | 'messages'
  Prelude.NonEmpty Message ->
  BatchPutMessage
newBatchPutMessage pMessages_ =
  BatchPutMessage'
    { messages =
        Lens.coerced Lens.# pMessages_
    }

-- | The list of messages to send. Each message has the following format:
-- @\'{ \"messageId\": \"string\", \"inputName\": \"string\", \"payload\": \"string\"}\'@
batchPutMessage_messages :: Lens.Lens' BatchPutMessage (Prelude.NonEmpty Message)
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
            Prelude.<$> ( x
                            Data..?> "BatchPutMessageErrorEntries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchPutMessage where
  hashWithSalt _salt BatchPutMessage' {..} =
    _salt `Prelude.hashWithSalt` messages

instance Prelude.NFData BatchPutMessage where
  rnf BatchPutMessage' {..} = Prelude.rnf messages

instance Data.ToHeaders BatchPutMessage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON BatchPutMessage where
  toJSON BatchPutMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("messages" Data..= messages)]
      )

instance Data.ToPath BatchPutMessage where
  toPath = Prelude.const "/inputs/messages"

instance Data.ToQuery BatchPutMessage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchPutMessageResponse' smart constructor.
data BatchPutMessageResponse = BatchPutMessageResponse'
  { -- | A list of any errors encountered when sending the messages.
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
-- 'batchPutMessageErrorEntries', 'batchPutMessageResponse_batchPutMessageErrorEntries' - A list of any errors encountered when sending the messages.
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

-- | A list of any errors encountered when sending the messages.
batchPutMessageResponse_batchPutMessageErrorEntries :: Lens.Lens' BatchPutMessageResponse (Prelude.Maybe [BatchPutMessageErrorEntry])
batchPutMessageResponse_batchPutMessageErrorEntries = Lens.lens (\BatchPutMessageResponse' {batchPutMessageErrorEntries} -> batchPutMessageErrorEntries) (\s@BatchPutMessageResponse' {} a -> s {batchPutMessageErrorEntries = a} :: BatchPutMessageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchPutMessageResponse_httpStatus :: Lens.Lens' BatchPutMessageResponse Prelude.Int
batchPutMessageResponse_httpStatus = Lens.lens (\BatchPutMessageResponse' {httpStatus} -> httpStatus) (\s@BatchPutMessageResponse' {} a -> s {httpStatus = a} :: BatchPutMessageResponse)

instance Prelude.NFData BatchPutMessageResponse where
  rnf BatchPutMessageResponse' {..} =
    Prelude.rnf batchPutMessageErrorEntries
      `Prelude.seq` Prelude.rnf httpStatus
