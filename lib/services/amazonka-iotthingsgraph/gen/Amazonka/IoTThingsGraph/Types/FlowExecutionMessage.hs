{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTThingsGraph.Types.FlowExecutionMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTThingsGraph.Types.FlowExecutionMessage where

import qualified Amazonka.Core as Core
import Amazonka.IoTThingsGraph.Types.FlowExecutionEventType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a flow event.
--
-- /See:/ 'newFlowExecutionMessage' smart constructor.
data FlowExecutionMessage = FlowExecutionMessage'
  { -- | The type of flow event .
    eventType :: Prelude.Maybe FlowExecutionEventType,
    -- | The date and time when the message was last updated.
    timestamp :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier of the message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | A string containing information about the flow event.
    payload :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FlowExecutionMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'flowExecutionMessage_eventType' - The type of flow event .
--
-- 'timestamp', 'flowExecutionMessage_timestamp' - The date and time when the message was last updated.
--
-- 'messageId', 'flowExecutionMessage_messageId' - The unique identifier of the message.
--
-- 'payload', 'flowExecutionMessage_payload' - A string containing information about the flow event.
newFlowExecutionMessage ::
  FlowExecutionMessage
newFlowExecutionMessage =
  FlowExecutionMessage'
    { eventType = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      messageId = Prelude.Nothing,
      payload = Prelude.Nothing
    }

-- | The type of flow event .
flowExecutionMessage_eventType :: Lens.Lens' FlowExecutionMessage (Prelude.Maybe FlowExecutionEventType)
flowExecutionMessage_eventType = Lens.lens (\FlowExecutionMessage' {eventType} -> eventType) (\s@FlowExecutionMessage' {} a -> s {eventType = a} :: FlowExecutionMessage)

-- | The date and time when the message was last updated.
flowExecutionMessage_timestamp :: Lens.Lens' FlowExecutionMessage (Prelude.Maybe Prelude.UTCTime)
flowExecutionMessage_timestamp = Lens.lens (\FlowExecutionMessage' {timestamp} -> timestamp) (\s@FlowExecutionMessage' {} a -> s {timestamp = a} :: FlowExecutionMessage) Prelude.. Lens.mapping Core._Time

-- | The unique identifier of the message.
flowExecutionMessage_messageId :: Lens.Lens' FlowExecutionMessage (Prelude.Maybe Prelude.Text)
flowExecutionMessage_messageId = Lens.lens (\FlowExecutionMessage' {messageId} -> messageId) (\s@FlowExecutionMessage' {} a -> s {messageId = a} :: FlowExecutionMessage)

-- | A string containing information about the flow event.
flowExecutionMessage_payload :: Lens.Lens' FlowExecutionMessage (Prelude.Maybe Prelude.Text)
flowExecutionMessage_payload = Lens.lens (\FlowExecutionMessage' {payload} -> payload) (\s@FlowExecutionMessage' {} a -> s {payload = a} :: FlowExecutionMessage)

instance Core.FromJSON FlowExecutionMessage where
  parseJSON =
    Core.withObject
      "FlowExecutionMessage"
      ( \x ->
          FlowExecutionMessage'
            Prelude.<$> (x Core..:? "eventType")
            Prelude.<*> (x Core..:? "timestamp")
            Prelude.<*> (x Core..:? "messageId")
            Prelude.<*> (x Core..:? "payload")
      )

instance Prelude.Hashable FlowExecutionMessage where
  hashWithSalt _salt FlowExecutionMessage' {..} =
    _salt `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` payload

instance Prelude.NFData FlowExecutionMessage where
  rnf FlowExecutionMessage' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf payload
