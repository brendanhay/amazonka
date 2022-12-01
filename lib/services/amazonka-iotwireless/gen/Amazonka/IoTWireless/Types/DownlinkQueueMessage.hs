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
-- Module      : Amazonka.IoTWireless.Types.DownlinkQueueMessage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DownlinkQueueMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice
import qualified Amazonka.Prelude as Prelude

-- | The message in the downlink queue.
--
-- /See:/ 'newDownlinkQueueMessage' smart constructor.
data DownlinkQueueMessage = DownlinkQueueMessage'
  { loRaWAN :: Prelude.Maybe LoRaWANSendDataToDevice,
    -- | The message ID assigned by IoT Wireless to each downlink message, which
    -- helps identify the message.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The time at which Iot Wireless received the downlink message.
    receivedAt :: Prelude.Maybe Prelude.Text,
    -- | The transmit mode to use for sending data to the wireless device. This
    -- can be @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
    transmitMode :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DownlinkQueueMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loRaWAN', 'downlinkQueueMessage_loRaWAN' - Undocumented member.
--
-- 'messageId', 'downlinkQueueMessage_messageId' - The message ID assigned by IoT Wireless to each downlink message, which
-- helps identify the message.
--
-- 'receivedAt', 'downlinkQueueMessage_receivedAt' - The time at which Iot Wireless received the downlink message.
--
-- 'transmitMode', 'downlinkQueueMessage_transmitMode' - The transmit mode to use for sending data to the wireless device. This
-- can be @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
newDownlinkQueueMessage ::
  DownlinkQueueMessage
newDownlinkQueueMessage =
  DownlinkQueueMessage'
    { loRaWAN = Prelude.Nothing,
      messageId = Prelude.Nothing,
      receivedAt = Prelude.Nothing,
      transmitMode = Prelude.Nothing
    }

-- | Undocumented member.
downlinkQueueMessage_loRaWAN :: Lens.Lens' DownlinkQueueMessage (Prelude.Maybe LoRaWANSendDataToDevice)
downlinkQueueMessage_loRaWAN = Lens.lens (\DownlinkQueueMessage' {loRaWAN} -> loRaWAN) (\s@DownlinkQueueMessage' {} a -> s {loRaWAN = a} :: DownlinkQueueMessage)

-- | The message ID assigned by IoT Wireless to each downlink message, which
-- helps identify the message.
downlinkQueueMessage_messageId :: Lens.Lens' DownlinkQueueMessage (Prelude.Maybe Prelude.Text)
downlinkQueueMessage_messageId = Lens.lens (\DownlinkQueueMessage' {messageId} -> messageId) (\s@DownlinkQueueMessage' {} a -> s {messageId = a} :: DownlinkQueueMessage)

-- | The time at which Iot Wireless received the downlink message.
downlinkQueueMessage_receivedAt :: Lens.Lens' DownlinkQueueMessage (Prelude.Maybe Prelude.Text)
downlinkQueueMessage_receivedAt = Lens.lens (\DownlinkQueueMessage' {receivedAt} -> receivedAt) (\s@DownlinkQueueMessage' {} a -> s {receivedAt = a} :: DownlinkQueueMessage)

-- | The transmit mode to use for sending data to the wireless device. This
-- can be @0@ for UM (unacknowledge mode) or @1@ for AM (acknowledge mode).
downlinkQueueMessage_transmitMode :: Lens.Lens' DownlinkQueueMessage (Prelude.Maybe Prelude.Natural)
downlinkQueueMessage_transmitMode = Lens.lens (\DownlinkQueueMessage' {transmitMode} -> transmitMode) (\s@DownlinkQueueMessage' {} a -> s {transmitMode = a} :: DownlinkQueueMessage)

instance Core.FromJSON DownlinkQueueMessage where
  parseJSON =
    Core.withObject
      "DownlinkQueueMessage"
      ( \x ->
          DownlinkQueueMessage'
            Prelude.<$> (x Core..:? "LoRaWAN")
            Prelude.<*> (x Core..:? "MessageId")
            Prelude.<*> (x Core..:? "ReceivedAt")
            Prelude.<*> (x Core..:? "TransmitMode")
      )

instance Prelude.Hashable DownlinkQueueMessage where
  hashWithSalt _salt DownlinkQueueMessage' {..} =
    _salt `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` receivedAt
      `Prelude.hashWithSalt` transmitMode

instance Prelude.NFData DownlinkQueueMessage where
  rnf DownlinkQueueMessage' {..} =
    Prelude.rnf loRaWAN
      `Prelude.seq` Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf receivedAt
      `Prelude.seq` Prelude.rnf transmitMode
