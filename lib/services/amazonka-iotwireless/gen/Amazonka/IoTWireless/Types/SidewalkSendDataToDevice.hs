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
-- Module      : Amazonka.IoTWireless.Types.SidewalkSendDataToDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkSendDataToDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.MessageType
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk router.
--
-- /See:/ 'newSidewalkSendDataToDevice' smart constructor.
data SidewalkSendDataToDevice = SidewalkSendDataToDevice'
  { -- | The sequence number.
    seq :: Prelude.Maybe Prelude.Natural,
    messageType :: Prelude.Maybe MessageType,
    -- | The duration of time in seconds for which you want to retry sending the
    -- ACK.
    ackModeRetryDurationSecs :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkSendDataToDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'seq', 'sidewalkSendDataToDevice_seq' - The sequence number.
--
-- 'messageType', 'sidewalkSendDataToDevice_messageType' - Undocumented member.
--
-- 'ackModeRetryDurationSecs', 'sidewalkSendDataToDevice_ackModeRetryDurationSecs' - The duration of time in seconds for which you want to retry sending the
-- ACK.
newSidewalkSendDataToDevice ::
  SidewalkSendDataToDevice
newSidewalkSendDataToDevice =
  SidewalkSendDataToDevice'
    { seq = Prelude.Nothing,
      messageType = Prelude.Nothing,
      ackModeRetryDurationSecs = Prelude.Nothing
    }

-- | The sequence number.
sidewalkSendDataToDevice_seq :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe Prelude.Natural)
sidewalkSendDataToDevice_seq = Lens.lens (\SidewalkSendDataToDevice' {seq} -> seq) (\s@SidewalkSendDataToDevice' {} a -> s {seq = a} :: SidewalkSendDataToDevice)

-- | Undocumented member.
sidewalkSendDataToDevice_messageType :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe MessageType)
sidewalkSendDataToDevice_messageType = Lens.lens (\SidewalkSendDataToDevice' {messageType} -> messageType) (\s@SidewalkSendDataToDevice' {} a -> s {messageType = a} :: SidewalkSendDataToDevice)

-- | The duration of time in seconds for which you want to retry sending the
-- ACK.
sidewalkSendDataToDevice_ackModeRetryDurationSecs :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe Prelude.Natural)
sidewalkSendDataToDevice_ackModeRetryDurationSecs = Lens.lens (\SidewalkSendDataToDevice' {ackModeRetryDurationSecs} -> ackModeRetryDurationSecs) (\s@SidewalkSendDataToDevice' {} a -> s {ackModeRetryDurationSecs = a} :: SidewalkSendDataToDevice)

instance Prelude.Hashable SidewalkSendDataToDevice where
  hashWithSalt _salt SidewalkSendDataToDevice' {..} =
    _salt `Prelude.hashWithSalt` seq
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` ackModeRetryDurationSecs

instance Prelude.NFData SidewalkSendDataToDevice where
  rnf SidewalkSendDataToDevice' {..} =
    Prelude.rnf seq
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf ackModeRetryDurationSecs

instance Core.ToJSON SidewalkSendDataToDevice where
  toJSON SidewalkSendDataToDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Seq" Core..=) Prelude.<$> seq,
            ("MessageType" Core..=) Prelude.<$> messageType,
            ("AckModeRetryDurationSecs" Core..=)
              Prelude.<$> ackModeRetryDurationSecs
          ]
      )
