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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkSendDataToDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.MessageType
import qualified Amazonka.Prelude as Prelude

-- | Information about a Sidewalk router.
--
-- /See:/ 'newSidewalkSendDataToDevice' smart constructor.
data SidewalkSendDataToDevice = SidewalkSendDataToDevice'
  { -- | The duration of time in seconds to retry sending the ACK.
    ackModeRetryDurationSecs :: Prelude.Maybe Prelude.Natural,
    messageType :: Prelude.Maybe MessageType,
    -- | The sequence number.
    seq :: Prelude.Maybe Prelude.Natural
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
-- 'ackModeRetryDurationSecs', 'sidewalkSendDataToDevice_ackModeRetryDurationSecs' - The duration of time in seconds to retry sending the ACK.
--
-- 'messageType', 'sidewalkSendDataToDevice_messageType' - Undocumented member.
--
-- 'seq', 'sidewalkSendDataToDevice_seq' - The sequence number.
newSidewalkSendDataToDevice ::
  SidewalkSendDataToDevice
newSidewalkSendDataToDevice =
  SidewalkSendDataToDevice'
    { ackModeRetryDurationSecs =
        Prelude.Nothing,
      messageType = Prelude.Nothing,
      seq = Prelude.Nothing
    }

-- | The duration of time in seconds to retry sending the ACK.
sidewalkSendDataToDevice_ackModeRetryDurationSecs :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe Prelude.Natural)
sidewalkSendDataToDevice_ackModeRetryDurationSecs = Lens.lens (\SidewalkSendDataToDevice' {ackModeRetryDurationSecs} -> ackModeRetryDurationSecs) (\s@SidewalkSendDataToDevice' {} a -> s {ackModeRetryDurationSecs = a} :: SidewalkSendDataToDevice)

-- | Undocumented member.
sidewalkSendDataToDevice_messageType :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe MessageType)
sidewalkSendDataToDevice_messageType = Lens.lens (\SidewalkSendDataToDevice' {messageType} -> messageType) (\s@SidewalkSendDataToDevice' {} a -> s {messageType = a} :: SidewalkSendDataToDevice)

-- | The sequence number.
sidewalkSendDataToDevice_seq :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe Prelude.Natural)
sidewalkSendDataToDevice_seq = Lens.lens (\SidewalkSendDataToDevice' {seq} -> seq) (\s@SidewalkSendDataToDevice' {} a -> s {seq = a} :: SidewalkSendDataToDevice)

instance Prelude.Hashable SidewalkSendDataToDevice where
  hashWithSalt _salt SidewalkSendDataToDevice' {..} =
    _salt
      `Prelude.hashWithSalt` ackModeRetryDurationSecs
      `Prelude.hashWithSalt` messageType
      `Prelude.hashWithSalt` seq

instance Prelude.NFData SidewalkSendDataToDevice where
  rnf SidewalkSendDataToDevice' {..} =
    Prelude.rnf ackModeRetryDurationSecs
      `Prelude.seq` Prelude.rnf messageType
      `Prelude.seq` Prelude.rnf seq

instance Data.ToJSON SidewalkSendDataToDevice where
  toJSON SidewalkSendDataToDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AckModeRetryDurationSecs" Data..=)
              Prelude.<$> ackModeRetryDurationSecs,
            ("MessageType" Data..=) Prelude.<$> messageType,
            ("Seq" Data..=) Prelude.<$> seq
          ]
      )
