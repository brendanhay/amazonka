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
-- Module      : Network.AWS.IoTWireless.Types.SidewalkSendDataToDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTWireless.Types.SidewalkSendDataToDevice where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTWireless.Types.MessageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a Sidewalk router.
--
-- /See:/ 'newSidewalkSendDataToDevice' smart constructor.
data SidewalkSendDataToDevice = SidewalkSendDataToDevice'
  { messageType :: Prelude.Maybe MessageType,
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
-- 'messageType', 'sidewalkSendDataToDevice_messageType' - Undocumented member.
--
-- 'seq', 'sidewalkSendDataToDevice_seq' - The sequence number.
newSidewalkSendDataToDevice ::
  SidewalkSendDataToDevice
newSidewalkSendDataToDevice =
  SidewalkSendDataToDevice'
    { messageType =
        Prelude.Nothing,
      seq = Prelude.Nothing
    }

-- | Undocumented member.
sidewalkSendDataToDevice_messageType :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe MessageType)
sidewalkSendDataToDevice_messageType = Lens.lens (\SidewalkSendDataToDevice' {messageType} -> messageType) (\s@SidewalkSendDataToDevice' {} a -> s {messageType = a} :: SidewalkSendDataToDevice)

-- | The sequence number.
sidewalkSendDataToDevice_seq :: Lens.Lens' SidewalkSendDataToDevice (Prelude.Maybe Prelude.Natural)
sidewalkSendDataToDevice_seq = Lens.lens (\SidewalkSendDataToDevice' {seq} -> seq) (\s@SidewalkSendDataToDevice' {} a -> s {seq = a} :: SidewalkSendDataToDevice)

instance Prelude.Hashable SidewalkSendDataToDevice

instance Prelude.NFData SidewalkSendDataToDevice

instance Core.ToJSON SidewalkSendDataToDevice where
  toJSON SidewalkSendDataToDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MessageType" Core..=) Prelude.<$> messageType,
            ("Seq" Core..=) Prelude.<$> seq
          ]
      )
