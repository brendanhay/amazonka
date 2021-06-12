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
-- Module      : Network.AWS.MediaLive.Types.TransferringInputDeviceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.TransferringInputDeviceSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceTransferType

-- | Details about the input device that is being transferred.
--
-- /See:/ 'newTransferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { -- | The type (direction) of the input device transfer.
    transferType :: Core.Maybe InputDeviceTransferType,
    -- | The optional message that the sender has attached to the transfer.
    message :: Core.Maybe Core.Text,
    -- | The unique ID of the input device.
    id :: Core.Maybe Core.Text,
    -- | The AWS account ID for the recipient of the input device transfer.
    targetCustomerId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TransferringInputDeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferType', 'transferringInputDeviceSummary_transferType' - The type (direction) of the input device transfer.
--
-- 'message', 'transferringInputDeviceSummary_message' - The optional message that the sender has attached to the transfer.
--
-- 'id', 'transferringInputDeviceSummary_id' - The unique ID of the input device.
--
-- 'targetCustomerId', 'transferringInputDeviceSummary_targetCustomerId' - The AWS account ID for the recipient of the input device transfer.
newTransferringInputDeviceSummary ::
  TransferringInputDeviceSummary
newTransferringInputDeviceSummary =
  TransferringInputDeviceSummary'
    { transferType =
        Core.Nothing,
      message = Core.Nothing,
      id = Core.Nothing,
      targetCustomerId = Core.Nothing
    }

-- | The type (direction) of the input device transfer.
transferringInputDeviceSummary_transferType :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe InputDeviceTransferType)
transferringInputDeviceSummary_transferType = Lens.lens (\TransferringInputDeviceSummary' {transferType} -> transferType) (\s@TransferringInputDeviceSummary' {} a -> s {transferType = a} :: TransferringInputDeviceSummary)

-- | The optional message that the sender has attached to the transfer.
transferringInputDeviceSummary_message :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
transferringInputDeviceSummary_message = Lens.lens (\TransferringInputDeviceSummary' {message} -> message) (\s@TransferringInputDeviceSummary' {} a -> s {message = a} :: TransferringInputDeviceSummary)

-- | The unique ID of the input device.
transferringInputDeviceSummary_id :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
transferringInputDeviceSummary_id = Lens.lens (\TransferringInputDeviceSummary' {id} -> id) (\s@TransferringInputDeviceSummary' {} a -> s {id = a} :: TransferringInputDeviceSummary)

-- | The AWS account ID for the recipient of the input device transfer.
transferringInputDeviceSummary_targetCustomerId :: Lens.Lens' TransferringInputDeviceSummary (Core.Maybe Core.Text)
transferringInputDeviceSummary_targetCustomerId = Lens.lens (\TransferringInputDeviceSummary' {targetCustomerId} -> targetCustomerId) (\s@TransferringInputDeviceSummary' {} a -> s {targetCustomerId = a} :: TransferringInputDeviceSummary)

instance Core.FromJSON TransferringInputDeviceSummary where
  parseJSON =
    Core.withObject
      "TransferringInputDeviceSummary"
      ( \x ->
          TransferringInputDeviceSummary'
            Core.<$> (x Core..:? "transferType")
            Core.<*> (x Core..:? "message")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "targetCustomerId")
      )

instance Core.Hashable TransferringInputDeviceSummary

instance Core.NFData TransferringInputDeviceSummary
