{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputDeviceTransferType
import qualified Network.AWS.Prelude as Prelude

-- | Details about the input device that is being transferred.
--
-- /See:/ 'newTransferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { -- | The type (direction) of the input device transfer.
    transferType :: Prelude.Maybe InputDeviceTransferType,
    -- | The optional message that the sender has attached to the transfer.
    message :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the input device.
    id :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID for the recipient of the input device transfer.
    targetCustomerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      message = Prelude.Nothing,
      id = Prelude.Nothing,
      targetCustomerId = Prelude.Nothing
    }

-- | The type (direction) of the input device transfer.
transferringInputDeviceSummary_transferType :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe InputDeviceTransferType)
transferringInputDeviceSummary_transferType = Lens.lens (\TransferringInputDeviceSummary' {transferType} -> transferType) (\s@TransferringInputDeviceSummary' {} a -> s {transferType = a} :: TransferringInputDeviceSummary)

-- | The optional message that the sender has attached to the transfer.
transferringInputDeviceSummary_message :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_message = Lens.lens (\TransferringInputDeviceSummary' {message} -> message) (\s@TransferringInputDeviceSummary' {} a -> s {message = a} :: TransferringInputDeviceSummary)

-- | The unique ID of the input device.
transferringInputDeviceSummary_id :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_id = Lens.lens (\TransferringInputDeviceSummary' {id} -> id) (\s@TransferringInputDeviceSummary' {} a -> s {id = a} :: TransferringInputDeviceSummary)

-- | The AWS account ID for the recipient of the input device transfer.
transferringInputDeviceSummary_targetCustomerId :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_targetCustomerId = Lens.lens (\TransferringInputDeviceSummary' {targetCustomerId} -> targetCustomerId) (\s@TransferringInputDeviceSummary' {} a -> s {targetCustomerId = a} :: TransferringInputDeviceSummary)

instance
  Prelude.FromJSON
    TransferringInputDeviceSummary
  where
  parseJSON =
    Prelude.withObject
      "TransferringInputDeviceSummary"
      ( \x ->
          TransferringInputDeviceSummary'
            Prelude.<$> (x Prelude..:? "transferType")
            Prelude.<*> (x Prelude..:? "message")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "targetCustomerId")
      )

instance
  Prelude.Hashable
    TransferringInputDeviceSummary

instance
  Prelude.NFData
    TransferringInputDeviceSummary
