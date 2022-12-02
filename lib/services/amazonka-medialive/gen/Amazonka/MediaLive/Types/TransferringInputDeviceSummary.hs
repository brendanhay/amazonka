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
-- Module      : Amazonka.MediaLive.Types.TransferringInputDeviceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.TransferringInputDeviceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputDeviceTransferType
import qualified Amazonka.Prelude as Prelude

-- | Details about the input device that is being transferred.
--
-- /See:/ 'newTransferringInputDeviceSummary' smart constructor.
data TransferringInputDeviceSummary = TransferringInputDeviceSummary'
  { -- | The optional message that the sender has attached to the transfer.
    message :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID for the recipient of the input device transfer.
    targetCustomerId :: Prelude.Maybe Prelude.Text,
    -- | The type (direction) of the input device transfer.
    transferType :: Prelude.Maybe InputDeviceTransferType,
    -- | The unique ID of the input device.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransferringInputDeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'transferringInputDeviceSummary_message' - The optional message that the sender has attached to the transfer.
--
-- 'targetCustomerId', 'transferringInputDeviceSummary_targetCustomerId' - The AWS account ID for the recipient of the input device transfer.
--
-- 'transferType', 'transferringInputDeviceSummary_transferType' - The type (direction) of the input device transfer.
--
-- 'id', 'transferringInputDeviceSummary_id' - The unique ID of the input device.
newTransferringInputDeviceSummary ::
  TransferringInputDeviceSummary
newTransferringInputDeviceSummary =
  TransferringInputDeviceSummary'
    { message =
        Prelude.Nothing,
      targetCustomerId = Prelude.Nothing,
      transferType = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The optional message that the sender has attached to the transfer.
transferringInputDeviceSummary_message :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_message = Lens.lens (\TransferringInputDeviceSummary' {message} -> message) (\s@TransferringInputDeviceSummary' {} a -> s {message = a} :: TransferringInputDeviceSummary)

-- | The AWS account ID for the recipient of the input device transfer.
transferringInputDeviceSummary_targetCustomerId :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_targetCustomerId = Lens.lens (\TransferringInputDeviceSummary' {targetCustomerId} -> targetCustomerId) (\s@TransferringInputDeviceSummary' {} a -> s {targetCustomerId = a} :: TransferringInputDeviceSummary)

-- | The type (direction) of the input device transfer.
transferringInputDeviceSummary_transferType :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe InputDeviceTransferType)
transferringInputDeviceSummary_transferType = Lens.lens (\TransferringInputDeviceSummary' {transferType} -> transferType) (\s@TransferringInputDeviceSummary' {} a -> s {transferType = a} :: TransferringInputDeviceSummary)

-- | The unique ID of the input device.
transferringInputDeviceSummary_id :: Lens.Lens' TransferringInputDeviceSummary (Prelude.Maybe Prelude.Text)
transferringInputDeviceSummary_id = Lens.lens (\TransferringInputDeviceSummary' {id} -> id) (\s@TransferringInputDeviceSummary' {} a -> s {id = a} :: TransferringInputDeviceSummary)

instance Data.FromJSON TransferringInputDeviceSummary where
  parseJSON =
    Data.withObject
      "TransferringInputDeviceSummary"
      ( \x ->
          TransferringInputDeviceSummary'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "targetCustomerId")
            Prelude.<*> (x Data..:? "transferType")
            Prelude.<*> (x Data..:? "id")
      )

instance
  Prelude.Hashable
    TransferringInputDeviceSummary
  where
  hashWithSalt
    _salt
    TransferringInputDeviceSummary' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` targetCustomerId
        `Prelude.hashWithSalt` transferType
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    TransferringInputDeviceSummary
  where
  rnf TransferringInputDeviceSummary' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf targetCustomerId
      `Prelude.seq` Prelude.rnf transferType
      `Prelude.seq` Prelude.rnf id
