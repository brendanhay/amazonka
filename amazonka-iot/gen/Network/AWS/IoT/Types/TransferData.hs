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
-- Module      : Network.AWS.IoT.Types.TransferData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TransferData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Data used to transfer a certificate to an AWS account.
--
-- /See:/ 'newTransferData' smart constructor.
data TransferData = TransferData'
  { -- | The date the transfer took place.
    transferDate :: Prelude.Maybe Prelude.POSIX,
    -- | The transfer message.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The date the transfer was accepted.
    acceptDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date the transfer was rejected.
    rejectDate :: Prelude.Maybe Prelude.POSIX,
    -- | The reason why the transfer was rejected.
    rejectReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TransferData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transferDate', 'transferData_transferDate' - The date the transfer took place.
--
-- 'transferMessage', 'transferData_transferMessage' - The transfer message.
--
-- 'acceptDate', 'transferData_acceptDate' - The date the transfer was accepted.
--
-- 'rejectDate', 'transferData_rejectDate' - The date the transfer was rejected.
--
-- 'rejectReason', 'transferData_rejectReason' - The reason why the transfer was rejected.
newTransferData ::
  TransferData
newTransferData =
  TransferData'
    { transferDate = Prelude.Nothing,
      transferMessage = Prelude.Nothing,
      acceptDate = Prelude.Nothing,
      rejectDate = Prelude.Nothing,
      rejectReason = Prelude.Nothing
    }

-- | The date the transfer took place.
transferData_transferDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_transferDate = Lens.lens (\TransferData' {transferDate} -> transferDate) (\s@TransferData' {} a -> s {transferDate = a} :: TransferData) Prelude.. Lens.mapping Prelude._Time

-- | The transfer message.
transferData_transferMessage :: Lens.Lens' TransferData (Prelude.Maybe Prelude.Text)
transferData_transferMessage = Lens.lens (\TransferData' {transferMessage} -> transferMessage) (\s@TransferData' {} a -> s {transferMessage = a} :: TransferData)

-- | The date the transfer was accepted.
transferData_acceptDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_acceptDate = Lens.lens (\TransferData' {acceptDate} -> acceptDate) (\s@TransferData' {} a -> s {acceptDate = a} :: TransferData) Prelude.. Lens.mapping Prelude._Time

-- | The date the transfer was rejected.
transferData_rejectDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_rejectDate = Lens.lens (\TransferData' {rejectDate} -> rejectDate) (\s@TransferData' {} a -> s {rejectDate = a} :: TransferData) Prelude.. Lens.mapping Prelude._Time

-- | The reason why the transfer was rejected.
transferData_rejectReason :: Lens.Lens' TransferData (Prelude.Maybe Prelude.Text)
transferData_rejectReason = Lens.lens (\TransferData' {rejectReason} -> rejectReason) (\s@TransferData' {} a -> s {rejectReason = a} :: TransferData)

instance Prelude.FromJSON TransferData where
  parseJSON =
    Prelude.withObject
      "TransferData"
      ( \x ->
          TransferData'
            Prelude.<$> (x Prelude..:? "transferDate")
            Prelude.<*> (x Prelude..:? "transferMessage")
            Prelude.<*> (x Prelude..:? "acceptDate")
            Prelude.<*> (x Prelude..:? "rejectDate")
            Prelude.<*> (x Prelude..:? "rejectReason")
      )

instance Prelude.Hashable TransferData

instance Prelude.NFData TransferData
