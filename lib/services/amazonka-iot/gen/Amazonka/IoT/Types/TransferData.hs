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
-- Module      : Amazonka.IoT.Types.TransferData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TransferData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Data used to transfer a certificate to an Amazon Web Services account.
--
-- /See:/ 'newTransferData' smart constructor.
data TransferData = TransferData'
  { -- | The date the transfer took place.
    transferDate :: Prelude.Maybe Data.POSIX,
    -- | The reason why the transfer was rejected.
    rejectReason :: Prelude.Maybe Prelude.Text,
    -- | The transfer message.
    transferMessage :: Prelude.Maybe Prelude.Text,
    -- | The date the transfer was rejected.
    rejectDate :: Prelude.Maybe Data.POSIX,
    -- | The date the transfer was accepted.
    acceptDate :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'rejectReason', 'transferData_rejectReason' - The reason why the transfer was rejected.
--
-- 'transferMessage', 'transferData_transferMessage' - The transfer message.
--
-- 'rejectDate', 'transferData_rejectDate' - The date the transfer was rejected.
--
-- 'acceptDate', 'transferData_acceptDate' - The date the transfer was accepted.
newTransferData ::
  TransferData
newTransferData =
  TransferData'
    { transferDate = Prelude.Nothing,
      rejectReason = Prelude.Nothing,
      transferMessage = Prelude.Nothing,
      rejectDate = Prelude.Nothing,
      acceptDate = Prelude.Nothing
    }

-- | The date the transfer took place.
transferData_transferDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_transferDate = Lens.lens (\TransferData' {transferDate} -> transferDate) (\s@TransferData' {} a -> s {transferDate = a} :: TransferData) Prelude.. Lens.mapping Data._Time

-- | The reason why the transfer was rejected.
transferData_rejectReason :: Lens.Lens' TransferData (Prelude.Maybe Prelude.Text)
transferData_rejectReason = Lens.lens (\TransferData' {rejectReason} -> rejectReason) (\s@TransferData' {} a -> s {rejectReason = a} :: TransferData)

-- | The transfer message.
transferData_transferMessage :: Lens.Lens' TransferData (Prelude.Maybe Prelude.Text)
transferData_transferMessage = Lens.lens (\TransferData' {transferMessage} -> transferMessage) (\s@TransferData' {} a -> s {transferMessage = a} :: TransferData)

-- | The date the transfer was rejected.
transferData_rejectDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_rejectDate = Lens.lens (\TransferData' {rejectDate} -> rejectDate) (\s@TransferData' {} a -> s {rejectDate = a} :: TransferData) Prelude.. Lens.mapping Data._Time

-- | The date the transfer was accepted.
transferData_acceptDate :: Lens.Lens' TransferData (Prelude.Maybe Prelude.UTCTime)
transferData_acceptDate = Lens.lens (\TransferData' {acceptDate} -> acceptDate) (\s@TransferData' {} a -> s {acceptDate = a} :: TransferData) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TransferData where
  parseJSON =
    Data.withObject
      "TransferData"
      ( \x ->
          TransferData'
            Prelude.<$> (x Data..:? "transferDate")
            Prelude.<*> (x Data..:? "rejectReason")
            Prelude.<*> (x Data..:? "transferMessage")
            Prelude.<*> (x Data..:? "rejectDate")
            Prelude.<*> (x Data..:? "acceptDate")
      )

instance Prelude.Hashable TransferData where
  hashWithSalt _salt TransferData' {..} =
    _salt `Prelude.hashWithSalt` transferDate
      `Prelude.hashWithSalt` rejectReason
      `Prelude.hashWithSalt` transferMessage
      `Prelude.hashWithSalt` rejectDate
      `Prelude.hashWithSalt` acceptDate

instance Prelude.NFData TransferData where
  rnf TransferData' {..} =
    Prelude.rnf transferDate
      `Prelude.seq` Prelude.rnf rejectReason
      `Prelude.seq` Prelude.rnf transferMessage
      `Prelude.seq` Prelude.rnf rejectDate
      `Prelude.seq` Prelude.rnf acceptDate
