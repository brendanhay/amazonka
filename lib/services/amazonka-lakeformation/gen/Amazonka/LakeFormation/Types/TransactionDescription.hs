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
-- Module      : Amazonka.LakeFormation.Types.TransactionDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.TransactionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types.TransactionStatus
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains information about a transaction.
--
-- /See:/ 'newTransactionDescription' smart constructor.
data TransactionDescription = TransactionDescription'
  { -- | The time when the transaction started.
    transactionStartTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the transaction.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | A status of ACTIVE, COMMITTED, or ABORTED.
    transactionStatus :: Prelude.Maybe TransactionStatus,
    -- | The time when the transaction committed or aborted, if it is not
    -- currently active.
    transactionEndTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransactionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transactionStartTime', 'transactionDescription_transactionStartTime' - The time when the transaction started.
--
-- 'transactionId', 'transactionDescription_transactionId' - The ID of the transaction.
--
-- 'transactionStatus', 'transactionDescription_transactionStatus' - A status of ACTIVE, COMMITTED, or ABORTED.
--
-- 'transactionEndTime', 'transactionDescription_transactionEndTime' - The time when the transaction committed or aborted, if it is not
-- currently active.
newTransactionDescription ::
  TransactionDescription
newTransactionDescription =
  TransactionDescription'
    { transactionStartTime =
        Prelude.Nothing,
      transactionId = Prelude.Nothing,
      transactionStatus = Prelude.Nothing,
      transactionEndTime = Prelude.Nothing
    }

-- | The time when the transaction started.
transactionDescription_transactionStartTime :: Lens.Lens' TransactionDescription (Prelude.Maybe Prelude.UTCTime)
transactionDescription_transactionStartTime = Lens.lens (\TransactionDescription' {transactionStartTime} -> transactionStartTime) (\s@TransactionDescription' {} a -> s {transactionStartTime = a} :: TransactionDescription) Prelude.. Lens.mapping Data._Time

-- | The ID of the transaction.
transactionDescription_transactionId :: Lens.Lens' TransactionDescription (Prelude.Maybe Prelude.Text)
transactionDescription_transactionId = Lens.lens (\TransactionDescription' {transactionId} -> transactionId) (\s@TransactionDescription' {} a -> s {transactionId = a} :: TransactionDescription)

-- | A status of ACTIVE, COMMITTED, or ABORTED.
transactionDescription_transactionStatus :: Lens.Lens' TransactionDescription (Prelude.Maybe TransactionStatus)
transactionDescription_transactionStatus = Lens.lens (\TransactionDescription' {transactionStatus} -> transactionStatus) (\s@TransactionDescription' {} a -> s {transactionStatus = a} :: TransactionDescription)

-- | The time when the transaction committed or aborted, if it is not
-- currently active.
transactionDescription_transactionEndTime :: Lens.Lens' TransactionDescription (Prelude.Maybe Prelude.UTCTime)
transactionDescription_transactionEndTime = Lens.lens (\TransactionDescription' {transactionEndTime} -> transactionEndTime) (\s@TransactionDescription' {} a -> s {transactionEndTime = a} :: TransactionDescription) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON TransactionDescription where
  parseJSON =
    Data.withObject
      "TransactionDescription"
      ( \x ->
          TransactionDescription'
            Prelude.<$> (x Data..:? "TransactionStartTime")
            Prelude.<*> (x Data..:? "TransactionId")
            Prelude.<*> (x Data..:? "TransactionStatus")
            Prelude.<*> (x Data..:? "TransactionEndTime")
      )

instance Prelude.Hashable TransactionDescription where
  hashWithSalt _salt TransactionDescription' {..} =
    _salt `Prelude.hashWithSalt` transactionStartTime
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` transactionStatus
      `Prelude.hashWithSalt` transactionEndTime

instance Prelude.NFData TransactionDescription where
  rnf TransactionDescription' {..} =
    Prelude.rnf transactionStartTime
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf transactionStatus
      `Prelude.seq` Prelude.rnf transactionEndTime
