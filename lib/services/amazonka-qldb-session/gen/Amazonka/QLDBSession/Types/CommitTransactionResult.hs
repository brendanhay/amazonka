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
-- Module      : Amazonka.QLDBSession.Types.CommitTransactionResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.CommitTransactionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the committed transaction.
--
-- /See:/ 'newCommitTransactionResult' smart constructor.
data CommitTransactionResult = CommitTransactionResult'
  { -- | The commit digest of the committed transaction.
    commitDigest :: Prelude.Maybe Data.Base64,
    -- | Contains metrics about the number of I\/O requests that were consumed.
    consumedIOs :: Prelude.Maybe IOUsage,
    -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | The transaction ID of the committed transaction.
    transactionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommitTransactionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitDigest', 'commitTransactionResult_commitDigest' - The commit digest of the committed transaction.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'consumedIOs', 'commitTransactionResult_consumedIOs' - Contains metrics about the number of I\/O requests that were consumed.
--
-- 'timingInformation', 'commitTransactionResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'transactionId', 'commitTransactionResult_transactionId' - The transaction ID of the committed transaction.
newCommitTransactionResult ::
  CommitTransactionResult
newCommitTransactionResult =
  CommitTransactionResult'
    { commitDigest =
        Prelude.Nothing,
      consumedIOs = Prelude.Nothing,
      timingInformation = Prelude.Nothing,
      transactionId = Prelude.Nothing
    }

-- | The commit digest of the committed transaction.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
commitTransactionResult_commitDigest :: Lens.Lens' CommitTransactionResult (Prelude.Maybe Prelude.ByteString)
commitTransactionResult_commitDigest = Lens.lens (\CommitTransactionResult' {commitDigest} -> commitDigest) (\s@CommitTransactionResult' {} a -> s {commitDigest = a} :: CommitTransactionResult) Prelude.. Lens.mapping Data._Base64

-- | Contains metrics about the number of I\/O requests that were consumed.
commitTransactionResult_consumedIOs :: Lens.Lens' CommitTransactionResult (Prelude.Maybe IOUsage)
commitTransactionResult_consumedIOs = Lens.lens (\CommitTransactionResult' {consumedIOs} -> consumedIOs) (\s@CommitTransactionResult' {} a -> s {consumedIOs = a} :: CommitTransactionResult)

-- | Contains server-side performance information for the command.
commitTransactionResult_timingInformation :: Lens.Lens' CommitTransactionResult (Prelude.Maybe TimingInformation)
commitTransactionResult_timingInformation = Lens.lens (\CommitTransactionResult' {timingInformation} -> timingInformation) (\s@CommitTransactionResult' {} a -> s {timingInformation = a} :: CommitTransactionResult)

-- | The transaction ID of the committed transaction.
commitTransactionResult_transactionId :: Lens.Lens' CommitTransactionResult (Prelude.Maybe Prelude.Text)
commitTransactionResult_transactionId = Lens.lens (\CommitTransactionResult' {transactionId} -> transactionId) (\s@CommitTransactionResult' {} a -> s {transactionId = a} :: CommitTransactionResult)

instance Data.FromJSON CommitTransactionResult where
  parseJSON =
    Data.withObject
      "CommitTransactionResult"
      ( \x ->
          CommitTransactionResult'
            Prelude.<$> (x Data..:? "CommitDigest")
            Prelude.<*> (x Data..:? "ConsumedIOs")
            Prelude.<*> (x Data..:? "TimingInformation")
            Prelude.<*> (x Data..:? "TransactionId")
      )

instance Prelude.Hashable CommitTransactionResult where
  hashWithSalt _salt CommitTransactionResult' {..} =
    _salt
      `Prelude.hashWithSalt` commitDigest
      `Prelude.hashWithSalt` consumedIOs
      `Prelude.hashWithSalt` timingInformation
      `Prelude.hashWithSalt` transactionId

instance Prelude.NFData CommitTransactionResult where
  rnf CommitTransactionResult' {..} =
    Prelude.rnf commitDigest
      `Prelude.seq` Prelude.rnf consumedIOs
      `Prelude.seq` Prelude.rnf timingInformation
      `Prelude.seq` Prelude.rnf transactionId
