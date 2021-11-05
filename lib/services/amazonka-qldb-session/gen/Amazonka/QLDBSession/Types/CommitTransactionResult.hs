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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.CommitTransactionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.IOUsage
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the committed transaction.
--
-- /See:/ 'newCommitTransactionResult' smart constructor.
data CommitTransactionResult = CommitTransactionResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | Contains metrics about the number of I\/O requests that were consumed.
    consumedIOs :: Prelude.Maybe IOUsage,
    -- | The commit digest of the committed transaction.
    commitDigest :: Prelude.Maybe Core.Base64,
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
-- 'timingInformation', 'commitTransactionResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'consumedIOs', 'commitTransactionResult_consumedIOs' - Contains metrics about the number of I\/O requests that were consumed.
--
-- 'commitDigest', 'commitTransactionResult_commitDigest' - The commit digest of the committed transaction.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'transactionId', 'commitTransactionResult_transactionId' - The transaction ID of the committed transaction.
newCommitTransactionResult ::
  CommitTransactionResult
newCommitTransactionResult =
  CommitTransactionResult'
    { timingInformation =
        Prelude.Nothing,
      consumedIOs = Prelude.Nothing,
      commitDigest = Prelude.Nothing,
      transactionId = Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
commitTransactionResult_timingInformation :: Lens.Lens' CommitTransactionResult (Prelude.Maybe TimingInformation)
commitTransactionResult_timingInformation = Lens.lens (\CommitTransactionResult' {timingInformation} -> timingInformation) (\s@CommitTransactionResult' {} a -> s {timingInformation = a} :: CommitTransactionResult)

-- | Contains metrics about the number of I\/O requests that were consumed.
commitTransactionResult_consumedIOs :: Lens.Lens' CommitTransactionResult (Prelude.Maybe IOUsage)
commitTransactionResult_consumedIOs = Lens.lens (\CommitTransactionResult' {consumedIOs} -> consumedIOs) (\s@CommitTransactionResult' {} a -> s {consumedIOs = a} :: CommitTransactionResult)

-- | The commit digest of the committed transaction.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
commitTransactionResult_commitDigest :: Lens.Lens' CommitTransactionResult (Prelude.Maybe Prelude.ByteString)
commitTransactionResult_commitDigest = Lens.lens (\CommitTransactionResult' {commitDigest} -> commitDigest) (\s@CommitTransactionResult' {} a -> s {commitDigest = a} :: CommitTransactionResult) Prelude.. Lens.mapping Core._Base64

-- | The transaction ID of the committed transaction.
commitTransactionResult_transactionId :: Lens.Lens' CommitTransactionResult (Prelude.Maybe Prelude.Text)
commitTransactionResult_transactionId = Lens.lens (\CommitTransactionResult' {transactionId} -> transactionId) (\s@CommitTransactionResult' {} a -> s {transactionId = a} :: CommitTransactionResult)

instance Core.FromJSON CommitTransactionResult where
  parseJSON =
    Core.withObject
      "CommitTransactionResult"
      ( \x ->
          CommitTransactionResult'
            Prelude.<$> (x Core..:? "TimingInformation")
            Prelude.<*> (x Core..:? "ConsumedIOs")
            Prelude.<*> (x Core..:? "CommitDigest")
            Prelude.<*> (x Core..:? "TransactionId")
      )

instance Prelude.Hashable CommitTransactionResult

instance Prelude.NFData CommitTransactionResult
