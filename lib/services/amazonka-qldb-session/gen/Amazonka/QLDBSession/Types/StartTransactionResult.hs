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
-- Module      : Amazonka.QLDBSession.Types.StartTransactionResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDBSession.Types.StartTransactionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDBSession.Types.TimingInformation

-- | Contains the details of the started transaction.
--
-- /See:/ 'newStartTransactionResult' smart constructor.
data StartTransactionResult = StartTransactionResult'
  { -- | Contains server-side performance information for the command.
    timingInformation :: Prelude.Maybe TimingInformation,
    -- | The transaction ID of the started transaction.
    transactionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartTransactionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timingInformation', 'startTransactionResult_timingInformation' - Contains server-side performance information for the command.
--
-- 'transactionId', 'startTransactionResult_transactionId' - The transaction ID of the started transaction.
newStartTransactionResult ::
  StartTransactionResult
newStartTransactionResult =
  StartTransactionResult'
    { timingInformation =
        Prelude.Nothing,
      transactionId = Prelude.Nothing
    }

-- | Contains server-side performance information for the command.
startTransactionResult_timingInformation :: Lens.Lens' StartTransactionResult (Prelude.Maybe TimingInformation)
startTransactionResult_timingInformation = Lens.lens (\StartTransactionResult' {timingInformation} -> timingInformation) (\s@StartTransactionResult' {} a -> s {timingInformation = a} :: StartTransactionResult)

-- | The transaction ID of the started transaction.
startTransactionResult_transactionId :: Lens.Lens' StartTransactionResult (Prelude.Maybe Prelude.Text)
startTransactionResult_transactionId = Lens.lens (\StartTransactionResult' {transactionId} -> transactionId) (\s@StartTransactionResult' {} a -> s {transactionId = a} :: StartTransactionResult)

instance Data.FromJSON StartTransactionResult where
  parseJSON =
    Data.withObject
      "StartTransactionResult"
      ( \x ->
          StartTransactionResult'
            Prelude.<$> (x Data..:? "TimingInformation")
            Prelude.<*> (x Data..:? "TransactionId")
      )

instance Prelude.Hashable StartTransactionResult where
  hashWithSalt _salt StartTransactionResult' {..} =
    _salt
      `Prelude.hashWithSalt` timingInformation
      `Prelude.hashWithSalt` transactionId

instance Prelude.NFData StartTransactionResult where
  rnf StartTransactionResult' {..} =
    Prelude.rnf timingInformation
      `Prelude.seq` Prelude.rnf transactionId
