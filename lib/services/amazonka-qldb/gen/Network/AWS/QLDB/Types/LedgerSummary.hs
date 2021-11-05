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
-- Module      : Amazonka.QLDB.Types.LedgerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QLDB.Types.LedgerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QLDB.Types.LedgerState

-- | Information about a ledger, including its name, state, and when it was
-- created.
--
-- /See:/ 'newLedgerSummary' smart constructor.
data LedgerSummary = LedgerSummary'
  { -- | The current status of the ledger.
    state :: Prelude.Maybe LedgerState,
    -- | The name of the ledger.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in epoch time format, when the ledger was created.
    -- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
    -- January 1, 1970 UTC.)
    creationDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LedgerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'ledgerSummary_state' - The current status of the ledger.
--
-- 'name', 'ledgerSummary_name' - The name of the ledger.
--
-- 'creationDateTime', 'ledgerSummary_creationDateTime' - The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
newLedgerSummary ::
  LedgerSummary
newLedgerSummary =
  LedgerSummary'
    { state = Prelude.Nothing,
      name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing
    }

-- | The current status of the ledger.
ledgerSummary_state :: Lens.Lens' LedgerSummary (Prelude.Maybe LedgerState)
ledgerSummary_state = Lens.lens (\LedgerSummary' {state} -> state) (\s@LedgerSummary' {} a -> s {state = a} :: LedgerSummary)

-- | The name of the ledger.
ledgerSummary_name :: Lens.Lens' LedgerSummary (Prelude.Maybe Prelude.Text)
ledgerSummary_name = Lens.lens (\LedgerSummary' {name} -> name) (\s@LedgerSummary' {} a -> s {name = a} :: LedgerSummary)

-- | The date and time, in epoch time format, when the ledger was created.
-- (Epoch time format is the number of seconds elapsed since 12:00:00 AM
-- January 1, 1970 UTC.)
ledgerSummary_creationDateTime :: Lens.Lens' LedgerSummary (Prelude.Maybe Prelude.UTCTime)
ledgerSummary_creationDateTime = Lens.lens (\LedgerSummary' {creationDateTime} -> creationDateTime) (\s@LedgerSummary' {} a -> s {creationDateTime = a} :: LedgerSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON LedgerSummary where
  parseJSON =
    Core.withObject
      "LedgerSummary"
      ( \x ->
          LedgerSummary'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "CreationDateTime")
      )

instance Prelude.Hashable LedgerSummary

instance Prelude.NFData LedgerSummary
