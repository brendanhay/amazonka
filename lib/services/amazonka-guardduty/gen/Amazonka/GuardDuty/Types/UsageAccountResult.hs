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
-- Module      : Amazonka.GuardDuty.Types.UsageAccountResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.UsageAccountResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GuardDuty.Types.Total
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the total of usage based on account IDs.
--
-- /See:/ 'newUsageAccountResult' smart constructor.
data UsageAccountResult = UsageAccountResult'
  { -- | Represents the total of usage for the Account ID.
    total :: Prelude.Maybe Total,
    -- | The Account ID that generated usage.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageAccountResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'usageAccountResult_total' - Represents the total of usage for the Account ID.
--
-- 'accountId', 'usageAccountResult_accountId' - The Account ID that generated usage.
newUsageAccountResult ::
  UsageAccountResult
newUsageAccountResult =
  UsageAccountResult'
    { total = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | Represents the total of usage for the Account ID.
usageAccountResult_total :: Lens.Lens' UsageAccountResult (Prelude.Maybe Total)
usageAccountResult_total = Lens.lens (\UsageAccountResult' {total} -> total) (\s@UsageAccountResult' {} a -> s {total = a} :: UsageAccountResult)

-- | The Account ID that generated usage.
usageAccountResult_accountId :: Lens.Lens' UsageAccountResult (Prelude.Maybe Prelude.Text)
usageAccountResult_accountId = Lens.lens (\UsageAccountResult' {accountId} -> accountId) (\s@UsageAccountResult' {} a -> s {accountId = a} :: UsageAccountResult)

instance Core.FromJSON UsageAccountResult where
  parseJSON =
    Core.withObject
      "UsageAccountResult"
      ( \x ->
          UsageAccountResult'
            Prelude.<$> (x Core..:? "total")
            Prelude.<*> (x Core..:? "accountId")
      )

instance Prelude.Hashable UsageAccountResult where
  hashWithSalt _salt UsageAccountResult' {..} =
    _salt `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData UsageAccountResult where
  rnf UsageAccountResult' {..} =
    Prelude.rnf total
      `Prelude.seq` Prelude.rnf accountId
