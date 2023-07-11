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
-- Module      : Amazonka.Inspector2.Types.UsageTotal
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.UsageTotal where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.Usage
import qualified Amazonka.Prelude as Prelude

-- | The total of usage for an account ID.
--
-- /See:/ 'newUsageTotal' smart constructor.
data UsageTotal = UsageTotal'
  { -- | The account ID of the account that usage data was retrieved for.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An object representing the total usage for an account.
    usage :: Prelude.Maybe [Usage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageTotal' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'usageTotal_accountId' - The account ID of the account that usage data was retrieved for.
--
-- 'usage', 'usageTotal_usage' - An object representing the total usage for an account.
newUsageTotal ::
  UsageTotal
newUsageTotal =
  UsageTotal'
    { accountId = Prelude.Nothing,
      usage = Prelude.Nothing
    }

-- | The account ID of the account that usage data was retrieved for.
usageTotal_accountId :: Lens.Lens' UsageTotal (Prelude.Maybe Prelude.Text)
usageTotal_accountId = Lens.lens (\UsageTotal' {accountId} -> accountId) (\s@UsageTotal' {} a -> s {accountId = a} :: UsageTotal)

-- | An object representing the total usage for an account.
usageTotal_usage :: Lens.Lens' UsageTotal (Prelude.Maybe [Usage])
usageTotal_usage = Lens.lens (\UsageTotal' {usage} -> usage) (\s@UsageTotal' {} a -> s {usage = a} :: UsageTotal) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON UsageTotal where
  parseJSON =
    Data.withObject
      "UsageTotal"
      ( \x ->
          UsageTotal'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "usage" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable UsageTotal where
  hashWithSalt _salt UsageTotal' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` usage

instance Prelude.NFData UsageTotal where
  rnf UsageTotal' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf usage
