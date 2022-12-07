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
-- Module      : Amazonka.DevOpsGuru.Types.AccountHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AccountHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.AccountInsightHealth
import qualified Amazonka.Prelude as Prelude

-- | Returns the number of open reactive insights, the number of open
-- proactive insights, and the number of metrics analyzed in your Amazon
-- Web Services account. Use these numbers to gauge the health of
-- operations in your Amazon Web Services account.
--
-- /See:/ 'newAccountHealth' smart constructor.
data AccountHealth = AccountHealth'
  { -- | Information about the health of the Amazon Web Services resources in
    -- your account, including the number of open proactive, open reactive
    -- insights, and the Mean Time to Recover (MTTR) of closed insights.
    insight :: Prelude.Maybe AccountInsightHealth,
    -- | The ID of the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insight', 'accountHealth_insight' - Information about the health of the Amazon Web Services resources in
-- your account, including the number of open proactive, open reactive
-- insights, and the Mean Time to Recover (MTTR) of closed insights.
--
-- 'accountId', 'accountHealth_accountId' - The ID of the Amazon Web Services account.
newAccountHealth ::
  AccountHealth
newAccountHealth =
  AccountHealth'
    { insight = Prelude.Nothing,
      accountId = Prelude.Nothing
    }

-- | Information about the health of the Amazon Web Services resources in
-- your account, including the number of open proactive, open reactive
-- insights, and the Mean Time to Recover (MTTR) of closed insights.
accountHealth_insight :: Lens.Lens' AccountHealth (Prelude.Maybe AccountInsightHealth)
accountHealth_insight = Lens.lens (\AccountHealth' {insight} -> insight) (\s@AccountHealth' {} a -> s {insight = a} :: AccountHealth)

-- | The ID of the Amazon Web Services account.
accountHealth_accountId :: Lens.Lens' AccountHealth (Prelude.Maybe Prelude.Text)
accountHealth_accountId = Lens.lens (\AccountHealth' {accountId} -> accountId) (\s@AccountHealth' {} a -> s {accountId = a} :: AccountHealth)

instance Data.FromJSON AccountHealth where
  parseJSON =
    Data.withObject
      "AccountHealth"
      ( \x ->
          AccountHealth'
            Prelude.<$> (x Data..:? "Insight")
            Prelude.<*> (x Data..:? "AccountId")
      )

instance Prelude.Hashable AccountHealth where
  hashWithSalt _salt AccountHealth' {..} =
    _salt `Prelude.hashWithSalt` insight
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData AccountHealth where
  rnf AccountHealth' {..} =
    Prelude.rnf insight
      `Prelude.seq` Prelude.rnf accountId
