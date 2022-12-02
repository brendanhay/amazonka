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
-- Module      : Amazonka.DevOpsGuru.Types.AccountInsightHealth
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.AccountInsightHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of open reactive and proactive insights
-- that can be used to gauge the health of your system.
--
-- /See:/ 'newAccountInsightHealth' smart constructor.
data AccountInsightHealth = AccountInsightHealth'
  { -- | An integer that specifies the number of open reactive insights in your
    -- Amazon Web Services account.
    openReactiveInsights :: Prelude.Maybe Prelude.Int,
    -- | An integer that specifies the number of open proactive insights in your
    -- Amazon Web Services account.
    openProactiveInsights :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountInsightHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openReactiveInsights', 'accountInsightHealth_openReactiveInsights' - An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
--
-- 'openProactiveInsights', 'accountInsightHealth_openProactiveInsights' - An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
newAccountInsightHealth ::
  AccountInsightHealth
newAccountInsightHealth =
  AccountInsightHealth'
    { openReactiveInsights =
        Prelude.Nothing,
      openProactiveInsights = Prelude.Nothing
    }

-- | An integer that specifies the number of open reactive insights in your
-- Amazon Web Services account.
accountInsightHealth_openReactiveInsights :: Lens.Lens' AccountInsightHealth (Prelude.Maybe Prelude.Int)
accountInsightHealth_openReactiveInsights = Lens.lens (\AccountInsightHealth' {openReactiveInsights} -> openReactiveInsights) (\s@AccountInsightHealth' {} a -> s {openReactiveInsights = a} :: AccountInsightHealth)

-- | An integer that specifies the number of open proactive insights in your
-- Amazon Web Services account.
accountInsightHealth_openProactiveInsights :: Lens.Lens' AccountInsightHealth (Prelude.Maybe Prelude.Int)
accountInsightHealth_openProactiveInsights = Lens.lens (\AccountInsightHealth' {openProactiveInsights} -> openProactiveInsights) (\s@AccountInsightHealth' {} a -> s {openProactiveInsights = a} :: AccountInsightHealth)

instance Data.FromJSON AccountInsightHealth where
  parseJSON =
    Data.withObject
      "AccountInsightHealth"
      ( \x ->
          AccountInsightHealth'
            Prelude.<$> (x Data..:? "OpenReactiveInsights")
            Prelude.<*> (x Data..:? "OpenProactiveInsights")
      )

instance Prelude.Hashable AccountInsightHealth where
  hashWithSalt _salt AccountInsightHealth' {..} =
    _salt `Prelude.hashWithSalt` openReactiveInsights
      `Prelude.hashWithSalt` openProactiveInsights

instance Prelude.NFData AccountInsightHealth where
  rnf AccountInsightHealth' {..} =
    Prelude.rnf openReactiveInsights
      `Prelude.seq` Prelude.rnf openProactiveInsights
