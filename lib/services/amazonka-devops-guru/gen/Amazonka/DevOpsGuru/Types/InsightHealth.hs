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
-- Module      : Amazonka.DevOpsGuru.Types.InsightHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.InsightHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of open reactive and proactive insights
-- that can be used to gauge the health of your system.
--
-- /See:/ 'newInsightHealth' smart constructor.
data InsightHealth = InsightHealth'
  { -- | The Meant Time to Recover (MTTR) for the insight.
    meanTimeToRecoverInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of open proactive insights.
    openProactiveInsights :: Prelude.Maybe Prelude.Int,
    -- | The number of open reactive insights.
    openReactiveInsights :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meanTimeToRecoverInMilliseconds', 'insightHealth_meanTimeToRecoverInMilliseconds' - The Meant Time to Recover (MTTR) for the insight.
--
-- 'openProactiveInsights', 'insightHealth_openProactiveInsights' - The number of open proactive insights.
--
-- 'openReactiveInsights', 'insightHealth_openReactiveInsights' - The number of open reactive insights.
newInsightHealth ::
  InsightHealth
newInsightHealth =
  InsightHealth'
    { meanTimeToRecoverInMilliseconds =
        Prelude.Nothing,
      openProactiveInsights = Prelude.Nothing,
      openReactiveInsights = Prelude.Nothing
    }

-- | The Meant Time to Recover (MTTR) for the insight.
insightHealth_meanTimeToRecoverInMilliseconds :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Integer)
insightHealth_meanTimeToRecoverInMilliseconds = Lens.lens (\InsightHealth' {meanTimeToRecoverInMilliseconds} -> meanTimeToRecoverInMilliseconds) (\s@InsightHealth' {} a -> s {meanTimeToRecoverInMilliseconds = a} :: InsightHealth)

-- | The number of open proactive insights.
insightHealth_openProactiveInsights :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Int)
insightHealth_openProactiveInsights = Lens.lens (\InsightHealth' {openProactiveInsights} -> openProactiveInsights) (\s@InsightHealth' {} a -> s {openProactiveInsights = a} :: InsightHealth)

-- | The number of open reactive insights.
insightHealth_openReactiveInsights :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Int)
insightHealth_openReactiveInsights = Lens.lens (\InsightHealth' {openReactiveInsights} -> openReactiveInsights) (\s@InsightHealth' {} a -> s {openReactiveInsights = a} :: InsightHealth)

instance Data.FromJSON InsightHealth where
  parseJSON =
    Data.withObject
      "InsightHealth"
      ( \x ->
          InsightHealth'
            Prelude.<$> (x Data..:? "MeanTimeToRecoverInMilliseconds")
            Prelude.<*> (x Data..:? "OpenProactiveInsights")
            Prelude.<*> (x Data..:? "OpenReactiveInsights")
      )

instance Prelude.Hashable InsightHealth where
  hashWithSalt _salt InsightHealth' {..} =
    _salt
      `Prelude.hashWithSalt` meanTimeToRecoverInMilliseconds
      `Prelude.hashWithSalt` openProactiveInsights
      `Prelude.hashWithSalt` openReactiveInsights

instance Prelude.NFData InsightHealth where
  rnf InsightHealth' {..} =
    Prelude.rnf meanTimeToRecoverInMilliseconds
      `Prelude.seq` Prelude.rnf openProactiveInsights
      `Prelude.seq` Prelude.rnf openReactiveInsights
