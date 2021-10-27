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
-- Module      : Network.AWS.DevOpsGuru.Types.InsightHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.InsightHealth where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the number of open reactive and proactive insights
-- that can be used to gauge the health of your system.
--
-- /See:/ 'newInsightHealth' smart constructor.
data InsightHealth = InsightHealth'
  { -- | The Meant Time to Recover (MTTR) for the insight.
    meanTimeToRecoverInMilliseconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of open reactive insights.
    openReactiveInsights :: Prelude.Maybe Prelude.Int,
    -- | The number of open proactive insights.
    openProactiveInsights :: Prelude.Maybe Prelude.Int
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
-- 'openReactiveInsights', 'insightHealth_openReactiveInsights' - The number of open reactive insights.
--
-- 'openProactiveInsights', 'insightHealth_openProactiveInsights' - The number of open proactive insights.
newInsightHealth ::
  InsightHealth
newInsightHealth =
  InsightHealth'
    { meanTimeToRecoverInMilliseconds =
        Prelude.Nothing,
      openReactiveInsights = Prelude.Nothing,
      openProactiveInsights = Prelude.Nothing
    }

-- | The Meant Time to Recover (MTTR) for the insight.
insightHealth_meanTimeToRecoverInMilliseconds :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Integer)
insightHealth_meanTimeToRecoverInMilliseconds = Lens.lens (\InsightHealth' {meanTimeToRecoverInMilliseconds} -> meanTimeToRecoverInMilliseconds) (\s@InsightHealth' {} a -> s {meanTimeToRecoverInMilliseconds = a} :: InsightHealth)

-- | The number of open reactive insights.
insightHealth_openReactiveInsights :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Int)
insightHealth_openReactiveInsights = Lens.lens (\InsightHealth' {openReactiveInsights} -> openReactiveInsights) (\s@InsightHealth' {} a -> s {openReactiveInsights = a} :: InsightHealth)

-- | The number of open proactive insights.
insightHealth_openProactiveInsights :: Lens.Lens' InsightHealth (Prelude.Maybe Prelude.Int)
insightHealth_openProactiveInsights = Lens.lens (\InsightHealth' {openProactiveInsights} -> openProactiveInsights) (\s@InsightHealth' {} a -> s {openProactiveInsights = a} :: InsightHealth)

instance Core.FromJSON InsightHealth where
  parseJSON =
    Core.withObject
      "InsightHealth"
      ( \x ->
          InsightHealth'
            Prelude.<$> (x Core..:? "MeanTimeToRecoverInMilliseconds")
            Prelude.<*> (x Core..:? "OpenReactiveInsights")
            Prelude.<*> (x Core..:? "OpenProactiveInsights")
      )

instance Prelude.Hashable InsightHealth

instance Prelude.NFData InsightHealth
