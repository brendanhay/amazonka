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
-- Module      : Network.AWS.DevOpsGuru.Types.ServiceInsightHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DevOpsGuru.Types.ServiceInsightHealth where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the number of open proactive and reactive insights in an
-- analyzed AWS service.
--
-- /See:/ 'newServiceInsightHealth' smart constructor.
data ServiceInsightHealth = ServiceInsightHealth'
  { -- | The number of open reactive insights in the AWS service
    openReactiveInsights :: Prelude.Maybe Prelude.Int,
    -- | The number of open proactive insights in the AWS service
    openProactiveInsights :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceInsightHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openReactiveInsights', 'serviceInsightHealth_openReactiveInsights' - The number of open reactive insights in the AWS service
--
-- 'openProactiveInsights', 'serviceInsightHealth_openProactiveInsights' - The number of open proactive insights in the AWS service
newServiceInsightHealth ::
  ServiceInsightHealth
newServiceInsightHealth =
  ServiceInsightHealth'
    { openReactiveInsights =
        Prelude.Nothing,
      openProactiveInsights = Prelude.Nothing
    }

-- | The number of open reactive insights in the AWS service
serviceInsightHealth_openReactiveInsights :: Lens.Lens' ServiceInsightHealth (Prelude.Maybe Prelude.Int)
serviceInsightHealth_openReactiveInsights = Lens.lens (\ServiceInsightHealth' {openReactiveInsights} -> openReactiveInsights) (\s@ServiceInsightHealth' {} a -> s {openReactiveInsights = a} :: ServiceInsightHealth)

-- | The number of open proactive insights in the AWS service
serviceInsightHealth_openProactiveInsights :: Lens.Lens' ServiceInsightHealth (Prelude.Maybe Prelude.Int)
serviceInsightHealth_openProactiveInsights = Lens.lens (\ServiceInsightHealth' {openProactiveInsights} -> openProactiveInsights) (\s@ServiceInsightHealth' {} a -> s {openProactiveInsights = a} :: ServiceInsightHealth)

instance Core.FromJSON ServiceInsightHealth where
  parseJSON =
    Core.withObject
      "ServiceInsightHealth"
      ( \x ->
          ServiceInsightHealth'
            Prelude.<$> (x Core..:? "OpenReactiveInsights")
            Prelude.<*> (x Core..:? "OpenProactiveInsights")
      )

instance Prelude.Hashable ServiceInsightHealth

instance Prelude.NFData ServiceInsightHealth
