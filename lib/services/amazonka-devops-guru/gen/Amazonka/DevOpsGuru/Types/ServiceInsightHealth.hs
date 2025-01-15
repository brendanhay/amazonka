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
-- Module      : Amazonka.DevOpsGuru.Types.ServiceInsightHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ServiceInsightHealth where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the number of open proactive and reactive insights in an
-- analyzed Amazon Web Services service.
--
-- /See:/ 'newServiceInsightHealth' smart constructor.
data ServiceInsightHealth = ServiceInsightHealth'
  { -- | The number of open proactive insights in the Amazon Web Services service
    openProactiveInsights :: Prelude.Maybe Prelude.Int,
    -- | The number of open reactive insights in the Amazon Web Services service
    openReactiveInsights :: Prelude.Maybe Prelude.Int
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
-- 'openProactiveInsights', 'serviceInsightHealth_openProactiveInsights' - The number of open proactive insights in the Amazon Web Services service
--
-- 'openReactiveInsights', 'serviceInsightHealth_openReactiveInsights' - The number of open reactive insights in the Amazon Web Services service
newServiceInsightHealth ::
  ServiceInsightHealth
newServiceInsightHealth =
  ServiceInsightHealth'
    { openProactiveInsights =
        Prelude.Nothing,
      openReactiveInsights = Prelude.Nothing
    }

-- | The number of open proactive insights in the Amazon Web Services service
serviceInsightHealth_openProactiveInsights :: Lens.Lens' ServiceInsightHealth (Prelude.Maybe Prelude.Int)
serviceInsightHealth_openProactiveInsights = Lens.lens (\ServiceInsightHealth' {openProactiveInsights} -> openProactiveInsights) (\s@ServiceInsightHealth' {} a -> s {openProactiveInsights = a} :: ServiceInsightHealth)

-- | The number of open reactive insights in the Amazon Web Services service
serviceInsightHealth_openReactiveInsights :: Lens.Lens' ServiceInsightHealth (Prelude.Maybe Prelude.Int)
serviceInsightHealth_openReactiveInsights = Lens.lens (\ServiceInsightHealth' {openReactiveInsights} -> openReactiveInsights) (\s@ServiceInsightHealth' {} a -> s {openReactiveInsights = a} :: ServiceInsightHealth)

instance Data.FromJSON ServiceInsightHealth where
  parseJSON =
    Data.withObject
      "ServiceInsightHealth"
      ( \x ->
          ServiceInsightHealth'
            Prelude.<$> (x Data..:? "OpenProactiveInsights")
            Prelude.<*> (x Data..:? "OpenReactiveInsights")
      )

instance Prelude.Hashable ServiceInsightHealth where
  hashWithSalt _salt ServiceInsightHealth' {..} =
    _salt
      `Prelude.hashWithSalt` openProactiveInsights
      `Prelude.hashWithSalt` openReactiveInsights

instance Prelude.NFData ServiceInsightHealth where
  rnf ServiceInsightHealth' {..} =
    Prelude.rnf openProactiveInsights `Prelude.seq`
      Prelude.rnf openReactiveInsights
