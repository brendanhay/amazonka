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
-- Module      : Amazonka.Discovery.Types.UsageMetricBasis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Discovery.Types.UsageMetricBasis where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the performance metrics to use for the server that is used for
-- recommendations.
--
-- /See:/ 'newUsageMetricBasis' smart constructor.
data UsageMetricBasis = UsageMetricBasis'
  { -- | A utilization metric that is used by the recommendations.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the percentage of the specified utilization metric that is
    -- used by the recommendations.
    percentageAdjust :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageMetricBasis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'usageMetricBasis_name' - A utilization metric that is used by the recommendations.
--
-- 'percentageAdjust', 'usageMetricBasis_percentageAdjust' - Specifies the percentage of the specified utilization metric that is
-- used by the recommendations.
newUsageMetricBasis ::
  UsageMetricBasis
newUsageMetricBasis =
  UsageMetricBasis'
    { name = Prelude.Nothing,
      percentageAdjust = Prelude.Nothing
    }

-- | A utilization metric that is used by the recommendations.
usageMetricBasis_name :: Lens.Lens' UsageMetricBasis (Prelude.Maybe Prelude.Text)
usageMetricBasis_name = Lens.lens (\UsageMetricBasis' {name} -> name) (\s@UsageMetricBasis' {} a -> s {name = a} :: UsageMetricBasis)

-- | Specifies the percentage of the specified utilization metric that is
-- used by the recommendations.
usageMetricBasis_percentageAdjust :: Lens.Lens' UsageMetricBasis (Prelude.Maybe Prelude.Double)
usageMetricBasis_percentageAdjust = Lens.lens (\UsageMetricBasis' {percentageAdjust} -> percentageAdjust) (\s@UsageMetricBasis' {} a -> s {percentageAdjust = a} :: UsageMetricBasis)

instance Prelude.Hashable UsageMetricBasis where
  hashWithSalt _salt UsageMetricBasis' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` percentageAdjust

instance Prelude.NFData UsageMetricBasis where
  rnf UsageMetricBasis' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf percentageAdjust

instance Data.ToJSON UsageMetricBasis where
  toJSON UsageMetricBasis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("percentageAdjust" Data..=)
              Prelude.<$> percentageAdjust
          ]
      )
