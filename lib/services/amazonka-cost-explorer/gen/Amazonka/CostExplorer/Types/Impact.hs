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
-- Module      : Amazonka.CostExplorer.Types.Impact
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Impact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The dollar value of the anomaly.
--
-- /See:/ 'newImpact' smart constructor.
data Impact = Impact'
  { -- | The cumulative dollar value that\'s observed for an anomaly.
    totalImpact :: Prelude.Maybe Prelude.Double,
    -- | The maximum dollar value that\'s observed for an anomaly.
    maxImpact :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Impact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalImpact', 'impact_totalImpact' - The cumulative dollar value that\'s observed for an anomaly.
--
-- 'maxImpact', 'impact_maxImpact' - The maximum dollar value that\'s observed for an anomaly.
newImpact ::
  -- | 'maxImpact'
  Prelude.Double ->
  Impact
newImpact pMaxImpact_ =
  Impact'
    { totalImpact = Prelude.Nothing,
      maxImpact = pMaxImpact_
    }

-- | The cumulative dollar value that\'s observed for an anomaly.
impact_totalImpact :: Lens.Lens' Impact (Prelude.Maybe Prelude.Double)
impact_totalImpact = Lens.lens (\Impact' {totalImpact} -> totalImpact) (\s@Impact' {} a -> s {totalImpact = a} :: Impact)

-- | The maximum dollar value that\'s observed for an anomaly.
impact_maxImpact :: Lens.Lens' Impact Prelude.Double
impact_maxImpact = Lens.lens (\Impact' {maxImpact} -> maxImpact) (\s@Impact' {} a -> s {maxImpact = a} :: Impact)

instance Data.FromJSON Impact where
  parseJSON =
    Data.withObject
      "Impact"
      ( \x ->
          Impact'
            Prelude.<$> (x Data..:? "TotalImpact")
            Prelude.<*> (x Data..: "MaxImpact")
      )

instance Prelude.Hashable Impact where
  hashWithSalt _salt Impact' {..} =
    _salt `Prelude.hashWithSalt` totalImpact
      `Prelude.hashWithSalt` maxImpact

instance Prelude.NFData Impact where
  rnf Impact' {..} =
    Prelude.rnf totalImpact
      `Prelude.seq` Prelude.rnf maxImpact
