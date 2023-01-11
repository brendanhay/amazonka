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
-- Module      : Amazonka.CostExplorer.Types.CoverageCost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CoverageCost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | How much it costs to run an instance.
--
-- /See:/ 'newCoverageCost' smart constructor.
data CoverageCost = CoverageCost'
  { -- | How much an On-Demand Instance costs.
    onDemandCost :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageCost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onDemandCost', 'coverageCost_onDemandCost' - How much an On-Demand Instance costs.
newCoverageCost ::
  CoverageCost
newCoverageCost =
  CoverageCost' {onDemandCost = Prelude.Nothing}

-- | How much an On-Demand Instance costs.
coverageCost_onDemandCost :: Lens.Lens' CoverageCost (Prelude.Maybe Prelude.Text)
coverageCost_onDemandCost = Lens.lens (\CoverageCost' {onDemandCost} -> onDemandCost) (\s@CoverageCost' {} a -> s {onDemandCost = a} :: CoverageCost)

instance Data.FromJSON CoverageCost where
  parseJSON =
    Data.withObject
      "CoverageCost"
      ( \x ->
          CoverageCost'
            Prelude.<$> (x Data..:? "OnDemandCost")
      )

instance Prelude.Hashable CoverageCost where
  hashWithSalt _salt CoverageCost' {..} =
    _salt `Prelude.hashWithSalt` onDemandCost

instance Prelude.NFData CoverageCost where
  rnf CoverageCost' {..} = Prelude.rnf onDemandCost
