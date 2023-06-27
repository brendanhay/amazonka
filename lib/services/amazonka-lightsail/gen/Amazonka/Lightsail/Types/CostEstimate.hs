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
-- Module      : Amazonka.Lightsail.Types.CostEstimate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.CostEstimate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.EstimateByTime
import qualified Amazonka.Prelude as Prelude

-- | Describes the estimated cost for resources in your Lightsail for
-- Research account.
--
-- /See:/ 'newCostEstimate' smart constructor.
data CostEstimate = CostEstimate'
  { -- | The cost estimate result that\'s associated with a time period.
    resultsByTime :: Prelude.Maybe [EstimateByTime],
    -- | The types of usage that are included in the estimate, such as costs,
    -- usage, or data transfer.
    usageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostEstimate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultsByTime', 'costEstimate_resultsByTime' - The cost estimate result that\'s associated with a time period.
--
-- 'usageType', 'costEstimate_usageType' - The types of usage that are included in the estimate, such as costs,
-- usage, or data transfer.
newCostEstimate ::
  CostEstimate
newCostEstimate =
  CostEstimate'
    { resultsByTime = Prelude.Nothing,
      usageType = Prelude.Nothing
    }

-- | The cost estimate result that\'s associated with a time period.
costEstimate_resultsByTime :: Lens.Lens' CostEstimate (Prelude.Maybe [EstimateByTime])
costEstimate_resultsByTime = Lens.lens (\CostEstimate' {resultsByTime} -> resultsByTime) (\s@CostEstimate' {} a -> s {resultsByTime = a} :: CostEstimate) Prelude.. Lens.mapping Lens.coerced

-- | The types of usage that are included in the estimate, such as costs,
-- usage, or data transfer.
costEstimate_usageType :: Lens.Lens' CostEstimate (Prelude.Maybe Prelude.Text)
costEstimate_usageType = Lens.lens (\CostEstimate' {usageType} -> usageType) (\s@CostEstimate' {} a -> s {usageType = a} :: CostEstimate)

instance Data.FromJSON CostEstimate where
  parseJSON =
    Data.withObject
      "CostEstimate"
      ( \x ->
          CostEstimate'
            Prelude.<$> (x Data..:? "resultsByTime" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "usageType")
      )

instance Prelude.Hashable CostEstimate where
  hashWithSalt _salt CostEstimate' {..} =
    _salt
      `Prelude.hashWithSalt` resultsByTime
      `Prelude.hashWithSalt` usageType

instance Prelude.NFData CostEstimate where
  rnf CostEstimate' {..} =
    Prelude.rnf resultsByTime
      `Prelude.seq` Prelude.rnf usageType
