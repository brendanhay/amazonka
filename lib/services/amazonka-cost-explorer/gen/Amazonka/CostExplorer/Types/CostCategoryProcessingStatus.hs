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
-- Module      : Amazonka.CostExplorer.Types.CostCategoryProcessingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryProcessingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryStatus
import Amazonka.CostExplorer.Types.CostCategoryStatusComponent
import qualified Amazonka.Prelude as Prelude

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- /See:/ 'newCostCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { -- | The process status for a specific cost category.
    status :: Prelude.Maybe CostCategoryStatus,
    -- | The Cost Management product name of the applied status.
    component :: Prelude.Maybe CostCategoryStatusComponent
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CostCategoryProcessingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'costCategoryProcessingStatus_status' - The process status for a specific cost category.
--
-- 'component', 'costCategoryProcessingStatus_component' - The Cost Management product name of the applied status.
newCostCategoryProcessingStatus ::
  CostCategoryProcessingStatus
newCostCategoryProcessingStatus =
  CostCategoryProcessingStatus'
    { status =
        Prelude.Nothing,
      component = Prelude.Nothing
    }

-- | The process status for a specific cost category.
costCategoryProcessingStatus_status :: Lens.Lens' CostCategoryProcessingStatus (Prelude.Maybe CostCategoryStatus)
costCategoryProcessingStatus_status = Lens.lens (\CostCategoryProcessingStatus' {status} -> status) (\s@CostCategoryProcessingStatus' {} a -> s {status = a} :: CostCategoryProcessingStatus)

-- | The Cost Management product name of the applied status.
costCategoryProcessingStatus_component :: Lens.Lens' CostCategoryProcessingStatus (Prelude.Maybe CostCategoryStatusComponent)
costCategoryProcessingStatus_component = Lens.lens (\CostCategoryProcessingStatus' {component} -> component) (\s@CostCategoryProcessingStatus' {} a -> s {component = a} :: CostCategoryProcessingStatus)

instance Core.FromJSON CostCategoryProcessingStatus where
  parseJSON =
    Core.withObject
      "CostCategoryProcessingStatus"
      ( \x ->
          CostCategoryProcessingStatus'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Component")
      )

instance
  Prelude.Hashable
    CostCategoryProcessingStatus
  where
  hashWithSalt _salt CostCategoryProcessingStatus' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` component

instance Prelude.NFData CostCategoryProcessingStatus where
  rnf CostCategoryProcessingStatus' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf component
