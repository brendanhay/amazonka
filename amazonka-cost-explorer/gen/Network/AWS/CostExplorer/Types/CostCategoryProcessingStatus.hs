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
-- Module      : Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.CostCategoryProcessingStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.CostCategoryStatus
import Network.AWS.CostExplorer.Types.CostCategoryStatusComponent
import qualified Network.AWS.Lens as Lens

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- /See:/ 'newCostCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { -- | The process status for a specific cost category.
    status :: Core.Maybe CostCategoryStatus,
    -- | The Cost Management product name of the applied status.
    component :: Core.Maybe CostCategoryStatusComponent
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      component = Core.Nothing
    }

-- | The process status for a specific cost category.
costCategoryProcessingStatus_status :: Lens.Lens' CostCategoryProcessingStatus (Core.Maybe CostCategoryStatus)
costCategoryProcessingStatus_status = Lens.lens (\CostCategoryProcessingStatus' {status} -> status) (\s@CostCategoryProcessingStatus' {} a -> s {status = a} :: CostCategoryProcessingStatus)

-- | The Cost Management product name of the applied status.
costCategoryProcessingStatus_component :: Lens.Lens' CostCategoryProcessingStatus (Core.Maybe CostCategoryStatusComponent)
costCategoryProcessingStatus_component = Lens.lens (\CostCategoryProcessingStatus' {component} -> component) (\s@CostCategoryProcessingStatus' {} a -> s {component = a} :: CostCategoryProcessingStatus)

instance Core.FromJSON CostCategoryProcessingStatus where
  parseJSON =
    Core.withObject
      "CostCategoryProcessingStatus"
      ( \x ->
          CostCategoryProcessingStatus'
            Core.<$> (x Core..:? "Status")
            Core.<*> (x Core..:? "Component")
      )

instance Core.Hashable CostCategoryProcessingStatus

instance Core.NFData CostCategoryProcessingStatus
