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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.CostCategoryProcessingStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CostExplorer.Types.CostCategoryStatus
import Amazonka.CostExplorer.Types.CostCategoryStatusComponent
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of processing statuses for Cost Management products for a
-- specific cost category.
--
-- /See:/ 'newCostCategoryProcessingStatus' smart constructor.
data CostCategoryProcessingStatus = CostCategoryProcessingStatus'
  { -- | The Cost Management product name of the applied status.
    component :: Prelude.Maybe CostCategoryStatusComponent,
    -- | The process status for a specific cost category.
    status :: Prelude.Maybe CostCategoryStatus
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
-- 'component', 'costCategoryProcessingStatus_component' - The Cost Management product name of the applied status.
--
-- 'status', 'costCategoryProcessingStatus_status' - The process status for a specific cost category.
newCostCategoryProcessingStatus ::
  CostCategoryProcessingStatus
newCostCategoryProcessingStatus =
  CostCategoryProcessingStatus'
    { component =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Cost Management product name of the applied status.
costCategoryProcessingStatus_component :: Lens.Lens' CostCategoryProcessingStatus (Prelude.Maybe CostCategoryStatusComponent)
costCategoryProcessingStatus_component = Lens.lens (\CostCategoryProcessingStatus' {component} -> component) (\s@CostCategoryProcessingStatus' {} a -> s {component = a} :: CostCategoryProcessingStatus)

-- | The process status for a specific cost category.
costCategoryProcessingStatus_status :: Lens.Lens' CostCategoryProcessingStatus (Prelude.Maybe CostCategoryStatus)
costCategoryProcessingStatus_status = Lens.lens (\CostCategoryProcessingStatus' {status} -> status) (\s@CostCategoryProcessingStatus' {} a -> s {status = a} :: CostCategoryProcessingStatus)

instance Data.FromJSON CostCategoryProcessingStatus where
  parseJSON =
    Data.withObject
      "CostCategoryProcessingStatus"
      ( \x ->
          CostCategoryProcessingStatus'
            Prelude.<$> (x Data..:? "Component")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    CostCategoryProcessingStatus
  where
  hashWithSalt _salt CostCategoryProcessingStatus' {..} =
    _salt
      `Prelude.hashWithSalt` component
      `Prelude.hashWithSalt` status

instance Prelude.NFData CostCategoryProcessingStatus where
  rnf CostCategoryProcessingStatus' {..} =
    Prelude.rnf component
      `Prelude.seq` Prelude.rnf status
