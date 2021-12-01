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
-- Module      : Amazonka.SSM.Types.PatchOrchestratorFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchOrchestratorFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines a filter used in Patch Manager APIs. Supported filter keys
-- depend on the API operation that includes the filter. Patch Manager API
-- operations that use @PatchOrchestratorFilter@ include the following:
--
-- -   DescribeAvailablePatches
--
-- -   DescribeInstancePatches
--
-- -   DescribePatchBaselines
--
-- -   DescribePatchGroups
--
-- /See:/ 'newPatchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { -- | The value for the filter.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The key for the filter.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchOrchestratorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'patchOrchestratorFilter_values' - The value for the filter.
--
-- 'key', 'patchOrchestratorFilter_key' - The key for the filter.
newPatchOrchestratorFilter ::
  PatchOrchestratorFilter
newPatchOrchestratorFilter =
  PatchOrchestratorFilter'
    { values = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The value for the filter.
patchOrchestratorFilter_values :: Lens.Lens' PatchOrchestratorFilter (Prelude.Maybe [Prelude.Text])
patchOrchestratorFilter_values = Lens.lens (\PatchOrchestratorFilter' {values} -> values) (\s@PatchOrchestratorFilter' {} a -> s {values = a} :: PatchOrchestratorFilter) Prelude.. Lens.mapping Lens.coerced

-- | The key for the filter.
patchOrchestratorFilter_key :: Lens.Lens' PatchOrchestratorFilter (Prelude.Maybe Prelude.Text)
patchOrchestratorFilter_key = Lens.lens (\PatchOrchestratorFilter' {key} -> key) (\s@PatchOrchestratorFilter' {} a -> s {key = a} :: PatchOrchestratorFilter)

instance Prelude.Hashable PatchOrchestratorFilter where
  hashWithSalt salt' PatchOrchestratorFilter' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData PatchOrchestratorFilter where
  rnf PatchOrchestratorFilter' {..} =
    Prelude.rnf values `Prelude.seq` Prelude.rnf key

instance Core.ToJSON PatchOrchestratorFilter where
  toJSON PatchOrchestratorFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
