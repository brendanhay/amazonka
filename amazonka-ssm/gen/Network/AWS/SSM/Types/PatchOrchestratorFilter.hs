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
-- Module      : Network.AWS.SSM.Types.PatchOrchestratorFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchOrchestratorFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Defines a filter used in Patch Manager APIs.
--
-- /See:/ 'newPatchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { -- | The key for the filter.
    key :: Core.Maybe Core.Text,
    -- | The value for the filter.
    values :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PatchOrchestratorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'patchOrchestratorFilter_key' - The key for the filter.
--
-- 'values', 'patchOrchestratorFilter_values' - The value for the filter.
newPatchOrchestratorFilter ::
  PatchOrchestratorFilter
newPatchOrchestratorFilter =
  PatchOrchestratorFilter'
    { key = Core.Nothing,
      values = Core.Nothing
    }

-- | The key for the filter.
patchOrchestratorFilter_key :: Lens.Lens' PatchOrchestratorFilter (Core.Maybe Core.Text)
patchOrchestratorFilter_key = Lens.lens (\PatchOrchestratorFilter' {key} -> key) (\s@PatchOrchestratorFilter' {} a -> s {key = a} :: PatchOrchestratorFilter)

-- | The value for the filter.
patchOrchestratorFilter_values :: Lens.Lens' PatchOrchestratorFilter (Core.Maybe [Core.Text])
patchOrchestratorFilter_values = Lens.lens (\PatchOrchestratorFilter' {values} -> values) (\s@PatchOrchestratorFilter' {} a -> s {values = a} :: PatchOrchestratorFilter) Core.. Lens.mapping Lens._Coerce

instance Core.Hashable PatchOrchestratorFilter

instance Core.NFData PatchOrchestratorFilter

instance Core.ToJSON PatchOrchestratorFilter where
  toJSON PatchOrchestratorFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Values" Core..=) Core.<$> values
          ]
      )
