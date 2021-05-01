{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines a filter used in Patch Manager APIs.
--
-- /See:/ 'newPatchOrchestratorFilter' smart constructor.
data PatchOrchestratorFilter = PatchOrchestratorFilter'
  { -- | The key for the filter.
    key :: Prelude.Maybe Prelude.Text,
    -- | The value for the filter.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      values = Prelude.Nothing
    }

-- | The key for the filter.
patchOrchestratorFilter_key :: Lens.Lens' PatchOrchestratorFilter (Prelude.Maybe Prelude.Text)
patchOrchestratorFilter_key = Lens.lens (\PatchOrchestratorFilter' {key} -> key) (\s@PatchOrchestratorFilter' {} a -> s {key = a} :: PatchOrchestratorFilter)

-- | The value for the filter.
patchOrchestratorFilter_values :: Lens.Lens' PatchOrchestratorFilter (Prelude.Maybe [Prelude.Text])
patchOrchestratorFilter_values = Lens.lens (\PatchOrchestratorFilter' {values} -> values) (\s@PatchOrchestratorFilter' {} a -> s {values = a} :: PatchOrchestratorFilter) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.Hashable PatchOrchestratorFilter

instance Prelude.NFData PatchOrchestratorFilter

instance Prelude.ToJSON PatchOrchestratorFilter where
  toJSON PatchOrchestratorFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Values" Prelude..=) Prelude.<$> values
          ]
      )
