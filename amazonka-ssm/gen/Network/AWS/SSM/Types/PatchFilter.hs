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
-- Module      : Network.AWS.SSM.Types.PatchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.PatchFilterKey

-- | Defines which patches should be included in a patch baseline.
--
-- A patch filter consists of a key and a set of values. The filter key is
-- a patch property. For example, the available filter keys for WINDOWS are
-- PATCH_SET, PRODUCT, PRODUCT_FAMILY, CLASSIFICATION, and MSRC_SEVERITY.
-- The filter values define a matching criterion for the patch property
-- indicated by the key. For example, if the filter key is PRODUCT and the
-- filter values are [\"Office 2013\", \"Office 2016\"], then the filter
-- accepts all patches where product name is either \"Office 2013\" or
-- \"Office 2016\". The filter values can be exact values for the patch
-- property given as a key, or a wildcard (*), which matches all values.
--
-- You can view lists of valid values for the patch properties by running
-- the @DescribePatchProperties@ command. For information about which patch
-- properties can be used with each major operating system, see
-- DescribePatchProperties.
--
-- /See:/ 'newPatchFilter' smart constructor.
data PatchFilter = PatchFilter'
  { -- | The key for the filter.
    --
    -- Run the DescribePatchProperties command to view lists of valid keys for
    -- each operating system type.
    key :: PatchFilterKey,
    -- | The value for the filter key.
    --
    -- Run the DescribePatchProperties command to view lists of valid values
    -- for each key based on operating system type.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'patchFilter_key' - The key for the filter.
--
-- Run the DescribePatchProperties command to view lists of valid keys for
-- each operating system type.
--
-- 'values', 'patchFilter_values' - The value for the filter key.
--
-- Run the DescribePatchProperties command to view lists of valid values
-- for each key based on operating system type.
newPatchFilter ::
  -- | 'key'
  PatchFilterKey ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  PatchFilter
newPatchFilter pKey_ pValues_ =
  PatchFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | The key for the filter.
--
-- Run the DescribePatchProperties command to view lists of valid keys for
-- each operating system type.
patchFilter_key :: Lens.Lens' PatchFilter PatchFilterKey
patchFilter_key = Lens.lens (\PatchFilter' {key} -> key) (\s@PatchFilter' {} a -> s {key = a} :: PatchFilter)

-- | The value for the filter key.
--
-- Run the DescribePatchProperties command to view lists of valid values
-- for each key based on operating system type.
patchFilter_values :: Lens.Lens' PatchFilter (Prelude.NonEmpty Prelude.Text)
patchFilter_values = Lens.lens (\PatchFilter' {values} -> values) (\s@PatchFilter' {} a -> s {values = a} :: PatchFilter) Prelude.. Lens._Coerce

instance Core.FromJSON PatchFilter where
  parseJSON =
    Core.withObject
      "PatchFilter"
      ( \x ->
          PatchFilter'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Values")
      )

instance Prelude.Hashable PatchFilter

instance Prelude.NFData PatchFilter

instance Core.ToJSON PatchFilter where
  toJSON PatchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Values" Core..= values)
          ]
      )
