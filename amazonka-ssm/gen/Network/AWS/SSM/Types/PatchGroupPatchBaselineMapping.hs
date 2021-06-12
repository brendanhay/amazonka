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
-- Module      : Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchGroupPatchBaselineMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.PatchBaselineIdentity

-- | The mapping between a patch group and the patch baseline the patch group
-- is registered with.
--
-- /See:/ 'newPatchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { -- | The patch baseline the patch group is registered with.
    baselineIdentity :: Core.Maybe PatchBaselineIdentity,
    -- | The name of the patch group registered with the patch baseline.
    patchGroup :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PatchGroupPatchBaselineMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineIdentity', 'patchGroupPatchBaselineMapping_baselineIdentity' - The patch baseline the patch group is registered with.
--
-- 'patchGroup', 'patchGroupPatchBaselineMapping_patchGroup' - The name of the patch group registered with the patch baseline.
newPatchGroupPatchBaselineMapping ::
  PatchGroupPatchBaselineMapping
newPatchGroupPatchBaselineMapping =
  PatchGroupPatchBaselineMapping'
    { baselineIdentity =
        Core.Nothing,
      patchGroup = Core.Nothing
    }

-- | The patch baseline the patch group is registered with.
patchGroupPatchBaselineMapping_baselineIdentity :: Lens.Lens' PatchGroupPatchBaselineMapping (Core.Maybe PatchBaselineIdentity)
patchGroupPatchBaselineMapping_baselineIdentity = Lens.lens (\PatchGroupPatchBaselineMapping' {baselineIdentity} -> baselineIdentity) (\s@PatchGroupPatchBaselineMapping' {} a -> s {baselineIdentity = a} :: PatchGroupPatchBaselineMapping)

-- | The name of the patch group registered with the patch baseline.
patchGroupPatchBaselineMapping_patchGroup :: Lens.Lens' PatchGroupPatchBaselineMapping (Core.Maybe Core.Text)
patchGroupPatchBaselineMapping_patchGroup = Lens.lens (\PatchGroupPatchBaselineMapping' {patchGroup} -> patchGroup) (\s@PatchGroupPatchBaselineMapping' {} a -> s {patchGroup = a} :: PatchGroupPatchBaselineMapping)

instance Core.FromJSON PatchGroupPatchBaselineMapping where
  parseJSON =
    Core.withObject
      "PatchGroupPatchBaselineMapping"
      ( \x ->
          PatchGroupPatchBaselineMapping'
            Core.<$> (x Core..:? "BaselineIdentity")
            Core.<*> (x Core..:? "PatchGroup")
      )

instance Core.Hashable PatchGroupPatchBaselineMapping

instance Core.NFData PatchGroupPatchBaselineMapping
