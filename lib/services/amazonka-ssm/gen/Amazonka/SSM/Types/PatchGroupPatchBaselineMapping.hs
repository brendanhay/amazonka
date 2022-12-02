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
-- Module      : Amazonka.SSM.Types.PatchGroupPatchBaselineMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchGroupPatchBaselineMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.PatchBaselineIdentity

-- | The mapping between a patch group and the patch baseline the patch group
-- is registered with.
--
-- /See:/ 'newPatchGroupPatchBaselineMapping' smart constructor.
data PatchGroupPatchBaselineMapping = PatchGroupPatchBaselineMapping'
  { -- | The name of the patch group registered with the patch baseline.
    patchGroup :: Prelude.Maybe Prelude.Text,
    -- | The patch baseline the patch group is registered with.
    baselineIdentity :: Prelude.Maybe PatchBaselineIdentity
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchGroupPatchBaselineMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchGroup', 'patchGroupPatchBaselineMapping_patchGroup' - The name of the patch group registered with the patch baseline.
--
-- 'baselineIdentity', 'patchGroupPatchBaselineMapping_baselineIdentity' - The patch baseline the patch group is registered with.
newPatchGroupPatchBaselineMapping ::
  PatchGroupPatchBaselineMapping
newPatchGroupPatchBaselineMapping =
  PatchGroupPatchBaselineMapping'
    { patchGroup =
        Prelude.Nothing,
      baselineIdentity = Prelude.Nothing
    }

-- | The name of the patch group registered with the patch baseline.
patchGroupPatchBaselineMapping_patchGroup :: Lens.Lens' PatchGroupPatchBaselineMapping (Prelude.Maybe Prelude.Text)
patchGroupPatchBaselineMapping_patchGroup = Lens.lens (\PatchGroupPatchBaselineMapping' {patchGroup} -> patchGroup) (\s@PatchGroupPatchBaselineMapping' {} a -> s {patchGroup = a} :: PatchGroupPatchBaselineMapping)

-- | The patch baseline the patch group is registered with.
patchGroupPatchBaselineMapping_baselineIdentity :: Lens.Lens' PatchGroupPatchBaselineMapping (Prelude.Maybe PatchBaselineIdentity)
patchGroupPatchBaselineMapping_baselineIdentity = Lens.lens (\PatchGroupPatchBaselineMapping' {baselineIdentity} -> baselineIdentity) (\s@PatchGroupPatchBaselineMapping' {} a -> s {baselineIdentity = a} :: PatchGroupPatchBaselineMapping)

instance Data.FromJSON PatchGroupPatchBaselineMapping where
  parseJSON =
    Data.withObject
      "PatchGroupPatchBaselineMapping"
      ( \x ->
          PatchGroupPatchBaselineMapping'
            Prelude.<$> (x Data..:? "PatchGroup")
            Prelude.<*> (x Data..:? "BaselineIdentity")
      )

instance
  Prelude.Hashable
    PatchGroupPatchBaselineMapping
  where
  hashWithSalt
    _salt
    PatchGroupPatchBaselineMapping' {..} =
      _salt `Prelude.hashWithSalt` patchGroup
        `Prelude.hashWithSalt` baselineIdentity

instance
  Prelude.NFData
    PatchGroupPatchBaselineMapping
  where
  rnf PatchGroupPatchBaselineMapping' {..} =
    Prelude.rnf patchGroup
      `Prelude.seq` Prelude.rnf baselineIdentity
