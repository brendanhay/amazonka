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
-- Module      : Network.AWS.SSM.Types.EffectivePatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.EffectivePatch where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.Patch
import Network.AWS.SSM.Types.PatchStatus

-- | The EffectivePatch structure defines metadata about a patch along with
-- the approval state of the patch in a particular patch baseline. The
-- approval state includes information about whether the patch is currently
-- approved, due to be approved by a rule, explicitly approved, or
-- explicitly rejected and the date the patch was or will be approved.
--
-- /See:/ 'newEffectivePatch' smart constructor.
data EffectivePatch = EffectivePatch'
  { -- | Provides metadata for a patch, including information such as the KB ID,
    -- severity, classification and a URL for where more information can be
    -- obtained about the patch.
    patch :: Prelude.Maybe Patch,
    -- | The status of the patch in a patch baseline. This includes information
    -- about whether the patch is currently approved, due to be approved by a
    -- rule, explicitly approved, or explicitly rejected and the date the patch
    -- was or will be approved.
    patchStatus :: Prelude.Maybe PatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EffectivePatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patch', 'effectivePatch_patch' - Provides metadata for a patch, including information such as the KB ID,
-- severity, classification and a URL for where more information can be
-- obtained about the patch.
--
-- 'patchStatus', 'effectivePatch_patchStatus' - The status of the patch in a patch baseline. This includes information
-- about whether the patch is currently approved, due to be approved by a
-- rule, explicitly approved, or explicitly rejected and the date the patch
-- was or will be approved.
newEffectivePatch ::
  EffectivePatch
newEffectivePatch =
  EffectivePatch'
    { patch = Prelude.Nothing,
      patchStatus = Prelude.Nothing
    }

-- | Provides metadata for a patch, including information such as the KB ID,
-- severity, classification and a URL for where more information can be
-- obtained about the patch.
effectivePatch_patch :: Lens.Lens' EffectivePatch (Prelude.Maybe Patch)
effectivePatch_patch = Lens.lens (\EffectivePatch' {patch} -> patch) (\s@EffectivePatch' {} a -> s {patch = a} :: EffectivePatch)

-- | The status of the patch in a patch baseline. This includes information
-- about whether the patch is currently approved, due to be approved by a
-- rule, explicitly approved, or explicitly rejected and the date the patch
-- was or will be approved.
effectivePatch_patchStatus :: Lens.Lens' EffectivePatch (Prelude.Maybe PatchStatus)
effectivePatch_patchStatus = Lens.lens (\EffectivePatch' {patchStatus} -> patchStatus) (\s@EffectivePatch' {} a -> s {patchStatus = a} :: EffectivePatch)

instance Prelude.FromJSON EffectivePatch where
  parseJSON =
    Prelude.withObject
      "EffectivePatch"
      ( \x ->
          EffectivePatch'
            Prelude.<$> (x Prelude..:? "Patch")
            Prelude.<*> (x Prelude..:? "PatchStatus")
      )

instance Prelude.Hashable EffectivePatch

instance Prelude.NFData EffectivePatch
