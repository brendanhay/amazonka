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
-- Module      : Network.AWS.SSM.Types.PatchBaselineIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchBaselineIdentity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OperatingSystem

-- | Defines the basic information about a patch baseline.
--
-- /See:/ 'newPatchBaselineIdentity' smart constructor.
data PatchBaselineIdentity = PatchBaselineIdentity'
  { -- | The name of the patch baseline.
    baselineName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | Whether this is the default baseline. Note that Systems Manager supports
    -- creating multiple default patch baselines. For example, you can create a
    -- default patch baseline for each operating system.
    defaultBaseline :: Prelude.Maybe Prelude.Bool,
    -- | The description of the patch baseline.
    baselineDescription :: Prelude.Maybe Prelude.Text,
    -- | Defines the operating system the patch baseline applies to. The Default
    -- value is WINDOWS.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PatchBaselineIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineName', 'patchBaselineIdentity_baselineName' - The name of the patch baseline.
--
-- 'baselineId', 'patchBaselineIdentity_baselineId' - The ID of the patch baseline.
--
-- 'defaultBaseline', 'patchBaselineIdentity_defaultBaseline' - Whether this is the default baseline. Note that Systems Manager supports
-- creating multiple default patch baselines. For example, you can create a
-- default patch baseline for each operating system.
--
-- 'baselineDescription', 'patchBaselineIdentity_baselineDescription' - The description of the patch baseline.
--
-- 'operatingSystem', 'patchBaselineIdentity_operatingSystem' - Defines the operating system the patch baseline applies to. The Default
-- value is WINDOWS.
newPatchBaselineIdentity ::
  PatchBaselineIdentity
newPatchBaselineIdentity =
  PatchBaselineIdentity'
    { baselineName =
        Prelude.Nothing,
      baselineId = Prelude.Nothing,
      defaultBaseline = Prelude.Nothing,
      baselineDescription = Prelude.Nothing,
      operatingSystem = Prelude.Nothing
    }

-- | The name of the patch baseline.
patchBaselineIdentity_baselineName :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineName = Lens.lens (\PatchBaselineIdentity' {baselineName} -> baselineName) (\s@PatchBaselineIdentity' {} a -> s {baselineName = a} :: PatchBaselineIdentity)

-- | The ID of the patch baseline.
patchBaselineIdentity_baselineId :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineId = Lens.lens (\PatchBaselineIdentity' {baselineId} -> baselineId) (\s@PatchBaselineIdentity' {} a -> s {baselineId = a} :: PatchBaselineIdentity)

-- | Whether this is the default baseline. Note that Systems Manager supports
-- creating multiple default patch baselines. For example, you can create a
-- default patch baseline for each operating system.
patchBaselineIdentity_defaultBaseline :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Bool)
patchBaselineIdentity_defaultBaseline = Lens.lens (\PatchBaselineIdentity' {defaultBaseline} -> defaultBaseline) (\s@PatchBaselineIdentity' {} a -> s {defaultBaseline = a} :: PatchBaselineIdentity)

-- | The description of the patch baseline.
patchBaselineIdentity_baselineDescription :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineDescription = Lens.lens (\PatchBaselineIdentity' {baselineDescription} -> baselineDescription) (\s@PatchBaselineIdentity' {} a -> s {baselineDescription = a} :: PatchBaselineIdentity)

-- | Defines the operating system the patch baseline applies to. The Default
-- value is WINDOWS.
patchBaselineIdentity_operatingSystem :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe OperatingSystem)
patchBaselineIdentity_operatingSystem = Lens.lens (\PatchBaselineIdentity' {operatingSystem} -> operatingSystem) (\s@PatchBaselineIdentity' {} a -> s {operatingSystem = a} :: PatchBaselineIdentity)

instance Prelude.FromJSON PatchBaselineIdentity where
  parseJSON =
    Prelude.withObject
      "PatchBaselineIdentity"
      ( \x ->
          PatchBaselineIdentity'
            Prelude.<$> (x Prelude..:? "BaselineName")
            Prelude.<*> (x Prelude..:? "BaselineId")
            Prelude.<*> (x Prelude..:? "DefaultBaseline")
            Prelude.<*> (x Prelude..:? "BaselineDescription")
            Prelude.<*> (x Prelude..:? "OperatingSystem")
      )

instance Prelude.Hashable PatchBaselineIdentity

instance Prelude.NFData PatchBaselineIdentity
