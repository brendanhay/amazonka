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
-- Module      : Amazonka.SSM.Types.PatchBaselineIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.PatchBaselineIdentity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OperatingSystem

-- | Defines the basic information about a patch baseline.
--
-- /See:/ 'newPatchBaselineIdentity' smart constructor.
data PatchBaselineIdentity = PatchBaselineIdentity'
  { -- | The description of the patch baseline.
    baselineDescription :: Prelude.Maybe Prelude.Text,
    -- | The ID of the patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The name of the patch baseline.
    baselineName :: Prelude.Maybe Prelude.Text,
    -- | Whether this is the default baseline. Amazon Web Services Systems
    -- Manager supports creating multiple default patch baselines. For example,
    -- you can create a default patch baseline for each operating system.
    defaultBaseline :: Prelude.Maybe Prelude.Bool,
    -- | Defines the operating system the patch baseline applies to. The default
    -- value is @WINDOWS@.
    operatingSystem :: Prelude.Maybe OperatingSystem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PatchBaselineIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineDescription', 'patchBaselineIdentity_baselineDescription' - The description of the patch baseline.
--
-- 'baselineId', 'patchBaselineIdentity_baselineId' - The ID of the patch baseline.
--
-- 'baselineName', 'patchBaselineIdentity_baselineName' - The name of the patch baseline.
--
-- 'defaultBaseline', 'patchBaselineIdentity_defaultBaseline' - Whether this is the default baseline. Amazon Web Services Systems
-- Manager supports creating multiple default patch baselines. For example,
-- you can create a default patch baseline for each operating system.
--
-- 'operatingSystem', 'patchBaselineIdentity_operatingSystem' - Defines the operating system the patch baseline applies to. The default
-- value is @WINDOWS@.
newPatchBaselineIdentity ::
  PatchBaselineIdentity
newPatchBaselineIdentity =
  PatchBaselineIdentity'
    { baselineDescription =
        Prelude.Nothing,
      baselineId = Prelude.Nothing,
      baselineName = Prelude.Nothing,
      defaultBaseline = Prelude.Nothing,
      operatingSystem = Prelude.Nothing
    }

-- | The description of the patch baseline.
patchBaselineIdentity_baselineDescription :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineDescription = Lens.lens (\PatchBaselineIdentity' {baselineDescription} -> baselineDescription) (\s@PatchBaselineIdentity' {} a -> s {baselineDescription = a} :: PatchBaselineIdentity)

-- | The ID of the patch baseline.
patchBaselineIdentity_baselineId :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineId = Lens.lens (\PatchBaselineIdentity' {baselineId} -> baselineId) (\s@PatchBaselineIdentity' {} a -> s {baselineId = a} :: PatchBaselineIdentity)

-- | The name of the patch baseline.
patchBaselineIdentity_baselineName :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Text)
patchBaselineIdentity_baselineName = Lens.lens (\PatchBaselineIdentity' {baselineName} -> baselineName) (\s@PatchBaselineIdentity' {} a -> s {baselineName = a} :: PatchBaselineIdentity)

-- | Whether this is the default baseline. Amazon Web Services Systems
-- Manager supports creating multiple default patch baselines. For example,
-- you can create a default patch baseline for each operating system.
patchBaselineIdentity_defaultBaseline :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe Prelude.Bool)
patchBaselineIdentity_defaultBaseline = Lens.lens (\PatchBaselineIdentity' {defaultBaseline} -> defaultBaseline) (\s@PatchBaselineIdentity' {} a -> s {defaultBaseline = a} :: PatchBaselineIdentity)

-- | Defines the operating system the patch baseline applies to. The default
-- value is @WINDOWS@.
patchBaselineIdentity_operatingSystem :: Lens.Lens' PatchBaselineIdentity (Prelude.Maybe OperatingSystem)
patchBaselineIdentity_operatingSystem = Lens.lens (\PatchBaselineIdentity' {operatingSystem} -> operatingSystem) (\s@PatchBaselineIdentity' {} a -> s {operatingSystem = a} :: PatchBaselineIdentity)

instance Data.FromJSON PatchBaselineIdentity where
  parseJSON =
    Data.withObject
      "PatchBaselineIdentity"
      ( \x ->
          PatchBaselineIdentity'
            Prelude.<$> (x Data..:? "BaselineDescription")
            Prelude.<*> (x Data..:? "BaselineId")
            Prelude.<*> (x Data..:? "BaselineName")
            Prelude.<*> (x Data..:? "DefaultBaseline")
            Prelude.<*> (x Data..:? "OperatingSystem")
      )

instance Prelude.Hashable PatchBaselineIdentity where
  hashWithSalt _salt PatchBaselineIdentity' {..} =
    _salt `Prelude.hashWithSalt` baselineDescription
      `Prelude.hashWithSalt` baselineId
      `Prelude.hashWithSalt` baselineName
      `Prelude.hashWithSalt` defaultBaseline
      `Prelude.hashWithSalt` operatingSystem

instance Prelude.NFData PatchBaselineIdentity where
  rnf PatchBaselineIdentity' {..} =
    Prelude.rnf baselineDescription
      `Prelude.seq` Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf baselineName
      `Prelude.seq` Prelude.rnf defaultBaseline
      `Prelude.seq` Prelude.rnf operatingSystem
