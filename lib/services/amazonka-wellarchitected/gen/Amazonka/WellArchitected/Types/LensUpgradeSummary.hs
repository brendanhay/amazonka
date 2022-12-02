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
-- Module      : Amazonka.WellArchitected.Types.LensUpgradeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.LensUpgradeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lens upgrade summary return object.
--
-- /See:/ 'newLensUpgradeSummary' smart constructor.
data LensUpgradeSummary = LensUpgradeSummary'
  { -- | The current version of the lens.
    currentLensVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    workloadName :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the lens.
    latestLensVersion :: Prelude.Maybe Prelude.Text,
    workloadId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LensUpgradeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentLensVersion', 'lensUpgradeSummary_currentLensVersion' - The current version of the lens.
--
-- 'lensArn', 'lensUpgradeSummary_lensArn' - The ARN for the lens.
--
-- 'lensAlias', 'lensUpgradeSummary_lensAlias' - Undocumented member.
--
-- 'workloadName', 'lensUpgradeSummary_workloadName' - Undocumented member.
--
-- 'latestLensVersion', 'lensUpgradeSummary_latestLensVersion' - The latest version of the lens.
--
-- 'workloadId', 'lensUpgradeSummary_workloadId' - Undocumented member.
newLensUpgradeSummary ::
  LensUpgradeSummary
newLensUpgradeSummary =
  LensUpgradeSummary'
    { currentLensVersion =
        Prelude.Nothing,
      lensArn = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      workloadName = Prelude.Nothing,
      latestLensVersion = Prelude.Nothing,
      workloadId = Prelude.Nothing
    }

-- | The current version of the lens.
lensUpgradeSummary_currentLensVersion :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_currentLensVersion = Lens.lens (\LensUpgradeSummary' {currentLensVersion} -> currentLensVersion) (\s@LensUpgradeSummary' {} a -> s {currentLensVersion = a} :: LensUpgradeSummary)

-- | The ARN for the lens.
lensUpgradeSummary_lensArn :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_lensArn = Lens.lens (\LensUpgradeSummary' {lensArn} -> lensArn) (\s@LensUpgradeSummary' {} a -> s {lensArn = a} :: LensUpgradeSummary)

-- | Undocumented member.
lensUpgradeSummary_lensAlias :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_lensAlias = Lens.lens (\LensUpgradeSummary' {lensAlias} -> lensAlias) (\s@LensUpgradeSummary' {} a -> s {lensAlias = a} :: LensUpgradeSummary)

-- | Undocumented member.
lensUpgradeSummary_workloadName :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_workloadName = Lens.lens (\LensUpgradeSummary' {workloadName} -> workloadName) (\s@LensUpgradeSummary' {} a -> s {workloadName = a} :: LensUpgradeSummary)

-- | The latest version of the lens.
lensUpgradeSummary_latestLensVersion :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_latestLensVersion = Lens.lens (\LensUpgradeSummary' {latestLensVersion} -> latestLensVersion) (\s@LensUpgradeSummary' {} a -> s {latestLensVersion = a} :: LensUpgradeSummary)

-- | Undocumented member.
lensUpgradeSummary_workloadId :: Lens.Lens' LensUpgradeSummary (Prelude.Maybe Prelude.Text)
lensUpgradeSummary_workloadId = Lens.lens (\LensUpgradeSummary' {workloadId} -> workloadId) (\s@LensUpgradeSummary' {} a -> s {workloadId = a} :: LensUpgradeSummary)

instance Data.FromJSON LensUpgradeSummary where
  parseJSON =
    Data.withObject
      "LensUpgradeSummary"
      ( \x ->
          LensUpgradeSummary'
            Prelude.<$> (x Data..:? "CurrentLensVersion")
            Prelude.<*> (x Data..:? "LensArn")
            Prelude.<*> (x Data..:? "LensAlias")
            Prelude.<*> (x Data..:? "WorkloadName")
            Prelude.<*> (x Data..:? "LatestLensVersion")
            Prelude.<*> (x Data..:? "WorkloadId")
      )

instance Prelude.Hashable LensUpgradeSummary where
  hashWithSalt _salt LensUpgradeSummary' {..} =
    _salt `Prelude.hashWithSalt` currentLensVersion
      `Prelude.hashWithSalt` lensArn
      `Prelude.hashWithSalt` lensAlias
      `Prelude.hashWithSalt` workloadName
      `Prelude.hashWithSalt` latestLensVersion
      `Prelude.hashWithSalt` workloadId

instance Prelude.NFData LensUpgradeSummary where
  rnf LensUpgradeSummary' {..} =
    Prelude.rnf currentLensVersion
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf workloadName
      `Prelude.seq` Prelude.rnf latestLensVersion
      `Prelude.seq` Prelude.rnf workloadId
