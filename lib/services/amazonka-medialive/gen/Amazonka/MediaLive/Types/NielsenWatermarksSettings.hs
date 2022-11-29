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
-- Module      : Amazonka.MediaLive.Types.NielsenWatermarksSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.NielsenWatermarksSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MediaLive.Types.NielsenCBET
import Amazonka.MediaLive.Types.NielsenNaesIiNw
import Amazonka.MediaLive.Types.NielsenWatermarksDistributionTypes
import qualified Amazonka.Prelude as Prelude

-- | Nielsen Watermarks Settings
--
-- /See:/ 'newNielsenWatermarksSettings' smart constructor.
data NielsenWatermarksSettings = NielsenWatermarksSettings'
  { -- | Choose the distribution types that you want to assign to the watermarks:
    -- - PROGRAM_CONTENT - FINAL_DISTRIBUTOR
    nielsenDistributionType :: Prelude.Maybe NielsenWatermarksDistributionTypes,
    -- | Complete these fields only if you want to insert watermarks of type
    -- Nielsen NAES II (N2) and Nielsen NAES VI (NW).
    nielsenNaesIiNwSettings :: Prelude.Maybe NielsenNaesIiNw,
    -- | Complete these fields only if you want to insert watermarks of type
    -- Nielsen CBET
    nielsenCbetSettings :: Prelude.Maybe NielsenCBET
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NielsenWatermarksSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nielsenDistributionType', 'nielsenWatermarksSettings_nielsenDistributionType' - Choose the distribution types that you want to assign to the watermarks:
-- - PROGRAM_CONTENT - FINAL_DISTRIBUTOR
--
-- 'nielsenNaesIiNwSettings', 'nielsenWatermarksSettings_nielsenNaesIiNwSettings' - Complete these fields only if you want to insert watermarks of type
-- Nielsen NAES II (N2) and Nielsen NAES VI (NW).
--
-- 'nielsenCbetSettings', 'nielsenWatermarksSettings_nielsenCbetSettings' - Complete these fields only if you want to insert watermarks of type
-- Nielsen CBET
newNielsenWatermarksSettings ::
  NielsenWatermarksSettings
newNielsenWatermarksSettings =
  NielsenWatermarksSettings'
    { nielsenDistributionType =
        Prelude.Nothing,
      nielsenNaesIiNwSettings = Prelude.Nothing,
      nielsenCbetSettings = Prelude.Nothing
    }

-- | Choose the distribution types that you want to assign to the watermarks:
-- - PROGRAM_CONTENT - FINAL_DISTRIBUTOR
nielsenWatermarksSettings_nielsenDistributionType :: Lens.Lens' NielsenWatermarksSettings (Prelude.Maybe NielsenWatermarksDistributionTypes)
nielsenWatermarksSettings_nielsenDistributionType = Lens.lens (\NielsenWatermarksSettings' {nielsenDistributionType} -> nielsenDistributionType) (\s@NielsenWatermarksSettings' {} a -> s {nielsenDistributionType = a} :: NielsenWatermarksSettings)

-- | Complete these fields only if you want to insert watermarks of type
-- Nielsen NAES II (N2) and Nielsen NAES VI (NW).
nielsenWatermarksSettings_nielsenNaesIiNwSettings :: Lens.Lens' NielsenWatermarksSettings (Prelude.Maybe NielsenNaesIiNw)
nielsenWatermarksSettings_nielsenNaesIiNwSettings = Lens.lens (\NielsenWatermarksSettings' {nielsenNaesIiNwSettings} -> nielsenNaesIiNwSettings) (\s@NielsenWatermarksSettings' {} a -> s {nielsenNaesIiNwSettings = a} :: NielsenWatermarksSettings)

-- | Complete these fields only if you want to insert watermarks of type
-- Nielsen CBET
nielsenWatermarksSettings_nielsenCbetSettings :: Lens.Lens' NielsenWatermarksSettings (Prelude.Maybe NielsenCBET)
nielsenWatermarksSettings_nielsenCbetSettings = Lens.lens (\NielsenWatermarksSettings' {nielsenCbetSettings} -> nielsenCbetSettings) (\s@NielsenWatermarksSettings' {} a -> s {nielsenCbetSettings = a} :: NielsenWatermarksSettings)

instance Core.FromJSON NielsenWatermarksSettings where
  parseJSON =
    Core.withObject
      "NielsenWatermarksSettings"
      ( \x ->
          NielsenWatermarksSettings'
            Prelude.<$> (x Core..:? "nielsenDistributionType")
            Prelude.<*> (x Core..:? "nielsenNaesIiNwSettings")
            Prelude.<*> (x Core..:? "nielsenCbetSettings")
      )

instance Prelude.Hashable NielsenWatermarksSettings where
  hashWithSalt _salt NielsenWatermarksSettings' {..} =
    _salt
      `Prelude.hashWithSalt` nielsenDistributionType
      `Prelude.hashWithSalt` nielsenNaesIiNwSettings
      `Prelude.hashWithSalt` nielsenCbetSettings

instance Prelude.NFData NielsenWatermarksSettings where
  rnf NielsenWatermarksSettings' {..} =
    Prelude.rnf nielsenDistributionType
      `Prelude.seq` Prelude.rnf nielsenNaesIiNwSettings
      `Prelude.seq` Prelude.rnf nielsenCbetSettings

instance Core.ToJSON NielsenWatermarksSettings where
  toJSON NielsenWatermarksSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nielsenDistributionType" Core..=)
              Prelude.<$> nielsenDistributionType,
            ("nielsenNaesIiNwSettings" Core..=)
              Prelude.<$> nielsenNaesIiNwSettings,
            ("nielsenCbetSettings" Core..=)
              Prelude.<$> nielsenCbetSettings
          ]
      )
