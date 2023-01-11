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
-- Module      : Amazonka.DirectoryService.Types.RegionsInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RegionsInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the Regions that are configured for
-- multi-Region replication.
--
-- /See:/ 'newRegionsInfo' smart constructor.
data RegionsInfo = RegionsInfo'
  { -- | Lists the Regions where the directory has been replicated, excluding the
    -- primary Region.
    additionalRegions :: Prelude.Maybe [Prelude.Text],
    -- | The Region where the Managed Microsoft AD directory was originally
    -- created.
    primaryRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionsInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalRegions', 'regionsInfo_additionalRegions' - Lists the Regions where the directory has been replicated, excluding the
-- primary Region.
--
-- 'primaryRegion', 'regionsInfo_primaryRegion' - The Region where the Managed Microsoft AD directory was originally
-- created.
newRegionsInfo ::
  RegionsInfo
newRegionsInfo =
  RegionsInfo'
    { additionalRegions = Prelude.Nothing,
      primaryRegion = Prelude.Nothing
    }

-- | Lists the Regions where the directory has been replicated, excluding the
-- primary Region.
regionsInfo_additionalRegions :: Lens.Lens' RegionsInfo (Prelude.Maybe [Prelude.Text])
regionsInfo_additionalRegions = Lens.lens (\RegionsInfo' {additionalRegions} -> additionalRegions) (\s@RegionsInfo' {} a -> s {additionalRegions = a} :: RegionsInfo) Prelude.. Lens.mapping Lens.coerced

-- | The Region where the Managed Microsoft AD directory was originally
-- created.
regionsInfo_primaryRegion :: Lens.Lens' RegionsInfo (Prelude.Maybe Prelude.Text)
regionsInfo_primaryRegion = Lens.lens (\RegionsInfo' {primaryRegion} -> primaryRegion) (\s@RegionsInfo' {} a -> s {primaryRegion = a} :: RegionsInfo)

instance Data.FromJSON RegionsInfo where
  parseJSON =
    Data.withObject
      "RegionsInfo"
      ( \x ->
          RegionsInfo'
            Prelude.<$> ( x Data..:? "AdditionalRegions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PrimaryRegion")
      )

instance Prelude.Hashable RegionsInfo where
  hashWithSalt _salt RegionsInfo' {..} =
    _salt `Prelude.hashWithSalt` additionalRegions
      `Prelude.hashWithSalt` primaryRegion

instance Prelude.NFData RegionsInfo where
  rnf RegionsInfo' {..} =
    Prelude.rnf additionalRegions
      `Prelude.seq` Prelude.rnf primaryRegion
