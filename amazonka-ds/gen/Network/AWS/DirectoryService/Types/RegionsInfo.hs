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
-- Module      : Network.AWS.DirectoryService.Types.RegionsInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.RegionsInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information about the Regions that are configured for
-- multi-Region replication.
--
-- /See:/ 'newRegionsInfo' smart constructor.
data RegionsInfo = RegionsInfo'
  { -- | Lists the Regions where the directory has been replicated, excluding the
    -- primary Region.
    additionalRegions :: Core.Maybe [Core.Text],
    -- | The Region where the AWS Managed Microsoft AD directory was originally
    -- created.
    primaryRegion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'primaryRegion', 'regionsInfo_primaryRegion' - The Region where the AWS Managed Microsoft AD directory was originally
-- created.
newRegionsInfo ::
  RegionsInfo
newRegionsInfo =
  RegionsInfo'
    { additionalRegions = Core.Nothing,
      primaryRegion = Core.Nothing
    }

-- | Lists the Regions where the directory has been replicated, excluding the
-- primary Region.
regionsInfo_additionalRegions :: Lens.Lens' RegionsInfo (Core.Maybe [Core.Text])
regionsInfo_additionalRegions = Lens.lens (\RegionsInfo' {additionalRegions} -> additionalRegions) (\s@RegionsInfo' {} a -> s {additionalRegions = a} :: RegionsInfo) Core.. Lens.mapping Lens._Coerce

-- | The Region where the AWS Managed Microsoft AD directory was originally
-- created.
regionsInfo_primaryRegion :: Lens.Lens' RegionsInfo (Core.Maybe Core.Text)
regionsInfo_primaryRegion = Lens.lens (\RegionsInfo' {primaryRegion} -> primaryRegion) (\s@RegionsInfo' {} a -> s {primaryRegion = a} :: RegionsInfo)

instance Core.FromJSON RegionsInfo where
  parseJSON =
    Core.withObject
      "RegionsInfo"
      ( \x ->
          RegionsInfo'
            Core.<$> (x Core..:? "AdditionalRegions" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PrimaryRegion")
      )

instance Core.Hashable RegionsInfo

instance Core.NFData RegionsInfo
