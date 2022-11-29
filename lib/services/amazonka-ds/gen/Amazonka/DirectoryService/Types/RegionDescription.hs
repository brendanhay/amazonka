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
-- Module      : Amazonka.DirectoryService.Types.RegionDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RegionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectoryService.Types.DirectoryStage
import Amazonka.DirectoryService.Types.DirectoryVpcSettings
import Amazonka.DirectoryService.Types.RegionType
import qualified Amazonka.Prelude as Prelude

-- | The replicated Region information for a directory.
--
-- /See:/ 'newRegionDescription' smart constructor.
data RegionDescription = RegionDescription'
  { -- | The date and time that the Region status was last updated.
    statusLastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | Specifies when the Region replication began.
    launchTime :: Prelude.Maybe Core.POSIX,
    vpcSettings :: Prelude.Maybe DirectoryVpcSettings,
    -- | The status of the replication process for the specified Region.
    status :: Prelude.Maybe DirectoryStage,
    -- | The name of the Region. For example, @us-east-1@.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the Region is the primary Region or an additional
    -- Region.
    regionType :: Prelude.Maybe RegionType,
    -- | The desired number of domain controllers in the specified Region for the
    -- specified directory.
    desiredNumberOfDomainControllers :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that the Region description was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusLastUpdatedDateTime', 'regionDescription_statusLastUpdatedDateTime' - The date and time that the Region status was last updated.
--
-- 'directoryId', 'regionDescription_directoryId' - The identifier of the directory.
--
-- 'launchTime', 'regionDescription_launchTime' - Specifies when the Region replication began.
--
-- 'vpcSettings', 'regionDescription_vpcSettings' - Undocumented member.
--
-- 'status', 'regionDescription_status' - The status of the replication process for the specified Region.
--
-- 'regionName', 'regionDescription_regionName' - The name of the Region. For example, @us-east-1@.
--
-- 'regionType', 'regionDescription_regionType' - Specifies whether the Region is the primary Region or an additional
-- Region.
--
-- 'desiredNumberOfDomainControllers', 'regionDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the specified Region for the
-- specified directory.
--
-- 'lastUpdatedDateTime', 'regionDescription_lastUpdatedDateTime' - The date and time that the Region description was last updated.
newRegionDescription ::
  RegionDescription
newRegionDescription =
  RegionDescription'
    { statusLastUpdatedDateTime =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      vpcSettings = Prelude.Nothing,
      status = Prelude.Nothing,
      regionName = Prelude.Nothing,
      regionType = Prelude.Nothing,
      desiredNumberOfDomainControllers = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The date and time that the Region status was last updated.
regionDescription_statusLastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_statusLastUpdatedDateTime = Lens.lens (\RegionDescription' {statusLastUpdatedDateTime} -> statusLastUpdatedDateTime) (\s@RegionDescription' {} a -> s {statusLastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

-- | The identifier of the directory.
regionDescription_directoryId :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_directoryId = Lens.lens (\RegionDescription' {directoryId} -> directoryId) (\s@RegionDescription' {} a -> s {directoryId = a} :: RegionDescription)

-- | Specifies when the Region replication began.
regionDescription_launchTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_launchTime = Lens.lens (\RegionDescription' {launchTime} -> launchTime) (\s@RegionDescription' {} a -> s {launchTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
regionDescription_vpcSettings :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryVpcSettings)
regionDescription_vpcSettings = Lens.lens (\RegionDescription' {vpcSettings} -> vpcSettings) (\s@RegionDescription' {} a -> s {vpcSettings = a} :: RegionDescription)

-- | The status of the replication process for the specified Region.
regionDescription_status :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryStage)
regionDescription_status = Lens.lens (\RegionDescription' {status} -> status) (\s@RegionDescription' {} a -> s {status = a} :: RegionDescription)

-- | The name of the Region. For example, @us-east-1@.
regionDescription_regionName :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_regionName = Lens.lens (\RegionDescription' {regionName} -> regionName) (\s@RegionDescription' {} a -> s {regionName = a} :: RegionDescription)

-- | Specifies whether the Region is the primary Region or an additional
-- Region.
regionDescription_regionType :: Lens.Lens' RegionDescription (Prelude.Maybe RegionType)
regionDescription_regionType = Lens.lens (\RegionDescription' {regionType} -> regionType) (\s@RegionDescription' {} a -> s {regionType = a} :: RegionDescription)

-- | The desired number of domain controllers in the specified Region for the
-- specified directory.
regionDescription_desiredNumberOfDomainControllers :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Natural)
regionDescription_desiredNumberOfDomainControllers = Lens.lens (\RegionDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@RegionDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: RegionDescription)

-- | The date and time that the Region description was last updated.
regionDescription_lastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_lastUpdatedDateTime = Lens.lens (\RegionDescription' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RegionDescription' {} a -> s {lastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RegionDescription where
  parseJSON =
    Core.withObject
      "RegionDescription"
      ( \x ->
          RegionDescription'
            Prelude.<$> (x Core..:? "StatusLastUpdatedDateTime")
            Prelude.<*> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "LaunchTime")
            Prelude.<*> (x Core..:? "VpcSettings")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> (x Core..:? "RegionType")
            Prelude.<*> (x Core..:? "DesiredNumberOfDomainControllers")
            Prelude.<*> (x Core..:? "LastUpdatedDateTime")
      )

instance Prelude.Hashable RegionDescription where
  hashWithSalt _salt RegionDescription' {..} =
    _salt
      `Prelude.hashWithSalt` statusLastUpdatedDateTime
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` vpcSettings
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` regionType
      `Prelude.hashWithSalt` desiredNumberOfDomainControllers
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData RegionDescription where
  rnf RegionDescription' {..} =
    Prelude.rnf statusLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf vpcSettings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf regionType
      `Prelude.seq` Prelude.rnf desiredNumberOfDomainControllers
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
