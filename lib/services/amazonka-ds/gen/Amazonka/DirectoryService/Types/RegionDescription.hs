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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.RegionDescription where

import qualified Amazonka.Core as Core
import Amazonka.DirectoryService.Types.DirectoryStage
import Amazonka.DirectoryService.Types.DirectoryVpcSettings
import Amazonka.DirectoryService.Types.RegionType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The replicated Region information for a directory.
--
-- /See:/ 'newRegionDescription' smart constructor.
data RegionDescription = RegionDescription'
  { -- | The status of the replication process for the specified Region.
    status :: Prelude.Maybe DirectoryStage,
    -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Region. For example, @us-east-1@.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | The desired number of domain controllers in the specified Region for the
    -- specified directory.
    desiredNumberOfDomainControllers :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether the Region is the primary Region or an additional
    -- Region.
    regionType :: Prelude.Maybe RegionType,
    -- | Specifies when the Region replication began.
    launchTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the Region description was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the Region status was last updated.
    statusLastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    vpcSettings :: Prelude.Maybe DirectoryVpcSettings
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
-- 'status', 'regionDescription_status' - The status of the replication process for the specified Region.
--
-- 'directoryId', 'regionDescription_directoryId' - The identifier of the directory.
--
-- 'regionName', 'regionDescription_regionName' - The name of the Region. For example, @us-east-1@.
--
-- 'desiredNumberOfDomainControllers', 'regionDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the specified Region for the
-- specified directory.
--
-- 'regionType', 'regionDescription_regionType' - Specifies whether the Region is the primary Region or an additional
-- Region.
--
-- 'launchTime', 'regionDescription_launchTime' - Specifies when the Region replication began.
--
-- 'lastUpdatedDateTime', 'regionDescription_lastUpdatedDateTime' - The date and time that the Region description was last updated.
--
-- 'statusLastUpdatedDateTime', 'regionDescription_statusLastUpdatedDateTime' - The date and time that the Region status was last updated.
--
-- 'vpcSettings', 'regionDescription_vpcSettings' - Undocumented member.
newRegionDescription ::
  RegionDescription
newRegionDescription =
  RegionDescription'
    { status = Prelude.Nothing,
      directoryId = Prelude.Nothing,
      regionName = Prelude.Nothing,
      desiredNumberOfDomainControllers = Prelude.Nothing,
      regionType = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      statusLastUpdatedDateTime = Prelude.Nothing,
      vpcSettings = Prelude.Nothing
    }

-- | The status of the replication process for the specified Region.
regionDescription_status :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryStage)
regionDescription_status = Lens.lens (\RegionDescription' {status} -> status) (\s@RegionDescription' {} a -> s {status = a} :: RegionDescription)

-- | The identifier of the directory.
regionDescription_directoryId :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_directoryId = Lens.lens (\RegionDescription' {directoryId} -> directoryId) (\s@RegionDescription' {} a -> s {directoryId = a} :: RegionDescription)

-- | The name of the Region. For example, @us-east-1@.
regionDescription_regionName :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_regionName = Lens.lens (\RegionDescription' {regionName} -> regionName) (\s@RegionDescription' {} a -> s {regionName = a} :: RegionDescription)

-- | The desired number of domain controllers in the specified Region for the
-- specified directory.
regionDescription_desiredNumberOfDomainControllers :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Natural)
regionDescription_desiredNumberOfDomainControllers = Lens.lens (\RegionDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@RegionDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: RegionDescription)

-- | Specifies whether the Region is the primary Region or an additional
-- Region.
regionDescription_regionType :: Lens.Lens' RegionDescription (Prelude.Maybe RegionType)
regionDescription_regionType = Lens.lens (\RegionDescription' {regionType} -> regionType) (\s@RegionDescription' {} a -> s {regionType = a} :: RegionDescription)

-- | Specifies when the Region replication began.
regionDescription_launchTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_launchTime = Lens.lens (\RegionDescription' {launchTime} -> launchTime) (\s@RegionDescription' {} a -> s {launchTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

-- | The date and time that the Region description was last updated.
regionDescription_lastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_lastUpdatedDateTime = Lens.lens (\RegionDescription' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RegionDescription' {} a -> s {lastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

-- | The date and time that the Region status was last updated.
regionDescription_statusLastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_statusLastUpdatedDateTime = Lens.lens (\RegionDescription' {statusLastUpdatedDateTime} -> statusLastUpdatedDateTime) (\s@RegionDescription' {} a -> s {statusLastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
regionDescription_vpcSettings :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryVpcSettings)
regionDescription_vpcSettings = Lens.lens (\RegionDescription' {vpcSettings} -> vpcSettings) (\s@RegionDescription' {} a -> s {vpcSettings = a} :: RegionDescription)

instance Core.FromJSON RegionDescription where
  parseJSON =
    Core.withObject
      "RegionDescription"
      ( \x ->
          RegionDescription'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DirectoryId")
            Prelude.<*> (x Core..:? "RegionName")
            Prelude.<*> (x Core..:? "DesiredNumberOfDomainControllers")
            Prelude.<*> (x Core..:? "RegionType")
            Prelude.<*> (x Core..:? "LaunchTime")
            Prelude.<*> (x Core..:? "LastUpdatedDateTime")
            Prelude.<*> (x Core..:? "StatusLastUpdatedDateTime")
            Prelude.<*> (x Core..:? "VpcSettings")
      )

instance Prelude.Hashable RegionDescription where
  hashWithSalt salt' RegionDescription' {..} =
    salt' `Prelude.hashWithSalt` vpcSettings
      `Prelude.hashWithSalt` statusLastUpdatedDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` regionType
      `Prelude.hashWithSalt` desiredNumberOfDomainControllers
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` status

instance Prelude.NFData RegionDescription where
  rnf RegionDescription' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcSettings
      `Prelude.seq` Prelude.rnf statusLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf regionType
      `Prelude.seq` Prelude.rnf desiredNumberOfDomainControllers
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf directoryId
