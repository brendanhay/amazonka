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
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types.DirectoryStage
import Amazonka.DirectoryService.Types.DirectoryVpcSettings
import Amazonka.DirectoryService.Types.RegionType
import qualified Amazonka.Prelude as Prelude

-- | The replicated Region information for a directory.
--
-- /See:/ 'newRegionDescription' smart constructor.
data RegionDescription = RegionDescription'
  { -- | The desired number of domain controllers in the specified Region for the
    -- specified directory.
    desiredNumberOfDomainControllers :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the Region description was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Specifies when the Region replication began.
    launchTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the Region. For example, @us-east-1@.
    regionName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the Region is the primary Region or an additional
    -- Region.
    regionType :: Prelude.Maybe RegionType,
    -- | The status of the replication process for the specified Region.
    status :: Prelude.Maybe DirectoryStage,
    -- | The date and time that the Region status was last updated.
    statusLastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
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
-- 'desiredNumberOfDomainControllers', 'regionDescription_desiredNumberOfDomainControllers' - The desired number of domain controllers in the specified Region for the
-- specified directory.
--
-- 'directoryId', 'regionDescription_directoryId' - The identifier of the directory.
--
-- 'lastUpdatedDateTime', 'regionDescription_lastUpdatedDateTime' - The date and time that the Region description was last updated.
--
-- 'launchTime', 'regionDescription_launchTime' - Specifies when the Region replication began.
--
-- 'regionName', 'regionDescription_regionName' - The name of the Region. For example, @us-east-1@.
--
-- 'regionType', 'regionDescription_regionType' - Specifies whether the Region is the primary Region or an additional
-- Region.
--
-- 'status', 'regionDescription_status' - The status of the replication process for the specified Region.
--
-- 'statusLastUpdatedDateTime', 'regionDescription_statusLastUpdatedDateTime' - The date and time that the Region status was last updated.
--
-- 'vpcSettings', 'regionDescription_vpcSettings' - Undocumented member.
newRegionDescription ::
  RegionDescription
newRegionDescription =
  RegionDescription'
    { desiredNumberOfDomainControllers =
        Prelude.Nothing,
      directoryId = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      launchTime = Prelude.Nothing,
      regionName = Prelude.Nothing,
      regionType = Prelude.Nothing,
      status = Prelude.Nothing,
      statusLastUpdatedDateTime = Prelude.Nothing,
      vpcSettings = Prelude.Nothing
    }

-- | The desired number of domain controllers in the specified Region for the
-- specified directory.
regionDescription_desiredNumberOfDomainControllers :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Natural)
regionDescription_desiredNumberOfDomainControllers = Lens.lens (\RegionDescription' {desiredNumberOfDomainControllers} -> desiredNumberOfDomainControllers) (\s@RegionDescription' {} a -> s {desiredNumberOfDomainControllers = a} :: RegionDescription)

-- | The identifier of the directory.
regionDescription_directoryId :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_directoryId = Lens.lens (\RegionDescription' {directoryId} -> directoryId) (\s@RegionDescription' {} a -> s {directoryId = a} :: RegionDescription)

-- | The date and time that the Region description was last updated.
regionDescription_lastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_lastUpdatedDateTime = Lens.lens (\RegionDescription' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@RegionDescription' {} a -> s {lastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Data._Time

-- | Specifies when the Region replication began.
regionDescription_launchTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_launchTime = Lens.lens (\RegionDescription' {launchTime} -> launchTime) (\s@RegionDescription' {} a -> s {launchTime = a} :: RegionDescription) Prelude.. Lens.mapping Data._Time

-- | The name of the Region. For example, @us-east-1@.
regionDescription_regionName :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.Text)
regionDescription_regionName = Lens.lens (\RegionDescription' {regionName} -> regionName) (\s@RegionDescription' {} a -> s {regionName = a} :: RegionDescription)

-- | Specifies whether the Region is the primary Region or an additional
-- Region.
regionDescription_regionType :: Lens.Lens' RegionDescription (Prelude.Maybe RegionType)
regionDescription_regionType = Lens.lens (\RegionDescription' {regionType} -> regionType) (\s@RegionDescription' {} a -> s {regionType = a} :: RegionDescription)

-- | The status of the replication process for the specified Region.
regionDescription_status :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryStage)
regionDescription_status = Lens.lens (\RegionDescription' {status} -> status) (\s@RegionDescription' {} a -> s {status = a} :: RegionDescription)

-- | The date and time that the Region status was last updated.
regionDescription_statusLastUpdatedDateTime :: Lens.Lens' RegionDescription (Prelude.Maybe Prelude.UTCTime)
regionDescription_statusLastUpdatedDateTime = Lens.lens (\RegionDescription' {statusLastUpdatedDateTime} -> statusLastUpdatedDateTime) (\s@RegionDescription' {} a -> s {statusLastUpdatedDateTime = a} :: RegionDescription) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
regionDescription_vpcSettings :: Lens.Lens' RegionDescription (Prelude.Maybe DirectoryVpcSettings)
regionDescription_vpcSettings = Lens.lens (\RegionDescription' {vpcSettings} -> vpcSettings) (\s@RegionDescription' {} a -> s {vpcSettings = a} :: RegionDescription)

instance Data.FromJSON RegionDescription where
  parseJSON =
    Data.withObject
      "RegionDescription"
      ( \x ->
          RegionDescription'
            Prelude.<$> (x Data..:? "DesiredNumberOfDomainControllers")
            Prelude.<*> (x Data..:? "DirectoryId")
            Prelude.<*> (x Data..:? "LastUpdatedDateTime")
            Prelude.<*> (x Data..:? "LaunchTime")
            Prelude.<*> (x Data..:? "RegionName")
            Prelude.<*> (x Data..:? "RegionType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusLastUpdatedDateTime")
            Prelude.<*> (x Data..:? "VpcSettings")
      )

instance Prelude.Hashable RegionDescription where
  hashWithSalt _salt RegionDescription' {..} =
    _salt
      `Prelude.hashWithSalt` desiredNumberOfDomainControllers
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` launchTime
      `Prelude.hashWithSalt` regionName
      `Prelude.hashWithSalt` regionType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusLastUpdatedDateTime
      `Prelude.hashWithSalt` vpcSettings

instance Prelude.NFData RegionDescription where
  rnf RegionDescription' {..} =
    Prelude.rnf desiredNumberOfDomainControllers
      `Prelude.seq` Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf launchTime
      `Prelude.seq` Prelude.rnf regionName
      `Prelude.seq` Prelude.rnf regionType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusLastUpdatedDateTime
      `Prelude.seq` Prelude.rnf vpcSettings
