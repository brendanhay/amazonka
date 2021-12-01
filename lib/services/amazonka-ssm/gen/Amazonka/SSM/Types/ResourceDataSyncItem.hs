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
-- Module      : Amazonka.SSM.Types.ResourceDataSyncItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceDataSyncItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.LastResourceDataSyncStatus
import Amazonka.SSM.Types.ResourceDataSyncS3Destination
import Amazonka.SSM.Types.ResourceDataSyncSourceWithState

-- | Information about a resource data sync configuration, including its
-- current status and last successful sync.
--
-- /See:/ 'newResourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { -- | The type of resource data sync. If @SyncType@ is @SyncToDestination@,
    -- then the resource data sync synchronizes data to an S3 bucket. If the
    -- @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes
    -- data from Organizations or from multiple Amazon Web Services Regions.
    syncType :: Prelude.Maybe Prelude.Text,
    -- | Information about the source where the data was synchronized.
    syncSource :: Prelude.Maybe ResourceDataSyncSourceWithState,
    -- | The status message details reported by the last sync.
    lastSyncStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time the configuration was created (UTC).
    syncCreatedTime :: Prelude.Maybe Core.POSIX,
    -- | The last time the configuration attempted to sync (UTC).
    lastSyncTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the resource data sync.
    syncName :: Prelude.Maybe Prelude.Text,
    -- | The status reported by the last sync.
    lastStatus :: Prelude.Maybe LastResourceDataSyncStatus,
    -- | The date and time the resource data sync was changed.
    syncLastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | Configuration information for the target S3 bucket.
    s3Destination :: Prelude.Maybe ResourceDataSyncS3Destination,
    -- | The last time the sync operations returned a status of @SUCCESSFUL@
    -- (UTC).
    lastSuccessfulSyncTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceDataSyncItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncType', 'resourceDataSyncItem_syncType' - The type of resource data sync. If @SyncType@ is @SyncToDestination@,
-- then the resource data sync synchronizes data to an S3 bucket. If the
-- @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes
-- data from Organizations or from multiple Amazon Web Services Regions.
--
-- 'syncSource', 'resourceDataSyncItem_syncSource' - Information about the source where the data was synchronized.
--
-- 'lastSyncStatusMessage', 'resourceDataSyncItem_lastSyncStatusMessage' - The status message details reported by the last sync.
--
-- 'syncCreatedTime', 'resourceDataSyncItem_syncCreatedTime' - The date and time the configuration was created (UTC).
--
-- 'lastSyncTime', 'resourceDataSyncItem_lastSyncTime' - The last time the configuration attempted to sync (UTC).
--
-- 'syncName', 'resourceDataSyncItem_syncName' - The name of the resource data sync.
--
-- 'lastStatus', 'resourceDataSyncItem_lastStatus' - The status reported by the last sync.
--
-- 'syncLastModifiedTime', 'resourceDataSyncItem_syncLastModifiedTime' - The date and time the resource data sync was changed.
--
-- 's3Destination', 'resourceDataSyncItem_s3Destination' - Configuration information for the target S3 bucket.
--
-- 'lastSuccessfulSyncTime', 'resourceDataSyncItem_lastSuccessfulSyncTime' - The last time the sync operations returned a status of @SUCCESSFUL@
-- (UTC).
newResourceDataSyncItem ::
  ResourceDataSyncItem
newResourceDataSyncItem =
  ResourceDataSyncItem'
    { syncType = Prelude.Nothing,
      syncSource = Prelude.Nothing,
      lastSyncStatusMessage = Prelude.Nothing,
      syncCreatedTime = Prelude.Nothing,
      lastSyncTime = Prelude.Nothing,
      syncName = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      syncLastModifiedTime = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      lastSuccessfulSyncTime = Prelude.Nothing
    }

-- | The type of resource data sync. If @SyncType@ is @SyncToDestination@,
-- then the resource data sync synchronizes data to an S3 bucket. If the
-- @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes
-- data from Organizations or from multiple Amazon Web Services Regions.
resourceDataSyncItem_syncType :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_syncType = Lens.lens (\ResourceDataSyncItem' {syncType} -> syncType) (\s@ResourceDataSyncItem' {} a -> s {syncType = a} :: ResourceDataSyncItem)

-- | Information about the source where the data was synchronized.
resourceDataSyncItem_syncSource :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe ResourceDataSyncSourceWithState)
resourceDataSyncItem_syncSource = Lens.lens (\ResourceDataSyncItem' {syncSource} -> syncSource) (\s@ResourceDataSyncItem' {} a -> s {syncSource = a} :: ResourceDataSyncItem)

-- | The status message details reported by the last sync.
resourceDataSyncItem_lastSyncStatusMessage :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_lastSyncStatusMessage = Lens.lens (\ResourceDataSyncItem' {lastSyncStatusMessage} -> lastSyncStatusMessage) (\s@ResourceDataSyncItem' {} a -> s {lastSyncStatusMessage = a} :: ResourceDataSyncItem)

-- | The date and time the configuration was created (UTC).
resourceDataSyncItem_syncCreatedTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_syncCreatedTime = Lens.lens (\ResourceDataSyncItem' {syncCreatedTime} -> syncCreatedTime) (\s@ResourceDataSyncItem' {} a -> s {syncCreatedTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Core._Time

-- | The last time the configuration attempted to sync (UTC).
resourceDataSyncItem_lastSyncTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_lastSyncTime = Lens.lens (\ResourceDataSyncItem' {lastSyncTime} -> lastSyncTime) (\s@ResourceDataSyncItem' {} a -> s {lastSyncTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Core._Time

-- | The name of the resource data sync.
resourceDataSyncItem_syncName :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_syncName = Lens.lens (\ResourceDataSyncItem' {syncName} -> syncName) (\s@ResourceDataSyncItem' {} a -> s {syncName = a} :: ResourceDataSyncItem)

-- | The status reported by the last sync.
resourceDataSyncItem_lastStatus :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe LastResourceDataSyncStatus)
resourceDataSyncItem_lastStatus = Lens.lens (\ResourceDataSyncItem' {lastStatus} -> lastStatus) (\s@ResourceDataSyncItem' {} a -> s {lastStatus = a} :: ResourceDataSyncItem)

-- | The date and time the resource data sync was changed.
resourceDataSyncItem_syncLastModifiedTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_syncLastModifiedTime = Lens.lens (\ResourceDataSyncItem' {syncLastModifiedTime} -> syncLastModifiedTime) (\s@ResourceDataSyncItem' {} a -> s {syncLastModifiedTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Core._Time

-- | Configuration information for the target S3 bucket.
resourceDataSyncItem_s3Destination :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe ResourceDataSyncS3Destination)
resourceDataSyncItem_s3Destination = Lens.lens (\ResourceDataSyncItem' {s3Destination} -> s3Destination) (\s@ResourceDataSyncItem' {} a -> s {s3Destination = a} :: ResourceDataSyncItem)

-- | The last time the sync operations returned a status of @SUCCESSFUL@
-- (UTC).
resourceDataSyncItem_lastSuccessfulSyncTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_lastSuccessfulSyncTime = Lens.lens (\ResourceDataSyncItem' {lastSuccessfulSyncTime} -> lastSuccessfulSyncTime) (\s@ResourceDataSyncItem' {} a -> s {lastSuccessfulSyncTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ResourceDataSyncItem where
  parseJSON =
    Core.withObject
      "ResourceDataSyncItem"
      ( \x ->
          ResourceDataSyncItem'
            Prelude.<$> (x Core..:? "SyncType")
            Prelude.<*> (x Core..:? "SyncSource")
            Prelude.<*> (x Core..:? "LastSyncStatusMessage")
            Prelude.<*> (x Core..:? "SyncCreatedTime")
            Prelude.<*> (x Core..:? "LastSyncTime")
            Prelude.<*> (x Core..:? "SyncName")
            Prelude.<*> (x Core..:? "LastStatus")
            Prelude.<*> (x Core..:? "SyncLastModifiedTime")
            Prelude.<*> (x Core..:? "S3Destination")
            Prelude.<*> (x Core..:? "LastSuccessfulSyncTime")
      )

instance Prelude.Hashable ResourceDataSyncItem where
  hashWithSalt salt' ResourceDataSyncItem' {..} =
    salt' `Prelude.hashWithSalt` lastSuccessfulSyncTime
      `Prelude.hashWithSalt` s3Destination
      `Prelude.hashWithSalt` syncLastModifiedTime
      `Prelude.hashWithSalt` lastStatus
      `Prelude.hashWithSalt` syncName
      `Prelude.hashWithSalt` lastSyncTime
      `Prelude.hashWithSalt` syncCreatedTime
      `Prelude.hashWithSalt` lastSyncStatusMessage
      `Prelude.hashWithSalt` syncSource
      `Prelude.hashWithSalt` syncType

instance Prelude.NFData ResourceDataSyncItem where
  rnf ResourceDataSyncItem' {..} =
    Prelude.rnf syncType
      `Prelude.seq` Prelude.rnf lastSuccessfulSyncTime
      `Prelude.seq` Prelude.rnf s3Destination
      `Prelude.seq` Prelude.rnf syncLastModifiedTime
      `Prelude.seq` Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf syncName
      `Prelude.seq` Prelude.rnf lastSyncTime
      `Prelude.seq` Prelude.rnf syncCreatedTime
      `Prelude.seq` Prelude.rnf lastSyncStatusMessage
      `Prelude.seq` Prelude.rnf syncSource
