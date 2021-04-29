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
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncItem where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.LastResourceDataSyncStatus
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState

-- | Information about a Resource Data Sync configuration, including its
-- current status and last successful sync.
--
-- /See:/ 'newResourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { -- | The type of resource data sync. If @SyncType@ is @SyncToDestination@,
    -- then the resource data sync synchronizes data to an S3 bucket. If the
    -- @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes
    -- data from AWS Organizations or from multiple AWS Regions.
    syncType :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for the target S3 bucket.
    s3Destination :: Prelude.Maybe ResourceDataSyncS3Destination,
    -- | The date and time the resource data sync was changed.
    syncLastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The last time the configuration attempted to sync (UTC).
    lastSyncTime :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the Resource Data Sync.
    syncName :: Prelude.Maybe Prelude.Text,
    -- | Information about the source where the data was synchronized.
    syncSource :: Prelude.Maybe ResourceDataSyncSourceWithState,
    -- | The last time the sync operations returned a status of @SUCCESSFUL@
    -- (UTC).
    lastSuccessfulSyncTime :: Prelude.Maybe Prelude.POSIX,
    -- | The status reported by the last sync.
    lastStatus :: Prelude.Maybe LastResourceDataSyncStatus,
    -- | The status message details reported by the last sync.
    lastSyncStatusMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time the configuration was created (UTC).
    syncCreatedTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- data from AWS Organizations or from multiple AWS Regions.
--
-- 's3Destination', 'resourceDataSyncItem_s3Destination' - Configuration information for the target S3 bucket.
--
-- 'syncLastModifiedTime', 'resourceDataSyncItem_syncLastModifiedTime' - The date and time the resource data sync was changed.
--
-- 'lastSyncTime', 'resourceDataSyncItem_lastSyncTime' - The last time the configuration attempted to sync (UTC).
--
-- 'syncName', 'resourceDataSyncItem_syncName' - The name of the Resource Data Sync.
--
-- 'syncSource', 'resourceDataSyncItem_syncSource' - Information about the source where the data was synchronized.
--
-- 'lastSuccessfulSyncTime', 'resourceDataSyncItem_lastSuccessfulSyncTime' - The last time the sync operations returned a status of @SUCCESSFUL@
-- (UTC).
--
-- 'lastStatus', 'resourceDataSyncItem_lastStatus' - The status reported by the last sync.
--
-- 'lastSyncStatusMessage', 'resourceDataSyncItem_lastSyncStatusMessage' - The status message details reported by the last sync.
--
-- 'syncCreatedTime', 'resourceDataSyncItem_syncCreatedTime' - The date and time the configuration was created (UTC).
newResourceDataSyncItem ::
  ResourceDataSyncItem
newResourceDataSyncItem =
  ResourceDataSyncItem'
    { syncType = Prelude.Nothing,
      s3Destination = Prelude.Nothing,
      syncLastModifiedTime = Prelude.Nothing,
      lastSyncTime = Prelude.Nothing,
      syncName = Prelude.Nothing,
      syncSource = Prelude.Nothing,
      lastSuccessfulSyncTime = Prelude.Nothing,
      lastStatus = Prelude.Nothing,
      lastSyncStatusMessage = Prelude.Nothing,
      syncCreatedTime = Prelude.Nothing
    }

-- | The type of resource data sync. If @SyncType@ is @SyncToDestination@,
-- then the resource data sync synchronizes data to an S3 bucket. If the
-- @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes
-- data from AWS Organizations or from multiple AWS Regions.
resourceDataSyncItem_syncType :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_syncType = Lens.lens (\ResourceDataSyncItem' {syncType} -> syncType) (\s@ResourceDataSyncItem' {} a -> s {syncType = a} :: ResourceDataSyncItem)

-- | Configuration information for the target S3 bucket.
resourceDataSyncItem_s3Destination :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe ResourceDataSyncS3Destination)
resourceDataSyncItem_s3Destination = Lens.lens (\ResourceDataSyncItem' {s3Destination} -> s3Destination) (\s@ResourceDataSyncItem' {} a -> s {s3Destination = a} :: ResourceDataSyncItem)

-- | The date and time the resource data sync was changed.
resourceDataSyncItem_syncLastModifiedTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_syncLastModifiedTime = Lens.lens (\ResourceDataSyncItem' {syncLastModifiedTime} -> syncLastModifiedTime) (\s@ResourceDataSyncItem' {} a -> s {syncLastModifiedTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Prelude._Time

-- | The last time the configuration attempted to sync (UTC).
resourceDataSyncItem_lastSyncTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_lastSyncTime = Lens.lens (\ResourceDataSyncItem' {lastSyncTime} -> lastSyncTime) (\s@ResourceDataSyncItem' {} a -> s {lastSyncTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Prelude._Time

-- | The name of the Resource Data Sync.
resourceDataSyncItem_syncName :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_syncName = Lens.lens (\ResourceDataSyncItem' {syncName} -> syncName) (\s@ResourceDataSyncItem' {} a -> s {syncName = a} :: ResourceDataSyncItem)

-- | Information about the source where the data was synchronized.
resourceDataSyncItem_syncSource :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe ResourceDataSyncSourceWithState)
resourceDataSyncItem_syncSource = Lens.lens (\ResourceDataSyncItem' {syncSource} -> syncSource) (\s@ResourceDataSyncItem' {} a -> s {syncSource = a} :: ResourceDataSyncItem)

-- | The last time the sync operations returned a status of @SUCCESSFUL@
-- (UTC).
resourceDataSyncItem_lastSuccessfulSyncTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_lastSuccessfulSyncTime = Lens.lens (\ResourceDataSyncItem' {lastSuccessfulSyncTime} -> lastSuccessfulSyncTime) (\s@ResourceDataSyncItem' {} a -> s {lastSuccessfulSyncTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Prelude._Time

-- | The status reported by the last sync.
resourceDataSyncItem_lastStatus :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe LastResourceDataSyncStatus)
resourceDataSyncItem_lastStatus = Lens.lens (\ResourceDataSyncItem' {lastStatus} -> lastStatus) (\s@ResourceDataSyncItem' {} a -> s {lastStatus = a} :: ResourceDataSyncItem)

-- | The status message details reported by the last sync.
resourceDataSyncItem_lastSyncStatusMessage :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.Text)
resourceDataSyncItem_lastSyncStatusMessage = Lens.lens (\ResourceDataSyncItem' {lastSyncStatusMessage} -> lastSyncStatusMessage) (\s@ResourceDataSyncItem' {} a -> s {lastSyncStatusMessage = a} :: ResourceDataSyncItem)

-- | The date and time the configuration was created (UTC).
resourceDataSyncItem_syncCreatedTime :: Lens.Lens' ResourceDataSyncItem (Prelude.Maybe Prelude.UTCTime)
resourceDataSyncItem_syncCreatedTime = Lens.lens (\ResourceDataSyncItem' {syncCreatedTime} -> syncCreatedTime) (\s@ResourceDataSyncItem' {} a -> s {syncCreatedTime = a} :: ResourceDataSyncItem) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON ResourceDataSyncItem where
  parseJSON =
    Prelude.withObject
      "ResourceDataSyncItem"
      ( \x ->
          ResourceDataSyncItem'
            Prelude.<$> (x Prelude..:? "SyncType")
            Prelude.<*> (x Prelude..:? "S3Destination")
            Prelude.<*> (x Prelude..:? "SyncLastModifiedTime")
            Prelude.<*> (x Prelude..:? "LastSyncTime")
            Prelude.<*> (x Prelude..:? "SyncName")
            Prelude.<*> (x Prelude..:? "SyncSource")
            Prelude.<*> (x Prelude..:? "LastSuccessfulSyncTime")
            Prelude.<*> (x Prelude..:? "LastStatus")
            Prelude.<*> (x Prelude..:? "LastSyncStatusMessage")
            Prelude.<*> (x Prelude..:? "SyncCreatedTime")
      )

instance Prelude.Hashable ResourceDataSyncItem

instance Prelude.NFData ResourceDataSyncItem
