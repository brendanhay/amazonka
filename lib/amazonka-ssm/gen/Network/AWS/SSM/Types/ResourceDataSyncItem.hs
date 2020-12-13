{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceDataSyncItem
  ( ResourceDataSyncItem (..),

    -- * Smart constructor
    mkResourceDataSyncItem,

    -- * Lenses
    rdsiSyncType,
    rdsiSyncSource,
    rdsiLastSyncStatusMessage,
    rdsiSyncCreatedTime,
    rdsiLastSyncTime,
    rdsiSyncName,
    rdsiLastStatus,
    rdsiSyncLastModifiedTime,
    rdsiS3Destination,
    rdsiLastSuccessfulSyncTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.LastResourceDataSyncStatus
import Network.AWS.SSM.Types.ResourceDataSyncS3Destination
import Network.AWS.SSM.Types.ResourceDataSyncSourceWithState

-- | Information about a Resource Data Sync configuration, including its current status and last successful sync.
--
-- /See:/ 'mkResourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { -- | The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
    syncType :: Lude.Maybe Lude.Text,
    -- | Information about the source where the data was synchronized.
    syncSource :: Lude.Maybe ResourceDataSyncSourceWithState,
    -- | The status message details reported by the last sync.
    lastSyncStatusMessage :: Lude.Maybe Lude.Text,
    -- | The date and time the configuration was created (UTC).
    syncCreatedTime :: Lude.Maybe Lude.Timestamp,
    -- | The last time the configuration attempted to sync (UTC).
    lastSyncTime :: Lude.Maybe Lude.Timestamp,
    -- | The name of the Resource Data Sync.
    syncName :: Lude.Maybe Lude.Text,
    -- | The status reported by the last sync.
    lastStatus :: Lude.Maybe LastResourceDataSyncStatus,
    -- | The date and time the resource data sync was changed.
    syncLastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | Configuration information for the target S3 bucket.
    s3Destination :: Lude.Maybe ResourceDataSyncS3Destination,
    -- | The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
    lastSuccessfulSyncTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDataSyncItem' with the minimum fields required to make a request.
--
-- * 'syncType' - The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
-- * 'syncSource' - Information about the source where the data was synchronized.
-- * 'lastSyncStatusMessage' - The status message details reported by the last sync.
-- * 'syncCreatedTime' - The date and time the configuration was created (UTC).
-- * 'lastSyncTime' - The last time the configuration attempted to sync (UTC).
-- * 'syncName' - The name of the Resource Data Sync.
-- * 'lastStatus' - The status reported by the last sync.
-- * 'syncLastModifiedTime' - The date and time the resource data sync was changed.
-- * 's3Destination' - Configuration information for the target S3 bucket.
-- * 'lastSuccessfulSyncTime' - The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
mkResourceDataSyncItem ::
  ResourceDataSyncItem
mkResourceDataSyncItem =
  ResourceDataSyncItem'
    { syncType = Lude.Nothing,
      syncSource = Lude.Nothing,
      lastSyncStatusMessage = Lude.Nothing,
      syncCreatedTime = Lude.Nothing,
      lastSyncTime = Lude.Nothing,
      syncName = Lude.Nothing,
      lastStatus = Lude.Nothing,
      syncLastModifiedTime = Lude.Nothing,
      s3Destination = Lude.Nothing,
      lastSuccessfulSyncTime = Lude.Nothing
    }

-- | The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncType :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Text)
rdsiSyncType = Lens.lens (syncType :: ResourceDataSyncItem -> Lude.Maybe Lude.Text) (\s a -> s {syncType = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiSyncType "Use generic-lens or generic-optics with 'syncType' instead." #-}

-- | Information about the source where the data was synchronized.
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncSource :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe ResourceDataSyncSourceWithState)
rdsiSyncSource = Lens.lens (syncSource :: ResourceDataSyncItem -> Lude.Maybe ResourceDataSyncSourceWithState) (\s a -> s {syncSource = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiSyncSource "Use generic-lens or generic-optics with 'syncSource' instead." #-}

-- | The status message details reported by the last sync.
--
-- /Note:/ Consider using 'lastSyncStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSyncStatusMessage :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Text)
rdsiLastSyncStatusMessage = Lens.lens (lastSyncStatusMessage :: ResourceDataSyncItem -> Lude.Maybe Lude.Text) (\s a -> s {lastSyncStatusMessage = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiLastSyncStatusMessage "Use generic-lens or generic-optics with 'lastSyncStatusMessage' instead." #-}

-- | The date and time the configuration was created (UTC).
--
-- /Note:/ Consider using 'syncCreatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncCreatedTime :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Timestamp)
rdsiSyncCreatedTime = Lens.lens (syncCreatedTime :: ResourceDataSyncItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {syncCreatedTime = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiSyncCreatedTime "Use generic-lens or generic-optics with 'syncCreatedTime' instead." #-}

-- | The last time the configuration attempted to sync (UTC).
--
-- /Note:/ Consider using 'lastSyncTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSyncTime :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Timestamp)
rdsiLastSyncTime = Lens.lens (lastSyncTime :: ResourceDataSyncItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSyncTime = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiLastSyncTime "Use generic-lens or generic-optics with 'lastSyncTime' instead." #-}

-- | The name of the Resource Data Sync.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncName :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Text)
rdsiSyncName = Lens.lens (syncName :: ResourceDataSyncItem -> Lude.Maybe Lude.Text) (\s a -> s {syncName = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

-- | The status reported by the last sync.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastStatus :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe LastResourceDataSyncStatus)
rdsiLastStatus = Lens.lens (lastStatus :: ResourceDataSyncItem -> Lude.Maybe LastResourceDataSyncStatus) (\s a -> s {lastStatus = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiLastStatus "Use generic-lens or generic-optics with 'lastStatus' instead." #-}

-- | The date and time the resource data sync was changed.
--
-- /Note:/ Consider using 'syncLastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncLastModifiedTime :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Timestamp)
rdsiSyncLastModifiedTime = Lens.lens (syncLastModifiedTime :: ResourceDataSyncItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {syncLastModifiedTime = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiSyncLastModifiedTime "Use generic-lens or generic-optics with 'syncLastModifiedTime' instead." #-}

-- | Configuration information for the target S3 bucket.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiS3Destination :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe ResourceDataSyncS3Destination)
rdsiS3Destination = Lens.lens (s3Destination :: ResourceDataSyncItem -> Lude.Maybe ResourceDataSyncS3Destination) (\s a -> s {s3Destination = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiS3Destination "Use generic-lens or generic-optics with 's3Destination' instead." #-}

-- | The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
--
-- /Note:/ Consider using 'lastSuccessfulSyncTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSuccessfulSyncTime :: Lens.Lens' ResourceDataSyncItem (Lude.Maybe Lude.Timestamp)
rdsiLastSuccessfulSyncTime = Lens.lens (lastSuccessfulSyncTime :: ResourceDataSyncItem -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastSuccessfulSyncTime = a} :: ResourceDataSyncItem)
{-# DEPRECATED rdsiLastSuccessfulSyncTime "Use generic-lens or generic-optics with 'lastSuccessfulSyncTime' instead." #-}

instance Lude.FromJSON ResourceDataSyncItem where
  parseJSON =
    Lude.withObject
      "ResourceDataSyncItem"
      ( \x ->
          ResourceDataSyncItem'
            Lude.<$> (x Lude..:? "SyncType")
            Lude.<*> (x Lude..:? "SyncSource")
            Lude.<*> (x Lude..:? "LastSyncStatusMessage")
            Lude.<*> (x Lude..:? "SyncCreatedTime")
            Lude.<*> (x Lude..:? "LastSyncTime")
            Lude.<*> (x Lude..:? "SyncName")
            Lude.<*> (x Lude..:? "LastStatus")
            Lude.<*> (x Lude..:? "SyncLastModifiedTime")
            Lude.<*> (x Lude..:? "S3Destination")
            Lude.<*> (x Lude..:? "LastSuccessfulSyncTime")
      )
