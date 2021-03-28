{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceDataSyncItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceDataSyncItem
  ( ResourceDataSyncItem (..)
  -- * Smart constructor
  , mkResourceDataSyncItem
  -- * Lenses
  , rdsiLastStatus
  , rdsiLastSuccessfulSyncTime
  , rdsiLastSyncStatusMessage
  , rdsiLastSyncTime
  , rdsiS3Destination
  , rdsiSyncCreatedTime
  , rdsiSyncLastModifiedTime
  , rdsiSyncName
  , rdsiSyncSource
  , rdsiSyncType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.LastResourceDataSyncMessage as Types
import qualified Network.AWS.SSM.Types.LastResourceDataSyncStatus as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncName as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncS3Destination as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncSourceWithState as Types
import qualified Network.AWS.SSM.Types.ResourceDataSyncType as Types

-- | Information about a Resource Data Sync configuration, including its current status and last successful sync.
--
-- /See:/ 'mkResourceDataSyncItem' smart constructor.
data ResourceDataSyncItem = ResourceDataSyncItem'
  { lastStatus :: Core.Maybe Types.LastResourceDataSyncStatus
    -- ^ The status reported by the last sync.
  , lastSuccessfulSyncTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
  , lastSyncStatusMessage :: Core.Maybe Types.LastResourceDataSyncMessage
    -- ^ The status message details reported by the last sync.
  , lastSyncTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last time the configuration attempted to sync (UTC).
  , s3Destination :: Core.Maybe Types.ResourceDataSyncS3Destination
    -- ^ Configuration information for the target S3 bucket.
  , syncCreatedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the configuration was created (UTC).
  , syncLastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time the resource data sync was changed. 
  , syncName :: Core.Maybe Types.ResourceDataSyncName
    -- ^ The name of the Resource Data Sync.
  , syncSource :: Core.Maybe Types.ResourceDataSyncSourceWithState
    -- ^ Information about the source where the data was synchronized. 
  , syncType :: Core.Maybe Types.ResourceDataSyncType
    -- ^ The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResourceDataSyncItem' value with any optional fields omitted.
mkResourceDataSyncItem
    :: ResourceDataSyncItem
mkResourceDataSyncItem
  = ResourceDataSyncItem'{lastStatus = Core.Nothing,
                          lastSuccessfulSyncTime = Core.Nothing,
                          lastSyncStatusMessage = Core.Nothing, lastSyncTime = Core.Nothing,
                          s3Destination = Core.Nothing, syncCreatedTime = Core.Nothing,
                          syncLastModifiedTime = Core.Nothing, syncName = Core.Nothing,
                          syncSource = Core.Nothing, syncType = Core.Nothing}

-- | The status reported by the last sync.
--
-- /Note:/ Consider using 'lastStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastStatus :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.LastResourceDataSyncStatus)
rdsiLastStatus = Lens.field @"lastStatus"
{-# INLINEABLE rdsiLastStatus #-}
{-# DEPRECATED lastStatus "Use generic-lens or generic-optics with 'lastStatus' instead"  #-}

-- | The last time the sync operations returned a status of @SUCCESSFUL@ (UTC).
--
-- /Note:/ Consider using 'lastSuccessfulSyncTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSuccessfulSyncTime :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Core.NominalDiffTime)
rdsiLastSuccessfulSyncTime = Lens.field @"lastSuccessfulSyncTime"
{-# INLINEABLE rdsiLastSuccessfulSyncTime #-}
{-# DEPRECATED lastSuccessfulSyncTime "Use generic-lens or generic-optics with 'lastSuccessfulSyncTime' instead"  #-}

-- | The status message details reported by the last sync.
--
-- /Note:/ Consider using 'lastSyncStatusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSyncStatusMessage :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.LastResourceDataSyncMessage)
rdsiLastSyncStatusMessage = Lens.field @"lastSyncStatusMessage"
{-# INLINEABLE rdsiLastSyncStatusMessage #-}
{-# DEPRECATED lastSyncStatusMessage "Use generic-lens or generic-optics with 'lastSyncStatusMessage' instead"  #-}

-- | The last time the configuration attempted to sync (UTC).
--
-- /Note:/ Consider using 'lastSyncTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiLastSyncTime :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Core.NominalDiffTime)
rdsiLastSyncTime = Lens.field @"lastSyncTime"
{-# INLINEABLE rdsiLastSyncTime #-}
{-# DEPRECATED lastSyncTime "Use generic-lens or generic-optics with 'lastSyncTime' instead"  #-}

-- | Configuration information for the target S3 bucket.
--
-- /Note:/ Consider using 's3Destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiS3Destination :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.ResourceDataSyncS3Destination)
rdsiS3Destination = Lens.field @"s3Destination"
{-# INLINEABLE rdsiS3Destination #-}
{-# DEPRECATED s3Destination "Use generic-lens or generic-optics with 's3Destination' instead"  #-}

-- | The date and time the configuration was created (UTC).
--
-- /Note:/ Consider using 'syncCreatedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncCreatedTime :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Core.NominalDiffTime)
rdsiSyncCreatedTime = Lens.field @"syncCreatedTime"
{-# INLINEABLE rdsiSyncCreatedTime #-}
{-# DEPRECATED syncCreatedTime "Use generic-lens or generic-optics with 'syncCreatedTime' instead"  #-}

-- | The date and time the resource data sync was changed. 
--
-- /Note:/ Consider using 'syncLastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncLastModifiedTime :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Core.NominalDiffTime)
rdsiSyncLastModifiedTime = Lens.field @"syncLastModifiedTime"
{-# INLINEABLE rdsiSyncLastModifiedTime #-}
{-# DEPRECATED syncLastModifiedTime "Use generic-lens or generic-optics with 'syncLastModifiedTime' instead"  #-}

-- | The name of the Resource Data Sync.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncName :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.ResourceDataSyncName)
rdsiSyncName = Lens.field @"syncName"
{-# INLINEABLE rdsiSyncName #-}
{-# DEPRECATED syncName "Use generic-lens or generic-optics with 'syncName' instead"  #-}

-- | Information about the source where the data was synchronized. 
--
-- /Note:/ Consider using 'syncSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncSource :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.ResourceDataSyncSourceWithState)
rdsiSyncSource = Lens.field @"syncSource"
{-# INLINEABLE rdsiSyncSource #-}
{-# DEPRECATED syncSource "Use generic-lens or generic-optics with 'syncSource' instead"  #-}

-- | The type of resource data sync. If @SyncType@ is @SyncToDestination@ , then the resource data sync synchronizes data to an S3 bucket. If the @SyncType@ is @SyncFromSource@ then the resource data sync synchronizes data from AWS Organizations or from multiple AWS Regions.
--
-- /Note:/ Consider using 'syncType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdsiSyncType :: Lens.Lens' ResourceDataSyncItem (Core.Maybe Types.ResourceDataSyncType)
rdsiSyncType = Lens.field @"syncType"
{-# INLINEABLE rdsiSyncType #-}
{-# DEPRECATED syncType "Use generic-lens or generic-optics with 'syncType' instead"  #-}

instance Core.FromJSON ResourceDataSyncItem where
        parseJSON
          = Core.withObject "ResourceDataSyncItem" Core.$
              \ x ->
                ResourceDataSyncItem' Core.<$>
                  (x Core..:? "LastStatus") Core.<*>
                    x Core..:? "LastSuccessfulSyncTime"
                    Core.<*> x Core..:? "LastSyncStatusMessage"
                    Core.<*> x Core..:? "LastSyncTime"
                    Core.<*> x Core..:? "S3Destination"
                    Core.<*> x Core..:? "SyncCreatedTime"
                    Core.<*> x Core..:? "SyncLastModifiedTime"
                    Core.<*> x Core..:? "SyncName"
                    Core.<*> x Core..:? "SyncSource"
                    Core.<*> x Core..:? "SyncType"
