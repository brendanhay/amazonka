{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.MaintenanceTrack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.MaintenanceTrack
  ( MaintenanceTrack (..)
  -- * Smart constructor
  , mkMaintenanceTrack
  -- * Lenses
  , mtDatabaseVersion
  , mtMaintenanceTrackName
  , mtUpdateTargets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.UpdateTarget as Types

-- | Defines a maintenance track that determines which Amazon Redshift version to apply during a maintenance window. If the value for @MaintenanceTrack@ is @current@ , the cluster is updated to the most recently certified maintenance release. If the value is @trailing@ , the cluster is updated to the previously certified maintenance release. 
--
-- /See:/ 'mkMaintenanceTrack' smart constructor.
data MaintenanceTrack = MaintenanceTrack'
  { databaseVersion :: Core.Maybe Core.Text
    -- ^ The version number for the cluster release.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the maintenance track. Possible values are @current@ and @trailing@ .
  , updateTargets :: Core.Maybe [Types.UpdateTarget]
    -- ^ An array of 'UpdateTarget' objects to update with the maintenance track. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MaintenanceTrack' value with any optional fields omitted.
mkMaintenanceTrack
    :: MaintenanceTrack
mkMaintenanceTrack
  = MaintenanceTrack'{databaseVersion = Core.Nothing,
                      maintenanceTrackName = Core.Nothing, updateTargets = Core.Nothing}

-- | The version number for the cluster release.
--
-- /Note:/ Consider using 'databaseVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtDatabaseVersion :: Lens.Lens' MaintenanceTrack (Core.Maybe Core.Text)
mtDatabaseVersion = Lens.field @"databaseVersion"
{-# INLINEABLE mtDatabaseVersion #-}
{-# DEPRECATED databaseVersion "Use generic-lens or generic-optics with 'databaseVersion' instead"  #-}

-- | The name of the maintenance track. Possible values are @current@ and @trailing@ .
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtMaintenanceTrackName :: Lens.Lens' MaintenanceTrack (Core.Maybe Core.Text)
mtMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE mtMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | An array of 'UpdateTarget' objects to update with the maintenance track. 
--
-- /Note:/ Consider using 'updateTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mtUpdateTargets :: Lens.Lens' MaintenanceTrack (Core.Maybe [Types.UpdateTarget])
mtUpdateTargets = Lens.field @"updateTargets"
{-# INLINEABLE mtUpdateTargets #-}
{-# DEPRECATED updateTargets "Use generic-lens or generic-optics with 'updateTargets' instead"  #-}

instance Core.FromXML MaintenanceTrack where
        parseXML x
          = MaintenanceTrack' Core.<$>
              (x Core..@? "DatabaseVersion") Core.<*>
                x Core..@? "MaintenanceTrackName"
                Core.<*>
                x Core..@? "UpdateTargets" Core..<@>
                  Core.parseXMLList "UpdateTarget"
