{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UpdateTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.UpdateTarget
  ( UpdateTarget (..)
  -- * Smart constructor
  , mkUpdateTarget
  -- * Lenses
  , utDatabaseVersion
  , utMaintenanceTrackName
  , utSupportedOperations
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.SupportedOperation as Types

-- | A maintenance track that you can switch the current track to.
--
-- /See:/ 'mkUpdateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { databaseVersion :: Core.Maybe Core.Text
    -- ^ The cluster version for the new maintenance track.
  , maintenanceTrackName :: Core.Maybe Core.Text
    -- ^ The name of the new maintenance track.
  , supportedOperations :: Core.Maybe [Types.SupportedOperation]
    -- ^ A list of operations supported by the maintenance track.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateTarget' value with any optional fields omitted.
mkUpdateTarget
    :: UpdateTarget
mkUpdateTarget
  = UpdateTarget'{databaseVersion = Core.Nothing,
                  maintenanceTrackName = Core.Nothing,
                  supportedOperations = Core.Nothing}

-- | The cluster version for the new maintenance track.
--
-- /Note:/ Consider using 'databaseVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utDatabaseVersion :: Lens.Lens' UpdateTarget (Core.Maybe Core.Text)
utDatabaseVersion = Lens.field @"databaseVersion"
{-# INLINEABLE utDatabaseVersion #-}
{-# DEPRECATED databaseVersion "Use generic-lens or generic-optics with 'databaseVersion' instead"  #-}

-- | The name of the new maintenance track.
--
-- /Note:/ Consider using 'maintenanceTrackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utMaintenanceTrackName :: Lens.Lens' UpdateTarget (Core.Maybe Core.Text)
utMaintenanceTrackName = Lens.field @"maintenanceTrackName"
{-# INLINEABLE utMaintenanceTrackName #-}
{-# DEPRECATED maintenanceTrackName "Use generic-lens or generic-optics with 'maintenanceTrackName' instead"  #-}

-- | A list of operations supported by the maintenance track.
--
-- /Note:/ Consider using 'supportedOperations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utSupportedOperations :: Lens.Lens' UpdateTarget (Core.Maybe [Types.SupportedOperation])
utSupportedOperations = Lens.field @"supportedOperations"
{-# INLINEABLE utSupportedOperations #-}
{-# DEPRECATED supportedOperations "Use generic-lens or generic-optics with 'supportedOperations' instead"  #-}

instance Core.FromXML UpdateTarget where
        parseXML x
          = UpdateTarget' Core.<$>
              (x Core..@? "DatabaseVersion") Core.<*>
                x Core..@? "MaintenanceTrackName"
                Core.<*>
                x Core..@? "SupportedOperations" Core..<@>
                  Core.parseXMLList "SupportedOperation"
