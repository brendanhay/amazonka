{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.InstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.InstallationMedia
  ( InstallationMedia (..)
  -- * Smart constructor
  , mkInstallationMedia
  -- * Lenses
  , imCustomAvailabilityZoneId
  , imEngine
  , imEngineInstallationMediaPath
  , imEngineVersion
  , imFailureCause
  , imInstallationMediaId
  , imOSInstallationMediaPath
  , imStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.InstallationMediaFailureCause as Types

-- | Contains the installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'mkInstallationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { customAvailabilityZoneId :: Core.Maybe Core.Text
    -- ^ The custom Availability Zone (AZ) that contains the installation media.
  , engine :: Core.Maybe Core.Text
    -- ^ The DB engine.
  , engineInstallationMediaPath :: Core.Maybe Core.Text
    -- ^ The path to the installation medium for the DB engine.
  , engineVersion :: Core.Maybe Core.Text
    -- ^ The engine version of the DB engine.
  , failureCause :: Core.Maybe Types.InstallationMediaFailureCause
    -- ^ If an installation media failure occurred, the cause of the failure.
  , installationMediaId :: Core.Maybe Core.Text
    -- ^ The installation medium ID.
  , oSInstallationMediaPath :: Core.Maybe Core.Text
    -- ^ The path to the installation medium for the operating system associated with the DB engine.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the installation medium.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstallationMedia' value with any optional fields omitted.
mkInstallationMedia
    :: InstallationMedia
mkInstallationMedia
  = InstallationMedia'{customAvailabilityZoneId = Core.Nothing,
                       engine = Core.Nothing, engineInstallationMediaPath = Core.Nothing,
                       engineVersion = Core.Nothing, failureCause = Core.Nothing,
                       installationMediaId = Core.Nothing,
                       oSInstallationMediaPath = Core.Nothing, status = Core.Nothing}

-- | The custom Availability Zone (AZ) that contains the installation media.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imCustomAvailabilityZoneId :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# INLINEABLE imCustomAvailabilityZoneId #-}
{-# DEPRECATED customAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead"  #-}

-- | The DB engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngine :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imEngine = Lens.field @"engine"
{-# INLINEABLE imEngine #-}
{-# DEPRECATED engine "Use generic-lens or generic-optics with 'engine' instead"  #-}

-- | The path to the installation medium for the DB engine.
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineInstallationMediaPath :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imEngineInstallationMediaPath = Lens.field @"engineInstallationMediaPath"
{-# INLINEABLE imEngineInstallationMediaPath #-}
{-# DEPRECATED engineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead"  #-}

-- | The engine version of the DB engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineVersion :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imEngineVersion = Lens.field @"engineVersion"
{-# INLINEABLE imEngineVersion #-}
{-# DEPRECATED engineVersion "Use generic-lens or generic-optics with 'engineVersion' instead"  #-}

-- | If an installation media failure occurred, the cause of the failure.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imFailureCause :: Lens.Lens' InstallationMedia (Core.Maybe Types.InstallationMediaFailureCause)
imFailureCause = Lens.field @"failureCause"
{-# INLINEABLE imFailureCause #-}
{-# DEPRECATED failureCause "Use generic-lens or generic-optics with 'failureCause' instead"  #-}

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imInstallationMediaId :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imInstallationMediaId = Lens.field @"installationMediaId"
{-# INLINEABLE imInstallationMediaId #-}
{-# DEPRECATED installationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead"  #-}

-- | The path to the installation medium for the operating system associated with the DB engine.
--
-- /Note:/ Consider using 'oSInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imOSInstallationMediaPath :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imOSInstallationMediaPath = Lens.field @"oSInstallationMediaPath"
{-# INLINEABLE imOSInstallationMediaPath #-}
{-# DEPRECATED oSInstallationMediaPath "Use generic-lens or generic-optics with 'oSInstallationMediaPath' instead"  #-}

-- | The status of the installation medium.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imStatus :: Lens.Lens' InstallationMedia (Core.Maybe Core.Text)
imStatus = Lens.field @"status"
{-# INLINEABLE imStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML InstallationMedia where
        parseXML x
          = InstallationMedia' Core.<$>
              (x Core..@? "CustomAvailabilityZoneId") Core.<*>
                x Core..@? "Engine"
                Core.<*> x Core..@? "EngineInstallationMediaPath"
                Core.<*> x Core..@? "EngineVersion"
                Core.<*> x Core..@? "FailureCause"
                Core.<*> x Core..@? "InstallationMediaId"
                Core.<*> x Core..@? "OSInstallationMediaPath"
                Core.<*> x Core..@? "Status"
