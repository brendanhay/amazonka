{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.InstallationMedia
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.InstallationMedia
  ( InstallationMedia (..),

    -- * Smart constructor
    mkInstallationMedia,

    -- * Lenses
    imCustomAvailabilityZoneId,
    imEngine,
    imEngineInstallationMediaPath,
    imEngineVersion,
    imFailureCause,
    imInstallationMediaId,
    imOSInstallationMediaPath,
    imStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.InstallationMediaFailureCause as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | Contains the installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'mkInstallationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { -- | The custom Availability Zone (AZ) that contains the installation media.
    customAvailabilityZoneId :: Core.Maybe Types.String,
    -- | The DB engine.
    engine :: Core.Maybe Types.String,
    -- | The path to the installation medium for the DB engine.
    engineInstallationMediaPath :: Core.Maybe Types.String,
    -- | The engine version of the DB engine.
    engineVersion :: Core.Maybe Types.String,
    -- | If an installation media failure occurred, the cause of the failure.
    failureCause :: Core.Maybe Types.InstallationMediaFailureCause,
    -- | The installation medium ID.
    installationMediaId :: Core.Maybe Types.String,
    -- | The path to the installation medium for the operating system associated with the DB engine.
    oSInstallationMediaPath :: Core.Maybe Types.String,
    -- | The status of the installation medium.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstallationMedia' value with any optional fields omitted.
mkInstallationMedia ::
  InstallationMedia
mkInstallationMedia =
  InstallationMedia'
    { customAvailabilityZoneId = Core.Nothing,
      engine = Core.Nothing,
      engineInstallationMediaPath = Core.Nothing,
      engineVersion = Core.Nothing,
      failureCause = Core.Nothing,
      installationMediaId = Core.Nothing,
      oSInstallationMediaPath = Core.Nothing,
      status = Core.Nothing
    }

-- | The custom Availability Zone (AZ) that contains the installation media.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imCustomAvailabilityZoneId :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imCustomAvailabilityZoneId = Lens.field @"customAvailabilityZoneId"
{-# DEPRECATED imCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

-- | The DB engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngine :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imEngine = Lens.field @"engine"
{-# DEPRECATED imEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The path to the installation medium for the DB engine.
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineInstallationMediaPath :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imEngineInstallationMediaPath = Lens.field @"engineInstallationMediaPath"
{-# DEPRECATED imEngineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead." #-}

-- | The engine version of the DB engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineVersion :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED imEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | If an installation media failure occurred, the cause of the failure.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imFailureCause :: Lens.Lens' InstallationMedia (Core.Maybe Types.InstallationMediaFailureCause)
imFailureCause = Lens.field @"failureCause"
{-# DEPRECATED imFailureCause "Use generic-lens or generic-optics with 'failureCause' instead." #-}

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imInstallationMediaId :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imInstallationMediaId = Lens.field @"installationMediaId"
{-# DEPRECATED imInstallationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead." #-}

-- | The path to the installation medium for the operating system associated with the DB engine.
--
-- /Note:/ Consider using 'oSInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imOSInstallationMediaPath :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imOSInstallationMediaPath = Lens.field @"oSInstallationMediaPath"
{-# DEPRECATED imOSInstallationMediaPath "Use generic-lens or generic-optics with 'oSInstallationMediaPath' instead." #-}

-- | The status of the installation medium.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imStatus :: Lens.Lens' InstallationMedia (Core.Maybe Types.String)
imStatus = Lens.field @"status"
{-# DEPRECATED imStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML InstallationMedia where
  parseXML x =
    InstallationMedia'
      Core.<$> (x Core..@? "CustomAvailabilityZoneId")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineInstallationMediaPath")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "FailureCause")
      Core.<*> (x Core..@? "InstallationMediaId")
      Core.<*> (x Core..@? "OSInstallationMediaPath")
      Core.<*> (x Core..@? "Status")
