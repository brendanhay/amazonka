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
    imEngineVersion,
    imStatus,
    imInstallationMediaId,
    imEngineInstallationMediaPath,
    imEngine,
    imOSInstallationMediaPath,
    imCustomAvailabilityZoneId,
    imFailureCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.InstallationMediaFailureCause

-- | Contains the installation media for a DB engine that requires an on-premises customer provided license, such as Microsoft SQL Server.
--
-- /See:/ 'mkInstallationMedia' smart constructor.
data InstallationMedia = InstallationMedia'
  { engineVersion ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    installationMediaId :: Lude.Maybe Lude.Text,
    engineInstallationMediaPath :: Lude.Maybe Lude.Text,
    engine :: Lude.Maybe Lude.Text,
    osInstallationMediaPath :: Lude.Maybe Lude.Text,
    customAvailabilityZoneId :: Lude.Maybe Lude.Text,
    failureCause ::
      Lude.Maybe InstallationMediaFailureCause
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstallationMedia' with the minimum fields required to make a request.
--
-- * 'customAvailabilityZoneId' - The custom Availability Zone (AZ) that contains the installation media.
-- * 'engine' - The DB engine.
-- * 'engineInstallationMediaPath' - The path to the installation medium for the DB engine.
-- * 'engineVersion' - The engine version of the DB engine.
-- * 'failureCause' - If an installation media failure occurred, the cause of the failure.
-- * 'installationMediaId' - The installation medium ID.
-- * 'osInstallationMediaPath' - The path to the installation medium for the operating system associated with the DB engine.
-- * 'status' - The status of the installation medium.
mkInstallationMedia ::
  InstallationMedia
mkInstallationMedia =
  InstallationMedia'
    { engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      installationMediaId = Lude.Nothing,
      engineInstallationMediaPath = Lude.Nothing,
      engine = Lude.Nothing,
      osInstallationMediaPath = Lude.Nothing,
      customAvailabilityZoneId = Lude.Nothing,
      failureCause = Lude.Nothing
    }

-- | The engine version of the DB engine.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineVersion :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imEngineVersion = Lens.lens (engineVersion :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: InstallationMedia)
{-# DEPRECATED imEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The status of the installation medium.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imStatus :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imStatus = Lens.lens (status :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: InstallationMedia)
{-# DEPRECATED imStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The installation medium ID.
--
-- /Note:/ Consider using 'installationMediaId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imInstallationMediaId :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imInstallationMediaId = Lens.lens (installationMediaId :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {installationMediaId = a} :: InstallationMedia)
{-# DEPRECATED imInstallationMediaId "Use generic-lens or generic-optics with 'installationMediaId' instead." #-}

-- | The path to the installation medium for the DB engine.
--
-- /Note:/ Consider using 'engineInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngineInstallationMediaPath :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imEngineInstallationMediaPath = Lens.lens (engineInstallationMediaPath :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {engineInstallationMediaPath = a} :: InstallationMedia)
{-# DEPRECATED imEngineInstallationMediaPath "Use generic-lens or generic-optics with 'engineInstallationMediaPath' instead." #-}

-- | The DB engine.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imEngine :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imEngine = Lens.lens (engine :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: InstallationMedia)
{-# DEPRECATED imEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The path to the installation medium for the operating system associated with the DB engine.
--
-- /Note:/ Consider using 'osInstallationMediaPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imOSInstallationMediaPath :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imOSInstallationMediaPath = Lens.lens (osInstallationMediaPath :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {osInstallationMediaPath = a} :: InstallationMedia)
{-# DEPRECATED imOSInstallationMediaPath "Use generic-lens or generic-optics with 'osInstallationMediaPath' instead." #-}

-- | The custom Availability Zone (AZ) that contains the installation media.
--
-- /Note:/ Consider using 'customAvailabilityZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imCustomAvailabilityZoneId :: Lens.Lens' InstallationMedia (Lude.Maybe Lude.Text)
imCustomAvailabilityZoneId = Lens.lens (customAvailabilityZoneId :: InstallationMedia -> Lude.Maybe Lude.Text) (\s a -> s {customAvailabilityZoneId = a} :: InstallationMedia)
{-# DEPRECATED imCustomAvailabilityZoneId "Use generic-lens or generic-optics with 'customAvailabilityZoneId' instead." #-}

-- | If an installation media failure occurred, the cause of the failure.
--
-- /Note:/ Consider using 'failureCause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
imFailureCause :: Lens.Lens' InstallationMedia (Lude.Maybe InstallationMediaFailureCause)
imFailureCause = Lens.lens (failureCause :: InstallationMedia -> Lude.Maybe InstallationMediaFailureCause) (\s a -> s {failureCause = a} :: InstallationMedia)
{-# DEPRECATED imFailureCause "Use generic-lens or generic-optics with 'failureCause' instead." #-}

instance Lude.FromXML InstallationMedia where
  parseXML x =
    InstallationMedia'
      Lude.<$> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "InstallationMediaId")
      Lude.<*> (x Lude..@? "EngineInstallationMediaPath")
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "OSInstallationMediaPath")
      Lude.<*> (x Lude..@? "CustomAvailabilityZoneId")
      Lude.<*> (x Lude..@? "FailureCause")
