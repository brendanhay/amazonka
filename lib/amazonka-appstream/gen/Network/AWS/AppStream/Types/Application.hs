{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Application
  ( Application (..),

    -- * Smart constructor
    mkApplication,

    -- * Lenses
    aDisplayName,
    aEnabled,
    aIconURL,
    aLaunchParameters,
    aLaunchPath,
    aMetadata,
    aName,
  )
where

import qualified Network.AWS.AppStream.Types.IconURL as Types
import qualified Network.AWS.AppStream.Types.LaunchParameters as Types
import qualified Network.AWS.AppStream.Types.LaunchPath as Types
import qualified Network.AWS.AppStream.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an application in the application catalog.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { -- | The application name to display.
    displayName :: Core.Maybe Types.String,
    -- | If there is a problem, the application can be disabled after image creation.
    enabled :: Core.Maybe Core.Bool,
    -- | The URL for the application icon. This URL might be time-limited.
    iconURL :: Core.Maybe Types.IconURL,
    -- | The arguments that are passed to the application at launch.
    launchParameters :: Core.Maybe Types.LaunchParameters,
    -- | The path to the application executable in the instance.
    launchPath :: Core.Maybe Types.LaunchPath,
    -- | Additional attributes that describe the application.
    metadata :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The name of the application.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Application' value with any optional fields omitted.
mkApplication ::
  Application
mkApplication =
  Application'
    { displayName = Core.Nothing,
      enabled = Core.Nothing,
      iconURL = Core.Nothing,
      launchParameters = Core.Nothing,
      launchPath = Core.Nothing,
      metadata = Core.Nothing,
      name = Core.Nothing
    }

-- | The application name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDisplayName :: Lens.Lens' Application (Core.Maybe Types.String)
aDisplayName = Lens.field @"displayName"
{-# DEPRECATED aDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | If there is a problem, the application can be disabled after image creation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnabled :: Lens.Lens' Application (Core.Maybe Core.Bool)
aEnabled = Lens.field @"enabled"
{-# DEPRECATED aEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The URL for the application icon. This URL might be time-limited.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIconURL :: Lens.Lens' Application (Core.Maybe Types.IconURL)
aIconURL = Lens.field @"iconURL"
{-# DEPRECATED aIconURL "Use generic-lens or generic-optics with 'iconURL' instead." #-}

-- | The arguments that are passed to the application at launch.
--
-- /Note:/ Consider using 'launchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLaunchParameters :: Lens.Lens' Application (Core.Maybe Types.LaunchParameters)
aLaunchParameters = Lens.field @"launchParameters"
{-# DEPRECATED aLaunchParameters "Use generic-lens or generic-optics with 'launchParameters' instead." #-}

-- | The path to the application executable in the instance.
--
-- /Note:/ Consider using 'launchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLaunchPath :: Lens.Lens' Application (Core.Maybe Types.LaunchPath)
aLaunchPath = Lens.field @"launchPath"
{-# DEPRECATED aLaunchPath "Use generic-lens or generic-optics with 'launchPath' instead." #-}

-- | Additional attributes that describe the application.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetadata :: Lens.Lens' Application (Core.Maybe (Core.HashMap Types.String Types.String))
aMetadata = Lens.field @"metadata"
{-# DEPRECATED aMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Application (Core.Maybe Types.String)
aName = Lens.field @"name"
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON Application where
  parseJSON =
    Core.withObject "Application" Core.$
      \x ->
        Application'
          Core.<$> (x Core..:? "DisplayName")
          Core.<*> (x Core..:? "Enabled")
          Core.<*> (x Core..:? "IconURL")
          Core.<*> (x Core..:? "LaunchParameters")
          Core.<*> (x Core..:? "LaunchPath")
          Core.<*> (x Core..:? "Metadata")
          Core.<*> (x Core..:? "Name")
