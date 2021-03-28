{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Application
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppStream.Types.Application
  ( Application (..)
  -- * Smart constructor
  , mkApplication
  -- * Lenses
  , aDisplayName
  , aEnabled
  , aIconURL
  , aLaunchParameters
  , aLaunchPath
  , aMetadata
  , aName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an application in the application catalog.
--
-- /See:/ 'mkApplication' smart constructor.
data Application = Application'
  { displayName :: Core.Maybe Core.Text
    -- ^ The application name to display.
  , enabled :: Core.Maybe Core.Bool
    -- ^ If there is a problem, the application can be disabled after image creation.
  , iconURL :: Core.Maybe Core.Text
    -- ^ The URL for the application icon. This URL might be time-limited.
  , launchParameters :: Core.Maybe Core.Text
    -- ^ The arguments that are passed to the application at launch.
  , launchPath :: Core.Maybe Core.Text
    -- ^ The path to the application executable in the instance.
  , metadata :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ Additional attributes that describe the application.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the application.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Application' value with any optional fields omitted.
mkApplication
    :: Application
mkApplication
  = Application'{displayName = Core.Nothing, enabled = Core.Nothing,
                 iconURL = Core.Nothing, launchParameters = Core.Nothing,
                 launchPath = Core.Nothing, metadata = Core.Nothing,
                 name = Core.Nothing}

-- | The application name to display.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDisplayName :: Lens.Lens' Application (Core.Maybe Core.Text)
aDisplayName = Lens.field @"displayName"
{-# INLINEABLE aDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | If there is a problem, the application can be disabled after image creation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEnabled :: Lens.Lens' Application (Core.Maybe Core.Bool)
aEnabled = Lens.field @"enabled"
{-# INLINEABLE aEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | The URL for the application icon. This URL might be time-limited.
--
-- /Note:/ Consider using 'iconURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIconURL :: Lens.Lens' Application (Core.Maybe Core.Text)
aIconURL = Lens.field @"iconURL"
{-# INLINEABLE aIconURL #-}
{-# DEPRECATED iconURL "Use generic-lens or generic-optics with 'iconURL' instead"  #-}

-- | The arguments that are passed to the application at launch.
--
-- /Note:/ Consider using 'launchParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLaunchParameters :: Lens.Lens' Application (Core.Maybe Core.Text)
aLaunchParameters = Lens.field @"launchParameters"
{-# INLINEABLE aLaunchParameters #-}
{-# DEPRECATED launchParameters "Use generic-lens or generic-optics with 'launchParameters' instead"  #-}

-- | The path to the application executable in the instance.
--
-- /Note:/ Consider using 'launchPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aLaunchPath :: Lens.Lens' Application (Core.Maybe Core.Text)
aLaunchPath = Lens.field @"launchPath"
{-# INLINEABLE aLaunchPath #-}
{-# DEPRECATED launchPath "Use generic-lens or generic-optics with 'launchPath' instead"  #-}

-- | Additional attributes that describe the application.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMetadata :: Lens.Lens' Application (Core.Maybe (Core.HashMap Core.Text Core.Text))
aMetadata = Lens.field @"metadata"
{-# INLINEABLE aMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The name of the application.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Application (Core.Maybe Core.Text)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Application where
        parseJSON
          = Core.withObject "Application" Core.$
              \ x ->
                Application' Core.<$>
                  (x Core..:? "DisplayName") Core.<*> x Core..:? "Enabled" Core.<*>
                    x Core..:? "IconURL"
                    Core.<*> x Core..:? "LaunchParameters"
                    Core.<*> x Core..:? "LaunchPath"
                    Core.<*> x Core..:? "Metadata"
                    Core.<*> x Core..:? "Name"
