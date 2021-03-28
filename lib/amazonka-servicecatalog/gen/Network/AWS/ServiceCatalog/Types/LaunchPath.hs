{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.LaunchPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServiceCatalog.Types.LaunchPath
  ( LaunchPath (..)
  -- * Smart constructor
  , mkLaunchPath
  -- * Lenses
  , lpId
  , lpName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServiceCatalog.Types.Id as Types
import qualified Network.AWS.ServiceCatalog.Types.PortfolioName as Types

-- | A launch path object.
--
-- /See:/ 'mkLaunchPath' smart constructor.
data LaunchPath = LaunchPath'
  { id :: Core.Maybe Types.Id
    -- ^ The identifier of the launch path.
  , name :: Core.Maybe Types.PortfolioName
    -- ^ The name of the launch path.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchPath' value with any optional fields omitted.
mkLaunchPath
    :: LaunchPath
mkLaunchPath = LaunchPath'{id = Core.Nothing, name = Core.Nothing}

-- | The identifier of the launch path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpId :: Lens.Lens' LaunchPath (Core.Maybe Types.Id)
lpId = Lens.field @"id"
{-# INLINEABLE lpId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the launch path.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpName :: Lens.Lens' LaunchPath (Core.Maybe Types.PortfolioName)
lpName = Lens.field @"name"
{-# INLINEABLE lpName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON LaunchPath where
        parseJSON
          = Core.withObject "LaunchPath" Core.$
              \ x ->
                LaunchPath' Core.<$> (x Core..:? "Id") Core.<*> x Core..:? "Name"
