{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.LaunchConfiguration
  ( LaunchConfiguration (..)
  -- * Smart constructor
  , mkLaunchConfiguration
  -- * Lenses
  , lcName
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ResourceId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Auto Scaling launch configuration.
--
-- /See:/ 'mkLaunchConfiguration' smart constructor.
newtype LaunchConfiguration = LaunchConfiguration'
  { name :: Core.Maybe Types.ResourceId
    -- ^ The name of the launch configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchConfiguration' value with any optional fields omitted.
mkLaunchConfiguration
    :: LaunchConfiguration
mkLaunchConfiguration = LaunchConfiguration'{name = Core.Nothing}

-- | The name of the launch configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcName :: Lens.Lens' LaunchConfiguration (Core.Maybe Types.ResourceId)
lcName = Lens.field @"name"
{-# INLINEABLE lcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromXML LaunchConfiguration where
        parseXML x = LaunchConfiguration' Core.<$> (x Core..@? "Name")
