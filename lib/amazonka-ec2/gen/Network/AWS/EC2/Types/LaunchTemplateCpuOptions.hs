{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCpuOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateCpuOptions
  ( LaunchTemplateCpuOptions (..)
  -- * Smart constructor
  , mkLaunchTemplateCpuOptions
  -- * Lenses
  , ltcoCoreCount
  , ltcoThreadsPerCore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The CPU options for the instance.
--
-- /See:/ 'mkLaunchTemplateCpuOptions' smart constructor.
data LaunchTemplateCpuOptions = LaunchTemplateCpuOptions'
  { coreCount :: Core.Maybe Core.Int
    -- ^ The number of CPU cores for the instance.
  , threadsPerCore :: Core.Maybe Core.Int
    -- ^ The number of threads per CPU core.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateCpuOptions' value with any optional fields omitted.
mkLaunchTemplateCpuOptions
    :: LaunchTemplateCpuOptions
mkLaunchTemplateCpuOptions
  = LaunchTemplateCpuOptions'{coreCount = Core.Nothing,
                              threadsPerCore = Core.Nothing}

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcoCoreCount :: Lens.Lens' LaunchTemplateCpuOptions (Core.Maybe Core.Int)
ltcoCoreCount = Lens.field @"coreCount"
{-# INLINEABLE ltcoCoreCount #-}
{-# DEPRECATED coreCount "Use generic-lens or generic-optics with 'coreCount' instead"  #-}

-- | The number of threads per CPU core.
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcoThreadsPerCore :: Lens.Lens' LaunchTemplateCpuOptions (Core.Maybe Core.Int)
ltcoThreadsPerCore = Lens.field @"threadsPerCore"
{-# INLINEABLE ltcoThreadsPerCore #-}
{-# DEPRECATED threadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead"  #-}

instance Core.FromXML LaunchTemplateCpuOptions where
        parseXML x
          = LaunchTemplateCpuOptions' Core.<$>
              (x Core..@? "coreCount") Core.<*> x Core..@? "threadsPerCore"
