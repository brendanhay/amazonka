{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateCpuOptionsRequest
  ( LaunchTemplateCpuOptionsRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateCpuOptionsRequest
  -- * Lenses
  , ltcorCoreCount
  , ltcorThreadsPerCore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The CPU options for the instance. Both the core count and threads per core must be specified in the request.
--
-- /See:/ 'mkLaunchTemplateCpuOptionsRequest' smart constructor.
data LaunchTemplateCpuOptionsRequest = LaunchTemplateCpuOptionsRequest'
  { coreCount :: Core.Maybe Core.Int
    -- ^ The number of CPU cores for the instance.
  , threadsPerCore :: Core.Maybe Core.Int
    -- ^ The number of threads per CPU core. To disable multithreading for the instance, specify a value of 1. Otherwise, specify the default value of 2.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateCpuOptionsRequest' value with any optional fields omitted.
mkLaunchTemplateCpuOptionsRequest
    :: LaunchTemplateCpuOptionsRequest
mkLaunchTemplateCpuOptionsRequest
  = LaunchTemplateCpuOptionsRequest'{coreCount = Core.Nothing,
                                     threadsPerCore = Core.Nothing}

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcorCoreCount :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Core.Maybe Core.Int)
ltcorCoreCount = Lens.field @"coreCount"
{-# INLINEABLE ltcorCoreCount #-}
{-# DEPRECATED coreCount "Use generic-lens or generic-optics with 'coreCount' instead"  #-}

-- | The number of threads per CPU core. To disable multithreading for the instance, specify a value of 1. Otherwise, specify the default value of 2.
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcorThreadsPerCore :: Lens.Lens' LaunchTemplateCpuOptionsRequest (Core.Maybe Core.Int)
ltcorThreadsPerCore = Lens.field @"threadsPerCore"
{-# INLINEABLE ltcorThreadsPerCore #-}
{-# DEPRECATED threadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead"  #-}

instance Core.ToQuery LaunchTemplateCpuOptionsRequest where
        toQuery LaunchTemplateCpuOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CoreCount") coreCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ThreadsPerCore")
                threadsPerCore
