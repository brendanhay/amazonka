{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CpuOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CpuOptions
  ( CpuOptions (..)
  -- * Smart constructor
  , mkCpuOptions
  -- * Lenses
  , coCoreCount
  , coThreadsPerCore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The CPU options for the instance.
--
-- /See:/ 'mkCpuOptions' smart constructor.
data CpuOptions = CpuOptions'
  { coreCount :: Core.Maybe Core.Int
    -- ^ The number of CPU cores for the instance.
  , threadsPerCore :: Core.Maybe Core.Int
    -- ^ The number of threads per CPU core.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CpuOptions' value with any optional fields omitted.
mkCpuOptions
    :: CpuOptions
mkCpuOptions
  = CpuOptions'{coreCount = Core.Nothing,
                threadsPerCore = Core.Nothing}

-- | The number of CPU cores for the instance.
--
-- /Note:/ Consider using 'coreCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCoreCount :: Lens.Lens' CpuOptions (Core.Maybe Core.Int)
coCoreCount = Lens.field @"coreCount"
{-# INLINEABLE coCoreCount #-}
{-# DEPRECATED coreCount "Use generic-lens or generic-optics with 'coreCount' instead"  #-}

-- | The number of threads per CPU core.
--
-- /Note:/ Consider using 'threadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coThreadsPerCore :: Lens.Lens' CpuOptions (Core.Maybe Core.Int)
coThreadsPerCore = Lens.field @"threadsPerCore"
{-# INLINEABLE coThreadsPerCore #-}
{-# DEPRECATED threadsPerCore "Use generic-lens or generic-optics with 'threadsPerCore' instead"  #-}

instance Core.FromXML CpuOptions where
        parseXML x
          = CpuOptions' Core.<$>
              (x Core..@? "coreCount") Core.<*> x Core..@? "threadsPerCore"
