{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VCpuInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.VCpuInfo
  ( VCpuInfo (..)
  -- * Smart constructor
  , mkVCpuInfo
  -- * Lenses
  , vciDefaultCores
  , vciDefaultThreadsPerCore
  , vciDefaultVCpus
  , vciValidCores
  , vciValidThreadsPerCore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the vCPU configurations for the instance type.
--
-- /See:/ 'mkVCpuInfo' smart constructor.
data VCpuInfo = VCpuInfo'
  { defaultCores :: Core.Maybe Core.Int
    -- ^ The default number of cores for the instance type.
  , defaultThreadsPerCore :: Core.Maybe Core.Int
    -- ^ The default number of threads per core for the instance type.
  , defaultVCpus :: Core.Maybe Core.Int
    -- ^ The default number of vCPUs for the instance type.
  , validCores :: Core.Maybe [Core.Int]
    -- ^ The valid number of cores that can be configured for the instance type.
  , validThreadsPerCore :: Core.Maybe [Core.Int]
    -- ^ The valid number of threads per core that can be configured for the instance type. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VCpuInfo' value with any optional fields omitted.
mkVCpuInfo
    :: VCpuInfo
mkVCpuInfo
  = VCpuInfo'{defaultCores = Core.Nothing,
              defaultThreadsPerCore = Core.Nothing, defaultVCpus = Core.Nothing,
              validCores = Core.Nothing, validThreadsPerCore = Core.Nothing}

-- | The default number of cores for the instance type.
--
-- /Note:/ Consider using 'defaultCores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultCores :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vciDefaultCores = Lens.field @"defaultCores"
{-# INLINEABLE vciDefaultCores #-}
{-# DEPRECATED defaultCores "Use generic-lens or generic-optics with 'defaultCores' instead"  #-}

-- | The default number of threads per core for the instance type.
--
-- /Note:/ Consider using 'defaultThreadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultThreadsPerCore :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vciDefaultThreadsPerCore = Lens.field @"defaultThreadsPerCore"
{-# INLINEABLE vciDefaultThreadsPerCore #-}
{-# DEPRECATED defaultThreadsPerCore "Use generic-lens or generic-optics with 'defaultThreadsPerCore' instead"  #-}

-- | The default number of vCPUs for the instance type.
--
-- /Note:/ Consider using 'defaultVCpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciDefaultVCpus :: Lens.Lens' VCpuInfo (Core.Maybe Core.Int)
vciDefaultVCpus = Lens.field @"defaultVCpus"
{-# INLINEABLE vciDefaultVCpus #-}
{-# DEPRECATED defaultVCpus "Use generic-lens or generic-optics with 'defaultVCpus' instead"  #-}

-- | The valid number of cores that can be configured for the instance type.
--
-- /Note:/ Consider using 'validCores' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciValidCores :: Lens.Lens' VCpuInfo (Core.Maybe [Core.Int])
vciValidCores = Lens.field @"validCores"
{-# INLINEABLE vciValidCores #-}
{-# DEPRECATED validCores "Use generic-lens or generic-optics with 'validCores' instead"  #-}

-- | The valid number of threads per core that can be configured for the instance type. 
--
-- /Note:/ Consider using 'validThreadsPerCore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vciValidThreadsPerCore :: Lens.Lens' VCpuInfo (Core.Maybe [Core.Int])
vciValidThreadsPerCore = Lens.field @"validThreadsPerCore"
{-# INLINEABLE vciValidThreadsPerCore #-}
{-# DEPRECATED validThreadsPerCore "Use generic-lens or generic-optics with 'validThreadsPerCore' instead"  #-}

instance Core.FromXML VCpuInfo where
        parseXML x
          = VCpuInfo' Core.<$>
              (x Core..@? "defaultCores") Core.<*>
                x Core..@? "defaultThreadsPerCore"
                Core.<*> x Core..@? "defaultVCpus"
                Core.<*> x Core..@? "validCores" Core..<@> Core.parseXMLList "item"
                Core.<*>
                x Core..@? "validThreadsPerCore" Core..<@> Core.parseXMLList "item"
