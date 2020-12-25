{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceUtilization
  ( EC2ResourceUtilization (..),

    -- * Smart constructor
    mkEC2ResourceUtilization,

    -- * Lenses
    ecruEBSResourceUtilization,
    ecruMaxCpuUtilizationPercentage,
    ecruMaxMemoryUtilizationPercentage,
    ecruMaxStorageUtilizationPercentage,
  )
where

import qualified Network.AWS.CostExplorer.Types.EBSResourceUtilization as Types
import qualified Network.AWS.CostExplorer.Types.MaxCpuUtilizationPercentage as Types
import qualified Network.AWS.CostExplorer.Types.MaxMemoryUtilizationPercentage as Types
import qualified Network.AWS.CostExplorer.Types.MaxStorageUtilizationPercentage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Utilization metrics of the instance.
--
-- /See:/ 'mkEC2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { -- | The EBS field that contains a list of EBS metrics associated with the current instance.
    eBSResourceUtilization :: Core.Maybe Types.EBSResourceUtilization,
    -- | Maximum observed or expected CPU utilization of the instance.
    maxCpuUtilizationPercentage :: Core.Maybe Types.MaxCpuUtilizationPercentage,
    -- | Maximum observed or expected memory utilization of the instance.
    maxMemoryUtilizationPercentage :: Core.Maybe Types.MaxMemoryUtilizationPercentage,
    -- | Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
    maxStorageUtilizationPercentage :: Core.Maybe Types.MaxStorageUtilizationPercentage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EC2ResourceUtilization' value with any optional fields omitted.
mkEC2ResourceUtilization ::
  EC2ResourceUtilization
mkEC2ResourceUtilization =
  EC2ResourceUtilization'
    { eBSResourceUtilization = Core.Nothing,
      maxCpuUtilizationPercentage = Core.Nothing,
      maxMemoryUtilizationPercentage = Core.Nothing,
      maxStorageUtilizationPercentage = Core.Nothing
    }

-- | The EBS field that contains a list of EBS metrics associated with the current instance.
--
-- /Note:/ Consider using 'eBSResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecruEBSResourceUtilization :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Types.EBSResourceUtilization)
ecruEBSResourceUtilization = Lens.field @"eBSResourceUtilization"
{-# DEPRECATED ecruEBSResourceUtilization "Use generic-lens or generic-optics with 'eBSResourceUtilization' instead." #-}

-- | Maximum observed or expected CPU utilization of the instance.
--
-- /Note:/ Consider using 'maxCpuUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecruMaxCpuUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Types.MaxCpuUtilizationPercentage)
ecruMaxCpuUtilizationPercentage = Lens.field @"maxCpuUtilizationPercentage"
{-# DEPRECATED ecruMaxCpuUtilizationPercentage "Use generic-lens or generic-optics with 'maxCpuUtilizationPercentage' instead." #-}

-- | Maximum observed or expected memory utilization of the instance.
--
-- /Note:/ Consider using 'maxMemoryUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecruMaxMemoryUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Types.MaxMemoryUtilizationPercentage)
ecruMaxMemoryUtilizationPercentage = Lens.field @"maxMemoryUtilizationPercentage"
{-# DEPRECATED ecruMaxMemoryUtilizationPercentage "Use generic-lens or generic-optics with 'maxMemoryUtilizationPercentage' instead." #-}

-- | Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
--
-- /Note:/ Consider using 'maxStorageUtilizationPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecruMaxStorageUtilizationPercentage :: Lens.Lens' EC2ResourceUtilization (Core.Maybe Types.MaxStorageUtilizationPercentage)
ecruMaxStorageUtilizationPercentage = Lens.field @"maxStorageUtilizationPercentage"
{-# DEPRECATED ecruMaxStorageUtilizationPercentage "Use generic-lens or generic-optics with 'maxStorageUtilizationPercentage' instead." #-}

instance Core.FromJSON EC2ResourceUtilization where
  parseJSON =
    Core.withObject "EC2ResourceUtilization" Core.$
      \x ->
        EC2ResourceUtilization'
          Core.<$> (x Core..:? "EBSResourceUtilization")
          Core.<*> (x Core..:? "MaxCpuUtilizationPercentage")
          Core.<*> (x Core..:? "MaxMemoryUtilizationPercentage")
          Core.<*> (x Core..:? "MaxStorageUtilizationPercentage")
