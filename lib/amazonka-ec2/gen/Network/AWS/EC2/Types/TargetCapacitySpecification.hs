{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetCapacitySpecification
  ( TargetCapacitySpecification (..)
  -- * Smart constructor
  , mkTargetCapacitySpecification
  -- * Lenses
  , tcsDefaultTargetCapacityType
  , tcsOnDemandTargetCapacity
  , tcsSpotTargetCapacity
  , tcsTotalTargetCapacity
  ) where

import qualified Network.AWS.EC2.Types.DefaultTargetCapacityType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot Instance @MaxTotalPrice@ , or both to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, EC2 Fleet will launch instances until it reaches the maximum amount that you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity. The @MaxTotalPrice@ parameters are located in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptions.html OnDemandOptions> and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptions SpotOptions> 
--
-- /See:/ 'mkTargetCapacitySpecification' smart constructor.
data TargetCapacitySpecification = TargetCapacitySpecification'
  { defaultTargetCapacityType :: Core.Maybe Types.DefaultTargetCapacityType
    -- ^ The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
  , onDemandTargetCapacity :: Core.Maybe Core.Int
    -- ^ The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
  , spotTargetCapacity :: Core.Maybe Core.Int
    -- ^ The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
  , totalTargetCapacity :: Core.Maybe Core.Int
    -- ^ The number of units to request, filled using @DefaultTargetCapacityType@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetCapacitySpecification' value with any optional fields omitted.
mkTargetCapacitySpecification
    :: TargetCapacitySpecification
mkTargetCapacitySpecification
  = TargetCapacitySpecification'{defaultTargetCapacityType =
                                   Core.Nothing,
                                 onDemandTargetCapacity = Core.Nothing,
                                 spotTargetCapacity = Core.Nothing,
                                 totalTargetCapacity = Core.Nothing}

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- /Note:/ Consider using 'defaultTargetCapacityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsDefaultTargetCapacityType :: Lens.Lens' TargetCapacitySpecification (Core.Maybe Types.DefaultTargetCapacityType)
tcsDefaultTargetCapacityType = Lens.field @"defaultTargetCapacityType"
{-# INLINEABLE tcsDefaultTargetCapacityType #-}
{-# DEPRECATED defaultTargetCapacityType "Use generic-lens or generic-optics with 'defaultTargetCapacityType' instead"  #-}

-- | The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsOnDemandTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Core.Maybe Core.Int)
tcsOnDemandTargetCapacity = Lens.field @"onDemandTargetCapacity"
{-# INLINEABLE tcsOnDemandTargetCapacity #-}
{-# DEPRECATED onDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead"  #-}

-- | The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
--
-- /Note:/ Consider using 'spotTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSpotTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Core.Maybe Core.Int)
tcsSpotTargetCapacity = Lens.field @"spotTargetCapacity"
{-# INLINEABLE tcsSpotTargetCapacity #-}
{-# DEPRECATED spotTargetCapacity "Use generic-lens or generic-optics with 'spotTargetCapacity' instead"  #-}

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
--
-- /Note:/ Consider using 'totalTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTotalTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Core.Maybe Core.Int)
tcsTotalTargetCapacity = Lens.field @"totalTargetCapacity"
{-# INLINEABLE tcsTotalTargetCapacity #-}
{-# DEPRECATED totalTargetCapacity "Use generic-lens or generic-optics with 'totalTargetCapacity' instead"  #-}

instance Core.FromXML TargetCapacitySpecification where
        parseXML x
          = TargetCapacitySpecification' Core.<$>
              (x Core..@? "defaultTargetCapacityType") Core.<*>
                x Core..@? "onDemandTargetCapacity"
                Core.<*> x Core..@? "spotTargetCapacity"
                Core.<*> x Core..@? "totalTargetCapacity"
