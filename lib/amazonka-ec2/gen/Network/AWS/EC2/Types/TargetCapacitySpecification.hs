{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetCapacitySpecification
  ( TargetCapacitySpecification (..),

    -- * Smart constructor
    mkTargetCapacitySpecification,

    -- * Lenses
    tcsOnDemandTargetCapacity,
    tcsDefaultTargetCapacityType,
    tcsTotalTargetCapacity,
    tcsSpotTargetCapacity,
  )
where

import Network.AWS.EC2.Types.DefaultTargetCapacityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of units to request. You can choose to set the target capacity in terms of instances or a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot Instance @MaxTotalPrice@ , or both to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, EC2 Fleet will launch instances until it reaches the maximum amount that you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity. The @MaxTotalPrice@ parameters are located in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptions.html OnDemandOptions> and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptions SpotOptions>
--
-- /See:/ 'mkTargetCapacitySpecification' smart constructor.
data TargetCapacitySpecification = TargetCapacitySpecification'
  { onDemandTargetCapacity ::
      Lude.Maybe Lude.Int,
    defaultTargetCapacityType ::
      Lude.Maybe
        DefaultTargetCapacityType,
    totalTargetCapacity ::
      Lude.Maybe Lude.Int,
    spotTargetCapacity ::
      Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetCapacitySpecification' with the minimum fields required to make a request.
--
-- * 'defaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
-- * 'onDemandTargetCapacity' - The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
-- * 'spotTargetCapacity' - The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
-- * 'totalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
mkTargetCapacitySpecification ::
  TargetCapacitySpecification
mkTargetCapacitySpecification =
  TargetCapacitySpecification'
    { onDemandTargetCapacity =
        Lude.Nothing,
      defaultTargetCapacityType = Lude.Nothing,
      totalTargetCapacity = Lude.Nothing,
      spotTargetCapacity = Lude.Nothing
    }

-- | The number of On-Demand units to request. If you specify a target capacity for Spot units, you cannot specify a target capacity for On-Demand units.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsOnDemandTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Lude.Maybe Lude.Int)
tcsOnDemandTargetCapacity = Lens.lens (onDemandTargetCapacity :: TargetCapacitySpecification -> Lude.Maybe Lude.Int) (\s a -> s {onDemandTargetCapacity = a} :: TargetCapacitySpecification)
{-# DEPRECATED tcsOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- /Note:/ Consider using 'defaultTargetCapacityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsDefaultTargetCapacityType :: Lens.Lens' TargetCapacitySpecification (Lude.Maybe DefaultTargetCapacityType)
tcsDefaultTargetCapacityType = Lens.lens (defaultTargetCapacityType :: TargetCapacitySpecification -> Lude.Maybe DefaultTargetCapacityType) (\s a -> s {defaultTargetCapacityType = a} :: TargetCapacitySpecification)
{-# DEPRECATED tcsDefaultTargetCapacityType "Use generic-lens or generic-optics with 'defaultTargetCapacityType' instead." #-}

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
--
-- /Note:/ Consider using 'totalTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsTotalTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Lude.Maybe Lude.Int)
tcsTotalTargetCapacity = Lens.lens (totalTargetCapacity :: TargetCapacitySpecification -> Lude.Maybe Lude.Int) (\s a -> s {totalTargetCapacity = a} :: TargetCapacitySpecification)
{-# DEPRECATED tcsTotalTargetCapacity "Use generic-lens or generic-optics with 'totalTargetCapacity' instead." #-}

-- | The maximum number of Spot units to launch. If you specify a target capacity for On-Demand units, you cannot specify a target capacity for Spot units.
--
-- /Note:/ Consider using 'spotTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSpotTargetCapacity :: Lens.Lens' TargetCapacitySpecification (Lude.Maybe Lude.Int)
tcsSpotTargetCapacity = Lens.lens (spotTargetCapacity :: TargetCapacitySpecification -> Lude.Maybe Lude.Int) (\s a -> s {spotTargetCapacity = a} :: TargetCapacitySpecification)
{-# DEPRECATED tcsSpotTargetCapacity "Use generic-lens or generic-optics with 'spotTargetCapacity' instead." #-}

instance Lude.FromXML TargetCapacitySpecification where
  parseXML x =
    TargetCapacitySpecification'
      Lude.<$> (x Lude..@? "onDemandTargetCapacity")
      Lude.<*> (x Lude..@? "defaultTargetCapacityType")
      Lude.<*> (x Lude..@? "totalTargetCapacity")
      Lude.<*> (x Lude..@? "spotTargetCapacity")
