-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetCapacitySpecificationRequest
  ( TargetCapacitySpecificationRequest (..),

    -- * Smart constructor
    mkTargetCapacitySpecificationRequest,

    -- * Lenses
    tcsrOnDemandTargetCapacity,
    tcsrDefaultTargetCapacityType,
    tcsrSpotTargetCapacity,
    tcsrTotalTargetCapacity,
  )
where

import Network.AWS.EC2.Types.DefaultTargetCapacityType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of units to request. You can choose to set the target capacity as the number of instances. Or you can set the target capacity to a performance characteristic that is important to your application workload, such as vCPUs, memory, or I/O. If the request type is @maintain@ , you can specify a target capacity of 0 and add capacity later.
--
-- You can use the On-Demand Instance @MaxTotalPrice@ parameter, the Spot Instance @MaxTotalPrice@ parameter, or both parameters to ensure that your fleet cost does not exceed your budget. If you set a maximum price per hour for the On-Demand Instances and Spot Instances in your request, EC2 Fleet will launch instances until it reaches the maximum amount that you're willing to pay. When the maximum amount you're willing to pay is reached, the fleet stops launching instances even if it hasn’t met the target capacity. The @MaxTotalPrice@ parameters are located in <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_OnDemandOptionsRequest OnDemandOptionsRequest> and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_SpotOptionsRequest SpotOptionsRequest> .
--
-- /See:/ 'mkTargetCapacitySpecificationRequest' smart constructor.
data TargetCapacitySpecificationRequest = TargetCapacitySpecificationRequest'
  { onDemandTargetCapacity ::
      Lude.Maybe Lude.Int,
    defaultTargetCapacityType ::
      Lude.Maybe
        DefaultTargetCapacityType,
    spotTargetCapacity ::
      Lude.Maybe Lude.Int,
    totalTargetCapacity ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetCapacitySpecificationRequest' with the minimum fields required to make a request.
--
-- * 'defaultTargetCapacityType' - The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
-- * 'onDemandTargetCapacity' - The number of On-Demand units to request.
-- * 'spotTargetCapacity' - The number of Spot units to request.
-- * 'totalTargetCapacity' - The number of units to request, filled using @DefaultTargetCapacityType@ .
mkTargetCapacitySpecificationRequest ::
  -- | 'totalTargetCapacity'
  Lude.Int ->
  TargetCapacitySpecificationRequest
mkTargetCapacitySpecificationRequest pTotalTargetCapacity_ =
  TargetCapacitySpecificationRequest'
    { onDemandTargetCapacity =
        Lude.Nothing,
      defaultTargetCapacityType = Lude.Nothing,
      spotTargetCapacity = Lude.Nothing,
      totalTargetCapacity = pTotalTargetCapacity_
    }

-- | The number of On-Demand units to request.
--
-- /Note:/ Consider using 'onDemandTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsrOnDemandTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest (Lude.Maybe Lude.Int)
tcsrOnDemandTargetCapacity = Lens.lens (onDemandTargetCapacity :: TargetCapacitySpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {onDemandTargetCapacity = a} :: TargetCapacitySpecificationRequest)
{-# DEPRECATED tcsrOnDemandTargetCapacity "Use generic-lens or generic-optics with 'onDemandTargetCapacity' instead." #-}

-- | The default @TotalTargetCapacity@ , which is either @Spot@ or @On-Demand@ .
--
-- /Note:/ Consider using 'defaultTargetCapacityType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsrDefaultTargetCapacityType :: Lens.Lens' TargetCapacitySpecificationRequest (Lude.Maybe DefaultTargetCapacityType)
tcsrDefaultTargetCapacityType = Lens.lens (defaultTargetCapacityType :: TargetCapacitySpecificationRequest -> Lude.Maybe DefaultTargetCapacityType) (\s a -> s {defaultTargetCapacityType = a} :: TargetCapacitySpecificationRequest)
{-# DEPRECATED tcsrDefaultTargetCapacityType "Use generic-lens or generic-optics with 'defaultTargetCapacityType' instead." #-}

-- | The number of Spot units to request.
--
-- /Note:/ Consider using 'spotTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsrSpotTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest (Lude.Maybe Lude.Int)
tcsrSpotTargetCapacity = Lens.lens (spotTargetCapacity :: TargetCapacitySpecificationRequest -> Lude.Maybe Lude.Int) (\s a -> s {spotTargetCapacity = a} :: TargetCapacitySpecificationRequest)
{-# DEPRECATED tcsrSpotTargetCapacity "Use generic-lens or generic-optics with 'spotTargetCapacity' instead." #-}

-- | The number of units to request, filled using @DefaultTargetCapacityType@ .
--
-- /Note:/ Consider using 'totalTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsrTotalTargetCapacity :: Lens.Lens' TargetCapacitySpecificationRequest Lude.Int
tcsrTotalTargetCapacity = Lens.lens (totalTargetCapacity :: TargetCapacitySpecificationRequest -> Lude.Int) (\s a -> s {totalTargetCapacity = a} :: TargetCapacitySpecificationRequest)
{-# DEPRECATED tcsrTotalTargetCapacity "Use generic-lens or generic-optics with 'totalTargetCapacity' instead." #-}

instance Lude.ToQuery TargetCapacitySpecificationRequest where
  toQuery TargetCapacitySpecificationRequest' {..} =
    Lude.mconcat
      [ "OnDemandTargetCapacity" Lude.=: onDemandTargetCapacity,
        "DefaultTargetCapacityType" Lude.=: defaultTargetCapacityType,
        "SpotTargetCapacity" Lude.=: spotTargetCapacity,
        "TotalTargetCapacity" Lude.=: totalTargetCapacity
      ]
