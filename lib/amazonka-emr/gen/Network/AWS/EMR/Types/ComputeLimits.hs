{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ComputeLimits
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ComputeLimits
  ( ComputeLimits (..),

    -- * Smart constructor
    mkComputeLimits,

    -- * Lenses
    clMaximumOnDemandCapacityUnits,
    clMaximumCoreCapacityUnits,
    clUnitType,
    clMaximumCapacityUnits,
    clMinimumCapacityUnits,
  )
where

import Network.AWS.EMR.Types.ComputeLimitsUnitType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The EC2 unit limits for a managed scaling policy. The managed scaling activity of a cluster can not be above or below these limits. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /See:/ 'mkComputeLimits' smart constructor.
data ComputeLimits = ComputeLimits'
  { -- | The upper boundary of On-Demand EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The On-Demand units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between On-Demand and Spot Instances.
    maximumOnDemandCapacityUnits :: Lude.Maybe Lude.Int,
    -- | The upper boundary of EC2 units for core node type in a cluster. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The core units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between core and task nodes.
    maximumCoreCapacityUnits :: Lude.Maybe Lude.Int,
    -- | The unit type used for specifying a managed scaling policy.
    unitType :: ComputeLimitsUnitType,
    -- | The upper boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
    maximumCapacityUnits :: Lude.Int,
    -- | The lower boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
    minimumCapacityUnits :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComputeLimits' with the minimum fields required to make a request.
--
-- * 'maximumOnDemandCapacityUnits' - The upper boundary of On-Demand EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The On-Demand units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between On-Demand and Spot Instances.
-- * 'maximumCoreCapacityUnits' - The upper boundary of EC2 units for core node type in a cluster. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The core units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between core and task nodes.
-- * 'unitType' - The unit type used for specifying a managed scaling policy.
-- * 'maximumCapacityUnits' - The upper boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
-- * 'minimumCapacityUnits' - The lower boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
mkComputeLimits ::
  -- | 'unitType'
  ComputeLimitsUnitType ->
  -- | 'maximumCapacityUnits'
  Lude.Int ->
  -- | 'minimumCapacityUnits'
  Lude.Int ->
  ComputeLimits
mkComputeLimits
  pUnitType_
  pMaximumCapacityUnits_
  pMinimumCapacityUnits_ =
    ComputeLimits'
      { maximumOnDemandCapacityUnits = Lude.Nothing,
        maximumCoreCapacityUnits = Lude.Nothing,
        unitType = pUnitType_,
        maximumCapacityUnits = pMaximumCapacityUnits_,
        minimumCapacityUnits = pMinimumCapacityUnits_
      }

-- | The upper boundary of On-Demand EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The On-Demand units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between On-Demand and Spot Instances.
--
-- /Note:/ Consider using 'maximumOnDemandCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumOnDemandCapacityUnits :: Lens.Lens' ComputeLimits (Lude.Maybe Lude.Int)
clMaximumOnDemandCapacityUnits = Lens.lens (maximumOnDemandCapacityUnits :: ComputeLimits -> Lude.Maybe Lude.Int) (\s a -> s {maximumOnDemandCapacityUnits = a} :: ComputeLimits)
{-# DEPRECATED clMaximumOnDemandCapacityUnits "Use generic-lens or generic-optics with 'maximumOnDemandCapacityUnits' instead." #-}

-- | The upper boundary of EC2 units for core node type in a cluster. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. The core units are not allowed to scale beyond this boundary. The parameter is used to split capacity allocation between core and task nodes.
--
-- /Note:/ Consider using 'maximumCoreCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumCoreCapacityUnits :: Lens.Lens' ComputeLimits (Lude.Maybe Lude.Int)
clMaximumCoreCapacityUnits = Lens.lens (maximumCoreCapacityUnits :: ComputeLimits -> Lude.Maybe Lude.Int) (\s a -> s {maximumCoreCapacityUnits = a} :: ComputeLimits)
{-# DEPRECATED clMaximumCoreCapacityUnits "Use generic-lens or generic-optics with 'maximumCoreCapacityUnits' instead." #-}

-- | The unit type used for specifying a managed scaling policy.
--
-- /Note:/ Consider using 'unitType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clUnitType :: Lens.Lens' ComputeLimits ComputeLimitsUnitType
clUnitType = Lens.lens (unitType :: ComputeLimits -> ComputeLimitsUnitType) (\s a -> s {unitType = a} :: ComputeLimits)
{-# DEPRECATED clUnitType "Use generic-lens or generic-optics with 'unitType' instead." #-}

-- | The upper boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /Note:/ Consider using 'maximumCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMaximumCapacityUnits :: Lens.Lens' ComputeLimits Lude.Int
clMaximumCapacityUnits = Lens.lens (maximumCapacityUnits :: ComputeLimits -> Lude.Int) (\s a -> s {maximumCapacityUnits = a} :: ComputeLimits)
{-# DEPRECATED clMaximumCapacityUnits "Use generic-lens or generic-optics with 'maximumCapacityUnits' instead." #-}

-- | The lower boundary of EC2 units. It is measured through vCPU cores or instances for instance groups and measured through units for instance fleets. Managed scaling activities are not allowed beyond this boundary. The limit only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
--
-- /Note:/ Consider using 'minimumCapacityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clMinimumCapacityUnits :: Lens.Lens' ComputeLimits Lude.Int
clMinimumCapacityUnits = Lens.lens (minimumCapacityUnits :: ComputeLimits -> Lude.Int) (\s a -> s {minimumCapacityUnits = a} :: ComputeLimits)
{-# DEPRECATED clMinimumCapacityUnits "Use generic-lens or generic-optics with 'minimumCapacityUnits' instead." #-}

instance Lude.FromJSON ComputeLimits where
  parseJSON =
    Lude.withObject
      "ComputeLimits"
      ( \x ->
          ComputeLimits'
            Lude.<$> (x Lude..:? "MaximumOnDemandCapacityUnits")
            Lude.<*> (x Lude..:? "MaximumCoreCapacityUnits")
            Lude.<*> (x Lude..: "UnitType")
            Lude.<*> (x Lude..: "MaximumCapacityUnits")
            Lude.<*> (x Lude..: "MinimumCapacityUnits")
      )

instance Lude.ToJSON ComputeLimits where
  toJSON ComputeLimits' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumOnDemandCapacityUnits" Lude..=)
              Lude.<$> maximumOnDemandCapacityUnits,
            ("MaximumCoreCapacityUnits" Lude..=)
              Lude.<$> maximumCoreCapacityUnits,
            Lude.Just ("UnitType" Lude..= unitType),
            Lude.Just ("MaximumCapacityUnits" Lude..= maximumCapacityUnits),
            Lude.Just ("MinimumCapacityUnits" Lude..= minimumCapacityUnits)
          ]
      )
