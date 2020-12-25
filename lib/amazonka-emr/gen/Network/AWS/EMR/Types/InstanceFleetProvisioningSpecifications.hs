{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetProvisioningSpecifications
  ( InstanceFleetProvisioningSpecifications (..),

    -- * Smart constructor
    mkInstanceFleetProvisioningSpecifications,

    -- * Lenses
    ifpsOnDemandSpecification,
    ifpsSpotSpecification,
  )
where

import qualified Network.AWS.EMR.Types.OnDemandProvisioningSpecification as Types
import qualified Network.AWS.EMR.Types.SpotProvisioningSpecification as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /See:/ 'mkInstanceFleetProvisioningSpecifications' smart constructor.
data InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'
  { -- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
    onDemandSpecification :: Core.Maybe Types.OnDemandProvisioningSpecification,
    -- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
    spotSpecification :: Core.Maybe Types.SpotProvisioningSpecification
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceFleetProvisioningSpecifications' value with any optional fields omitted.
mkInstanceFleetProvisioningSpecifications ::
  InstanceFleetProvisioningSpecifications
mkInstanceFleetProvisioningSpecifications =
  InstanceFleetProvisioningSpecifications'
    { onDemandSpecification =
        Core.Nothing,
      spotSpecification = Core.Nothing
    }

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
--
-- /Note:/ Consider using 'onDemandSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifpsOnDemandSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Core.Maybe Types.OnDemandProvisioningSpecification)
ifpsOnDemandSpecification = Lens.field @"onDemandSpecification"
{-# DEPRECATED ifpsOnDemandSpecification "Use generic-lens or generic-optics with 'onDemandSpecification' instead." #-}

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /Note:/ Consider using 'spotSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifpsSpotSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Core.Maybe Types.SpotProvisioningSpecification)
ifpsSpotSpecification = Lens.field @"spotSpecification"
{-# DEPRECATED ifpsSpotSpecification "Use generic-lens or generic-optics with 'spotSpecification' instead." #-}

instance Core.FromJSON InstanceFleetProvisioningSpecifications where
  toJSON InstanceFleetProvisioningSpecifications {..} =
    Core.object
      ( Core.catMaybes
          [ ("OnDemandSpecification" Core..=) Core.<$> onDemandSpecification,
            ("SpotSpecification" Core..=) Core.<$> spotSpecification
          ]
      )

instance Core.FromJSON InstanceFleetProvisioningSpecifications where
  parseJSON =
    Core.withObject "InstanceFleetProvisioningSpecifications" Core.$
      \x ->
        InstanceFleetProvisioningSpecifications'
          Core.<$> (x Core..:? "OnDemandSpecification")
          Core.<*> (x Core..:? "SpotSpecification")
