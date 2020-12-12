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
    ifpsSpotSpecification,
    ifpsOnDemandSpecification,
  )
where

import Network.AWS.EMR.Types.OnDemandProvisioningSpecification
import Network.AWS.EMR.Types.SpotProvisioningSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /See:/ 'mkInstanceFleetProvisioningSpecifications' smart constructor.
data InstanceFleetProvisioningSpecifications = InstanceFleetProvisioningSpecifications'
  { spotSpecification ::
      Lude.Maybe
        SpotProvisioningSpecification,
    onDemandSpecification ::
      Lude.Maybe
        OnDemandProvisioningSpecification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleetProvisioningSpecifications' with the minimum fields required to make a request.
--
-- * 'onDemandSpecification' - The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
-- * 'spotSpecification' - The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
mkInstanceFleetProvisioningSpecifications ::
  InstanceFleetProvisioningSpecifications
mkInstanceFleetProvisioningSpecifications =
  InstanceFleetProvisioningSpecifications'
    { spotSpecification =
        Lude.Nothing,
      onDemandSpecification = Lude.Nothing
    }

-- | The launch specification for Spot Instances in the fleet, which determines the defined duration, provisioning timeout behavior, and allocation strategy.
--
-- /Note:/ Consider using 'spotSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifpsSpotSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Lude.Maybe SpotProvisioningSpecification)
ifpsSpotSpecification = Lens.lens (spotSpecification :: InstanceFleetProvisioningSpecifications -> Lude.Maybe SpotProvisioningSpecification) (\s a -> s {spotSpecification = a} :: InstanceFleetProvisioningSpecifications)
{-# DEPRECATED ifpsSpotSpecification "Use generic-lens or generic-optics with 'spotSpecification' instead." #-}

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
--
-- /Note:/ Consider using 'onDemandSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifpsOnDemandSpecification :: Lens.Lens' InstanceFleetProvisioningSpecifications (Lude.Maybe OnDemandProvisioningSpecification)
ifpsOnDemandSpecification = Lens.lens (onDemandSpecification :: InstanceFleetProvisioningSpecifications -> Lude.Maybe OnDemandProvisioningSpecification) (\s a -> s {onDemandSpecification = a} :: InstanceFleetProvisioningSpecifications)
{-# DEPRECATED ifpsOnDemandSpecification "Use generic-lens or generic-optics with 'onDemandSpecification' instead." #-}

instance Lude.FromJSON InstanceFleetProvisioningSpecifications where
  parseJSON =
    Lude.withObject
      "InstanceFleetProvisioningSpecifications"
      ( \x ->
          InstanceFleetProvisioningSpecifications'
            Lude.<$> (x Lude..:? "SpotSpecification")
            Lude.<*> (x Lude..:? "OnDemandSpecification")
      )

instance Lude.ToJSON InstanceFleetProvisioningSpecifications where
  toJSON InstanceFleetProvisioningSpecifications' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SpotSpecification" Lude..=) Lude.<$> spotSpecification,
            ("OnDemandSpecification" Lude..=) Lude.<$> onDemandSpecification
          ]
      )
