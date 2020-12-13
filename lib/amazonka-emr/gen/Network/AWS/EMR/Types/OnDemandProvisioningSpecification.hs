{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.OnDemandProvisioningSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.OnDemandProvisioningSpecification
  ( OnDemandProvisioningSpecification (..),

    -- * Smart constructor
    mkOnDemandProvisioningSpecification,

    -- * Lenses
    odpsAllocationStrategy,
  )
where

import Network.AWS.EMR.Types.OnDemandProvisioningAllocationStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The launch specification for On-Demand Instances in the instance fleet, which determines the allocation strategy.
--
-- /See:/ 'mkOnDemandProvisioningSpecification' smart constructor.
newtype OnDemandProvisioningSpecification = OnDemandProvisioningSpecification'
  { -- | Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first.
    allocationStrategy :: OnDemandProvisioningAllocationStrategy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnDemandProvisioningSpecification' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first.
mkOnDemandProvisioningSpecification ::
  -- | 'allocationStrategy'
  OnDemandProvisioningAllocationStrategy ->
  OnDemandProvisioningSpecification
mkOnDemandProvisioningSpecification pAllocationStrategy_ =
  OnDemandProvisioningSpecification'
    { allocationStrategy =
        pAllocationStrategy_
    }

-- | Specifies the strategy to use in launching On-Demand Instance fleets. Currently, the only option is lowest-price (the default), which launches the lowest price first.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odpsAllocationStrategy :: Lens.Lens' OnDemandProvisioningSpecification OnDemandProvisioningAllocationStrategy
odpsAllocationStrategy = Lens.lens (allocationStrategy :: OnDemandProvisioningSpecification -> OnDemandProvisioningAllocationStrategy) (\s a -> s {allocationStrategy = a} :: OnDemandProvisioningSpecification)
{-# DEPRECATED odpsAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

instance Lude.FromJSON OnDemandProvisioningSpecification where
  parseJSON =
    Lude.withObject
      "OnDemandProvisioningSpecification"
      ( \x ->
          OnDemandProvisioningSpecification'
            Lude.<$> (x Lude..: "AllocationStrategy")
      )

instance Lude.ToJSON OnDemandProvisioningSpecification where
  toJSON OnDemandProvisioningSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AllocationStrategy" Lude..= allocationStrategy)]
      )
