{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPrice
  ( SpotPrice (..),

    -- * Smart constructor
    mkSpotPrice,

    -- * Lenses
    sAvailabilityZone,
    sInstanceType,
    sProductDescription,
    sSpotPrice,
    sTimestamp,
  )
where

import qualified Network.AWS.EC2.Types.InstanceType as Types
import qualified Network.AWS.EC2.Types.RIProductDescription as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /See:/ 'mkSpotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { -- | The Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | The instance type.
    instanceType :: Core.Maybe Types.InstanceType,
    -- | A general description of the AMI.
    productDescription :: Core.Maybe Types.RIProductDescription,
    -- | The maximum price per hour that you are willing to pay for a Spot Instance.
    spotPrice :: Core.Maybe Types.String,
    -- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
    timestamp :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SpotPrice' value with any optional fields omitted.
mkSpotPrice ::
  SpotPrice
mkSpotPrice =
  SpotPrice'
    { availabilityZone = Core.Nothing,
      instanceType = Core.Nothing,
      productDescription = Core.Nothing,
      spotPrice = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAvailabilityZone :: Lens.Lens' SpotPrice (Core.Maybe Types.String)
sAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED sAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInstanceType :: Lens.Lens' SpotPrice (Core.Maybe Types.InstanceType)
sInstanceType = Lens.field @"instanceType"
{-# DEPRECATED sInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A general description of the AMI.
--
-- /Note:/ Consider using 'productDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProductDescription :: Lens.Lens' SpotPrice (Core.Maybe Types.RIProductDescription)
sProductDescription = Lens.field @"productDescription"
{-# DEPRECATED sProductDescription "Use generic-lens or generic-optics with 'productDescription' instead." #-}

-- | The maximum price per hour that you are willing to pay for a Spot Instance.
--
-- /Note:/ Consider using 'spotPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSpotPrice :: Lens.Lens' SpotPrice (Core.Maybe Types.String)
sSpotPrice = Lens.field @"spotPrice"
{-# DEPRECATED sSpotPrice "Use generic-lens or generic-optics with 'spotPrice' instead." #-}

-- | The date and time the request was created, in UTC format (for example, /YYYY/ -/MM/ -/DD/ T/HH/ :/MM/ :/SS/ Z).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTimestamp :: Lens.Lens' SpotPrice (Core.Maybe Core.UTCTime)
sTimestamp = Lens.field @"timestamp"
{-# DEPRECATED sTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Core.<$> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "productDescription")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "timestamp")
