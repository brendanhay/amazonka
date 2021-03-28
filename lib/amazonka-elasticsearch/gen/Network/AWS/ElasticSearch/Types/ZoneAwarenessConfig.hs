{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticSearch.Types.ZoneAwarenessConfig
  ( ZoneAwarenessConfig (..)
  -- * Smart constructor
  , mkZoneAwarenessConfig
  -- * Lenses
  , zacAvailabilityZoneCount
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the zone awareness configuration for the domain cluster, such as the number of availability zones.
--
-- /See:/ 'mkZoneAwarenessConfig' smart constructor.
newtype ZoneAwarenessConfig = ZoneAwarenessConfig'
  { availabilityZoneCount :: Core.Maybe Core.Int
    -- ^ An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ZoneAwarenessConfig' value with any optional fields omitted.
mkZoneAwarenessConfig
    :: ZoneAwarenessConfig
mkZoneAwarenessConfig
  = ZoneAwarenessConfig'{availabilityZoneCount = Core.Nothing}

-- | An integer value to indicate the number of availability zones for a domain when zone awareness is enabled. This should be equal to number of subnets if VPC endpoints is enabled
--
-- /Note:/ Consider using 'availabilityZoneCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
zacAvailabilityZoneCount :: Lens.Lens' ZoneAwarenessConfig (Core.Maybe Core.Int)
zacAvailabilityZoneCount = Lens.field @"availabilityZoneCount"
{-# INLINEABLE zacAvailabilityZoneCount #-}
{-# DEPRECATED availabilityZoneCount "Use generic-lens or generic-optics with 'availabilityZoneCount' instead"  #-}

instance Core.FromJSON ZoneAwarenessConfig where
        toJSON ZoneAwarenessConfig{..}
          = Core.object
              (Core.catMaybes
                 [("AvailabilityZoneCount" Core..=) Core.<$> availabilityZoneCount])

instance Core.FromJSON ZoneAwarenessConfig where
        parseJSON
          = Core.withObject "ZoneAwarenessConfig" Core.$
              \ x ->
                ZoneAwarenessConfig' Core.<$> (x Core..:? "AvailabilityZoneCount")
