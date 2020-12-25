{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InputOrigin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InputOrigin
  ( InputOrigin (..),

    -- * Smart constructor
    mkInputOrigin,

    -- * Lenses
    ioName,
    ioProtocolPolicy,
    ioRegionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum as Types
import qualified Network.AWS.Lightsail.Types.RegionName as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
-- /See:/ 'mkInputOrigin' smart constructor.
data InputOrigin = InputOrigin'
  { -- | The name of the origin resource.
    name :: Core.Maybe Types.ResourceName,
    -- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
    protocolPolicy :: Core.Maybe Types.OriginProtocolPolicyEnum,
    -- | The AWS Region name of the origin resource.
    regionName :: Core.Maybe Types.RegionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputOrigin' value with any optional fields omitted.
mkInputOrigin ::
  InputOrigin
mkInputOrigin =
  InputOrigin'
    { name = Core.Nothing,
      protocolPolicy = Core.Nothing,
      regionName = Core.Nothing
    }

-- | The name of the origin resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioName :: Lens.Lens' InputOrigin (Core.Maybe Types.ResourceName)
ioName = Lens.field @"name"
{-# DEPRECATED ioName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
--
-- /Note:/ Consider using 'protocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioProtocolPolicy :: Lens.Lens' InputOrigin (Core.Maybe Types.OriginProtocolPolicyEnum)
ioProtocolPolicy = Lens.field @"protocolPolicy"
{-# DEPRECATED ioProtocolPolicy "Use generic-lens or generic-optics with 'protocolPolicy' instead." #-}

-- | The AWS Region name of the origin resource.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioRegionName :: Lens.Lens' InputOrigin (Core.Maybe Types.RegionName)
ioRegionName = Lens.field @"regionName"
{-# DEPRECATED ioRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

instance Core.FromJSON InputOrigin where
  toJSON InputOrigin {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("protocolPolicy" Core..=) Core.<$> protocolPolicy,
            ("regionName" Core..=) Core.<$> regionName
          ]
      )
