{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Origin
  ( Origin (..)
  -- * Smart constructor
  , mkOrigin
  -- * Lenses
  , oName
  , oProtocolPolicy
  , oRegionName
  , oResourceType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum as Types
import qualified Network.AWS.Lightsail.Types.RegionName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
-- /See:/ 'mkOrigin' smart constructor.
data Origin = Origin'
  { name :: Core.Maybe Types.Name
    -- ^ The name of the origin resource.
  , protocolPolicy :: Core.Maybe Types.OriginProtocolPolicyEnum
    -- ^ The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
  , regionName :: Core.Maybe Types.RegionName
    -- ^ The AWS Region name of the origin resource.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type of the origin resource (e.g., /Instance/ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Origin' value with any optional fields omitted.
mkOrigin
    :: Origin
mkOrigin
  = Origin'{name = Core.Nothing, protocolPolicy = Core.Nothing,
            regionName = Core.Nothing, resourceType = Core.Nothing}

-- | The name of the origin resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oName :: Lens.Lens' Origin (Core.Maybe Types.Name)
oName = Lens.field @"name"
{-# INLINEABLE oName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
--
-- /Note:/ Consider using 'protocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oProtocolPolicy :: Lens.Lens' Origin (Core.Maybe Types.OriginProtocolPolicyEnum)
oProtocolPolicy = Lens.field @"protocolPolicy"
{-# INLINEABLE oProtocolPolicy #-}
{-# DEPRECATED protocolPolicy "Use generic-lens or generic-optics with 'protocolPolicy' instead"  #-}

-- | The AWS Region name of the origin resource.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRegionName :: Lens.Lens' Origin (Core.Maybe Types.RegionName)
oRegionName = Lens.field @"regionName"
{-# INLINEABLE oRegionName #-}
{-# DEPRECATED regionName "Use generic-lens or generic-optics with 'regionName' instead"  #-}

-- | The resource type of the origin resource (e.g., /Instance/ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceType :: Lens.Lens' Origin (Core.Maybe Types.ResourceType)
oResourceType = Lens.field @"resourceType"
{-# INLINEABLE oResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

instance Core.FromJSON Origin where
        parseJSON
          = Core.withObject "Origin" Core.$
              \ x ->
                Origin' Core.<$>
                  (x Core..:? "name") Core.<*> x Core..:? "protocolPolicy" Core.<*>
                    x Core..:? "regionName"
                    Core.<*> x Core..:? "resourceType"
