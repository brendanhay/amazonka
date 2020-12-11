-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Origin
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Origin
  ( Origin (..),

    -- * Smart constructor
    mkOrigin,

    -- * Lenses
    oRegionName,
    oResourceType,
    oName,
    oProtocolPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.RegionName
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
-- /See:/ 'mkOrigin' smart constructor.
data Origin = Origin'
  { regionName :: Lude.Maybe RegionName,
    resourceType :: Lude.Maybe ResourceType,
    name :: Lude.Maybe Lude.Text,
    protocolPolicy :: Lude.Maybe OriginProtocolPolicyEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- * 'name' - The name of the origin resource.
-- * 'protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
-- * 'regionName' - The AWS Region name of the origin resource.
-- * 'resourceType' - The resource type of the origin resource (e.g., /Instance/ ).
mkOrigin ::
  Origin
mkOrigin =
  Origin'
    { regionName = Lude.Nothing,
      resourceType = Lude.Nothing,
      name = Lude.Nothing,
      protocolPolicy = Lude.Nothing
    }

-- | The AWS Region name of the origin resource.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oRegionName :: Lens.Lens' Origin (Lude.Maybe RegionName)
oRegionName = Lens.lens (regionName :: Origin -> Lude.Maybe RegionName) (\s a -> s {regionName = a} :: Origin)
{-# DEPRECATED oRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The resource type of the origin resource (e.g., /Instance/ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceType :: Lens.Lens' Origin (Lude.Maybe ResourceType)
oResourceType = Lens.lens (resourceType :: Origin -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Origin)
{-# DEPRECATED oResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the origin resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oName :: Lens.Lens' Origin (Lude.Maybe Lude.Text)
oName = Lens.lens (name :: Origin -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Origin)
{-# DEPRECATED oName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
--
-- /Note:/ Consider using 'protocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oProtocolPolicy :: Lens.Lens' Origin (Lude.Maybe OriginProtocolPolicyEnum)
oProtocolPolicy = Lens.lens (protocolPolicy :: Origin -> Lude.Maybe OriginProtocolPolicyEnum) (\s a -> s {protocolPolicy = a} :: Origin)
{-# DEPRECATED oProtocolPolicy "Use generic-lens or generic-optics with 'protocolPolicy' instead." #-}

instance Lude.FromJSON Origin where
  parseJSON =
    Lude.withObject
      "Origin"
      ( \x ->
          Origin'
            Lude.<$> (x Lude..:? "regionName")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "protocolPolicy")
      )
