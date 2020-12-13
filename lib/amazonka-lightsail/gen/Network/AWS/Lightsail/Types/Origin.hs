{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ofRegionName,
    ofResourceType,
    ofName,
    ofProtocolPolicy,
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
  { -- | The AWS Region name of the origin resource.
    regionName :: Lude.Maybe RegionName,
    -- | The resource type of the origin resource (e.g., /Instance/ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The name of the origin resource.
    name :: Lude.Maybe Lude.Text,
    -- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
    protocolPolicy :: Lude.Maybe OriginProtocolPolicyEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Origin' with the minimum fields required to make a request.
--
-- * 'regionName' - The AWS Region name of the origin resource.
-- * 'resourceType' - The resource type of the origin resource (e.g., /Instance/ ).
-- * 'name' - The name of the origin resource.
-- * 'protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
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
ofRegionName :: Lens.Lens' Origin (Lude.Maybe RegionName)
ofRegionName = Lens.lens (regionName :: Origin -> Lude.Maybe RegionName) (\s a -> s {regionName = a} :: Origin)
{-# DEPRECATED ofRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The resource type of the origin resource (e.g., /Instance/ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofResourceType :: Lens.Lens' Origin (Lude.Maybe ResourceType)
ofResourceType = Lens.lens (resourceType :: Origin -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Origin)
{-# DEPRECATED ofResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The name of the origin resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofName :: Lens.Lens' Origin (Lude.Maybe Lude.Text)
ofName = Lens.lens (name :: Origin -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Origin)
{-# DEPRECATED ofName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
--
-- /Note:/ Consider using 'protocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofProtocolPolicy :: Lens.Lens' Origin (Lude.Maybe OriginProtocolPolicyEnum)
ofProtocolPolicy = Lens.lens (protocolPolicy :: Origin -> Lude.Maybe OriginProtocolPolicyEnum) (\s a -> s {protocolPolicy = a} :: Origin)
{-# DEPRECATED ofProtocolPolicy "Use generic-lens or generic-optics with 'protocolPolicy' instead." #-}

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
