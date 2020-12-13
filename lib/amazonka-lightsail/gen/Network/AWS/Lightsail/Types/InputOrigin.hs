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
    ioRegionName,
    ioName,
    ioProtocolPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OriginProtocolPolicyEnum
import Network.AWS.Lightsail.Types.RegionName
import qualified Network.AWS.Prelude as Lude

-- | Describes the origin resource of an Amazon Lightsail content delivery network (CDN) distribution.
--
-- An origin can be a Lightsail instance or load balancer. A distribution pulls content from an origin, caches it, and serves it to viewers via a worldwide network of edge servers.
--
-- /See:/ 'mkInputOrigin' smart constructor.
data InputOrigin = InputOrigin'
  { -- | The AWS Region name of the origin resource.
    regionName :: Lude.Maybe RegionName,
    -- | The name of the origin resource.
    name :: Lude.Maybe Lude.Text,
    -- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
    protocolPolicy :: Lude.Maybe OriginProtocolPolicyEnum
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputOrigin' with the minimum fields required to make a request.
--
-- * 'regionName' - The AWS Region name of the origin resource.
-- * 'name' - The name of the origin resource.
-- * 'protocolPolicy' - The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
mkInputOrigin ::
  InputOrigin
mkInputOrigin =
  InputOrigin'
    { regionName = Lude.Nothing,
      name = Lude.Nothing,
      protocolPolicy = Lude.Nothing
    }

-- | The AWS Region name of the origin resource.
--
-- /Note:/ Consider using 'regionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioRegionName :: Lens.Lens' InputOrigin (Lude.Maybe RegionName)
ioRegionName = Lens.lens (regionName :: InputOrigin -> Lude.Maybe RegionName) (\s a -> s {regionName = a} :: InputOrigin)
{-# DEPRECATED ioRegionName "Use generic-lens or generic-optics with 'regionName' instead." #-}

-- | The name of the origin resource.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioName :: Lens.Lens' InputOrigin (Lude.Maybe Lude.Text)
ioName = Lens.lens (name :: InputOrigin -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InputOrigin)
{-# DEPRECATED ioName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocol that your Amazon Lightsail distribution uses when establishing a connection with your origin to pull content.
--
-- /Note:/ Consider using 'protocolPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioProtocolPolicy :: Lens.Lens' InputOrigin (Lude.Maybe OriginProtocolPolicyEnum)
ioProtocolPolicy = Lens.lens (protocolPolicy :: InputOrigin -> Lude.Maybe OriginProtocolPolicyEnum) (\s a -> s {protocolPolicy = a} :: InputOrigin)
{-# DEPRECATED ioProtocolPolicy "Use generic-lens or generic-optics with 'protocolPolicy' instead." #-}

instance Lude.ToJSON InputOrigin where
  toJSON InputOrigin' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("regionName" Lude..=) Lude.<$> regionName,
            ("name" Lude..=) Lude.<$> name,
            ("protocolPolicy" Lude..=) Lude.<$> protocolPolicy
          ]
      )
