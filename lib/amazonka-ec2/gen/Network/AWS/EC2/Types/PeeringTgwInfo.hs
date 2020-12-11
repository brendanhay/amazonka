-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PeeringTgwInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PeeringTgwInfo
  ( PeeringTgwInfo (..),

    -- * Smart constructor
    mkPeeringTgwInfo,

    -- * Lenses
    ptiOwnerId,
    ptiTransitGatewayId,
    ptiRegion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the transit gateway in the peering attachment.
--
-- /See:/ 'mkPeeringTgwInfo' smart constructor.
data PeeringTgwInfo = PeeringTgwInfo'
  { ownerId ::
      Lude.Maybe Lude.Text,
    transitGatewayId :: Lude.Maybe Lude.Text,
    region :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PeeringTgwInfo' with the minimum fields required to make a request.
--
-- * 'ownerId' - The AWS account ID of the owner of the transit gateway.
-- * 'region' - The Region of the transit gateway.
-- * 'transitGatewayId' - The ID of the transit gateway.
mkPeeringTgwInfo ::
  PeeringTgwInfo
mkPeeringTgwInfo =
  PeeringTgwInfo'
    { ownerId = Lude.Nothing,
      transitGatewayId = Lude.Nothing,
      region = Lude.Nothing
    }

-- | The AWS account ID of the owner of the transit gateway.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiOwnerId :: Lens.Lens' PeeringTgwInfo (Lude.Maybe Lude.Text)
ptiOwnerId = Lens.lens (ownerId :: PeeringTgwInfo -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: PeeringTgwInfo)
{-# DEPRECATED ptiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiTransitGatewayId :: Lens.Lens' PeeringTgwInfo (Lude.Maybe Lude.Text)
ptiTransitGatewayId = Lens.lens (transitGatewayId :: PeeringTgwInfo -> Lude.Maybe Lude.Text) (\s a -> s {transitGatewayId = a} :: PeeringTgwInfo)
{-# DEPRECATED ptiTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | The Region of the transit gateway.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptiRegion :: Lens.Lens' PeeringTgwInfo (Lude.Maybe Lude.Text)
ptiRegion = Lens.lens (region :: PeeringTgwInfo -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: PeeringTgwInfo)
{-# DEPRECATED ptiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

instance Lude.FromXML PeeringTgwInfo where
  parseXML x =
    PeeringTgwInfo'
      Lude.<$> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "transitGatewayId")
      Lude.<*> (x Lude..@? "region")
