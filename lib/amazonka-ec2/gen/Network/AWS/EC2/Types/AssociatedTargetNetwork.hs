{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.AssociatedTargetNetwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AssociatedTargetNetwork
  ( AssociatedTargetNetwork (..),

    -- * Smart constructor
    mkAssociatedTargetNetwork,

    -- * Lenses
    atnNetworkId,
    atnNetworkType,
  )
where

import Network.AWS.EC2.Types.AssociatedNetworkType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a target network that is associated with a Client VPN endpoint. A target network is a subnet in a VPC.
--
-- /See:/ 'mkAssociatedTargetNetwork' smart constructor.
data AssociatedTargetNetwork = AssociatedTargetNetwork'
  { networkId ::
      Lude.Maybe Lude.Text,
    networkType ::
      Lude.Maybe AssociatedNetworkType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociatedTargetNetwork' with the minimum fields required to make a request.
--
-- * 'networkId' - The ID of the subnet.
-- * 'networkType' - The target network type.
mkAssociatedTargetNetwork ::
  AssociatedTargetNetwork
mkAssociatedTargetNetwork =
  AssociatedTargetNetwork'
    { networkId = Lude.Nothing,
      networkType = Lude.Nothing
    }

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'networkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atnNetworkId :: Lens.Lens' AssociatedTargetNetwork (Lude.Maybe Lude.Text)
atnNetworkId = Lens.lens (networkId :: AssociatedTargetNetwork -> Lude.Maybe Lude.Text) (\s a -> s {networkId = a} :: AssociatedTargetNetwork)
{-# DEPRECATED atnNetworkId "Use generic-lens or generic-optics with 'networkId' instead." #-}

-- | The target network type.
--
-- /Note:/ Consider using 'networkType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atnNetworkType :: Lens.Lens' AssociatedTargetNetwork (Lude.Maybe AssociatedNetworkType)
atnNetworkType = Lens.lens (networkType :: AssociatedTargetNetwork -> Lude.Maybe AssociatedNetworkType) (\s a -> s {networkType = a} :: AssociatedTargetNetwork)
{-# DEPRECATED atnNetworkType "Use generic-lens or generic-optics with 'networkType' instead." #-}

instance Lude.FromXML AssociatedTargetNetwork where
  parseXML x =
    AssociatedTargetNetwork'
      Lude.<$> (x Lude..@? "networkId") Lude.<*> (x Lude..@? "networkType")
