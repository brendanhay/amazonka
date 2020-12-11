-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.PrivateIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.PrivateIP
  ( PrivateIP (..),

    -- * Smart constructor
    mkPrivateIP,

    -- * Lenses
    piPrivateIPAddress,
    piPrivateDNSName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a private IP address associated with a network interface. This data type is used as a response element in the 'DescribeFindings' action.
--
-- /See:/ 'mkPrivateIP' smart constructor.
data PrivateIP = PrivateIP'
  { privateIPAddress ::
      Lude.Maybe Lude.Text,
    privateDNSName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrivateIP' with the minimum fields required to make a request.
--
-- * 'privateDNSName' - The DNS name of the private IP address.
-- * 'privateIPAddress' - The full IP address of the network inteface.
mkPrivateIP ::
  PrivateIP
mkPrivateIP =
  PrivateIP'
    { privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing
    }

-- | The full IP address of the network inteface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPrivateIPAddress :: Lens.Lens' PrivateIP (Lude.Maybe Lude.Text)
piPrivateIPAddress = Lens.lens (privateIPAddress :: PrivateIP -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: PrivateIP)
{-# DEPRECATED piPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The DNS name of the private IP address.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPrivateDNSName :: Lens.Lens' PrivateIP (Lude.Maybe Lude.Text)
piPrivateDNSName = Lens.lens (privateDNSName :: PrivateIP -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: PrivateIP)
{-# DEPRECATED piPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

instance Lude.FromJSON PrivateIP where
  parseJSON =
    Lude.withObject
      "PrivateIP"
      ( \x ->
          PrivateIP'
            Lude.<$> (x Lude..:? "privateIpAddress")
            Lude.<*> (x Lude..:? "privateDnsName")
      )
