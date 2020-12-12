{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.NewBGPPeer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.NewBGPPeer
  ( NewBGPPeer (..),

    -- * Smart constructor
    mkNewBGPPeer,

    -- * Lenses
    nbpCustomerAddress,
    nbpAmazonAddress,
    nbpAddressFamily,
    nbpAsn,
    nbpAuthKey,
  )
where

import Network.AWS.DirectConnect.Types.AddressFamily
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a new BGP peer.
--
-- /See:/ 'mkNewBGPPeer' smart constructor.
data NewBGPPeer = NewBGPPeer'
  { customerAddress ::
      Lude.Maybe Lude.Text,
    amazonAddress :: Lude.Maybe Lude.Text,
    addressFamily :: Lude.Maybe AddressFamily,
    asn :: Lude.Maybe Lude.Int,
    authKey :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NewBGPPeer' with the minimum fields required to make a request.
--
-- * 'addressFamily' - The address family for the BGP peer.
-- * 'amazonAddress' - The IP address assigned to the Amazon interface.
-- * 'asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
-- * 'authKey' - The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
-- * 'customerAddress' - The IP address assigned to the customer interface.
mkNewBGPPeer ::
  NewBGPPeer
mkNewBGPPeer =
  NewBGPPeer'
    { customerAddress = Lude.Nothing,
      amazonAddress = Lude.Nothing,
      addressFamily = Lude.Nothing,
      asn = Lude.Nothing,
      authKey = Lude.Nothing
    }

-- | The IP address assigned to the customer interface.
--
-- /Note:/ Consider using 'customerAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbpCustomerAddress :: Lens.Lens' NewBGPPeer (Lude.Maybe Lude.Text)
nbpCustomerAddress = Lens.lens (customerAddress :: NewBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {customerAddress = a} :: NewBGPPeer)
{-# DEPRECATED nbpCustomerAddress "Use generic-lens or generic-optics with 'customerAddress' instead." #-}

-- | The IP address assigned to the Amazon interface.
--
-- /Note:/ Consider using 'amazonAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbpAmazonAddress :: Lens.Lens' NewBGPPeer (Lude.Maybe Lude.Text)
nbpAmazonAddress = Lens.lens (amazonAddress :: NewBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {amazonAddress = a} :: NewBGPPeer)
{-# DEPRECATED nbpAmazonAddress "Use generic-lens or generic-optics with 'amazonAddress' instead." #-}

-- | The address family for the BGP peer.
--
-- /Note:/ Consider using 'addressFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbpAddressFamily :: Lens.Lens' NewBGPPeer (Lude.Maybe AddressFamily)
nbpAddressFamily = Lens.lens (addressFamily :: NewBGPPeer -> Lude.Maybe AddressFamily) (\s a -> s {addressFamily = a} :: NewBGPPeer)
{-# DEPRECATED nbpAddressFamily "Use generic-lens or generic-optics with 'addressFamily' instead." #-}

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP) configuration.
--
-- /Note:/ Consider using 'asn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbpAsn :: Lens.Lens' NewBGPPeer (Lude.Maybe Lude.Int)
nbpAsn = Lens.lens (asn :: NewBGPPeer -> Lude.Maybe Lude.Int) (\s a -> s {asn = a} :: NewBGPPeer)
{-# DEPRECATED nbpAsn "Use generic-lens or generic-optics with 'asn' instead." #-}

-- | The authentication key for BGP configuration. This string has a minimum length of 6 characters and and a maximun lenth of 80 characters.
--
-- /Note:/ Consider using 'authKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nbpAuthKey :: Lens.Lens' NewBGPPeer (Lude.Maybe Lude.Text)
nbpAuthKey = Lens.lens (authKey :: NewBGPPeer -> Lude.Maybe Lude.Text) (\s a -> s {authKey = a} :: NewBGPPeer)
{-# DEPRECATED nbpAuthKey "Use generic-lens or generic-optics with 'authKey' instead." #-}

instance Lude.ToJSON NewBGPPeer where
  toJSON NewBGPPeer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customerAddress" Lude..=) Lude.<$> customerAddress,
            ("amazonAddress" Lude..=) Lude.<$> amazonAddress,
            ("addressFamily" Lude..=) Lude.<$> addressFamily,
            ("asn" Lude..=) Lude.<$> asn,
            ("authKey" Lude..=) Lude.<$> authKey
          ]
      )
