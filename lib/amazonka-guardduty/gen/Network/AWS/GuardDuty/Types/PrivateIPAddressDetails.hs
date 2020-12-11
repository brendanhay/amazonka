-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.PrivateIPAddressDetails
  ( PrivateIPAddressDetails (..),

    -- * Smart constructor
    mkPrivateIPAddressDetails,

    -- * Lenses
    piadPrivateIPAddress,
    piadPrivateDNSName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains other private IP address information of the EC2 instance.
--
-- /See:/ 'mkPrivateIPAddressDetails' smart constructor.
data PrivateIPAddressDetails = PrivateIPAddressDetails'
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

-- | Creates a value of 'PrivateIPAddressDetails' with the minimum fields required to make a request.
--
-- * 'privateDNSName' - The private DNS name of the EC2 instance.
-- * 'privateIPAddress' - The private IP address of the EC2 instance.
mkPrivateIPAddressDetails ::
  PrivateIPAddressDetails
mkPrivateIPAddressDetails =
  PrivateIPAddressDetails'
    { privateIPAddress = Lude.Nothing,
      privateDNSName = Lude.Nothing
    }

-- | The private IP address of the EC2 instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piadPrivateIPAddress :: Lens.Lens' PrivateIPAddressDetails (Lude.Maybe Lude.Text)
piadPrivateIPAddress = Lens.lens (privateIPAddress :: PrivateIPAddressDetails -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: PrivateIPAddressDetails)
{-# DEPRECATED piadPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The private DNS name of the EC2 instance.
--
-- /Note:/ Consider using 'privateDNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piadPrivateDNSName :: Lens.Lens' PrivateIPAddressDetails (Lude.Maybe Lude.Text)
piadPrivateDNSName = Lens.lens (privateDNSName :: PrivateIPAddressDetails -> Lude.Maybe Lude.Text) (\s a -> s {privateDNSName = a} :: PrivateIPAddressDetails)
{-# DEPRECATED piadPrivateDNSName "Use generic-lens or generic-optics with 'privateDNSName' instead." #-}

instance Lude.FromJSON PrivateIPAddressDetails where
  parseJSON =
    Lude.withObject
      "PrivateIPAddressDetails"
      ( \x ->
          PrivateIPAddressDetails'
            Lude.<$> (x Lude..:? "privateIpAddress")
            Lude.<*> (x Lude..:? "privateDnsName")
      )
