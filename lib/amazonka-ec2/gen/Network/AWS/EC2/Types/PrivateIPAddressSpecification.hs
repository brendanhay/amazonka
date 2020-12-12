{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PrivateIPAddressSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PrivateIPAddressSpecification
  ( PrivateIPAddressSpecification (..),

    -- * Smart constructor
    mkPrivateIPAddressSpecification,

    -- * Lenses
    piasPrimary,
    piasPrivateIPAddress,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a secondary private IPv4 address for a network interface.
--
-- /See:/ 'mkPrivateIPAddressSpecification' smart constructor.
data PrivateIPAddressSpecification = PrivateIPAddressSpecification'
  { primary ::
      Lude.Maybe Lude.Bool,
    privateIPAddress ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PrivateIPAddressSpecification' with the minimum fields required to make a request.
--
-- * 'primary' - Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
-- * 'privateIPAddress' - The private IPv4 addresses.
mkPrivateIPAddressSpecification ::
  PrivateIPAddressSpecification
mkPrivateIPAddressSpecification =
  PrivateIPAddressSpecification'
    { primary = Lude.Nothing,
      privateIPAddress = Lude.Nothing
    }

-- | Indicates whether the private IPv4 address is the primary private IPv4 address. Only one IPv4 address can be designated as primary.
--
-- /Note:/ Consider using 'primary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piasPrimary :: Lens.Lens' PrivateIPAddressSpecification (Lude.Maybe Lude.Bool)
piasPrimary = Lens.lens (primary :: PrivateIPAddressSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {primary = a} :: PrivateIPAddressSpecification)
{-# DEPRECATED piasPrimary "Use generic-lens or generic-optics with 'primary' instead." #-}

-- | The private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piasPrivateIPAddress :: Lens.Lens' PrivateIPAddressSpecification (Lude.Maybe Lude.Text)
piasPrivateIPAddress = Lens.lens (privateIPAddress :: PrivateIPAddressSpecification -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: PrivateIPAddressSpecification)
{-# DEPRECATED piasPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

instance Lude.FromXML PrivateIPAddressSpecification where
  parseXML x =
    PrivateIPAddressSpecification'
      Lude.<$> (x Lude..@? "primary") Lude.<*> (x Lude..@? "privateIpAddress")

instance Lude.ToQuery PrivateIPAddressSpecification where
  toQuery PrivateIPAddressSpecification' {..} =
    Lude.mconcat
      [ "Primary" Lude.=: primary,
        "PrivateIpAddress" Lude.=: privateIPAddress
      ]
