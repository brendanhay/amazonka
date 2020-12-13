{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIPv4PoolRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIPv4PoolRange
  ( PublicIPv4PoolRange (..),

    -- * Smart constructor
    mkPublicIPv4PoolRange,

    -- * Lenses
    piprAvailableAddressCount,
    piprLastAddress,
    piprFirstAddress,
    piprAddressCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an address range of an IPv4 address pool.
--
-- /See:/ 'mkPublicIPv4PoolRange' smart constructor.
data PublicIPv4PoolRange = PublicIPv4PoolRange'
  { -- | The number of available addresses in the range.
    availableAddressCount :: Lude.Maybe Lude.Int,
    -- | The last IP address in the range.
    lastAddress :: Lude.Maybe Lude.Text,
    -- | The first IP address in the range.
    firstAddress :: Lude.Maybe Lude.Text,
    -- | The number of addresses in the range.
    addressCount :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicIPv4PoolRange' with the minimum fields required to make a request.
--
-- * 'availableAddressCount' - The number of available addresses in the range.
-- * 'lastAddress' - The last IP address in the range.
-- * 'firstAddress' - The first IP address in the range.
-- * 'addressCount' - The number of addresses in the range.
mkPublicIPv4PoolRange ::
  PublicIPv4PoolRange
mkPublicIPv4PoolRange =
  PublicIPv4PoolRange'
    { availableAddressCount = Lude.Nothing,
      lastAddress = Lude.Nothing,
      firstAddress = Lude.Nothing,
      addressCount = Lude.Nothing
    }

-- | The number of available addresses in the range.
--
-- /Note:/ Consider using 'availableAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprAvailableAddressCount :: Lens.Lens' PublicIPv4PoolRange (Lude.Maybe Lude.Int)
piprAvailableAddressCount = Lens.lens (availableAddressCount :: PublicIPv4PoolRange -> Lude.Maybe Lude.Int) (\s a -> s {availableAddressCount = a} :: PublicIPv4PoolRange)
{-# DEPRECATED piprAvailableAddressCount "Use generic-lens or generic-optics with 'availableAddressCount' instead." #-}

-- | The last IP address in the range.
--
-- /Note:/ Consider using 'lastAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprLastAddress :: Lens.Lens' PublicIPv4PoolRange (Lude.Maybe Lude.Text)
piprLastAddress = Lens.lens (lastAddress :: PublicIPv4PoolRange -> Lude.Maybe Lude.Text) (\s a -> s {lastAddress = a} :: PublicIPv4PoolRange)
{-# DEPRECATED piprLastAddress "Use generic-lens or generic-optics with 'lastAddress' instead." #-}

-- | The first IP address in the range.
--
-- /Note:/ Consider using 'firstAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprFirstAddress :: Lens.Lens' PublicIPv4PoolRange (Lude.Maybe Lude.Text)
piprFirstAddress = Lens.lens (firstAddress :: PublicIPv4PoolRange -> Lude.Maybe Lude.Text) (\s a -> s {firstAddress = a} :: PublicIPv4PoolRange)
{-# DEPRECATED piprFirstAddress "Use generic-lens or generic-optics with 'firstAddress' instead." #-}

-- | The number of addresses in the range.
--
-- /Note:/ Consider using 'addressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprAddressCount :: Lens.Lens' PublicIPv4PoolRange (Lude.Maybe Lude.Int)
piprAddressCount = Lens.lens (addressCount :: PublicIPv4PoolRange -> Lude.Maybe Lude.Int) (\s a -> s {addressCount = a} :: PublicIPv4PoolRange)
{-# DEPRECATED piprAddressCount "Use generic-lens or generic-optics with 'addressCount' instead." #-}

instance Lude.FromXML PublicIPv4PoolRange where
  parseXML x =
    PublicIPv4PoolRange'
      Lude.<$> (x Lude..@? "availableAddressCount")
      Lude.<*> (x Lude..@? "lastAddress")
      Lude.<*> (x Lude..@? "firstAddress")
      Lude.<*> (x Lude..@? "addressCount")
