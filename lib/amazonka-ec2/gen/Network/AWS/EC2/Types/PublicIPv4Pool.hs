-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.PublicIPv4Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PublicIPv4Pool
  ( PublicIPv4Pool (..),

    -- * Smart constructor
    mkPublicIPv4Pool,

    -- * Lenses
    pipTotalAddressCount,
    pipNetworkBorderGroup,
    pipTotalAvailableAddressCount,
    pipPoolAddressRanges,
    pipPoolId,
    pipDescription,
    pipTags,
  )
where

import Network.AWS.EC2.Types.PublicIPv4PoolRange
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv4 address pool.
--
-- /See:/ 'mkPublicIPv4Pool' smart constructor.
data PublicIPv4Pool = PublicIPv4Pool'
  { totalAddressCount ::
      Lude.Maybe Lude.Int,
    networkBorderGroup :: Lude.Maybe Lude.Text,
    totalAvailableAddressCount :: Lude.Maybe Lude.Int,
    poolAddressRanges :: Lude.Maybe [PublicIPv4PoolRange],
    poolId :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PublicIPv4Pool' with the minimum fields required to make a request.
--
-- * 'description' - A description of the address pool.
-- * 'networkBorderGroup' - The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
-- * 'poolAddressRanges' - The address ranges.
-- * 'poolId' - The ID of the address pool.
-- * 'tags' - Any tags for the address pool.
-- * 'totalAddressCount' - The total number of addresses.
-- * 'totalAvailableAddressCount' - The total number of available addresses.
mkPublicIPv4Pool ::
  PublicIPv4Pool
mkPublicIPv4Pool =
  PublicIPv4Pool'
    { totalAddressCount = Lude.Nothing,
      networkBorderGroup = Lude.Nothing,
      totalAvailableAddressCount = Lude.Nothing,
      poolAddressRanges = Lude.Nothing,
      poolId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The total number of addresses.
--
-- /Note:/ Consider using 'totalAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTotalAddressCount :: Lens.Lens' PublicIPv4Pool (Lude.Maybe Lude.Int)
pipTotalAddressCount = Lens.lens (totalAddressCount :: PublicIPv4Pool -> Lude.Maybe Lude.Int) (\s a -> s {totalAddressCount = a} :: PublicIPv4Pool)
{-# DEPRECATED pipTotalAddressCount "Use generic-lens or generic-optics with 'totalAddressCount' instead." #-}

-- | The name of the location from which the address pool is advertised. A network border group is a unique set of Availability Zones or Local Zones from where AWS advertises public IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipNetworkBorderGroup :: Lens.Lens' PublicIPv4Pool (Lude.Maybe Lude.Text)
pipNetworkBorderGroup = Lens.lens (networkBorderGroup :: PublicIPv4Pool -> Lude.Maybe Lude.Text) (\s a -> s {networkBorderGroup = a} :: PublicIPv4Pool)
{-# DEPRECATED pipNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | The total number of available addresses.
--
-- /Note:/ Consider using 'totalAvailableAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTotalAvailableAddressCount :: Lens.Lens' PublicIPv4Pool (Lude.Maybe Lude.Int)
pipTotalAvailableAddressCount = Lens.lens (totalAvailableAddressCount :: PublicIPv4Pool -> Lude.Maybe Lude.Int) (\s a -> s {totalAvailableAddressCount = a} :: PublicIPv4Pool)
{-# DEPRECATED pipTotalAvailableAddressCount "Use generic-lens or generic-optics with 'totalAvailableAddressCount' instead." #-}

-- | The address ranges.
--
-- /Note:/ Consider using 'poolAddressRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPoolAddressRanges :: Lens.Lens' PublicIPv4Pool (Lude.Maybe [PublicIPv4PoolRange])
pipPoolAddressRanges = Lens.lens (poolAddressRanges :: PublicIPv4Pool -> Lude.Maybe [PublicIPv4PoolRange]) (\s a -> s {poolAddressRanges = a} :: PublicIPv4Pool)
{-# DEPRECATED pipPoolAddressRanges "Use generic-lens or generic-optics with 'poolAddressRanges' instead." #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPoolId :: Lens.Lens' PublicIPv4Pool (Lude.Maybe Lude.Text)
pipPoolId = Lens.lens (poolId :: PublicIPv4Pool -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: PublicIPv4Pool)
{-# DEPRECATED pipPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | A description of the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipDescription :: Lens.Lens' PublicIPv4Pool (Lude.Maybe Lude.Text)
pipDescription = Lens.lens (description :: PublicIPv4Pool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: PublicIPv4Pool)
{-# DEPRECATED pipDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags for the address pool.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipTags :: Lens.Lens' PublicIPv4Pool (Lude.Maybe [Tag])
pipTags = Lens.lens (tags :: PublicIPv4Pool -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: PublicIPv4Pool)
{-# DEPRECATED pipTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML PublicIPv4Pool where
  parseXML x =
    PublicIPv4Pool'
      Lude.<$> (x Lude..@? "totalAddressCount")
      Lude.<*> (x Lude..@? "networkBorderGroup")
      Lude.<*> (x Lude..@? "totalAvailableAddressCount")
      Lude.<*> ( x Lude..@? "poolAddressRangeSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "poolId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
