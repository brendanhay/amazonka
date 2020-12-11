-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.IPv6Pool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.IPv6Pool
  ( IPv6Pool (..),

    -- * Smart constructor
    mkIPv6Pool,

    -- * Lenses
    ipPoolCidrBlocks,
    ipPoolId,
    ipDescription,
    ipTags,
  )
where

import Network.AWS.EC2.Types.PoolCidrBlock
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an IPv6 address pool.
--
-- /See:/ 'mkIPv6Pool' smart constructor.
data IPv6Pool = IPv6Pool'
  { poolCidrBlocks ::
      Lude.Maybe [PoolCidrBlock],
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

-- | Creates a value of 'IPv6Pool' with the minimum fields required to make a request.
--
-- * 'description' - The description for the address pool.
-- * 'poolCidrBlocks' - The CIDR blocks for the address pool.
-- * 'poolId' - The ID of the address pool.
-- * 'tags' - Any tags for the address pool.
mkIPv6Pool ::
  IPv6Pool
mkIPv6Pool =
  IPv6Pool'
    { poolCidrBlocks = Lude.Nothing,
      poolId = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The CIDR blocks for the address pool.
--
-- /Note:/ Consider using 'poolCidrBlocks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPoolCidrBlocks :: Lens.Lens' IPv6Pool (Lude.Maybe [PoolCidrBlock])
ipPoolCidrBlocks = Lens.lens (poolCidrBlocks :: IPv6Pool -> Lude.Maybe [PoolCidrBlock]) (\s a -> s {poolCidrBlocks = a} :: IPv6Pool)
{-# DEPRECATED ipPoolCidrBlocks "Use generic-lens or generic-optics with 'poolCidrBlocks' instead." #-}

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipPoolId :: Lens.Lens' IPv6Pool (Lude.Maybe Lude.Text)
ipPoolId = Lens.lens (poolId :: IPv6Pool -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: IPv6Pool)
{-# DEPRECATED ipPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The description for the address pool.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipDescription :: Lens.Lens' IPv6Pool (Lude.Maybe Lude.Text)
ipDescription = Lens.lens (description :: IPv6Pool -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: IPv6Pool)
{-# DEPRECATED ipDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any tags for the address pool.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipTags :: Lens.Lens' IPv6Pool (Lude.Maybe [Tag])
ipTags = Lens.lens (tags :: IPv6Pool -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: IPv6Pool)
{-# DEPRECATED ipTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML IPv6Pool where
  parseXML x =
    IPv6Pool'
      Lude.<$> ( x Lude..@? "poolCidrBlockSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "poolId")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
