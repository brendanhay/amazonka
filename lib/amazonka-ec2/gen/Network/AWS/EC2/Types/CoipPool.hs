{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CoipPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CoipPool
  ( CoipPool (..),

    -- * Smart constructor
    mkCoipPool,

    -- * Lenses
    cpPoolId,
    cpLocalGatewayRouteTableId,
    cpPoolCidrs,
    cpTags,
    cpPoolARN,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a customer-owned address pool.
--
-- /See:/ 'mkCoipPool' smart constructor.
data CoipPool = CoipPool'
  { -- | The ID of the address pool.
    poolId :: Lude.Maybe Lude.Text,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Lude.Maybe Lude.Text,
    -- | The address ranges of the address pool.
    poolCidrs :: Lude.Maybe [Lude.Text],
    -- | The tags.
    tags :: Lude.Maybe [Tag],
    -- | The ARN of the address pool.
    poolARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CoipPool' with the minimum fields required to make a request.
--
-- * 'poolId' - The ID of the address pool.
-- * 'localGatewayRouteTableId' - The ID of the local gateway route table.
-- * 'poolCidrs' - The address ranges of the address pool.
-- * 'tags' - The tags.
-- * 'poolARN' - The ARN of the address pool.
mkCoipPool ::
  CoipPool
mkCoipPool =
  CoipPool'
    { poolId = Lude.Nothing,
      localGatewayRouteTableId = Lude.Nothing,
      poolCidrs = Lude.Nothing,
      tags = Lude.Nothing,
      poolARN = Lude.Nothing
    }

-- | The ID of the address pool.
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolId :: Lens.Lens' CoipPool (Lude.Maybe Lude.Text)
cpPoolId = Lens.lens (poolId :: CoipPool -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: CoipPool)
{-# DEPRECATED cpPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The ID of the local gateway route table.
--
-- /Note:/ Consider using 'localGatewayRouteTableId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpLocalGatewayRouteTableId :: Lens.Lens' CoipPool (Lude.Maybe Lude.Text)
cpLocalGatewayRouteTableId = Lens.lens (localGatewayRouteTableId :: CoipPool -> Lude.Maybe Lude.Text) (\s a -> s {localGatewayRouteTableId = a} :: CoipPool)
{-# DEPRECATED cpLocalGatewayRouteTableId "Use generic-lens or generic-optics with 'localGatewayRouteTableId' instead." #-}

-- | The address ranges of the address pool.
--
-- /Note:/ Consider using 'poolCidrs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolCidrs :: Lens.Lens' CoipPool (Lude.Maybe [Lude.Text])
cpPoolCidrs = Lens.lens (poolCidrs :: CoipPool -> Lude.Maybe [Lude.Text]) (\s a -> s {poolCidrs = a} :: CoipPool)
{-# DEPRECATED cpPoolCidrs "Use generic-lens or generic-optics with 'poolCidrs' instead." #-}

-- | The tags.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpTags :: Lens.Lens' CoipPool (Lude.Maybe [Tag])
cpTags = Lens.lens (tags :: CoipPool -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CoipPool)
{-# DEPRECATED cpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the address pool.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpPoolARN :: Lens.Lens' CoipPool (Lude.Maybe Lude.Text)
cpPoolARN = Lens.lens (poolARN :: CoipPool -> Lude.Maybe Lude.Text) (\s a -> s {poolARN = a} :: CoipPool)
{-# DEPRECATED cpPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

instance Lude.FromXML CoipPool where
  parseXML x =
    CoipPool'
      Lude.<$> (x Lude..@? "poolId")
      Lude.<*> (x Lude..@? "localGatewayRouteTableId")
      Lude.<*> ( x Lude..@? "poolCidrSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "poolArn")
