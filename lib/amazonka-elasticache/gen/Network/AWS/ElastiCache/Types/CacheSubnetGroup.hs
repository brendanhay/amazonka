{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSubnetGroup
  ( CacheSubnetGroup (..),

    -- * Smart constructor
    mkCacheSubnetGroup,

    -- * Lenses
    cARN,
    cVPCId,
    cSubnets,
    cCacheSubnetGroupName,
    cCacheSubnetGroupDescription,
  )
where

import Network.AWS.ElastiCache.Types.Subnet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of one of the following operations:
--
--
--     * @CreateCacheSubnetGroup@
--
--
--     * @ModifyCacheSubnetGroup@
--
--
--
-- /See:/ 'mkCacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { arn ::
      Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    subnets :: Lude.Maybe [Subnet],
    cacheSubnetGroupName :: Lude.Maybe Lude.Text,
    cacheSubnetGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheSubnetGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN (Amazon Resource Name) of the cache subnet group.
-- * 'cacheSubnetGroupDescription' - The description of the cache subnet group.
-- * 'cacheSubnetGroupName' - The name of the cache subnet group.
-- * 'subnets' - A list of subnets associated with the cache subnet group.
-- * 'vpcId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
mkCacheSubnetGroup ::
  CacheSubnetGroup
mkCacheSubnetGroup =
  CacheSubnetGroup'
    { arn = Lude.Nothing,
      vpcId = Lude.Nothing,
      subnets = Lude.Nothing,
      cacheSubnetGroupName = Lude.Nothing,
      cacheSubnetGroupDescription = Lude.Nothing
    }

-- | The ARN (Amazon Resource Name) of the cache subnet group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cARN :: Lens.Lens' CacheSubnetGroup (Lude.Maybe Lude.Text)
cARN = Lens.lens (arn :: CacheSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CacheSubnetGroup)
{-# DEPRECATED cARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cVPCId :: Lens.Lens' CacheSubnetGroup (Lude.Maybe Lude.Text)
cVPCId = Lens.lens (vpcId :: CacheSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: CacheSubnetGroup)
{-# DEPRECATED cVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | A list of subnets associated with the cache subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubnets :: Lens.Lens' CacheSubnetGroup (Lude.Maybe [Subnet])
cSubnets = Lens.lens (subnets :: CacheSubnetGroup -> Lude.Maybe [Subnet]) (\s a -> s {subnets = a} :: CacheSubnetGroup)
{-# DEPRECATED cSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | The name of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheSubnetGroupName :: Lens.Lens' CacheSubnetGroup (Lude.Maybe Lude.Text)
cCacheSubnetGroupName = Lens.lens (cacheSubnetGroupName :: CacheSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupName = a} :: CacheSubnetGroup)
{-# DEPRECATED cCacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead." #-}

-- | The description of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCacheSubnetGroupDescription :: Lens.Lens' CacheSubnetGroup (Lude.Maybe Lude.Text)
cCacheSubnetGroupDescription = Lens.lens (cacheSubnetGroupDescription :: CacheSubnetGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheSubnetGroupDescription = a} :: CacheSubnetGroup)
{-# DEPRECATED cCacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead." #-}

instance Lude.FromXML CacheSubnetGroup where
  parseXML x =
    CacheSubnetGroup'
      Lude.<$> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> ( x Lude..@? "Subnets" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Subnet")
               )
      Lude.<*> (x Lude..@? "CacheSubnetGroupName")
      Lude.<*> (x Lude..@? "CacheSubnetGroupDescription")
