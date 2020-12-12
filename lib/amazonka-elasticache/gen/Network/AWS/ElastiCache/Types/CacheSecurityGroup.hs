{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSecurityGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSecurityGroup
  ( CacheSecurityGroup (..),

    -- * Smart constructor
    mkCacheSecurityGroup,

    -- * Lenses
    csgCacheSecurityGroupName,
    csgARN,
    csgOwnerId,
    csgEC2SecurityGroups,
    csgDescription,
  )
where

import Network.AWS.ElastiCache.Types.EC2SecurityGroup
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of one of the following operations:
--
--
--     * @AuthorizeCacheSecurityGroupIngress@
--
--
--     * @CreateCacheSecurityGroup@
--
--
--     * @RevokeCacheSecurityGroupIngress@
--
--
--
-- /See:/ 'mkCacheSecurityGroup' smart constructor.
data CacheSecurityGroup = CacheSecurityGroup'
  { cacheSecurityGroupName ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    ownerId :: Lude.Maybe Lude.Text,
    ec2SecurityGroups :: Lude.Maybe [EC2SecurityGroup],
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheSecurityGroup' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the cache security group,
-- * 'cacheSecurityGroupName' - The name of the cache security group.
-- * 'description' - The description of the cache security group.
-- * 'ec2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache security group.
-- * 'ownerId' - The AWS account ID of the cache security group owner.
mkCacheSecurityGroup ::
  CacheSecurityGroup
mkCacheSecurityGroup =
  CacheSecurityGroup'
    { cacheSecurityGroupName = Lude.Nothing,
      arn = Lude.Nothing,
      ownerId = Lude.Nothing,
      ec2SecurityGroups = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The name of the cache security group.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgCacheSecurityGroupName :: Lens.Lens' CacheSecurityGroup (Lude.Maybe Lude.Text)
csgCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: CacheSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: CacheSecurityGroup)
{-# DEPRECATED csgCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The ARN of the cache security group,
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgARN :: Lens.Lens' CacheSecurityGroup (Lude.Maybe Lude.Text)
csgARN = Lens.lens (arn :: CacheSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: CacheSecurityGroup)
{-# DEPRECATED csgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The AWS account ID of the cache security group owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgOwnerId :: Lens.Lens' CacheSecurityGroup (Lude.Maybe Lude.Text)
csgOwnerId = Lens.lens (ownerId :: CacheSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: CacheSecurityGroup)
{-# DEPRECATED csgOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | A list of Amazon EC2 security groups that are associated with this cache security group.
--
-- /Note:/ Consider using 'ec2SecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgEC2SecurityGroups :: Lens.Lens' CacheSecurityGroup (Lude.Maybe [EC2SecurityGroup])
csgEC2SecurityGroups = Lens.lens (ec2SecurityGroups :: CacheSecurityGroup -> Lude.Maybe [EC2SecurityGroup]) (\s a -> s {ec2SecurityGroups = a} :: CacheSecurityGroup)
{-# DEPRECATED csgEC2SecurityGroups "Use generic-lens or generic-optics with 'ec2SecurityGroups' instead." #-}

-- | The description of the cache security group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgDescription :: Lens.Lens' CacheSecurityGroup (Lude.Maybe Lude.Text)
csgDescription = Lens.lens (description :: CacheSecurityGroup -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CacheSecurityGroup)
{-# DEPRECATED csgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML CacheSecurityGroup where
  parseXML x =
    CacheSecurityGroup'
      Lude.<$> (x Lude..@? "CacheSecurityGroupName")
      Lude.<*> (x Lude..@? "ARN")
      Lude.<*> (x Lude..@? "OwnerId")
      Lude.<*> ( x Lude..@? "EC2SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "EC2SecurityGroup")
               )
      Lude.<*> (x Lude..@? "Description")
