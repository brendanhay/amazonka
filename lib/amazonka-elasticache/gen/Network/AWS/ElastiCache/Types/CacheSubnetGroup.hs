{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSubnetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.CacheSubnetGroup
  ( CacheSubnetGroup (..)
  -- * Smart constructor
  , mkCacheSubnetGroup
  -- * Lenses
  , csgARN
  , csgCacheSubnetGroupDescription
  , csgCacheSubnetGroupName
  , csgSubnets
  , csgVpcId
  ) where

import qualified Network.AWS.ElastiCache.Types.Subnet as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN (Amazon Resource Name) of the cache subnet group.
  , cacheSubnetGroupDescription :: Core.Maybe Core.Text
    -- ^ The description of the cache subnet group.
  , cacheSubnetGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cache subnet group.
  , subnets :: Core.Maybe [Types.Subnet]
    -- ^ A list of subnets associated with the cache subnet group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheSubnetGroup' value with any optional fields omitted.
mkCacheSubnetGroup
    :: CacheSubnetGroup
mkCacheSubnetGroup
  = CacheSubnetGroup'{arn = Core.Nothing,
                      cacheSubnetGroupDescription = Core.Nothing,
                      cacheSubnetGroupName = Core.Nothing, subnets = Core.Nothing,
                      vpcId = Core.Nothing}

-- | The ARN (Amazon Resource Name) of the cache subnet group.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgARN :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
csgARN = Lens.field @"arn"
{-# INLINEABLE csgARN #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The description of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgCacheSubnetGroupDescription :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
csgCacheSubnetGroupDescription = Lens.field @"cacheSubnetGroupDescription"
{-# INLINEABLE csgCacheSubnetGroupDescription #-}
{-# DEPRECATED cacheSubnetGroupDescription "Use generic-lens or generic-optics with 'cacheSubnetGroupDescription' instead"  #-}

-- | The name of the cache subnet group.
--
-- /Note:/ Consider using 'cacheSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgCacheSubnetGroupName :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
csgCacheSubnetGroupName = Lens.field @"cacheSubnetGroupName"
{-# INLINEABLE csgCacheSubnetGroupName #-}
{-# DEPRECATED cacheSubnetGroupName "Use generic-lens or generic-optics with 'cacheSubnetGroupName' instead"  #-}

-- | A list of subnets associated with the cache subnet group.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgSubnets :: Lens.Lens' CacheSubnetGroup (Core.Maybe [Types.Subnet])
csgSubnets = Lens.field @"subnets"
{-# INLINEABLE csgSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgVpcId :: Lens.Lens' CacheSubnetGroup (Core.Maybe Core.Text)
csgVpcId = Lens.field @"vpcId"
{-# INLINEABLE csgVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML CacheSubnetGroup where
        parseXML x
          = CacheSubnetGroup' Core.<$>
              (x Core..@? "ARN") Core.<*>
                x Core..@? "CacheSubnetGroupDescription"
                Core.<*> x Core..@? "CacheSubnetGroupName"
                Core.<*> x Core..@? "Subnets" Core..<@> Core.parseXMLList "Subnet"
                Core.<*> x Core..@? "VpcId"
