{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheSecurityGroupMembership
  ( CacheSecurityGroupMembership (..),

    -- * Smart constructor
    mkCacheSecurityGroupMembership,

    -- * Lenses
    csgmCacheSecurityGroupName,
    csgmStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a cluster's status within a particular cache security group.
--
-- /See:/ 'mkCacheSecurityGroupMembership' smart constructor.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership'
  { -- | The name of the cache security group.
    cacheSecurityGroupName :: Core.Maybe Types.String,
    -- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
    status :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CacheSecurityGroupMembership' value with any optional fields omitted.
mkCacheSecurityGroupMembership ::
  CacheSecurityGroupMembership
mkCacheSecurityGroupMembership =
  CacheSecurityGroupMembership'
    { cacheSecurityGroupName =
        Core.Nothing,
      status = Core.Nothing
    }

-- | The name of the cache security group.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmCacheSecurityGroupName :: Lens.Lens' CacheSecurityGroupMembership (Core.Maybe Types.String)
csgmCacheSecurityGroupName = Lens.field @"cacheSecurityGroupName"
{-# DEPRECATED csgmCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

-- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmStatus :: Lens.Lens' CacheSecurityGroupMembership (Core.Maybe Types.String)
csgmStatus = Lens.field @"status"
{-# DEPRECATED csgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromXML CacheSecurityGroupMembership where
  parseXML x =
    CacheSecurityGroupMembership'
      Core.<$> (x Core..@? "CacheSecurityGroupName")
      Core.<*> (x Core..@? "Status")
