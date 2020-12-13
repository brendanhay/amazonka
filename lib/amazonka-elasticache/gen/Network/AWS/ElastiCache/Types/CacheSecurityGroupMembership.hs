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
    csgmStatus,
    csgmCacheSecurityGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a cluster's status within a particular cache security group.
--
-- /See:/ 'mkCacheSecurityGroupMembership' smart constructor.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership'
  { -- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
    status :: Lude.Maybe Lude.Text,
    -- | The name of the cache security group.
    cacheSecurityGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CacheSecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'status' - The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
-- * 'cacheSecurityGroupName' - The name of the cache security group.
mkCacheSecurityGroupMembership ::
  CacheSecurityGroupMembership
mkCacheSecurityGroupMembership =
  CacheSecurityGroupMembership'
    { status = Lude.Nothing,
      cacheSecurityGroupName = Lude.Nothing
    }

-- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmStatus :: Lens.Lens' CacheSecurityGroupMembership (Lude.Maybe Lude.Text)
csgmStatus = Lens.lens (status :: CacheSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: CacheSecurityGroupMembership)
{-# DEPRECATED csgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the cache security group.
--
-- /Note:/ Consider using 'cacheSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmCacheSecurityGroupName :: Lens.Lens' CacheSecurityGroupMembership (Lude.Maybe Lude.Text)
csgmCacheSecurityGroupName = Lens.lens (cacheSecurityGroupName :: CacheSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {cacheSecurityGroupName = a} :: CacheSecurityGroupMembership)
{-# DEPRECATED csgmCacheSecurityGroupName "Use generic-lens or generic-optics with 'cacheSecurityGroupName' instead." #-}

instance Lude.FromXML CacheSecurityGroupMembership where
  parseXML x =
    CacheSecurityGroupMembership'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "CacheSecurityGroupName")
