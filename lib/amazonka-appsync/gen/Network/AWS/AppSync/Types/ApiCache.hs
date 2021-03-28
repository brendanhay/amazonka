{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ApiCache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.ApiCache
  ( ApiCache (..)
  -- * Smart constructor
  , mkApiCache
  -- * Lenses
  , acApiCachingBehavior
  , acAtRestEncryptionEnabled
  , acStatus
  , acTransitEncryptionEnabled
  , acTtl
  , acType
  ) where

import qualified Network.AWS.AppSync.Types.ApiCacheStatus as Types
import qualified Network.AWS.AppSync.Types.ApiCacheType as Types
import qualified Network.AWS.AppSync.Types.ApiCachingBehavior as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @ApiCache@ object.
--
-- /See:/ 'mkApiCache' smart constructor.
data ApiCache = ApiCache'
  { apiCachingBehavior :: Core.Maybe Types.ApiCachingBehavior
    -- ^ Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
  , atRestEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ At rest encryption flag for cache. This setting cannot be updated after creation.
  , status :: Core.Maybe Types.ApiCacheStatus
    -- ^ The cache instance status.
--
--
--     * __AVAILABLE__ : The instance is available for use.
--
--
--     * __CREATING__ : The instance is currently creating.
--
--
--     * __DELETING__ : The instance is currently deleting.
--
--
--     * __MODIFYING__ : The instance is currently modifying.
--
--
--     * __FAILED__ : The instance has failed creation.
--
--
  , transitEncryptionEnabled :: Core.Maybe Core.Bool
    -- ^ Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
  , ttl :: Core.Maybe Core.Integer
    -- ^ TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
  , type' :: Core.Maybe Types.ApiCacheType
    -- ^ The cache instance type. Valid values are 
--
--
--     * @SMALL@ 
--
--
--     * @MEDIUM@ 
--
--
--     * @LARGE@ 
--
--
--     * @XLARGE@ 
--
--
--     * @LARGE_2X@ 
--
--
--     * @LARGE_4X@ 
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@ 
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApiCache' value with any optional fields omitted.
mkApiCache
    :: ApiCache
mkApiCache
  = ApiCache'{apiCachingBehavior = Core.Nothing,
              atRestEncryptionEnabled = Core.Nothing, status = Core.Nothing,
              transitEncryptionEnabled = Core.Nothing, ttl = Core.Nothing,
              type' = Core.Nothing}

-- | Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
--
-- /Note:/ Consider using 'apiCachingBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acApiCachingBehavior :: Lens.Lens' ApiCache (Core.Maybe Types.ApiCachingBehavior)
acApiCachingBehavior = Lens.field @"apiCachingBehavior"
{-# INLINEABLE acApiCachingBehavior #-}
{-# DEPRECATED apiCachingBehavior "Use generic-lens or generic-optics with 'apiCachingBehavior' instead"  #-}

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAtRestEncryptionEnabled :: Lens.Lens' ApiCache (Core.Maybe Core.Bool)
acAtRestEncryptionEnabled = Lens.field @"atRestEncryptionEnabled"
{-# INLINEABLE acAtRestEncryptionEnabled #-}
{-# DEPRECATED atRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead"  #-}

-- | The cache instance status.
--
--
--     * __AVAILABLE__ : The instance is available for use.
--
--
--     * __CREATING__ : The instance is currently creating.
--
--
--     * __DELETING__ : The instance is currently deleting.
--
--
--     * __MODIFYING__ : The instance is currently modifying.
--
--
--     * __FAILED__ : The instance has failed creation.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acStatus :: Lens.Lens' ApiCache (Core.Maybe Types.ApiCacheStatus)
acStatus = Lens.field @"status"
{-# INLINEABLE acStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTransitEncryptionEnabled :: Lens.Lens' ApiCache (Core.Maybe Core.Bool)
acTransitEncryptionEnabled = Lens.field @"transitEncryptionEnabled"
{-# INLINEABLE acTransitEncryptionEnabled #-}
{-# DEPRECATED transitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead"  #-}

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTtl :: Lens.Lens' ApiCache (Core.Maybe Core.Integer)
acTtl = Lens.field @"ttl"
{-# INLINEABLE acTtl #-}
{-# DEPRECATED ttl "Use generic-lens or generic-optics with 'ttl' instead"  #-}

-- | The cache instance type. Valid values are 
--
--
--     * @SMALL@ 
--
--
--     * @MEDIUM@ 
--
--
--     * @LARGE@ 
--
--
--     * @XLARGE@ 
--
--
--     * @LARGE_2X@ 
--
--
--     * @LARGE_4X@ 
--
--
--     * @LARGE_8X@ (not available in all regions)
--
--
--     * @LARGE_12X@ 
--
--
-- Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used.
-- The following legacy instance types are available, but their use is discouraged:
--
--     * __T2_SMALL__ : A t2.small instance type.
--
--
--     * __T2_MEDIUM__ : A t2.medium instance type.
--
--
--     * __R4_LARGE__ : A r4.large instance type.
--
--
--     * __R4_XLARGE__ : A r4.xlarge instance type.
--
--
--     * __R4_2XLARGE__ : A r4.2xlarge instance type.
--
--
--     * __R4_4XLARGE__ : A r4.4xlarge instance type.
--
--
--     * __R4_8XLARGE__ : A r4.8xlarge instance type.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acType :: Lens.Lens' ApiCache (Core.Maybe Types.ApiCacheType)
acType = Lens.field @"type'"
{-# INLINEABLE acType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ApiCache where
        parseJSON
          = Core.withObject "ApiCache" Core.$
              \ x ->
                ApiCache' Core.<$>
                  (x Core..:? "apiCachingBehavior") Core.<*>
                    x Core..:? "atRestEncryptionEnabled"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "transitEncryptionEnabled"
                    Core.<*> x Core..:? "ttl"
                    Core.<*> x Core..:? "type"
