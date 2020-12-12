{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICache
  ( APICache (..),

    -- * Smart constructor
    mkAPICache,

    -- * Lenses
    acTtl,
    acStatus,
    acAtRestEncryptionEnabled,
    acTransitEncryptionEnabled,
    acApiCachingBehavior,
    acType,
  )
where

import Network.AWS.AppSync.Types.APICacheStatus
import Network.AWS.AppSync.Types.APICacheType
import Network.AWS.AppSync.Types.APICachingBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The @ApiCache@ object.
--
-- /See:/ 'mkAPICache' smart constructor.
data APICache = APICache'
  { ttl :: Lude.Maybe Lude.Integer,
    status :: Lude.Maybe APICacheStatus,
    atRestEncryptionEnabled :: Lude.Maybe Lude.Bool,
    transitEncryptionEnabled :: Lude.Maybe Lude.Bool,
    apiCachingBehavior :: Lude.Maybe APICachingBehavior,
    type' :: Lude.Maybe APICacheType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APICache' with the minimum fields required to make a request.
--
-- * 'apiCachingBehavior' - Caching behavior.
--
--
--     * __FULL_REQUEST_CACHING__ : All requests are fully cached.
--
--
--     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
--
-- * 'atRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after creation.
-- * 'status' - The cache instance status.
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
-- * 'transitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
-- * 'ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
-- * 'type'' - The cache instance type. Valid values are
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
mkAPICache ::
  APICache
mkAPICache =
  APICache'
    { ttl = Lude.Nothing,
      status = Lude.Nothing,
      atRestEncryptionEnabled = Lude.Nothing,
      transitEncryptionEnabled = Lude.Nothing,
      apiCachingBehavior = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- /Note:/ Consider using 'ttl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTtl :: Lens.Lens' APICache (Lude.Maybe Lude.Integer)
acTtl = Lens.lens (ttl :: APICache -> Lude.Maybe Lude.Integer) (\s a -> s {ttl = a} :: APICache)
{-# DEPRECATED acTtl "Use generic-lens or generic-optics with 'ttl' instead." #-}

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
acStatus :: Lens.Lens' APICache (Lude.Maybe APICacheStatus)
acStatus = Lens.lens (status :: APICache -> Lude.Maybe APICacheStatus) (\s a -> s {status = a} :: APICache)
{-# DEPRECATED acStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'atRestEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAtRestEncryptionEnabled :: Lens.Lens' APICache (Lude.Maybe Lude.Bool)
acAtRestEncryptionEnabled = Lens.lens (atRestEncryptionEnabled :: APICache -> Lude.Maybe Lude.Bool) (\s a -> s {atRestEncryptionEnabled = a} :: APICache)
{-# DEPRECATED acAtRestEncryptionEnabled "Use generic-lens or generic-optics with 'atRestEncryptionEnabled' instead." #-}

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- /Note:/ Consider using 'transitEncryptionEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acTransitEncryptionEnabled :: Lens.Lens' APICache (Lude.Maybe Lude.Bool)
acTransitEncryptionEnabled = Lens.lens (transitEncryptionEnabled :: APICache -> Lude.Maybe Lude.Bool) (\s a -> s {transitEncryptionEnabled = a} :: APICache)
{-# DEPRECATED acTransitEncryptionEnabled "Use generic-lens or generic-optics with 'transitEncryptionEnabled' instead." #-}

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
acApiCachingBehavior :: Lens.Lens' APICache (Lude.Maybe APICachingBehavior)
acApiCachingBehavior = Lens.lens (apiCachingBehavior :: APICache -> Lude.Maybe APICachingBehavior) (\s a -> s {apiCachingBehavior = a} :: APICache)
{-# DEPRECATED acApiCachingBehavior "Use generic-lens or generic-optics with 'apiCachingBehavior' instead." #-}

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
acType :: Lens.Lens' APICache (Lude.Maybe APICacheType)
acType = Lens.lens (type' :: APICache -> Lude.Maybe APICacheType) (\s a -> s {type' = a} :: APICache)
{-# DEPRECATED acType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON APICache where
  parseJSON =
    Lude.withObject
      "APICache"
      ( \x ->
          APICache'
            Lude.<$> (x Lude..:? "ttl")
            Lude.<*> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "atRestEncryptionEnabled")
            Lude.<*> (x Lude..:? "transitEncryptionEnabled")
            Lude.<*> (x Lude..:? "apiCachingBehavior")
            Lude.<*> (x Lude..:? "type")
      )
