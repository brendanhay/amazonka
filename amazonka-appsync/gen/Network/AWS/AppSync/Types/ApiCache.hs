{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.ApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.ApiCache where

import Network.AWS.AppSync.Types.ApiCacheStatus
import Network.AWS.AppSync.Types.ApiCacheType
import Network.AWS.AppSync.Types.ApiCachingBehavior
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The @ApiCache@ object.
--
-- /See:/ 'newApiCache' smart constructor.
data ApiCache = ApiCache'
  { -- | The cache instance status.
    --
    -- -   __AVAILABLE__: The instance is available for use.
    --
    -- -   __CREATING__: The instance is currently creating.
    --
    -- -   __DELETING__: The instance is currently deleting.
    --
    -- -   __MODIFYING__: The instance is currently modifying.
    --
    -- -   __FAILED__: The instance has failed creation.
    status :: Core.Maybe ApiCacheStatus,
    -- | At rest encryption flag for cache. This setting cannot be updated after
    -- creation.
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | TTL in seconds for cache entries.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Core.Maybe Core.Integer,
    -- | The cache instance type. Valid values are
    --
    -- -   @SMALL@
    --
    -- -   @MEDIUM@
    --
    -- -   @LARGE@
    --
    -- -   @XLARGE@
    --
    -- -   @LARGE_2X@
    --
    -- -   @LARGE_4X@
    --
    -- -   @LARGE_8X@ (not available in all regions)
    --
    -- -   @LARGE_12X@
    --
    -- Historically, instance types were identified by an EC2-style value. As
    -- of July 2020, this is deprecated, and the generic identifiers above
    -- should be used.
    --
    -- The following legacy instance types are available, but their use is
    -- discouraged:
    --
    -- -   __T2_SMALL__: A t2.small instance type.
    --
    -- -   __T2_MEDIUM__: A t2.medium instance type.
    --
    -- -   __R4_LARGE__: A r4.large instance type.
    --
    -- -   __R4_XLARGE__: A r4.xlarge instance type.
    --
    -- -   __R4_2XLARGE__: A r4.2xlarge instance type.
    --
    -- -   __R4_4XLARGE__: A r4.4xlarge instance type.
    --
    -- -   __R4_8XLARGE__: A r4.8xlarge instance type.
    type' :: Core.Maybe ApiCacheType,
    -- | Transit encryption flag when connecting to cache. This setting cannot be
    -- updated after creation.
    transitEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | Caching behavior.
    --
    -- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
    --
    -- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
    --     cached.
    apiCachingBehavior :: Core.Maybe ApiCachingBehavior
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'apiCache_status' - The cache instance status.
--
-- -   __AVAILABLE__: The instance is available for use.
--
-- -   __CREATING__: The instance is currently creating.
--
-- -   __DELETING__: The instance is currently deleting.
--
-- -   __MODIFYING__: The instance is currently modifying.
--
-- -   __FAILED__: The instance has failed creation.
--
-- 'atRestEncryptionEnabled', 'apiCache_atRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after
-- creation.
--
-- 'ttl', 'apiCache_ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- 'type'', 'apiCache_type' - The cache instance type. Valid values are
--
-- -   @SMALL@
--
-- -   @MEDIUM@
--
-- -   @LARGE@
--
-- -   @XLARGE@
--
-- -   @LARGE_2X@
--
-- -   @LARGE_4X@
--
-- -   @LARGE_8X@ (not available in all regions)
--
-- -   @LARGE_12X@
--
-- Historically, instance types were identified by an EC2-style value. As
-- of July 2020, this is deprecated, and the generic identifiers above
-- should be used.
--
-- The following legacy instance types are available, but their use is
-- discouraged:
--
-- -   __T2_SMALL__: A t2.small instance type.
--
-- -   __T2_MEDIUM__: A t2.medium instance type.
--
-- -   __R4_LARGE__: A r4.large instance type.
--
-- -   __R4_XLARGE__: A r4.xlarge instance type.
--
-- -   __R4_2XLARGE__: A r4.2xlarge instance type.
--
-- -   __R4_4XLARGE__: A r4.4xlarge instance type.
--
-- -   __R4_8XLARGE__: A r4.8xlarge instance type.
--
-- 'transitEncryptionEnabled', 'apiCache_transitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be
-- updated after creation.
--
-- 'apiCachingBehavior', 'apiCache_apiCachingBehavior' - Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
newApiCache ::
  ApiCache
newApiCache =
  ApiCache'
    { status = Core.Nothing,
      atRestEncryptionEnabled = Core.Nothing,
      ttl = Core.Nothing,
      type' = Core.Nothing,
      transitEncryptionEnabled = Core.Nothing,
      apiCachingBehavior = Core.Nothing
    }

-- | The cache instance status.
--
-- -   __AVAILABLE__: The instance is available for use.
--
-- -   __CREATING__: The instance is currently creating.
--
-- -   __DELETING__: The instance is currently deleting.
--
-- -   __MODIFYING__: The instance is currently modifying.
--
-- -   __FAILED__: The instance has failed creation.
apiCache_status :: Lens.Lens' ApiCache (Core.Maybe ApiCacheStatus)
apiCache_status = Lens.lens (\ApiCache' {status} -> status) (\s@ApiCache' {} a -> s {status = a} :: ApiCache)

-- | At rest encryption flag for cache. This setting cannot be updated after
-- creation.
apiCache_atRestEncryptionEnabled :: Lens.Lens' ApiCache (Core.Maybe Core.Bool)
apiCache_atRestEncryptionEnabled = Lens.lens (\ApiCache' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@ApiCache' {} a -> s {atRestEncryptionEnabled = a} :: ApiCache)

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
apiCache_ttl :: Lens.Lens' ApiCache (Core.Maybe Core.Integer)
apiCache_ttl = Lens.lens (\ApiCache' {ttl} -> ttl) (\s@ApiCache' {} a -> s {ttl = a} :: ApiCache)

-- | The cache instance type. Valid values are
--
-- -   @SMALL@
--
-- -   @MEDIUM@
--
-- -   @LARGE@
--
-- -   @XLARGE@
--
-- -   @LARGE_2X@
--
-- -   @LARGE_4X@
--
-- -   @LARGE_8X@ (not available in all regions)
--
-- -   @LARGE_12X@
--
-- Historically, instance types were identified by an EC2-style value. As
-- of July 2020, this is deprecated, and the generic identifiers above
-- should be used.
--
-- The following legacy instance types are available, but their use is
-- discouraged:
--
-- -   __T2_SMALL__: A t2.small instance type.
--
-- -   __T2_MEDIUM__: A t2.medium instance type.
--
-- -   __R4_LARGE__: A r4.large instance type.
--
-- -   __R4_XLARGE__: A r4.xlarge instance type.
--
-- -   __R4_2XLARGE__: A r4.2xlarge instance type.
--
-- -   __R4_4XLARGE__: A r4.4xlarge instance type.
--
-- -   __R4_8XLARGE__: A r4.8xlarge instance type.
apiCache_type :: Lens.Lens' ApiCache (Core.Maybe ApiCacheType)
apiCache_type = Lens.lens (\ApiCache' {type'} -> type') (\s@ApiCache' {} a -> s {type' = a} :: ApiCache)

-- | Transit encryption flag when connecting to cache. This setting cannot be
-- updated after creation.
apiCache_transitEncryptionEnabled :: Lens.Lens' ApiCache (Core.Maybe Core.Bool)
apiCache_transitEncryptionEnabled = Lens.lens (\ApiCache' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ApiCache' {} a -> s {transitEncryptionEnabled = a} :: ApiCache)

-- | Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
apiCache_apiCachingBehavior :: Lens.Lens' ApiCache (Core.Maybe ApiCachingBehavior)
apiCache_apiCachingBehavior = Lens.lens (\ApiCache' {apiCachingBehavior} -> apiCachingBehavior) (\s@ApiCache' {} a -> s {apiCachingBehavior = a} :: ApiCache)

instance Core.FromJSON ApiCache where
  parseJSON =
    Core.withObject
      "ApiCache"
      ( \x ->
          ApiCache'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "atRestEncryptionEnabled")
            Core.<*> (x Core..:? "ttl")
            Core.<*> (x Core..:? "type")
            Core.<*> (x Core..:? "transitEncryptionEnabled")
            Core.<*> (x Core..:? "apiCachingBehavior")
      )

instance Core.Hashable ApiCache

instance Core.NFData ApiCache
