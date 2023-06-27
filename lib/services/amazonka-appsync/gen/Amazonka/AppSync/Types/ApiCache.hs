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
-- Module      : Amazonka.AppSync.Types.ApiCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.ApiCache where

import Amazonka.AppSync.Types.ApiCacheStatus
import Amazonka.AppSync.Types.ApiCacheType
import Amazonka.AppSync.Types.ApiCachingBehavior
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The @ApiCache@ object.
--
-- /See:/ 'newApiCache' smart constructor.
data ApiCache = ApiCache'
  { -- | Caching behavior.
    --
    -- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
    --
    -- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
    --     cached.
    apiCachingBehavior :: Prelude.Maybe ApiCachingBehavior,
    -- | At-rest encryption flag for cache. You cannot update this setting after
    -- creation.
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
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
    status :: Prelude.Maybe ApiCacheStatus,
    -- | Transit encryption flag when connecting to cache. You cannot update this
    -- setting after creation.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | TTL in seconds for cache entries.
    --
    -- Valid values are 1–3,600 seconds.
    ttl :: Prelude.Maybe Prelude.Integer,
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
    type' :: Prelude.Maybe ApiCacheType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCachingBehavior', 'apiCache_apiCachingBehavior' - Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
--
-- 'atRestEncryptionEnabled', 'apiCache_atRestEncryptionEnabled' - At-rest encryption flag for cache. You cannot update this setting after
-- creation.
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
-- 'transitEncryptionEnabled', 'apiCache_transitEncryptionEnabled' - Transit encryption flag when connecting to cache. You cannot update this
-- setting after creation.
--
-- 'ttl', 'apiCache_ttl' - TTL in seconds for cache entries.
--
-- Valid values are 1–3,600 seconds.
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
newApiCache ::
  ApiCache
newApiCache =
  ApiCache'
    { apiCachingBehavior = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      status = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing,
      ttl = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
apiCache_apiCachingBehavior :: Lens.Lens' ApiCache (Prelude.Maybe ApiCachingBehavior)
apiCache_apiCachingBehavior = Lens.lens (\ApiCache' {apiCachingBehavior} -> apiCachingBehavior) (\s@ApiCache' {} a -> s {apiCachingBehavior = a} :: ApiCache)

-- | At-rest encryption flag for cache. You cannot update this setting after
-- creation.
apiCache_atRestEncryptionEnabled :: Lens.Lens' ApiCache (Prelude.Maybe Prelude.Bool)
apiCache_atRestEncryptionEnabled = Lens.lens (\ApiCache' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@ApiCache' {} a -> s {atRestEncryptionEnabled = a} :: ApiCache)

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
apiCache_status :: Lens.Lens' ApiCache (Prelude.Maybe ApiCacheStatus)
apiCache_status = Lens.lens (\ApiCache' {status} -> status) (\s@ApiCache' {} a -> s {status = a} :: ApiCache)

-- | Transit encryption flag when connecting to cache. You cannot update this
-- setting after creation.
apiCache_transitEncryptionEnabled :: Lens.Lens' ApiCache (Prelude.Maybe Prelude.Bool)
apiCache_transitEncryptionEnabled = Lens.lens (\ApiCache' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@ApiCache' {} a -> s {transitEncryptionEnabled = a} :: ApiCache)

-- | TTL in seconds for cache entries.
--
-- Valid values are 1–3,600 seconds.
apiCache_ttl :: Lens.Lens' ApiCache (Prelude.Maybe Prelude.Integer)
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
apiCache_type :: Lens.Lens' ApiCache (Prelude.Maybe ApiCacheType)
apiCache_type = Lens.lens (\ApiCache' {type'} -> type') (\s@ApiCache' {} a -> s {type' = a} :: ApiCache)

instance Data.FromJSON ApiCache where
  parseJSON =
    Data.withObject
      "ApiCache"
      ( \x ->
          ApiCache'
            Prelude.<$> (x Data..:? "apiCachingBehavior")
            Prelude.<*> (x Data..:? "atRestEncryptionEnabled")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "transitEncryptionEnabled")
            Prelude.<*> (x Data..:? "ttl")
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable ApiCache where
  hashWithSalt _salt ApiCache' {..} =
    _salt
      `Prelude.hashWithSalt` apiCachingBehavior
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ApiCache where
  rnf ApiCache' {..} =
    Prelude.rnf apiCachingBehavior
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf type'
