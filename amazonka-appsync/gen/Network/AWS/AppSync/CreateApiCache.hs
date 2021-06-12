{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.CreateApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache for the GraphQL API.
module Network.AWS.AppSync.CreateApiCache
  ( -- * Creating a Request
    CreateApiCache (..),
    newCreateApiCache,

    -- * Request Lenses
    createApiCache_atRestEncryptionEnabled,
    createApiCache_transitEncryptionEnabled,
    createApiCache_apiId,
    createApiCache_ttl,
    createApiCache_apiCachingBehavior,
    createApiCache_type,

    -- * Destructuring the Response
    CreateApiCacheResponse (..),
    newCreateApiCacheResponse,

    -- * Response Lenses
    createApiCacheResponse_apiCache,
    createApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateApiCache@ operation.
--
-- /See:/ 'newCreateApiCache' smart constructor.
data CreateApiCache = CreateApiCache'
  { -- | At rest encryption flag for cache. This setting cannot be updated after
    -- creation.
    atRestEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | Transit encryption flag when connecting to cache. This setting cannot be
    -- updated after creation.
    transitEncryptionEnabled :: Core.Maybe Core.Bool,
    -- | The GraphQL API Id.
    apiId :: Core.Text,
    -- | TTL in seconds for cache entries.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Core.Integer,
    -- | Caching behavior.
    --
    -- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
    --
    -- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
    --     cached.
    apiCachingBehavior :: ApiCachingBehavior,
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
    type' :: ApiCacheType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'atRestEncryptionEnabled', 'createApiCache_atRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after
-- creation.
--
-- 'transitEncryptionEnabled', 'createApiCache_transitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be
-- updated after creation.
--
-- 'apiId', 'createApiCache_apiId' - The GraphQL API Id.
--
-- 'ttl', 'createApiCache_ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- 'apiCachingBehavior', 'createApiCache_apiCachingBehavior' - Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
--
-- 'type'', 'createApiCache_type' - The cache instance type. Valid values are
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
newCreateApiCache ::
  -- | 'apiId'
  Core.Text ->
  -- | 'ttl'
  Core.Integer ->
  -- | 'apiCachingBehavior'
  ApiCachingBehavior ->
  -- | 'type''
  ApiCacheType ->
  CreateApiCache
newCreateApiCache
  pApiId_
  pTtl_
  pApiCachingBehavior_
  pType_ =
    CreateApiCache'
      { atRestEncryptionEnabled =
          Core.Nothing,
        transitEncryptionEnabled = Core.Nothing,
        apiId = pApiId_,
        ttl = pTtl_,
        apiCachingBehavior = pApiCachingBehavior_,
        type' = pType_
      }

-- | At rest encryption flag for cache. This setting cannot be updated after
-- creation.
createApiCache_atRestEncryptionEnabled :: Lens.Lens' CreateApiCache (Core.Maybe Core.Bool)
createApiCache_atRestEncryptionEnabled = Lens.lens (\CreateApiCache' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@CreateApiCache' {} a -> s {atRestEncryptionEnabled = a} :: CreateApiCache)

-- | Transit encryption flag when connecting to cache. This setting cannot be
-- updated after creation.
createApiCache_transitEncryptionEnabled :: Lens.Lens' CreateApiCache (Core.Maybe Core.Bool)
createApiCache_transitEncryptionEnabled = Lens.lens (\CreateApiCache' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@CreateApiCache' {} a -> s {transitEncryptionEnabled = a} :: CreateApiCache)

-- | The GraphQL API Id.
createApiCache_apiId :: Lens.Lens' CreateApiCache Core.Text
createApiCache_apiId = Lens.lens (\CreateApiCache' {apiId} -> apiId) (\s@CreateApiCache' {} a -> s {apiId = a} :: CreateApiCache)

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
createApiCache_ttl :: Lens.Lens' CreateApiCache Core.Integer
createApiCache_ttl = Lens.lens (\CreateApiCache' {ttl} -> ttl) (\s@CreateApiCache' {} a -> s {ttl = a} :: CreateApiCache)

-- | Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
createApiCache_apiCachingBehavior :: Lens.Lens' CreateApiCache ApiCachingBehavior
createApiCache_apiCachingBehavior = Lens.lens (\CreateApiCache' {apiCachingBehavior} -> apiCachingBehavior) (\s@CreateApiCache' {} a -> s {apiCachingBehavior = a} :: CreateApiCache)

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
createApiCache_type :: Lens.Lens' CreateApiCache ApiCacheType
createApiCache_type = Lens.lens (\CreateApiCache' {type'} -> type') (\s@CreateApiCache' {} a -> s {type' = a} :: CreateApiCache)

instance Core.AWSRequest CreateApiCache where
  type
    AWSResponse CreateApiCache =
      CreateApiCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiCacheResponse'
            Core.<$> (x Core..?> "apiCache")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateApiCache

instance Core.NFData CreateApiCache

instance Core.ToHeaders CreateApiCache where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApiCache where
  toJSON CreateApiCache' {..} =
    Core.object
      ( Core.catMaybes
          [ ("atRestEncryptionEnabled" Core..=)
              Core.<$> atRestEncryptionEnabled,
            ("transitEncryptionEnabled" Core..=)
              Core.<$> transitEncryptionEnabled,
            Core.Just ("ttl" Core..= ttl),
            Core.Just
              ("apiCachingBehavior" Core..= apiCachingBehavior),
            Core.Just ("type" Core..= type')
          ]
      )

instance Core.ToPath CreateApiCache where
  toPath CreateApiCache' {..} =
    Core.mconcat
      ["/v1/apis/", Core.toBS apiId, "/ApiCaches"]

instance Core.ToQuery CreateApiCache where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @CreateApiCache@ operation.
--
-- /See:/ 'newCreateApiCacheResponse' smart constructor.
data CreateApiCacheResponse = CreateApiCacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Core.Maybe ApiCache,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCache', 'createApiCacheResponse_apiCache' - The @ApiCache@ object.
--
-- 'httpStatus', 'createApiCacheResponse_httpStatus' - The response's http status code.
newCreateApiCacheResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateApiCacheResponse
newCreateApiCacheResponse pHttpStatus_ =
  CreateApiCacheResponse'
    { apiCache = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiCache@ object.
createApiCacheResponse_apiCache :: Lens.Lens' CreateApiCacheResponse (Core.Maybe ApiCache)
createApiCacheResponse_apiCache = Lens.lens (\CreateApiCacheResponse' {apiCache} -> apiCache) (\s@CreateApiCacheResponse' {} a -> s {apiCache = a} :: CreateApiCacheResponse)

-- | The response's http status code.
createApiCacheResponse_httpStatus :: Lens.Lens' CreateApiCacheResponse Core.Int
createApiCacheResponse_httpStatus = Lens.lens (\CreateApiCacheResponse' {httpStatus} -> httpStatus) (\s@CreateApiCacheResponse' {} a -> s {httpStatus = a} :: CreateApiCacheResponse)

instance Core.NFData CreateApiCacheResponse
