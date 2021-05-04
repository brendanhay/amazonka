{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AppSync.UpdateApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the cache for the GraphQL API.
module Network.AWS.AppSync.UpdateApiCache
  ( -- * Creating a Request
    UpdateApiCache (..),
    newUpdateApiCache,

    -- * Request Lenses
    updateApiCache_apiId,
    updateApiCache_ttl,
    updateApiCache_apiCachingBehavior,
    updateApiCache_type,

    -- * Destructuring the Response
    UpdateApiCacheResponse (..),
    newUpdateApiCacheResponse,

    -- * Response Lenses
    updateApiCacheResponse_apiCache,
    updateApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @UpdateApiCache@ operation.
--
-- /See:/ 'newUpdateApiCache' smart constructor.
data UpdateApiCache = UpdateApiCache'
  { -- | The GraphQL API Id.
    apiId :: Prelude.Text,
    -- | TTL in seconds for cache entries.
    --
    -- Valid values are between 1 and 3600 seconds.
    ttl :: Prelude.Integer,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'updateApiCache_apiId' - The GraphQL API Id.
--
-- 'ttl', 'updateApiCache_ttl' - TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
--
-- 'apiCachingBehavior', 'updateApiCache_apiCachingBehavior' - Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
--
-- 'type'', 'updateApiCache_type' - The cache instance type. Valid values are
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
newUpdateApiCache ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'ttl'
  Prelude.Integer ->
  -- | 'apiCachingBehavior'
  ApiCachingBehavior ->
  -- | 'type''
  ApiCacheType ->
  UpdateApiCache
newUpdateApiCache
  pApiId_
  pTtl_
  pApiCachingBehavior_
  pType_ =
    UpdateApiCache'
      { apiId = pApiId_,
        ttl = pTtl_,
        apiCachingBehavior = pApiCachingBehavior_,
        type' = pType_
      }

-- | The GraphQL API Id.
updateApiCache_apiId :: Lens.Lens' UpdateApiCache Prelude.Text
updateApiCache_apiId = Lens.lens (\UpdateApiCache' {apiId} -> apiId) (\s@UpdateApiCache' {} a -> s {apiId = a} :: UpdateApiCache)

-- | TTL in seconds for cache entries.
--
-- Valid values are between 1 and 3600 seconds.
updateApiCache_ttl :: Lens.Lens' UpdateApiCache Prelude.Integer
updateApiCache_ttl = Lens.lens (\UpdateApiCache' {ttl} -> ttl) (\s@UpdateApiCache' {} a -> s {ttl = a} :: UpdateApiCache)

-- | Caching behavior.
--
-- -   __FULL_REQUEST_CACHING__: All requests are fully cached.
--
-- -   __PER_RESOLVER_CACHING__: Individual resolvers that you specify are
--     cached.
updateApiCache_apiCachingBehavior :: Lens.Lens' UpdateApiCache ApiCachingBehavior
updateApiCache_apiCachingBehavior = Lens.lens (\UpdateApiCache' {apiCachingBehavior} -> apiCachingBehavior) (\s@UpdateApiCache' {} a -> s {apiCachingBehavior = a} :: UpdateApiCache)

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
updateApiCache_type :: Lens.Lens' UpdateApiCache ApiCacheType
updateApiCache_type = Lens.lens (\UpdateApiCache' {type'} -> type') (\s@UpdateApiCache' {} a -> s {type' = a} :: UpdateApiCache)

instance Prelude.AWSRequest UpdateApiCache where
  type Rs UpdateApiCache = UpdateApiCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApiCacheResponse'
            Prelude.<$> (x Prelude..?> "apiCache")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApiCache

instance Prelude.NFData UpdateApiCache

instance Prelude.ToHeaders UpdateApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateApiCache where
  toJSON UpdateApiCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ttl" Prelude..= ttl),
            Prelude.Just
              ("apiCachingBehavior" Prelude..= apiCachingBehavior),
            Prelude.Just ("type" Prelude..= type')
          ]
      )

instance Prelude.ToPath UpdateApiCache where
  toPath UpdateApiCache' {..} =
    Prelude.mconcat
      [ "/v1/apis/",
        Prelude.toBS apiId,
        "/ApiCaches/update"
      ]

instance Prelude.ToQuery UpdateApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @UpdateApiCache@ operation.
--
-- /See:/ 'newUpdateApiCacheResponse' smart constructor.
data UpdateApiCacheResponse = UpdateApiCacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Prelude.Maybe ApiCache,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCache', 'updateApiCacheResponse_apiCache' - The @ApiCache@ object.
--
-- 'httpStatus', 'updateApiCacheResponse_httpStatus' - The response's http status code.
newUpdateApiCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApiCacheResponse
newUpdateApiCacheResponse pHttpStatus_ =
  UpdateApiCacheResponse'
    { apiCache = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiCache@ object.
updateApiCacheResponse_apiCache :: Lens.Lens' UpdateApiCacheResponse (Prelude.Maybe ApiCache)
updateApiCacheResponse_apiCache = Lens.lens (\UpdateApiCacheResponse' {apiCache} -> apiCache) (\s@UpdateApiCacheResponse' {} a -> s {apiCache = a} :: UpdateApiCacheResponse)

-- | The response's http status code.
updateApiCacheResponse_httpStatus :: Lens.Lens' UpdateApiCacheResponse Prelude.Int
updateApiCacheResponse_httpStatus = Lens.lens (\UpdateApiCacheResponse' {httpStatus} -> httpStatus) (\s@UpdateApiCacheResponse' {} a -> s {httpStatus = a} :: UpdateApiCacheResponse)

instance Prelude.NFData UpdateApiCacheResponse
