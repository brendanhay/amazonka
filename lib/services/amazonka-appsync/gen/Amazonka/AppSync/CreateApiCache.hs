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
-- Module      : Amazonka.AppSync.CreateApiCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a cache for the GraphQL API.
module Amazonka.AppSync.CreateApiCache
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @CreateApiCache@ operation.
--
-- /See:/ 'newCreateApiCache' smart constructor.
data CreateApiCache = CreateApiCache'
  { -- | At-rest encryption flag for cache. You cannot update this setting after
    -- creation.
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Transit encryption flag when connecting to cache. You cannot update this
    -- setting after creation.
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The GraphQL API ID.
    apiId :: Prelude.Text,
    -- | TTL in seconds for cache entries.
    --
    -- Valid values are 1–3,600 seconds.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'atRestEncryptionEnabled', 'createApiCache_atRestEncryptionEnabled' - At-rest encryption flag for cache. You cannot update this setting after
-- creation.
--
-- 'transitEncryptionEnabled', 'createApiCache_transitEncryptionEnabled' - Transit encryption flag when connecting to cache. You cannot update this
-- setting after creation.
--
-- 'apiId', 'createApiCache_apiId' - The GraphQL API ID.
--
-- 'ttl', 'createApiCache_ttl' - TTL in seconds for cache entries.
--
-- Valid values are 1–3,600 seconds.
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
  Prelude.Text ->
  -- | 'ttl'
  Prelude.Integer ->
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
          Prelude.Nothing,
        transitEncryptionEnabled = Prelude.Nothing,
        apiId = pApiId_,
        ttl = pTtl_,
        apiCachingBehavior = pApiCachingBehavior_,
        type' = pType_
      }

-- | At-rest encryption flag for cache. You cannot update this setting after
-- creation.
createApiCache_atRestEncryptionEnabled :: Lens.Lens' CreateApiCache (Prelude.Maybe Prelude.Bool)
createApiCache_atRestEncryptionEnabled = Lens.lens (\CreateApiCache' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@CreateApiCache' {} a -> s {atRestEncryptionEnabled = a} :: CreateApiCache)

-- | Transit encryption flag when connecting to cache. You cannot update this
-- setting after creation.
createApiCache_transitEncryptionEnabled :: Lens.Lens' CreateApiCache (Prelude.Maybe Prelude.Bool)
createApiCache_transitEncryptionEnabled = Lens.lens (\CreateApiCache' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@CreateApiCache' {} a -> s {transitEncryptionEnabled = a} :: CreateApiCache)

-- | The GraphQL API ID.
createApiCache_apiId :: Lens.Lens' CreateApiCache Prelude.Text
createApiCache_apiId = Lens.lens (\CreateApiCache' {apiId} -> apiId) (\s@CreateApiCache' {} a -> s {apiId = a} :: CreateApiCache)

-- | TTL in seconds for cache entries.
--
-- Valid values are 1–3,600 seconds.
createApiCache_ttl :: Lens.Lens' CreateApiCache Prelude.Integer
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApiCacheResponse'
            Prelude.<$> (x Data..?> "apiCache")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApiCache where
  hashWithSalt _salt CreateApiCache' {..} =
    _salt
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` ttl
      `Prelude.hashWithSalt` apiCachingBehavior
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateApiCache where
  rnf CreateApiCache' {..} =
    Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf ttl
      `Prelude.seq` Prelude.rnf apiCachingBehavior
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders CreateApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApiCache where
  toJSON CreateApiCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("atRestEncryptionEnabled" Data..=)
              Prelude.<$> atRestEncryptionEnabled,
            ("transitEncryptionEnabled" Data..=)
              Prelude.<$> transitEncryptionEnabled,
            Prelude.Just ("ttl" Data..= ttl),
            Prelude.Just
              ("apiCachingBehavior" Data..= apiCachingBehavior),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath CreateApiCache where
  toPath CreateApiCache' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/ApiCaches"]

instance Data.ToQuery CreateApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateApiCache@ operation.
--
-- /See:/ 'newCreateApiCacheResponse' smart constructor.
data CreateApiCacheResponse = CreateApiCacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Prelude.Maybe ApiCache,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateApiCacheResponse
newCreateApiCacheResponse pHttpStatus_ =
  CreateApiCacheResponse'
    { apiCache = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiCache@ object.
createApiCacheResponse_apiCache :: Lens.Lens' CreateApiCacheResponse (Prelude.Maybe ApiCache)
createApiCacheResponse_apiCache = Lens.lens (\CreateApiCacheResponse' {apiCache} -> apiCache) (\s@CreateApiCacheResponse' {} a -> s {apiCache = a} :: CreateApiCacheResponse)

-- | The response's http status code.
createApiCacheResponse_httpStatus :: Lens.Lens' CreateApiCacheResponse Prelude.Int
createApiCacheResponse_httpStatus = Lens.lens (\CreateApiCacheResponse' {httpStatus} -> httpStatus) (\s@CreateApiCacheResponse' {} a -> s {httpStatus = a} :: CreateApiCacheResponse)

instance Prelude.NFData CreateApiCacheResponse where
  rnf CreateApiCacheResponse' {..} =
    Prelude.rnf apiCache
      `Prelude.seq` Prelude.rnf httpStatus
