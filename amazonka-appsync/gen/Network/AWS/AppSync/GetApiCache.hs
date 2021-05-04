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
-- Module      : Network.AWS.AppSync.GetApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an @ApiCache@ object.
module Network.AWS.AppSync.GetApiCache
  ( -- * Creating a Request
    GetApiCache (..),
    newGetApiCache,

    -- * Request Lenses
    getApiCache_apiId,

    -- * Destructuring the Response
    GetApiCacheResponse (..),
    newGetApiCacheResponse,

    -- * Response Lenses
    getApiCacheResponse_apiCache,
    getApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApiCache@ operation.
--
-- /See:/ 'newGetApiCache' smart constructor.
data GetApiCache = GetApiCache'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getApiCache_apiId' - The API ID.
newGetApiCache ::
  -- | 'apiId'
  Prelude.Text ->
  GetApiCache
newGetApiCache pApiId_ =
  GetApiCache' {apiId = pApiId_}

-- | The API ID.
getApiCache_apiId :: Lens.Lens' GetApiCache Prelude.Text
getApiCache_apiId = Lens.lens (\GetApiCache' {apiId} -> apiId) (\s@GetApiCache' {} a -> s {apiId = a} :: GetApiCache)

instance Prelude.AWSRequest GetApiCache where
  type Rs GetApiCache = GetApiCacheResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApiCacheResponse'
            Prelude.<$> (x Prelude..?> "apiCache")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApiCache

instance Prelude.NFData GetApiCache

instance Prelude.ToHeaders GetApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath GetApiCache where
  toPath GetApiCache' {..} =
    Prelude.mconcat
      ["/v1/apis/", Prelude.toBS apiId, "/ApiCaches"]

instance Prelude.ToQuery GetApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetApiCache@ operation.
--
-- /See:/ 'newGetApiCacheResponse' smart constructor.
data GetApiCacheResponse = GetApiCacheResponse'
  { -- | The @ApiCache@ object.
    apiCache :: Prelude.Maybe ApiCache,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCache', 'getApiCacheResponse_apiCache' - The @ApiCache@ object.
--
-- 'httpStatus', 'getApiCacheResponse_httpStatus' - The response's http status code.
newGetApiCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApiCacheResponse
newGetApiCacheResponse pHttpStatus_ =
  GetApiCacheResponse'
    { apiCache = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ApiCache@ object.
getApiCacheResponse_apiCache :: Lens.Lens' GetApiCacheResponse (Prelude.Maybe ApiCache)
getApiCacheResponse_apiCache = Lens.lens (\GetApiCacheResponse' {apiCache} -> apiCache) (\s@GetApiCacheResponse' {} a -> s {apiCache = a} :: GetApiCacheResponse)

-- | The response's http status code.
getApiCacheResponse_httpStatus :: Lens.Lens' GetApiCacheResponse Prelude.Int
getApiCacheResponse_httpStatus = Lens.lens (\GetApiCacheResponse' {httpStatus} -> httpStatus) (\s@GetApiCacheResponse' {} a -> s {httpStatus = a} :: GetApiCacheResponse)

instance Prelude.NFData GetApiCacheResponse
