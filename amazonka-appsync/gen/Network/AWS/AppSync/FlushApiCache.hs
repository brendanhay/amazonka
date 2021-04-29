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
-- Module      : Network.AWS.AppSync.FlushApiCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Network.AWS.AppSync.FlushApiCache
  ( -- * Creating a Request
    FlushApiCache (..),
    newFlushApiCache,

    -- * Request Lenses
    flushApiCache_apiId,

    -- * Destructuring the Response
    FlushApiCacheResponse (..),
    newFlushApiCacheResponse,

    -- * Response Lenses
    flushApiCacheResponse_httpStatus,
  )
where

import Network.AWS.AppSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCache' smart constructor.
data FlushApiCache = FlushApiCache'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushApiCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'flushApiCache_apiId' - The API ID.
newFlushApiCache ::
  -- | 'apiId'
  Prelude.Text ->
  FlushApiCache
newFlushApiCache pApiId_ =
  FlushApiCache' {apiId = pApiId_}

-- | The API ID.
flushApiCache_apiId :: Lens.Lens' FlushApiCache Prelude.Text
flushApiCache_apiId = Lens.lens (\FlushApiCache' {apiId} -> apiId) (\s@FlushApiCache' {} a -> s {apiId = a} :: FlushApiCache)

instance Prelude.AWSRequest FlushApiCache where
  type Rs FlushApiCache = FlushApiCacheResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          FlushApiCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FlushApiCache

instance Prelude.NFData FlushApiCache

instance Prelude.ToHeaders FlushApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath FlushApiCache where
  toPath FlushApiCache' {..} =
    Prelude.mconcat
      ["/v1/apis/", Prelude.toBS apiId, "/FlushCache"]

instance Prelude.ToQuery FlushApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCacheResponse' smart constructor.
data FlushApiCacheResponse = FlushApiCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FlushApiCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'flushApiCacheResponse_httpStatus' - The response's http status code.
newFlushApiCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  FlushApiCacheResponse
newFlushApiCacheResponse pHttpStatus_ =
  FlushApiCacheResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
flushApiCacheResponse_httpStatus :: Lens.Lens' FlushApiCacheResponse Prelude.Int
flushApiCacheResponse_httpStatus = Lens.lens (\FlushApiCacheResponse' {httpStatus} -> httpStatus) (\s@FlushApiCacheResponse' {} a -> s {httpStatus = a} :: FlushApiCacheResponse)

instance Prelude.NFData FlushApiCacheResponse
