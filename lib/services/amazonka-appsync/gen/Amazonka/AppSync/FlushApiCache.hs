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
-- Module      : Amazonka.AppSync.FlushApiCache
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Flushes an @ApiCache@ object.
module Amazonka.AppSync.FlushApiCache
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

import Amazonka.AppSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCache' smart constructor.
data FlushApiCache = FlushApiCache'
  { -- | The API ID.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest FlushApiCache where
  type
    AWSResponse FlushApiCache =
      FlushApiCacheResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          FlushApiCacheResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable FlushApiCache where
  hashWithSalt _salt FlushApiCache' {..} =
    _salt `Prelude.hashWithSalt` apiId

instance Prelude.NFData FlushApiCache where
  rnf FlushApiCache' {..} = Prelude.rnf apiId

instance Data.ToHeaders FlushApiCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath FlushApiCache where
  toPath FlushApiCache' {..} =
    Prelude.mconcat
      ["/v1/apis/", Data.toBS apiId, "/FlushCache"]

instance Data.ToQuery FlushApiCache where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @FlushApiCache@ operation.
--
-- /See:/ 'newFlushApiCacheResponse' smart constructor.
data FlushApiCacheResponse = FlushApiCacheResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData FlushApiCacheResponse where
  rnf FlushApiCacheResponse' {..} =
    Prelude.rnf httpStatus
