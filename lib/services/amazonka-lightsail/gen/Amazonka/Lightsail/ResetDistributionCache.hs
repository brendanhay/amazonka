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
-- Module      : Amazonka.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- After resetting the cache, the next time a content request is made, your
-- distribution pulls, serves, and caches it from the origin.
module Amazonka.Lightsail.ResetDistributionCache
  ( -- * Creating a Request
    ResetDistributionCache (..),
    newResetDistributionCache,

    -- * Request Lenses
    resetDistributionCache_distributionName,

    -- * Destructuring the Response
    ResetDistributionCacheResponse (..),
    newResetDistributionCacheResponse,

    -- * Response Lenses
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetDistributionCache' smart constructor.
data ResetDistributionCache = ResetDistributionCache'
  { -- | The name of the distribution for which to reset cache.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDistributionCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'distributionName', 'resetDistributionCache_distributionName' - The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
newResetDistributionCache ::
  ResetDistributionCache
newResetDistributionCache =
  ResetDistributionCache'
    { distributionName =
        Prelude.Nothing
    }

-- | The name of the distribution for which to reset cache.
--
-- Use the @GetDistributions@ action to get a list of distribution names
-- that you can specify.
resetDistributionCache_distributionName :: Lens.Lens' ResetDistributionCache (Prelude.Maybe Prelude.Text)
resetDistributionCache_distributionName = Lens.lens (\ResetDistributionCache' {distributionName} -> distributionName) (\s@ResetDistributionCache' {} a -> s {distributionName = a} :: ResetDistributionCache)

instance Core.AWSRequest ResetDistributionCache where
  type
    AWSResponse ResetDistributionCache =
      ResetDistributionCacheResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetDistributionCacheResponse'
            Prelude.<$> (x Data..?> "createTime")
            Prelude.<*> (x Data..?> "operation")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetDistributionCache where
  hashWithSalt _salt ResetDistributionCache' {..} =
    _salt `Prelude.hashWithSalt` distributionName

instance Prelude.NFData ResetDistributionCache where
  rnf ResetDistributionCache' {..} =
    Prelude.rnf distributionName

instance Data.ToHeaders ResetDistributionCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.ResetDistributionCache" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetDistributionCache where
  toJSON ResetDistributionCache' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("distributionName" Data..=)
              Prelude.<$> distributionName
          ]
      )

instance Data.ToPath ResetDistributionCache where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetDistributionCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { -- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
    -- time format.
    createTime :: Prelude.Maybe Data.POSIX,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The status of the reset cache request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDistributionCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createTime', 'resetDistributionCacheResponse_createTime' - The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
--
-- 'operation', 'resetDistributionCacheResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'status', 'resetDistributionCacheResponse_status' - The status of the reset cache request.
--
-- 'httpStatus', 'resetDistributionCacheResponse_httpStatus' - The response's http status code.
newResetDistributionCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetDistributionCacheResponse
newResetDistributionCacheResponse pHttpStatus_ =
  ResetDistributionCacheResponse'
    { createTime =
        Prelude.Nothing,
      operation = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
resetDistributionCacheResponse_createTime :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Prelude.UTCTime)
resetDistributionCacheResponse_createTime = Lens.lens (\ResetDistributionCacheResponse' {createTime} -> createTime) (\s@ResetDistributionCacheResponse' {} a -> s {createTime = a} :: ResetDistributionCacheResponse) Prelude.. Lens.mapping Data._Time

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
resetDistributionCacheResponse_operation :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Operation)
resetDistributionCacheResponse_operation = Lens.lens (\ResetDistributionCacheResponse' {operation} -> operation) (\s@ResetDistributionCacheResponse' {} a -> s {operation = a} :: ResetDistributionCacheResponse)

-- | The status of the reset cache request.
resetDistributionCacheResponse_status :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Prelude.Text)
resetDistributionCacheResponse_status = Lens.lens (\ResetDistributionCacheResponse' {status} -> status) (\s@ResetDistributionCacheResponse' {} a -> s {status = a} :: ResetDistributionCacheResponse)

-- | The response's http status code.
resetDistributionCacheResponse_httpStatus :: Lens.Lens' ResetDistributionCacheResponse Prelude.Int
resetDistributionCacheResponse_httpStatus = Lens.lens (\ResetDistributionCacheResponse' {httpStatus} -> httpStatus) (\s@ResetDistributionCacheResponse' {} a -> s {httpStatus = a} :: ResetDistributionCacheResponse)

instance
  Prelude.NFData
    ResetDistributionCacheResponse
  where
  rnf ResetDistributionCacheResponse' {..} =
    Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
