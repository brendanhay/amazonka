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
-- Module      : Network.AWS.Lightsail.ResetDistributionCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes currently cached content from your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- After resetting the cache, the next time a content request is made, your
-- distribution pulls, serves, and caches it from the origin.
module Network.AWS.Lightsail.ResetDistributionCache
  ( -- * Creating a Request
    ResetDistributionCache (..),
    newResetDistributionCache,

    -- * Request Lenses
    resetDistributionCache_distributionName,

    -- * Destructuring the Response
    ResetDistributionCacheResponse (..),
    newResetDistributionCacheResponse,

    -- * Response Lenses
    resetDistributionCacheResponse_status,
    resetDistributionCacheResponse_operation,
    resetDistributionCacheResponse_createTime,
    resetDistributionCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResetDistributionCache' smart constructor.
data ResetDistributionCache = ResetDistributionCache'
  { -- | The name of the distribution for which to reset cache.
    --
    -- Use the @GetDistributions@ action to get a list of distribution names
    -- that you can specify.
    distributionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest ResetDistributionCache where
  type
    Rs ResetDistributionCache =
      ResetDistributionCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetDistributionCacheResponse'
            Prelude.<$> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "operation")
            Prelude.<*> (x Prelude..?> "createTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetDistributionCache

instance Prelude.NFData ResetDistributionCache

instance Prelude.ToHeaders ResetDistributionCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.ResetDistributionCache" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResetDistributionCache where
  toJSON ResetDistributionCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("distributionName" Prelude..=)
              Prelude.<$> distributionName
          ]
      )

instance Prelude.ToPath ResetDistributionCache where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetDistributionCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetDistributionCacheResponse' smart constructor.
data ResetDistributionCacheResponse = ResetDistributionCacheResponse'
  { -- | The status of the reset cache request.
    status :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operation :: Prelude.Maybe Operation,
    -- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
    -- time format.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetDistributionCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'resetDistributionCacheResponse_status' - The status of the reset cache request.
--
-- 'operation', 'resetDistributionCacheResponse_operation' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'createTime', 'resetDistributionCacheResponse_createTime' - The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
--
-- 'httpStatus', 'resetDistributionCacheResponse_httpStatus' - The response's http status code.
newResetDistributionCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetDistributionCacheResponse
newResetDistributionCacheResponse pHttpStatus_ =
  ResetDistributionCacheResponse'
    { status =
        Prelude.Nothing,
      operation = Prelude.Nothing,
      createTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the reset cache request.
resetDistributionCacheResponse_status :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Prelude.Text)
resetDistributionCacheResponse_status = Lens.lens (\ResetDistributionCacheResponse' {status} -> status) (\s@ResetDistributionCacheResponse' {} a -> s {status = a} :: ResetDistributionCacheResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
resetDistributionCacheResponse_operation :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Operation)
resetDistributionCacheResponse_operation = Lens.lens (\ResetDistributionCacheResponse' {operation} -> operation) (\s@ResetDistributionCacheResponse' {} a -> s {operation = a} :: ResetDistributionCacheResponse)

-- | The timestamp of the reset cache request (e.g., @1479734909.17@) in Unix
-- time format.
resetDistributionCacheResponse_createTime :: Lens.Lens' ResetDistributionCacheResponse (Prelude.Maybe Prelude.UTCTime)
resetDistributionCacheResponse_createTime = Lens.lens (\ResetDistributionCacheResponse' {createTime} -> createTime) (\s@ResetDistributionCacheResponse' {} a -> s {createTime = a} :: ResetDistributionCacheResponse) Prelude.. Lens.mapping Prelude._Time

-- | The response's http status code.
resetDistributionCacheResponse_httpStatus :: Lens.Lens' ResetDistributionCacheResponse Prelude.Int
resetDistributionCacheResponse_httpStatus = Lens.lens (\ResetDistributionCacheResponse' {httpStatus} -> httpStatus) (\s@ResetDistributionCacheResponse' {} a -> s {httpStatus = a} :: ResetDistributionCacheResponse)

instance
  Prelude.NFData
    ResetDistributionCacheResponse
