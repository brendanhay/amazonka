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
-- Module      : Network.AWS.StorageGateway.ResetCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets all cache disks that have encountered an error and makes the
-- disks available for reconfiguration as cache storage. If your cache disk
-- encounters an error, the gateway prevents read and write operations on
-- virtual tapes in the gateway. For example, an error can occur when a
-- disk is corrupted or removed from the gateway. When a cache is reset,
-- the gateway loses its cache storage. At this point, you can reconfigure
-- the disks as cache disks. This operation is only supported in the cached
-- volume and tape types.
--
-- If the cache disk you are resetting contains data that has not been
-- uploaded to Amazon S3 yet, that data can be lost. After you reset cache
-- disks, there will be no configured cache disks left in the gateway, so
-- you must configure at least one new cache disk for your gateway to
-- function properly.
module Network.AWS.StorageGateway.ResetCache
  ( -- * Creating a Request
    ResetCache (..),
    newResetCache,

    -- * Request Lenses
    resetCache_gatewayARN,

    -- * Destructuring the Response
    ResetCacheResponse (..),
    newResetCacheResponse,

    -- * Response Lenses
    resetCacheResponse_gatewayARN,
    resetCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newResetCache' smart constructor.
data ResetCache = ResetCache'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'resetCache_gatewayARN' - Undocumented member.
newResetCache ::
  -- | 'gatewayARN'
  Prelude.Text ->
  ResetCache
newResetCache pGatewayARN_ =
  ResetCache' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
resetCache_gatewayARN :: Lens.Lens' ResetCache Prelude.Text
resetCache_gatewayARN = Lens.lens (\ResetCache' {gatewayARN} -> gatewayARN) (\s@ResetCache' {} a -> s {gatewayARN = a} :: ResetCache)

instance Prelude.AWSRequest ResetCache where
  type Rs ResetCache = ResetCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetCacheResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetCache

instance Prelude.NFData ResetCache

instance Prelude.ToHeaders ResetCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.ResetCache" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResetCache where
  toJSON ResetCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath ResetCache where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResetCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetCacheResponse' smart constructor.
data ResetCacheResponse = ResetCacheResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResetCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'resetCacheResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'resetCacheResponse_httpStatus' - The response's http status code.
newResetCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetCacheResponse
newResetCacheResponse pHttpStatus_ =
  ResetCacheResponse'
    { gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
resetCacheResponse_gatewayARN :: Lens.Lens' ResetCacheResponse (Prelude.Maybe Prelude.Text)
resetCacheResponse_gatewayARN = Lens.lens (\ResetCacheResponse' {gatewayARN} -> gatewayARN) (\s@ResetCacheResponse' {} a -> s {gatewayARN = a} :: ResetCacheResponse)

-- | The response's http status code.
resetCacheResponse_httpStatus :: Lens.Lens' ResetCacheResponse Prelude.Int
resetCacheResponse_httpStatus = Lens.lens (\ResetCacheResponse' {httpStatus} -> httpStatus) (\s@ResetCacheResponse' {} a -> s {httpStatus = a} :: ResetCacheResponse)

instance Prelude.NFData ResetCacheResponse
