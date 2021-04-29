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
-- Module      : Network.AWS.StorageGateway.AddCache
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as cache for a gateway. This
-- operation is only supported in the cached volume, tape, and file gateway
-- type (see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/StorageGatewayConcepts.html How AWS Storage Gateway works (architecture)>.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add cache, and one or more disk IDs that you want to
-- configure as cache.
module Network.AWS.StorageGateway.AddCache
  ( -- * Creating a Request
    AddCache (..),
    newAddCache,

    -- * Request Lenses
    addCache_gatewayARN,
    addCache_diskIds,

    -- * Destructuring the Response
    AddCacheResponse (..),
    newAddCacheResponse,

    -- * Response Lenses
    addCacheResponse_gatewayARN,
    addCacheResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newAddCache' smart constructor.
data AddCache = AddCache'
  { gatewayARN :: Prelude.Text,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddCache' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addCache_gatewayARN' - Undocumented member.
--
-- 'diskIds', 'addCache_diskIds' - An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
newAddCache ::
  -- | 'gatewayARN'
  Prelude.Text ->
  AddCache
newAddCache pGatewayARN_ =
  AddCache'
    { gatewayARN = pGatewayARN_,
      diskIds = Prelude.mempty
    }

-- | Undocumented member.
addCache_gatewayARN :: Lens.Lens' AddCache Prelude.Text
addCache_gatewayARN = Lens.lens (\AddCache' {gatewayARN} -> gatewayARN) (\s@AddCache' {} a -> s {gatewayARN = a} :: AddCache)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
addCache_diskIds :: Lens.Lens' AddCache [Prelude.Text]
addCache_diskIds = Lens.lens (\AddCache' {diskIds} -> diskIds) (\s@AddCache' {} a -> s {diskIds = a} :: AddCache) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddCache where
  type Rs AddCache = AddCacheResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCacheResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddCache

instance Prelude.NFData AddCache

instance Prelude.ToHeaders AddCache where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.AddCache" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddCache where
  toJSON AddCache' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("DiskIds" Prelude..= diskIds)
          ]
      )

instance Prelude.ToPath AddCache where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddCache where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddCacheResponse' smart constructor.
data AddCacheResponse = AddCacheResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddCacheResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addCacheResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'addCacheResponse_httpStatus' - The response's http status code.
newAddCacheResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddCacheResponse
newAddCacheResponse pHttpStatus_ =
  AddCacheResponse'
    { gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
addCacheResponse_gatewayARN :: Lens.Lens' AddCacheResponse (Prelude.Maybe Prelude.Text)
addCacheResponse_gatewayARN = Lens.lens (\AddCacheResponse' {gatewayARN} -> gatewayARN) (\s@AddCacheResponse' {} a -> s {gatewayARN = a} :: AddCacheResponse)

-- | The response's http status code.
addCacheResponse_httpStatus :: Lens.Lens' AddCacheResponse Prelude.Int
addCacheResponse_httpStatus = Lens.lens (\AddCacheResponse' {httpStatus} -> httpStatus) (\s@AddCacheResponse' {} a -> s {httpStatus = a} :: AddCacheResponse)

instance Prelude.NFData AddCacheResponse
