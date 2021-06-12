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
-- Module      : Network.AWS.StorageGateway.AddWorkingStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as working storage for a
-- gateway. This operation is only supported in the stored volume gateway
-- type. This operation is deprecated in cached volume API version
-- 20120630. Use AddUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use
-- the AddUploadBuffer operation to add upload buffer to a stored volume
-- gateway.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add working storage, and one or more disk IDs that you
-- want to configure as working storage.
module Network.AWS.StorageGateway.AddWorkingStorage
  ( -- * Creating a Request
    AddWorkingStorage (..),
    newAddWorkingStorage,

    -- * Request Lenses
    addWorkingStorage_gatewayARN,
    addWorkingStorage_diskIds,

    -- * Destructuring the Response
    AddWorkingStorageResponse (..),
    newAddWorkingStorageResponse,

    -- * Response Lenses
    addWorkingStorageResponse_gatewayARN,
    addWorkingStorageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   AddWorkingStorageInput$DiskIds
--
-- /See:/ 'newAddWorkingStorage' smart constructor.
data AddWorkingStorage = AddWorkingStorage'
  { gatewayARN :: Core.Text,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddWorkingStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addWorkingStorage_gatewayARN' - Undocumented member.
--
-- 'diskIds', 'addWorkingStorage_diskIds' - An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
newAddWorkingStorage ::
  -- | 'gatewayARN'
  Core.Text ->
  AddWorkingStorage
newAddWorkingStorage pGatewayARN_ =
  AddWorkingStorage'
    { gatewayARN = pGatewayARN_,
      diskIds = Core.mempty
    }

-- | Undocumented member.
addWorkingStorage_gatewayARN :: Lens.Lens' AddWorkingStorage Core.Text
addWorkingStorage_gatewayARN = Lens.lens (\AddWorkingStorage' {gatewayARN} -> gatewayARN) (\s@AddWorkingStorage' {} a -> s {gatewayARN = a} :: AddWorkingStorage)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
addWorkingStorage_diskIds :: Lens.Lens' AddWorkingStorage [Core.Text]
addWorkingStorage_diskIds = Lens.lens (\AddWorkingStorage' {diskIds} -> diskIds) (\s@AddWorkingStorage' {} a -> s {diskIds = a} :: AddWorkingStorage) Core.. Lens._Coerce

instance Core.AWSRequest AddWorkingStorage where
  type
    AWSResponse AddWorkingStorage =
      AddWorkingStorageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddWorkingStorageResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddWorkingStorage

instance Core.NFData AddWorkingStorage

instance Core.ToHeaders AddWorkingStorage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.AddWorkingStorage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddWorkingStorage where
  toJSON AddWorkingStorage' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DiskIds" Core..= diskIds)
          ]
      )

instance Core.ToPath AddWorkingStorage where
  toPath = Core.const "/"

instance Core.ToQuery AddWorkingStorage where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- for which working storage was configured.
--
-- /See:/ 'newAddWorkingStorageResponse' smart constructor.
data AddWorkingStorageResponse = AddWorkingStorageResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddWorkingStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addWorkingStorageResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'addWorkingStorageResponse_httpStatus' - The response's http status code.
newAddWorkingStorageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddWorkingStorageResponse
newAddWorkingStorageResponse pHttpStatus_ =
  AddWorkingStorageResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
addWorkingStorageResponse_gatewayARN :: Lens.Lens' AddWorkingStorageResponse (Core.Maybe Core.Text)
addWorkingStorageResponse_gatewayARN = Lens.lens (\AddWorkingStorageResponse' {gatewayARN} -> gatewayARN) (\s@AddWorkingStorageResponse' {} a -> s {gatewayARN = a} :: AddWorkingStorageResponse)

-- | The response's http status code.
addWorkingStorageResponse_httpStatus :: Lens.Lens' AddWorkingStorageResponse Core.Int
addWorkingStorageResponse_httpStatus = Lens.lens (\AddWorkingStorageResponse' {httpStatus} -> httpStatus) (\s@AddWorkingStorageResponse' {} a -> s {httpStatus = a} :: AddWorkingStorageResponse)

instance Core.NFData AddWorkingStorageResponse
