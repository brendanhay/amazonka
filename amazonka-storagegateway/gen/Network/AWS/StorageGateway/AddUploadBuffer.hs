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
-- Module      : Network.AWS.StorageGateway.AddUploadBuffer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures one or more gateway local disks as upload buffer for a
-- specified gateway. This operation is supported for the stored volume,
-- cached volume and tape gateway types.
--
-- In the request, you specify the gateway Amazon Resource Name (ARN) to
-- which you want to add upload buffer, and one or more disk IDs that you
-- want to configure as upload buffer.
module Network.AWS.StorageGateway.AddUploadBuffer
  ( -- * Creating a Request
    AddUploadBuffer (..),
    newAddUploadBuffer,

    -- * Request Lenses
    addUploadBuffer_gatewayARN,
    addUploadBuffer_diskIds,

    -- * Destructuring the Response
    AddUploadBufferResponse (..),
    newAddUploadBufferResponse,

    -- * Response Lenses
    addUploadBufferResponse_gatewayARN,
    addUploadBufferResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newAddUploadBuffer' smart constructor.
data AddUploadBuffer = AddUploadBuffer'
  { gatewayARN :: Core.Text,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddUploadBuffer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addUploadBuffer_gatewayARN' - Undocumented member.
--
-- 'diskIds', 'addUploadBuffer_diskIds' - An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
newAddUploadBuffer ::
  -- | 'gatewayARN'
  Core.Text ->
  AddUploadBuffer
newAddUploadBuffer pGatewayARN_ =
  AddUploadBuffer'
    { gatewayARN = pGatewayARN_,
      diskIds = Core.mempty
    }

-- | Undocumented member.
addUploadBuffer_gatewayARN :: Lens.Lens' AddUploadBuffer Core.Text
addUploadBuffer_gatewayARN = Lens.lens (\AddUploadBuffer' {gatewayARN} -> gatewayARN) (\s@AddUploadBuffer' {} a -> s {gatewayARN = a} :: AddUploadBuffer)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
addUploadBuffer_diskIds :: Lens.Lens' AddUploadBuffer [Core.Text]
addUploadBuffer_diskIds = Lens.lens (\AddUploadBuffer' {diskIds} -> diskIds) (\s@AddUploadBuffer' {} a -> s {diskIds = a} :: AddUploadBuffer) Core.. Lens._Coerce

instance Core.AWSRequest AddUploadBuffer where
  type
    AWSResponse AddUploadBuffer =
      AddUploadBufferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddUploadBufferResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AddUploadBuffer

instance Core.NFData AddUploadBuffer

instance Core.ToHeaders AddUploadBuffer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.AddUploadBuffer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AddUploadBuffer where
  toJSON AddUploadBuffer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("DiskIds" Core..= diskIds)
          ]
      )

instance Core.ToPath AddUploadBuffer where
  toPath = Core.const "/"

instance Core.ToQuery AddUploadBuffer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAddUploadBufferResponse' smart constructor.
data AddUploadBufferResponse = AddUploadBufferResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AddUploadBufferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'addUploadBufferResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'addUploadBufferResponse_httpStatus' - The response's http status code.
newAddUploadBufferResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AddUploadBufferResponse
newAddUploadBufferResponse pHttpStatus_ =
  AddUploadBufferResponse'
    { gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
addUploadBufferResponse_gatewayARN :: Lens.Lens' AddUploadBufferResponse (Core.Maybe Core.Text)
addUploadBufferResponse_gatewayARN = Lens.lens (\AddUploadBufferResponse' {gatewayARN} -> gatewayARN) (\s@AddUploadBufferResponse' {} a -> s {gatewayARN = a} :: AddUploadBufferResponse)

-- | The response's http status code.
addUploadBufferResponse_httpStatus :: Lens.Lens' AddUploadBufferResponse Core.Int
addUploadBufferResponse_httpStatus = Lens.lens (\AddUploadBufferResponse' {httpStatus} -> httpStatus) (\s@AddUploadBufferResponse' {} a -> s {httpStatus = a} :: AddUploadBufferResponse)

instance Core.NFData AddUploadBufferResponse
