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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newAddUploadBuffer' smart constructor.
data AddUploadBuffer = AddUploadBuffer'
  { gatewayARN :: Prelude.Text,
    -- | An array of strings that identify disks that are to be configured as
    -- working storage. Each string has a minimum length of 1 and maximum
    -- length of 300. You can get the disk IDs from the ListLocalDisks API.
    diskIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  AddUploadBuffer
newAddUploadBuffer pGatewayARN_ =
  AddUploadBuffer'
    { gatewayARN = pGatewayARN_,
      diskIds = Prelude.mempty
    }

-- | Undocumented member.
addUploadBuffer_gatewayARN :: Lens.Lens' AddUploadBuffer Prelude.Text
addUploadBuffer_gatewayARN = Lens.lens (\AddUploadBuffer' {gatewayARN} -> gatewayARN) (\s@AddUploadBuffer' {} a -> s {gatewayARN = a} :: AddUploadBuffer)

-- | An array of strings that identify disks that are to be configured as
-- working storage. Each string has a minimum length of 1 and maximum
-- length of 300. You can get the disk IDs from the ListLocalDisks API.
addUploadBuffer_diskIds :: Lens.Lens' AddUploadBuffer [Prelude.Text]
addUploadBuffer_diskIds = Lens.lens (\AddUploadBuffer' {diskIds} -> diskIds) (\s@AddUploadBuffer' {} a -> s {diskIds = a} :: AddUploadBuffer) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest AddUploadBuffer where
  type Rs AddUploadBuffer = AddUploadBufferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddUploadBufferResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddUploadBuffer

instance Prelude.NFData AddUploadBuffer

instance Prelude.ToHeaders AddUploadBuffer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.AddUploadBuffer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddUploadBuffer where
  toJSON AddUploadBuffer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just ("DiskIds" Prelude..= diskIds)
          ]
      )

instance Prelude.ToPath AddUploadBuffer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddUploadBuffer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddUploadBufferResponse' smart constructor.
data AddUploadBufferResponse = AddUploadBufferResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AddUploadBufferResponse
newAddUploadBufferResponse pHttpStatus_ =
  AddUploadBufferResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
addUploadBufferResponse_gatewayARN :: Lens.Lens' AddUploadBufferResponse (Prelude.Maybe Prelude.Text)
addUploadBufferResponse_gatewayARN = Lens.lens (\AddUploadBufferResponse' {gatewayARN} -> gatewayARN) (\s@AddUploadBufferResponse' {} a -> s {gatewayARN = a} :: AddUploadBufferResponse)

-- | The response's http status code.
addUploadBufferResponse_httpStatus :: Lens.Lens' AddUploadBufferResponse Prelude.Int
addUploadBufferResponse_httpStatus = Lens.lens (\AddUploadBufferResponse' {httpStatus} -> httpStatus) (\s@AddUploadBufferResponse' {} a -> s {httpStatus = a} :: AddUploadBufferResponse)

instance Prelude.NFData AddUploadBufferResponse
