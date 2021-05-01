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
-- Module      : Network.AWS.StorageGateway.DescribeUploadBuffer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the upload buffer of a gateway. This operation
-- is supported for the stored volume, cached volume, and tape gateway
-- types.
--
-- The response includes disk IDs that are configured as upload buffer
-- space, and it includes the amount of upload buffer space allocated and
-- used.
module Network.AWS.StorageGateway.DescribeUploadBuffer
  ( -- * Creating a Request
    DescribeUploadBuffer (..),
    newDescribeUploadBuffer,

    -- * Request Lenses
    describeUploadBuffer_gatewayARN,

    -- * Destructuring the Response
    DescribeUploadBufferResponse (..),
    newDescribeUploadBufferResponse,

    -- * Response Lenses
    describeUploadBufferResponse_uploadBufferAllocatedInBytes,
    describeUploadBufferResponse_uploadBufferUsedInBytes,
    describeUploadBufferResponse_diskIds,
    describeUploadBufferResponse_gatewayARN,
    describeUploadBufferResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeUploadBuffer' smart constructor.
data DescribeUploadBuffer = DescribeUploadBuffer'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeUploadBuffer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeUploadBuffer_gatewayARN' - Undocumented member.
newDescribeUploadBuffer ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeUploadBuffer
newDescribeUploadBuffer pGatewayARN_ =
  DescribeUploadBuffer' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeUploadBuffer_gatewayARN :: Lens.Lens' DescribeUploadBuffer Prelude.Text
describeUploadBuffer_gatewayARN = Lens.lens (\DescribeUploadBuffer' {gatewayARN} -> gatewayARN) (\s@DescribeUploadBuffer' {} a -> s {gatewayARN = a} :: DescribeUploadBuffer)

instance Prelude.AWSRequest DescribeUploadBuffer where
  type
    Rs DescribeUploadBuffer =
      DescribeUploadBufferResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUploadBufferResponse'
            Prelude.<$> (x Prelude..?> "UploadBufferAllocatedInBytes")
            Prelude.<*> (x Prelude..?> "UploadBufferUsedInBytes")
            Prelude.<*> (x Prelude..?> "DiskIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUploadBuffer

instance Prelude.NFData DescribeUploadBuffer

instance Prelude.ToHeaders DescribeUploadBuffer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DescribeUploadBuffer" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeUploadBuffer where
  toJSON DescribeUploadBuffer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath DescribeUploadBuffer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeUploadBuffer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUploadBufferResponse' smart constructor.
data DescribeUploadBufferResponse = DescribeUploadBufferResponse'
  { -- | The total number of bytes allocated in the gateway\'s as upload buffer.
    uploadBufferAllocatedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total number of bytes being used in the gateway\'s upload buffer.
    uploadBufferUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | An array of the gateway\'s local disk IDs that are configured as working
    -- storage. Each local disk ID is specified as a string (minimum length of
    -- 1 and maximum length of 300). If no local disks are configured as
    -- working storage, then the DiskIds array is empty.
    diskIds :: Prelude.Maybe [Prelude.Text],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeUploadBufferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadBufferAllocatedInBytes', 'describeUploadBufferResponse_uploadBufferAllocatedInBytes' - The total number of bytes allocated in the gateway\'s as upload buffer.
--
-- 'uploadBufferUsedInBytes', 'describeUploadBufferResponse_uploadBufferUsedInBytes' - The total number of bytes being used in the gateway\'s upload buffer.
--
-- 'diskIds', 'describeUploadBufferResponse_diskIds' - An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
--
-- 'gatewayARN', 'describeUploadBufferResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'describeUploadBufferResponse_httpStatus' - The response's http status code.
newDescribeUploadBufferResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUploadBufferResponse
newDescribeUploadBufferResponse pHttpStatus_ =
  DescribeUploadBufferResponse'
    { uploadBufferAllocatedInBytes =
        Prelude.Nothing,
      uploadBufferUsedInBytes = Prelude.Nothing,
      diskIds = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of bytes allocated in the gateway\'s as upload buffer.
describeUploadBufferResponse_uploadBufferAllocatedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Prelude.Maybe Prelude.Integer)
describeUploadBufferResponse_uploadBufferAllocatedInBytes = Lens.lens (\DescribeUploadBufferResponse' {uploadBufferAllocatedInBytes} -> uploadBufferAllocatedInBytes) (\s@DescribeUploadBufferResponse' {} a -> s {uploadBufferAllocatedInBytes = a} :: DescribeUploadBufferResponse)

-- | The total number of bytes being used in the gateway\'s upload buffer.
describeUploadBufferResponse_uploadBufferUsedInBytes :: Lens.Lens' DescribeUploadBufferResponse (Prelude.Maybe Prelude.Integer)
describeUploadBufferResponse_uploadBufferUsedInBytes = Lens.lens (\DescribeUploadBufferResponse' {uploadBufferUsedInBytes} -> uploadBufferUsedInBytes) (\s@DescribeUploadBufferResponse' {} a -> s {uploadBufferUsedInBytes = a} :: DescribeUploadBufferResponse)

-- | An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
describeUploadBufferResponse_diskIds :: Lens.Lens' DescribeUploadBufferResponse (Prelude.Maybe [Prelude.Text])
describeUploadBufferResponse_diskIds = Lens.lens (\DescribeUploadBufferResponse' {diskIds} -> diskIds) (\s@DescribeUploadBufferResponse' {} a -> s {diskIds = a} :: DescribeUploadBufferResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
describeUploadBufferResponse_gatewayARN :: Lens.Lens' DescribeUploadBufferResponse (Prelude.Maybe Prelude.Text)
describeUploadBufferResponse_gatewayARN = Lens.lens (\DescribeUploadBufferResponse' {gatewayARN} -> gatewayARN) (\s@DescribeUploadBufferResponse' {} a -> s {gatewayARN = a} :: DescribeUploadBufferResponse)

-- | The response's http status code.
describeUploadBufferResponse_httpStatus :: Lens.Lens' DescribeUploadBufferResponse Prelude.Int
describeUploadBufferResponse_httpStatus = Lens.lens (\DescribeUploadBufferResponse' {httpStatus} -> httpStatus) (\s@DescribeUploadBufferResponse' {} a -> s {httpStatus = a} :: DescribeUploadBufferResponse)

instance Prelude.NFData DescribeUploadBufferResponse
