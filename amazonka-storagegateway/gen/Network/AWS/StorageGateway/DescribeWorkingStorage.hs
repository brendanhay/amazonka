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
-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the working storage of a gateway. This
-- operation is only supported in the stored volumes gateway type. This
-- operation is deprecated in cached volumes API version (20120630). Use
-- DescribeUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use
-- the DescribeUploadBuffer operation to add upload buffer to a stored
-- volume gateway.
--
-- The response includes disk IDs that are configured as working storage,
-- and it includes the amount of working storage allocated and used.
module Network.AWS.StorageGateway.DescribeWorkingStorage
  ( -- * Creating a Request
    DescribeWorkingStorage (..),
    newDescribeWorkingStorage,

    -- * Request Lenses
    describeWorkingStorage_gatewayARN,

    -- * Destructuring the Response
    DescribeWorkingStorageResponse (..),
    newDescribeWorkingStorageResponse,

    -- * Response Lenses
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_gatewayARN,
    describeWorkingStorageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeWorkingStorage' smart constructor.
data DescribeWorkingStorage = DescribeWorkingStorage'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkingStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeWorkingStorage_gatewayARN' - Undocumented member.
newDescribeWorkingStorage ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeWorkingStorage
newDescribeWorkingStorage pGatewayARN_ =
  DescribeWorkingStorage' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeWorkingStorage_gatewayARN :: Lens.Lens' DescribeWorkingStorage Core.Text
describeWorkingStorage_gatewayARN = Lens.lens (\DescribeWorkingStorage' {gatewayARN} -> gatewayARN) (\s@DescribeWorkingStorage' {} a -> s {gatewayARN = a} :: DescribeWorkingStorage)

instance Core.AWSRequest DescribeWorkingStorage where
  type
    AWSResponse DescribeWorkingStorage =
      DescribeWorkingStorageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkingStorageResponse'
            Core.<$> (x Core..?> "WorkingStorageUsedInBytes")
            Core.<*> (x Core..?> "WorkingStorageAllocatedInBytes")
            Core.<*> (x Core..?> "DiskIds" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeWorkingStorage

instance Core.NFData DescribeWorkingStorage

instance Core.ToHeaders DescribeWorkingStorage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeWorkingStorage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkingStorage where
  toJSON DescribeWorkingStorage' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance Core.ToPath DescribeWorkingStorage where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkingStorage where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { -- | The total working storage in bytes in use by the gateway. If no working
    -- storage is configured for the gateway, this field returns 0.
    workingStorageUsedInBytes :: Core.Maybe Core.Integer,
    -- | The total working storage in bytes allocated for the gateway. If no
    -- working storage is configured for the gateway, this field returns 0.
    workingStorageAllocatedInBytes :: Core.Maybe Core.Integer,
    -- | An array of the gateway\'s local disk IDs that are configured as working
    -- storage. Each local disk ID is specified as a string (minimum length of
    -- 1 and maximum length of 300). If no local disks are configured as
    -- working storage, then the DiskIds array is empty.
    diskIds :: Core.Maybe [Core.Text],
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkingStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workingStorageUsedInBytes', 'describeWorkingStorageResponse_workingStorageUsedInBytes' - The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
--
-- 'workingStorageAllocatedInBytes', 'describeWorkingStorageResponse_workingStorageAllocatedInBytes' - The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
--
-- 'diskIds', 'describeWorkingStorageResponse_diskIds' - An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
--
-- 'gatewayARN', 'describeWorkingStorageResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'describeWorkingStorageResponse_httpStatus' - The response's http status code.
newDescribeWorkingStorageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeWorkingStorageResponse
newDescribeWorkingStorageResponse pHttpStatus_ =
  DescribeWorkingStorageResponse'
    { workingStorageUsedInBytes =
        Core.Nothing,
      workingStorageAllocatedInBytes =
        Core.Nothing,
      diskIds = Core.Nothing,
      gatewayARN = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageUsedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Core.Integer)
describeWorkingStorageResponse_workingStorageUsedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageUsedInBytes} -> workingStorageUsedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageUsedInBytes = a} :: DescribeWorkingStorageResponse)

-- | The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageAllocatedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Core.Integer)
describeWorkingStorageResponse_workingStorageAllocatedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageAllocatedInBytes} -> workingStorageAllocatedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageAllocatedInBytes = a} :: DescribeWorkingStorageResponse)

-- | An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
describeWorkingStorageResponse_diskIds :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe [Core.Text])
describeWorkingStorageResponse_diskIds = Lens.lens (\DescribeWorkingStorageResponse' {diskIds} -> diskIds) (\s@DescribeWorkingStorageResponse' {} a -> s {diskIds = a} :: DescribeWorkingStorageResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeWorkingStorageResponse_gatewayARN :: Lens.Lens' DescribeWorkingStorageResponse (Core.Maybe Core.Text)
describeWorkingStorageResponse_gatewayARN = Lens.lens (\DescribeWorkingStorageResponse' {gatewayARN} -> gatewayARN) (\s@DescribeWorkingStorageResponse' {} a -> s {gatewayARN = a} :: DescribeWorkingStorageResponse)

-- | The response's http status code.
describeWorkingStorageResponse_httpStatus :: Lens.Lens' DescribeWorkingStorageResponse Core.Int
describeWorkingStorageResponse_httpStatus = Lens.lens (\DescribeWorkingStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkingStorageResponse' {} a -> s {httpStatus = a} :: DescribeWorkingStorageResponse)

instance Core.NFData DescribeWorkingStorageResponse
