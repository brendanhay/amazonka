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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeWorkingStorage' smart constructor.
data DescribeWorkingStorage = DescribeWorkingStorage'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DescribeWorkingStorage
newDescribeWorkingStorage pGatewayARN_ =
  DescribeWorkingStorage' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
describeWorkingStorage_gatewayARN :: Lens.Lens' DescribeWorkingStorage Prelude.Text
describeWorkingStorage_gatewayARN = Lens.lens (\DescribeWorkingStorage' {gatewayARN} -> gatewayARN) (\s@DescribeWorkingStorage' {} a -> s {gatewayARN = a} :: DescribeWorkingStorage)

instance Prelude.AWSRequest DescribeWorkingStorage where
  type
    Rs DescribeWorkingStorage =
      DescribeWorkingStorageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkingStorageResponse'
            Prelude.<$> (x Prelude..?> "WorkingStorageUsedInBytes")
            Prelude.<*> (x Prelude..?> "WorkingStorageAllocatedInBytes")
            Prelude.<*> (x Prelude..?> "DiskIds" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkingStorage

instance Prelude.NFData DescribeWorkingStorage

instance Prelude.ToHeaders DescribeWorkingStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DescribeWorkingStorage" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeWorkingStorage where
  toJSON DescribeWorkingStorage' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath DescribeWorkingStorage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeWorkingStorage where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { -- | The total working storage in bytes in use by the gateway. If no working
    -- storage is configured for the gateway, this field returns 0.
    workingStorageUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total working storage in bytes allocated for the gateway. If no
    -- working storage is configured for the gateway, this field returns 0.
    workingStorageAllocatedInBytes :: Prelude.Maybe Prelude.Integer,
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
  Prelude.Int ->
  DescribeWorkingStorageResponse
newDescribeWorkingStorageResponse pHttpStatus_ =
  DescribeWorkingStorageResponse'
    { workingStorageUsedInBytes =
        Prelude.Nothing,
      workingStorageAllocatedInBytes =
        Prelude.Nothing,
      diskIds = Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageUsedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Integer)
describeWorkingStorageResponse_workingStorageUsedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageUsedInBytes} -> workingStorageUsedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageUsedInBytes = a} :: DescribeWorkingStorageResponse)

-- | The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageAllocatedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Integer)
describeWorkingStorageResponse_workingStorageAllocatedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageAllocatedInBytes} -> workingStorageAllocatedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageAllocatedInBytes = a} :: DescribeWorkingStorageResponse)

-- | An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
describeWorkingStorageResponse_diskIds :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe [Prelude.Text])
describeWorkingStorageResponse_diskIds = Lens.lens (\DescribeWorkingStorageResponse' {diskIds} -> diskIds) (\s@DescribeWorkingStorageResponse' {} a -> s {diskIds = a} :: DescribeWorkingStorageResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
describeWorkingStorageResponse_gatewayARN :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Text)
describeWorkingStorageResponse_gatewayARN = Lens.lens (\DescribeWorkingStorageResponse' {gatewayARN} -> gatewayARN) (\s@DescribeWorkingStorageResponse' {} a -> s {gatewayARN = a} :: DescribeWorkingStorageResponse)

-- | The response's http status code.
describeWorkingStorageResponse_httpStatus :: Lens.Lens' DescribeWorkingStorageResponse Prelude.Int
describeWorkingStorageResponse_httpStatus = Lens.lens (\DescribeWorkingStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkingStorageResponse' {} a -> s {httpStatus = a} :: DescribeWorkingStorageResponse)

instance
  Prelude.NFData
    DescribeWorkingStorageResponse
