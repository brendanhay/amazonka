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
-- Module      : Amazonka.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.StorageGateway.DescribeWorkingStorage
  ( -- * Creating a Request
    DescribeWorkingStorage (..),
    newDescribeWorkingStorage,

    -- * Request Lenses
    describeWorkingStorage_gatewayARN,

    -- * Destructuring the Response
    DescribeWorkingStorageResponse (..),
    newDescribeWorkingStorageResponse,

    -- * Response Lenses
    describeWorkingStorageResponse_diskIds,
    describeWorkingStorageResponse_gatewayARN,
    describeWorkingStorageResponse_workingStorageAllocatedInBytes,
    describeWorkingStorageResponse_workingStorageUsedInBytes,
    describeWorkingStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeWorkingStorage' smart constructor.
data DescribeWorkingStorage = DescribeWorkingStorage'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeWorkingStorage where
  type
    AWSResponse DescribeWorkingStorage =
      DescribeWorkingStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkingStorageResponse'
            Prelude.<$> (x Data..?> "DiskIds" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (x Data..?> "WorkingStorageAllocatedInBytes")
            Prelude.<*> (x Data..?> "WorkingStorageUsedInBytes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkingStorage where
  hashWithSalt _salt DescribeWorkingStorage' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DescribeWorkingStorage where
  rnf DescribeWorkingStorage' {..} =
    Prelude.rnf gatewayARN

instance Data.ToHeaders DescribeWorkingStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeWorkingStorage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeWorkingStorage where
  toJSON DescribeWorkingStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DescribeWorkingStorage where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeWorkingStorage where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { -- | An array of the gateway\'s local disk IDs that are configured as working
    -- storage. Each local disk ID is specified as a string (minimum length of
    -- 1 and maximum length of 300). If no local disks are configured as
    -- working storage, then the DiskIds array is empty.
    diskIds :: Prelude.Maybe [Prelude.Text],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The total working storage in bytes allocated for the gateway. If no
    -- working storage is configured for the gateway, this field returns 0.
    workingStorageAllocatedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total working storage in bytes in use by the gateway. If no working
    -- storage is configured for the gateway, this field returns 0.
    workingStorageUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkingStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'diskIds', 'describeWorkingStorageResponse_diskIds' - An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
--
-- 'gatewayARN', 'describeWorkingStorageResponse_gatewayARN' - Undocumented member.
--
-- 'workingStorageAllocatedInBytes', 'describeWorkingStorageResponse_workingStorageAllocatedInBytes' - The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
--
-- 'workingStorageUsedInBytes', 'describeWorkingStorageResponse_workingStorageUsedInBytes' - The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
--
-- 'httpStatus', 'describeWorkingStorageResponse_httpStatus' - The response's http status code.
newDescribeWorkingStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkingStorageResponse
newDescribeWorkingStorageResponse pHttpStatus_ =
  DescribeWorkingStorageResponse'
    { diskIds =
        Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      workingStorageAllocatedInBytes =
        Prelude.Nothing,
      workingStorageUsedInBytes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
describeWorkingStorageResponse_diskIds :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe [Prelude.Text])
describeWorkingStorageResponse_diskIds = Lens.lens (\DescribeWorkingStorageResponse' {diskIds} -> diskIds) (\s@DescribeWorkingStorageResponse' {} a -> s {diskIds = a} :: DescribeWorkingStorageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeWorkingStorageResponse_gatewayARN :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Text)
describeWorkingStorageResponse_gatewayARN = Lens.lens (\DescribeWorkingStorageResponse' {gatewayARN} -> gatewayARN) (\s@DescribeWorkingStorageResponse' {} a -> s {gatewayARN = a} :: DescribeWorkingStorageResponse)

-- | The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageAllocatedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Integer)
describeWorkingStorageResponse_workingStorageAllocatedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageAllocatedInBytes} -> workingStorageAllocatedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageAllocatedInBytes = a} :: DescribeWorkingStorageResponse)

-- | The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
describeWorkingStorageResponse_workingStorageUsedInBytes :: Lens.Lens' DescribeWorkingStorageResponse (Prelude.Maybe Prelude.Integer)
describeWorkingStorageResponse_workingStorageUsedInBytes = Lens.lens (\DescribeWorkingStorageResponse' {workingStorageUsedInBytes} -> workingStorageUsedInBytes) (\s@DescribeWorkingStorageResponse' {} a -> s {workingStorageUsedInBytes = a} :: DescribeWorkingStorageResponse)

-- | The response's http status code.
describeWorkingStorageResponse_httpStatus :: Lens.Lens' DescribeWorkingStorageResponse Prelude.Int
describeWorkingStorageResponse_httpStatus = Lens.lens (\DescribeWorkingStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkingStorageResponse' {} a -> s {httpStatus = a} :: DescribeWorkingStorageResponse)

instance
  Prelude.NFData
    DescribeWorkingStorageResponse
  where
  rnf DescribeWorkingStorageResponse' {..} =
    Prelude.rnf diskIds `Prelude.seq`
      Prelude.rnf gatewayARN `Prelude.seq`
        Prelude.rnf workingStorageAllocatedInBytes `Prelude.seq`
          Prelude.rnf workingStorageUsedInBytes `Prelude.seq`
            Prelude.rnf httpStatus
