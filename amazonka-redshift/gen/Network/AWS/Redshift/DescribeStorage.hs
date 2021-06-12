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
-- Module      : Network.AWS.Redshift.DescribeStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns account level backups storage size and provisional storage.
module Network.AWS.Redshift.DescribeStorage
  ( -- * Creating a Request
    DescribeStorage (..),
    newDescribeStorage,

    -- * Destructuring the Response
    DescribeStorageResponse (..),
    newDescribeStorageResponse,

    -- * Response Lenses
    describeStorageResponse_totalProvisionedStorageInMegaBytes,
    describeStorageResponse_totalBackupSizeInMegaBytes,
    describeStorageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStorage' smart constructor.
data DescribeStorage = DescribeStorage'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeStorage ::
  DescribeStorage
newDescribeStorage = DescribeStorage'

instance Core.AWSRequest DescribeStorage where
  type
    AWSResponse DescribeStorage =
      DescribeStorageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeStorageResult"
      ( \s h x ->
          DescribeStorageResponse'
            Core.<$> (x Core..@? "TotalProvisionedStorageInMegaBytes")
            Core.<*> (x Core..@? "TotalBackupSizeInMegaBytes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStorage

instance Core.NFData DescribeStorage

instance Core.ToHeaders DescribeStorage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeStorage where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStorage where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("DescribeStorage" :: Core.ByteString),
            "Version" Core.=: ("2012-12-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeStorageResponse' smart constructor.
data DescribeStorageResponse = DescribeStorageResponse'
  { -- | The total amount of storage currently provisioned.
    totalProvisionedStorageInMegaBytes :: Core.Maybe Core.Double,
    -- | The total amount of storage currently used for snapshots.
    totalBackupSizeInMegaBytes :: Core.Maybe Core.Double,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalProvisionedStorageInMegaBytes', 'describeStorageResponse_totalProvisionedStorageInMegaBytes' - The total amount of storage currently provisioned.
--
-- 'totalBackupSizeInMegaBytes', 'describeStorageResponse_totalBackupSizeInMegaBytes' - The total amount of storage currently used for snapshots.
--
-- 'httpStatus', 'describeStorageResponse_httpStatus' - The response's http status code.
newDescribeStorageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStorageResponse
newDescribeStorageResponse pHttpStatus_ =
  DescribeStorageResponse'
    { totalProvisionedStorageInMegaBytes =
        Core.Nothing,
      totalBackupSizeInMegaBytes = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total amount of storage currently provisioned.
describeStorageResponse_totalProvisionedStorageInMegaBytes :: Lens.Lens' DescribeStorageResponse (Core.Maybe Core.Double)
describeStorageResponse_totalProvisionedStorageInMegaBytes = Lens.lens (\DescribeStorageResponse' {totalProvisionedStorageInMegaBytes} -> totalProvisionedStorageInMegaBytes) (\s@DescribeStorageResponse' {} a -> s {totalProvisionedStorageInMegaBytes = a} :: DescribeStorageResponse)

-- | The total amount of storage currently used for snapshots.
describeStorageResponse_totalBackupSizeInMegaBytes :: Lens.Lens' DescribeStorageResponse (Core.Maybe Core.Double)
describeStorageResponse_totalBackupSizeInMegaBytes = Lens.lens (\DescribeStorageResponse' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@DescribeStorageResponse' {} a -> s {totalBackupSizeInMegaBytes = a} :: DescribeStorageResponse)

-- | The response's http status code.
describeStorageResponse_httpStatus :: Lens.Lens' DescribeStorageResponse Core.Int
describeStorageResponse_httpStatus = Lens.lens (\DescribeStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageResponse' {} a -> s {httpStatus = a} :: DescribeStorageResponse)

instance Core.NFData DescribeStorageResponse
