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
-- Module      : Amazonka.Redshift.DescribeStorage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns account level backups storage size and provisional storage.
module Amazonka.Redshift.DescribeStorage
  ( -- * Creating a Request
    DescribeStorage (..),
    newDescribeStorage,

    -- * Destructuring the Response
    DescribeStorageResponse (..),
    newDescribeStorageResponse,

    -- * Response Lenses
    describeStorageResponse_totalBackupSizeInMegaBytes,
    describeStorageResponse_totalProvisionedStorageInMegaBytes,
    describeStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStorage' smart constructor.
data DescribeStorage = DescribeStorage'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeStorageResult"
      ( \s h x ->
          DescribeStorageResponse'
            Prelude.<$> (x Data..@? "TotalBackupSizeInMegaBytes")
            Prelude.<*> (x Data..@? "TotalProvisionedStorageInMegaBytes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStorage where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeStorage where
  rnf _ = ()

instance Data.ToHeaders DescribeStorage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeStorage where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStorage where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("DescribeStorage" :: Prelude.ByteString),
            "Version"
              Data.=: ("2012-12-01" :: Prelude.ByteString)
          ]
      )

-- | /See:/ 'newDescribeStorageResponse' smart constructor.
data DescribeStorageResponse = DescribeStorageResponse'
  { -- | The total amount of storage currently used for snapshots.
    totalBackupSizeInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The total amount of storage currently provisioned.
    totalProvisionedStorageInMegaBytes :: Prelude.Maybe Prelude.Double,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalBackupSizeInMegaBytes', 'describeStorageResponse_totalBackupSizeInMegaBytes' - The total amount of storage currently used for snapshots.
--
-- 'totalProvisionedStorageInMegaBytes', 'describeStorageResponse_totalProvisionedStorageInMegaBytes' - The total amount of storage currently provisioned.
--
-- 'httpStatus', 'describeStorageResponse_httpStatus' - The response's http status code.
newDescribeStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStorageResponse
newDescribeStorageResponse pHttpStatus_ =
  DescribeStorageResponse'
    { totalBackupSizeInMegaBytes =
        Prelude.Nothing,
      totalProvisionedStorageInMegaBytes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total amount of storage currently used for snapshots.
describeStorageResponse_totalBackupSizeInMegaBytes :: Lens.Lens' DescribeStorageResponse (Prelude.Maybe Prelude.Double)
describeStorageResponse_totalBackupSizeInMegaBytes = Lens.lens (\DescribeStorageResponse' {totalBackupSizeInMegaBytes} -> totalBackupSizeInMegaBytes) (\s@DescribeStorageResponse' {} a -> s {totalBackupSizeInMegaBytes = a} :: DescribeStorageResponse)

-- | The total amount of storage currently provisioned.
describeStorageResponse_totalProvisionedStorageInMegaBytes :: Lens.Lens' DescribeStorageResponse (Prelude.Maybe Prelude.Double)
describeStorageResponse_totalProvisionedStorageInMegaBytes = Lens.lens (\DescribeStorageResponse' {totalProvisionedStorageInMegaBytes} -> totalProvisionedStorageInMegaBytes) (\s@DescribeStorageResponse' {} a -> s {totalProvisionedStorageInMegaBytes = a} :: DescribeStorageResponse)

-- | The response's http status code.
describeStorageResponse_httpStatus :: Lens.Lens' DescribeStorageResponse Prelude.Int
describeStorageResponse_httpStatus = Lens.lens (\DescribeStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageResponse' {} a -> s {httpStatus = a} :: DescribeStorageResponse)

instance Prelude.NFData DescribeStorageResponse where
  rnf DescribeStorageResponse' {..} =
    Prelude.rnf totalBackupSizeInMegaBytes
      `Prelude.seq` Prelude.rnf totalProvisionedStorageInMegaBytes
      `Prelude.seq` Prelude.rnf httpStatus
