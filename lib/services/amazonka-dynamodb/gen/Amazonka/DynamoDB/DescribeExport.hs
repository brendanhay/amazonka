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
-- Module      : Amazonka.DynamoDB.DescribeExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing table export.
module Amazonka.DynamoDB.DescribeExport
  ( -- * Creating a Request
    DescribeExport (..),
    newDescribeExport,

    -- * Request Lenses
    describeExport_exportArn,

    -- * Destructuring the Response
    DescribeExportResponse (..),
    newDescribeExportResponse,

    -- * Response Lenses
    describeExportResponse_exportDescription,
    describeExportResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeExport' smart constructor.
data DescribeExport = DescribeExport'
  { -- | The Amazon Resource Name (ARN) associated with the export.
    exportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportArn', 'describeExport_exportArn' - The Amazon Resource Name (ARN) associated with the export.
newDescribeExport ::
  -- | 'exportArn'
  Prelude.Text ->
  DescribeExport
newDescribeExport pExportArn_ =
  DescribeExport' {exportArn = pExportArn_}

-- | The Amazon Resource Name (ARN) associated with the export.
describeExport_exportArn :: Lens.Lens' DescribeExport Prelude.Text
describeExport_exportArn = Lens.lens (\DescribeExport' {exportArn} -> exportArn) (\s@DescribeExport' {} a -> s {exportArn = a} :: DescribeExport)

instance Core.AWSRequest DescribeExport where
  type
    AWSResponse DescribeExport =
      DescribeExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Prelude.<$> (x Core..?> "ExportDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeExport where
  hashWithSalt _salt DescribeExport' {..} =
    _salt `Prelude.hashWithSalt` exportArn

instance Prelude.NFData DescribeExport where
  rnf DescribeExport' {..} = Prelude.rnf exportArn

instance Core.ToHeaders DescribeExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeExport where
  toJSON DescribeExport' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ExportArn" Core..= exportArn)]
      )

instance Core.ToPath DescribeExport where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | Represents the properties of the export.
    exportDescription :: Prelude.Maybe ExportDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportDescription', 'describeExportResponse_exportDescription' - Represents the properties of the export.
--
-- 'httpStatus', 'describeExportResponse_httpStatus' - The response's http status code.
newDescribeExportResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeExportResponse
newDescribeExportResponse pHttpStatus_ =
  DescribeExportResponse'
    { exportDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of the export.
describeExportResponse_exportDescription :: Lens.Lens' DescribeExportResponse (Prelude.Maybe ExportDescription)
describeExportResponse_exportDescription = Lens.lens (\DescribeExportResponse' {exportDescription} -> exportDescription) (\s@DescribeExportResponse' {} a -> s {exportDescription = a} :: DescribeExportResponse)

-- | The response's http status code.
describeExportResponse_httpStatus :: Lens.Lens' DescribeExportResponse Prelude.Int
describeExportResponse_httpStatus = Lens.lens (\DescribeExportResponse' {httpStatus} -> httpStatus) (\s@DescribeExportResponse' {} a -> s {httpStatus = a} :: DescribeExportResponse)

instance Prelude.NFData DescribeExportResponse where
  rnf DescribeExportResponse' {..} =
    Prelude.rnf exportDescription
      `Prelude.seq` Prelude.rnf httpStatus
