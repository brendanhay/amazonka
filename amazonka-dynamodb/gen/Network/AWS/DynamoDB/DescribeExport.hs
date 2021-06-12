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
-- Module      : Network.AWS.DynamoDB.DescribeExport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an existing table export.
module Network.AWS.DynamoDB.DescribeExport
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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeExport' smart constructor.
data DescribeExport = DescribeExport'
  { -- | The Amazon Resource Name (ARN) associated with the export.
    exportArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeExport
newDescribeExport pExportArn_ =
  DescribeExport' {exportArn = pExportArn_}

-- | The Amazon Resource Name (ARN) associated with the export.
describeExport_exportArn :: Lens.Lens' DescribeExport Core.Text
describeExport_exportArn = Lens.lens (\DescribeExport' {exportArn} -> exportArn) (\s@DescribeExport' {} a -> s {exportArn = a} :: DescribeExport)

instance Core.AWSRequest DescribeExport where
  type
    AWSResponse DescribeExport =
      DescribeExportResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeExportResponse'
            Core.<$> (x Core..?> "ExportDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeExport

instance Core.NFData DescribeExport

instance Core.ToHeaders DescribeExport where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeExport" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeExport where
  toJSON DescribeExport' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ExportArn" Core..= exportArn)]
      )

instance Core.ToPath DescribeExport where
  toPath = Core.const "/"

instance Core.ToQuery DescribeExport where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeExportResponse' smart constructor.
data DescribeExportResponse = DescribeExportResponse'
  { -- | Represents the properties of the export.
    exportDescription :: Core.Maybe ExportDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeExportResponse
newDescribeExportResponse pHttpStatus_ =
  DescribeExportResponse'
    { exportDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the properties of the export.
describeExportResponse_exportDescription :: Lens.Lens' DescribeExportResponse (Core.Maybe ExportDescription)
describeExportResponse_exportDescription = Lens.lens (\DescribeExportResponse' {exportDescription} -> exportDescription) (\s@DescribeExportResponse' {} a -> s {exportDescription = a} :: DescribeExportResponse)

-- | The response's http status code.
describeExportResponse_httpStatus :: Lens.Lens' DescribeExportResponse Core.Int
describeExportResponse_httpStatus = Lens.lens (\DescribeExportResponse' {httpStatus} -> httpStatus) (\s@DescribeExportResponse' {} a -> s {httpStatus = a} :: DescribeExportResponse)

instance Core.NFData DescribeExportResponse
