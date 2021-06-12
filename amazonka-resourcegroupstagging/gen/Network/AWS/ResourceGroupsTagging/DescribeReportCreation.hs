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
-- Module      : Network.AWS.ResourceGroupsTagging.DescribeReportCreation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the @StartReportCreation@ operation.
--
-- You can call this operation only from the organization\'s management
-- account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.DescribeReportCreation
  ( -- * Creating a Request
    DescribeReportCreation (..),
    newDescribeReportCreation,

    -- * Destructuring the Response
    DescribeReportCreationResponse (..),
    newDescribeReportCreationResponse,

    -- * Response Lenses
    describeReportCreationResponse_status,
    describeReportCreationResponse_s3Location,
    describeReportCreationResponse_errorMessage,
    describeReportCreationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeReportCreation' smart constructor.
data DescribeReportCreation = DescribeReportCreation'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReportCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeReportCreation ::
  DescribeReportCreation
newDescribeReportCreation = DescribeReportCreation'

instance Core.AWSRequest DescribeReportCreation where
  type
    AWSResponse DescribeReportCreation =
      DescribeReportCreationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportCreationResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "S3Location")
            Core.<*> (x Core..?> "ErrorMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeReportCreation

instance Core.NFData DescribeReportCreation

instance Core.ToHeaders DescribeReportCreation where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ResourceGroupsTaggingAPI_20170126.DescribeReportCreation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeReportCreation where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath DescribeReportCreation where
  toPath = Core.const "/"

instance Core.ToQuery DescribeReportCreation where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeReportCreationResponse' smart constructor.
data DescribeReportCreationResponse = DescribeReportCreationResponse'
  { -- | Reports the status of the operation.
    --
    -- The operation status can be one of the following:
    --
    -- -   @RUNNING@ - Report creation is in progress.
    --
    -- -   @SUCCEEDED@ - Report creation is complete. You can open the report
    --     from the Amazon S3 bucket that you specified when you ran
    --     @StartReportCreation@.
    --
    -- -   @FAILED@ - Report creation timed out or the Amazon S3 bucket is not
    --     accessible.
    --
    -- -   @NO REPORT@ - No report was generated in the last 90 days.
    status :: Core.Maybe Core.Text,
    -- | The path to the Amazon S3 bucket where the report was stored on
    -- creation.
    s3Location :: Core.Maybe Core.Text,
    -- | Details of the common errors that all operations return.
    errorMessage :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeReportCreationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeReportCreationResponse_status' - Reports the status of the operation.
--
-- The operation status can be one of the following:
--
-- -   @RUNNING@ - Report creation is in progress.
--
-- -   @SUCCEEDED@ - Report creation is complete. You can open the report
--     from the Amazon S3 bucket that you specified when you ran
--     @StartReportCreation@.
--
-- -   @FAILED@ - Report creation timed out or the Amazon S3 bucket is not
--     accessible.
--
-- -   @NO REPORT@ - No report was generated in the last 90 days.
--
-- 's3Location', 'describeReportCreationResponse_s3Location' - The path to the Amazon S3 bucket where the report was stored on
-- creation.
--
-- 'errorMessage', 'describeReportCreationResponse_errorMessage' - Details of the common errors that all operations return.
--
-- 'httpStatus', 'describeReportCreationResponse_httpStatus' - The response's http status code.
newDescribeReportCreationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeReportCreationResponse
newDescribeReportCreationResponse pHttpStatus_ =
  DescribeReportCreationResponse'
    { status =
        Core.Nothing,
      s3Location = Core.Nothing,
      errorMessage = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Reports the status of the operation.
--
-- The operation status can be one of the following:
--
-- -   @RUNNING@ - Report creation is in progress.
--
-- -   @SUCCEEDED@ - Report creation is complete. You can open the report
--     from the Amazon S3 bucket that you specified when you ran
--     @StartReportCreation@.
--
-- -   @FAILED@ - Report creation timed out or the Amazon S3 bucket is not
--     accessible.
--
-- -   @NO REPORT@ - No report was generated in the last 90 days.
describeReportCreationResponse_status :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Core.Text)
describeReportCreationResponse_status = Lens.lens (\DescribeReportCreationResponse' {status} -> status) (\s@DescribeReportCreationResponse' {} a -> s {status = a} :: DescribeReportCreationResponse)

-- | The path to the Amazon S3 bucket where the report was stored on
-- creation.
describeReportCreationResponse_s3Location :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Core.Text)
describeReportCreationResponse_s3Location = Lens.lens (\DescribeReportCreationResponse' {s3Location} -> s3Location) (\s@DescribeReportCreationResponse' {} a -> s {s3Location = a} :: DescribeReportCreationResponse)

-- | Details of the common errors that all operations return.
describeReportCreationResponse_errorMessage :: Lens.Lens' DescribeReportCreationResponse (Core.Maybe Core.Text)
describeReportCreationResponse_errorMessage = Lens.lens (\DescribeReportCreationResponse' {errorMessage} -> errorMessage) (\s@DescribeReportCreationResponse' {} a -> s {errorMessage = a} :: DescribeReportCreationResponse)

-- | The response's http status code.
describeReportCreationResponse_httpStatus :: Lens.Lens' DescribeReportCreationResponse Core.Int
describeReportCreationResponse_httpStatus = Lens.lens (\DescribeReportCreationResponse' {httpStatus} -> httpStatus) (\s@DescribeReportCreationResponse' {} a -> s {httpStatus = a} :: DescribeReportCreationResponse)

instance Core.NFData DescribeReportCreationResponse
