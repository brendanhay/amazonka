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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeReportCreation' smart constructor.
data DescribeReportCreation = DescribeReportCreation'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribeReportCreation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeReportCreation ::
  DescribeReportCreation
newDescribeReportCreation = DescribeReportCreation'

instance Prelude.AWSRequest DescribeReportCreation where
  type
    Rs DescribeReportCreation =
      DescribeReportCreationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeReportCreationResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "S3Location")
            Prelude.<*> (x Prelude..?> "ErrorMessage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeReportCreation

instance Prelude.NFData DescribeReportCreation

instance Prelude.ToHeaders DescribeReportCreation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ResourceGroupsTaggingAPI_20170126.DescribeReportCreation" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeReportCreation where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DescribeReportCreation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeReportCreation where
  toQuery = Prelude.const Prelude.mempty

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
    status :: Prelude.Maybe Prelude.Text,
    -- | The path to the Amazon S3 bucket where the report was stored on
    -- creation.
    s3Location :: Prelude.Maybe Prelude.Text,
    -- | Details of the common errors that all operations return.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeReportCreationResponse
newDescribeReportCreationResponse pHttpStatus_ =
  DescribeReportCreationResponse'
    { status =
        Prelude.Nothing,
      s3Location = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
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
describeReportCreationResponse_status :: Lens.Lens' DescribeReportCreationResponse (Prelude.Maybe Prelude.Text)
describeReportCreationResponse_status = Lens.lens (\DescribeReportCreationResponse' {status} -> status) (\s@DescribeReportCreationResponse' {} a -> s {status = a} :: DescribeReportCreationResponse)

-- | The path to the Amazon S3 bucket where the report was stored on
-- creation.
describeReportCreationResponse_s3Location :: Lens.Lens' DescribeReportCreationResponse (Prelude.Maybe Prelude.Text)
describeReportCreationResponse_s3Location = Lens.lens (\DescribeReportCreationResponse' {s3Location} -> s3Location) (\s@DescribeReportCreationResponse' {} a -> s {s3Location = a} :: DescribeReportCreationResponse)

-- | Details of the common errors that all operations return.
describeReportCreationResponse_errorMessage :: Lens.Lens' DescribeReportCreationResponse (Prelude.Maybe Prelude.Text)
describeReportCreationResponse_errorMessage = Lens.lens (\DescribeReportCreationResponse' {errorMessage} -> errorMessage) (\s@DescribeReportCreationResponse' {} a -> s {errorMessage = a} :: DescribeReportCreationResponse)

-- | The response's http status code.
describeReportCreationResponse_httpStatus :: Lens.Lens' DescribeReportCreationResponse Prelude.Int
describeReportCreationResponse_httpStatus = Lens.lens (\DescribeReportCreationResponse' {httpStatus} -> httpStatus) (\s@DescribeReportCreationResponse' {} a -> s {httpStatus = a} :: DescribeReportCreationResponse)

instance
  Prelude.NFData
    DescribeReportCreationResponse
