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
-- Module      : Network.AWS.Comprehend.DescribeDocumentClassificationJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a document classification job. Use
-- this operation to get the status of a classification job.
module Network.AWS.Comprehend.DescribeDocumentClassificationJob
  ( -- * Creating a Request
    DescribeDocumentClassificationJob (..),
    newDescribeDocumentClassificationJob,

    -- * Request Lenses
    describeDocumentClassificationJob_jobId,

    -- * Destructuring the Response
    DescribeDocumentClassificationJobResponse (..),
    newDescribeDocumentClassificationJobResponse,

    -- * Response Lenses
    describeDocumentClassificationJobResponse_documentClassificationJobProperties,
    describeDocumentClassificationJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDocumentClassificationJob' smart constructor.
data DescribeDocumentClassificationJob = DescribeDocumentClassificationJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentClassificationJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeDocumentClassificationJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeDocumentClassificationJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeDocumentClassificationJob
newDescribeDocumentClassificationJob pJobId_ =
  DescribeDocumentClassificationJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeDocumentClassificationJob_jobId :: Lens.Lens' DescribeDocumentClassificationJob Prelude.Text
describeDocumentClassificationJob_jobId = Lens.lens (\DescribeDocumentClassificationJob' {jobId} -> jobId) (\s@DescribeDocumentClassificationJob' {} a -> s {jobId = a} :: DescribeDocumentClassificationJob)

instance
  Core.AWSRequest
    DescribeDocumentClassificationJob
  where
  type
    AWSResponse DescribeDocumentClassificationJob =
      DescribeDocumentClassificationJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentClassificationJobResponse'
            Prelude.<$> (x Core..?> "DocumentClassificationJobProperties")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDocumentClassificationJob

instance
  Prelude.NFData
    DescribeDocumentClassificationJob

instance
  Core.ToHeaders
    DescribeDocumentClassificationJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeDocumentClassificationJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    DescribeDocumentClassificationJob
  where
  toJSON DescribeDocumentClassificationJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance
  Core.ToPath
    DescribeDocumentClassificationJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDocumentClassificationJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDocumentClassificationJobResponse' smart constructor.
data DescribeDocumentClassificationJobResponse = DescribeDocumentClassificationJobResponse'
  { -- | An object that describes the properties associated with the document
    -- classification job.
    documentClassificationJobProperties :: Prelude.Maybe DocumentClassificationJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentClassificationJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassificationJobProperties', 'describeDocumentClassificationJobResponse_documentClassificationJobProperties' - An object that describes the properties associated with the document
-- classification job.
--
-- 'httpStatus', 'describeDocumentClassificationJobResponse_httpStatus' - The response's http status code.
newDescribeDocumentClassificationJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDocumentClassificationJobResponse
newDescribeDocumentClassificationJobResponse
  pHttpStatus_ =
    DescribeDocumentClassificationJobResponse'
      { documentClassificationJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that describes the properties associated with the document
-- classification job.
describeDocumentClassificationJobResponse_documentClassificationJobProperties :: Lens.Lens' DescribeDocumentClassificationJobResponse (Prelude.Maybe DocumentClassificationJobProperties)
describeDocumentClassificationJobResponse_documentClassificationJobProperties = Lens.lens (\DescribeDocumentClassificationJobResponse' {documentClassificationJobProperties} -> documentClassificationJobProperties) (\s@DescribeDocumentClassificationJobResponse' {} a -> s {documentClassificationJobProperties = a} :: DescribeDocumentClassificationJobResponse)

-- | The response's http status code.
describeDocumentClassificationJobResponse_httpStatus :: Lens.Lens' DescribeDocumentClassificationJobResponse Prelude.Int
describeDocumentClassificationJobResponse_httpStatus = Lens.lens (\DescribeDocumentClassificationJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentClassificationJobResponse' {} a -> s {httpStatus = a} :: DescribeDocumentClassificationJobResponse)

instance
  Prelude.NFData
    DescribeDocumentClassificationJobResponse
