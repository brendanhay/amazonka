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
-- Module      : Amazonka.LookoutVision.DescribeModelPackagingJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon Lookout for Vision model packaging job.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DescribeModelPackagingJob@ operation.
--
-- For more information, see /Using your Amazon Lookout for Vision model on
-- an edge device/ in the Amazon Lookout for Vision Developer Guide.
module Amazonka.LookoutVision.DescribeModelPackagingJob
  ( -- * Creating a Request
    DescribeModelPackagingJob (..),
    newDescribeModelPackagingJob,

    -- * Request Lenses
    describeModelPackagingJob_projectName,
    describeModelPackagingJob_jobName,

    -- * Destructuring the Response
    DescribeModelPackagingJobResponse (..),
    newDescribeModelPackagingJobResponse,

    -- * Response Lenses
    describeModelPackagingJobResponse_modelPackagingDescription,
    describeModelPackagingJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeModelPackagingJob' smart constructor.
data DescribeModelPackagingJob = DescribeModelPackagingJob'
  { -- | The name of the project that contains the model packaging job that you
    -- want to describe.
    projectName :: Prelude.Text,
    -- | The job name for the model packaging job.
    jobName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackagingJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'describeModelPackagingJob_projectName' - The name of the project that contains the model packaging job that you
-- want to describe.
--
-- 'jobName', 'describeModelPackagingJob_jobName' - The job name for the model packaging job.
newDescribeModelPackagingJob ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'jobName'
  Prelude.Text ->
  DescribeModelPackagingJob
newDescribeModelPackagingJob pProjectName_ pJobName_ =
  DescribeModelPackagingJob'
    { projectName =
        pProjectName_,
      jobName = pJobName_
    }

-- | The name of the project that contains the model packaging job that you
-- want to describe.
describeModelPackagingJob_projectName :: Lens.Lens' DescribeModelPackagingJob Prelude.Text
describeModelPackagingJob_projectName = Lens.lens (\DescribeModelPackagingJob' {projectName} -> projectName) (\s@DescribeModelPackagingJob' {} a -> s {projectName = a} :: DescribeModelPackagingJob)

-- | The job name for the model packaging job.
describeModelPackagingJob_jobName :: Lens.Lens' DescribeModelPackagingJob Prelude.Text
describeModelPackagingJob_jobName = Lens.lens (\DescribeModelPackagingJob' {jobName} -> jobName) (\s@DescribeModelPackagingJob' {} a -> s {jobName = a} :: DescribeModelPackagingJob)

instance Core.AWSRequest DescribeModelPackagingJob where
  type
    AWSResponse DescribeModelPackagingJob =
      DescribeModelPackagingJobResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelPackagingJobResponse'
            Prelude.<$> (x Core..?> "ModelPackagingDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeModelPackagingJob where
  hashWithSalt _salt DescribeModelPackagingJob' {..} =
    _salt `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData DescribeModelPackagingJob where
  rnf DescribeModelPackagingJob' {..} =
    Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf jobName

instance Core.ToHeaders DescribeModelPackagingJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeModelPackagingJob where
  toPath DescribeModelPackagingJob' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Core.toBS projectName,
        "/modelpackagingjobs/",
        Core.toBS jobName
      ]

instance Core.ToQuery DescribeModelPackagingJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelPackagingJobResponse' smart constructor.
data DescribeModelPackagingJobResponse = DescribeModelPackagingJobResponse'
  { -- | The description of the model packaging job.
    modelPackagingDescription :: Prelude.Maybe ModelPackagingDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelPackagingJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackagingDescription', 'describeModelPackagingJobResponse_modelPackagingDescription' - The description of the model packaging job.
--
-- 'httpStatus', 'describeModelPackagingJobResponse_httpStatus' - The response's http status code.
newDescribeModelPackagingJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeModelPackagingJobResponse
newDescribeModelPackagingJobResponse pHttpStatus_ =
  DescribeModelPackagingJobResponse'
    { modelPackagingDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the model packaging job.
describeModelPackagingJobResponse_modelPackagingDescription :: Lens.Lens' DescribeModelPackagingJobResponse (Prelude.Maybe ModelPackagingDescription)
describeModelPackagingJobResponse_modelPackagingDescription = Lens.lens (\DescribeModelPackagingJobResponse' {modelPackagingDescription} -> modelPackagingDescription) (\s@DescribeModelPackagingJobResponse' {} a -> s {modelPackagingDescription = a} :: DescribeModelPackagingJobResponse)

-- | The response's http status code.
describeModelPackagingJobResponse_httpStatus :: Lens.Lens' DescribeModelPackagingJobResponse Prelude.Int
describeModelPackagingJobResponse_httpStatus = Lens.lens (\DescribeModelPackagingJobResponse' {httpStatus} -> httpStatus) (\s@DescribeModelPackagingJobResponse' {} a -> s {httpStatus = a} :: DescribeModelPackagingJobResponse)

instance
  Prelude.NFData
    DescribeModelPackagingJobResponse
  where
  rnf DescribeModelPackagingJobResponse' {..} =
    Prelude.rnf modelPackagingDescription
      `Prelude.seq` Prelude.rnf httpStatus
