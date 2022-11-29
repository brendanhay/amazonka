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
-- Module      : Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a key phrases detection job. Use
-- this operation to get the status of a detection job.
module Amazonka.Comprehend.DescribeKeyPhrasesDetectionJob
  ( -- * Creating a Request
    DescribeKeyPhrasesDetectionJob (..),
    newDescribeKeyPhrasesDetectionJob,

    -- * Request Lenses
    describeKeyPhrasesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeKeyPhrasesDetectionJobResponse (..),
    newDescribeKeyPhrasesDetectionJobResponse,

    -- * Response Lenses
    describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties,
    describeKeyPhrasesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKeyPhrasesDetectionJob' smart constructor.
data DescribeKeyPhrasesDetectionJob = DescribeKeyPhrasesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeyPhrasesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeKeyPhrasesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeKeyPhrasesDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeKeyPhrasesDetectionJob
newDescribeKeyPhrasesDetectionJob pJobId_ =
  DescribeKeyPhrasesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeKeyPhrasesDetectionJob_jobId :: Lens.Lens' DescribeKeyPhrasesDetectionJob Prelude.Text
describeKeyPhrasesDetectionJob_jobId = Lens.lens (\DescribeKeyPhrasesDetectionJob' {jobId} -> jobId) (\s@DescribeKeyPhrasesDetectionJob' {} a -> s {jobId = a} :: DescribeKeyPhrasesDetectionJob)

instance
  Core.AWSRequest
    DescribeKeyPhrasesDetectionJob
  where
  type
    AWSResponse DescribeKeyPhrasesDetectionJob =
      DescribeKeyPhrasesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyPhrasesDetectionJobResponse'
            Prelude.<$> (x Core..?> "KeyPhrasesDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeKeyPhrasesDetectionJob
  where
  hashWithSalt
    _salt
    DescribeKeyPhrasesDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    DescribeKeyPhrasesDetectionJob
  where
  rnf DescribeKeyPhrasesDetectionJob' {..} =
    Prelude.rnf jobId

instance
  Core.ToHeaders
    DescribeKeyPhrasesDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeKeyPhrasesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeKeyPhrasesDetectionJob where
  toJSON DescribeKeyPhrasesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeKeyPhrasesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeKeyPhrasesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeKeyPhrasesDetectionJobResponse' smart constructor.
data DescribeKeyPhrasesDetectionJobResponse = DescribeKeyPhrasesDetectionJobResponse'
  { -- | An object that contains the properties associated with a key phrases
    -- detection job.
    keyPhrasesDetectionJobProperties :: Prelude.Maybe KeyPhrasesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeyPhrasesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPhrasesDetectionJobProperties', 'describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties' - An object that contains the properties associated with a key phrases
-- detection job.
--
-- 'httpStatus', 'describeKeyPhrasesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeKeyPhrasesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeKeyPhrasesDetectionJobResponse
newDescribeKeyPhrasesDetectionJobResponse
  pHttpStatus_ =
    DescribeKeyPhrasesDetectionJobResponse'
      { keyPhrasesDetectionJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that contains the properties associated with a key phrases
-- detection job.
describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse (Prelude.Maybe KeyPhrasesDetectionJobProperties)
describeKeyPhrasesDetectionJobResponse_keyPhrasesDetectionJobProperties = Lens.lens (\DescribeKeyPhrasesDetectionJobResponse' {keyPhrasesDetectionJobProperties} -> keyPhrasesDetectionJobProperties) (\s@DescribeKeyPhrasesDetectionJobResponse' {} a -> s {keyPhrasesDetectionJobProperties = a} :: DescribeKeyPhrasesDetectionJobResponse)

-- | The response's http status code.
describeKeyPhrasesDetectionJobResponse_httpStatus :: Lens.Lens' DescribeKeyPhrasesDetectionJobResponse Prelude.Int
describeKeyPhrasesDetectionJobResponse_httpStatus = Lens.lens (\DescribeKeyPhrasesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyPhrasesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeKeyPhrasesDetectionJobResponse)

instance
  Prelude.NFData
    DescribeKeyPhrasesDetectionJobResponse
  where
  rnf DescribeKeyPhrasesDetectionJobResponse' {..} =
    Prelude.rnf keyPhrasesDetectionJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
