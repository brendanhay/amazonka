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
-- Module      : Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a dominant language detection job.
-- Use this operation to get the status of a detection job.
module Network.AWS.Comprehend.DescribeDominantLanguageDetectionJob
  ( -- * Creating a Request
    DescribeDominantLanguageDetectionJob (..),
    newDescribeDominantLanguageDetectionJob,

    -- * Request Lenses
    describeDominantLanguageDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeDominantLanguageDetectionJobResponse (..),
    newDescribeDominantLanguageDetectionJobResponse,

    -- * Response Lenses
    describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties,
    describeDominantLanguageDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDominantLanguageDetectionJob' smart constructor.
data DescribeDominantLanguageDetectionJob = DescribeDominantLanguageDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDominantLanguageDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeDominantLanguageDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeDominantLanguageDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeDominantLanguageDetectionJob
newDescribeDominantLanguageDetectionJob pJobId_ =
  DescribeDominantLanguageDetectionJob'
    { jobId =
        pJobId_
    }

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeDominantLanguageDetectionJob_jobId :: Lens.Lens' DescribeDominantLanguageDetectionJob Prelude.Text
describeDominantLanguageDetectionJob_jobId = Lens.lens (\DescribeDominantLanguageDetectionJob' {jobId} -> jobId) (\s@DescribeDominantLanguageDetectionJob' {} a -> s {jobId = a} :: DescribeDominantLanguageDetectionJob)

instance
  Core.AWSRequest
    DescribeDominantLanguageDetectionJob
  where
  type
    AWSResponse DescribeDominantLanguageDetectionJob =
      DescribeDominantLanguageDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDominantLanguageDetectionJobResponse'
            Prelude.<$> (x Core..?> "DominantLanguageDetectionJobProperties")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDominantLanguageDetectionJob

instance
  Prelude.NFData
    DescribeDominantLanguageDetectionJob

instance
  Core.ToHeaders
    DescribeDominantLanguageDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeDominantLanguageDetectionJob" ::
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
    DescribeDominantLanguageDetectionJob
  where
  toJSON DescribeDominantLanguageDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance
  Core.ToPath
    DescribeDominantLanguageDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DescribeDominantLanguageDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDominantLanguageDetectionJobResponse' smart constructor.
data DescribeDominantLanguageDetectionJobResponse = DescribeDominantLanguageDetectionJobResponse'
  { -- | An object that contains the properties associated with a dominant
    -- language detection job.
    dominantLanguageDetectionJobProperties :: Prelude.Maybe DominantLanguageDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDominantLanguageDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dominantLanguageDetectionJobProperties', 'describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties' - An object that contains the properties associated with a dominant
-- language detection job.
--
-- 'httpStatus', 'describeDominantLanguageDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeDominantLanguageDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDominantLanguageDetectionJobResponse
newDescribeDominantLanguageDetectionJobResponse
  pHttpStatus_ =
    DescribeDominantLanguageDetectionJobResponse'
      { dominantLanguageDetectionJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object that contains the properties associated with a dominant
-- language detection job.
describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse (Prelude.Maybe DominantLanguageDetectionJobProperties)
describeDominantLanguageDetectionJobResponse_dominantLanguageDetectionJobProperties = Lens.lens (\DescribeDominantLanguageDetectionJobResponse' {dominantLanguageDetectionJobProperties} -> dominantLanguageDetectionJobProperties) (\s@DescribeDominantLanguageDetectionJobResponse' {} a -> s {dominantLanguageDetectionJobProperties = a} :: DescribeDominantLanguageDetectionJobResponse)

-- | The response's http status code.
describeDominantLanguageDetectionJobResponse_httpStatus :: Lens.Lens' DescribeDominantLanguageDetectionJobResponse Prelude.Int
describeDominantLanguageDetectionJobResponse_httpStatus = Lens.lens (\DescribeDominantLanguageDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeDominantLanguageDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeDominantLanguageDetectionJobResponse)

instance
  Prelude.NFData
    DescribeDominantLanguageDetectionJobResponse
