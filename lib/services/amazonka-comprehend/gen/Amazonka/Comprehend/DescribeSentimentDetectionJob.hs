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
-- Module      : Amazonka.Comprehend.DescribeSentimentDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a sentiment detection job. Use this
-- operation to get the status of a detection job.
module Amazonka.Comprehend.DescribeSentimentDetectionJob
  ( -- * Creating a Request
    DescribeSentimentDetectionJob (..),
    newDescribeSentimentDetectionJob,

    -- * Request Lenses
    describeSentimentDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeSentimentDetectionJobResponse (..),
    newDescribeSentimentDetectionJobResponse,

    -- * Response Lenses
    describeSentimentDetectionJobResponse_sentimentDetectionJobProperties,
    describeSentimentDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSentimentDetectionJob' smart constructor.
data DescribeSentimentDetectionJob = DescribeSentimentDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSentimentDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeSentimentDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeSentimentDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeSentimentDetectionJob
newDescribeSentimentDetectionJob pJobId_ =
  DescribeSentimentDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeSentimentDetectionJob_jobId :: Lens.Lens' DescribeSentimentDetectionJob Prelude.Text
describeSentimentDetectionJob_jobId = Lens.lens (\DescribeSentimentDetectionJob' {jobId} -> jobId) (\s@DescribeSentimentDetectionJob' {} a -> s {jobId = a} :: DescribeSentimentDetectionJob)

instance
  Core.AWSRequest
    DescribeSentimentDetectionJob
  where
  type
    AWSResponse DescribeSentimentDetectionJob =
      DescribeSentimentDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSentimentDetectionJobResponse'
            Prelude.<$> (x Data..?> "SentimentDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeSentimentDetectionJob
  where
  hashWithSalt _salt DescribeSentimentDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeSentimentDetectionJob where
  rnf DescribeSentimentDetectionJob' {..} =
    Prelude.rnf jobId

instance Data.ToHeaders DescribeSentimentDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribeSentimentDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSentimentDetectionJob where
  toJSON DescribeSentimentDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribeSentimentDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSentimentDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSentimentDetectionJobResponse' smart constructor.
data DescribeSentimentDetectionJobResponse = DescribeSentimentDetectionJobResponse'
  { -- | An object that contains the properties associated with a sentiment
    -- detection job.
    sentimentDetectionJobProperties :: Prelude.Maybe SentimentDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSentimentDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sentimentDetectionJobProperties', 'describeSentimentDetectionJobResponse_sentimentDetectionJobProperties' - An object that contains the properties associated with a sentiment
-- detection job.
--
-- 'httpStatus', 'describeSentimentDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeSentimentDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSentimentDetectionJobResponse
newDescribeSentimentDetectionJobResponse pHttpStatus_ =
  DescribeSentimentDetectionJobResponse'
    { sentimentDetectionJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with a sentiment
-- detection job.
describeSentimentDetectionJobResponse_sentimentDetectionJobProperties :: Lens.Lens' DescribeSentimentDetectionJobResponse (Prelude.Maybe SentimentDetectionJobProperties)
describeSentimentDetectionJobResponse_sentimentDetectionJobProperties = Lens.lens (\DescribeSentimentDetectionJobResponse' {sentimentDetectionJobProperties} -> sentimentDetectionJobProperties) (\s@DescribeSentimentDetectionJobResponse' {} a -> s {sentimentDetectionJobProperties = a} :: DescribeSentimentDetectionJobResponse)

-- | The response's http status code.
describeSentimentDetectionJobResponse_httpStatus :: Lens.Lens' DescribeSentimentDetectionJobResponse Prelude.Int
describeSentimentDetectionJobResponse_httpStatus = Lens.lens (\DescribeSentimentDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeSentimentDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeSentimentDetectionJobResponse)

instance
  Prelude.NFData
    DescribeSentimentDetectionJobResponse
  where
  rnf DescribeSentimentDetectionJobResponse' {..} =
    Prelude.rnf sentimentDetectionJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
