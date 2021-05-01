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
-- Module      : Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For
-- example, you can use this operation to get the job status.
module Network.AWS.Comprehend.DescribePiiEntitiesDetectionJob
  ( -- * Creating a Request
    DescribePiiEntitiesDetectionJob (..),
    newDescribePiiEntitiesDetectionJob,

    -- * Request Lenses
    describePiiEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribePiiEntitiesDetectionJobResponse (..),
    newDescribePiiEntitiesDetectionJobResponse,

    -- * Response Lenses
    describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties,
    describePiiEntitiesDetectionJobResponse_httpStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePiiEntitiesDetectionJob' smart constructor.
data DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribePiiEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describePiiEntitiesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribePiiEntitiesDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribePiiEntitiesDetectionJob
newDescribePiiEntitiesDetectionJob pJobId_ =
  DescribePiiEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describePiiEntitiesDetectionJob_jobId :: Lens.Lens' DescribePiiEntitiesDetectionJob Prelude.Text
describePiiEntitiesDetectionJob_jobId = Lens.lens (\DescribePiiEntitiesDetectionJob' {jobId} -> jobId) (\s@DescribePiiEntitiesDetectionJob' {} a -> s {jobId = a} :: DescribePiiEntitiesDetectionJob)

instance
  Prelude.AWSRequest
    DescribePiiEntitiesDetectionJob
  where
  type
    Rs DescribePiiEntitiesDetectionJob =
      DescribePiiEntitiesDetectionJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePiiEntitiesDetectionJobResponse'
            Prelude.<$> (x Prelude..?> "PiiEntitiesDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePiiEntitiesDetectionJob

instance
  Prelude.NFData
    DescribePiiEntitiesDetectionJob

instance
  Prelude.ToHeaders
    DescribePiiEntitiesDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Comprehend_20171127.DescribePiiEntitiesDetectionJob" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DescribePiiEntitiesDetectionJob
  where
  toJSON DescribePiiEntitiesDetectionJob' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Prelude..= jobId)]
      )

instance
  Prelude.ToPath
    DescribePiiEntitiesDetectionJob
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DescribePiiEntitiesDetectionJob
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Prelude.Maybe PiiEntitiesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DescribePiiEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'piiEntitiesDetectionJobProperties', 'describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties' - Undocumented member.
--
-- 'httpStatus', 'describePiiEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribePiiEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePiiEntitiesDetectionJobResponse
newDescribePiiEntitiesDetectionJobResponse
  pHttpStatus_ =
    DescribePiiEntitiesDetectionJobResponse'
      { piiEntitiesDetectionJobProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse (Prelude.Maybe PiiEntitiesDetectionJobProperties)
describePiiEntitiesDetectionJobResponse_piiEntitiesDetectionJobProperties = Lens.lens (\DescribePiiEntitiesDetectionJobResponse' {piiEntitiesDetectionJobProperties} -> piiEntitiesDetectionJobProperties) (\s@DescribePiiEntitiesDetectionJobResponse' {} a -> s {piiEntitiesDetectionJobProperties = a} :: DescribePiiEntitiesDetectionJobResponse)

-- | The response's http status code.
describePiiEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' DescribePiiEntitiesDetectionJobResponse Prelude.Int
describePiiEntitiesDetectionJobResponse_httpStatus = Lens.lens (\DescribePiiEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribePiiEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribePiiEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    DescribePiiEntitiesDetectionJobResponse
