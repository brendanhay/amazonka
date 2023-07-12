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
-- Module      : Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with a PII entities detection job. For
-- example, you can use this operation to get the job status.
module Amazonka.Comprehend.DescribePiiEntitiesDetectionJob
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

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePiiEntitiesDetectionJob' smart constructor.
data DescribePiiEntitiesDetectionJob = DescribePiiEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DescribePiiEntitiesDetectionJob
  where
  type
    AWSResponse DescribePiiEntitiesDetectionJob =
      DescribePiiEntitiesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePiiEntitiesDetectionJobResponse'
            Prelude.<$> (x Data..?> "PiiEntitiesDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribePiiEntitiesDetectionJob
  where
  hashWithSalt
    _salt
    DescribePiiEntitiesDetectionJob' {..} =
      _salt `Prelude.hashWithSalt` jobId

instance
  Prelude.NFData
    DescribePiiEntitiesDetectionJob
  where
  rnf DescribePiiEntitiesDetectionJob' {..} =
    Prelude.rnf jobId

instance
  Data.ToHeaders
    DescribePiiEntitiesDetectionJob
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DescribePiiEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePiiEntitiesDetectionJob where
  toJSON DescribePiiEntitiesDetectionJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Data..= jobId)]
      )

instance Data.ToPath DescribePiiEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePiiEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePiiEntitiesDetectionJobResponse' smart constructor.
data DescribePiiEntitiesDetectionJobResponse = DescribePiiEntitiesDetectionJobResponse'
  { piiEntitiesDetectionJobProperties :: Prelude.Maybe PiiEntitiesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DescribePiiEntitiesDetectionJobResponse' {..} =
    Prelude.rnf piiEntitiesDetectionJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
