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
-- Module      : Amazonka.Comprehend.DescribeEntitiesDetectionJob
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the properties associated with an entities detection job. Use this
-- operation to get the status of a detection job.
module Amazonka.Comprehend.DescribeEntitiesDetectionJob
  ( -- * Creating a Request
    DescribeEntitiesDetectionJob (..),
    newDescribeEntitiesDetectionJob,

    -- * Request Lenses
    describeEntitiesDetectionJob_jobId,

    -- * Destructuring the Response
    DescribeEntitiesDetectionJobResponse (..),
    newDescribeEntitiesDetectionJobResponse,

    -- * Response Lenses
    describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties,
    describeEntitiesDetectionJobResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEntitiesDetectionJob' smart constructor.
data DescribeEntitiesDetectionJob = DescribeEntitiesDetectionJob'
  { -- | The identifier that Amazon Comprehend generated for the job. The
    -- operation returns this identifier in its response.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'describeEntitiesDetectionJob_jobId' - The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
newDescribeEntitiesDetectionJob ::
  -- | 'jobId'
  Prelude.Text ->
  DescribeEntitiesDetectionJob
newDescribeEntitiesDetectionJob pJobId_ =
  DescribeEntitiesDetectionJob' {jobId = pJobId_}

-- | The identifier that Amazon Comprehend generated for the job. The
-- operation returns this identifier in its response.
describeEntitiesDetectionJob_jobId :: Lens.Lens' DescribeEntitiesDetectionJob Prelude.Text
describeEntitiesDetectionJob_jobId = Lens.lens (\DescribeEntitiesDetectionJob' {jobId} -> jobId) (\s@DescribeEntitiesDetectionJob' {} a -> s {jobId = a} :: DescribeEntitiesDetectionJob)

instance Core.AWSRequest DescribeEntitiesDetectionJob where
  type
    AWSResponse DescribeEntitiesDetectionJob =
      DescribeEntitiesDetectionJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEntitiesDetectionJobResponse'
            Prelude.<$> (x Core..?> "EntitiesDetectionJobProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeEntitiesDetectionJob
  where
  hashWithSalt _salt DescribeEntitiesDetectionJob' {..} =
    _salt `Prelude.hashWithSalt` jobId

instance Prelude.NFData DescribeEntitiesDetectionJob where
  rnf DescribeEntitiesDetectionJob' {..} =
    Prelude.rnf jobId

instance Core.ToHeaders DescribeEntitiesDetectionJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Comprehend_20171127.DescribeEntitiesDetectionJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEntitiesDetectionJob where
  toJSON DescribeEntitiesDetectionJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("JobId" Core..= jobId)]
      )

instance Core.ToPath DescribeEntitiesDetectionJob where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEntitiesDetectionJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEntitiesDetectionJobResponse' smart constructor.
data DescribeEntitiesDetectionJobResponse = DescribeEntitiesDetectionJobResponse'
  { -- | An object that contains the properties associated with an entities
    -- detection job.
    entitiesDetectionJobProperties :: Prelude.Maybe EntitiesDetectionJobProperties,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEntitiesDetectionJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitiesDetectionJobProperties', 'describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties' - An object that contains the properties associated with an entities
-- detection job.
--
-- 'httpStatus', 'describeEntitiesDetectionJobResponse_httpStatus' - The response's http status code.
newDescribeEntitiesDetectionJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEntitiesDetectionJobResponse
newDescribeEntitiesDetectionJobResponse pHttpStatus_ =
  DescribeEntitiesDetectionJobResponse'
    { entitiesDetectionJobProperties =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains the properties associated with an entities
-- detection job.
describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties :: Lens.Lens' DescribeEntitiesDetectionJobResponse (Prelude.Maybe EntitiesDetectionJobProperties)
describeEntitiesDetectionJobResponse_entitiesDetectionJobProperties = Lens.lens (\DescribeEntitiesDetectionJobResponse' {entitiesDetectionJobProperties} -> entitiesDetectionJobProperties) (\s@DescribeEntitiesDetectionJobResponse' {} a -> s {entitiesDetectionJobProperties = a} :: DescribeEntitiesDetectionJobResponse)

-- | The response's http status code.
describeEntitiesDetectionJobResponse_httpStatus :: Lens.Lens' DescribeEntitiesDetectionJobResponse Prelude.Int
describeEntitiesDetectionJobResponse_httpStatus = Lens.lens (\DescribeEntitiesDetectionJobResponse' {httpStatus} -> httpStatus) (\s@DescribeEntitiesDetectionJobResponse' {} a -> s {httpStatus = a} :: DescribeEntitiesDetectionJobResponse)

instance
  Prelude.NFData
    DescribeEntitiesDetectionJobResponse
  where
  rnf DescribeEntitiesDetectionJobResponse' {..} =
    Prelude.rnf entitiesDetectionJobProperties
      `Prelude.seq` Prelude.rnf httpStatus
