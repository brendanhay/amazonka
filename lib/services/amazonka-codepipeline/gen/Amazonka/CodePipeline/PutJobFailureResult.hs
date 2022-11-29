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
-- Module      : Amazonka.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job
-- worker. Used for custom actions only.
module Amazonka.CodePipeline.PutJobFailureResult
  ( -- * Creating a Request
    PutJobFailureResult (..),
    newPutJobFailureResult,

    -- * Request Lenses
    putJobFailureResult_jobId,
    putJobFailureResult_failureDetails,

    -- * Destructuring the Response
    PutJobFailureResultResponse (..),
    newPutJobFailureResultResponse,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @PutJobFailureResult@ action.
--
-- /See:/ 'newPutJobFailureResult' smart constructor.
data PutJobFailureResult = PutJobFailureResult'
  { -- | The unique system-generated ID of the job that failed. This is the same
    -- ID returned from @PollForJobs@.
    jobId :: Prelude.Text,
    -- | The details about the failure of a job.
    failureDetails :: FailureDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutJobFailureResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'putJobFailureResult_jobId' - The unique system-generated ID of the job that failed. This is the same
-- ID returned from @PollForJobs@.
--
-- 'failureDetails', 'putJobFailureResult_failureDetails' - The details about the failure of a job.
newPutJobFailureResult ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'failureDetails'
  FailureDetails ->
  PutJobFailureResult
newPutJobFailureResult pJobId_ pFailureDetails_ =
  PutJobFailureResult'
    { jobId = pJobId_,
      failureDetails = pFailureDetails_
    }

-- | The unique system-generated ID of the job that failed. This is the same
-- ID returned from @PollForJobs@.
putJobFailureResult_jobId :: Lens.Lens' PutJobFailureResult Prelude.Text
putJobFailureResult_jobId = Lens.lens (\PutJobFailureResult' {jobId} -> jobId) (\s@PutJobFailureResult' {} a -> s {jobId = a} :: PutJobFailureResult)

-- | The details about the failure of a job.
putJobFailureResult_failureDetails :: Lens.Lens' PutJobFailureResult FailureDetails
putJobFailureResult_failureDetails = Lens.lens (\PutJobFailureResult' {failureDetails} -> failureDetails) (\s@PutJobFailureResult' {} a -> s {failureDetails = a} :: PutJobFailureResult)

instance Core.AWSRequest PutJobFailureResult where
  type
    AWSResponse PutJobFailureResult =
      PutJobFailureResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutJobFailureResultResponse'

instance Prelude.Hashable PutJobFailureResult where
  hashWithSalt _salt PutJobFailureResult' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` failureDetails

instance Prelude.NFData PutJobFailureResult where
  rnf PutJobFailureResult' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf failureDetails

instance Core.ToHeaders PutJobFailureResult where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.PutJobFailureResult" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutJobFailureResult where
  toJSON PutJobFailureResult' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Core..= jobId),
            Prelude.Just
              ("failureDetails" Core..= failureDetails)
          ]
      )

instance Core.ToPath PutJobFailureResult where
  toPath = Prelude.const "/"

instance Core.ToQuery PutJobFailureResult where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse = PutJobFailureResultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutJobFailureResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutJobFailureResultResponse ::
  PutJobFailureResultResponse
newPutJobFailureResultResponse =
  PutJobFailureResultResponse'

instance Prelude.NFData PutJobFailureResultResponse where
  rnf _ = ()
