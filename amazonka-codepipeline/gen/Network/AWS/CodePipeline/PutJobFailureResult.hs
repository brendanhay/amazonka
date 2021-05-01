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
-- Module      : Network.AWS.CodePipeline.PutJobFailureResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a job as returned to the pipeline by a job
-- worker. Used for custom actions only.
module Network.AWS.CodePipeline.PutJobFailureResult
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

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest PutJobFailureResult where
  type
    Rs PutJobFailureResult =
      PutJobFailureResultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutJobFailureResultResponse'

instance Prelude.Hashable PutJobFailureResult

instance Prelude.NFData PutJobFailureResult

instance Prelude.ToHeaders PutJobFailureResult where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodePipeline_20150709.PutJobFailureResult" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutJobFailureResult where
  toJSON PutJobFailureResult' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("jobId" Prelude..= jobId),
            Prelude.Just
              ("failureDetails" Prelude..= failureDetails)
          ]
      )

instance Prelude.ToPath PutJobFailureResult where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutJobFailureResult where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutJobFailureResultResponse' smart constructor.
data PutJobFailureResultResponse = PutJobFailureResultResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutJobFailureResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutJobFailureResultResponse ::
  PutJobFailureResultResponse
newPutJobFailureResultResponse =
  PutJobFailureResultResponse'

instance Prelude.NFData PutJobFailureResultResponse
