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
-- Module      : Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents the failure of a third party job as returned to the pipeline
-- by a job worker. Used for partner actions only.
module Network.AWS.CodePipeline.PutThirdPartyJobFailureResult
  ( -- * Creating a Request
    PutThirdPartyJobFailureResult (..),
    newPutThirdPartyJobFailureResult,

    -- * Request Lenses
    putThirdPartyJobFailureResult_jobId,
    putThirdPartyJobFailureResult_clientToken,
    putThirdPartyJobFailureResult_failureDetails,

    -- * Destructuring the Response
    PutThirdPartyJobFailureResultResponse (..),
    newPutThirdPartyJobFailureResultResponse,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PutThirdPartyJobFailureResult@ action.
--
-- /See:/ 'newPutThirdPartyJobFailureResult' smart constructor.
data PutThirdPartyJobFailureResult = PutThirdPartyJobFailureResult'
  { -- | The ID of the job that failed. This is the same ID returned from
    -- @PollForThirdPartyJobs@.
    jobId :: Core.Text,
    -- | The clientToken portion of the clientId and clientToken pair used to
    -- verify that the calling entity is allowed access to the job and its
    -- details.
    clientToken :: Core.Text,
    -- | Represents information about failure details.
    failureDetails :: FailureDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutThirdPartyJobFailureResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'putThirdPartyJobFailureResult_jobId' - The ID of the job that failed. This is the same ID returned from
-- @PollForThirdPartyJobs@.
--
-- 'clientToken', 'putThirdPartyJobFailureResult_clientToken' - The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
--
-- 'failureDetails', 'putThirdPartyJobFailureResult_failureDetails' - Represents information about failure details.
newPutThirdPartyJobFailureResult ::
  -- | 'jobId'
  Core.Text ->
  -- | 'clientToken'
  Core.Text ->
  -- | 'failureDetails'
  FailureDetails ->
  PutThirdPartyJobFailureResult
newPutThirdPartyJobFailureResult
  pJobId_
  pClientToken_
  pFailureDetails_ =
    PutThirdPartyJobFailureResult'
      { jobId = pJobId_,
        clientToken = pClientToken_,
        failureDetails = pFailureDetails_
      }

-- | The ID of the job that failed. This is the same ID returned from
-- @PollForThirdPartyJobs@.
putThirdPartyJobFailureResult_jobId :: Lens.Lens' PutThirdPartyJobFailureResult Core.Text
putThirdPartyJobFailureResult_jobId = Lens.lens (\PutThirdPartyJobFailureResult' {jobId} -> jobId) (\s@PutThirdPartyJobFailureResult' {} a -> s {jobId = a} :: PutThirdPartyJobFailureResult)

-- | The clientToken portion of the clientId and clientToken pair used to
-- verify that the calling entity is allowed access to the job and its
-- details.
putThirdPartyJobFailureResult_clientToken :: Lens.Lens' PutThirdPartyJobFailureResult Core.Text
putThirdPartyJobFailureResult_clientToken = Lens.lens (\PutThirdPartyJobFailureResult' {clientToken} -> clientToken) (\s@PutThirdPartyJobFailureResult' {} a -> s {clientToken = a} :: PutThirdPartyJobFailureResult)

-- | Represents information about failure details.
putThirdPartyJobFailureResult_failureDetails :: Lens.Lens' PutThirdPartyJobFailureResult FailureDetails
putThirdPartyJobFailureResult_failureDetails = Lens.lens (\PutThirdPartyJobFailureResult' {failureDetails} -> failureDetails) (\s@PutThirdPartyJobFailureResult' {} a -> s {failureDetails = a} :: PutThirdPartyJobFailureResult)

instance
  Core.AWSRequest
    PutThirdPartyJobFailureResult
  where
  type
    AWSResponse PutThirdPartyJobFailureResult =
      PutThirdPartyJobFailureResultResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      PutThirdPartyJobFailureResultResponse'

instance Core.Hashable PutThirdPartyJobFailureResult

instance Core.NFData PutThirdPartyJobFailureResult

instance Core.ToHeaders PutThirdPartyJobFailureResult where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.PutThirdPartyJobFailureResult" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutThirdPartyJobFailureResult where
  toJSON PutThirdPartyJobFailureResult' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("jobId" Core..= jobId),
            Core.Just ("clientToken" Core..= clientToken),
            Core.Just ("failureDetails" Core..= failureDetails)
          ]
      )

instance Core.ToPath PutThirdPartyJobFailureResult where
  toPath = Core.const "/"

instance Core.ToQuery PutThirdPartyJobFailureResult where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutThirdPartyJobFailureResultResponse' smart constructor.
data PutThirdPartyJobFailureResultResponse = PutThirdPartyJobFailureResultResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutThirdPartyJobFailureResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutThirdPartyJobFailureResultResponse ::
  PutThirdPartyJobFailureResultResponse
newPutThirdPartyJobFailureResultResponse =
  PutThirdPartyJobFailureResultResponse'

instance
  Core.NFData
    PutThirdPartyJobFailureResultResponse
