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
-- Module      : Network.AWS.CodePipeline.PollForThirdPartyJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Determines whether there are any third party jobs for a job worker to
-- act on. Used for partner actions only.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the S3 bucket used to store artifacts for the pipeline, if the
-- action requires access to that S3 bucket for input or output artifacts.
module Network.AWS.CodePipeline.PollForThirdPartyJobs
  ( -- * Creating a Request
    PollForThirdPartyJobs (..),
    newPollForThirdPartyJobs,

    -- * Request Lenses
    pollForThirdPartyJobs_maxBatchSize,
    pollForThirdPartyJobs_actionTypeId,

    -- * Destructuring the Response
    PollForThirdPartyJobsResponse (..),
    newPollForThirdPartyJobsResponse,

    -- * Response Lenses
    pollForThirdPartyJobsResponse_jobs,
    pollForThirdPartyJobsResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'newPollForThirdPartyJobs' smart constructor.
data PollForThirdPartyJobs = PollForThirdPartyJobs'
  { -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | Represents information about an action type.
    actionTypeId :: ActionTypeId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForThirdPartyJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBatchSize', 'pollForThirdPartyJobs_maxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
--
-- 'actionTypeId', 'pollForThirdPartyJobs_actionTypeId' - Represents information about an action type.
newPollForThirdPartyJobs ::
  -- | 'actionTypeId'
  ActionTypeId ->
  PollForThirdPartyJobs
newPollForThirdPartyJobs pActionTypeId_ =
  PollForThirdPartyJobs'
    { maxBatchSize =
        Prelude.Nothing,
      actionTypeId = pActionTypeId_
    }

-- | The maximum number of jobs to return in a poll for jobs call.
pollForThirdPartyJobs_maxBatchSize :: Lens.Lens' PollForThirdPartyJobs (Prelude.Maybe Prelude.Natural)
pollForThirdPartyJobs_maxBatchSize = Lens.lens (\PollForThirdPartyJobs' {maxBatchSize} -> maxBatchSize) (\s@PollForThirdPartyJobs' {} a -> s {maxBatchSize = a} :: PollForThirdPartyJobs)

-- | Represents information about an action type.
pollForThirdPartyJobs_actionTypeId :: Lens.Lens' PollForThirdPartyJobs ActionTypeId
pollForThirdPartyJobs_actionTypeId = Lens.lens (\PollForThirdPartyJobs' {actionTypeId} -> actionTypeId) (\s@PollForThirdPartyJobs' {} a -> s {actionTypeId = a} :: PollForThirdPartyJobs)

instance Core.AWSRequest PollForThirdPartyJobs where
  type
    AWSResponse PollForThirdPartyJobs =
      PollForThirdPartyJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForThirdPartyJobsResponse'
            Prelude.<$> (x Core..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PollForThirdPartyJobs

instance Prelude.NFData PollForThirdPartyJobs

instance Core.ToHeaders PollForThirdPartyJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.PollForThirdPartyJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PollForThirdPartyJobs where
  toJSON PollForThirdPartyJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxBatchSize" Core..=) Prelude.<$> maxBatchSize,
            Prelude.Just ("actionTypeId" Core..= actionTypeId)
          ]
      )

instance Core.ToPath PollForThirdPartyJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery PollForThirdPartyJobs where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PollForThirdPartyJobs@ action.
--
-- /See:/ 'newPollForThirdPartyJobsResponse' smart constructor.
data PollForThirdPartyJobsResponse = PollForThirdPartyJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Prelude.Maybe [ThirdPartyJob],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForThirdPartyJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'pollForThirdPartyJobsResponse_jobs' - Information about the jobs to take action on.
--
-- 'httpStatus', 'pollForThirdPartyJobsResponse_httpStatus' - The response's http status code.
newPollForThirdPartyJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PollForThirdPartyJobsResponse
newPollForThirdPartyJobsResponse pHttpStatus_ =
  PollForThirdPartyJobsResponse'
    { jobs =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the jobs to take action on.
pollForThirdPartyJobsResponse_jobs :: Lens.Lens' PollForThirdPartyJobsResponse (Prelude.Maybe [ThirdPartyJob])
pollForThirdPartyJobsResponse_jobs = Lens.lens (\PollForThirdPartyJobsResponse' {jobs} -> jobs) (\s@PollForThirdPartyJobsResponse' {} a -> s {jobs = a} :: PollForThirdPartyJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
pollForThirdPartyJobsResponse_httpStatus :: Lens.Lens' PollForThirdPartyJobsResponse Prelude.Int
pollForThirdPartyJobsResponse_httpStatus = Lens.lens (\PollForThirdPartyJobsResponse' {httpStatus} -> httpStatus) (\s@PollForThirdPartyJobsResponse' {} a -> s {httpStatus = a} :: PollForThirdPartyJobsResponse)

instance Prelude.NFData PollForThirdPartyJobsResponse
