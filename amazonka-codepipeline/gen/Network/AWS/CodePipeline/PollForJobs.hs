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
-- Module      : Network.AWS.CodePipeline.PollForJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about any jobs for AWS CodePipeline to act on.
-- @PollForJobs@ is valid only for action types with \"Custom\" in the
-- owner field. If the action type contains \"AWS\" or \"ThirdParty\" in
-- the owner field, the @PollForJobs@ action returns an error.
--
-- When this API is called, AWS CodePipeline returns temporary credentials
-- for the S3 bucket used to store artifacts for the pipeline, if the
-- action requires access to that S3 bucket for input or output artifacts.
-- This API also returns any secret values defined for the action.
module Network.AWS.CodePipeline.PollForJobs
  ( -- * Creating a Request
    PollForJobs (..),
    newPollForJobs,

    -- * Request Lenses
    pollForJobs_queryParam,
    pollForJobs_maxBatchSize,
    pollForJobs_actionTypeId,

    -- * Destructuring the Response
    PollForJobsResponse (..),
    newPollForJobsResponse,

    -- * Response Lenses
    pollForJobsResponse_jobs,
    pollForJobsResponse_httpStatus,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @PollForJobs@ action.
--
-- /See:/ 'newPollForJobs' smart constructor.
data PollForJobs = PollForJobs'
  { -- | A map of property names and values. For an action type with no queryable
    -- properties, this value must be null or an empty map. For an action type
    -- with a queryable property, you must supply that property as a key in the
    -- map. Only jobs whose action configuration matches the mapped value are
    -- returned.
    queryParam :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Core.Maybe Core.Natural,
    -- | Represents information about an action type.
    actionTypeId :: ActionTypeId
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PollForJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryParam', 'pollForJobs_queryParam' - A map of property names and values. For an action type with no queryable
-- properties, this value must be null or an empty map. For an action type
-- with a queryable property, you must supply that property as a key in the
-- map. Only jobs whose action configuration matches the mapped value are
-- returned.
--
-- 'maxBatchSize', 'pollForJobs_maxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
--
-- 'actionTypeId', 'pollForJobs_actionTypeId' - Represents information about an action type.
newPollForJobs ::
  -- | 'actionTypeId'
  ActionTypeId ->
  PollForJobs
newPollForJobs pActionTypeId_ =
  PollForJobs'
    { queryParam = Core.Nothing,
      maxBatchSize = Core.Nothing,
      actionTypeId = pActionTypeId_
    }

-- | A map of property names and values. For an action type with no queryable
-- properties, this value must be null or an empty map. For an action type
-- with a queryable property, you must supply that property as a key in the
-- map. Only jobs whose action configuration matches the mapped value are
-- returned.
pollForJobs_queryParam :: Lens.Lens' PollForJobs (Core.Maybe (Core.HashMap Core.Text Core.Text))
pollForJobs_queryParam = Lens.lens (\PollForJobs' {queryParam} -> queryParam) (\s@PollForJobs' {} a -> s {queryParam = a} :: PollForJobs) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of jobs to return in a poll for jobs call.
pollForJobs_maxBatchSize :: Lens.Lens' PollForJobs (Core.Maybe Core.Natural)
pollForJobs_maxBatchSize = Lens.lens (\PollForJobs' {maxBatchSize} -> maxBatchSize) (\s@PollForJobs' {} a -> s {maxBatchSize = a} :: PollForJobs)

-- | Represents information about an action type.
pollForJobs_actionTypeId :: Lens.Lens' PollForJobs ActionTypeId
pollForJobs_actionTypeId = Lens.lens (\PollForJobs' {actionTypeId} -> actionTypeId) (\s@PollForJobs' {} a -> s {actionTypeId = a} :: PollForJobs)

instance Core.AWSRequest PollForJobs where
  type AWSResponse PollForJobs = PollForJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForJobsResponse'
            Core.<$> (x Core..?> "jobs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PollForJobs

instance Core.NFData PollForJobs

instance Core.ToHeaders PollForJobs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.PollForJobs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PollForJobs where
  toJSON PollForJobs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("queryParam" Core..=) Core.<$> queryParam,
            ("maxBatchSize" Core..=) Core.<$> maxBatchSize,
            Core.Just ("actionTypeId" Core..= actionTypeId)
          ]
      )

instance Core.ToPath PollForJobs where
  toPath = Core.const "/"

instance Core.ToQuery PollForJobs where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @PollForJobs@ action.
--
-- /See:/ 'newPollForJobsResponse' smart constructor.
data PollForJobsResponse = PollForJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Core.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PollForJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobs', 'pollForJobsResponse_jobs' - Information about the jobs to take action on.
--
-- 'httpStatus', 'pollForJobsResponse_httpStatus' - The response's http status code.
newPollForJobsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PollForJobsResponse
newPollForJobsResponse pHttpStatus_ =
  PollForJobsResponse'
    { jobs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the jobs to take action on.
pollForJobsResponse_jobs :: Lens.Lens' PollForJobsResponse (Core.Maybe [Job])
pollForJobsResponse_jobs = Lens.lens (\PollForJobsResponse' {jobs} -> jobs) (\s@PollForJobsResponse' {} a -> s {jobs = a} :: PollForJobsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
pollForJobsResponse_httpStatus :: Lens.Lens' PollForJobsResponse Core.Int
pollForJobsResponse_httpStatus = Lens.lens (\PollForJobsResponse' {httpStatus} -> httpStatus) (\s@PollForJobsResponse' {} a -> s {httpStatus = a} :: PollForJobsResponse)

instance Core.NFData PollForJobsResponse
