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
-- Module      : Amazonka.CodePipeline.PollForJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CodePipeline.PollForJobs
  ( -- * Creating a Request
    PollForJobs (..),
    newPollForJobs,

    -- * Request Lenses
    pollForJobs_maxBatchSize,
    pollForJobs_queryParam,
    pollForJobs_actionTypeId,

    -- * Destructuring the Response
    PollForJobsResponse (..),
    newPollForJobsResponse,

    -- * Response Lenses
    pollForJobsResponse_jobs,
    pollForJobsResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @PollForJobs@ action.
--
-- /See:/ 'newPollForJobs' smart constructor.
data PollForJobs = PollForJobs'
  { -- | The maximum number of jobs to return in a poll for jobs call.
    maxBatchSize :: Prelude.Maybe Prelude.Natural,
    -- | A map of property names and values. For an action type with no queryable
    -- properties, this value must be null or an empty map. For an action type
    -- with a queryable property, you must supply that property as a key in the
    -- map. Only jobs whose action configuration matches the mapped value are
    -- returned.
    queryParam :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Represents information about an action type.
    actionTypeId :: ActionTypeId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PollForJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxBatchSize', 'pollForJobs_maxBatchSize' - The maximum number of jobs to return in a poll for jobs call.
--
-- 'queryParam', 'pollForJobs_queryParam' - A map of property names and values. For an action type with no queryable
-- properties, this value must be null or an empty map. For an action type
-- with a queryable property, you must supply that property as a key in the
-- map. Only jobs whose action configuration matches the mapped value are
-- returned.
--
-- 'actionTypeId', 'pollForJobs_actionTypeId' - Represents information about an action type.
newPollForJobs ::
  -- | 'actionTypeId'
  ActionTypeId ->
  PollForJobs
newPollForJobs pActionTypeId_ =
  PollForJobs'
    { maxBatchSize = Prelude.Nothing,
      queryParam = Prelude.Nothing,
      actionTypeId = pActionTypeId_
    }

-- | The maximum number of jobs to return in a poll for jobs call.
pollForJobs_maxBatchSize :: Lens.Lens' PollForJobs (Prelude.Maybe Prelude.Natural)
pollForJobs_maxBatchSize = Lens.lens (\PollForJobs' {maxBatchSize} -> maxBatchSize) (\s@PollForJobs' {} a -> s {maxBatchSize = a} :: PollForJobs)

-- | A map of property names and values. For an action type with no queryable
-- properties, this value must be null or an empty map. For an action type
-- with a queryable property, you must supply that property as a key in the
-- map. Only jobs whose action configuration matches the mapped value are
-- returned.
pollForJobs_queryParam :: Lens.Lens' PollForJobs (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
pollForJobs_queryParam = Lens.lens (\PollForJobs' {queryParam} -> queryParam) (\s@PollForJobs' {} a -> s {queryParam = a} :: PollForJobs) Prelude.. Lens.mapping Lens.coerced

-- | Represents information about an action type.
pollForJobs_actionTypeId :: Lens.Lens' PollForJobs ActionTypeId
pollForJobs_actionTypeId = Lens.lens (\PollForJobs' {actionTypeId} -> actionTypeId) (\s@PollForJobs' {} a -> s {actionTypeId = a} :: PollForJobs)

instance Core.AWSRequest PollForJobs where
  type AWSResponse PollForJobs = PollForJobsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PollForJobsResponse'
            Prelude.<$> (x Data..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PollForJobs where
  hashWithSalt _salt PollForJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxBatchSize
      `Prelude.hashWithSalt` queryParam
      `Prelude.hashWithSalt` actionTypeId

instance Prelude.NFData PollForJobs where
  rnf PollForJobs' {..} =
    Prelude.rnf maxBatchSize
      `Prelude.seq` Prelude.rnf queryParam
      `Prelude.seq` Prelude.rnf actionTypeId

instance Data.ToHeaders PollForJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.PollForJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PollForJobs where
  toJSON PollForJobs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxBatchSize" Data..=) Prelude.<$> maxBatchSize,
            ("queryParam" Data..=) Prelude.<$> queryParam,
            Prelude.Just ("actionTypeId" Data..= actionTypeId)
          ]
      )

instance Data.ToPath PollForJobs where
  toPath = Prelude.const "/"

instance Data.ToQuery PollForJobs where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PollForJobs@ action.
--
-- /See:/ 'newPollForJobsResponse' smart constructor.
data PollForJobsResponse = PollForJobsResponse'
  { -- | Information about the jobs to take action on.
    jobs :: Prelude.Maybe [Job],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PollForJobsResponse
newPollForJobsResponse pHttpStatus_ =
  PollForJobsResponse'
    { jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the jobs to take action on.
pollForJobsResponse_jobs :: Lens.Lens' PollForJobsResponse (Prelude.Maybe [Job])
pollForJobsResponse_jobs = Lens.lens (\PollForJobsResponse' {jobs} -> jobs) (\s@PollForJobsResponse' {} a -> s {jobs = a} :: PollForJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
pollForJobsResponse_httpStatus :: Lens.Lens' PollForJobsResponse Prelude.Int
pollForJobsResponse_httpStatus = Lens.lens (\PollForJobsResponse' {httpStatus} -> httpStatus) (\s@PollForJobsResponse' {} a -> s {httpStatus = a} :: PollForJobsResponse)

instance Prelude.NFData PollForJobsResponse where
  rnf PollForJobsResponse' {..} =
    Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
