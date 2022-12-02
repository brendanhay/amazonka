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
-- Module      : Amazonka.IoT.ListJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists jobs.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListJobs>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_nextToken,
    listJobs_targetSelection,
    listJobs_thingGroupName,
    listJobs_status,
    listJobs_maxResults,
    listJobs_namespaceId,
    listJobs_thingGroupId,

    -- * Destructuring the Response
    ListJobsResponse (..),
    newListJobsResponse,

    -- * Response Lenses
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    --
    -- We recommend that you use continuous jobs instead of snapshot jobs for
    -- dynamic thing group targets. By using continuous jobs, devices that join
    -- the group receive the job execution even after the job has been created.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | A filter that limits the returned jobs to those for the specified group.
    thingGroupName :: Prelude.Maybe Prelude.Text,
    -- | An optional filter that lets you search for jobs that have the specified
    -- status.
    status :: Prelude.Maybe JobStatus,
    -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, Amazon Web Services IoT
    -- Core sends jobs notifications to MQTT topics that contain the value in
    -- the following format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | A filter that limits the returned jobs to those for the specified group.
    thingGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobs_nextToken' - The token to retrieve the next set of results.
--
-- 'targetSelection', 'listJobs_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
--
-- 'thingGroupName', 'listJobs_thingGroupName' - A filter that limits the returned jobs to those for the specified group.
--
-- 'status', 'listJobs_status' - An optional filter that lets you search for jobs that have the specified
-- status.
--
-- 'maxResults', 'listJobs_maxResults' - The maximum number of results to return per request.
--
-- 'namespaceId', 'listJobs_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'thingGroupId', 'listJobs_thingGroupId' - A filter that limits the returned jobs to those for the specified group.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { nextToken = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      thingGroupId = Prelude.Nothing
    }

-- | The token to retrieve the next set of results.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
listJobs_targetSelection :: Lens.Lens' ListJobs (Prelude.Maybe TargetSelection)
listJobs_targetSelection = Lens.lens (\ListJobs' {targetSelection} -> targetSelection) (\s@ListJobs' {} a -> s {targetSelection = a} :: ListJobs)

-- | A filter that limits the returned jobs to those for the specified group.
listJobs_thingGroupName :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_thingGroupName = Lens.lens (\ListJobs' {thingGroupName} -> thingGroupName) (\s@ListJobs' {} a -> s {thingGroupName = a} :: ListJobs)

-- | An optional filter that lets you search for jobs that have the specified
-- status.
listJobs_status :: Lens.Lens' ListJobs (Prelude.Maybe JobStatus)
listJobs_status = Lens.lens (\ListJobs' {status} -> status) (\s@ListJobs' {} a -> s {status = a} :: ListJobs)

-- | The maximum number of results to return per request.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
listJobs_namespaceId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_namespaceId = Lens.lens (\ListJobs' {namespaceId} -> namespaceId) (\s@ListJobs' {} a -> s {namespaceId = a} :: ListJobs)

-- | A filter that limits the returned jobs to those for the specified group.
listJobs_thingGroupId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_thingGroupId = Lens.lens (\ListJobs' {thingGroupId} -> thingGroupId) (\s@ListJobs' {} a -> s {thingGroupId = a} :: ListJobs)

instance Core.AWSPager ListJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobsResponse_jobs Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobs_nextToken
          Lens..~ rs
          Lens.^? listJobsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListJobs where
  type AWSResponse ListJobs = ListJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs where
  hashWithSalt _salt ListJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` thingGroupName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` thingGroupId

instance Prelude.NFData ListJobs where
  rnf ListJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf thingGroupName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf thingGroupId

instance Data.ToHeaders ListJobs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobs where
  toPath = Prelude.const "/jobs"

instance Data.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "targetSelection" Data.=: targetSelection,
        "thingGroupName" Data.=: thingGroupName,
        "status" Data.=: status,
        "maxResults" Data.=: maxResults,
        "namespaceId" Data.=: namespaceId,
        "thingGroupId" Data.=: thingGroupId
      ]

-- | /See:/ 'newListJobsResponse' smart constructor.
data ListJobsResponse = ListJobsResponse'
  { -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of jobs.
    jobs :: Prelude.Maybe [JobSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobsResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'jobs', 'listJobsResponse_jobs' - A list of jobs.
--
-- 'httpStatus', 'listJobsResponse_httpStatus' - The response's http status code.
newListJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobsResponse
newListJobsResponse pHttpStatus_ =
  ListJobsResponse'
    { nextToken = Prelude.Nothing,
      jobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobsResponse_nextToken :: Lens.Lens' ListJobsResponse (Prelude.Maybe Prelude.Text)
listJobsResponse_nextToken = Lens.lens (\ListJobsResponse' {nextToken} -> nextToken) (\s@ListJobsResponse' {} a -> s {nextToken = a} :: ListJobsResponse)

-- | A list of jobs.
listJobsResponse_jobs :: Lens.Lens' ListJobsResponse (Prelude.Maybe [JobSummary])
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse where
  rnf ListJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobs
      `Prelude.seq` Prelude.rnf httpStatus
