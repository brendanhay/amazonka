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
-- Module      : Network.AWS.IoT.ListJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists jobs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobs
  ( -- * Creating a Request
    ListJobs (..),
    newListJobs,

    -- * Request Lenses
    listJobs_nextToken,
    listJobs_status,
    listJobs_targetSelection,
    listJobs_maxResults,
    listJobs_namespaceId,
    listJobs_thingGroupName,
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

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJobs' smart constructor.
data ListJobs = ListJobs'
  { -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional filter that lets you search for jobs that have the specified
    -- status.
    status :: Prelude.Maybe JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | The maximum number of results to return per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | A filter that limits the returned jobs to those for the specified group.
    thingGroupName :: Prelude.Maybe Prelude.Text,
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
-- 'status', 'listJobs_status' - An optional filter that lets you search for jobs that have the specified
-- status.
--
-- 'targetSelection', 'listJobs_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- 'maxResults', 'listJobs_maxResults' - The maximum number of results to return per request.
--
-- 'namespaceId', 'listJobs_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'thingGroupName', 'listJobs_thingGroupName' - A filter that limits the returned jobs to those for the specified group.
--
-- 'thingGroupId', 'listJobs_thingGroupId' - A filter that limits the returned jobs to those for the specified group.
newListJobs ::
  ListJobs
newListJobs =
  ListJobs'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      thingGroupName = Prelude.Nothing,
      thingGroupId = Prelude.Nothing
    }

-- | The token to retrieve the next set of results.
listJobs_nextToken :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_nextToken = Lens.lens (\ListJobs' {nextToken} -> nextToken) (\s@ListJobs' {} a -> s {nextToken = a} :: ListJobs)

-- | An optional filter that lets you search for jobs that have the specified
-- status.
listJobs_status :: Lens.Lens' ListJobs (Prelude.Maybe JobStatus)
listJobs_status = Lens.lens (\ListJobs' {status} -> status) (\s@ListJobs' {} a -> s {status = a} :: ListJobs)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
listJobs_targetSelection :: Lens.Lens' ListJobs (Prelude.Maybe TargetSelection)
listJobs_targetSelection = Lens.lens (\ListJobs' {targetSelection} -> targetSelection) (\s@ListJobs' {} a -> s {targetSelection = a} :: ListJobs)

-- | The maximum number of results to return per request.
listJobs_maxResults :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Natural)
listJobs_maxResults = Lens.lens (\ListJobs' {maxResults} -> maxResults) (\s@ListJobs' {} a -> s {maxResults = a} :: ListJobs)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
listJobs_namespaceId :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_namespaceId = Lens.lens (\ListJobs' {namespaceId} -> namespaceId) (\s@ListJobs' {} a -> s {namespaceId = a} :: ListJobs)

-- | A filter that limits the returned jobs to those for the specified group.
listJobs_thingGroupName :: Lens.Lens' ListJobs (Prelude.Maybe Prelude.Text)
listJobs_thingGroupName = Lens.lens (\ListJobs' {thingGroupName} -> thingGroupName) (\s@ListJobs' {} a -> s {thingGroupName = a} :: ListJobs)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "jobs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobs

instance Prelude.NFData ListJobs

instance Core.ToHeaders ListJobs where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListJobs where
  toPath = Prelude.const "/jobs"

instance Core.ToQuery ListJobs where
  toQuery ListJobs' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "targetSelection" Core.=: targetSelection,
        "maxResults" Core.=: maxResults,
        "namespaceId" Core.=: namespaceId,
        "thingGroupName" Core.=: thingGroupName,
        "thingGroupId" Core.=: thingGroupId
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
listJobsResponse_jobs = Lens.lens (\ListJobsResponse' {jobs} -> jobs) (\s@ListJobsResponse' {} a -> s {jobs = a} :: ListJobsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobsResponse_httpStatus :: Lens.Lens' ListJobsResponse Prelude.Int
listJobsResponse_httpStatus = Lens.lens (\ListJobsResponse' {httpStatus} -> httpStatus) (\s@ListJobsResponse' {} a -> s {httpStatus = a} :: ListJobsResponse)

instance Prelude.NFData ListJobsResponse
