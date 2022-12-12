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
-- Module      : Amazonka.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListJobExecutionsForThing>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListJobExecutionsForThing
  ( -- * Creating a Request
    ListJobExecutionsForThing (..),
    newListJobExecutionsForThing,

    -- * Request Lenses
    listJobExecutionsForThing_jobId,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_thingName,

    -- * Destructuring the Response
    ListJobExecutionsForThingResponse (..),
    newListJobExecutionsForThingResponse,

    -- * Response Lenses
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
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
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An optional filter that lets you search for jobs that have the specified
    -- status.
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobId', 'listJobExecutionsForThing_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'maxResults', 'listJobExecutionsForThing_maxResults' - The maximum number of results to be returned per request.
--
-- 'namespaceId', 'listJobExecutionsForThing_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'nextToken', 'listJobExecutionsForThing_nextToken' - The token to retrieve the next set of results.
--
-- 'status', 'listJobExecutionsForThing_status' - An optional filter that lets you search for jobs that have the specified
-- status.
--
-- 'thingName', 'listJobExecutionsForThing_thingName' - The thing name.
newListJobExecutionsForThing ::
  -- | 'thingName'
  Prelude.Text ->
  ListJobExecutionsForThing
newListJobExecutionsForThing pThingName_ =
  ListJobExecutionsForThing'
    { jobId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      thingName = pThingName_
    }

-- | The unique identifier you assigned to this job when it was created.
listJobExecutionsForThing_jobId :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Text)
listJobExecutionsForThing_jobId = Lens.lens (\ListJobExecutionsForThing' {jobId} -> jobId) (\s@ListJobExecutionsForThing' {} a -> s {jobId = a} :: ListJobExecutionsForThing)

-- | The maximum number of results to be returned per request.
listJobExecutionsForThing_maxResults :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Natural)
listJobExecutionsForThing_maxResults = Lens.lens (\ListJobExecutionsForThing' {maxResults} -> maxResults) (\s@ListJobExecutionsForThing' {} a -> s {maxResults = a} :: ListJobExecutionsForThing)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
listJobExecutionsForThing_namespaceId :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Text)
listJobExecutionsForThing_namespaceId = Lens.lens (\ListJobExecutionsForThing' {namespaceId} -> namespaceId) (\s@ListJobExecutionsForThing' {} a -> s {namespaceId = a} :: ListJobExecutionsForThing)

-- | The token to retrieve the next set of results.
listJobExecutionsForThing_nextToken :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Text)
listJobExecutionsForThing_nextToken = Lens.lens (\ListJobExecutionsForThing' {nextToken} -> nextToken) (\s@ListJobExecutionsForThing' {} a -> s {nextToken = a} :: ListJobExecutionsForThing)

-- | An optional filter that lets you search for jobs that have the specified
-- status.
listJobExecutionsForThing_status :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe JobExecutionStatus)
listJobExecutionsForThing_status = Lens.lens (\ListJobExecutionsForThing' {status} -> status) (\s@ListJobExecutionsForThing' {} a -> s {status = a} :: ListJobExecutionsForThing)

-- | The thing name.
listJobExecutionsForThing_thingName :: Lens.Lens' ListJobExecutionsForThing Prelude.Text
listJobExecutionsForThing_thingName = Lens.lens (\ListJobExecutionsForThing' {thingName} -> thingName) (\s@ListJobExecutionsForThing' {} a -> s {thingName = a} :: ListJobExecutionsForThing)

instance Core.AWSPager ListJobExecutionsForThing where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_executionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobExecutionsForThing_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForThingResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForThing where
  type
    AWSResponse ListJobExecutionsForThing =
      ListJobExecutionsForThingResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForThingResponse'
            Prelude.<$> ( x Data..?> "executionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobExecutionsForThing where
  hashWithSalt _salt ListJobExecutionsForThing' {..} =
    _salt `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` thingName

instance Prelude.NFData ListJobExecutionsForThing where
  rnf ListJobExecutionsForThing' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf namespaceId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf thingName

instance Data.ToHeaders ListJobExecutionsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListJobExecutionsForThing where
  toPath ListJobExecutionsForThing' {..} =
    Prelude.mconcat
      ["/things/", Data.toBS thingName, "/jobs"]

instance Data.ToQuery ListJobExecutionsForThing where
  toQuery ListJobExecutionsForThing' {..} =
    Prelude.mconcat
      [ "jobId" Data.=: jobId,
        "maxResults" Data.=: maxResults,
        "namespaceId" Data.=: namespaceId,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status
      ]

-- | /See:/ 'newListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Prelude.Maybe [JobExecutionSummaryForThing],
    -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionSummaries', 'listJobExecutionsForThingResponse_executionSummaries' - A list of job execution summaries.
--
-- 'nextToken', 'listJobExecutionsForThingResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'httpStatus', 'listJobExecutionsForThingResponse_httpStatus' - The response's http status code.
newListJobExecutionsForThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobExecutionsForThingResponse
newListJobExecutionsForThingResponse pHttpStatus_ =
  ListJobExecutionsForThingResponse'
    { executionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job execution summaries.
listJobExecutionsForThingResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Prelude.Maybe [JobExecutionSummaryForThing])
listJobExecutionsForThingResponse_executionSummaries = Lens.lens (\ListJobExecutionsForThingResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForThingResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForThingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForThingResponse_nextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Prelude.Maybe Prelude.Text)
listJobExecutionsForThingResponse_nextToken = Lens.lens (\ListJobExecutionsForThingResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForThingResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForThingResponse)

-- | The response's http status code.
listJobExecutionsForThingResponse_httpStatus :: Lens.Lens' ListJobExecutionsForThingResponse Prelude.Int
listJobExecutionsForThingResponse_httpStatus = Lens.lens (\ListJobExecutionsForThingResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForThingResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForThingResponse)

instance
  Prelude.NFData
    ListJobExecutionsForThingResponse
  where
  rnf ListJobExecutionsForThingResponse' {..} =
    Prelude.rnf executionSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
