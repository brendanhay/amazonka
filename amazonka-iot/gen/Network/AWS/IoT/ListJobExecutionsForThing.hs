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
-- Module      : Network.AWS.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListJobExecutionsForThing
  ( -- * Creating a Request
    ListJobExecutionsForThing (..),
    newListJobExecutionsForThing,

    -- * Request Lenses
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_thingName,

    -- * Destructuring the Response
    ListJobExecutionsForThingResponse (..),
    newListJobExecutionsForThingResponse,

    -- * Response Lenses
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { -- | The token to retrieve the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | An optional filter that lets you search for jobs that have the specified
    -- status.
    status :: Core.Maybe JobExecutionStatus,
    -- | The maximum number of results to be returned per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, AWS IoT Core sends jobs
    -- notifications to MQTT topics that contain the value in the following
    -- format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Core.Maybe Core.Text,
    -- | The thing name.
    thingName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobExecutionsForThing_nextToken' - The token to retrieve the next set of results.
--
-- 'status', 'listJobExecutionsForThing_status' - An optional filter that lets you search for jobs that have the specified
-- status.
--
-- 'maxResults', 'listJobExecutionsForThing_maxResults' - The maximum number of results to be returned per request.
--
-- 'namespaceId', 'listJobExecutionsForThing_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'thingName', 'listJobExecutionsForThing_thingName' - The thing name.
newListJobExecutionsForThing ::
  -- | 'thingName'
  Core.Text ->
  ListJobExecutionsForThing
newListJobExecutionsForThing pThingName_ =
  ListJobExecutionsForThing'
    { nextToken =
        Core.Nothing,
      status = Core.Nothing,
      maxResults = Core.Nothing,
      namespaceId = Core.Nothing,
      thingName = pThingName_
    }

-- | The token to retrieve the next set of results.
listJobExecutionsForThing_nextToken :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Core.Text)
listJobExecutionsForThing_nextToken = Lens.lens (\ListJobExecutionsForThing' {nextToken} -> nextToken) (\s@ListJobExecutionsForThing' {} a -> s {nextToken = a} :: ListJobExecutionsForThing)

-- | An optional filter that lets you search for jobs that have the specified
-- status.
listJobExecutionsForThing_status :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe JobExecutionStatus)
listJobExecutionsForThing_status = Lens.lens (\ListJobExecutionsForThing' {status} -> status) (\s@ListJobExecutionsForThing' {} a -> s {status = a} :: ListJobExecutionsForThing)

-- | The maximum number of results to be returned per request.
listJobExecutionsForThing_maxResults :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Core.Natural)
listJobExecutionsForThing_maxResults = Lens.lens (\ListJobExecutionsForThing' {maxResults} -> maxResults) (\s@ListJobExecutionsForThing' {} a -> s {maxResults = a} :: ListJobExecutionsForThing)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, AWS IoT Core sends jobs
-- notifications to MQTT topics that contain the value in the following
-- format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
listJobExecutionsForThing_namespaceId :: Lens.Lens' ListJobExecutionsForThing (Core.Maybe Core.Text)
listJobExecutionsForThing_namespaceId = Lens.lens (\ListJobExecutionsForThing' {namespaceId} -> namespaceId) (\s@ListJobExecutionsForThing' {} a -> s {namespaceId = a} :: ListJobExecutionsForThing)

-- | The thing name.
listJobExecutionsForThing_thingName :: Lens.Lens' ListJobExecutionsForThing Core.Text
listJobExecutionsForThing_thingName = Lens.lens (\ListJobExecutionsForThing' {thingName} -> thingName) (\s@ListJobExecutionsForThing' {} a -> s {thingName = a} :: ListJobExecutionsForThing)

instance Core.AWSPager ListJobExecutionsForThing where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_executionSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listJobExecutionsForThing_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForThingResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForThing where
  type
    AWSResponse ListJobExecutionsForThing =
      ListJobExecutionsForThingResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForThingResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "executionSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListJobExecutionsForThing

instance Core.NFData ListJobExecutionsForThing

instance Core.ToHeaders ListJobExecutionsForThing where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListJobExecutionsForThing where
  toPath ListJobExecutionsForThing' {..} =
    Core.mconcat
      ["/things/", Core.toBS thingName, "/jobs"]

instance Core.ToQuery ListJobExecutionsForThing where
  toQuery ListJobExecutionsForThing' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "status" Core.=: status,
        "maxResults" Core.=: maxResults,
        "namespaceId" Core.=: namespaceId
      ]

-- | /See:/ 'newListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of job execution summaries.
    executionSummaries :: Core.Maybe [JobExecutionSummaryForThing],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobExecutionsForThingResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'executionSummaries', 'listJobExecutionsForThingResponse_executionSummaries' - A list of job execution summaries.
--
-- 'httpStatus', 'listJobExecutionsForThingResponse_httpStatus' - The response's http status code.
newListJobExecutionsForThingResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListJobExecutionsForThingResponse
newListJobExecutionsForThingResponse pHttpStatus_ =
  ListJobExecutionsForThingResponse'
    { nextToken =
        Core.Nothing,
      executionSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForThingResponse_nextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe Core.Text)
listJobExecutionsForThingResponse_nextToken = Lens.lens (\ListJobExecutionsForThingResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForThingResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForThingResponse)

-- | A list of job execution summaries.
listJobExecutionsForThingResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Core.Maybe [JobExecutionSummaryForThing])
listJobExecutionsForThingResponse_executionSummaries = Lens.lens (\ListJobExecutionsForThingResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForThingResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForThingResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listJobExecutionsForThingResponse_httpStatus :: Lens.Lens' ListJobExecutionsForThingResponse Core.Int
listJobExecutionsForThingResponse_httpStatus = Lens.lens (\ListJobExecutionsForThingResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForThingResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForThingResponse)

instance
  Core.NFData
    ListJobExecutionsForThingResponse
