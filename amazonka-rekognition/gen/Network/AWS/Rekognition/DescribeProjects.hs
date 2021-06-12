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
-- Module      : Network.AWS.Rekognition.DescribeProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists and gets information about your Amazon Rekognition Custom Labels
-- projects.
--
-- This operation requires permissions to perform the
-- @rekognition:DescribeProjects@ action.
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.DescribeProjects
  ( -- * Creating a Request
    DescribeProjects (..),
    newDescribeProjects,

    -- * Request Lenses
    describeProjects_nextToken,
    describeProjects_maxResults,

    -- * Destructuring the Response
    DescribeProjectsResponse (..),
    newDescribeProjectsResponse,

    -- * Response Lenses
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProjects' smart constructor.
data DescribeProjects = DescribeProjects'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeProjects_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'maxResults', 'describeProjects_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
newDescribeProjects ::
  DescribeProjects
newDescribeProjects =
  DescribeProjects'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjects_nextToken :: Lens.Lens' DescribeProjects (Core.Maybe Core.Text)
describeProjects_nextToken = Lens.lens (\DescribeProjects' {nextToken} -> nextToken) (\s@DescribeProjects' {} a -> s {nextToken = a} :: DescribeProjects)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
describeProjects_maxResults :: Lens.Lens' DescribeProjects (Core.Maybe Core.Natural)
describeProjects_maxResults = Lens.lens (\DescribeProjects' {maxResults} -> maxResults) (\s@DescribeProjects' {} a -> s {maxResults = a} :: DescribeProjects)

instance Core.AWSPager DescribeProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeProjectsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeProjectsResponse_projectDescriptions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeProjects_nextToken
          Lens..~ rs
          Lens.^? describeProjectsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeProjects where
  type
    AWSResponse DescribeProjects =
      DescribeProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ProjectDescriptions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProjects

instance Core.NFData DescribeProjects

instance Core.ToHeaders DescribeProjects where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DescribeProjects" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProjects where
  toJSON DescribeProjects' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeProjects where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProjects where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProjectsResponse' smart constructor.
data DescribeProjectsResponse = DescribeProjectsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of project descriptions. The list is sorted by the date and time
    -- the projects are created.
    projectDescriptions :: Core.Maybe [ProjectDescription],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeProjectsResponse_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'projectDescriptions', 'describeProjectsResponse_projectDescriptions' - A list of project descriptions. The list is sorted by the date and time
-- the projects are created.
--
-- 'httpStatus', 'describeProjectsResponse_httpStatus' - The response's http status code.
newDescribeProjectsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProjectsResponse
newDescribeProjectsResponse pHttpStatus_ =
  DescribeProjectsResponse'
    { nextToken = Core.Nothing,
      projectDescriptions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectsResponse_nextToken :: Lens.Lens' DescribeProjectsResponse (Core.Maybe Core.Text)
describeProjectsResponse_nextToken = Lens.lens (\DescribeProjectsResponse' {nextToken} -> nextToken) (\s@DescribeProjectsResponse' {} a -> s {nextToken = a} :: DescribeProjectsResponse)

-- | A list of project descriptions. The list is sorted by the date and time
-- the projects are created.
describeProjectsResponse_projectDescriptions :: Lens.Lens' DescribeProjectsResponse (Core.Maybe [ProjectDescription])
describeProjectsResponse_projectDescriptions = Lens.lens (\DescribeProjectsResponse' {projectDescriptions} -> projectDescriptions) (\s@DescribeProjectsResponse' {} a -> s {projectDescriptions = a} :: DescribeProjectsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProjectsResponse_httpStatus :: Lens.Lens' DescribeProjectsResponse Core.Int
describeProjectsResponse_httpStatus = Lens.lens (\DescribeProjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectsResponse' {} a -> s {httpStatus = a} :: DescribeProjectsResponse)

instance Core.NFData DescribeProjectsResponse
