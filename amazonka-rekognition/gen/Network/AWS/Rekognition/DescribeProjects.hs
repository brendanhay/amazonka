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
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProjects' smart constructor.
data DescribeProjects = DescribeProjects'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjects_nextToken :: Lens.Lens' DescribeProjects (Prelude.Maybe Prelude.Text)
describeProjects_nextToken = Lens.lens (\DescribeProjects' {nextToken} -> nextToken) (\s@DescribeProjects' {} a -> s {nextToken = a} :: DescribeProjects)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
describeProjects_maxResults :: Lens.Lens' DescribeProjects (Prelude.Maybe Prelude.Natural)
describeProjects_maxResults = Lens.lens (\DescribeProjects' {maxResults} -> maxResults) (\s@DescribeProjects' {} a -> s {maxResults = a} :: DescribeProjects)

instance Core.AWSPager DescribeProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeProjectsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeProjectsResponse_projectDescriptions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeProjects_nextToken
          Lens..~ rs
          Lens.^? describeProjectsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeProjects where
  type
    AWSResponse DescribeProjects =
      DescribeProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ProjectDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProjects

instance Prelude.NFData DescribeProjects

instance Core.ToHeaders DescribeProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.DescribeProjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProjects where
  toJSON DescribeProjects' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeProjects where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectsResponse' smart constructor.
data DescribeProjectsResponse = DescribeProjectsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of project descriptions. The list is sorted by the date and time
    -- the projects are created.
    projectDescriptions :: Prelude.Maybe [ProjectDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeProjectsResponse
newDescribeProjectsResponse pHttpStatus_ =
  DescribeProjectsResponse'
    { nextToken =
        Prelude.Nothing,
      projectDescriptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjectsResponse_nextToken :: Lens.Lens' DescribeProjectsResponse (Prelude.Maybe Prelude.Text)
describeProjectsResponse_nextToken = Lens.lens (\DescribeProjectsResponse' {nextToken} -> nextToken) (\s@DescribeProjectsResponse' {} a -> s {nextToken = a} :: DescribeProjectsResponse)

-- | A list of project descriptions. The list is sorted by the date and time
-- the projects are created.
describeProjectsResponse_projectDescriptions :: Lens.Lens' DescribeProjectsResponse (Prelude.Maybe [ProjectDescription])
describeProjectsResponse_projectDescriptions = Lens.lens (\DescribeProjectsResponse' {projectDescriptions} -> projectDescriptions) (\s@DescribeProjectsResponse' {} a -> s {projectDescriptions = a} :: DescribeProjectsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeProjectsResponse_httpStatus :: Lens.Lens' DescribeProjectsResponse Prelude.Int
describeProjectsResponse_httpStatus = Lens.lens (\DescribeProjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectsResponse' {} a -> s {httpStatus = a} :: DescribeProjectsResponse)

instance Prelude.NFData DescribeProjectsResponse
