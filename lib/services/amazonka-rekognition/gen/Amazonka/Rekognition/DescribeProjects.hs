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
-- Module      : Amazonka.Rekognition.DescribeProjects
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about your Amazon Rekognition Custom Labels projects.
--
-- This operation requires permissions to perform the
-- @rekognition:DescribeProjects@ action.
--
-- This operation returns paginated results.
module Amazonka.Rekognition.DescribeProjects
  ( -- * Creating a Request
    DescribeProjects (..),
    newDescribeProjects,

    -- * Request Lenses
    describeProjects_maxResults,
    describeProjects_nextToken,
    describeProjects_projectNames,

    -- * Destructuring the Response
    DescribeProjectsResponse (..),
    newDescribeProjectsResponse,

    -- * Response Lenses
    describeProjectsResponse_nextToken,
    describeProjectsResponse_projectDescriptions,
    describeProjectsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProjects' smart constructor.
data DescribeProjects = DescribeProjects'
  { -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Rekognition Custom Labels returns a pagination
    -- token in the response. You can use this pagination token to retrieve the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the projects that you want Amazon Rekognition Custom Labels to
    -- describe. If you don\'t specify a value, the response includes
    -- descriptions for all the projects in your AWS account.
    projectNames :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
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
-- 'maxResults', 'describeProjects_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'nextToken', 'describeProjects_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
--
-- 'projectNames', 'describeProjects_projectNames' - A list of the projects that you want Amazon Rekognition Custom Labels to
-- describe. If you don\'t specify a value, the response includes
-- descriptions for all the projects in your AWS account.
newDescribeProjects ::
  DescribeProjects
newDescribeProjects =
  DescribeProjects'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      projectNames = Prelude.Nothing
    }

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
describeProjects_maxResults :: Lens.Lens' DescribeProjects (Prelude.Maybe Prelude.Natural)
describeProjects_maxResults = Lens.lens (\DescribeProjects' {maxResults} -> maxResults) (\s@DescribeProjects' {} a -> s {maxResults = a} :: DescribeProjects)

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Rekognition Custom Labels returns a pagination
-- token in the response. You can use this pagination token to retrieve the
-- next set of results.
describeProjects_nextToken :: Lens.Lens' DescribeProjects (Prelude.Maybe Prelude.Text)
describeProjects_nextToken = Lens.lens (\DescribeProjects' {nextToken} -> nextToken) (\s@DescribeProjects' {} a -> s {nextToken = a} :: DescribeProjects)

-- | A list of the projects that you want Amazon Rekognition Custom Labels to
-- describe. If you don\'t specify a value, the response includes
-- descriptions for all the projects in your AWS account.
describeProjects_projectNames :: Lens.Lens' DescribeProjects (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeProjects_projectNames = Lens.lens (\DescribeProjects' {projectNames} -> projectNames) (\s@DescribeProjects' {} a -> s {projectNames = a} :: DescribeProjects) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ProjectDescriptions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeProjects where
  hashWithSalt _salt DescribeProjects' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` projectNames

instance Prelude.NFData DescribeProjects where
  rnf DescribeProjects' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectNames

instance Data.ToHeaders DescribeProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DescribeProjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeProjects where
  toJSON DescribeProjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ProjectNames" Data..=) Prelude.<$> projectNames
          ]
      )

instance Data.ToPath DescribeProjects where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeProjects where
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
describeProjectsResponse_projectDescriptions = Lens.lens (\DescribeProjectsResponse' {projectDescriptions} -> projectDescriptions) (\s@DescribeProjectsResponse' {} a -> s {projectDescriptions = a} :: DescribeProjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProjectsResponse_httpStatus :: Lens.Lens' DescribeProjectsResponse Prelude.Int
describeProjectsResponse_httpStatus = Lens.lens (\DescribeProjectsResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectsResponse' {} a -> s {httpStatus = a} :: DescribeProjectsResponse)

instance Prelude.NFData DescribeProjectsResponse where
  rnf DescribeProjectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projectDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
