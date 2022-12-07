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
-- Module      : Amazonka.LookoutVision.ListModelPackagingJobs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the model packaging jobs created for an Amazon Lookout for Vision
-- project.
--
-- This operation requires permissions to perform the
-- @lookoutvision:ListModelPackagingJobs@ operation.
--
-- For more information, see /Using your Amazon Lookout for Vision model on
-- an edge device/ in the Amazon Lookout for Vision Developer Guide.
--
-- This operation returns paginated results.
module Amazonka.LookoutVision.ListModelPackagingJobs
  ( -- * Creating a Request
    ListModelPackagingJobs (..),
    newListModelPackagingJobs,

    -- * Request Lenses
    listModelPackagingJobs_nextToken,
    listModelPackagingJobs_maxResults,
    listModelPackagingJobs_projectName,

    -- * Destructuring the Response
    ListModelPackagingJobsResponse (..),
    newListModelPackagingJobsResponse,

    -- * Response Lenses
    listModelPackagingJobsResponse_nextToken,
    listModelPackagingJobsResponse_modelPackagingJobs,
    listModelPackagingJobsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListModelPackagingJobs' smart constructor.
data ListModelPackagingJobs = ListModelPackagingJobs'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Lookout for Vision returns a pagination token in
    -- the response. You can use this pagination token to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per paginated call. The largest
    -- value you can specify is 100. If you specify a value greater than 100, a
    -- ValidationException error occurs. The default value is 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the project for which you want to list the model packaging
    -- jobs.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackagingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelPackagingJobs_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Lookout for Vision returns a pagination token in
-- the response. You can use this pagination token to retrieve the next set
-- of results.
--
-- 'maxResults', 'listModelPackagingJobs_maxResults' - The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
--
-- 'projectName', 'listModelPackagingJobs_projectName' - The name of the project for which you want to list the model packaging
-- jobs.
newListModelPackagingJobs ::
  -- | 'projectName'
  Prelude.Text ->
  ListModelPackagingJobs
newListModelPackagingJobs pProjectName_ =
  ListModelPackagingJobs'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Lookout for Vision returns a pagination token in
-- the response. You can use this pagination token to retrieve the next set
-- of results.
listModelPackagingJobs_nextToken :: Lens.Lens' ListModelPackagingJobs (Prelude.Maybe Prelude.Text)
listModelPackagingJobs_nextToken = Lens.lens (\ListModelPackagingJobs' {nextToken} -> nextToken) (\s@ListModelPackagingJobs' {} a -> s {nextToken = a} :: ListModelPackagingJobs)

-- | The maximum number of results to return per paginated call. The largest
-- value you can specify is 100. If you specify a value greater than 100, a
-- ValidationException error occurs. The default value is 100.
listModelPackagingJobs_maxResults :: Lens.Lens' ListModelPackagingJobs (Prelude.Maybe Prelude.Natural)
listModelPackagingJobs_maxResults = Lens.lens (\ListModelPackagingJobs' {maxResults} -> maxResults) (\s@ListModelPackagingJobs' {} a -> s {maxResults = a} :: ListModelPackagingJobs)

-- | The name of the project for which you want to list the model packaging
-- jobs.
listModelPackagingJobs_projectName :: Lens.Lens' ListModelPackagingJobs Prelude.Text
listModelPackagingJobs_projectName = Lens.lens (\ListModelPackagingJobs' {projectName} -> projectName) (\s@ListModelPackagingJobs' {} a -> s {projectName = a} :: ListModelPackagingJobs)

instance Core.AWSPager ListModelPackagingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelPackagingJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listModelPackagingJobsResponse_modelPackagingJobs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModelPackagingJobs_nextToken
          Lens..~ rs
          Lens.^? listModelPackagingJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListModelPackagingJobs where
  type
    AWSResponse ListModelPackagingJobs =
      ListModelPackagingJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelPackagingJobsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ModelPackagingJobs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListModelPackagingJobs where
  hashWithSalt _salt ListModelPackagingJobs' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData ListModelPackagingJobs where
  rnf ListModelPackagingJobs' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders ListModelPackagingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListModelPackagingJobs where
  toPath ListModelPackagingJobs' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/modelpackagingjobs"
      ]

instance Data.ToQuery ListModelPackagingJobs where
  toQuery ListModelPackagingJobs' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListModelPackagingJobsResponse' smart constructor.
data ListModelPackagingJobsResponse = ListModelPackagingJobsResponse'
  { -- | If the previous response was incomplete (because there is more results
    -- to retrieve), Amazon Lookout for Vision returns a pagination token in
    -- the response. You can use this pagination token to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the model packaging jobs created for the specified Amazon
    -- Lookout for Vision project.
    modelPackagingJobs :: Prelude.Maybe [ModelPackagingJobMetadata],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackagingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelPackagingJobsResponse_nextToken' - If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Lookout for Vision returns a pagination token in
-- the response. You can use this pagination token to retrieve the next set
-- of results.
--
-- 'modelPackagingJobs', 'listModelPackagingJobsResponse_modelPackagingJobs' - A list of the model packaging jobs created for the specified Amazon
-- Lookout for Vision project.
--
-- 'httpStatus', 'listModelPackagingJobsResponse_httpStatus' - The response's http status code.
newListModelPackagingJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelPackagingJobsResponse
newListModelPackagingJobsResponse pHttpStatus_ =
  ListModelPackagingJobsResponse'
    { nextToken =
        Prelude.Nothing,
      modelPackagingJobs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous response was incomplete (because there is more results
-- to retrieve), Amazon Lookout for Vision returns a pagination token in
-- the response. You can use this pagination token to retrieve the next set
-- of results.
listModelPackagingJobsResponse_nextToken :: Lens.Lens' ListModelPackagingJobsResponse (Prelude.Maybe Prelude.Text)
listModelPackagingJobsResponse_nextToken = Lens.lens (\ListModelPackagingJobsResponse' {nextToken} -> nextToken) (\s@ListModelPackagingJobsResponse' {} a -> s {nextToken = a} :: ListModelPackagingJobsResponse)

-- | A list of the model packaging jobs created for the specified Amazon
-- Lookout for Vision project.
listModelPackagingJobsResponse_modelPackagingJobs :: Lens.Lens' ListModelPackagingJobsResponse (Prelude.Maybe [ModelPackagingJobMetadata])
listModelPackagingJobsResponse_modelPackagingJobs = Lens.lens (\ListModelPackagingJobsResponse' {modelPackagingJobs} -> modelPackagingJobs) (\s@ListModelPackagingJobsResponse' {} a -> s {modelPackagingJobs = a} :: ListModelPackagingJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listModelPackagingJobsResponse_httpStatus :: Lens.Lens' ListModelPackagingJobsResponse Prelude.Int
listModelPackagingJobsResponse_httpStatus = Lens.lens (\ListModelPackagingJobsResponse' {httpStatus} -> httpStatus) (\s@ListModelPackagingJobsResponse' {} a -> s {httpStatus = a} :: ListModelPackagingJobsResponse)

instance
  Prelude.NFData
    ListModelPackagingJobsResponse
  where
  rnf ListModelPackagingJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf modelPackagingJobs
      `Prelude.seq` Prelude.rnf httpStatus
