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
-- Module      : Amazonka.AmplifyUiBuilder.ListCodegenJobs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of code generation jobs for a specified Amplify app and
-- backend environment.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ListCodegenJobs
  ( -- * Creating a Request
    ListCodegenJobs (..),
    newListCodegenJobs,

    -- * Request Lenses
    listCodegenJobs_maxResults,
    listCodegenJobs_nextToken,
    listCodegenJobs_appId,
    listCodegenJobs_environmentName,

    -- * Destructuring the Response
    ListCodegenJobsResponse (..),
    newListCodegenJobsResponse,

    -- * Response Lenses
    listCodegenJobsResponse_nextToken,
    listCodegenJobsResponse_httpStatus,
    listCodegenJobsResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCodegenJobs' smart constructor.
data ListCodegenJobs = ListCodegenJobs'
  { -- | The maximum number of jobs to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID for the Amplify app.
    appId :: Prelude.Text,
    -- | The name of the backend environment that is a part of the Amplify app.
    environmentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodegenJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCodegenJobs_maxResults' - The maximum number of jobs to retrieve.
--
-- 'nextToken', 'listCodegenJobs_nextToken' - The token to request the next page of results.
--
-- 'appId', 'listCodegenJobs_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'listCodegenJobs_environmentName' - The name of the backend environment that is a part of the Amplify app.
newListCodegenJobs ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ListCodegenJobs
newListCodegenJobs pAppId_ pEnvironmentName_ =
  ListCodegenJobs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The maximum number of jobs to retrieve.
listCodegenJobs_maxResults :: Lens.Lens' ListCodegenJobs (Prelude.Maybe Prelude.Natural)
listCodegenJobs_maxResults = Lens.lens (\ListCodegenJobs' {maxResults} -> maxResults) (\s@ListCodegenJobs' {} a -> s {maxResults = a} :: ListCodegenJobs)

-- | The token to request the next page of results.
listCodegenJobs_nextToken :: Lens.Lens' ListCodegenJobs (Prelude.Maybe Prelude.Text)
listCodegenJobs_nextToken = Lens.lens (\ListCodegenJobs' {nextToken} -> nextToken) (\s@ListCodegenJobs' {} a -> s {nextToken = a} :: ListCodegenJobs)

-- | The unique ID for the Amplify app.
listCodegenJobs_appId :: Lens.Lens' ListCodegenJobs Prelude.Text
listCodegenJobs_appId = Lens.lens (\ListCodegenJobs' {appId} -> appId) (\s@ListCodegenJobs' {} a -> s {appId = a} :: ListCodegenJobs)

-- | The name of the backend environment that is a part of the Amplify app.
listCodegenJobs_environmentName :: Lens.Lens' ListCodegenJobs Prelude.Text
listCodegenJobs_environmentName = Lens.lens (\ListCodegenJobs' {environmentName} -> environmentName) (\s@ListCodegenJobs' {} a -> s {environmentName = a} :: ListCodegenJobs)

instance Core.AWSPager ListCodegenJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCodegenJobsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listCodegenJobsResponse_entities) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listCodegenJobs_nextToken
          Lens..~ rs
          Lens.^? listCodegenJobsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListCodegenJobs where
  type
    AWSResponse ListCodegenJobs =
      ListCodegenJobsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodegenJobsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListCodegenJobs where
  hashWithSalt _salt ListCodegenJobs' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ListCodegenJobs where
  rnf ListCodegenJobs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ListCodegenJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListCodegenJobs where
  toPath ListCodegenJobs' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/codegen-jobs"
      ]

instance Data.ToQuery ListCodegenJobs where
  toQuery ListCodegenJobs' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListCodegenJobsResponse' smart constructor.
data ListCodegenJobsResponse = ListCodegenJobsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of code generation jobs for the Amplify app.
    entities :: [CodegenJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCodegenJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCodegenJobsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listCodegenJobsResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'listCodegenJobsResponse_entities' - The list of code generation jobs for the Amplify app.
newListCodegenJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCodegenJobsResponse
newListCodegenJobsResponse pHttpStatus_ =
  ListCodegenJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
listCodegenJobsResponse_nextToken :: Lens.Lens' ListCodegenJobsResponse (Prelude.Maybe Prelude.Text)
listCodegenJobsResponse_nextToken = Lens.lens (\ListCodegenJobsResponse' {nextToken} -> nextToken) (\s@ListCodegenJobsResponse' {} a -> s {nextToken = a} :: ListCodegenJobsResponse)

-- | The response's http status code.
listCodegenJobsResponse_httpStatus :: Lens.Lens' ListCodegenJobsResponse Prelude.Int
listCodegenJobsResponse_httpStatus = Lens.lens (\ListCodegenJobsResponse' {httpStatus} -> httpStatus) (\s@ListCodegenJobsResponse' {} a -> s {httpStatus = a} :: ListCodegenJobsResponse)

-- | The list of code generation jobs for the Amplify app.
listCodegenJobsResponse_entities :: Lens.Lens' ListCodegenJobsResponse [CodegenJobSummary]
listCodegenJobsResponse_entities = Lens.lens (\ListCodegenJobsResponse' {entities} -> entities) (\s@ListCodegenJobsResponse' {} a -> s {entities = a} :: ListCodegenJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListCodegenJobsResponse where
  rnf ListCodegenJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
