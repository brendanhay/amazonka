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
-- Module      : Amazonka.Amplify.ListArtifacts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of artifacts for a specified app, branch, and job.
module Amazonka.Amplify.ListArtifacts
  ( -- * Creating a Request
    ListArtifacts (..),
    newListArtifacts,

    -- * Request Lenses
    listArtifacts_nextToken,
    listArtifacts_maxResults,
    listArtifacts_appId,
    listArtifacts_branchName,
    listArtifacts_jobId,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,
    listArtifactsResponse_artifacts,
  )
where

import Amazonka.Amplify.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Describes the request structure for the list artifacts request.
--
-- /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | A pagination token. Set to null to start listing artifacts from start.
    -- If a non-null pagination token is returned in a result, pass its value
    -- in here to list more artifacts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to list in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique ID for an Amplify app.
    appId :: Prelude.Text,
    -- | The name of a branch that is part of an Amplify app.
    branchName :: Prelude.Text,
    -- | The unique ID for a job.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifacts_nextToken' - A pagination token. Set to null to start listing artifacts from start.
-- If a non-null pagination token is returned in a result, pass its value
-- in here to list more artifacts.
--
-- 'maxResults', 'listArtifacts_maxResults' - The maximum number of records to list in a single response.
--
-- 'appId', 'listArtifacts_appId' - The unique ID for an Amplify app.
--
-- 'branchName', 'listArtifacts_branchName' - The name of a branch that is part of an Amplify app.
--
-- 'jobId', 'listArtifacts_jobId' - The unique ID for a job.
newListArtifacts ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'branchName'
  Prelude.Text ->
  -- | 'jobId'
  Prelude.Text ->
  ListArtifacts
newListArtifacts pAppId_ pBranchName_ pJobId_ =
  ListArtifacts'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      appId = pAppId_,
      branchName = pBranchName_,
      jobId = pJobId_
    }

-- | A pagination token. Set to null to start listing artifacts from start.
-- If a non-null pagination token is returned in a result, pass its value
-- in here to list more artifacts.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | The maximum number of records to list in a single response.
listArtifacts_maxResults :: Lens.Lens' ListArtifacts (Prelude.Maybe Prelude.Natural)
listArtifacts_maxResults = Lens.lens (\ListArtifacts' {maxResults} -> maxResults) (\s@ListArtifacts' {} a -> s {maxResults = a} :: ListArtifacts)

-- | The unique ID for an Amplify app.
listArtifacts_appId :: Lens.Lens' ListArtifacts Prelude.Text
listArtifacts_appId = Lens.lens (\ListArtifacts' {appId} -> appId) (\s@ListArtifacts' {} a -> s {appId = a} :: ListArtifacts)

-- | The name of a branch that is part of an Amplify app.
listArtifacts_branchName :: Lens.Lens' ListArtifacts Prelude.Text
listArtifacts_branchName = Lens.lens (\ListArtifacts' {branchName} -> branchName) (\s@ListArtifacts' {} a -> s {branchName = a} :: ListArtifacts)

-- | The unique ID for a job.
listArtifacts_jobId :: Lens.Lens' ListArtifacts Prelude.Text
listArtifacts_jobId = Lens.lens (\ListArtifacts' {jobId} -> jobId) (\s@ListArtifacts' {} a -> s {jobId = a} :: ListArtifacts)

instance Core.AWSRequest ListArtifacts where
  type
    AWSResponse ListArtifacts =
      ListArtifactsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "artifacts" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListArtifacts where
  hashWithSalt _salt ListArtifacts' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` branchName
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData ListArtifacts where
  rnf ListArtifacts' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf branchName
      `Prelude.seq` Prelude.rnf jobId

instance Data.ToHeaders ListArtifacts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListArtifacts where
  toPath ListArtifacts' {..} =
    Prelude.mconcat
      [ "/apps/",
        Data.toBS appId,
        "/branches/",
        Data.toBS branchName,
        "/jobs/",
        Data.toBS jobId,
        "/artifacts"
      ]

instance Data.ToQuery ListArtifacts where
  toQuery ListArtifacts' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | The result structure for the list artifacts request.
--
-- /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | A pagination token. If a non-null pagination token is returned in a
    -- result, pass its value in another request to retrieve more entries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of artifacts.
    artifacts :: [Artifact]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - A pagination token. If a non-null pagination token is returned in a
-- result, pass its value in another request to retrieve more entries.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
--
-- 'artifacts', 'listArtifactsResponse_artifacts' - A list of artifacts.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      artifacts = Prelude.mempty
    }

-- | A pagination token. If a non-null pagination token is returned in a
-- result, pass its value in another request to retrieve more entries.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Prelude.Maybe Prelude.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Prelude.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

-- | A list of artifacts.
listArtifactsResponse_artifacts :: Lens.Lens' ListArtifactsResponse [Artifact]
listArtifactsResponse_artifacts = Lens.lens (\ListArtifactsResponse' {artifacts} -> artifacts) (\s@ListArtifactsResponse' {} a -> s {artifacts = a} :: ListArtifactsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListArtifactsResponse where
  rnf ListArtifactsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf artifacts
