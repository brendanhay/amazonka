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
-- Module      : Amazonka.Evidently.ListFeatures
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns configuration details about all the features in the specified
-- project.
--
-- This operation returns paginated results.
module Amazonka.Evidently.ListFeatures
  ( -- * Creating a Request
    ListFeatures (..),
    newListFeatures,

    -- * Request Lenses
    listFeatures_nextToken,
    listFeatures_maxResults,
    listFeatures_project,

    -- * Destructuring the Response
    ListFeaturesResponse (..),
    newListFeaturesResponse,

    -- * Response Lenses
    listFeaturesResponse_nextToken,
    listFeaturesResponse_features,
    listFeaturesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFeatures' smart constructor.
data ListFeatures = ListFeatures'
  { -- | The token to use when requesting the next set of results. You received
    -- this token from a previous @ListFeatures@ operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name or ARN of the project to return the feature list from.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeatures' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFeatures_nextToken' - The token to use when requesting the next set of results. You received
-- this token from a previous @ListFeatures@ operation.
--
-- 'maxResults', 'listFeatures_maxResults' - The maximum number of results to include in the response.
--
-- 'project', 'listFeatures_project' - The name or ARN of the project to return the feature list from.
newListFeatures ::
  -- | 'project'
  Prelude.Text ->
  ListFeatures
newListFeatures pProject_ =
  ListFeatures'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      project = pProject_
    }

-- | The token to use when requesting the next set of results. You received
-- this token from a previous @ListFeatures@ operation.
listFeatures_nextToken :: Lens.Lens' ListFeatures (Prelude.Maybe Prelude.Text)
listFeatures_nextToken = Lens.lens (\ListFeatures' {nextToken} -> nextToken) (\s@ListFeatures' {} a -> s {nextToken = a} :: ListFeatures)

-- | The maximum number of results to include in the response.
listFeatures_maxResults :: Lens.Lens' ListFeatures (Prelude.Maybe Prelude.Natural)
listFeatures_maxResults = Lens.lens (\ListFeatures' {maxResults} -> maxResults) (\s@ListFeatures' {} a -> s {maxResults = a} :: ListFeatures)

-- | The name or ARN of the project to return the feature list from.
listFeatures_project :: Lens.Lens' ListFeatures Prelude.Text
listFeatures_project = Lens.lens (\ListFeatures' {project} -> project) (\s@ListFeatures' {} a -> s {project = a} :: ListFeatures)

instance Core.AWSPager ListFeatures where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFeaturesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFeaturesResponse_features Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFeatures_nextToken
          Lens..~ rs
          Lens.^? listFeaturesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListFeatures where
  type AWSResponse ListFeatures = ListFeaturesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFeaturesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "features" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFeatures where
  hashWithSalt _salt ListFeatures' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` project

instance Prelude.NFData ListFeatures where
  rnf ListFeatures' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf project

instance Data.ToHeaders ListFeatures where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFeatures where
  toPath ListFeatures' {..} =
    Prelude.mconcat
      ["/projects/", Data.toBS project, "/features"]

instance Data.ToQuery ListFeatures where
  toQuery ListFeatures' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListFeaturesResponse' smart constructor.
data ListFeaturesResponse = ListFeaturesResponse'
  { -- | The token to use in a subsequent @ListFeatures@ operation to return the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of structures that contain the configuration details of the
    -- features in the specified project.
    features :: Prelude.Maybe [FeatureSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeaturesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFeaturesResponse_nextToken' - The token to use in a subsequent @ListFeatures@ operation to return the
-- next set of results.
--
-- 'features', 'listFeaturesResponse_features' - An array of structures that contain the configuration details of the
-- features in the specified project.
--
-- 'httpStatus', 'listFeaturesResponse_httpStatus' - The response's http status code.
newListFeaturesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFeaturesResponse
newListFeaturesResponse pHttpStatus_ =
  ListFeaturesResponse'
    { nextToken = Prelude.Nothing,
      features = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use in a subsequent @ListFeatures@ operation to return the
-- next set of results.
listFeaturesResponse_nextToken :: Lens.Lens' ListFeaturesResponse (Prelude.Maybe Prelude.Text)
listFeaturesResponse_nextToken = Lens.lens (\ListFeaturesResponse' {nextToken} -> nextToken) (\s@ListFeaturesResponse' {} a -> s {nextToken = a} :: ListFeaturesResponse)

-- | An array of structures that contain the configuration details of the
-- features in the specified project.
listFeaturesResponse_features :: Lens.Lens' ListFeaturesResponse (Prelude.Maybe [FeatureSummary])
listFeaturesResponse_features = Lens.lens (\ListFeaturesResponse' {features} -> features) (\s@ListFeaturesResponse' {} a -> s {features = a} :: ListFeaturesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listFeaturesResponse_httpStatus :: Lens.Lens' ListFeaturesResponse Prelude.Int
listFeaturesResponse_httpStatus = Lens.lens (\ListFeaturesResponse' {httpStatus} -> httpStatus) (\s@ListFeaturesResponse' {} a -> s {httpStatus = a} :: ListFeaturesResponse)

instance Prelude.NFData ListFeaturesResponse where
  rnf ListFeaturesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf httpStatus
