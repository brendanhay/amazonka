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
-- Module      : Amazonka.AmplifyUiBuilder.ListThemes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of themes for a specified Amplify app and backend
-- environment.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ListThemes
  ( -- * Creating a Request
    ListThemes (..),
    newListThemes,

    -- * Request Lenses
    listThemes_maxResults,
    listThemes_nextToken,
    listThemes_appId,
    listThemes_environmentName,

    -- * Destructuring the Response
    ListThemesResponse (..),
    newListThemesResponse,

    -- * Response Lenses
    listThemesResponse_nextToken,
    listThemesResponse_httpStatus,
    listThemesResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListThemes' smart constructor.
data ListThemes = ListThemes'
  { -- | The maximum number of theme results to return in the response.
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
-- Create a value of 'ListThemes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listThemes_maxResults' - The maximum number of theme results to return in the response.
--
-- 'nextToken', 'listThemes_nextToken' - The token to request the next page of results.
--
-- 'appId', 'listThemes_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'listThemes_environmentName' - The name of the backend environment that is a part of the Amplify app.
newListThemes ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ListThemes
newListThemes pAppId_ pEnvironmentName_ =
  ListThemes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The maximum number of theme results to return in the response.
listThemes_maxResults :: Lens.Lens' ListThemes (Prelude.Maybe Prelude.Natural)
listThemes_maxResults = Lens.lens (\ListThemes' {maxResults} -> maxResults) (\s@ListThemes' {} a -> s {maxResults = a} :: ListThemes)

-- | The token to request the next page of results.
listThemes_nextToken :: Lens.Lens' ListThemes (Prelude.Maybe Prelude.Text)
listThemes_nextToken = Lens.lens (\ListThemes' {nextToken} -> nextToken) (\s@ListThemes' {} a -> s {nextToken = a} :: ListThemes)

-- | The unique ID for the Amplify app.
listThemes_appId :: Lens.Lens' ListThemes Prelude.Text
listThemes_appId = Lens.lens (\ListThemes' {appId} -> appId) (\s@ListThemes' {} a -> s {appId = a} :: ListThemes)

-- | The name of the backend environment that is a part of the Amplify app.
listThemes_environmentName :: Lens.Lens' ListThemes Prelude.Text
listThemes_environmentName = Lens.lens (\ListThemes' {environmentName} -> environmentName) (\s@ListThemes' {} a -> s {environmentName = a} :: ListThemes)

instance Core.AWSPager ListThemes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listThemesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listThemesResponse_entities) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listThemes_nextToken
          Lens..~ rs
          Lens.^? listThemesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListThemes where
  type AWSResponse ListThemes = ListThemesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThemesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListThemes where
  hashWithSalt _salt ListThemes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ListThemes where
  rnf ListThemes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ListThemes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListThemes where
  toPath ListThemes' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/themes"
      ]

instance Data.ToQuery ListThemes where
  toQuery ListThemes' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListThemesResponse' smart constructor.
data ListThemesResponse = ListThemesResponse'
  { -- | The pagination token that\'s returned if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of themes for the Amplify app.
    entities :: [ThemeSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListThemesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listThemesResponse_nextToken' - The pagination token that\'s returned if more results are available.
--
-- 'httpStatus', 'listThemesResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'listThemesResponse_entities' - The list of themes for the Amplify app.
newListThemesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListThemesResponse
newListThemesResponse pHttpStatus_ =
  ListThemesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s returned if more results are available.
listThemesResponse_nextToken :: Lens.Lens' ListThemesResponse (Prelude.Maybe Prelude.Text)
listThemesResponse_nextToken = Lens.lens (\ListThemesResponse' {nextToken} -> nextToken) (\s@ListThemesResponse' {} a -> s {nextToken = a} :: ListThemesResponse)

-- | The response's http status code.
listThemesResponse_httpStatus :: Lens.Lens' ListThemesResponse Prelude.Int
listThemesResponse_httpStatus = Lens.lens (\ListThemesResponse' {httpStatus} -> httpStatus) (\s@ListThemesResponse' {} a -> s {httpStatus = a} :: ListThemesResponse)

-- | The list of themes for the Amplify app.
listThemesResponse_entities :: Lens.Lens' ListThemesResponse [ThemeSummary]
listThemesResponse_entities = Lens.lens (\ListThemesResponse' {entities} -> entities) (\s@ListThemesResponse' {} a -> s {entities = a} :: ListThemesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListThemesResponse where
  rnf ListThemesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
