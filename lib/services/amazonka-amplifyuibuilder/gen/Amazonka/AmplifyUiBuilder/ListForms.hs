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
-- Module      : Amazonka.AmplifyUiBuilder.ListForms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of forms for a specified Amplify app and backend
-- environment.
--
-- This operation returns paginated results.
module Amazonka.AmplifyUiBuilder.ListForms
  ( -- * Creating a Request
    ListForms (..),
    newListForms,

    -- * Request Lenses
    listForms_maxResults,
    listForms_nextToken,
    listForms_appId,
    listForms_environmentName,

    -- * Destructuring the Response
    ListFormsResponse (..),
    newListFormsResponse,

    -- * Response Lenses
    listFormsResponse_nextToken,
    listFormsResponse_httpStatus,
    listFormsResponse_entities,
  )
where

import Amazonka.AmplifyUiBuilder.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListForms' smart constructor.
data ListForms = ListForms'
  { -- | The maximum number of forms to retrieve.
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
-- Create a value of 'ListForms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listForms_maxResults' - The maximum number of forms to retrieve.
--
-- 'nextToken', 'listForms_nextToken' - The token to request the next page of results.
--
-- 'appId', 'listForms_appId' - The unique ID for the Amplify app.
--
-- 'environmentName', 'listForms_environmentName' - The name of the backend environment that is a part of the Amplify app.
newListForms ::
  -- | 'appId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  ListForms
newListForms pAppId_ pEnvironmentName_ =
  ListForms'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appId = pAppId_,
      environmentName = pEnvironmentName_
    }

-- | The maximum number of forms to retrieve.
listForms_maxResults :: Lens.Lens' ListForms (Prelude.Maybe Prelude.Natural)
listForms_maxResults = Lens.lens (\ListForms' {maxResults} -> maxResults) (\s@ListForms' {} a -> s {maxResults = a} :: ListForms)

-- | The token to request the next page of results.
listForms_nextToken :: Lens.Lens' ListForms (Prelude.Maybe Prelude.Text)
listForms_nextToken = Lens.lens (\ListForms' {nextToken} -> nextToken) (\s@ListForms' {} a -> s {nextToken = a} :: ListForms)

-- | The unique ID for the Amplify app.
listForms_appId :: Lens.Lens' ListForms Prelude.Text
listForms_appId = Lens.lens (\ListForms' {appId} -> appId) (\s@ListForms' {} a -> s {appId = a} :: ListForms)

-- | The name of the backend environment that is a part of the Amplify app.
listForms_environmentName :: Lens.Lens' ListForms Prelude.Text
listForms_environmentName = Lens.lens (\ListForms' {environmentName} -> environmentName) (\s@ListForms' {} a -> s {environmentName = a} :: ListForms)

instance Core.AWSPager ListForms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFormsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listFormsResponse_entities) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listForms_nextToken
          Lens..~ rs
          Lens.^? listFormsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListForms where
  type AWSResponse ListForms = ListFormsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFormsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "entities" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListForms where
  hashWithSalt _salt ListForms' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appId
      `Prelude.hashWithSalt` environmentName

instance Prelude.NFData ListForms where
  rnf ListForms' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appId
      `Prelude.seq` Prelude.rnf environmentName

instance Data.ToHeaders ListForms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListForms where
  toPath ListForms' {..} =
    Prelude.mconcat
      [ "/app/",
        Data.toBS appId,
        "/environment/",
        Data.toBS environmentName,
        "/forms"
      ]

instance Data.ToQuery ListForms where
  toQuery ListForms' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFormsResponse' smart constructor.
data ListFormsResponse = ListFormsResponse'
  { -- | The pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of forms for the Amplify app.
    entities :: [FormSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFormsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFormsResponse_nextToken' - The pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listFormsResponse_httpStatus' - The response's http status code.
--
-- 'entities', 'listFormsResponse_entities' - The list of forms for the Amplify app.
newListFormsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFormsResponse
newListFormsResponse pHttpStatus_ =
  ListFormsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      entities = Prelude.mempty
    }

-- | The pagination token that\'s included if more results are available.
listFormsResponse_nextToken :: Lens.Lens' ListFormsResponse (Prelude.Maybe Prelude.Text)
listFormsResponse_nextToken = Lens.lens (\ListFormsResponse' {nextToken} -> nextToken) (\s@ListFormsResponse' {} a -> s {nextToken = a} :: ListFormsResponse)

-- | The response's http status code.
listFormsResponse_httpStatus :: Lens.Lens' ListFormsResponse Prelude.Int
listFormsResponse_httpStatus = Lens.lens (\ListFormsResponse' {httpStatus} -> httpStatus) (\s@ListFormsResponse' {} a -> s {httpStatus = a} :: ListFormsResponse)

-- | The list of forms for the Amplify app.
listFormsResponse_entities :: Lens.Lens' ListFormsResponse [FormSummary]
listFormsResponse_entities = Lens.lens (\ListFormsResponse' {entities} -> entities) (\s@ListFormsResponse' {} a -> s {entities = a} :: ListFormsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFormsResponse where
  rnf ListFormsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf entities
