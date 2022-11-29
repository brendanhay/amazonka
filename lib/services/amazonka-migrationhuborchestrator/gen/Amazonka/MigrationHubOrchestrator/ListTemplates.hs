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
-- Module      : Amazonka.MigrationHubOrchestrator.ListTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the templates available in Migration Hub Orchestrator to create a
-- migration workflow.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListTemplates
  ( -- * Creating a Request
    ListTemplates (..),
    newListTemplates,

    -- * Request Lenses
    listTemplates_name,
    listTemplates_nextToken,
    listTemplates_maxResults,

    -- * Destructuring the Response
    ListTemplatesResponse (..),
    newListTemplatesResponse,

    -- * Response Lenses
    listTemplatesResponse_nextToken,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templateSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'listTemplates_name' - The name of the template.
--
-- 'nextToken', 'listTemplates_nextToken' - The pagination token.
--
-- 'maxResults', 'listTemplates_maxResults' - The maximum number of results that can be returned.
newListTemplates ::
  ListTemplates
newListTemplates =
  ListTemplates'
    { name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The name of the template.
listTemplates_name :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Text)
listTemplates_name = Lens.lens (\ListTemplates' {name} -> name) (\s@ListTemplates' {} a -> s {name = a} :: ListTemplates)

-- | The pagination token.
listTemplates_nextToken :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Text)
listTemplates_nextToken = Lens.lens (\ListTemplates' {nextToken} -> nextToken) (\s@ListTemplates' {} a -> s {nextToken = a} :: ListTemplates)

-- | The maximum number of results that can be returned.
listTemplates_maxResults :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Natural)
listTemplates_maxResults = Lens.lens (\ListTemplates' {maxResults} -> maxResults) (\s@ListTemplates' {} a -> s {maxResults = a} :: ListTemplates)

instance Core.AWSPager ListTemplates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplatesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listTemplatesResponse_templateSummary) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTemplates_nextToken
          Lens..~ rs
          Lens.^? listTemplatesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTemplates where
  type
    AWSResponse ListTemplates =
      ListTemplatesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "templateSummary"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTemplates where
  hashWithSalt _salt ListTemplates' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListTemplates where
  rnf ListTemplates' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTemplates where
  toPath = Prelude.const "/migrationworkflowtemplates"

instance Core.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Prelude.mconcat
      [ "name" Core.=: name,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary of the template.
    templateSummary :: [TemplateSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplatesResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'templateSummary', 'listTemplatesResponse_templateSummary' - The summary of the template.
newListTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplatesResponse
newListTemplatesResponse pHttpStatus_ =
  ListTemplatesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      templateSummary = Prelude.mempty
    }

-- | The pagination token.
listTemplatesResponse_nextToken :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe Prelude.Text)
listTemplatesResponse_nextToken = Lens.lens (\ListTemplatesResponse' {nextToken} -> nextToken) (\s@ListTemplatesResponse' {} a -> s {nextToken = a} :: ListTemplatesResponse)

-- | The response's http status code.
listTemplatesResponse_httpStatus :: Lens.Lens' ListTemplatesResponse Prelude.Int
listTemplatesResponse_httpStatus = Lens.lens (\ListTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListTemplatesResponse' {} a -> s {httpStatus = a} :: ListTemplatesResponse)

-- | The summary of the template.
listTemplatesResponse_templateSummary :: Lens.Lens' ListTemplatesResponse [TemplateSummary]
listTemplatesResponse_templateSummary = Lens.lens (\ListTemplatesResponse' {templateSummary} -> templateSummary) (\s@ListTemplatesResponse' {} a -> s {templateSummary = a} :: ListTemplatesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTemplatesResponse where
  rnf ListTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templateSummary
