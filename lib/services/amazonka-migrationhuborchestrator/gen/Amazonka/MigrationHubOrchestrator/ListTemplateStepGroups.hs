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
-- Module      : Amazonka.MigrationHubOrchestrator.ListTemplateStepGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the step groups in a template.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListTemplateStepGroups
  ( -- * Creating a Request
    ListTemplateStepGroups (..),
    newListTemplateStepGroups,

    -- * Request Lenses
    listTemplateStepGroups_nextToken,
    listTemplateStepGroups_maxResults,
    listTemplateStepGroups_templateId,

    -- * Destructuring the Response
    ListTemplateStepGroupsResponse (..),
    newListTemplateStepGroupsResponse,

    -- * Response Lenses
    listTemplateStepGroupsResponse_nextToken,
    listTemplateStepGroupsResponse_httpStatus,
    listTemplateStepGroupsResponse_templateStepGroupSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplateStepGroups' smart constructor.
data ListTemplateStepGroups = ListTemplateStepGroups'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateStepGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateStepGroups_nextToken' - The pagination token.
--
-- 'maxResults', 'listTemplateStepGroups_maxResults' - The maximum number of results that can be returned.
--
-- 'templateId', 'listTemplateStepGroups_templateId' - The ID of the template.
newListTemplateStepGroups ::
  -- | 'templateId'
  Prelude.Text ->
  ListTemplateStepGroups
newListTemplateStepGroups pTemplateId_ =
  ListTemplateStepGroups'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      templateId = pTemplateId_
    }

-- | The pagination token.
listTemplateStepGroups_nextToken :: Lens.Lens' ListTemplateStepGroups (Prelude.Maybe Prelude.Text)
listTemplateStepGroups_nextToken = Lens.lens (\ListTemplateStepGroups' {nextToken} -> nextToken) (\s@ListTemplateStepGroups' {} a -> s {nextToken = a} :: ListTemplateStepGroups)

-- | The maximum number of results that can be returned.
listTemplateStepGroups_maxResults :: Lens.Lens' ListTemplateStepGroups (Prelude.Maybe Prelude.Natural)
listTemplateStepGroups_maxResults = Lens.lens (\ListTemplateStepGroups' {maxResults} -> maxResults) (\s@ListTemplateStepGroups' {} a -> s {maxResults = a} :: ListTemplateStepGroups)

-- | The ID of the template.
listTemplateStepGroups_templateId :: Lens.Lens' ListTemplateStepGroups Prelude.Text
listTemplateStepGroups_templateId = Lens.lens (\ListTemplateStepGroups' {templateId} -> templateId) (\s@ListTemplateStepGroups' {} a -> s {templateId = a} :: ListTemplateStepGroups)

instance Core.AWSPager ListTemplateStepGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplateStepGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listTemplateStepGroupsResponse_templateStepGroupSummary
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTemplateStepGroups_nextToken
          Lens..~ rs
          Lens.^? listTemplateStepGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTemplateStepGroups where
  type
    AWSResponse ListTemplateStepGroups =
      ListTemplateStepGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateStepGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "templateStepGroupSummary"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListTemplateStepGroups where
  hashWithSalt _salt ListTemplateStepGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData ListTemplateStepGroups where
  rnf ListTemplateStepGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf templateId

instance Core.ToHeaders ListTemplateStepGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTemplateStepGroups where
  toPath ListTemplateStepGroups' {..} =
    Prelude.mconcat
      ["/templatestepgroups/", Core.toBS templateId]

instance Core.ToQuery ListTemplateStepGroups where
  toQuery ListTemplateStepGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTemplateStepGroupsResponse' smart constructor.
data ListTemplateStepGroupsResponse = ListTemplateStepGroupsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary of the step group in the template.
    templateStepGroupSummary :: [TemplateStepGroupSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateStepGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateStepGroupsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listTemplateStepGroupsResponse_httpStatus' - The response's http status code.
--
-- 'templateStepGroupSummary', 'listTemplateStepGroupsResponse_templateStepGroupSummary' - The summary of the step group in the template.
newListTemplateStepGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplateStepGroupsResponse
newListTemplateStepGroupsResponse pHttpStatus_ =
  ListTemplateStepGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      templateStepGroupSummary = Prelude.mempty
    }

-- | The pagination token.
listTemplateStepGroupsResponse_nextToken :: Lens.Lens' ListTemplateStepGroupsResponse (Prelude.Maybe Prelude.Text)
listTemplateStepGroupsResponse_nextToken = Lens.lens (\ListTemplateStepGroupsResponse' {nextToken} -> nextToken) (\s@ListTemplateStepGroupsResponse' {} a -> s {nextToken = a} :: ListTemplateStepGroupsResponse)

-- | The response's http status code.
listTemplateStepGroupsResponse_httpStatus :: Lens.Lens' ListTemplateStepGroupsResponse Prelude.Int
listTemplateStepGroupsResponse_httpStatus = Lens.lens (\ListTemplateStepGroupsResponse' {httpStatus} -> httpStatus) (\s@ListTemplateStepGroupsResponse' {} a -> s {httpStatus = a} :: ListTemplateStepGroupsResponse)

-- | The summary of the step group in the template.
listTemplateStepGroupsResponse_templateStepGroupSummary :: Lens.Lens' ListTemplateStepGroupsResponse [TemplateStepGroupSummary]
listTemplateStepGroupsResponse_templateStepGroupSummary = Lens.lens (\ListTemplateStepGroupsResponse' {templateStepGroupSummary} -> templateStepGroupSummary) (\s@ListTemplateStepGroupsResponse' {} a -> s {templateStepGroupSummary = a} :: ListTemplateStepGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListTemplateStepGroupsResponse
  where
  rnf ListTemplateStepGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templateStepGroupSummary
