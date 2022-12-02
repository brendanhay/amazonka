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
-- Module      : Amazonka.ConnectCases.ListTemplates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the templates in a Cases domain. Each list item is a
-- condensed summary object of the template.
module Amazonka.ConnectCases.ListTemplates
  ( -- * Creating a Request
    ListTemplates (..),
    newListTemplates,

    -- * Request Lenses
    listTemplates_nextToken,
    listTemplates_status,
    listTemplates_maxResults,
    listTemplates_domainId,

    -- * Destructuring the Response
    ListTemplatesResponse (..),
    newListTemplatesResponse,

    -- * Response Lenses
    listTemplatesResponse_nextToken,
    listTemplatesResponse_httpStatus,
    listTemplatesResponse_templates,
  )
where

import Amazonka.ConnectCases.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplates' smart constructor.
data ListTemplates = ListTemplates'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of status values to filter on.
    status :: Prelude.Maybe (Prelude.NonEmpty TemplateStatus),
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier of the Cases domain.
    domainId :: Prelude.Text
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
-- 'nextToken', 'listTemplates_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'status', 'listTemplates_status' - A list of status values to filter on.
--
-- 'maxResults', 'listTemplates_maxResults' - The maximum number of results to return per page.
--
-- 'domainId', 'listTemplates_domainId' - The unique identifier of the Cases domain.
newListTemplates ::
  -- | 'domainId'
  Prelude.Text ->
  ListTemplates
newListTemplates pDomainId_ =
  ListTemplates'
    { nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      domainId = pDomainId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listTemplates_nextToken :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Text)
listTemplates_nextToken = Lens.lens (\ListTemplates' {nextToken} -> nextToken) (\s@ListTemplates' {} a -> s {nextToken = a} :: ListTemplates)

-- | A list of status values to filter on.
listTemplates_status :: Lens.Lens' ListTemplates (Prelude.Maybe (Prelude.NonEmpty TemplateStatus))
listTemplates_status = Lens.lens (\ListTemplates' {status} -> status) (\s@ListTemplates' {} a -> s {status = a} :: ListTemplates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return per page.
listTemplates_maxResults :: Lens.Lens' ListTemplates (Prelude.Maybe Prelude.Natural)
listTemplates_maxResults = Lens.lens (\ListTemplates' {maxResults} -> maxResults) (\s@ListTemplates' {} a -> s {maxResults = a} :: ListTemplates)

-- | The unique identifier of the Cases domain.
listTemplates_domainId :: Lens.Lens' ListTemplates Prelude.Text
listTemplates_domainId = Lens.lens (\ListTemplates' {domainId} -> domainId) (\s@ListTemplates' {} a -> s {domainId = a} :: ListTemplates)

instance Core.AWSRequest ListTemplates where
  type
    AWSResponse ListTemplates =
      ListTemplatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplatesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "templates" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTemplates where
  hashWithSalt _salt ListTemplates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` domainId

instance Prelude.NFData ListTemplates where
  rnf ListTemplates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf domainId

instance Data.ToHeaders ListTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTemplates where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListTemplates where
  toPath ListTemplates' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainId, "/templates-list"]

instance Data.ToQuery ListTemplates where
  toQuery ListTemplates' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "status"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> status),
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListTemplatesResponse' smart constructor.
data ListTemplatesResponse = ListTemplatesResponse'
  { -- | The token for the next set of results. This is null if there are no more
    -- results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of template summary objects.
    templates :: [TemplateSummary]
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
-- 'nextToken', 'listTemplatesResponse_nextToken' - The token for the next set of results. This is null if there are no more
-- results to return.
--
-- 'httpStatus', 'listTemplatesResponse_httpStatus' - The response's http status code.
--
-- 'templates', 'listTemplatesResponse_templates' - List of template summary objects.
newListTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplatesResponse
newListTemplatesResponse pHttpStatus_ =
  ListTemplatesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      templates = Prelude.mempty
    }

-- | The token for the next set of results. This is null if there are no more
-- results to return.
listTemplatesResponse_nextToken :: Lens.Lens' ListTemplatesResponse (Prelude.Maybe Prelude.Text)
listTemplatesResponse_nextToken = Lens.lens (\ListTemplatesResponse' {nextToken} -> nextToken) (\s@ListTemplatesResponse' {} a -> s {nextToken = a} :: ListTemplatesResponse)

-- | The response's http status code.
listTemplatesResponse_httpStatus :: Lens.Lens' ListTemplatesResponse Prelude.Int
listTemplatesResponse_httpStatus = Lens.lens (\ListTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListTemplatesResponse' {} a -> s {httpStatus = a} :: ListTemplatesResponse)

-- | List of template summary objects.
listTemplatesResponse_templates :: Lens.Lens' ListTemplatesResponse [TemplateSummary]
listTemplatesResponse_templates = Lens.lens (\ListTemplatesResponse' {templates} -> templates) (\s@ListTemplatesResponse' {} a -> s {templates = a} :: ListTemplatesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTemplatesResponse where
  rnf ListTemplatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf templates
