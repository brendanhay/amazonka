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
-- Module      : Amazonka.MigrationHubOrchestrator.ListWorkflows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the migration workflows.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListWorkflows
  ( -- * Creating a Request
    ListWorkflows (..),
    newListWorkflows,

    -- * Request Lenses
    listWorkflows_adsApplicationConfigurationName,
    listWorkflows_maxResults,
    listWorkflows_name,
    listWorkflows_nextToken,
    listWorkflows_status,
    listWorkflows_templateId,

    -- * Destructuring the Response
    ListWorkflowsResponse (..),
    newListWorkflowsResponse,

    -- * Response Lenses
    listWorkflowsResponse_nextToken,
    listWorkflowsResponse_httpStatus,
    listWorkflowsResponse_migrationWorkflowSummary,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWorkflows' smart constructor.
data ListWorkflows = ListWorkflows'
  { -- | The name of the application configured in Application Discovery Service.
    adsApplicationConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the migration workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the migration workflow.
    status :: Prelude.Maybe MigrationWorkflowStatusEnum,
    -- | The ID of the template.
    templateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adsApplicationConfigurationName', 'listWorkflows_adsApplicationConfigurationName' - The name of the application configured in Application Discovery Service.
--
-- 'maxResults', 'listWorkflows_maxResults' - The maximum number of results that can be returned.
--
-- 'name', 'listWorkflows_name' - The name of the migration workflow.
--
-- 'nextToken', 'listWorkflows_nextToken' - The pagination token.
--
-- 'status', 'listWorkflows_status' - The status of the migration workflow.
--
-- 'templateId', 'listWorkflows_templateId' - The ID of the template.
newListWorkflows ::
  ListWorkflows
newListWorkflows =
  ListWorkflows'
    { adsApplicationConfigurationName =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      templateId = Prelude.Nothing
    }

-- | The name of the application configured in Application Discovery Service.
listWorkflows_adsApplicationConfigurationName :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_adsApplicationConfigurationName = Lens.lens (\ListWorkflows' {adsApplicationConfigurationName} -> adsApplicationConfigurationName) (\s@ListWorkflows' {} a -> s {adsApplicationConfigurationName = a} :: ListWorkflows)

-- | The maximum number of results that can be returned.
listWorkflows_maxResults :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Natural)
listWorkflows_maxResults = Lens.lens (\ListWorkflows' {maxResults} -> maxResults) (\s@ListWorkflows' {} a -> s {maxResults = a} :: ListWorkflows)

-- | The name of the migration workflow.
listWorkflows_name :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_name = Lens.lens (\ListWorkflows' {name} -> name) (\s@ListWorkflows' {} a -> s {name = a} :: ListWorkflows)

-- | The pagination token.
listWorkflows_nextToken :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_nextToken = Lens.lens (\ListWorkflows' {nextToken} -> nextToken) (\s@ListWorkflows' {} a -> s {nextToken = a} :: ListWorkflows)

-- | The status of the migration workflow.
listWorkflows_status :: Lens.Lens' ListWorkflows (Prelude.Maybe MigrationWorkflowStatusEnum)
listWorkflows_status = Lens.lens (\ListWorkflows' {status} -> status) (\s@ListWorkflows' {} a -> s {status = a} :: ListWorkflows)

-- | The ID of the template.
listWorkflows_templateId :: Lens.Lens' ListWorkflows (Prelude.Maybe Prelude.Text)
listWorkflows_templateId = Lens.lens (\ListWorkflows' {templateId} -> templateId) (\s@ListWorkflows' {} a -> s {templateId = a} :: ListWorkflows)

instance Core.AWSPager ListWorkflows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorkflowsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listWorkflowsResponse_migrationWorkflowSummary
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorkflows_nextToken
          Lens..~ rs
          Lens.^? listWorkflowsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWorkflows where
  type
    AWSResponse ListWorkflows =
      ListWorkflowsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorkflowsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "migrationWorkflowSummary"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListWorkflows where
  hashWithSalt _salt ListWorkflows' {..} =
    _salt
      `Prelude.hashWithSalt` adsApplicationConfigurationName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData ListWorkflows where
  rnf ListWorkflows' {..} =
    Prelude.rnf adsApplicationConfigurationName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateId

instance Data.ToHeaders ListWorkflows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListWorkflows where
  toPath = Prelude.const "/migrationworkflows"

instance Data.ToQuery ListWorkflows where
  toQuery ListWorkflows' {..} =
    Prelude.mconcat
      [ "adsApplicationConfigurationName"
          Data.=: adsApplicationConfigurationName,
        "maxResults" Data.=: maxResults,
        "name" Data.=: name,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status,
        "templateId" Data.=: templateId
      ]

-- | /See:/ 'newListWorkflowsResponse' smart constructor.
data ListWorkflowsResponse = ListWorkflowsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summary of the migration workflow.
    migrationWorkflowSummary :: [MigrationWorkflowSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorkflowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorkflowsResponse_nextToken' - The pagination token.
--
-- 'httpStatus', 'listWorkflowsResponse_httpStatus' - The response's http status code.
--
-- 'migrationWorkflowSummary', 'listWorkflowsResponse_migrationWorkflowSummary' - The summary of the migration workflow.
newListWorkflowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorkflowsResponse
newListWorkflowsResponse pHttpStatus_ =
  ListWorkflowsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      migrationWorkflowSummary = Prelude.mempty
    }

-- | The pagination token.
listWorkflowsResponse_nextToken :: Lens.Lens' ListWorkflowsResponse (Prelude.Maybe Prelude.Text)
listWorkflowsResponse_nextToken = Lens.lens (\ListWorkflowsResponse' {nextToken} -> nextToken) (\s@ListWorkflowsResponse' {} a -> s {nextToken = a} :: ListWorkflowsResponse)

-- | The response's http status code.
listWorkflowsResponse_httpStatus :: Lens.Lens' ListWorkflowsResponse Prelude.Int
listWorkflowsResponse_httpStatus = Lens.lens (\ListWorkflowsResponse' {httpStatus} -> httpStatus) (\s@ListWorkflowsResponse' {} a -> s {httpStatus = a} :: ListWorkflowsResponse)

-- | The summary of the migration workflow.
listWorkflowsResponse_migrationWorkflowSummary :: Lens.Lens' ListWorkflowsResponse [MigrationWorkflowSummary]
listWorkflowsResponse_migrationWorkflowSummary = Lens.lens (\ListWorkflowsResponse' {migrationWorkflowSummary} -> migrationWorkflowSummary) (\s@ListWorkflowsResponse' {} a -> s {migrationWorkflowSummary = a} :: ListWorkflowsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListWorkflowsResponse where
  rnf ListWorkflowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf migrationWorkflowSummary
