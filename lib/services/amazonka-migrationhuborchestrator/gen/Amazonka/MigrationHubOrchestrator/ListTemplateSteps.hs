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
-- Module      : Amazonka.MigrationHubOrchestrator.ListTemplateSteps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the steps in a template.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubOrchestrator.ListTemplateSteps
  ( -- * Creating a Request
    ListTemplateSteps (..),
    newListTemplateSteps,

    -- * Request Lenses
    listTemplateSteps_maxResults,
    listTemplateSteps_nextToken,
    listTemplateSteps_templateId,
    listTemplateSteps_stepGroupId,

    -- * Destructuring the Response
    ListTemplateStepsResponse (..),
    newListTemplateStepsResponse,

    -- * Response Lenses
    listTemplateStepsResponse_nextToken,
    listTemplateStepsResponse_templateStepSummaryList,
    listTemplateStepsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTemplateSteps' smart constructor.
data ListTemplateSteps = ListTemplateSteps'
  { -- | The maximum number of results that can be returned.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template.
    templateId :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateSteps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTemplateSteps_maxResults' - The maximum number of results that can be returned.
--
-- 'nextToken', 'listTemplateSteps_nextToken' - The pagination token.
--
-- 'templateId', 'listTemplateSteps_templateId' - The ID of the template.
--
-- 'stepGroupId', 'listTemplateSteps_stepGroupId' - The ID of the step group.
newListTemplateSteps ::
  -- | 'templateId'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  ListTemplateSteps
newListTemplateSteps pTemplateId_ pStepGroupId_ =
  ListTemplateSteps'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      templateId = pTemplateId_,
      stepGroupId = pStepGroupId_
    }

-- | The maximum number of results that can be returned.
listTemplateSteps_maxResults :: Lens.Lens' ListTemplateSteps (Prelude.Maybe Prelude.Natural)
listTemplateSteps_maxResults = Lens.lens (\ListTemplateSteps' {maxResults} -> maxResults) (\s@ListTemplateSteps' {} a -> s {maxResults = a} :: ListTemplateSteps)

-- | The pagination token.
listTemplateSteps_nextToken :: Lens.Lens' ListTemplateSteps (Prelude.Maybe Prelude.Text)
listTemplateSteps_nextToken = Lens.lens (\ListTemplateSteps' {nextToken} -> nextToken) (\s@ListTemplateSteps' {} a -> s {nextToken = a} :: ListTemplateSteps)

-- | The ID of the template.
listTemplateSteps_templateId :: Lens.Lens' ListTemplateSteps Prelude.Text
listTemplateSteps_templateId = Lens.lens (\ListTemplateSteps' {templateId} -> templateId) (\s@ListTemplateSteps' {} a -> s {templateId = a} :: ListTemplateSteps)

-- | The ID of the step group.
listTemplateSteps_stepGroupId :: Lens.Lens' ListTemplateSteps Prelude.Text
listTemplateSteps_stepGroupId = Lens.lens (\ListTemplateSteps' {stepGroupId} -> stepGroupId) (\s@ListTemplateSteps' {} a -> s {stepGroupId = a} :: ListTemplateSteps)

instance Core.AWSPager ListTemplateSteps where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTemplateStepsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTemplateStepsResponse_templateStepSummaryList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTemplateSteps_nextToken
          Lens..~ rs
          Lens.^? listTemplateStepsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListTemplateSteps where
  type
    AWSResponse ListTemplateSteps =
      ListTemplateStepsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTemplateStepsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "templateStepSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTemplateSteps where
  hashWithSalt _salt ListTemplateSteps' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` stepGroupId

instance Prelude.NFData ListTemplateSteps where
  rnf ListTemplateSteps' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf stepGroupId

instance Data.ToHeaders ListTemplateSteps where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTemplateSteps where
  toPath = Prelude.const "/templatesteps"

instance Data.ToQuery ListTemplateSteps where
  toQuery ListTemplateSteps' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "templateId" Data.=: templateId,
        "stepGroupId" Data.=: stepGroupId
      ]

-- | /See:/ 'newListTemplateStepsResponse' smart constructor.
data ListTemplateStepsResponse = ListTemplateStepsResponse'
  { -- | The pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of summaries of steps in a template.
    templateStepSummaryList :: Prelude.Maybe [TemplateStepSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTemplateStepsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTemplateStepsResponse_nextToken' - The pagination token.
--
-- 'templateStepSummaryList', 'listTemplateStepsResponse_templateStepSummaryList' - The list of summaries of steps in a template.
--
-- 'httpStatus', 'listTemplateStepsResponse_httpStatus' - The response's http status code.
newListTemplateStepsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTemplateStepsResponse
newListTemplateStepsResponse pHttpStatus_ =
  ListTemplateStepsResponse'
    { nextToken =
        Prelude.Nothing,
      templateStepSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token.
listTemplateStepsResponse_nextToken :: Lens.Lens' ListTemplateStepsResponse (Prelude.Maybe Prelude.Text)
listTemplateStepsResponse_nextToken = Lens.lens (\ListTemplateStepsResponse' {nextToken} -> nextToken) (\s@ListTemplateStepsResponse' {} a -> s {nextToken = a} :: ListTemplateStepsResponse)

-- | The list of summaries of steps in a template.
listTemplateStepsResponse_templateStepSummaryList :: Lens.Lens' ListTemplateStepsResponse (Prelude.Maybe [TemplateStepSummary])
listTemplateStepsResponse_templateStepSummaryList = Lens.lens (\ListTemplateStepsResponse' {templateStepSummaryList} -> templateStepSummaryList) (\s@ListTemplateStepsResponse' {} a -> s {templateStepSummaryList = a} :: ListTemplateStepsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTemplateStepsResponse_httpStatus :: Lens.Lens' ListTemplateStepsResponse Prelude.Int
listTemplateStepsResponse_httpStatus = Lens.lens (\ListTemplateStepsResponse' {httpStatus} -> httpStatus) (\s@ListTemplateStepsResponse' {} a -> s {httpStatus = a} :: ListTemplateStepsResponse)

instance Prelude.NFData ListTemplateStepsResponse where
  rnf ListTemplateStepsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf templateStepSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
