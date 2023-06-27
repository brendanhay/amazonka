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
-- Module      : Amazonka.Connect.ListEvaluationForms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists evaluation forms in the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListEvaluationForms
  ( -- * Creating a Request
    ListEvaluationForms (..),
    newListEvaluationForms,

    -- * Request Lenses
    listEvaluationForms_maxResults,
    listEvaluationForms_nextToken,
    listEvaluationForms_instanceId,

    -- * Destructuring the Response
    ListEvaluationFormsResponse (..),
    newListEvaluationFormsResponse,

    -- * Response Lenses
    listEvaluationFormsResponse_nextToken,
    listEvaluationFormsResponse_httpStatus,
    listEvaluationFormsResponse_evaluationFormSummaryList,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEvaluationForms' smart constructor.
data ListEvaluationForms = ListEvaluationForms'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEvaluationForms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEvaluationForms_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listEvaluationForms_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listEvaluationForms_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newListEvaluationForms ::
  -- | 'instanceId'
  Prelude.Text ->
  ListEvaluationForms
newListEvaluationForms pInstanceId_ =
  ListEvaluationForms'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The maximum number of results to return per page.
listEvaluationForms_maxResults :: Lens.Lens' ListEvaluationForms (Prelude.Maybe Prelude.Natural)
listEvaluationForms_maxResults = Lens.lens (\ListEvaluationForms' {maxResults} -> maxResults) (\s@ListEvaluationForms' {} a -> s {maxResults = a} :: ListEvaluationForms)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listEvaluationForms_nextToken :: Lens.Lens' ListEvaluationForms (Prelude.Maybe Prelude.Text)
listEvaluationForms_nextToken = Lens.lens (\ListEvaluationForms' {nextToken} -> nextToken) (\s@ListEvaluationForms' {} a -> s {nextToken = a} :: ListEvaluationForms)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listEvaluationForms_instanceId :: Lens.Lens' ListEvaluationForms Prelude.Text
listEvaluationForms_instanceId = Lens.lens (\ListEvaluationForms' {instanceId} -> instanceId) (\s@ListEvaluationForms' {} a -> s {instanceId = a} :: ListEvaluationForms)

instance Core.AWSPager ListEvaluationForms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEvaluationFormsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEvaluationFormsResponse_evaluationFormSummaryList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEvaluationForms_nextToken
          Lens..~ rs
          Lens.^? listEvaluationFormsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEvaluationForms where
  type
    AWSResponse ListEvaluationForms =
      ListEvaluationFormsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEvaluationFormsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "EvaluationFormSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEvaluationForms where
  hashWithSalt _salt ListEvaluationForms' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListEvaluationForms where
  rnf ListEvaluationForms' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListEvaluationForms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEvaluationForms where
  toPath ListEvaluationForms' {..} =
    Prelude.mconcat
      ["/evaluation-forms/", Data.toBS instanceId]

instance Data.ToQuery ListEvaluationForms where
  toQuery ListEvaluationForms' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEvaluationFormsResponse' smart constructor.
data ListEvaluationFormsResponse = ListEvaluationFormsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides details about a list of evaluation forms belonging to an
    -- instance.
    evaluationFormSummaryList :: [EvaluationFormSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEvaluationFormsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEvaluationFormsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listEvaluationFormsResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormSummaryList', 'listEvaluationFormsResponse_evaluationFormSummaryList' - Provides details about a list of evaluation forms belonging to an
-- instance.
newListEvaluationFormsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEvaluationFormsResponse
newListEvaluationFormsResponse pHttpStatus_ =
  ListEvaluationFormsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      evaluationFormSummaryList = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listEvaluationFormsResponse_nextToken :: Lens.Lens' ListEvaluationFormsResponse (Prelude.Maybe Prelude.Text)
listEvaluationFormsResponse_nextToken = Lens.lens (\ListEvaluationFormsResponse' {nextToken} -> nextToken) (\s@ListEvaluationFormsResponse' {} a -> s {nextToken = a} :: ListEvaluationFormsResponse)

-- | The response's http status code.
listEvaluationFormsResponse_httpStatus :: Lens.Lens' ListEvaluationFormsResponse Prelude.Int
listEvaluationFormsResponse_httpStatus = Lens.lens (\ListEvaluationFormsResponse' {httpStatus} -> httpStatus) (\s@ListEvaluationFormsResponse' {} a -> s {httpStatus = a} :: ListEvaluationFormsResponse)

-- | Provides details about a list of evaluation forms belonging to an
-- instance.
listEvaluationFormsResponse_evaluationFormSummaryList :: Lens.Lens' ListEvaluationFormsResponse [EvaluationFormSummary]
listEvaluationFormsResponse_evaluationFormSummaryList = Lens.lens (\ListEvaluationFormsResponse' {evaluationFormSummaryList} -> evaluationFormSummaryList) (\s@ListEvaluationFormsResponse' {} a -> s {evaluationFormSummaryList = a} :: ListEvaluationFormsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEvaluationFormsResponse where
  rnf ListEvaluationFormsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormSummaryList
