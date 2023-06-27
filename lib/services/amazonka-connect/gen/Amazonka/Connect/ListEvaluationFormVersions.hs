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
-- Module      : Amazonka.Connect.ListEvaluationFormVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists versions of an evaluation form in the specified Amazon Connect
-- instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListEvaluationFormVersions
  ( -- * Creating a Request
    ListEvaluationFormVersions (..),
    newListEvaluationFormVersions,

    -- * Request Lenses
    listEvaluationFormVersions_maxResults,
    listEvaluationFormVersions_nextToken,
    listEvaluationFormVersions_instanceId,
    listEvaluationFormVersions_evaluationFormId,

    -- * Destructuring the Response
    ListEvaluationFormVersionsResponse (..),
    newListEvaluationFormVersionsResponse,

    -- * Response Lenses
    listEvaluationFormVersionsResponse_nextToken,
    listEvaluationFormVersionsResponse_httpStatus,
    listEvaluationFormVersionsResponse_evaluationFormVersionSummaryList,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEvaluationFormVersions' smart constructor.
data ListEvaluationFormVersions = ListEvaluationFormVersions'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The unique identifier for the evaluation form.
    evaluationFormId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEvaluationFormVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEvaluationFormVersions_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listEvaluationFormVersions_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listEvaluationFormVersions_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'evaluationFormId', 'listEvaluationFormVersions_evaluationFormId' - The unique identifier for the evaluation form.
newListEvaluationFormVersions ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'evaluationFormId'
  Prelude.Text ->
  ListEvaluationFormVersions
newListEvaluationFormVersions
  pInstanceId_
  pEvaluationFormId_ =
    ListEvaluationFormVersions'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        instanceId = pInstanceId_,
        evaluationFormId = pEvaluationFormId_
      }

-- | The maximum number of results to return per page.
listEvaluationFormVersions_maxResults :: Lens.Lens' ListEvaluationFormVersions (Prelude.Maybe Prelude.Natural)
listEvaluationFormVersions_maxResults = Lens.lens (\ListEvaluationFormVersions' {maxResults} -> maxResults) (\s@ListEvaluationFormVersions' {} a -> s {maxResults = a} :: ListEvaluationFormVersions)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listEvaluationFormVersions_nextToken :: Lens.Lens' ListEvaluationFormVersions (Prelude.Maybe Prelude.Text)
listEvaluationFormVersions_nextToken = Lens.lens (\ListEvaluationFormVersions' {nextToken} -> nextToken) (\s@ListEvaluationFormVersions' {} a -> s {nextToken = a} :: ListEvaluationFormVersions)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listEvaluationFormVersions_instanceId :: Lens.Lens' ListEvaluationFormVersions Prelude.Text
listEvaluationFormVersions_instanceId = Lens.lens (\ListEvaluationFormVersions' {instanceId} -> instanceId) (\s@ListEvaluationFormVersions' {} a -> s {instanceId = a} :: ListEvaluationFormVersions)

-- | The unique identifier for the evaluation form.
listEvaluationFormVersions_evaluationFormId :: Lens.Lens' ListEvaluationFormVersions Prelude.Text
listEvaluationFormVersions_evaluationFormId = Lens.lens (\ListEvaluationFormVersions' {evaluationFormId} -> evaluationFormId) (\s@ListEvaluationFormVersions' {} a -> s {evaluationFormId = a} :: ListEvaluationFormVersions)

instance Core.AWSPager ListEvaluationFormVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEvaluationFormVersionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEvaluationFormVersionsResponse_evaluationFormVersionSummaryList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEvaluationFormVersions_nextToken
          Lens..~ rs
          Lens.^? listEvaluationFormVersionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEvaluationFormVersions where
  type
    AWSResponse ListEvaluationFormVersions =
      ListEvaluationFormVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEvaluationFormVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "EvaluationFormVersionSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEvaluationFormVersions where
  hashWithSalt _salt ListEvaluationFormVersions' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` evaluationFormId

instance Prelude.NFData ListEvaluationFormVersions where
  rnf ListEvaluationFormVersions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf evaluationFormId

instance Data.ToHeaders ListEvaluationFormVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEvaluationFormVersions where
  toPath ListEvaluationFormVersions' {..} =
    Prelude.mconcat
      [ "/evaluation-forms/",
        Data.toBS instanceId,
        "/",
        Data.toBS evaluationFormId,
        "/versions"
      ]

instance Data.ToQuery ListEvaluationFormVersions where
  toQuery ListEvaluationFormVersions' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEvaluationFormVersionsResponse' smart constructor.
data ListEvaluationFormVersionsResponse = ListEvaluationFormVersionsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides details about a list of evaluation forms belonging to an
    -- instance.
    evaluationFormVersionSummaryList :: [EvaluationFormVersionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEvaluationFormVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEvaluationFormVersionsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listEvaluationFormVersionsResponse_httpStatus' - The response's http status code.
--
-- 'evaluationFormVersionSummaryList', 'listEvaluationFormVersionsResponse_evaluationFormVersionSummaryList' - Provides details about a list of evaluation forms belonging to an
-- instance.
newListEvaluationFormVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEvaluationFormVersionsResponse
newListEvaluationFormVersionsResponse pHttpStatus_ =
  ListEvaluationFormVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      evaluationFormVersionSummaryList =
        Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listEvaluationFormVersionsResponse_nextToken :: Lens.Lens' ListEvaluationFormVersionsResponse (Prelude.Maybe Prelude.Text)
listEvaluationFormVersionsResponse_nextToken = Lens.lens (\ListEvaluationFormVersionsResponse' {nextToken} -> nextToken) (\s@ListEvaluationFormVersionsResponse' {} a -> s {nextToken = a} :: ListEvaluationFormVersionsResponse)

-- | The response's http status code.
listEvaluationFormVersionsResponse_httpStatus :: Lens.Lens' ListEvaluationFormVersionsResponse Prelude.Int
listEvaluationFormVersionsResponse_httpStatus = Lens.lens (\ListEvaluationFormVersionsResponse' {httpStatus} -> httpStatus) (\s@ListEvaluationFormVersionsResponse' {} a -> s {httpStatus = a} :: ListEvaluationFormVersionsResponse)

-- | Provides details about a list of evaluation forms belonging to an
-- instance.
listEvaluationFormVersionsResponse_evaluationFormVersionSummaryList :: Lens.Lens' ListEvaluationFormVersionsResponse [EvaluationFormVersionSummary]
listEvaluationFormVersionsResponse_evaluationFormVersionSummaryList = Lens.lens (\ListEvaluationFormVersionsResponse' {evaluationFormVersionSummaryList} -> evaluationFormVersionSummaryList) (\s@ListEvaluationFormVersionsResponse' {} a -> s {evaluationFormVersionSummaryList = a} :: ListEvaluationFormVersionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListEvaluationFormVersionsResponse
  where
  rnf ListEvaluationFormVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationFormVersionSummaryList
