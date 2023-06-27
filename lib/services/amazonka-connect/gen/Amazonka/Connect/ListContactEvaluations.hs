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
-- Module      : Amazonka.Connect.ListContactEvaluations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists contact evaluations in the specified Amazon Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListContactEvaluations
  ( -- * Creating a Request
    ListContactEvaluations (..),
    newListContactEvaluations,

    -- * Request Lenses
    listContactEvaluations_nextToken,
    listContactEvaluations_instanceId,
    listContactEvaluations_contactId,

    -- * Destructuring the Response
    ListContactEvaluationsResponse (..),
    newListContactEvaluationsResponse,

    -- * Response Lenses
    listContactEvaluationsResponse_nextToken,
    listContactEvaluationsResponse_httpStatus,
    listContactEvaluationsResponse_evaluationSummaryList,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContactEvaluations' smart constructor.
data ListContactEvaluations = ListContactEvaluations'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    --
    -- This is not expected to be set because the value returned in the
    -- previous response is always null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the contact in this instance of Amazon Connect.
    contactId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactEvaluations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactEvaluations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- This is not expected to be set because the value returned in the
-- previous response is always null.
--
-- 'instanceId', 'listContactEvaluations_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
--
-- 'contactId', 'listContactEvaluations_contactId' - The identifier of the contact in this instance of Amazon Connect.
newListContactEvaluations ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactId'
  Prelude.Text ->
  ListContactEvaluations
newListContactEvaluations pInstanceId_ pContactId_ =
  ListContactEvaluations'
    { nextToken =
        Prelude.Nothing,
      instanceId = pInstanceId_,
      contactId = pContactId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- This is not expected to be set because the value returned in the
-- previous response is always null.
listContactEvaluations_nextToken :: Lens.Lens' ListContactEvaluations (Prelude.Maybe Prelude.Text)
listContactEvaluations_nextToken = Lens.lens (\ListContactEvaluations' {nextToken} -> nextToken) (\s@ListContactEvaluations' {} a -> s {nextToken = a} :: ListContactEvaluations)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listContactEvaluations_instanceId :: Lens.Lens' ListContactEvaluations Prelude.Text
listContactEvaluations_instanceId = Lens.lens (\ListContactEvaluations' {instanceId} -> instanceId) (\s@ListContactEvaluations' {} a -> s {instanceId = a} :: ListContactEvaluations)

-- | The identifier of the contact in this instance of Amazon Connect.
listContactEvaluations_contactId :: Lens.Lens' ListContactEvaluations Prelude.Text
listContactEvaluations_contactId = Lens.lens (\ListContactEvaluations' {contactId} -> contactId) (\s@ListContactEvaluations' {} a -> s {contactId = a} :: ListContactEvaluations)

instance Core.AWSPager ListContactEvaluations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactEvaluationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listContactEvaluationsResponse_evaluationSummaryList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listContactEvaluations_nextToken
          Lens..~ rs
          Lens.^? listContactEvaluationsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListContactEvaluations where
  type
    AWSResponse ListContactEvaluations =
      ListContactEvaluationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactEvaluationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "EvaluationSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListContactEvaluations where
  hashWithSalt _salt ListContactEvaluations' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactId

instance Prelude.NFData ListContactEvaluations where
  rnf ListContactEvaluations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactId

instance Data.ToHeaders ListContactEvaluations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListContactEvaluations where
  toPath ListContactEvaluations' {..} =
    Prelude.mconcat
      ["/contact-evaluations/", Data.toBS instanceId]

instance Data.ToQuery ListContactEvaluations where
  toQuery ListContactEvaluations' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "contactId" Data.=: contactId
      ]

-- | /See:/ 'newListContactEvaluationsResponse' smart constructor.
data ListContactEvaluationsResponse = ListContactEvaluationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    --
    -- This is always returned as null in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Provides details about a list of contact evaluations belonging to an
    -- instance.
    evaluationSummaryList :: [EvaluationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactEvaluationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContactEvaluationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as null in the response.
--
-- 'httpStatus', 'listContactEvaluationsResponse_httpStatus' - The response's http status code.
--
-- 'evaluationSummaryList', 'listContactEvaluationsResponse_evaluationSummaryList' - Provides details about a list of contact evaluations belonging to an
-- instance.
newListContactEvaluationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactEvaluationsResponse
newListContactEvaluationsResponse pHttpStatus_ =
  ListContactEvaluationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      evaluationSummaryList = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
--
-- This is always returned as null in the response.
listContactEvaluationsResponse_nextToken :: Lens.Lens' ListContactEvaluationsResponse (Prelude.Maybe Prelude.Text)
listContactEvaluationsResponse_nextToken = Lens.lens (\ListContactEvaluationsResponse' {nextToken} -> nextToken) (\s@ListContactEvaluationsResponse' {} a -> s {nextToken = a} :: ListContactEvaluationsResponse)

-- | The response's http status code.
listContactEvaluationsResponse_httpStatus :: Lens.Lens' ListContactEvaluationsResponse Prelude.Int
listContactEvaluationsResponse_httpStatus = Lens.lens (\ListContactEvaluationsResponse' {httpStatus} -> httpStatus) (\s@ListContactEvaluationsResponse' {} a -> s {httpStatus = a} :: ListContactEvaluationsResponse)

-- | Provides details about a list of contact evaluations belonging to an
-- instance.
listContactEvaluationsResponse_evaluationSummaryList :: Lens.Lens' ListContactEvaluationsResponse [EvaluationSummary]
listContactEvaluationsResponse_evaluationSummaryList = Lens.lens (\ListContactEvaluationsResponse' {evaluationSummaryList} -> evaluationSummaryList) (\s@ListContactEvaluationsResponse' {} a -> s {evaluationSummaryList = a} :: ListContactEvaluationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListContactEvaluationsResponse
  where
  rnf ListContactEvaluationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf evaluationSummaryList
