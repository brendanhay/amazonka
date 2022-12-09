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
-- Module      : Amazonka.Wisdom.ListAssistantAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about assistant associations.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.ListAssistantAssociations
  ( -- * Creating a Request
    ListAssistantAssociations (..),
    newListAssistantAssociations,

    -- * Request Lenses
    listAssistantAssociations_maxResults,
    listAssistantAssociations_nextToken,
    listAssistantAssociations_assistantId,

    -- * Destructuring the Response
    ListAssistantAssociationsResponse (..),
    newListAssistantAssociationsResponse,

    -- * Response Lenses
    listAssistantAssociationsResponse_nextToken,
    listAssistantAssociationsResponse_httpStatus,
    listAssistantAssociationsResponse_assistantAssociationSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newListAssistantAssociations' smart constructor.
data ListAssistantAssociations = ListAssistantAssociations'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
    -- URLs cannot contain the ARN.
    assistantId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssistantAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssistantAssociations_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listAssistantAssociations_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'assistantId', 'listAssistantAssociations_assistantId' - The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
newListAssistantAssociations ::
  -- | 'assistantId'
  Prelude.Text ->
  ListAssistantAssociations
newListAssistantAssociations pAssistantId_ =
  ListAssistantAssociations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      assistantId = pAssistantId_
    }

-- | The maximum number of results to return per page.
listAssistantAssociations_maxResults :: Lens.Lens' ListAssistantAssociations (Prelude.Maybe Prelude.Natural)
listAssistantAssociations_maxResults = Lens.lens (\ListAssistantAssociations' {maxResults} -> maxResults) (\s@ListAssistantAssociations' {} a -> s {maxResults = a} :: ListAssistantAssociations)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listAssistantAssociations_nextToken :: Lens.Lens' ListAssistantAssociations (Prelude.Maybe Prelude.Text)
listAssistantAssociations_nextToken = Lens.lens (\ListAssistantAssociations' {nextToken} -> nextToken) (\s@ListAssistantAssociations' {} a -> s {nextToken = a} :: ListAssistantAssociations)

-- | The identifier of the Wisdom assistant. Can be either the ID or the ARN.
-- URLs cannot contain the ARN.
listAssistantAssociations_assistantId :: Lens.Lens' ListAssistantAssociations Prelude.Text
listAssistantAssociations_assistantId = Lens.lens (\ListAssistantAssociations' {assistantId} -> assistantId) (\s@ListAssistantAssociations' {} a -> s {assistantId = a} :: ListAssistantAssociations)

instance Core.AWSPager ListAssistantAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssistantAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssistantAssociationsResponse_assistantAssociationSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAssistantAssociations_nextToken
          Lens..~ rs
          Lens.^? listAssistantAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAssistantAssociations where
  type
    AWSResponse ListAssistantAssociations =
      ListAssistantAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssistantAssociationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "assistantAssociationSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssistantAssociations where
  hashWithSalt _salt ListAssistantAssociations' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` assistantId

instance Prelude.NFData ListAssistantAssociations where
  rnf ListAssistantAssociations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf assistantId

instance Data.ToHeaders ListAssistantAssociations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssistantAssociations where
  toPath ListAssistantAssociations' {..} =
    Prelude.mconcat
      [ "/assistants/",
        Data.toBS assistantId,
        "/associations"
      ]

instance Data.ToQuery ListAssistantAssociations where
  toQuery ListAssistantAssociations' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAssistantAssociationsResponse' smart constructor.
data ListAssistantAssociationsResponse = ListAssistantAssociationsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary information about assistant associations.
    assistantAssociationSummaries :: [AssistantAssociationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssistantAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssistantAssociationsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listAssistantAssociationsResponse_httpStatus' - The response's http status code.
--
-- 'assistantAssociationSummaries', 'listAssistantAssociationsResponse_assistantAssociationSummaries' - Summary information about assistant associations.
newListAssistantAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssistantAssociationsResponse
newListAssistantAssociationsResponse pHttpStatus_ =
  ListAssistantAssociationsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assistantAssociationSummaries =
        Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listAssistantAssociationsResponse_nextToken :: Lens.Lens' ListAssistantAssociationsResponse (Prelude.Maybe Prelude.Text)
listAssistantAssociationsResponse_nextToken = Lens.lens (\ListAssistantAssociationsResponse' {nextToken} -> nextToken) (\s@ListAssistantAssociationsResponse' {} a -> s {nextToken = a} :: ListAssistantAssociationsResponse)

-- | The response's http status code.
listAssistantAssociationsResponse_httpStatus :: Lens.Lens' ListAssistantAssociationsResponse Prelude.Int
listAssistantAssociationsResponse_httpStatus = Lens.lens (\ListAssistantAssociationsResponse' {httpStatus} -> httpStatus) (\s@ListAssistantAssociationsResponse' {} a -> s {httpStatus = a} :: ListAssistantAssociationsResponse)

-- | Summary information about assistant associations.
listAssistantAssociationsResponse_assistantAssociationSummaries :: Lens.Lens' ListAssistantAssociationsResponse [AssistantAssociationSummary]
listAssistantAssociationsResponse_assistantAssociationSummaries = Lens.lens (\ListAssistantAssociationsResponse' {assistantAssociationSummaries} -> assistantAssociationSummaries) (\s@ListAssistantAssociationsResponse' {} a -> s {assistantAssociationSummaries = a} :: ListAssistantAssociationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListAssistantAssociationsResponse
  where
  rnf ListAssistantAssociationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf assistantAssociationSummaries
