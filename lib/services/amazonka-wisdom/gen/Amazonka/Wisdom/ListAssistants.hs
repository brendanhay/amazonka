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
-- Module      : Amazonka.Wisdom.ListAssistants
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about assistants.
--
-- This operation returns paginated results.
module Amazonka.Wisdom.ListAssistants
  ( -- * Creating a Request
    ListAssistants (..),
    newListAssistants,

    -- * Request Lenses
    listAssistants_maxResults,
    listAssistants_nextToken,

    -- * Destructuring the Response
    ListAssistantsResponse (..),
    newListAssistantsResponse,

    -- * Response Lenses
    listAssistantsResponse_nextToken,
    listAssistantsResponse_httpStatus,
    listAssistantsResponse_assistantSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Wisdom.Types

-- | /See:/ 'newListAssistants' smart constructor.
data ListAssistants = ListAssistants'
  { -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssistants' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAssistants_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listAssistants_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
newListAssistants ::
  ListAssistants
newListAssistants =
  ListAssistants'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return per page.
listAssistants_maxResults :: Lens.Lens' ListAssistants (Prelude.Maybe Prelude.Natural)
listAssistants_maxResults = Lens.lens (\ListAssistants' {maxResults} -> maxResults) (\s@ListAssistants' {} a -> s {maxResults = a} :: ListAssistants)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listAssistants_nextToken :: Lens.Lens' ListAssistants (Prelude.Maybe Prelude.Text)
listAssistants_nextToken = Lens.lens (\ListAssistants' {nextToken} -> nextToken) (\s@ListAssistants' {} a -> s {nextToken = a} :: ListAssistants)

instance Core.AWSPager ListAssistants where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAssistantsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAssistantsResponse_assistantSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAssistants_nextToken
              Lens..~ rs
              Lens.^? listAssistantsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAssistants where
  type
    AWSResponse ListAssistants =
      ListAssistantsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssistantsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "assistantSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAssistants where
  hashWithSalt _salt ListAssistants' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListAssistants where
  rnf ListAssistants' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListAssistants where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListAssistants where
  toPath = Prelude.const "/assistants"

instance Data.ToQuery ListAssistants where
  toQuery ListAssistants' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListAssistantsResponse' smart constructor.
data ListAssistantsResponse = ListAssistantsResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the assistants.
    assistantSummaries :: [AssistantSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssistantsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssistantsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listAssistantsResponse_httpStatus' - The response's http status code.
--
-- 'assistantSummaries', 'listAssistantsResponse_assistantSummaries' - Information about the assistants.
newListAssistantsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssistantsResponse
newListAssistantsResponse pHttpStatus_ =
  ListAssistantsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      assistantSummaries = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listAssistantsResponse_nextToken :: Lens.Lens' ListAssistantsResponse (Prelude.Maybe Prelude.Text)
listAssistantsResponse_nextToken = Lens.lens (\ListAssistantsResponse' {nextToken} -> nextToken) (\s@ListAssistantsResponse' {} a -> s {nextToken = a} :: ListAssistantsResponse)

-- | The response's http status code.
listAssistantsResponse_httpStatus :: Lens.Lens' ListAssistantsResponse Prelude.Int
listAssistantsResponse_httpStatus = Lens.lens (\ListAssistantsResponse' {httpStatus} -> httpStatus) (\s@ListAssistantsResponse' {} a -> s {httpStatus = a} :: ListAssistantsResponse)

-- | Information about the assistants.
listAssistantsResponse_assistantSummaries :: Lens.Lens' ListAssistantsResponse [AssistantSummary]
listAssistantsResponse_assistantSummaries = Lens.lens (\ListAssistantsResponse' {assistantSummaries} -> assistantSummaries) (\s@ListAssistantsResponse' {} a -> s {assistantSummaries = a} :: ListAssistantsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAssistantsResponse where
  rnf ListAssistantsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf assistantSummaries
