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
-- Module      : Amazonka.Transcribe.ListMedicalVocabularies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom medical vocabularies that match the specified
-- criteria. If no criteria are specified, all custom medical vocabularies
-- are returned.
--
-- To get detailed information about a specific custom medical vocabulary,
-- use the operation.
module Amazonka.Transcribe.ListMedicalVocabularies
  ( -- * Creating a Request
    ListMedicalVocabularies (..),
    newListMedicalVocabularies,

    -- * Request Lenses
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_stateEquals,

    -- * Destructuring the Response
    ListMedicalVocabulariesResponse (..),
    newListMedicalVocabulariesResponse,

    -- * Response Lenses
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { -- | The maximum number of custom medical vocabularies to return in each page
    -- of results. If there are fewer results than the value that you specify,
    -- only the actual results are returned. If you don\'t specify a value, a
    -- default of 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns only the custom medical vocabularies that contain the specified
    -- string. The search is not case sensitive.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If your @ListMedicalVocabularies@ request returns more results than can
    -- be displayed, @NextToken@ is displayed in the response with an
    -- associated string. To get the next page of results, copy this string and
    -- repeat your request, including @NextToken@ with the value of the copied
    -- string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only custom medical vocabularies with the specified state.
    -- Custom vocabularies are ordered by creation date, with the newest
    -- vocabulary first. If you don\'t include @StateEquals@, all custom
    -- medical vocabularies are returned.
    stateEquals :: Prelude.Maybe VocabularyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMedicalVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMedicalVocabularies_maxResults' - The maximum number of custom medical vocabularies to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
--
-- 'nameContains', 'listMedicalVocabularies_nameContains' - Returns only the custom medical vocabularies that contain the specified
-- string. The search is not case sensitive.
--
-- 'nextToken', 'listMedicalVocabularies_nextToken' - If your @ListMedicalVocabularies@ request returns more results than can
-- be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
--
-- 'stateEquals', 'listMedicalVocabularies_stateEquals' - Returns only custom medical vocabularies with the specified state.
-- Custom vocabularies are ordered by creation date, with the newest
-- vocabulary first. If you don\'t include @StateEquals@, all custom
-- medical vocabularies are returned.
newListMedicalVocabularies ::
  ListMedicalVocabularies
newListMedicalVocabularies =
  ListMedicalVocabularies'
    { maxResults =
        Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateEquals = Prelude.Nothing
    }

-- | The maximum number of custom medical vocabularies to return in each page
-- of results. If there are fewer results than the value that you specify,
-- only the actual results are returned. If you don\'t specify a value, a
-- default of 5 is used.
listMedicalVocabularies_maxResults :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Natural)
listMedicalVocabularies_maxResults = Lens.lens (\ListMedicalVocabularies' {maxResults} -> maxResults) (\s@ListMedicalVocabularies' {} a -> s {maxResults = a} :: ListMedicalVocabularies)

-- | Returns only the custom medical vocabularies that contain the specified
-- string. The search is not case sensitive.
listMedicalVocabularies_nameContains :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Text)
listMedicalVocabularies_nameContains = Lens.lens (\ListMedicalVocabularies' {nameContains} -> nameContains) (\s@ListMedicalVocabularies' {} a -> s {nameContains = a} :: ListMedicalVocabularies)

-- | If your @ListMedicalVocabularies@ request returns more results than can
-- be displayed, @NextToken@ is displayed in the response with an
-- associated string. To get the next page of results, copy this string and
-- repeat your request, including @NextToken@ with the value of the copied
-- string. Repeat as needed to view all your results.
listMedicalVocabularies_nextToken :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Text)
listMedicalVocabularies_nextToken = Lens.lens (\ListMedicalVocabularies' {nextToken} -> nextToken) (\s@ListMedicalVocabularies' {} a -> s {nextToken = a} :: ListMedicalVocabularies)

-- | Returns only custom medical vocabularies with the specified state.
-- Custom vocabularies are ordered by creation date, with the newest
-- vocabulary first. If you don\'t include @StateEquals@, all custom
-- medical vocabularies are returned.
listMedicalVocabularies_stateEquals :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe VocabularyState)
listMedicalVocabularies_stateEquals = Lens.lens (\ListMedicalVocabularies' {stateEquals} -> stateEquals) (\s@ListMedicalVocabularies' {} a -> s {stateEquals = a} :: ListMedicalVocabularies)

instance Core.AWSRequest ListMedicalVocabularies where
  type
    AWSResponse ListMedicalVocabularies =
      ListMedicalVocabulariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Vocabularies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMedicalVocabularies where
  hashWithSalt _salt ListMedicalVocabularies' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateEquals

instance Prelude.NFData ListMedicalVocabularies where
  rnf ListMedicalVocabularies' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateEquals

instance Data.ToHeaders ListMedicalVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Transcribe.ListMedicalVocabularies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("StateEquals" Data..=) Prelude.<$> stateEquals
          ]
      )

instance Data.ToPath ListMedicalVocabularies where
  toPath = Prelude.const "/"

instance Data.ToQuery ListMedicalVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all custom medical vocabularies that have the status specified in
    -- your request. Custom vocabularies are ordered by creation date, with the
    -- newest vocabulary first.
    status :: Prelude.Maybe VocabularyState,
    -- | Provides information about the custom medical vocabularies that match
    -- the criteria specified in your request.
    vocabularies :: Prelude.Maybe [VocabularyInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMedicalVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMedicalVocabulariesResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'status', 'listMedicalVocabulariesResponse_status' - Lists all custom medical vocabularies that have the status specified in
-- your request. Custom vocabularies are ordered by creation date, with the
-- newest vocabulary first.
--
-- 'vocabularies', 'listMedicalVocabulariesResponse_vocabularies' - Provides information about the custom medical vocabularies that match
-- the criteria specified in your request.
--
-- 'httpStatus', 'listMedicalVocabulariesResponse_httpStatus' - The response's http status code.
newListMedicalVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMedicalVocabulariesResponse
newListMedicalVocabulariesResponse pHttpStatus_ =
  ListMedicalVocabulariesResponse'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      vocabularies = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
listMedicalVocabulariesResponse_nextToken :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe Prelude.Text)
listMedicalVocabulariesResponse_nextToken = Lens.lens (\ListMedicalVocabulariesResponse' {nextToken} -> nextToken) (\s@ListMedicalVocabulariesResponse' {} a -> s {nextToken = a} :: ListMedicalVocabulariesResponse)

-- | Lists all custom medical vocabularies that have the status specified in
-- your request. Custom vocabularies are ordered by creation date, with the
-- newest vocabulary first.
listMedicalVocabulariesResponse_status :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe VocabularyState)
listMedicalVocabulariesResponse_status = Lens.lens (\ListMedicalVocabulariesResponse' {status} -> status) (\s@ListMedicalVocabulariesResponse' {} a -> s {status = a} :: ListMedicalVocabulariesResponse)

-- | Provides information about the custom medical vocabularies that match
-- the criteria specified in your request.
listMedicalVocabulariesResponse_vocabularies :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe [VocabularyInfo])
listMedicalVocabulariesResponse_vocabularies = Lens.lens (\ListMedicalVocabulariesResponse' {vocabularies} -> vocabularies) (\s@ListMedicalVocabulariesResponse' {} a -> s {vocabularies = a} :: ListMedicalVocabulariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listMedicalVocabulariesResponse_httpStatus :: Lens.Lens' ListMedicalVocabulariesResponse Prelude.Int
listMedicalVocabulariesResponse_httpStatus = Lens.lens (\ListMedicalVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListMedicalVocabulariesResponse' {} a -> s {httpStatus = a} :: ListMedicalVocabulariesResponse)

instance
  Prelude.NFData
    ListMedicalVocabulariesResponse
  where
  rnf ListMedicalVocabulariesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vocabularies
      `Prelude.seq` Prelude.rnf httpStatus
