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
-- Module      : Amazonka.Transcribe.ListVocabularies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides a list of custom vocabularies that match the specified
-- criteria. If no criteria are specified, all custom vocabularies are
-- returned.
--
-- To get detailed information about a specific custom vocabulary, use the
-- operation.
module Amazonka.Transcribe.ListVocabularies
  ( -- * Creating a Request
    ListVocabularies (..),
    newListVocabularies,

    -- * Request Lenses
    listVocabularies_nextToken,
    listVocabularies_nameContains,
    listVocabularies_maxResults,
    listVocabularies_stateEquals,

    -- * Destructuring the Response
    ListVocabulariesResponse (..),
    newListVocabulariesResponse,

    -- * Response Lenses
    listVocabulariesResponse_nextToken,
    listVocabulariesResponse_status,
    listVocabulariesResponse_vocabularies,
    listVocabulariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListVocabularies' smart constructor.
data ListVocabularies = ListVocabularies'
  { -- | If your @ListVocabularies@ request returns more results than can be
    -- displayed, @NextToken@ is displayed in the response with an associated
    -- string. To get the next page of results, copy this string and repeat
    -- your request, including @NextToken@ with the value of the copied string.
    -- Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only the custom vocabularies that contain the specified string.
    -- The search is not case sensitive.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of custom vocabularies to return in each page of
    -- results. If there are fewer results than the value you specify, only the
    -- actual results are returned. If you don\'t specify a value, a default of
    -- 5 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Returns only custom vocabularies with the specified state. Vocabularies
    -- are ordered by creation date, with the newest vocabulary first. If you
    -- don\'t include @StateEquals@, all custom medical vocabularies are
    -- returned.
    stateEquals :: Prelude.Maybe VocabularyState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVocabularies_nextToken' - If your @ListVocabularies@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
--
-- 'nameContains', 'listVocabularies_nameContains' - Returns only the custom vocabularies that contain the specified string.
-- The search is not case sensitive.
--
-- 'maxResults', 'listVocabularies_maxResults' - The maximum number of custom vocabularies to return in each page of
-- results. If there are fewer results than the value you specify, only the
-- actual results are returned. If you don\'t specify a value, a default of
-- 5 is used.
--
-- 'stateEquals', 'listVocabularies_stateEquals' - Returns only custom vocabularies with the specified state. Vocabularies
-- are ordered by creation date, with the newest vocabulary first. If you
-- don\'t include @StateEquals@, all custom medical vocabularies are
-- returned.
newListVocabularies ::
  ListVocabularies
newListVocabularies =
  ListVocabularies'
    { nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      stateEquals = Prelude.Nothing
    }

-- | If your @ListVocabularies@ request returns more results than can be
-- displayed, @NextToken@ is displayed in the response with an associated
-- string. To get the next page of results, copy this string and repeat
-- your request, including @NextToken@ with the value of the copied string.
-- Repeat as needed to view all your results.
listVocabularies_nextToken :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Text)
listVocabularies_nextToken = Lens.lens (\ListVocabularies' {nextToken} -> nextToken) (\s@ListVocabularies' {} a -> s {nextToken = a} :: ListVocabularies)

-- | Returns only the custom vocabularies that contain the specified string.
-- The search is not case sensitive.
listVocabularies_nameContains :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Text)
listVocabularies_nameContains = Lens.lens (\ListVocabularies' {nameContains} -> nameContains) (\s@ListVocabularies' {} a -> s {nameContains = a} :: ListVocabularies)

-- | The maximum number of custom vocabularies to return in each page of
-- results. If there are fewer results than the value you specify, only the
-- actual results are returned. If you don\'t specify a value, a default of
-- 5 is used.
listVocabularies_maxResults :: Lens.Lens' ListVocabularies (Prelude.Maybe Prelude.Natural)
listVocabularies_maxResults = Lens.lens (\ListVocabularies' {maxResults} -> maxResults) (\s@ListVocabularies' {} a -> s {maxResults = a} :: ListVocabularies)

-- | Returns only custom vocabularies with the specified state. Vocabularies
-- are ordered by creation date, with the newest vocabulary first. If you
-- don\'t include @StateEquals@, all custom medical vocabularies are
-- returned.
listVocabularies_stateEquals :: Lens.Lens' ListVocabularies (Prelude.Maybe VocabularyState)
listVocabularies_stateEquals = Lens.lens (\ListVocabularies' {stateEquals} -> stateEquals) (\s@ListVocabularies' {} a -> s {stateEquals = a} :: ListVocabularies)

instance Core.AWSRequest ListVocabularies where
  type
    AWSResponse ListVocabularies =
      ListVocabulariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVocabulariesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Vocabularies" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVocabularies where
  hashWithSalt _salt ListVocabularies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` stateEquals

instance Prelude.NFData ListVocabularies where
  rnf ListVocabularies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf stateEquals

instance Core.ToHeaders ListVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListVocabularies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListVocabularies where
  toJSON ListVocabularies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StateEquals" Core..=) Prelude.<$> stateEquals
          ]
      )

instance Core.ToPath ListVocabularies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListVocabulariesResponse' smart constructor.
data ListVocabulariesResponse = ListVocabulariesResponse'
  { -- | If @NextToken@ is present in your response, it indicates that not all
    -- results are displayed. To view the next set of results, copy the string
    -- associated with the @NextToken@ parameter in your results output, then
    -- run your request again including @NextToken@ with the value of the
    -- copied string. Repeat as needed to view all your results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists all custom vocabularies that have the status specified in your
    -- request. Vocabularies are ordered by creation date, with the newest
    -- vocabulary first.
    status :: Prelude.Maybe VocabularyState,
    -- | Provides information about the custom vocabularies that match the
    -- criteria specified in your request.
    vocabularies :: Prelude.Maybe [VocabularyInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVocabulariesResponse_nextToken' - If @NextToken@ is present in your response, it indicates that not all
-- results are displayed. To view the next set of results, copy the string
-- associated with the @NextToken@ parameter in your results output, then
-- run your request again including @NextToken@ with the value of the
-- copied string. Repeat as needed to view all your results.
--
-- 'status', 'listVocabulariesResponse_status' - Lists all custom vocabularies that have the status specified in your
-- request. Vocabularies are ordered by creation date, with the newest
-- vocabulary first.
--
-- 'vocabularies', 'listVocabulariesResponse_vocabularies' - Provides information about the custom vocabularies that match the
-- criteria specified in your request.
--
-- 'httpStatus', 'listVocabulariesResponse_httpStatus' - The response's http status code.
newListVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVocabulariesResponse
newListVocabulariesResponse pHttpStatus_ =
  ListVocabulariesResponse'
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
listVocabulariesResponse_nextToken :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe Prelude.Text)
listVocabulariesResponse_nextToken = Lens.lens (\ListVocabulariesResponse' {nextToken} -> nextToken) (\s@ListVocabulariesResponse' {} a -> s {nextToken = a} :: ListVocabulariesResponse)

-- | Lists all custom vocabularies that have the status specified in your
-- request. Vocabularies are ordered by creation date, with the newest
-- vocabulary first.
listVocabulariesResponse_status :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe VocabularyState)
listVocabulariesResponse_status = Lens.lens (\ListVocabulariesResponse' {status} -> status) (\s@ListVocabulariesResponse' {} a -> s {status = a} :: ListVocabulariesResponse)

-- | Provides information about the custom vocabularies that match the
-- criteria specified in your request.
listVocabulariesResponse_vocabularies :: Lens.Lens' ListVocabulariesResponse (Prelude.Maybe [VocabularyInfo])
listVocabulariesResponse_vocabularies = Lens.lens (\ListVocabulariesResponse' {vocabularies} -> vocabularies) (\s@ListVocabulariesResponse' {} a -> s {vocabularies = a} :: ListVocabulariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVocabulariesResponse_httpStatus :: Lens.Lens' ListVocabulariesResponse Prelude.Int
listVocabulariesResponse_httpStatus = Lens.lens (\ListVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListVocabulariesResponse' {} a -> s {httpStatus = a} :: ListVocabulariesResponse)

instance Prelude.NFData ListVocabulariesResponse where
  rnf ListVocabulariesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vocabularies
      `Prelude.seq` Prelude.rnf httpStatus
