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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If you
-- don\'t enter a value in any of the request parameters, returns the
-- entire list of vocabularies.
module Amazonka.Transcribe.ListMedicalVocabularies
  ( -- * Creating a Request
    ListMedicalVocabularies (..),
    newListMedicalVocabularies,

    -- * Request Lenses
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_stateEquals,
    listMedicalVocabularies_maxResults,

    -- * Destructuring the Response
    ListMedicalVocabulariesResponse (..),
    newListMedicalVocabulariesResponse,

    -- * Response Lenses
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transcribe.Types

-- | /See:/ 'newListMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { -- | Returns vocabularies whose names contain the specified string. The
    -- search is not case sensitive. @ListMedicalVocabularies@ returns both
    -- \"@vocabularyname@\" and \"@VocabularyName@\".
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of your previous request to @ListMedicalVocabularies@ was
    -- truncated, include the @NextToken@ to fetch the next set of
    -- vocabularies.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When specified, returns only vocabularies with the @VocabularyState@
    -- equal to the specified vocabulary state. Use this field to see which
    -- vocabularies are ready for your medical transcription jobs.
    stateEquals :: Prelude.Maybe VocabularyState,
    -- | The maximum number of vocabularies to return in each page of results. If
    -- there are fewer results than the value you specify, only the actual
    -- results are returned. If you do not specify a value, the default of 5 is
    -- used.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nameContains', 'listMedicalVocabularies_nameContains' - Returns vocabularies whose names contain the specified string. The
-- search is not case sensitive. @ListMedicalVocabularies@ returns both
-- \"@vocabularyname@\" and \"@VocabularyName@\".
--
-- 'nextToken', 'listMedicalVocabularies_nextToken' - If the result of your previous request to @ListMedicalVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of
-- vocabularies.
--
-- 'stateEquals', 'listMedicalVocabularies_stateEquals' - When specified, returns only vocabularies with the @VocabularyState@
-- equal to the specified vocabulary state. Use this field to see which
-- vocabularies are ready for your medical transcription jobs.
--
-- 'maxResults', 'listMedicalVocabularies_maxResults' - The maximum number of vocabularies to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
newListMedicalVocabularies ::
  ListMedicalVocabularies
newListMedicalVocabularies =
  ListMedicalVocabularies'
    { nameContains =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      stateEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Returns vocabularies whose names contain the specified string. The
-- search is not case sensitive. @ListMedicalVocabularies@ returns both
-- \"@vocabularyname@\" and \"@VocabularyName@\".
listMedicalVocabularies_nameContains :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Text)
listMedicalVocabularies_nameContains = Lens.lens (\ListMedicalVocabularies' {nameContains} -> nameContains) (\s@ListMedicalVocabularies' {} a -> s {nameContains = a} :: ListMedicalVocabularies)

-- | If the result of your previous request to @ListMedicalVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of
-- vocabularies.
listMedicalVocabularies_nextToken :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Text)
listMedicalVocabularies_nextToken = Lens.lens (\ListMedicalVocabularies' {nextToken} -> nextToken) (\s@ListMedicalVocabularies' {} a -> s {nextToken = a} :: ListMedicalVocabularies)

-- | When specified, returns only vocabularies with the @VocabularyState@
-- equal to the specified vocabulary state. Use this field to see which
-- vocabularies are ready for your medical transcription jobs.
listMedicalVocabularies_stateEquals :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe VocabularyState)
listMedicalVocabularies_stateEquals = Lens.lens (\ListMedicalVocabularies' {stateEquals} -> stateEquals) (\s@ListMedicalVocabularies' {} a -> s {stateEquals = a} :: ListMedicalVocabularies)

-- | The maximum number of vocabularies to return in each page of results. If
-- there are fewer results than the value you specify, only the actual
-- results are returned. If you do not specify a value, the default of 5 is
-- used.
listMedicalVocabularies_maxResults :: Lens.Lens' ListMedicalVocabularies (Prelude.Maybe Prelude.Natural)
listMedicalVocabularies_maxResults = Lens.lens (\ListMedicalVocabularies' {maxResults} -> maxResults) (\s@ListMedicalVocabularies' {} a -> s {maxResults = a} :: ListMedicalVocabularies)

instance Core.AWSRequest ListMedicalVocabularies where
  type
    AWSResponse ListMedicalVocabularies =
      ListMedicalVocabulariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            Prelude.<$> (x Core..?> "Vocabularies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListMedicalVocabularies where
  hashWithSalt _salt ListMedicalVocabularies' {..} =
    _salt `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` stateEquals
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListMedicalVocabularies where
  rnf ListMedicalVocabularies' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf stateEquals
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListMedicalVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListMedicalVocabularies" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("StateEquals" Core..=) Prelude.<$> stateEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListMedicalVocabularies where
  toPath = Prelude.const "/"

instance Core.ToQuery ListMedicalVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { -- | A list of objects that describe the vocabularies that match your search
    -- criteria.
    vocabularies :: Prelude.Maybe [VocabularyInfo],
    -- | The requested vocabulary state.
    status :: Prelude.Maybe VocabularyState,
    -- | The @ListMedicalVocabularies@ operation returns a page of vocabularies
    -- at a time. You set the maximum number of vocabularies to return on a
    -- page with the @MaxResults@ parameter. If there are more jobs in the list
    -- will fit on a page, Amazon Transcribe Medical returns the @NextPage@
    -- token. To return the next page of vocabularies, include the token in the
    -- next request to the @ListMedicalVocabularies@ operation .
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'vocabularies', 'listMedicalVocabulariesResponse_vocabularies' - A list of objects that describe the vocabularies that match your search
-- criteria.
--
-- 'status', 'listMedicalVocabulariesResponse_status' - The requested vocabulary state.
--
-- 'nextToken', 'listMedicalVocabulariesResponse_nextToken' - The @ListMedicalVocabularies@ operation returns a page of vocabularies
-- at a time. You set the maximum number of vocabularies to return on a
-- page with the @MaxResults@ parameter. If there are more jobs in the list
-- will fit on a page, Amazon Transcribe Medical returns the @NextPage@
-- token. To return the next page of vocabularies, include the token in the
-- next request to the @ListMedicalVocabularies@ operation .
--
-- 'httpStatus', 'listMedicalVocabulariesResponse_httpStatus' - The response's http status code.
newListMedicalVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMedicalVocabulariesResponse
newListMedicalVocabulariesResponse pHttpStatus_ =
  ListMedicalVocabulariesResponse'
    { vocabularies =
        Prelude.Nothing,
      status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects that describe the vocabularies that match your search
-- criteria.
listMedicalVocabulariesResponse_vocabularies :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe [VocabularyInfo])
listMedicalVocabulariesResponse_vocabularies = Lens.lens (\ListMedicalVocabulariesResponse' {vocabularies} -> vocabularies) (\s@ListMedicalVocabulariesResponse' {} a -> s {vocabularies = a} :: ListMedicalVocabulariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The requested vocabulary state.
listMedicalVocabulariesResponse_status :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe VocabularyState)
listMedicalVocabulariesResponse_status = Lens.lens (\ListMedicalVocabulariesResponse' {status} -> status) (\s@ListMedicalVocabulariesResponse' {} a -> s {status = a} :: ListMedicalVocabulariesResponse)

-- | The @ListMedicalVocabularies@ operation returns a page of vocabularies
-- at a time. You set the maximum number of vocabularies to return on a
-- page with the @MaxResults@ parameter. If there are more jobs in the list
-- will fit on a page, Amazon Transcribe Medical returns the @NextPage@
-- token. To return the next page of vocabularies, include the token in the
-- next request to the @ListMedicalVocabularies@ operation .
listMedicalVocabulariesResponse_nextToken :: Lens.Lens' ListMedicalVocabulariesResponse (Prelude.Maybe Prelude.Text)
listMedicalVocabulariesResponse_nextToken = Lens.lens (\ListMedicalVocabulariesResponse' {nextToken} -> nextToken) (\s@ListMedicalVocabulariesResponse' {} a -> s {nextToken = a} :: ListMedicalVocabulariesResponse)

-- | The response's http status code.
listMedicalVocabulariesResponse_httpStatus :: Lens.Lens' ListMedicalVocabulariesResponse Prelude.Int
listMedicalVocabulariesResponse_httpStatus = Lens.lens (\ListMedicalVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListMedicalVocabulariesResponse' {} a -> s {httpStatus = a} :: ListMedicalVocabulariesResponse)

instance
  Prelude.NFData
    ListMedicalVocabulariesResponse
  where
  rnf ListMedicalVocabulariesResponse' {..} =
    Prelude.rnf vocabularies
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
