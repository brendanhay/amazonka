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
-- Module      : Network.AWS.Transcribe.ListMedicalVocabularies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of vocabularies that match the specified criteria. If you
-- don\'t enter a value in any of the request parameters, returns the
-- entire list of vocabularies.
module Network.AWS.Transcribe.ListMedicalVocabularies
  ( -- * Creating a Request
    ListMedicalVocabularies (..),
    newListMedicalVocabularies,

    -- * Request Lenses
    listMedicalVocabularies_nextToken,
    listMedicalVocabularies_nameContains,
    listMedicalVocabularies_maxResults,
    listMedicalVocabularies_stateEquals,

    -- * Destructuring the Response
    ListMedicalVocabulariesResponse (..),
    newListMedicalVocabulariesResponse,

    -- * Response Lenses
    listMedicalVocabulariesResponse_status,
    listMedicalVocabulariesResponse_nextToken,
    listMedicalVocabulariesResponse_vocabularies,
    listMedicalVocabulariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transcribe.Types

-- | /See:/ 'newListMedicalVocabularies' smart constructor.
data ListMedicalVocabularies = ListMedicalVocabularies'
  { -- | If the result of your previous request to @ListMedicalVocabularies@ was
    -- truncated, include the @NextToken@ to fetch the next set of
    -- vocabularies.
    nextToken :: Core.Maybe Core.Text,
    -- | Returns vocabularies whose names contain the specified string. The
    -- search is not case sensitive. @ListMedicalVocabularies@ returns both
    -- \"@vocabularyname@\" and \"@VocabularyName@\".
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of vocabularies to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | When specified, returns only vocabularies with the @VocabularyState@
    -- equal to the specified vocabulary state. Use this field to see which
    -- vocabularies are ready for your medical transcription jobs.
    stateEquals :: Core.Maybe VocabularyState
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMedicalVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listMedicalVocabularies_nextToken' - If the result of your previous request to @ListMedicalVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of
-- vocabularies.
--
-- 'nameContains', 'listMedicalVocabularies_nameContains' - Returns vocabularies whose names contain the specified string. The
-- search is not case sensitive. @ListMedicalVocabularies@ returns both
-- \"@vocabularyname@\" and \"@VocabularyName@\".
--
-- 'maxResults', 'listMedicalVocabularies_maxResults' - The maximum number of vocabularies to return in the response.
--
-- 'stateEquals', 'listMedicalVocabularies_stateEquals' - When specified, returns only vocabularies with the @VocabularyState@
-- equal to the specified vocabulary state. Use this field to see which
-- vocabularies are ready for your medical transcription jobs.
newListMedicalVocabularies ::
  ListMedicalVocabularies
newListMedicalVocabularies =
  ListMedicalVocabularies'
    { nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      stateEquals = Core.Nothing
    }

-- | If the result of your previous request to @ListMedicalVocabularies@ was
-- truncated, include the @NextToken@ to fetch the next set of
-- vocabularies.
listMedicalVocabularies_nextToken :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Core.Text)
listMedicalVocabularies_nextToken = Lens.lens (\ListMedicalVocabularies' {nextToken} -> nextToken) (\s@ListMedicalVocabularies' {} a -> s {nextToken = a} :: ListMedicalVocabularies)

-- | Returns vocabularies whose names contain the specified string. The
-- search is not case sensitive. @ListMedicalVocabularies@ returns both
-- \"@vocabularyname@\" and \"@VocabularyName@\".
listMedicalVocabularies_nameContains :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Core.Text)
listMedicalVocabularies_nameContains = Lens.lens (\ListMedicalVocabularies' {nameContains} -> nameContains) (\s@ListMedicalVocabularies' {} a -> s {nameContains = a} :: ListMedicalVocabularies)

-- | The maximum number of vocabularies to return in the response.
listMedicalVocabularies_maxResults :: Lens.Lens' ListMedicalVocabularies (Core.Maybe Core.Natural)
listMedicalVocabularies_maxResults = Lens.lens (\ListMedicalVocabularies' {maxResults} -> maxResults) (\s@ListMedicalVocabularies' {} a -> s {maxResults = a} :: ListMedicalVocabularies)

-- | When specified, returns only vocabularies with the @VocabularyState@
-- equal to the specified vocabulary state. Use this field to see which
-- vocabularies are ready for your medical transcription jobs.
listMedicalVocabularies_stateEquals :: Lens.Lens' ListMedicalVocabularies (Core.Maybe VocabularyState)
listMedicalVocabularies_stateEquals = Lens.lens (\ListMedicalVocabularies' {stateEquals} -> stateEquals) (\s@ListMedicalVocabularies' {} a -> s {stateEquals = a} :: ListMedicalVocabularies)

instance Core.AWSRequest ListMedicalVocabularies where
  type
    AWSResponse ListMedicalVocabularies =
      ListMedicalVocabulariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMedicalVocabulariesResponse'
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Vocabularies" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListMedicalVocabularies

instance Core.NFData ListMedicalVocabularies

instance Core.ToHeaders ListMedicalVocabularies where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Transcribe.ListMedicalVocabularies" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListMedicalVocabularies where
  toJSON ListMedicalVocabularies' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("StateEquals" Core..=) Core.<$> stateEquals
          ]
      )

instance Core.ToPath ListMedicalVocabularies where
  toPath = Core.const "/"

instance Core.ToQuery ListMedicalVocabularies where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListMedicalVocabulariesResponse' smart constructor.
data ListMedicalVocabulariesResponse = ListMedicalVocabulariesResponse'
  { -- | The requested vocabulary state.
    status :: Core.Maybe VocabularyState,
    -- | The @ListMedicalVocabularies@ operation returns a page of vocabularies
    -- at a time. You set the maximum number of vocabularies to return on a
    -- page with the @MaxResults@ parameter. If there are more jobs in the list
    -- will fit on a page, Amazon Transcribe Medical returns the @NextPage@
    -- token. To return the next page of vocabularies, include the token in the
    -- next request to the @ListMedicalVocabularies@ operation .
    nextToken :: Core.Maybe Core.Text,
    -- | A list of objects that describe the vocabularies that match your search
    -- criteria.
    vocabularies :: Core.Maybe [VocabularyInfo],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListMedicalVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'vocabularies', 'listMedicalVocabulariesResponse_vocabularies' - A list of objects that describe the vocabularies that match your search
-- criteria.
--
-- 'httpStatus', 'listMedicalVocabulariesResponse_httpStatus' - The response's http status code.
newListMedicalVocabulariesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListMedicalVocabulariesResponse
newListMedicalVocabulariesResponse pHttpStatus_ =
  ListMedicalVocabulariesResponse'
    { status =
        Core.Nothing,
      nextToken = Core.Nothing,
      vocabularies = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested vocabulary state.
listMedicalVocabulariesResponse_status :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe VocabularyState)
listMedicalVocabulariesResponse_status = Lens.lens (\ListMedicalVocabulariesResponse' {status} -> status) (\s@ListMedicalVocabulariesResponse' {} a -> s {status = a} :: ListMedicalVocabulariesResponse)

-- | The @ListMedicalVocabularies@ operation returns a page of vocabularies
-- at a time. You set the maximum number of vocabularies to return on a
-- page with the @MaxResults@ parameter. If there are more jobs in the list
-- will fit on a page, Amazon Transcribe Medical returns the @NextPage@
-- token. To return the next page of vocabularies, include the token in the
-- next request to the @ListMedicalVocabularies@ operation .
listMedicalVocabulariesResponse_nextToken :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe Core.Text)
listMedicalVocabulariesResponse_nextToken = Lens.lens (\ListMedicalVocabulariesResponse' {nextToken} -> nextToken) (\s@ListMedicalVocabulariesResponse' {} a -> s {nextToken = a} :: ListMedicalVocabulariesResponse)

-- | A list of objects that describe the vocabularies that match your search
-- criteria.
listMedicalVocabulariesResponse_vocabularies :: Lens.Lens' ListMedicalVocabulariesResponse (Core.Maybe [VocabularyInfo])
listMedicalVocabulariesResponse_vocabularies = Lens.lens (\ListMedicalVocabulariesResponse' {vocabularies} -> vocabularies) (\s@ListMedicalVocabulariesResponse' {} a -> s {vocabularies = a} :: ListMedicalVocabulariesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listMedicalVocabulariesResponse_httpStatus :: Lens.Lens' ListMedicalVocabulariesResponse Core.Int
listMedicalVocabulariesResponse_httpStatus = Lens.lens (\ListMedicalVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListMedicalVocabulariesResponse' {} a -> s {httpStatus = a} :: ListMedicalVocabulariesResponse)

instance Core.NFData ListMedicalVocabulariesResponse
