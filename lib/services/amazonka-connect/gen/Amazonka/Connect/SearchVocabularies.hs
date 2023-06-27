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
-- Module      : Amazonka.Connect.SearchVocabularies
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for vocabularies within a specific Amazon Connect instance
-- using @State@, @NameStartsWith@, and @LanguageCode@.
--
-- This operation returns paginated results.
module Amazonka.Connect.SearchVocabularies
  ( -- * Creating a Request
    SearchVocabularies (..),
    newSearchVocabularies,

    -- * Request Lenses
    searchVocabularies_languageCode,
    searchVocabularies_maxResults,
    searchVocabularies_nameStartsWith,
    searchVocabularies_nextToken,
    searchVocabularies_state,
    searchVocabularies_instanceId,

    -- * Destructuring the Response
    SearchVocabulariesResponse (..),
    newSearchVocabulariesResponse,

    -- * Response Lenses
    searchVocabulariesResponse_nextToken,
    searchVocabulariesResponse_vocabularySummaryList,
    searchVocabulariesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchVocabularies' smart constructor.
data SearchVocabularies = SearchVocabularies'
  { -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: Prelude.Maybe VocabularyLanguageCode,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The starting pattern of the name of the vocabulary.
    nameStartsWith :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The current state of the custom vocabulary.
    state :: Prelude.Maybe VocabularyState,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'searchVocabularies_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'maxResults', 'searchVocabularies_maxResults' - The maximum number of results to return per page.
--
-- 'nameStartsWith', 'searchVocabularies_nameStartsWith' - The starting pattern of the name of the vocabulary.
--
-- 'nextToken', 'searchVocabularies_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'state', 'searchVocabularies_state' - The current state of the custom vocabulary.
--
-- 'instanceId', 'searchVocabularies_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newSearchVocabularies ::
  -- | 'instanceId'
  Prelude.Text ->
  SearchVocabularies
newSearchVocabularies pInstanceId_ =
  SearchVocabularies'
    { languageCode = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameStartsWith = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      state = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
searchVocabularies_languageCode :: Lens.Lens' SearchVocabularies (Prelude.Maybe VocabularyLanguageCode)
searchVocabularies_languageCode = Lens.lens (\SearchVocabularies' {languageCode} -> languageCode) (\s@SearchVocabularies' {} a -> s {languageCode = a} :: SearchVocabularies)

-- | The maximum number of results to return per page.
searchVocabularies_maxResults :: Lens.Lens' SearchVocabularies (Prelude.Maybe Prelude.Natural)
searchVocabularies_maxResults = Lens.lens (\SearchVocabularies' {maxResults} -> maxResults) (\s@SearchVocabularies' {} a -> s {maxResults = a} :: SearchVocabularies)

-- | The starting pattern of the name of the vocabulary.
searchVocabularies_nameStartsWith :: Lens.Lens' SearchVocabularies (Prelude.Maybe Prelude.Text)
searchVocabularies_nameStartsWith = Lens.lens (\SearchVocabularies' {nameStartsWith} -> nameStartsWith) (\s@SearchVocabularies' {} a -> s {nameStartsWith = a} :: SearchVocabularies)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
searchVocabularies_nextToken :: Lens.Lens' SearchVocabularies (Prelude.Maybe Prelude.Text)
searchVocabularies_nextToken = Lens.lens (\SearchVocabularies' {nextToken} -> nextToken) (\s@SearchVocabularies' {} a -> s {nextToken = a} :: SearchVocabularies)

-- | The current state of the custom vocabulary.
searchVocabularies_state :: Lens.Lens' SearchVocabularies (Prelude.Maybe VocabularyState)
searchVocabularies_state = Lens.lens (\SearchVocabularies' {state} -> state) (\s@SearchVocabularies' {} a -> s {state = a} :: SearchVocabularies)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
searchVocabularies_instanceId :: Lens.Lens' SearchVocabularies Prelude.Text
searchVocabularies_instanceId = Lens.lens (\SearchVocabularies' {instanceId} -> instanceId) (\s@SearchVocabularies' {} a -> s {instanceId = a} :: SearchVocabularies)

instance Core.AWSPager SearchVocabularies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchVocabulariesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchVocabulariesResponse_vocabularySummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& searchVocabularies_nextToken
          Lens..~ rs
          Lens.^? searchVocabulariesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest SearchVocabularies where
  type
    AWSResponse SearchVocabularies =
      SearchVocabulariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchVocabulariesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "VocabularySummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchVocabularies where
  hashWithSalt _salt SearchVocabularies' {..} =
    _salt
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameStartsWith
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData SearchVocabularies where
  rnf SearchVocabularies' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameStartsWith
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders SearchVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchVocabularies where
  toJSON SearchVocabularies' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameStartsWith" Data..=)
              Prelude.<$> nameStartsWith,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("State" Data..=) Prelude.<$> state
          ]
      )

instance Data.ToPath SearchVocabularies where
  toPath SearchVocabularies' {..} =
    Prelude.mconcat
      ["/vocabulary-summary/", Data.toBS instanceId]

instance Data.ToQuery SearchVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchVocabulariesResponse' smart constructor.
data SearchVocabulariesResponse = SearchVocabulariesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of the available custom vocabularies.
    vocabularySummaryList :: Prelude.Maybe [VocabularySummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchVocabulariesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'vocabularySummaryList', 'searchVocabulariesResponse_vocabularySummaryList' - The list of the available custom vocabularies.
--
-- 'httpStatus', 'searchVocabulariesResponse_httpStatus' - The response's http status code.
newSearchVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchVocabulariesResponse
newSearchVocabulariesResponse pHttpStatus_ =
  SearchVocabulariesResponse'
    { nextToken =
        Prelude.Nothing,
      vocabularySummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If there are additional results, this is the token for the next set of
-- results.
searchVocabulariesResponse_nextToken :: Lens.Lens' SearchVocabulariesResponse (Prelude.Maybe Prelude.Text)
searchVocabulariesResponse_nextToken = Lens.lens (\SearchVocabulariesResponse' {nextToken} -> nextToken) (\s@SearchVocabulariesResponse' {} a -> s {nextToken = a} :: SearchVocabulariesResponse)

-- | The list of the available custom vocabularies.
searchVocabulariesResponse_vocabularySummaryList :: Lens.Lens' SearchVocabulariesResponse (Prelude.Maybe [VocabularySummary])
searchVocabulariesResponse_vocabularySummaryList = Lens.lens (\SearchVocabulariesResponse' {vocabularySummaryList} -> vocabularySummaryList) (\s@SearchVocabulariesResponse' {} a -> s {vocabularySummaryList = a} :: SearchVocabulariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchVocabulariesResponse_httpStatus :: Lens.Lens' SearchVocabulariesResponse Prelude.Int
searchVocabulariesResponse_httpStatus = Lens.lens (\SearchVocabulariesResponse' {httpStatus} -> httpStatus) (\s@SearchVocabulariesResponse' {} a -> s {httpStatus = a} :: SearchVocabulariesResponse)

instance Prelude.NFData SearchVocabulariesResponse where
  rnf SearchVocabulariesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vocabularySummaryList
      `Prelude.seq` Prelude.rnf httpStatus
