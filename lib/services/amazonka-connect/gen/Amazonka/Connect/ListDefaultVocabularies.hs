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
-- Module      : Amazonka.Connect.ListDefaultVocabularies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the default vocabularies for the specified Amazon Connect
-- instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListDefaultVocabularies
  ( -- * Creating a Request
    ListDefaultVocabularies (..),
    newListDefaultVocabularies,

    -- * Request Lenses
    listDefaultVocabularies_nextToken,
    listDefaultVocabularies_languageCode,
    listDefaultVocabularies_maxResults,
    listDefaultVocabularies_instanceId,

    -- * Destructuring the Response
    ListDefaultVocabulariesResponse (..),
    newListDefaultVocabulariesResponse,

    -- * Response Lenses
    listDefaultVocabulariesResponse_nextToken,
    listDefaultVocabulariesResponse_httpStatus,
    listDefaultVocabulariesResponse_defaultVocabularyList,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDefaultVocabularies' smart constructor.
data ListDefaultVocabularies = ListDefaultVocabularies'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The language code of the vocabulary entries. For a list of languages and
    -- their corresponding language codes, see
    -- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
    languageCode :: Prelude.Maybe VocabularyLanguageCode,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDefaultVocabularies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDefaultVocabularies_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'languageCode', 'listDefaultVocabularies_languageCode' - The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
--
-- 'maxResults', 'listDefaultVocabularies_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listDefaultVocabularies_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
newListDefaultVocabularies ::
  -- | 'instanceId'
  Prelude.Text ->
  ListDefaultVocabularies
newListDefaultVocabularies pInstanceId_ =
  ListDefaultVocabularies'
    { nextToken =
        Prelude.Nothing,
      languageCode = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listDefaultVocabularies_nextToken :: Lens.Lens' ListDefaultVocabularies (Prelude.Maybe Prelude.Text)
listDefaultVocabularies_nextToken = Lens.lens (\ListDefaultVocabularies' {nextToken} -> nextToken) (\s@ListDefaultVocabularies' {} a -> s {nextToken = a} :: ListDefaultVocabularies)

-- | The language code of the vocabulary entries. For a list of languages and
-- their corresponding language codes, see
-- <https://docs.aws.amazon.com/transcribe/latest/dg/transcribe-whatis.html What is Amazon Transcribe?>
listDefaultVocabularies_languageCode :: Lens.Lens' ListDefaultVocabularies (Prelude.Maybe VocabularyLanguageCode)
listDefaultVocabularies_languageCode = Lens.lens (\ListDefaultVocabularies' {languageCode} -> languageCode) (\s@ListDefaultVocabularies' {} a -> s {languageCode = a} :: ListDefaultVocabularies)

-- | The maximum number of results to return per page.
listDefaultVocabularies_maxResults :: Lens.Lens' ListDefaultVocabularies (Prelude.Maybe Prelude.Natural)
listDefaultVocabularies_maxResults = Lens.lens (\ListDefaultVocabularies' {maxResults} -> maxResults) (\s@ListDefaultVocabularies' {} a -> s {maxResults = a} :: ListDefaultVocabularies)

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
listDefaultVocabularies_instanceId :: Lens.Lens' ListDefaultVocabularies Prelude.Text
listDefaultVocabularies_instanceId = Lens.lens (\ListDefaultVocabularies' {instanceId} -> instanceId) (\s@ListDefaultVocabularies' {} a -> s {instanceId = a} :: ListDefaultVocabularies)

instance Core.AWSPager ListDefaultVocabularies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDefaultVocabulariesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDefaultVocabulariesResponse_defaultVocabularyList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDefaultVocabularies_nextToken
          Lens..~ rs
          Lens.^? listDefaultVocabulariesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDefaultVocabularies where
  type
    AWSResponse ListDefaultVocabularies =
      ListDefaultVocabulariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDefaultVocabulariesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "DefaultVocabularyList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDefaultVocabularies where
  hashWithSalt _salt ListDefaultVocabularies' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListDefaultVocabularies where
  rnf ListDefaultVocabularies' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf instanceId

instance Core.ToHeaders ListDefaultVocabularies where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDefaultVocabularies where
  toJSON ListDefaultVocabularies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LanguageCode" Core..=) Prelude.<$> languageCode,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDefaultVocabularies where
  toPath ListDefaultVocabularies' {..} =
    Prelude.mconcat
      [ "/default-vocabulary-summary/",
        Core.toBS instanceId
      ]

instance Core.ToQuery ListDefaultVocabularies where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDefaultVocabulariesResponse' smart constructor.
data ListDefaultVocabulariesResponse = ListDefaultVocabulariesResponse'
  { -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of default vocabularies.
    defaultVocabularyList :: [DefaultVocabulary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDefaultVocabulariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDefaultVocabulariesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listDefaultVocabulariesResponse_httpStatus' - The response's http status code.
--
-- 'defaultVocabularyList', 'listDefaultVocabulariesResponse_defaultVocabularyList' - A list of default vocabularies.
newListDefaultVocabulariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDefaultVocabulariesResponse
newListDefaultVocabulariesResponse pHttpStatus_ =
  ListDefaultVocabulariesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      defaultVocabularyList = Prelude.mempty
    }

-- | If there are additional results, this is the token for the next set of
-- results.
listDefaultVocabulariesResponse_nextToken :: Lens.Lens' ListDefaultVocabulariesResponse (Prelude.Maybe Prelude.Text)
listDefaultVocabulariesResponse_nextToken = Lens.lens (\ListDefaultVocabulariesResponse' {nextToken} -> nextToken) (\s@ListDefaultVocabulariesResponse' {} a -> s {nextToken = a} :: ListDefaultVocabulariesResponse)

-- | The response's http status code.
listDefaultVocabulariesResponse_httpStatus :: Lens.Lens' ListDefaultVocabulariesResponse Prelude.Int
listDefaultVocabulariesResponse_httpStatus = Lens.lens (\ListDefaultVocabulariesResponse' {httpStatus} -> httpStatus) (\s@ListDefaultVocabulariesResponse' {} a -> s {httpStatus = a} :: ListDefaultVocabulariesResponse)

-- | A list of default vocabularies.
listDefaultVocabulariesResponse_defaultVocabularyList :: Lens.Lens' ListDefaultVocabulariesResponse [DefaultVocabulary]
listDefaultVocabulariesResponse_defaultVocabularyList = Lens.lens (\ListDefaultVocabulariesResponse' {defaultVocabularyList} -> defaultVocabularyList) (\s@ListDefaultVocabulariesResponse' {} a -> s {defaultVocabularyList = a} :: ListDefaultVocabulariesResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDefaultVocabulariesResponse
  where
  rnf ListDefaultVocabulariesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf defaultVocabularyList
