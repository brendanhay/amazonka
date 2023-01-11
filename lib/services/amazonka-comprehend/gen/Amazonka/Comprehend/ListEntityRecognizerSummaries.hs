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
-- Module      : Amazonka.Comprehend.ListEntityRecognizerSummaries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of summaries for the entity recognizers that you have
-- created.
module Amazonka.Comprehend.ListEntityRecognizerSummaries
  ( -- * Creating a Request
    ListEntityRecognizerSummaries (..),
    newListEntityRecognizerSummaries,

    -- * Request Lenses
    listEntityRecognizerSummaries_maxResults,
    listEntityRecognizerSummaries_nextToken,

    -- * Destructuring the Response
    ListEntityRecognizerSummariesResponse (..),
    newListEntityRecognizerSummariesResponse,

    -- * Response Lenses
    listEntityRecognizerSummariesResponse_entityRecognizerSummariesList,
    listEntityRecognizerSummariesResponse_nextToken,
    listEntityRecognizerSummariesResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEntityRecognizerSummaries' smart constructor.
data ListEntityRecognizerSummaries = ListEntityRecognizerSummaries'
  { -- | The maximum number of results to return on each page. The default is
    -- 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityRecognizerSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEntityRecognizerSummaries_maxResults' - The maximum number of results to return on each page. The default is
-- 100.
--
-- 'nextToken', 'listEntityRecognizerSummaries_nextToken' - Identifies the next page of results to return.
newListEntityRecognizerSummaries ::
  ListEntityRecognizerSummaries
newListEntityRecognizerSummaries =
  ListEntityRecognizerSummaries'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return on each page. The default is
-- 100.
listEntityRecognizerSummaries_maxResults :: Lens.Lens' ListEntityRecognizerSummaries (Prelude.Maybe Prelude.Natural)
listEntityRecognizerSummaries_maxResults = Lens.lens (\ListEntityRecognizerSummaries' {maxResults} -> maxResults) (\s@ListEntityRecognizerSummaries' {} a -> s {maxResults = a} :: ListEntityRecognizerSummaries)

-- | Identifies the next page of results to return.
listEntityRecognizerSummaries_nextToken :: Lens.Lens' ListEntityRecognizerSummaries (Prelude.Maybe Prelude.Text)
listEntityRecognizerSummaries_nextToken = Lens.lens (\ListEntityRecognizerSummaries' {nextToken} -> nextToken) (\s@ListEntityRecognizerSummaries' {} a -> s {nextToken = a} :: ListEntityRecognizerSummaries)

instance
  Core.AWSRequest
    ListEntityRecognizerSummaries
  where
  type
    AWSResponse ListEntityRecognizerSummaries =
      ListEntityRecognizerSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEntityRecognizerSummariesResponse'
            Prelude.<$> ( x Data..?> "EntityRecognizerSummariesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListEntityRecognizerSummaries
  where
  hashWithSalt _salt ListEntityRecognizerSummaries' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListEntityRecognizerSummaries where
  rnf ListEntityRecognizerSummaries' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListEntityRecognizerSummaries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListEntityRecognizerSummaries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListEntityRecognizerSummaries where
  toJSON ListEntityRecognizerSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListEntityRecognizerSummaries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListEntityRecognizerSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEntityRecognizerSummariesResponse' smart constructor.
data ListEntityRecognizerSummariesResponse = ListEntityRecognizerSummariesResponse'
  { -- | The list entity recognizer summaries.
    entityRecognizerSummariesList :: Prelude.Maybe [EntityRecognizerSummary],
    -- | The list entity recognizer summaries.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEntityRecognizerSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityRecognizerSummariesList', 'listEntityRecognizerSummariesResponse_entityRecognizerSummariesList' - The list entity recognizer summaries.
--
-- 'nextToken', 'listEntityRecognizerSummariesResponse_nextToken' - The list entity recognizer summaries.
--
-- 'httpStatus', 'listEntityRecognizerSummariesResponse_httpStatus' - The response's http status code.
newListEntityRecognizerSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEntityRecognizerSummariesResponse
newListEntityRecognizerSummariesResponse pHttpStatus_ =
  ListEntityRecognizerSummariesResponse'
    { entityRecognizerSummariesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list entity recognizer summaries.
listEntityRecognizerSummariesResponse_entityRecognizerSummariesList :: Lens.Lens' ListEntityRecognizerSummariesResponse (Prelude.Maybe [EntityRecognizerSummary])
listEntityRecognizerSummariesResponse_entityRecognizerSummariesList = Lens.lens (\ListEntityRecognizerSummariesResponse' {entityRecognizerSummariesList} -> entityRecognizerSummariesList) (\s@ListEntityRecognizerSummariesResponse' {} a -> s {entityRecognizerSummariesList = a} :: ListEntityRecognizerSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list entity recognizer summaries.
listEntityRecognizerSummariesResponse_nextToken :: Lens.Lens' ListEntityRecognizerSummariesResponse (Prelude.Maybe Prelude.Text)
listEntityRecognizerSummariesResponse_nextToken = Lens.lens (\ListEntityRecognizerSummariesResponse' {nextToken} -> nextToken) (\s@ListEntityRecognizerSummariesResponse' {} a -> s {nextToken = a} :: ListEntityRecognizerSummariesResponse)

-- | The response's http status code.
listEntityRecognizerSummariesResponse_httpStatus :: Lens.Lens' ListEntityRecognizerSummariesResponse Prelude.Int
listEntityRecognizerSummariesResponse_httpStatus = Lens.lens (\ListEntityRecognizerSummariesResponse' {httpStatus} -> httpStatus) (\s@ListEntityRecognizerSummariesResponse' {} a -> s {httpStatus = a} :: ListEntityRecognizerSummariesResponse)

instance
  Prelude.NFData
    ListEntityRecognizerSummariesResponse
  where
  rnf ListEntityRecognizerSummariesResponse' {..} =
    Prelude.rnf entityRecognizerSummariesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
