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
-- Module      : Amazonka.AuditManager.ListKeywordsForDataSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of keywords that are pre-mapped to the specified control
-- data source.
module Amazonka.AuditManager.ListKeywordsForDataSource
  ( -- * Creating a Request
    ListKeywordsForDataSource (..),
    newListKeywordsForDataSource,

    -- * Request Lenses
    listKeywordsForDataSource_maxResults,
    listKeywordsForDataSource_nextToken,
    listKeywordsForDataSource_source,

    -- * Destructuring the Response
    ListKeywordsForDataSourceResponse (..),
    newListKeywordsForDataSourceResponse,

    -- * Response Lenses
    listKeywordsForDataSourceResponse_keywords,
    listKeywordsForDataSourceResponse_nextToken,
    listKeywordsForDataSourceResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListKeywordsForDataSource' smart constructor.
data ListKeywordsForDataSource = ListKeywordsForDataSource'
  { -- | Represents the maximum number of results on a page or for an API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The control mapping data source that the keywords apply to.
    source :: SourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeywordsForDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listKeywordsForDataSource_maxResults' - Represents the maximum number of results on a page or for an API request
-- call.
--
-- 'nextToken', 'listKeywordsForDataSource_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'source', 'listKeywordsForDataSource_source' - The control mapping data source that the keywords apply to.
newListKeywordsForDataSource ::
  -- | 'source'
  SourceType ->
  ListKeywordsForDataSource
newListKeywordsForDataSource pSource_ =
  ListKeywordsForDataSource'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      source = pSource_
    }

-- | Represents the maximum number of results on a page or for an API request
-- call.
listKeywordsForDataSource_maxResults :: Lens.Lens' ListKeywordsForDataSource (Prelude.Maybe Prelude.Natural)
listKeywordsForDataSource_maxResults = Lens.lens (\ListKeywordsForDataSource' {maxResults} -> maxResults) (\s@ListKeywordsForDataSource' {} a -> s {maxResults = a} :: ListKeywordsForDataSource)

-- | The pagination token that\'s used to fetch the next set of results.
listKeywordsForDataSource_nextToken :: Lens.Lens' ListKeywordsForDataSource (Prelude.Maybe Prelude.Text)
listKeywordsForDataSource_nextToken = Lens.lens (\ListKeywordsForDataSource' {nextToken} -> nextToken) (\s@ListKeywordsForDataSource' {} a -> s {nextToken = a} :: ListKeywordsForDataSource)

-- | The control mapping data source that the keywords apply to.
listKeywordsForDataSource_source :: Lens.Lens' ListKeywordsForDataSource SourceType
listKeywordsForDataSource_source = Lens.lens (\ListKeywordsForDataSource' {source} -> source) (\s@ListKeywordsForDataSource' {} a -> s {source = a} :: ListKeywordsForDataSource)

instance Core.AWSRequest ListKeywordsForDataSource where
  type
    AWSResponse ListKeywordsForDataSource =
      ListKeywordsForDataSourceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListKeywordsForDataSourceResponse'
            Prelude.<$> (x Data..?> "keywords" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListKeywordsForDataSource where
  hashWithSalt _salt ListKeywordsForDataSource' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` source

instance Prelude.NFData ListKeywordsForDataSource where
  rnf ListKeywordsForDataSource' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf source

instance Data.ToHeaders ListKeywordsForDataSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListKeywordsForDataSource where
  toPath = Prelude.const "/dataSourceKeywords"

instance Data.ToQuery ListKeywordsForDataSource where
  toQuery ListKeywordsForDataSource' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "source" Data.=: source
      ]

-- | /See:/ 'newListKeywordsForDataSourceResponse' smart constructor.
data ListKeywordsForDataSourceResponse = ListKeywordsForDataSourceResponse'
  { -- | The list of keywords for the event mapping source.
    keywords :: Prelude.Maybe [Prelude.Text],
    -- | The pagination token that\'s used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListKeywordsForDataSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keywords', 'listKeywordsForDataSourceResponse_keywords' - The list of keywords for the event mapping source.
--
-- 'nextToken', 'listKeywordsForDataSourceResponse_nextToken' - The pagination token that\'s used to fetch the next set of results.
--
-- 'httpStatus', 'listKeywordsForDataSourceResponse_httpStatus' - The response's http status code.
newListKeywordsForDataSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListKeywordsForDataSourceResponse
newListKeywordsForDataSourceResponse pHttpStatus_ =
  ListKeywordsForDataSourceResponse'
    { keywords =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of keywords for the event mapping source.
listKeywordsForDataSourceResponse_keywords :: Lens.Lens' ListKeywordsForDataSourceResponse (Prelude.Maybe [Prelude.Text])
listKeywordsForDataSourceResponse_keywords = Lens.lens (\ListKeywordsForDataSourceResponse' {keywords} -> keywords) (\s@ListKeywordsForDataSourceResponse' {} a -> s {keywords = a} :: ListKeywordsForDataSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token that\'s used to fetch the next set of results.
listKeywordsForDataSourceResponse_nextToken :: Lens.Lens' ListKeywordsForDataSourceResponse (Prelude.Maybe Prelude.Text)
listKeywordsForDataSourceResponse_nextToken = Lens.lens (\ListKeywordsForDataSourceResponse' {nextToken} -> nextToken) (\s@ListKeywordsForDataSourceResponse' {} a -> s {nextToken = a} :: ListKeywordsForDataSourceResponse)

-- | The response's http status code.
listKeywordsForDataSourceResponse_httpStatus :: Lens.Lens' ListKeywordsForDataSourceResponse Prelude.Int
listKeywordsForDataSourceResponse_httpStatus = Lens.lens (\ListKeywordsForDataSourceResponse' {httpStatus} -> httpStatus) (\s@ListKeywordsForDataSourceResponse' {} a -> s {httpStatus = a} :: ListKeywordsForDataSourceResponse)

instance
  Prelude.NFData
    ListKeywordsForDataSourceResponse
  where
  rnf ListKeywordsForDataSourceResponse' {..} =
    Prelude.rnf keywords
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
