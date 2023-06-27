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
-- Module      : Amazonka.Glue.ListCrawlers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all crawler resources in this Amazon Web Services
-- account, or the resources with the specified tag. This operation allows
-- you to see which resources are available in your account, and their
-- names.
--
-- This operation takes the optional @Tags@ field, which you can use as a
-- filter on the response so that tagged resources can be retrieved as a
-- group. If you choose to use tags filtering, only resources with the tag
-- are retrieved.
module Amazonka.Glue.ListCrawlers
  ( -- * Creating a Request
    ListCrawlers (..),
    newListCrawlers,

    -- * Request Lenses
    listCrawlers_maxResults,
    listCrawlers_nextToken,
    listCrawlers_tags,

    -- * Destructuring the Response
    ListCrawlersResponse (..),
    newListCrawlersResponse,

    -- * Response Lenses
    listCrawlersResponse_crawlerNames,
    listCrawlersResponse_nextToken,
    listCrawlersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCrawlers' smart constructor.
data ListCrawlers = ListCrawlers'
  { -- | The maximum size of a list to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifies to return only these tagged resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrawlers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCrawlers_maxResults' - The maximum size of a list to return.
--
-- 'nextToken', 'listCrawlers_nextToken' - A continuation token, if this is a continuation request.
--
-- 'tags', 'listCrawlers_tags' - Specifies to return only these tagged resources.
newListCrawlers ::
  ListCrawlers
newListCrawlers =
  ListCrawlers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The maximum size of a list to return.
listCrawlers_maxResults :: Lens.Lens' ListCrawlers (Prelude.Maybe Prelude.Natural)
listCrawlers_maxResults = Lens.lens (\ListCrawlers' {maxResults} -> maxResults) (\s@ListCrawlers' {} a -> s {maxResults = a} :: ListCrawlers)

-- | A continuation token, if this is a continuation request.
listCrawlers_nextToken :: Lens.Lens' ListCrawlers (Prelude.Maybe Prelude.Text)
listCrawlers_nextToken = Lens.lens (\ListCrawlers' {nextToken} -> nextToken) (\s@ListCrawlers' {} a -> s {nextToken = a} :: ListCrawlers)

-- | Specifies to return only these tagged resources.
listCrawlers_tags :: Lens.Lens' ListCrawlers (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listCrawlers_tags = Lens.lens (\ListCrawlers' {tags} -> tags) (\s@ListCrawlers' {} a -> s {tags = a} :: ListCrawlers) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListCrawlers where
  type AWSResponse ListCrawlers = ListCrawlersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCrawlersResponse'
            Prelude.<$> (x Data..?> "CrawlerNames" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCrawlers where
  hashWithSalt _salt ListCrawlers' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ListCrawlers where
  rnf ListCrawlers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders ListCrawlers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.ListCrawlers" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCrawlers where
  toJSON ListCrawlers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath ListCrawlers where
  toPath = Prelude.const "/"

instance Data.ToQuery ListCrawlers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCrawlersResponse' smart constructor.
data ListCrawlersResponse = ListCrawlersResponse'
  { -- | The names of all crawlers in the account, or the crawlers with the
    -- specified tags.
    crawlerNames :: Prelude.Maybe [Prelude.Text],
    -- | A continuation token, if the returned list does not contain the last
    -- metric available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCrawlersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crawlerNames', 'listCrawlersResponse_crawlerNames' - The names of all crawlers in the account, or the crawlers with the
-- specified tags.
--
-- 'nextToken', 'listCrawlersResponse_nextToken' - A continuation token, if the returned list does not contain the last
-- metric available.
--
-- 'httpStatus', 'listCrawlersResponse_httpStatus' - The response's http status code.
newListCrawlersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCrawlersResponse
newListCrawlersResponse pHttpStatus_ =
  ListCrawlersResponse'
    { crawlerNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The names of all crawlers in the account, or the crawlers with the
-- specified tags.
listCrawlersResponse_crawlerNames :: Lens.Lens' ListCrawlersResponse (Prelude.Maybe [Prelude.Text])
listCrawlersResponse_crawlerNames = Lens.lens (\ListCrawlersResponse' {crawlerNames} -> crawlerNames) (\s@ListCrawlersResponse' {} a -> s {crawlerNames = a} :: ListCrawlersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the returned list does not contain the last
-- metric available.
listCrawlersResponse_nextToken :: Lens.Lens' ListCrawlersResponse (Prelude.Maybe Prelude.Text)
listCrawlersResponse_nextToken = Lens.lens (\ListCrawlersResponse' {nextToken} -> nextToken) (\s@ListCrawlersResponse' {} a -> s {nextToken = a} :: ListCrawlersResponse)

-- | The response's http status code.
listCrawlersResponse_httpStatus :: Lens.Lens' ListCrawlersResponse Prelude.Int
listCrawlersResponse_httpStatus = Lens.lens (\ListCrawlersResponse' {httpStatus} -> httpStatus) (\s@ListCrawlersResponse' {} a -> s {httpStatus = a} :: ListCrawlersResponse)

instance Prelude.NFData ListCrawlersResponse where
  rnf ListCrawlersResponse' {..} =
    Prelude.rnf crawlerNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
