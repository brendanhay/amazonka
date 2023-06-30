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
-- Module      : Amazonka.OAM.ListLinks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation in a source account to return a list of links to
-- monitoring account sinks that this source account has.
--
-- To find a list of links for one monitoring account sink, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListAttachedLinks.html ListAttachedLinks>
-- from within the monitoring account.
--
-- This operation returns paginated results.
module Amazonka.OAM.ListLinks
  ( -- * Creating a Request
    ListLinks (..),
    newListLinks,

    -- * Request Lenses
    listLinks_maxResults,
    listLinks_nextToken,

    -- * Destructuring the Response
    ListLinksResponse (..),
    newListLinksResponse,

    -- * Response Lenses
    listLinksResponse_nextToken,
    listLinksResponse_httpStatus,
    listLinksResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLinks' smart constructor.
data ListLinks = ListLinks'
  { -- | Limits the number of returned links to the specified number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. You received this token
    -- from a previous call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listLinks_maxResults' - Limits the number of returned links to the specified number.
--
-- 'nextToken', 'listLinks_nextToken' - The token for the next set of items to return. You received this token
-- from a previous call.
newListLinks ::
  ListLinks
newListLinks =
  ListLinks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Limits the number of returned links to the specified number.
listLinks_maxResults :: Lens.Lens' ListLinks (Prelude.Maybe Prelude.Natural)
listLinks_maxResults = Lens.lens (\ListLinks' {maxResults} -> maxResults) (\s@ListLinks' {} a -> s {maxResults = a} :: ListLinks)

-- | The token for the next set of items to return. You received this token
-- from a previous call.
listLinks_nextToken :: Lens.Lens' ListLinks (Prelude.Maybe Prelude.Text)
listLinks_nextToken = Lens.lens (\ListLinks' {nextToken} -> nextToken) (\s@ListLinks' {} a -> s {nextToken = a} :: ListLinks)

instance Core.AWSPager ListLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLinksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listLinksResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLinks_nextToken
          Lens..~ rs
          Lens.^? listLinksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListLinks where
  type AWSResponse ListLinks = ListLinksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLinksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListLinks where
  hashWithSalt _salt ListLinks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLinks where
  rnf ListLinks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLinks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLinks where
  toJSON ListLinks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLinks where
  toPath = Prelude.const "/ListLinks"

instance Data.ToQuery ListLinks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLinksResponse' smart constructor.
data ListLinksResponse = ListLinksResponse'
  { -- | The token to use when requesting the next set of links.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of structures that contain the information about the returned
    -- links.
    items :: [ListLinksItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLinksResponse_nextToken' - The token to use when requesting the next set of links.
--
-- 'httpStatus', 'listLinksResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listLinksResponse_items' - An array of structures that contain the information about the returned
-- links.
newListLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLinksResponse
newListLinksResponse pHttpStatus_ =
  ListLinksResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | The token to use when requesting the next set of links.
listLinksResponse_nextToken :: Lens.Lens' ListLinksResponse (Prelude.Maybe Prelude.Text)
listLinksResponse_nextToken = Lens.lens (\ListLinksResponse' {nextToken} -> nextToken) (\s@ListLinksResponse' {} a -> s {nextToken = a} :: ListLinksResponse)

-- | The response's http status code.
listLinksResponse_httpStatus :: Lens.Lens' ListLinksResponse Prelude.Int
listLinksResponse_httpStatus = Lens.lens (\ListLinksResponse' {httpStatus} -> httpStatus) (\s@ListLinksResponse' {} a -> s {httpStatus = a} :: ListLinksResponse)

-- | An array of structures that contain the information about the returned
-- links.
listLinksResponse_items :: Lens.Lens' ListLinksResponse [ListLinksItem]
listLinksResponse_items = Lens.lens (\ListLinksResponse' {items} -> items) (\s@ListLinksResponse' {} a -> s {items = a} :: ListLinksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListLinksResponse where
  rnf ListLinksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
