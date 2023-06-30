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
-- Module      : Amazonka.OAM.ListAttachedLinks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of source account links that are linked to this
-- monitoring account sink.
--
-- To use this operation, provide the sink ARN. To retrieve a list of sink
-- ARNs, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListSinks.html ListSinks>.
--
-- To find a list of links for one source account, use
-- <https://docs.aws.amazon.com/OAM/latest/APIReference/API_ListLinks.html ListLinks>.
--
-- This operation returns paginated results.
module Amazonka.OAM.ListAttachedLinks
  ( -- * Creating a Request
    ListAttachedLinks (..),
    newListAttachedLinks,

    -- * Request Lenses
    listAttachedLinks_maxResults,
    listAttachedLinks_nextToken,
    listAttachedLinks_sinkIdentifier,

    -- * Destructuring the Response
    ListAttachedLinksResponse (..),
    newListAttachedLinksResponse,

    -- * Response Lenses
    listAttachedLinksResponse_nextToken,
    listAttachedLinksResponse_httpStatus,
    listAttachedLinksResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachedLinks' smart constructor.
data ListAttachedLinks = ListAttachedLinks'
  { -- | Limits the number of returned links to the specified number.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. You received this token
    -- from a previous call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the sink that you want to retrieve links for.
    sinkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedLinks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAttachedLinks_maxResults' - Limits the number of returned links to the specified number.
--
-- 'nextToken', 'listAttachedLinks_nextToken' - The token for the next set of items to return. You received this token
-- from a previous call.
--
-- 'sinkIdentifier', 'listAttachedLinks_sinkIdentifier' - The ARN of the sink that you want to retrieve links for.
newListAttachedLinks ::
  -- | 'sinkIdentifier'
  Prelude.Text ->
  ListAttachedLinks
newListAttachedLinks pSinkIdentifier_ =
  ListAttachedLinks'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sinkIdentifier = pSinkIdentifier_
    }

-- | Limits the number of returned links to the specified number.
listAttachedLinks_maxResults :: Lens.Lens' ListAttachedLinks (Prelude.Maybe Prelude.Natural)
listAttachedLinks_maxResults = Lens.lens (\ListAttachedLinks' {maxResults} -> maxResults) (\s@ListAttachedLinks' {} a -> s {maxResults = a} :: ListAttachedLinks)

-- | The token for the next set of items to return. You received this token
-- from a previous call.
listAttachedLinks_nextToken :: Lens.Lens' ListAttachedLinks (Prelude.Maybe Prelude.Text)
listAttachedLinks_nextToken = Lens.lens (\ListAttachedLinks' {nextToken} -> nextToken) (\s@ListAttachedLinks' {} a -> s {nextToken = a} :: ListAttachedLinks)

-- | The ARN of the sink that you want to retrieve links for.
listAttachedLinks_sinkIdentifier :: Lens.Lens' ListAttachedLinks Prelude.Text
listAttachedLinks_sinkIdentifier = Lens.lens (\ListAttachedLinks' {sinkIdentifier} -> sinkIdentifier) (\s@ListAttachedLinks' {} a -> s {sinkIdentifier = a} :: ListAttachedLinks)

instance Core.AWSPager ListAttachedLinks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedLinksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listAttachedLinksResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listAttachedLinks_nextToken
          Lens..~ rs
          Lens.^? listAttachedLinksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListAttachedLinks where
  type
    AWSResponse ListAttachedLinks =
      ListAttachedLinksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedLinksResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListAttachedLinks where
  hashWithSalt _salt ListAttachedLinks' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sinkIdentifier

instance Prelude.NFData ListAttachedLinks where
  rnf ListAttachedLinks' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sinkIdentifier

instance Data.ToHeaders ListAttachedLinks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAttachedLinks where
  toJSON ListAttachedLinks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("SinkIdentifier" Data..= sinkIdentifier)
          ]
      )

instance Data.ToPath ListAttachedLinks where
  toPath = Prelude.const "/ListAttachedLinks"

instance Data.ToQuery ListAttachedLinks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAttachedLinksResponse' smart constructor.
data ListAttachedLinksResponse = ListAttachedLinksResponse'
  { -- | The token to use when requesting the next set of links.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of structures that contain the information about the attached
    -- links.
    items :: [ListAttachedLinksItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedLinksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAttachedLinksResponse_nextToken' - The token to use when requesting the next set of links.
--
-- 'httpStatus', 'listAttachedLinksResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listAttachedLinksResponse_items' - An array of structures that contain the information about the attached
-- links.
newListAttachedLinksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachedLinksResponse
newListAttachedLinksResponse pHttpStatus_ =
  ListAttachedLinksResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | The token to use when requesting the next set of links.
listAttachedLinksResponse_nextToken :: Lens.Lens' ListAttachedLinksResponse (Prelude.Maybe Prelude.Text)
listAttachedLinksResponse_nextToken = Lens.lens (\ListAttachedLinksResponse' {nextToken} -> nextToken) (\s@ListAttachedLinksResponse' {} a -> s {nextToken = a} :: ListAttachedLinksResponse)

-- | The response's http status code.
listAttachedLinksResponse_httpStatus :: Lens.Lens' ListAttachedLinksResponse Prelude.Int
listAttachedLinksResponse_httpStatus = Lens.lens (\ListAttachedLinksResponse' {httpStatus} -> httpStatus) (\s@ListAttachedLinksResponse' {} a -> s {httpStatus = a} :: ListAttachedLinksResponse)

-- | An array of structures that contain the information about the attached
-- links.
listAttachedLinksResponse_items :: Lens.Lens' ListAttachedLinksResponse [ListAttachedLinksItem]
listAttachedLinksResponse_items = Lens.lens (\ListAttachedLinksResponse' {items} -> items) (\s@ListAttachedLinksResponse' {} a -> s {items = a} :: ListAttachedLinksResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAttachedLinksResponse where
  rnf ListAttachedLinksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
