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
-- Module      : Amazonka.SSMIncidents.ListRelatedItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all related items for an incident record.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListRelatedItems
  ( -- * Creating a Request
    ListRelatedItems (..),
    newListRelatedItems,

    -- * Request Lenses
    listRelatedItems_maxResults,
    listRelatedItems_nextToken,
    listRelatedItems_incidentRecordArn,

    -- * Destructuring the Response
    ListRelatedItemsResponse (..),
    newListRelatedItemsResponse,

    -- * Response Lenses
    listRelatedItemsResponse_nextToken,
    listRelatedItemsResponse_httpStatus,
    listRelatedItemsResponse_relatedItems,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListRelatedItems' smart constructor.
data ListRelatedItems = ListRelatedItems'
  { -- | The maximum number of related items per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident record containing the
    -- listed related items.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRelatedItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRelatedItems_maxResults' - The maximum number of related items per page.
--
-- 'nextToken', 'listRelatedItems_nextToken' - The pagination token to continue to the next page of results.
--
-- 'incidentRecordArn', 'listRelatedItems_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident record containing the
-- listed related items.
newListRelatedItems ::
  -- | 'incidentRecordArn'
  Prelude.Text ->
  ListRelatedItems
newListRelatedItems pIncidentRecordArn_ =
  ListRelatedItems'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      incidentRecordArn = pIncidentRecordArn_
    }

-- | The maximum number of related items per page.
listRelatedItems_maxResults :: Lens.Lens' ListRelatedItems (Prelude.Maybe Prelude.Natural)
listRelatedItems_maxResults = Lens.lens (\ListRelatedItems' {maxResults} -> maxResults) (\s@ListRelatedItems' {} a -> s {maxResults = a} :: ListRelatedItems)

-- | The pagination token to continue to the next page of results.
listRelatedItems_nextToken :: Lens.Lens' ListRelatedItems (Prelude.Maybe Prelude.Text)
listRelatedItems_nextToken = Lens.lens (\ListRelatedItems' {nextToken} -> nextToken) (\s@ListRelatedItems' {} a -> s {nextToken = a} :: ListRelatedItems)

-- | The Amazon Resource Name (ARN) of the incident record containing the
-- listed related items.
listRelatedItems_incidentRecordArn :: Lens.Lens' ListRelatedItems Prelude.Text
listRelatedItems_incidentRecordArn = Lens.lens (\ListRelatedItems' {incidentRecordArn} -> incidentRecordArn) (\s@ListRelatedItems' {} a -> s {incidentRecordArn = a} :: ListRelatedItems)

instance Core.AWSPager ListRelatedItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRelatedItemsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listRelatedItemsResponse_relatedItems) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRelatedItems_nextToken
          Lens..~ rs
          Lens.^? listRelatedItemsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRelatedItems where
  type
    AWSResponse ListRelatedItems =
      ListRelatedItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRelatedItemsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "relatedItems" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListRelatedItems where
  hashWithSalt _salt ListRelatedItems' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` incidentRecordArn

instance Prelude.NFData ListRelatedItems where
  rnf ListRelatedItems' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf incidentRecordArn

instance Data.ToHeaders ListRelatedItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRelatedItems where
  toJSON ListRelatedItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("incidentRecordArn" Data..= incidentRecordArn)
          ]
      )

instance Data.ToPath ListRelatedItems where
  toPath = Prelude.const "/listRelatedItems"

instance Data.ToQuery ListRelatedItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRelatedItemsResponse' smart constructor.
data ListRelatedItemsResponse = ListRelatedItemsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details about each related item.
    relatedItems :: [RelatedItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRelatedItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRelatedItemsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listRelatedItemsResponse_httpStatus' - The response's http status code.
--
-- 'relatedItems', 'listRelatedItemsResponse_relatedItems' - Details about each related item.
newListRelatedItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRelatedItemsResponse
newListRelatedItemsResponse pHttpStatus_ =
  ListRelatedItemsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      relatedItems = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listRelatedItemsResponse_nextToken :: Lens.Lens' ListRelatedItemsResponse (Prelude.Maybe Prelude.Text)
listRelatedItemsResponse_nextToken = Lens.lens (\ListRelatedItemsResponse' {nextToken} -> nextToken) (\s@ListRelatedItemsResponse' {} a -> s {nextToken = a} :: ListRelatedItemsResponse)

-- | The response's http status code.
listRelatedItemsResponse_httpStatus :: Lens.Lens' ListRelatedItemsResponse Prelude.Int
listRelatedItemsResponse_httpStatus = Lens.lens (\ListRelatedItemsResponse' {httpStatus} -> httpStatus) (\s@ListRelatedItemsResponse' {} a -> s {httpStatus = a} :: ListRelatedItemsResponse)

-- | Details about each related item.
listRelatedItemsResponse_relatedItems :: Lens.Lens' ListRelatedItemsResponse [RelatedItem]
listRelatedItemsResponse_relatedItems = Lens.lens (\ListRelatedItemsResponse' {relatedItems} -> relatedItems) (\s@ListRelatedItemsResponse' {} a -> s {relatedItems = a} :: ListRelatedItemsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListRelatedItemsResponse where
  rnf ListRelatedItemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf relatedItems
