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
-- Module      : Amazonka.BillingConductor.ListCustomLineItemVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A paginated call to get a list of all custom line item versions.
--
-- This operation returns paginated results.
module Amazonka.BillingConductor.ListCustomLineItemVersions
  ( -- * Creating a Request
    ListCustomLineItemVersions (..),
    newListCustomLineItemVersions,

    -- * Request Lenses
    listCustomLineItemVersions_nextToken,
    listCustomLineItemVersions_filters,
    listCustomLineItemVersions_maxResults,
    listCustomLineItemVersions_arn,

    -- * Destructuring the Response
    ListCustomLineItemVersionsResponse (..),
    newListCustomLineItemVersionsResponse,

    -- * Response Lenses
    listCustomLineItemVersionsResponse_nextToken,
    listCustomLineItemVersionsResponse_customLineItemVersions,
    listCustomLineItemVersionsResponse_httpStatus,
  )
where

import Amazonka.BillingConductor.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCustomLineItemVersions' smart constructor.
data ListCustomLineItemVersions = ListCustomLineItemVersions'
  { -- | The pagination token that\'s used on subsequent calls to retrieve custom
    -- line item versions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A @ListCustomLineItemVersionsFilter@ that specifies the billing period
    -- range in which the custom line item versions are applied.
    filters :: Prelude.Maybe ListCustomLineItemVersionsFilter,
    -- | The maximum number of custom line item versions to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) for the custom line item.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomLineItemVersions_nextToken' - The pagination token that\'s used on subsequent calls to retrieve custom
-- line item versions.
--
-- 'filters', 'listCustomLineItemVersions_filters' - A @ListCustomLineItemVersionsFilter@ that specifies the billing period
-- range in which the custom line item versions are applied.
--
-- 'maxResults', 'listCustomLineItemVersions_maxResults' - The maximum number of custom line item versions to retrieve.
--
-- 'arn', 'listCustomLineItemVersions_arn' - The Amazon Resource Name (ARN) for the custom line item.
newListCustomLineItemVersions ::
  -- | 'arn'
  Prelude.Text ->
  ListCustomLineItemVersions
newListCustomLineItemVersions pArn_ =
  ListCustomLineItemVersions'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      arn = pArn_
    }

-- | The pagination token that\'s used on subsequent calls to retrieve custom
-- line item versions.
listCustomLineItemVersions_nextToken :: Lens.Lens' ListCustomLineItemVersions (Prelude.Maybe Prelude.Text)
listCustomLineItemVersions_nextToken = Lens.lens (\ListCustomLineItemVersions' {nextToken} -> nextToken) (\s@ListCustomLineItemVersions' {} a -> s {nextToken = a} :: ListCustomLineItemVersions)

-- | A @ListCustomLineItemVersionsFilter@ that specifies the billing period
-- range in which the custom line item versions are applied.
listCustomLineItemVersions_filters :: Lens.Lens' ListCustomLineItemVersions (Prelude.Maybe ListCustomLineItemVersionsFilter)
listCustomLineItemVersions_filters = Lens.lens (\ListCustomLineItemVersions' {filters} -> filters) (\s@ListCustomLineItemVersions' {} a -> s {filters = a} :: ListCustomLineItemVersions)

-- | The maximum number of custom line item versions to retrieve.
listCustomLineItemVersions_maxResults :: Lens.Lens' ListCustomLineItemVersions (Prelude.Maybe Prelude.Natural)
listCustomLineItemVersions_maxResults = Lens.lens (\ListCustomLineItemVersions' {maxResults} -> maxResults) (\s@ListCustomLineItemVersions' {} a -> s {maxResults = a} :: ListCustomLineItemVersions)

-- | The Amazon Resource Name (ARN) for the custom line item.
listCustomLineItemVersions_arn :: Lens.Lens' ListCustomLineItemVersions Prelude.Text
listCustomLineItemVersions_arn = Lens.lens (\ListCustomLineItemVersions' {arn} -> arn) (\s@ListCustomLineItemVersions' {} a -> s {arn = a} :: ListCustomLineItemVersions)

instance Core.AWSPager ListCustomLineItemVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCustomLineItemVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCustomLineItemVersionsResponse_customLineItemVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCustomLineItemVersions_nextToken
          Lens..~ rs
          Lens.^? listCustomLineItemVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCustomLineItemVersions where
  type
    AWSResponse ListCustomLineItemVersions =
      ListCustomLineItemVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCustomLineItemVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "CustomLineItemVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCustomLineItemVersions where
  hashWithSalt _salt ListCustomLineItemVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListCustomLineItemVersions where
  rnf ListCustomLineItemVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf arn

instance Core.ToHeaders ListCustomLineItemVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListCustomLineItemVersions where
  toJSON ListCustomLineItemVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Filters" Core..=) Prelude.<$> filters,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Arn" Core..= arn)
          ]
      )

instance Core.ToPath ListCustomLineItemVersions where
  toPath =
    Prelude.const "/list-custom-line-item-versions"

instance Core.ToQuery ListCustomLineItemVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCustomLineItemVersionsResponse' smart constructor.
data ListCustomLineItemVersionsResponse = ListCustomLineItemVersionsResponse'
  { -- | The pagination token that\'s used on subsequent calls to retrieve custom
    -- line item versions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of @CustomLineItemVersionListElements@ that are received.
    customLineItemVersions :: Prelude.Maybe [CustomLineItemVersionListElement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCustomLineItemVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCustomLineItemVersionsResponse_nextToken' - The pagination token that\'s used on subsequent calls to retrieve custom
-- line item versions.
--
-- 'customLineItemVersions', 'listCustomLineItemVersionsResponse_customLineItemVersions' - A list of @CustomLineItemVersionListElements@ that are received.
--
-- 'httpStatus', 'listCustomLineItemVersionsResponse_httpStatus' - The response's http status code.
newListCustomLineItemVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCustomLineItemVersionsResponse
newListCustomLineItemVersionsResponse pHttpStatus_ =
  ListCustomLineItemVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      customLineItemVersions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token that\'s used on subsequent calls to retrieve custom
-- line item versions.
listCustomLineItemVersionsResponse_nextToken :: Lens.Lens' ListCustomLineItemVersionsResponse (Prelude.Maybe Prelude.Text)
listCustomLineItemVersionsResponse_nextToken = Lens.lens (\ListCustomLineItemVersionsResponse' {nextToken} -> nextToken) (\s@ListCustomLineItemVersionsResponse' {} a -> s {nextToken = a} :: ListCustomLineItemVersionsResponse)

-- | A list of @CustomLineItemVersionListElements@ that are received.
listCustomLineItemVersionsResponse_customLineItemVersions :: Lens.Lens' ListCustomLineItemVersionsResponse (Prelude.Maybe [CustomLineItemVersionListElement])
listCustomLineItemVersionsResponse_customLineItemVersions = Lens.lens (\ListCustomLineItemVersionsResponse' {customLineItemVersions} -> customLineItemVersions) (\s@ListCustomLineItemVersionsResponse' {} a -> s {customLineItemVersions = a} :: ListCustomLineItemVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCustomLineItemVersionsResponse_httpStatus :: Lens.Lens' ListCustomLineItemVersionsResponse Prelude.Int
listCustomLineItemVersionsResponse_httpStatus = Lens.lens (\ListCustomLineItemVersionsResponse' {httpStatus} -> httpStatus) (\s@ListCustomLineItemVersionsResponse' {} a -> s {httpStatus = a} :: ListCustomLineItemVersionsResponse)

instance
  Prelude.NFData
    ListCustomLineItemVersionsResponse
  where
  rnf ListCustomLineItemVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf customLineItemVersions
      `Prelude.seq` Prelude.rnf httpStatus
