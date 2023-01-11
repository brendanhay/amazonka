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
-- Module      : Amazonka.Route53Domains.ListDomains
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns all the domain names registered with Amazon Route
-- 53 for the current Amazon Web Services account if no filtering
-- conditions are used.
--
-- This operation returns paginated results.
module Amazonka.Route53Domains.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_filterConditions,
    listDomains_marker,
    listDomains_maxItems,
    listDomains_sortCondition,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_domains,
    listDomainsResponse_nextPageMarker,
    listDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | The ListDomains request includes the following elements.
--
-- /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | A complex type that contains information about the filters applied
    -- during the @ListDomains@ request. The filter conditions can include
    -- domain name and domain expiration.
    filterConditions :: Prelude.Maybe [FilterCondition],
    -- | For an initial request for a list of domains, omit this element. If the
    -- number of domains that are associated with the current Amazon Web
    -- Services account is greater than the value that you specified for
    -- @MaxItems@, you can use @Marker@ to return additional domains. Get the
    -- value of @NextPageMarker@ from the previous response, and submit another
    -- request that includes the value of @NextPageMarker@ in the @Marker@
    -- element.
    --
    -- Constraints: The marker must match the value specified in the previous
    -- request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | Number of domains to be returned.
    --
    -- Default: 20
    maxItems :: Prelude.Maybe Prelude.Int,
    -- | A complex type that contains information about the requested ordering of
    -- domains in the returned list.
    sortCondition :: Prelude.Maybe SortCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomains' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterConditions', 'listDomains_filterConditions' - A complex type that contains information about the filters applied
-- during the @ListDomains@ request. The filter conditions can include
-- domain name and domain expiration.
--
-- 'marker', 'listDomains_marker' - For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current Amazon Web
-- Services account is greater than the value that you specified for
-- @MaxItems@, you can use @Marker@ to return additional domains. Get the
-- value of @NextPageMarker@ from the previous response, and submit another
-- request that includes the value of @NextPageMarker@ in the @Marker@
-- element.
--
-- Constraints: The marker must match the value specified in the previous
-- request.
--
-- 'maxItems', 'listDomains_maxItems' - Number of domains to be returned.
--
-- Default: 20
--
-- 'sortCondition', 'listDomains_sortCondition' - A complex type that contains information about the requested ordering of
-- domains in the returned list.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { filterConditions = Prelude.Nothing,
      marker = Prelude.Nothing,
      maxItems = Prelude.Nothing,
      sortCondition = Prelude.Nothing
    }

-- | A complex type that contains information about the filters applied
-- during the @ListDomains@ request. The filter conditions can include
-- domain name and domain expiration.
listDomains_filterConditions :: Lens.Lens' ListDomains (Prelude.Maybe [FilterCondition])
listDomains_filterConditions = Lens.lens (\ListDomains' {filterConditions} -> filterConditions) (\s@ListDomains' {} a -> s {filterConditions = a} :: ListDomains) Prelude.. Lens.mapping Lens.coerced

-- | For an initial request for a list of domains, omit this element. If the
-- number of domains that are associated with the current Amazon Web
-- Services account is greater than the value that you specified for
-- @MaxItems@, you can use @Marker@ to return additional domains. Get the
-- value of @NextPageMarker@ from the previous response, and submit another
-- request that includes the value of @NextPageMarker@ in the @Marker@
-- element.
--
-- Constraints: The marker must match the value specified in the previous
-- request.
listDomains_marker :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_marker = Lens.lens (\ListDomains' {marker} -> marker) (\s@ListDomains' {} a -> s {marker = a} :: ListDomains)

-- | Number of domains to be returned.
--
-- Default: 20
listDomains_maxItems :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Int)
listDomains_maxItems = Lens.lens (\ListDomains' {maxItems} -> maxItems) (\s@ListDomains' {} a -> s {maxItems = a} :: ListDomains)

-- | A complex type that contains information about the requested ordering of
-- domains in the returned list.
listDomains_sortCondition :: Lens.Lens' ListDomains (Prelude.Maybe SortCondition)
listDomains_sortCondition = Lens.lens (\ListDomains' {sortCondition} -> sortCondition) (\s@ListDomains' {} a -> s {sortCondition = a} :: ListDomains)

instance Core.AWSPager ListDomains where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_nextPageMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDomainsResponse_domains Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDomains_marker
          Lens..~ rs
          Lens.^? listDomainsResponse_nextPageMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (x Data..?> "Domains" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextPageMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomains where
  hashWithSalt _salt ListDomains' {..} =
    _salt `Prelude.hashWithSalt` filterConditions
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxItems
      `Prelude.hashWithSalt` sortCondition

instance Prelude.NFData ListDomains where
  rnf ListDomains' {..} =
    Prelude.rnf filterConditions
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxItems
      `Prelude.seq` Prelude.rnf sortCondition

instance Data.ToHeaders ListDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.ListDomains" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDomains where
  toJSON ListDomains' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterConditions" Data..=)
              Prelude.<$> filterConditions,
            ("Marker" Data..=) Prelude.<$> marker,
            ("MaxItems" Data..=) Prelude.<$> maxItems,
            ("SortCondition" Data..=) Prelude.<$> sortCondition
          ]
      )

instance Data.ToPath ListDomains where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDomains where
  toQuery = Prelude.const Prelude.mempty

-- | The ListDomains response includes the following elements.
--
-- /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | A list of domains.
    domains :: Prelude.Maybe [DomainSummary],
    -- | If there are more domains than you specified for @MaxItems@ in the
    -- request, submit another request and include the value of
    -- @NextPageMarker@ in the value of @Marker@.
    nextPageMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDomainsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domains', 'listDomainsResponse_domains' - A list of domains.
--
-- 'nextPageMarker', 'listDomainsResponse_nextPageMarker' - If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { domains = Prelude.Nothing,
      nextPageMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of domains.
listDomainsResponse_domains :: Lens.Lens' ListDomainsResponse (Prelude.Maybe [DomainSummary])
listDomainsResponse_domains = Lens.lens (\ListDomainsResponse' {domains} -> domains) (\s@ListDomainsResponse' {} a -> s {domains = a} :: ListDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more domains than you specified for @MaxItems@ in the
-- request, submit another request and include the value of
-- @NextPageMarker@ in the value of @Marker@.
listDomainsResponse_nextPageMarker :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextPageMarker = Lens.lens (\ListDomainsResponse' {nextPageMarker} -> nextPageMarker) (\s@ListDomainsResponse' {} a -> s {nextPageMarker = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Prelude.NFData ListDomainsResponse where
  rnf ListDomainsResponse' {..} =
    Prelude.rnf domains
      `Prelude.seq` Prelude.rnf nextPageMarker
      `Prelude.seq` Prelude.rnf httpStatus
