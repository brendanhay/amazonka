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
-- Module      : Amazonka.CustomerProfiles.ListDomains
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the domains for an AWS account that have been
-- created.
module Amazonka.CustomerProfiles.ListDomains
  ( -- * Creating a Request
    ListDomains (..),
    newListDomains,

    -- * Request Lenses
    listDomains_nextToken,
    listDomains_maxResults,

    -- * Destructuring the Response
    ListDomainsResponse (..),
    newListDomainsResponse,

    -- * Response Lenses
    listDomainsResponse_items,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDomains' smart constructor.
data ListDomains = ListDomains'
  { -- | The pagination token from the previous ListDomain API call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listDomains_nextToken' - The pagination token from the previous ListDomain API call.
--
-- 'maxResults', 'listDomains_maxResults' - The maximum number of objects returned per page.
newListDomains ::
  ListDomains
newListDomains =
  ListDomains'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token from the previous ListDomain API call.
listDomains_nextToken :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Text)
listDomains_nextToken = Lens.lens (\ListDomains' {nextToken} -> nextToken) (\s@ListDomains' {} a -> s {nextToken = a} :: ListDomains)

-- | The maximum number of objects returned per page.
listDomains_maxResults :: Lens.Lens' ListDomains (Prelude.Maybe Prelude.Natural)
listDomains_maxResults = Lens.lens (\ListDomains' {maxResults} -> maxResults) (\s@ListDomains' {} a -> s {maxResults = a} :: ListDomains)

instance Core.AWSRequest ListDomains where
  type AWSResponse ListDomains = ListDomainsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDomainsResponse'
            Prelude.<$> (x Core..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDomains where
  hashWithSalt _salt ListDomains' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListDomains where
  rnf ListDomains' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListDomains where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDomains where
  toPath = Prelude.const "/domains"

instance Core.ToQuery ListDomains where
  toQuery ListDomains' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { -- | The list of ListDomains instances.
    items :: Prelude.Maybe [ListDomainItem],
    -- | The pagination token from the previous ListDomains API call.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'items', 'listDomainsResponse_items' - The list of ListDomains instances.
--
-- 'nextToken', 'listDomainsResponse_nextToken' - The pagination token from the previous ListDomains API call.
--
-- 'httpStatus', 'listDomainsResponse_httpStatus' - The response's http status code.
newListDomainsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDomainsResponse
newListDomainsResponse pHttpStatus_ =
  ListDomainsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of ListDomains instances.
listDomainsResponse_items :: Lens.Lens' ListDomainsResponse (Prelude.Maybe [ListDomainItem])
listDomainsResponse_items = Lens.lens (\ListDomainsResponse' {items} -> items) (\s@ListDomainsResponse' {} a -> s {items = a} :: ListDomainsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token from the previous ListDomains API call.
listDomainsResponse_nextToken :: Lens.Lens' ListDomainsResponse (Prelude.Maybe Prelude.Text)
listDomainsResponse_nextToken = Lens.lens (\ListDomainsResponse' {nextToken} -> nextToken) (\s@ListDomainsResponse' {} a -> s {nextToken = a} :: ListDomainsResponse)

-- | The response's http status code.
listDomainsResponse_httpStatus :: Lens.Lens' ListDomainsResponse Prelude.Int
listDomainsResponse_httpStatus = Lens.lens (\ListDomainsResponse' {httpStatus} -> httpStatus) (\s@ListDomainsResponse' {} a -> s {httpStatus = a} :: ListDomainsResponse)

instance Prelude.NFData ListDomainsResponse where
  rnf ListDomainsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
