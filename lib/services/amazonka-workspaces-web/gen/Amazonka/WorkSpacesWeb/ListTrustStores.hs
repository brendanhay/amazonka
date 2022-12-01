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
-- Module      : Amazonka.WorkSpacesWeb.ListTrustStores
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of trust stores.
module Amazonka.WorkSpacesWeb.ListTrustStores
  ( -- * Creating a Request
    ListTrustStores (..),
    newListTrustStores,

    -- * Request Lenses
    listTrustStores_nextToken,
    listTrustStores_maxResults,

    -- * Destructuring the Response
    ListTrustStoresResponse (..),
    newListTrustStoresResponse,

    -- * Response Lenses
    listTrustStoresResponse_nextToken,
    listTrustStoresResponse_trustStores,
    listTrustStoresResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListTrustStores' smart constructor.
data ListTrustStores = ListTrustStores'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustStores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrustStores_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'maxResults', 'listTrustStores_maxResults' - The maximum number of results to be included in the next page.
newListTrustStores ::
  ListTrustStores
newListTrustStores =
  ListTrustStores'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listTrustStores_nextToken :: Lens.Lens' ListTrustStores (Prelude.Maybe Prelude.Text)
listTrustStores_nextToken = Lens.lens (\ListTrustStores' {nextToken} -> nextToken) (\s@ListTrustStores' {} a -> s {nextToken = a} :: ListTrustStores)

-- | The maximum number of results to be included in the next page.
listTrustStores_maxResults :: Lens.Lens' ListTrustStores (Prelude.Maybe Prelude.Natural)
listTrustStores_maxResults = Lens.lens (\ListTrustStores' {maxResults} -> maxResults) (\s@ListTrustStores' {} a -> s {maxResults = a} :: ListTrustStores)

instance Core.AWSRequest ListTrustStores where
  type
    AWSResponse ListTrustStores =
      ListTrustStoresResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrustStoresResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "trustStores" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrustStores where
  hashWithSalt _salt ListTrustStores' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListTrustStores where
  rnf ListTrustStores' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListTrustStores where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListTrustStores where
  toPath = Prelude.const "/trustStores"

instance Core.ToQuery ListTrustStores where
  toQuery ListTrustStores' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListTrustStoresResponse' smart constructor.
data ListTrustStoresResponse = ListTrustStoresResponse'
  { -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The trust stores.
    trustStores :: Prelude.Maybe [TrustStoreSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrustStoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrustStoresResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'trustStores', 'listTrustStoresResponse_trustStores' - The trust stores.
--
-- 'httpStatus', 'listTrustStoresResponse_httpStatus' - The response's http status code.
newListTrustStoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrustStoresResponse
newListTrustStoresResponse pHttpStatus_ =
  ListTrustStoresResponse'
    { nextToken =
        Prelude.Nothing,
      trustStores = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listTrustStoresResponse_nextToken :: Lens.Lens' ListTrustStoresResponse (Prelude.Maybe Prelude.Text)
listTrustStoresResponse_nextToken = Lens.lens (\ListTrustStoresResponse' {nextToken} -> nextToken) (\s@ListTrustStoresResponse' {} a -> s {nextToken = a} :: ListTrustStoresResponse)

-- | The trust stores.
listTrustStoresResponse_trustStores :: Lens.Lens' ListTrustStoresResponse (Prelude.Maybe [TrustStoreSummary])
listTrustStoresResponse_trustStores = Lens.lens (\ListTrustStoresResponse' {trustStores} -> trustStores) (\s@ListTrustStoresResponse' {} a -> s {trustStores = a} :: ListTrustStoresResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrustStoresResponse_httpStatus :: Lens.Lens' ListTrustStoresResponse Prelude.Int
listTrustStoresResponse_httpStatus = Lens.lens (\ListTrustStoresResponse' {httpStatus} -> httpStatus) (\s@ListTrustStoresResponse' {} a -> s {httpStatus = a} :: ListTrustStoresResponse)

instance Prelude.NFData ListTrustStoresResponse where
  rnf ListTrustStoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trustStores
      `Prelude.seq` Prelude.rnf httpStatus
