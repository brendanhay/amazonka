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
-- Module      : Amazonka.WorkSpacesWeb.ListIdentityProviders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of identity providers for a specific web portal.
module Amazonka.WorkSpacesWeb.ListIdentityProviders
  ( -- * Creating a Request
    ListIdentityProviders (..),
    newListIdentityProviders,

    -- * Request Lenses
    listIdentityProviders_maxResults,
    listIdentityProviders_nextToken,
    listIdentityProviders_portalArn,

    -- * Destructuring the Response
    ListIdentityProvidersResponse (..),
    newListIdentityProvidersResponse,

    -- * Response Lenses
    listIdentityProvidersResponse_identityProviders,
    listIdentityProvidersResponse_nextToken,
    listIdentityProvidersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpacesWeb.Types

-- | /See:/ 'newListIdentityProviders' smart constructor.
data ListIdentityProviders = ListIdentityProviders'
  { -- | The maximum number of results to be included in the next page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the web portal.
    portalArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIdentityProviders_maxResults' - The maximum number of results to be included in the next page.
--
-- 'nextToken', 'listIdentityProviders_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'portalArn', 'listIdentityProviders_portalArn' - The ARN of the web portal.
newListIdentityProviders ::
  -- | 'portalArn'
  Prelude.Text ->
  ListIdentityProviders
newListIdentityProviders pPortalArn_ =
  ListIdentityProviders'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      portalArn = pPortalArn_
    }

-- | The maximum number of results to be included in the next page.
listIdentityProviders_maxResults :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Natural)
listIdentityProviders_maxResults = Lens.lens (\ListIdentityProviders' {maxResults} -> maxResults) (\s@ListIdentityProviders' {} a -> s {maxResults = a} :: ListIdentityProviders)

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listIdentityProviders_nextToken :: Lens.Lens' ListIdentityProviders (Prelude.Maybe Prelude.Text)
listIdentityProviders_nextToken = Lens.lens (\ListIdentityProviders' {nextToken} -> nextToken) (\s@ListIdentityProviders' {} a -> s {nextToken = a} :: ListIdentityProviders)

-- | The ARN of the web portal.
listIdentityProviders_portalArn :: Lens.Lens' ListIdentityProviders Prelude.Text
listIdentityProviders_portalArn = Lens.lens (\ListIdentityProviders' {portalArn} -> portalArn) (\s@ListIdentityProviders' {} a -> s {portalArn = a} :: ListIdentityProviders)

instance Core.AWSRequest ListIdentityProviders where
  type
    AWSResponse ListIdentityProviders =
      ListIdentityProvidersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIdentityProvidersResponse'
            Prelude.<$> ( x
                            Data..?> "identityProviders"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIdentityProviders where
  hashWithSalt _salt ListIdentityProviders' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` portalArn

instance Prelude.NFData ListIdentityProviders where
  rnf ListIdentityProviders' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf portalArn

instance Data.ToHeaders ListIdentityProviders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListIdentityProviders where
  toPath ListIdentityProviders' {..} =
    Prelude.mconcat
      [ "/portals/",
        Data.toBS portalArn,
        "/identityProviders"
      ]

instance Data.ToQuery ListIdentityProviders where
  toQuery ListIdentityProviders' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListIdentityProvidersResponse' smart constructor.
data ListIdentityProvidersResponse = ListIdentityProvidersResponse'
  { -- | The identity providers.
    identityProviders :: Prelude.Maybe [IdentityProviderSummary],
    -- | The pagination token used to retrieve the next page of results for this
    -- operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIdentityProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityProviders', 'listIdentityProvidersResponse_identityProviders' - The identity providers.
--
-- 'nextToken', 'listIdentityProvidersResponse_nextToken' - The pagination token used to retrieve the next page of results for this
-- operation.
--
-- 'httpStatus', 'listIdentityProvidersResponse_httpStatus' - The response's http status code.
newListIdentityProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIdentityProvidersResponse
newListIdentityProvidersResponse pHttpStatus_ =
  ListIdentityProvidersResponse'
    { identityProviders =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identity providers.
listIdentityProvidersResponse_identityProviders :: Lens.Lens' ListIdentityProvidersResponse (Prelude.Maybe [IdentityProviderSummary])
listIdentityProvidersResponse_identityProviders = Lens.lens (\ListIdentityProvidersResponse' {identityProviders} -> identityProviders) (\s@ListIdentityProvidersResponse' {} a -> s {identityProviders = a} :: ListIdentityProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to retrieve the next page of results for this
-- operation.
listIdentityProvidersResponse_nextToken :: Lens.Lens' ListIdentityProvidersResponse (Prelude.Maybe Prelude.Text)
listIdentityProvidersResponse_nextToken = Lens.lens (\ListIdentityProvidersResponse' {nextToken} -> nextToken) (\s@ListIdentityProvidersResponse' {} a -> s {nextToken = a} :: ListIdentityProvidersResponse)

-- | The response's http status code.
listIdentityProvidersResponse_httpStatus :: Lens.Lens' ListIdentityProvidersResponse Prelude.Int
listIdentityProvidersResponse_httpStatus = Lens.lens (\ListIdentityProvidersResponse' {httpStatus} -> httpStatus) (\s@ListIdentityProvidersResponse' {} a -> s {httpStatus = a} :: ListIdentityProvidersResponse)

instance Prelude.NFData ListIdentityProvidersResponse where
  rnf ListIdentityProvidersResponse' {..} =
    Prelude.rnf identityProviders
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
