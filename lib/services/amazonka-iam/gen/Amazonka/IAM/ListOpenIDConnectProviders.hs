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
-- Module      : Amazonka.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the IAM OpenID Connect (OIDC) provider resource
-- objects defined in the Amazon Web Services account.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for an OIDC provider, see
-- GetOpenIDConnectProvider.
module Amazonka.IAM.ListOpenIDConnectProviders
  ( -- * Creating a Request
    ListOpenIDConnectProviders (..),
    newListOpenIDConnectProviders,

    -- * Destructuring the Response
    ListOpenIDConnectProvidersResponse (..),
    newListOpenIDConnectProvidersResponse,

    -- * Response Lenses
    listOpenIDConnectProvidersResponse_openIDConnectProviderList,
    listOpenIDConnectProvidersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOpenIDConnectProviders' smart constructor.
data ListOpenIDConnectProviders = ListOpenIDConnectProviders'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListOpenIDConnectProviders ::
  ListOpenIDConnectProviders
newListOpenIDConnectProviders =
  ListOpenIDConnectProviders'

instance Core.AWSRequest ListOpenIDConnectProviders where
  type
    AWSResponse ListOpenIDConnectProviders =
      ListOpenIDConnectProvidersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListOpenIDConnectProvidersResult"
      ( \s h x ->
          ListOpenIDConnectProvidersResponse'
            Prelude.<$> ( x Data..@? "OpenIDConnectProviderList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOpenIDConnectProviders where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData ListOpenIDConnectProviders where
  rnf _ = ()

instance Data.ToHeaders ListOpenIDConnectProviders where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListOpenIDConnectProviders where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOpenIDConnectProviders where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("ListOpenIDConnectProviders" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful ListOpenIDConnectProviders
-- request.
--
-- /See:/ 'newListOpenIDConnectProvidersResponse' smart constructor.
data ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'
  { -- | The list of IAM OIDC provider resource objects defined in the Amazon Web
    -- Services account.
    openIDConnectProviderList :: Prelude.Maybe [OpenIDConnectProviderListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderList', 'listOpenIDConnectProvidersResponse_openIDConnectProviderList' - The list of IAM OIDC provider resource objects defined in the Amazon Web
-- Services account.
--
-- 'httpStatus', 'listOpenIDConnectProvidersResponse_httpStatus' - The response's http status code.
newListOpenIDConnectProvidersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOpenIDConnectProvidersResponse
newListOpenIDConnectProvidersResponse pHttpStatus_ =
  ListOpenIDConnectProvidersResponse'
    { openIDConnectProviderList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of IAM OIDC provider resource objects defined in the Amazon Web
-- Services account.
listOpenIDConnectProvidersResponse_openIDConnectProviderList :: Lens.Lens' ListOpenIDConnectProvidersResponse (Prelude.Maybe [OpenIDConnectProviderListEntry])
listOpenIDConnectProvidersResponse_openIDConnectProviderList = Lens.lens (\ListOpenIDConnectProvidersResponse' {openIDConnectProviderList} -> openIDConnectProviderList) (\s@ListOpenIDConnectProvidersResponse' {} a -> s {openIDConnectProviderList = a} :: ListOpenIDConnectProvidersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOpenIDConnectProvidersResponse_httpStatus :: Lens.Lens' ListOpenIDConnectProvidersResponse Prelude.Int
listOpenIDConnectProvidersResponse_httpStatus = Lens.lens (\ListOpenIDConnectProvidersResponse' {httpStatus} -> httpStatus) (\s@ListOpenIDConnectProvidersResponse' {} a -> s {httpStatus = a} :: ListOpenIDConnectProvidersResponse)

instance
  Prelude.NFData
    ListOpenIDConnectProvidersResponse
  where
  rnf ListOpenIDConnectProvidersResponse' {..} =
    Prelude.rnf openIDConnectProviderList
      `Prelude.seq` Prelude.rnf httpStatus
