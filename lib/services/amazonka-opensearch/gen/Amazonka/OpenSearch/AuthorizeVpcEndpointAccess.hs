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
-- Module      : Amazonka.OpenSearch.AuthorizeVpcEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides access to an Amazon OpenSearch Service domain through the use
-- of an interface VPC endpoint.
module Amazonka.OpenSearch.AuthorizeVpcEndpointAccess
  ( -- * Creating a Request
    AuthorizeVpcEndpointAccess (..),
    newAuthorizeVpcEndpointAccess,

    -- * Request Lenses
    authorizeVpcEndpointAccess_domainName,
    authorizeVpcEndpointAccess_account,

    -- * Destructuring the Response
    AuthorizeVpcEndpointAccessResponse (..),
    newAuthorizeVpcEndpointAccessResponse,

    -- * Response Lenses
    authorizeVpcEndpointAccessResponse_httpStatus,
    authorizeVpcEndpointAccessResponse_authorizedPrincipal,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAuthorizeVpcEndpointAccess' smart constructor.
data AuthorizeVpcEndpointAccess = AuthorizeVpcEndpointAccess'
  { -- | The name of the OpenSearch Service domain to provide access to.
    domainName :: Prelude.Text,
    -- | The Amazon Web Services account ID to grant access to.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeVpcEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'authorizeVpcEndpointAccess_domainName' - The name of the OpenSearch Service domain to provide access to.
--
-- 'account', 'authorizeVpcEndpointAccess_account' - The Amazon Web Services account ID to grant access to.
newAuthorizeVpcEndpointAccess ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'account'
  Prelude.Text ->
  AuthorizeVpcEndpointAccess
newAuthorizeVpcEndpointAccess pDomainName_ pAccount_ =
  AuthorizeVpcEndpointAccess'
    { domainName =
        pDomainName_,
      account = pAccount_
    }

-- | The name of the OpenSearch Service domain to provide access to.
authorizeVpcEndpointAccess_domainName :: Lens.Lens' AuthorizeVpcEndpointAccess Prelude.Text
authorizeVpcEndpointAccess_domainName = Lens.lens (\AuthorizeVpcEndpointAccess' {domainName} -> domainName) (\s@AuthorizeVpcEndpointAccess' {} a -> s {domainName = a} :: AuthorizeVpcEndpointAccess)

-- | The Amazon Web Services account ID to grant access to.
authorizeVpcEndpointAccess_account :: Lens.Lens' AuthorizeVpcEndpointAccess Prelude.Text
authorizeVpcEndpointAccess_account = Lens.lens (\AuthorizeVpcEndpointAccess' {account} -> account) (\s@AuthorizeVpcEndpointAccess' {} a -> s {account = a} :: AuthorizeVpcEndpointAccess)

instance Core.AWSRequest AuthorizeVpcEndpointAccess where
  type
    AWSResponse AuthorizeVpcEndpointAccess =
      AuthorizeVpcEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AuthorizeVpcEndpointAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "AuthorizedPrincipal")
      )

instance Prelude.Hashable AuthorizeVpcEndpointAccess where
  hashWithSalt _salt AuthorizeVpcEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` account

instance Prelude.NFData AuthorizeVpcEndpointAccess where
  rnf AuthorizeVpcEndpointAccess' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf account

instance Data.ToHeaders AuthorizeVpcEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AuthorizeVpcEndpointAccess where
  toJSON AuthorizeVpcEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Account" Data..= account)]
      )

instance Data.ToPath AuthorizeVpcEndpointAccess where
  toPath AuthorizeVpcEndpointAccess' {..} =
    Prelude.mconcat
      [ "/2021-01-01/opensearch/domain/",
        Data.toBS domainName,
        "/authorizeVpcEndpointAccess"
      ]

instance Data.ToQuery AuthorizeVpcEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAuthorizeVpcEndpointAccessResponse' smart constructor.
data AuthorizeVpcEndpointAccessResponse = AuthorizeVpcEndpointAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the Amazon Web Services account or service that was
    -- provided access to the domain.
    authorizedPrincipal :: AuthorizedPrincipal
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizeVpcEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'authorizeVpcEndpointAccessResponse_httpStatus' - The response's http status code.
--
-- 'authorizedPrincipal', 'authorizeVpcEndpointAccessResponse_authorizedPrincipal' - Information about the Amazon Web Services account or service that was
-- provided access to the domain.
newAuthorizeVpcEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'authorizedPrincipal'
  AuthorizedPrincipal ->
  AuthorizeVpcEndpointAccessResponse
newAuthorizeVpcEndpointAccessResponse
  pHttpStatus_
  pAuthorizedPrincipal_ =
    AuthorizeVpcEndpointAccessResponse'
      { httpStatus =
          pHttpStatus_,
        authorizedPrincipal =
          pAuthorizedPrincipal_
      }

-- | The response's http status code.
authorizeVpcEndpointAccessResponse_httpStatus :: Lens.Lens' AuthorizeVpcEndpointAccessResponse Prelude.Int
authorizeVpcEndpointAccessResponse_httpStatus = Lens.lens (\AuthorizeVpcEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@AuthorizeVpcEndpointAccessResponse' {} a -> s {httpStatus = a} :: AuthorizeVpcEndpointAccessResponse)

-- | Information about the Amazon Web Services account or service that was
-- provided access to the domain.
authorizeVpcEndpointAccessResponse_authorizedPrincipal :: Lens.Lens' AuthorizeVpcEndpointAccessResponse AuthorizedPrincipal
authorizeVpcEndpointAccessResponse_authorizedPrincipal = Lens.lens (\AuthorizeVpcEndpointAccessResponse' {authorizedPrincipal} -> authorizedPrincipal) (\s@AuthorizeVpcEndpointAccessResponse' {} a -> s {authorizedPrincipal = a} :: AuthorizeVpcEndpointAccessResponse)

instance
  Prelude.NFData
    AuthorizeVpcEndpointAccessResponse
  where
  rnf AuthorizeVpcEndpointAccessResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf authorizedPrincipal
