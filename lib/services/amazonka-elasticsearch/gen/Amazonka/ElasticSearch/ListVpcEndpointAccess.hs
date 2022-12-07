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
-- Module      : Amazonka.ElasticSearch.ListVpcEndpointAccess
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about each principal that is allowed to access a
-- given Amazon OpenSearch Service domain through the use of an interface
-- VPC endpoint.
module Amazonka.ElasticSearch.ListVpcEndpointAccess
  ( -- * Creating a Request
    ListVpcEndpointAccess (..),
    newListVpcEndpointAccess,

    -- * Request Lenses
    listVpcEndpointAccess_nextToken,
    listVpcEndpointAccess_domainName,

    -- * Destructuring the Response
    ListVpcEndpointAccessResponse (..),
    newListVpcEndpointAccessResponse,

    -- * Response Lenses
    listVpcEndpointAccessResponse_httpStatus,
    listVpcEndpointAccessResponse_authorizedPrincipalList,
    listVpcEndpointAccessResponse_nextToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Retrieves information about each principal that is allowed to access a
-- given Amazon OpenSearch Service domain through the use of an interface
-- VPC endpoint
--
-- /See:/ 'newListVpcEndpointAccess' smart constructor.
data ListVpcEndpointAccess = ListVpcEndpointAccess'
  { -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the OpenSearch Service domain to retrieve access information
    -- for.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcEndpointAccess_nextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- 'domainName', 'listVpcEndpointAccess_domainName' - The name of the OpenSearch Service domain to retrieve access information
-- for.
newListVpcEndpointAccess ::
  -- | 'domainName'
  Prelude.Text ->
  ListVpcEndpointAccess
newListVpcEndpointAccess pDomainName_ =
  ListVpcEndpointAccess'
    { nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Provides an identifier to allow retrieval of paginated results.
listVpcEndpointAccess_nextToken :: Lens.Lens' ListVpcEndpointAccess (Prelude.Maybe Prelude.Text)
listVpcEndpointAccess_nextToken = Lens.lens (\ListVpcEndpointAccess' {nextToken} -> nextToken) (\s@ListVpcEndpointAccess' {} a -> s {nextToken = a} :: ListVpcEndpointAccess)

-- | The name of the OpenSearch Service domain to retrieve access information
-- for.
listVpcEndpointAccess_domainName :: Lens.Lens' ListVpcEndpointAccess Prelude.Text
listVpcEndpointAccess_domainName = Lens.lens (\ListVpcEndpointAccess' {domainName} -> domainName) (\s@ListVpcEndpointAccess' {} a -> s {domainName = a} :: ListVpcEndpointAccess)

instance Core.AWSRequest ListVpcEndpointAccess where
  type
    AWSResponse ListVpcEndpointAccess =
      ListVpcEndpointAccessResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcEndpointAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "AuthorizedPrincipalList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "NextToken")
      )

instance Prelude.Hashable ListVpcEndpointAccess where
  hashWithSalt _salt ListVpcEndpointAccess' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListVpcEndpointAccess where
  rnf ListVpcEndpointAccess' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListVpcEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListVpcEndpointAccess where
  toPath ListVpcEndpointAccess' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/listVpcEndpointAccess"
      ]

instance Data.ToQuery ListVpcEndpointAccess where
  toQuery ListVpcEndpointAccess' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | Container for response parameters to the @ListVpcEndpointAccess@
-- operation. Returns a list of accounts id and account type authorized to
-- manage VPC endpoints.
--
-- /See:/ 'newListVpcEndpointAccessResponse' smart constructor.
data ListVpcEndpointAccessResponse = ListVpcEndpointAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of @AuthorizedPrincipal@ describing the details of the permissions
    -- to manage VPC endpoints against the specified domain.
    authorizedPrincipalList :: [AuthorizedPrincipal],
    -- | Provides an identifier to allow retrieval of paginated results.
    nextToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'listVpcEndpointAccessResponse_httpStatus' - The response's http status code.
--
-- 'authorizedPrincipalList', 'listVpcEndpointAccessResponse_authorizedPrincipalList' - List of @AuthorizedPrincipal@ describing the details of the permissions
-- to manage VPC endpoints against the specified domain.
--
-- 'nextToken', 'listVpcEndpointAccessResponse_nextToken' - Provides an identifier to allow retrieval of paginated results.
newListVpcEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'nextToken'
  Prelude.Text ->
  ListVpcEndpointAccessResponse
newListVpcEndpointAccessResponse
  pHttpStatus_
  pNextToken_ =
    ListVpcEndpointAccessResponse'
      { httpStatus =
          pHttpStatus_,
        authorizedPrincipalList = Prelude.mempty,
        nextToken = pNextToken_
      }

-- | The response's http status code.
listVpcEndpointAccessResponse_httpStatus :: Lens.Lens' ListVpcEndpointAccessResponse Prelude.Int
listVpcEndpointAccessResponse_httpStatus = Lens.lens (\ListVpcEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@ListVpcEndpointAccessResponse' {} a -> s {httpStatus = a} :: ListVpcEndpointAccessResponse)

-- | List of @AuthorizedPrincipal@ describing the details of the permissions
-- to manage VPC endpoints against the specified domain.
listVpcEndpointAccessResponse_authorizedPrincipalList :: Lens.Lens' ListVpcEndpointAccessResponse [AuthorizedPrincipal]
listVpcEndpointAccessResponse_authorizedPrincipalList = Lens.lens (\ListVpcEndpointAccessResponse' {authorizedPrincipalList} -> authorizedPrincipalList) (\s@ListVpcEndpointAccessResponse' {} a -> s {authorizedPrincipalList = a} :: ListVpcEndpointAccessResponse) Prelude.. Lens.coerced

-- | Provides an identifier to allow retrieval of paginated results.
listVpcEndpointAccessResponse_nextToken :: Lens.Lens' ListVpcEndpointAccessResponse Prelude.Text
listVpcEndpointAccessResponse_nextToken = Lens.lens (\ListVpcEndpointAccessResponse' {nextToken} -> nextToken) (\s@ListVpcEndpointAccessResponse' {} a -> s {nextToken = a} :: ListVpcEndpointAccessResponse)

instance Prelude.NFData ListVpcEndpointAccessResponse where
  rnf ListVpcEndpointAccessResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf authorizedPrincipalList
      `Prelude.seq` Prelude.rnf nextToken
