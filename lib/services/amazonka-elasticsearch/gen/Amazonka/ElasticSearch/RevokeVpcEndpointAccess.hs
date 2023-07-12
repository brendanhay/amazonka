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
-- Module      : Amazonka.ElasticSearch.RevokeVpcEndpointAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes access to an Amazon OpenSearch Service domain that was provided
-- through an interface VPC endpoint.
module Amazonka.ElasticSearch.RevokeVpcEndpointAccess
  ( -- * Creating a Request
    RevokeVpcEndpointAccess (..),
    newRevokeVpcEndpointAccess,

    -- * Request Lenses
    revokeVpcEndpointAccess_domainName,
    revokeVpcEndpointAccess_account,

    -- * Destructuring the Response
    RevokeVpcEndpointAccessResponse (..),
    newRevokeVpcEndpointAccessResponse,

    -- * Response Lenses
    revokeVpcEndpointAccessResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Revokes access to an Amazon OpenSearch Service domain that was provided
-- through an interface VPC endpoint.
--
-- /See:/ 'newRevokeVpcEndpointAccess' smart constructor.
data RevokeVpcEndpointAccess = RevokeVpcEndpointAccess'
  { -- | The name of the OpenSearch Service domain.
    domainName :: Prelude.Text,
    -- | The account ID to revoke access from.
    account :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeVpcEndpointAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'revokeVpcEndpointAccess_domainName' - The name of the OpenSearch Service domain.
--
-- 'account', 'revokeVpcEndpointAccess_account' - The account ID to revoke access from.
newRevokeVpcEndpointAccess ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'account'
  Prelude.Text ->
  RevokeVpcEndpointAccess
newRevokeVpcEndpointAccess pDomainName_ pAccount_ =
  RevokeVpcEndpointAccess'
    { domainName = pDomainName_,
      account = pAccount_
    }

-- | The name of the OpenSearch Service domain.
revokeVpcEndpointAccess_domainName :: Lens.Lens' RevokeVpcEndpointAccess Prelude.Text
revokeVpcEndpointAccess_domainName = Lens.lens (\RevokeVpcEndpointAccess' {domainName} -> domainName) (\s@RevokeVpcEndpointAccess' {} a -> s {domainName = a} :: RevokeVpcEndpointAccess)

-- | The account ID to revoke access from.
revokeVpcEndpointAccess_account :: Lens.Lens' RevokeVpcEndpointAccess Prelude.Text
revokeVpcEndpointAccess_account = Lens.lens (\RevokeVpcEndpointAccess' {account} -> account) (\s@RevokeVpcEndpointAccess' {} a -> s {account = a} :: RevokeVpcEndpointAccess)

instance Core.AWSRequest RevokeVpcEndpointAccess where
  type
    AWSResponse RevokeVpcEndpointAccess =
      RevokeVpcEndpointAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          RevokeVpcEndpointAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RevokeVpcEndpointAccess where
  hashWithSalt _salt RevokeVpcEndpointAccess' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` account

instance Prelude.NFData RevokeVpcEndpointAccess where
  rnf RevokeVpcEndpointAccess' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf account

instance Data.ToHeaders RevokeVpcEndpointAccess where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RevokeVpcEndpointAccess where
  toJSON RevokeVpcEndpointAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Account" Data..= account)]
      )

instance Data.ToPath RevokeVpcEndpointAccess where
  toPath RevokeVpcEndpointAccess' {..} =
    Prelude.mconcat
      [ "/2015-01-01/es/domain/",
        Data.toBS domainName,
        "/revokeVpcEndpointAccess"
      ]

instance Data.ToQuery RevokeVpcEndpointAccess where
  toQuery = Prelude.const Prelude.mempty

-- | Container for response parameters to the @RevokeVpcEndpointAccess@
-- operation. The response body for this operation is empty.
--
-- /See:/ 'newRevokeVpcEndpointAccessResponse' smart constructor.
data RevokeVpcEndpointAccessResponse = RevokeVpcEndpointAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevokeVpcEndpointAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'revokeVpcEndpointAccessResponse_httpStatus' - The response's http status code.
newRevokeVpcEndpointAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RevokeVpcEndpointAccessResponse
newRevokeVpcEndpointAccessResponse pHttpStatus_ =
  RevokeVpcEndpointAccessResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
revokeVpcEndpointAccessResponse_httpStatus :: Lens.Lens' RevokeVpcEndpointAccessResponse Prelude.Int
revokeVpcEndpointAccessResponse_httpStatus = Lens.lens (\RevokeVpcEndpointAccessResponse' {httpStatus} -> httpStatus) (\s@RevokeVpcEndpointAccessResponse' {} a -> s {httpStatus = a} :: RevokeVpcEndpointAccessResponse)

instance
  Prelude.NFData
    RevokeVpcEndpointAccessResponse
  where
  rnf RevokeVpcEndpointAccessResponse' {..} =
    Prelude.rnf httpStatus
