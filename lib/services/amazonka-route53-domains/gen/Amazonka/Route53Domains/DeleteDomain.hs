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
-- Module      : Amazonka.Route53Domains.DeleteDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified domain. This action is permanent.
-- For more information, see
-- <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-delete.html Deleting a domain name registration>.
--
-- To transfer the domain registration to another registrar, use the
-- transfer process that’s provided by the registrar to which you want to
-- transfer the registration. Otherwise, the following apply:
--
-- 1.  You can’t get a refund for the cost of a deleted domain
--     registration.
--
-- 2.  The registry for the top-level domain might hold the domain name for
--     a brief time before releasing it for other users to register (varies
--     by registry).
--
-- 3.  When the registration has been deleted, we\'ll send you a
--     confirmation to the registrant contact. The email will come from
--     @noreply\@domainnameverification.net@ or
--     @noreply\@registrar.amazon.com@.
module Amazonka.Route53Domains.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainName,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,

    -- * Response Lenses
    deleteDomainResponse_operationId,
    deleteDomainResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Domains.Types

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | Name of the domain to be deleted.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteDomain_domainName' - Name of the domain to be deleted.
newDeleteDomain ::
  -- | 'domainName'
  Prelude.Text ->
  DeleteDomain
newDeleteDomain pDomainName_ =
  DeleteDomain' {domainName = pDomainName_}

-- | Name of the domain to be deleted.
deleteDomain_domainName :: Lens.Lens' DeleteDomain Prelude.Text
deleteDomain_domainName = Lens.lens (\DeleteDomain' {domainName} -> domainName) (\s@DeleteDomain' {} a -> s {domainName = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomain where
  hashWithSalt _salt DeleteDomain' {..} =
    _salt `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteDomain where
  rnf DeleteDomain' {..} = Prelude.rnf domainName

instance Data.ToHeaders DeleteDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Domains_v20140515.DeleteDomain" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteDomain where
  toJSON DeleteDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("DomainName" Data..= domainName)]
      )

instance Data.ToPath DeleteDomain where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { -- | Identifier for tracking the progress of the request. To query the
    -- operation status, use
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'deleteDomainResponse_operationId' - Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
--
-- 'httpStatus', 'deleteDomainResponse_httpStatus' - The response's http status code.
newDeleteDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainResponse
newDeleteDomainResponse pHttpStatus_ =
  DeleteDomainResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Identifier for tracking the progress of the request. To query the
-- operation status, use
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_domains_GetOperationDetail.html GetOperationDetail>.
deleteDomainResponse_operationId :: Lens.Lens' DeleteDomainResponse (Prelude.Maybe Prelude.Text)
deleteDomainResponse_operationId = Lens.lens (\DeleteDomainResponse' {operationId} -> operationId) (\s@DeleteDomainResponse' {} a -> s {operationId = a} :: DeleteDomainResponse)

-- | The response's http status code.
deleteDomainResponse_httpStatus :: Lens.Lens' DeleteDomainResponse Prelude.Int
deleteDomainResponse_httpStatus = Lens.lens (\DeleteDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainResponse' {} a -> s {httpStatus = a} :: DeleteDomainResponse)

instance Prelude.NFData DeleteDomainResponse where
  rnf DeleteDomainResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
