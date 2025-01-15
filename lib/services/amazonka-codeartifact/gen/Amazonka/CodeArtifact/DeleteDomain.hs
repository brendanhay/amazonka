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
-- Module      : Amazonka.CodeArtifact.DeleteDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a domain. You cannot delete a domain that contains repositories.
-- If you want to delete a domain with repositories, first delete its
-- repositories.
module Amazonka.CodeArtifact.DeleteDomain
  ( -- * Creating a Request
    DeleteDomain (..),
    newDeleteDomain,

    -- * Request Lenses
    deleteDomain_domainOwner,
    deleteDomain_domain,

    -- * Destructuring the Response
    DeleteDomainResponse (..),
    newDeleteDomainResponse,

    -- * Response Lenses
    deleteDomainResponse_domain,
    deleteDomainResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDomain' smart constructor.
data DeleteDomain = DeleteDomain'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain to delete.
    domain :: Prelude.Text
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
-- 'domainOwner', 'deleteDomain_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'deleteDomain_domain' - The name of the domain to delete.
newDeleteDomain ::
  -- | 'domain'
  Prelude.Text ->
  DeleteDomain
newDeleteDomain pDomain_ =
  DeleteDomain'
    { domainOwner = Prelude.Nothing,
      domain = pDomain_
    }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deleteDomain_domainOwner :: Lens.Lens' DeleteDomain (Prelude.Maybe Prelude.Text)
deleteDomain_domainOwner = Lens.lens (\DeleteDomain' {domainOwner} -> domainOwner) (\s@DeleteDomain' {} a -> s {domainOwner = a} :: DeleteDomain)

-- | The name of the domain to delete.
deleteDomain_domain :: Lens.Lens' DeleteDomain Prelude.Text
deleteDomain_domain = Lens.lens (\DeleteDomain' {domain} -> domain) (\s@DeleteDomain' {} a -> s {domain = a} :: DeleteDomain)

instance Core.AWSRequest DeleteDomain where
  type AWSResponse DeleteDomain = DeleteDomainResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainResponse'
            Prelude.<$> (x Data..?> "domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteDomain where
  hashWithSalt _salt DeleteDomain' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain

instance Prelude.NFData DeleteDomain where
  rnf DeleteDomain' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf domain

instance Data.ToHeaders DeleteDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDomain where
  toPath = Prelude.const "/v1/domain"

instance Data.ToQuery DeleteDomain where
  toQuery DeleteDomain' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "domain" Data.=: domain
      ]

-- | /See:/ 'newDeleteDomainResponse' smart constructor.
data DeleteDomainResponse = DeleteDomainResponse'
  { -- | Contains information about the deleted domain after processing the
    -- request.
    domain :: Prelude.Maybe DomainDescription,
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
-- 'domain', 'deleteDomainResponse_domain' - Contains information about the deleted domain after processing the
-- request.
--
-- 'httpStatus', 'deleteDomainResponse_httpStatus' - The response's http status code.
newDeleteDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainResponse
newDeleteDomainResponse pHttpStatus_ =
  DeleteDomainResponse'
    { domain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the deleted domain after processing the
-- request.
deleteDomainResponse_domain :: Lens.Lens' DeleteDomainResponse (Prelude.Maybe DomainDescription)
deleteDomainResponse_domain = Lens.lens (\DeleteDomainResponse' {domain} -> domain) (\s@DeleteDomainResponse' {} a -> s {domain = a} :: DeleteDomainResponse)

-- | The response's http status code.
deleteDomainResponse_httpStatus :: Lens.Lens' DeleteDomainResponse Prelude.Int
deleteDomainResponse_httpStatus = Lens.lens (\DeleteDomainResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainResponse' {} a -> s {httpStatus = a} :: DeleteDomainResponse)

instance Prelude.NFData DeleteDomainResponse where
  rnf DeleteDomainResponse' {..} =
    Prelude.rnf domain `Prelude.seq`
      Prelude.rnf httpStatus
