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
-- Module      : Amazonka.CodeArtifact.DeleteDomainPermissionsPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource policy set on a domain.
module Amazonka.CodeArtifact.DeleteDomainPermissionsPolicy
  ( -- * Creating a Request
    DeleteDomainPermissionsPolicy (..),
    newDeleteDomainPermissionsPolicy,

    -- * Request Lenses
    deleteDomainPermissionsPolicy_domainOwner,
    deleteDomainPermissionsPolicy_policyRevision,
    deleteDomainPermissionsPolicy_domain,

    -- * Destructuring the Response
    DeleteDomainPermissionsPolicyResponse (..),
    newDeleteDomainPermissionsPolicyResponse,

    -- * Response Lenses
    deleteDomainPermissionsPolicyResponse_policy,
    deleteDomainPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteDomainPermissionsPolicy' smart constructor.
data DeleteDomainPermissionsPolicy = DeleteDomainPermissionsPolicy'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The current revision of the resource policy to be deleted. This revision
    -- is used for optimistic locking, which prevents others from overwriting
    -- your changes to the domain\'s resource policy.
    policyRevision :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain associated with the resource policy to be
    -- deleted.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'deleteDomainPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'policyRevision', 'deleteDomainPermissionsPolicy_policyRevision' - The current revision of the resource policy to be deleted. This revision
-- is used for optimistic locking, which prevents others from overwriting
-- your changes to the domain\'s resource policy.
--
-- 'domain', 'deleteDomainPermissionsPolicy_domain' - The name of the domain associated with the resource policy to be
-- deleted.
newDeleteDomainPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  DeleteDomainPermissionsPolicy
newDeleteDomainPermissionsPolicy pDomain_ =
  DeleteDomainPermissionsPolicy'
    { domainOwner =
        Prelude.Nothing,
      policyRevision = Prelude.Nothing,
      domain = pDomain_
    }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deleteDomainPermissionsPolicy_domainOwner :: Lens.Lens' DeleteDomainPermissionsPolicy (Prelude.Maybe Prelude.Text)
deleteDomainPermissionsPolicy_domainOwner = Lens.lens (\DeleteDomainPermissionsPolicy' {domainOwner} -> domainOwner) (\s@DeleteDomainPermissionsPolicy' {} a -> s {domainOwner = a} :: DeleteDomainPermissionsPolicy)

-- | The current revision of the resource policy to be deleted. This revision
-- is used for optimistic locking, which prevents others from overwriting
-- your changes to the domain\'s resource policy.
deleteDomainPermissionsPolicy_policyRevision :: Lens.Lens' DeleteDomainPermissionsPolicy (Prelude.Maybe Prelude.Text)
deleteDomainPermissionsPolicy_policyRevision = Lens.lens (\DeleteDomainPermissionsPolicy' {policyRevision} -> policyRevision) (\s@DeleteDomainPermissionsPolicy' {} a -> s {policyRevision = a} :: DeleteDomainPermissionsPolicy)

-- | The name of the domain associated with the resource policy to be
-- deleted.
deleteDomainPermissionsPolicy_domain :: Lens.Lens' DeleteDomainPermissionsPolicy Prelude.Text
deleteDomainPermissionsPolicy_domain = Lens.lens (\DeleteDomainPermissionsPolicy' {domain} -> domain) (\s@DeleteDomainPermissionsPolicy' {} a -> s {domain = a} :: DeleteDomainPermissionsPolicy)

instance
  Core.AWSRequest
    DeleteDomainPermissionsPolicy
  where
  type
    AWSResponse DeleteDomainPermissionsPolicy =
      DeleteDomainPermissionsPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDomainPermissionsPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteDomainPermissionsPolicy
  where
  hashWithSalt _salt DeleteDomainPermissionsPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` policyRevision
      `Prelude.hashWithSalt` domain

instance Prelude.NFData DeleteDomainPermissionsPolicy where
  rnf DeleteDomainPermissionsPolicy' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf policyRevision
      `Prelude.seq` Prelude.rnf domain

instance Data.ToHeaders DeleteDomainPermissionsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteDomainPermissionsPolicy where
  toPath =
    Prelude.const "/v1/domain/permissions/policy"

instance Data.ToQuery DeleteDomainPermissionsPolicy where
  toQuery DeleteDomainPermissionsPolicy' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "policy-revision" Data.=: policyRevision,
        "domain" Data.=: domain
      ]

-- | /See:/ 'newDeleteDomainPermissionsPolicyResponse' smart constructor.
data DeleteDomainPermissionsPolicyResponse = DeleteDomainPermissionsPolicyResponse'
  { -- | Information about the deleted resource policy after processing the
    -- request.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDomainPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'deleteDomainPermissionsPolicyResponse_policy' - Information about the deleted resource policy after processing the
-- request.
--
-- 'httpStatus', 'deleteDomainPermissionsPolicyResponse_httpStatus' - The response's http status code.
newDeleteDomainPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteDomainPermissionsPolicyResponse
newDeleteDomainPermissionsPolicyResponse pHttpStatus_ =
  DeleteDomainPermissionsPolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the deleted resource policy after processing the
-- request.
deleteDomainPermissionsPolicyResponse_policy :: Lens.Lens' DeleteDomainPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
deleteDomainPermissionsPolicyResponse_policy = Lens.lens (\DeleteDomainPermissionsPolicyResponse' {policy} -> policy) (\s@DeleteDomainPermissionsPolicyResponse' {} a -> s {policy = a} :: DeleteDomainPermissionsPolicyResponse)

-- | The response's http status code.
deleteDomainPermissionsPolicyResponse_httpStatus :: Lens.Lens' DeleteDomainPermissionsPolicyResponse Prelude.Int
deleteDomainPermissionsPolicyResponse_httpStatus = Lens.lens (\DeleteDomainPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteDomainPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: DeleteDomainPermissionsPolicyResponse)

instance
  Prelude.NFData
    DeleteDomainPermissionsPolicyResponse
  where
  rnf DeleteDomainPermissionsPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
