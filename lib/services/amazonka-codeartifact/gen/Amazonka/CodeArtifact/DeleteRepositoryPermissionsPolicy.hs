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
-- Module      : Amazonka.CodeArtifact.DeleteRepositoryPermissionsPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource policy that is set on a repository. After a
-- resource policy is deleted, the permissions allowed and denied by the
-- deleted policy are removed. The effect of deleting a resource policy
-- might not be immediate.
--
-- Use @DeleteRepositoryPermissionsPolicy@ with caution. After a policy is
-- deleted, Amazon Web Services users, roles, and accounts lose permissions
-- to perform the repository actions granted by the deleted policy.
module Amazonka.CodeArtifact.DeleteRepositoryPermissionsPolicy
  ( -- * Creating a Request
    DeleteRepositoryPermissionsPolicy (..),
    newDeleteRepositoryPermissionsPolicy,

    -- * Request Lenses
    deleteRepositoryPermissionsPolicy_domainOwner,
    deleteRepositoryPermissionsPolicy_policyRevision,
    deleteRepositoryPermissionsPolicy_domain,
    deleteRepositoryPermissionsPolicy_repository,

    -- * Destructuring the Response
    DeleteRepositoryPermissionsPolicyResponse (..),
    newDeleteRepositoryPermissionsPolicyResponse,

    -- * Response Lenses
    deleteRepositoryPermissionsPolicyResponse_policy,
    deleteRepositoryPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRepositoryPermissionsPolicy' smart constructor.
data DeleteRepositoryPermissionsPolicy = DeleteRepositoryPermissionsPolicy'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The revision of the repository\'s resource policy to be deleted. This
    -- revision is used for optimistic locking, which prevents others from
    -- accidentally overwriting your changes to the repository\'s resource
    -- policy.
    policyRevision :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository associated with the
    -- resource policy to be deleted.
    domain :: Prelude.Text,
    -- | The name of the repository that is associated with the resource policy
    -- to be deleted
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'deleteRepositoryPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'policyRevision', 'deleteRepositoryPermissionsPolicy_policyRevision' - The revision of the repository\'s resource policy to be deleted. This
-- revision is used for optimistic locking, which prevents others from
-- accidentally overwriting your changes to the repository\'s resource
-- policy.
--
-- 'domain', 'deleteRepositoryPermissionsPolicy_domain' - The name of the domain that contains the repository associated with the
-- resource policy to be deleted.
--
-- 'repository', 'deleteRepositoryPermissionsPolicy_repository' - The name of the repository that is associated with the resource policy
-- to be deleted
newDeleteRepositoryPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  DeleteRepositoryPermissionsPolicy
newDeleteRepositoryPermissionsPolicy
  pDomain_
  pRepository_ =
    DeleteRepositoryPermissionsPolicy'
      { domainOwner =
          Prelude.Nothing,
        policyRevision = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
deleteRepositoryPermissionsPolicy_domainOwner :: Lens.Lens' DeleteRepositoryPermissionsPolicy (Prelude.Maybe Prelude.Text)
deleteRepositoryPermissionsPolicy_domainOwner = Lens.lens (\DeleteRepositoryPermissionsPolicy' {domainOwner} -> domainOwner) (\s@DeleteRepositoryPermissionsPolicy' {} a -> s {domainOwner = a} :: DeleteRepositoryPermissionsPolicy)

-- | The revision of the repository\'s resource policy to be deleted. This
-- revision is used for optimistic locking, which prevents others from
-- accidentally overwriting your changes to the repository\'s resource
-- policy.
deleteRepositoryPermissionsPolicy_policyRevision :: Lens.Lens' DeleteRepositoryPermissionsPolicy (Prelude.Maybe Prelude.Text)
deleteRepositoryPermissionsPolicy_policyRevision = Lens.lens (\DeleteRepositoryPermissionsPolicy' {policyRevision} -> policyRevision) (\s@DeleteRepositoryPermissionsPolicy' {} a -> s {policyRevision = a} :: DeleteRepositoryPermissionsPolicy)

-- | The name of the domain that contains the repository associated with the
-- resource policy to be deleted.
deleteRepositoryPermissionsPolicy_domain :: Lens.Lens' DeleteRepositoryPermissionsPolicy Prelude.Text
deleteRepositoryPermissionsPolicy_domain = Lens.lens (\DeleteRepositoryPermissionsPolicy' {domain} -> domain) (\s@DeleteRepositoryPermissionsPolicy' {} a -> s {domain = a} :: DeleteRepositoryPermissionsPolicy)

-- | The name of the repository that is associated with the resource policy
-- to be deleted
deleteRepositoryPermissionsPolicy_repository :: Lens.Lens' DeleteRepositoryPermissionsPolicy Prelude.Text
deleteRepositoryPermissionsPolicy_repository = Lens.lens (\DeleteRepositoryPermissionsPolicy' {repository} -> repository) (\s@DeleteRepositoryPermissionsPolicy' {} a -> s {repository = a} :: DeleteRepositoryPermissionsPolicy)

instance
  Core.AWSRequest
    DeleteRepositoryPermissionsPolicy
  where
  type
    AWSResponse DeleteRepositoryPermissionsPolicy =
      DeleteRepositoryPermissionsPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRepositoryPermissionsPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteRepositoryPermissionsPolicy
  where
  hashWithSalt
    _salt
    DeleteRepositoryPermissionsPolicy' {..} =
      _salt `Prelude.hashWithSalt` domainOwner
        `Prelude.hashWithSalt` policyRevision
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` repository

instance
  Prelude.NFData
    DeleteRepositoryPermissionsPolicy
  where
  rnf DeleteRepositoryPermissionsPolicy' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf policyRevision
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance
  Data.ToHeaders
    DeleteRepositoryPermissionsPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteRepositoryPermissionsPolicy
  where
  toPath =
    Prelude.const "/v1/repository/permissions/policies"

instance
  Data.ToQuery
    DeleteRepositoryPermissionsPolicy
  where
  toQuery DeleteRepositoryPermissionsPolicy' {..} =
    Prelude.mconcat
      [ "domain-owner" Data.=: domainOwner,
        "policy-revision" Data.=: policyRevision,
        "domain" Data.=: domain,
        "repository" Data.=: repository
      ]

-- | /See:/ 'newDeleteRepositoryPermissionsPolicyResponse' smart constructor.
data DeleteRepositoryPermissionsPolicyResponse = DeleteRepositoryPermissionsPolicyResponse'
  { -- | Information about the deleted policy after processing the request.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRepositoryPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'deleteRepositoryPermissionsPolicyResponse_policy' - Information about the deleted policy after processing the request.
--
-- 'httpStatus', 'deleteRepositoryPermissionsPolicyResponse_httpStatus' - The response's http status code.
newDeleteRepositoryPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRepositoryPermissionsPolicyResponse
newDeleteRepositoryPermissionsPolicyResponse
  pHttpStatus_ =
    DeleteRepositoryPermissionsPolicyResponse'
      { policy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted policy after processing the request.
deleteRepositoryPermissionsPolicyResponse_policy :: Lens.Lens' DeleteRepositoryPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
deleteRepositoryPermissionsPolicyResponse_policy = Lens.lens (\DeleteRepositoryPermissionsPolicyResponse' {policy} -> policy) (\s@DeleteRepositoryPermissionsPolicyResponse' {} a -> s {policy = a} :: DeleteRepositoryPermissionsPolicyResponse)

-- | The response's http status code.
deleteRepositoryPermissionsPolicyResponse_httpStatus :: Lens.Lens' DeleteRepositoryPermissionsPolicyResponse Prelude.Int
deleteRepositoryPermissionsPolicyResponse_httpStatus = Lens.lens (\DeleteRepositoryPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteRepositoryPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: DeleteRepositoryPermissionsPolicyResponse)

instance
  Prelude.NFData
    DeleteRepositoryPermissionsPolicyResponse
  where
  rnf DeleteRepositoryPermissionsPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
