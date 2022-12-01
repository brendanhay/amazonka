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
-- Module      : Amazonka.CodeArtifact.PutRepositoryPermissionsPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the resource policy on a repository that specifies permissions to
-- access it.
--
-- When you call @PutRepositoryPermissionsPolicy@, the resource policy on
-- the repository is ignored when evaluting permissions. This ensures that
-- the owner of a repository cannot lock themselves out of the repository,
-- which would prevent them from being able to update the resource policy.
module Amazonka.CodeArtifact.PutRepositoryPermissionsPolicy
  ( -- * Creating a Request
    PutRepositoryPermissionsPolicy (..),
    newPutRepositoryPermissionsPolicy,

    -- * Request Lenses
    putRepositoryPermissionsPolicy_policyRevision,
    putRepositoryPermissionsPolicy_domainOwner,
    putRepositoryPermissionsPolicy_domain,
    putRepositoryPermissionsPolicy_repository,
    putRepositoryPermissionsPolicy_policyDocument,

    -- * Destructuring the Response
    PutRepositoryPermissionsPolicyResponse (..),
    newPutRepositoryPermissionsPolicyResponse,

    -- * Response Lenses
    putRepositoryPermissionsPolicyResponse_policy,
    putRepositoryPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRepositoryPermissionsPolicy' smart constructor.
data PutRepositoryPermissionsPolicy = PutRepositoryPermissionsPolicy'
  { -- | Sets the revision of the resource policy that specifies permissions to
    -- access the repository. This revision is used for optimistic locking,
    -- which prevents others from overwriting your changes to the repository\'s
    -- resource policy.
    policyRevision :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain containing the repository to set the resource
    -- policy on.
    domain :: Prelude.Text,
    -- | The name of the repository to set the resource policy on.
    repository :: Prelude.Text,
    -- | A valid displayable JSON Aspen policy string to be set as the access
    -- control resource policy on the provided repository.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRepositoryPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyRevision', 'putRepositoryPermissionsPolicy_policyRevision' - Sets the revision of the resource policy that specifies permissions to
-- access the repository. This revision is used for optimistic locking,
-- which prevents others from overwriting your changes to the repository\'s
-- resource policy.
--
-- 'domainOwner', 'putRepositoryPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'putRepositoryPermissionsPolicy_domain' - The name of the domain containing the repository to set the resource
-- policy on.
--
-- 'repository', 'putRepositoryPermissionsPolicy_repository' - The name of the repository to set the resource policy on.
--
-- 'policyDocument', 'putRepositoryPermissionsPolicy_policyDocument' - A valid displayable JSON Aspen policy string to be set as the access
-- control resource policy on the provided repository.
newPutRepositoryPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutRepositoryPermissionsPolicy
newPutRepositoryPermissionsPolicy
  pDomain_
  pRepository_
  pPolicyDocument_ =
    PutRepositoryPermissionsPolicy'
      { policyRevision =
          Prelude.Nothing,
        domainOwner = Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_,
        policyDocument = pPolicyDocument_
      }

-- | Sets the revision of the resource policy that specifies permissions to
-- access the repository. This revision is used for optimistic locking,
-- which prevents others from overwriting your changes to the repository\'s
-- resource policy.
putRepositoryPermissionsPolicy_policyRevision :: Lens.Lens' PutRepositoryPermissionsPolicy (Prelude.Maybe Prelude.Text)
putRepositoryPermissionsPolicy_policyRevision = Lens.lens (\PutRepositoryPermissionsPolicy' {policyRevision} -> policyRevision) (\s@PutRepositoryPermissionsPolicy' {} a -> s {policyRevision = a} :: PutRepositoryPermissionsPolicy)

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
putRepositoryPermissionsPolicy_domainOwner :: Lens.Lens' PutRepositoryPermissionsPolicy (Prelude.Maybe Prelude.Text)
putRepositoryPermissionsPolicy_domainOwner = Lens.lens (\PutRepositoryPermissionsPolicy' {domainOwner} -> domainOwner) (\s@PutRepositoryPermissionsPolicy' {} a -> s {domainOwner = a} :: PutRepositoryPermissionsPolicy)

-- | The name of the domain containing the repository to set the resource
-- policy on.
putRepositoryPermissionsPolicy_domain :: Lens.Lens' PutRepositoryPermissionsPolicy Prelude.Text
putRepositoryPermissionsPolicy_domain = Lens.lens (\PutRepositoryPermissionsPolicy' {domain} -> domain) (\s@PutRepositoryPermissionsPolicy' {} a -> s {domain = a} :: PutRepositoryPermissionsPolicy)

-- | The name of the repository to set the resource policy on.
putRepositoryPermissionsPolicy_repository :: Lens.Lens' PutRepositoryPermissionsPolicy Prelude.Text
putRepositoryPermissionsPolicy_repository = Lens.lens (\PutRepositoryPermissionsPolicy' {repository} -> repository) (\s@PutRepositoryPermissionsPolicy' {} a -> s {repository = a} :: PutRepositoryPermissionsPolicy)

-- | A valid displayable JSON Aspen policy string to be set as the access
-- control resource policy on the provided repository.
putRepositoryPermissionsPolicy_policyDocument :: Lens.Lens' PutRepositoryPermissionsPolicy Prelude.Text
putRepositoryPermissionsPolicy_policyDocument = Lens.lens (\PutRepositoryPermissionsPolicy' {policyDocument} -> policyDocument) (\s@PutRepositoryPermissionsPolicy' {} a -> s {policyDocument = a} :: PutRepositoryPermissionsPolicy)

instance
  Core.AWSRequest
    PutRepositoryPermissionsPolicy
  where
  type
    AWSResponse PutRepositoryPermissionsPolicy =
      PutRepositoryPermissionsPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRepositoryPermissionsPolicyResponse'
            Prelude.<$> (x Core..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutRepositoryPermissionsPolicy
  where
  hashWithSalt
    _salt
    PutRepositoryPermissionsPolicy' {..} =
      _salt `Prelude.hashWithSalt` policyRevision
        `Prelude.hashWithSalt` domainOwner
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` repository
        `Prelude.hashWithSalt` policyDocument

instance
  Prelude.NFData
    PutRepositoryPermissionsPolicy
  where
  rnf PutRepositoryPermissionsPolicy' {..} =
    Prelude.rnf policyRevision
      `Prelude.seq` Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf policyDocument

instance
  Core.ToHeaders
    PutRepositoryPermissionsPolicy
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutRepositoryPermissionsPolicy where
  toJSON PutRepositoryPermissionsPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("policyRevision" Core..=)
              Prelude.<$> policyRevision,
            Prelude.Just
              ("policyDocument" Core..= policyDocument)
          ]
      )

instance Core.ToPath PutRepositoryPermissionsPolicy where
  toPath =
    Prelude.const "/v1/repository/permissions/policy"

instance Core.ToQuery PutRepositoryPermissionsPolicy where
  toQuery PutRepositoryPermissionsPolicy' {..} =
    Prelude.mconcat
      [ "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "repository" Core.=: repository
      ]

-- | /See:/ 'newPutRepositoryPermissionsPolicyResponse' smart constructor.
data PutRepositoryPermissionsPolicyResponse = PutRepositoryPermissionsPolicyResponse'
  { -- | The resource policy that was set after processing the request.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRepositoryPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putRepositoryPermissionsPolicyResponse_policy' - The resource policy that was set after processing the request.
--
-- 'httpStatus', 'putRepositoryPermissionsPolicyResponse_httpStatus' - The response's http status code.
newPutRepositoryPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRepositoryPermissionsPolicyResponse
newPutRepositoryPermissionsPolicyResponse
  pHttpStatus_ =
    PutRepositoryPermissionsPolicyResponse'
      { policy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The resource policy that was set after processing the request.
putRepositoryPermissionsPolicyResponse_policy :: Lens.Lens' PutRepositoryPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
putRepositoryPermissionsPolicyResponse_policy = Lens.lens (\PutRepositoryPermissionsPolicyResponse' {policy} -> policy) (\s@PutRepositoryPermissionsPolicyResponse' {} a -> s {policy = a} :: PutRepositoryPermissionsPolicyResponse)

-- | The response's http status code.
putRepositoryPermissionsPolicyResponse_httpStatus :: Lens.Lens' PutRepositoryPermissionsPolicyResponse Prelude.Int
putRepositoryPermissionsPolicyResponse_httpStatus = Lens.lens (\PutRepositoryPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRepositoryPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: PutRepositoryPermissionsPolicyResponse)

instance
  Prelude.NFData
    PutRepositoryPermissionsPolicyResponse
  where
  rnf PutRepositoryPermissionsPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
