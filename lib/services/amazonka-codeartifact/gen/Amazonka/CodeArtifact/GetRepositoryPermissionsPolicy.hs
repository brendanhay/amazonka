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
-- Module      : Amazonka.CodeArtifact.GetRepositoryPermissionsPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource policy that is set on a repository.
module Amazonka.CodeArtifact.GetRepositoryPermissionsPolicy
  ( -- * Creating a Request
    GetRepositoryPermissionsPolicy (..),
    newGetRepositoryPermissionsPolicy,

    -- * Request Lenses
    getRepositoryPermissionsPolicy_domainOwner,
    getRepositoryPermissionsPolicy_domain,
    getRepositoryPermissionsPolicy_repository,

    -- * Destructuring the Response
    GetRepositoryPermissionsPolicyResponse (..),
    newGetRepositoryPermissionsPolicyResponse,

    -- * Response Lenses
    getRepositoryPermissionsPolicyResponse_policy,
    getRepositoryPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRepositoryPermissionsPolicy' smart constructor.
data GetRepositoryPermissionsPolicy = GetRepositoryPermissionsPolicy'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain containing the repository whose associated
    -- resource policy is to be retrieved.
    domain :: Prelude.Text,
    -- | The name of the repository whose associated resource policy is to be
    -- retrieved.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'getRepositoryPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'getRepositoryPermissionsPolicy_domain' - The name of the domain containing the repository whose associated
-- resource policy is to be retrieved.
--
-- 'repository', 'getRepositoryPermissionsPolicy_repository' - The name of the repository whose associated resource policy is to be
-- retrieved.
newGetRepositoryPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  GetRepositoryPermissionsPolicy
newGetRepositoryPermissionsPolicy
  pDomain_
  pRepository_ =
    GetRepositoryPermissionsPolicy'
      { domainOwner =
          Prelude.Nothing,
        domain = pDomain_,
        repository = pRepository_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
getRepositoryPermissionsPolicy_domainOwner :: Lens.Lens' GetRepositoryPermissionsPolicy (Prelude.Maybe Prelude.Text)
getRepositoryPermissionsPolicy_domainOwner = Lens.lens (\GetRepositoryPermissionsPolicy' {domainOwner} -> domainOwner) (\s@GetRepositoryPermissionsPolicy' {} a -> s {domainOwner = a} :: GetRepositoryPermissionsPolicy)

-- | The name of the domain containing the repository whose associated
-- resource policy is to be retrieved.
getRepositoryPermissionsPolicy_domain :: Lens.Lens' GetRepositoryPermissionsPolicy Prelude.Text
getRepositoryPermissionsPolicy_domain = Lens.lens (\GetRepositoryPermissionsPolicy' {domain} -> domain) (\s@GetRepositoryPermissionsPolicy' {} a -> s {domain = a} :: GetRepositoryPermissionsPolicy)

-- | The name of the repository whose associated resource policy is to be
-- retrieved.
getRepositoryPermissionsPolicy_repository :: Lens.Lens' GetRepositoryPermissionsPolicy Prelude.Text
getRepositoryPermissionsPolicy_repository = Lens.lens (\GetRepositoryPermissionsPolicy' {repository} -> repository) (\s@GetRepositoryPermissionsPolicy' {} a -> s {repository = a} :: GetRepositoryPermissionsPolicy)

instance
  Core.AWSRequest
    GetRepositoryPermissionsPolicy
  where
  type
    AWSResponse GetRepositoryPermissionsPolicy =
      GetRepositoryPermissionsPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryPermissionsPolicyResponse'
            Prelude.<$> (x Core..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRepositoryPermissionsPolicy
  where
  hashWithSalt
    _salt
    GetRepositoryPermissionsPolicy' {..} =
      _salt `Prelude.hashWithSalt` domainOwner
        `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` repository

instance
  Prelude.NFData
    GetRepositoryPermissionsPolicy
  where
  rnf GetRepositoryPermissionsPolicy' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf repository

instance
  Core.ToHeaders
    GetRepositoryPermissionsPolicy
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

instance Core.ToPath GetRepositoryPermissionsPolicy where
  toPath =
    Prelude.const "/v1/repository/permissions/policy"

instance Core.ToQuery GetRepositoryPermissionsPolicy where
  toQuery GetRepositoryPermissionsPolicy' {..} =
    Prelude.mconcat
      [ "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "repository" Core.=: repository
      ]

-- | /See:/ 'newGetRepositoryPermissionsPolicyResponse' smart constructor.
data GetRepositoryPermissionsPolicyResponse = GetRepositoryPermissionsPolicyResponse'
  { -- | The returned resource policy.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getRepositoryPermissionsPolicyResponse_policy' - The returned resource policy.
--
-- 'httpStatus', 'getRepositoryPermissionsPolicyResponse_httpStatus' - The response's http status code.
newGetRepositoryPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRepositoryPermissionsPolicyResponse
newGetRepositoryPermissionsPolicyResponse
  pHttpStatus_ =
    GetRepositoryPermissionsPolicyResponse'
      { policy =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The returned resource policy.
getRepositoryPermissionsPolicyResponse_policy :: Lens.Lens' GetRepositoryPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
getRepositoryPermissionsPolicyResponse_policy = Lens.lens (\GetRepositoryPermissionsPolicyResponse' {policy} -> policy) (\s@GetRepositoryPermissionsPolicyResponse' {} a -> s {policy = a} :: GetRepositoryPermissionsPolicyResponse)

-- | The response's http status code.
getRepositoryPermissionsPolicyResponse_httpStatus :: Lens.Lens' GetRepositoryPermissionsPolicyResponse Prelude.Int
getRepositoryPermissionsPolicyResponse_httpStatus = Lens.lens (\GetRepositoryPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: GetRepositoryPermissionsPolicyResponse)

instance
  Prelude.NFData
    GetRepositoryPermissionsPolicyResponse
  where
  rnf GetRepositoryPermissionsPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
