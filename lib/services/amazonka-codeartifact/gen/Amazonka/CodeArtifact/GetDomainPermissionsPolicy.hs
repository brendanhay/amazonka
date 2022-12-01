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
-- Module      : Amazonka.CodeArtifact.GetDomainPermissionsPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource policy attached to the specified domain.
--
-- The policy is a resource-based policy, not an identity-based policy. For
-- more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_identity-vs-resource.html Identity-based policies and resource-based policies>
-- in the /IAM User Guide/.
module Amazonka.CodeArtifact.GetDomainPermissionsPolicy
  ( -- * Creating a Request
    GetDomainPermissionsPolicy (..),
    newGetDomainPermissionsPolicy,

    -- * Request Lenses
    getDomainPermissionsPolicy_domainOwner,
    getDomainPermissionsPolicy_domain,

    -- * Destructuring the Response
    GetDomainPermissionsPolicyResponse (..),
    newGetDomainPermissionsPolicyResponse,

    -- * Response Lenses
    getDomainPermissionsPolicyResponse_policy,
    getDomainPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDomainPermissionsPolicy' smart constructor.
data GetDomainPermissionsPolicy = GetDomainPermissionsPolicy'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain to which the resource policy is attached.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'getDomainPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'domain', 'getDomainPermissionsPolicy_domain' - The name of the domain to which the resource policy is attached.
newGetDomainPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  GetDomainPermissionsPolicy
newGetDomainPermissionsPolicy pDomain_ =
  GetDomainPermissionsPolicy'
    { domainOwner =
        Prelude.Nothing,
      domain = pDomain_
    }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
getDomainPermissionsPolicy_domainOwner :: Lens.Lens' GetDomainPermissionsPolicy (Prelude.Maybe Prelude.Text)
getDomainPermissionsPolicy_domainOwner = Lens.lens (\GetDomainPermissionsPolicy' {domainOwner} -> domainOwner) (\s@GetDomainPermissionsPolicy' {} a -> s {domainOwner = a} :: GetDomainPermissionsPolicy)

-- | The name of the domain to which the resource policy is attached.
getDomainPermissionsPolicy_domain :: Lens.Lens' GetDomainPermissionsPolicy Prelude.Text
getDomainPermissionsPolicy_domain = Lens.lens (\GetDomainPermissionsPolicy' {domain} -> domain) (\s@GetDomainPermissionsPolicy' {} a -> s {domain = a} :: GetDomainPermissionsPolicy)

instance Core.AWSRequest GetDomainPermissionsPolicy where
  type
    AWSResponse GetDomainPermissionsPolicy =
      GetDomainPermissionsPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainPermissionsPolicyResponse'
            Prelude.<$> (x Core..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainPermissionsPolicy where
  hashWithSalt _salt GetDomainPermissionsPolicy' {..} =
    _salt `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` domain

instance Prelude.NFData GetDomainPermissionsPolicy where
  rnf GetDomainPermissionsPolicy' {..} =
    Prelude.rnf domainOwner
      `Prelude.seq` Prelude.rnf domain

instance Core.ToHeaders GetDomainPermissionsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDomainPermissionsPolicy where
  toPath =
    Prelude.const "/v1/domain/permissions/policy"

instance Core.ToQuery GetDomainPermissionsPolicy where
  toQuery GetDomainPermissionsPolicy' {..} =
    Prelude.mconcat
      [ "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain
      ]

-- | /See:/ 'newGetDomainPermissionsPolicyResponse' smart constructor.
data GetDomainPermissionsPolicyResponse = GetDomainPermissionsPolicyResponse'
  { -- | The returned resource policy.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDomainPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getDomainPermissionsPolicyResponse_policy' - The returned resource policy.
--
-- 'httpStatus', 'getDomainPermissionsPolicyResponse_httpStatus' - The response's http status code.
newGetDomainPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainPermissionsPolicyResponse
newGetDomainPermissionsPolicyResponse pHttpStatus_ =
  GetDomainPermissionsPolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned resource policy.
getDomainPermissionsPolicyResponse_policy :: Lens.Lens' GetDomainPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
getDomainPermissionsPolicyResponse_policy = Lens.lens (\GetDomainPermissionsPolicyResponse' {policy} -> policy) (\s@GetDomainPermissionsPolicyResponse' {} a -> s {policy = a} :: GetDomainPermissionsPolicyResponse)

-- | The response's http status code.
getDomainPermissionsPolicyResponse_httpStatus :: Lens.Lens' GetDomainPermissionsPolicyResponse Prelude.Int
getDomainPermissionsPolicyResponse_httpStatus = Lens.lens (\GetDomainPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@GetDomainPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: GetDomainPermissionsPolicyResponse)

instance
  Prelude.NFData
    GetDomainPermissionsPolicyResponse
  where
  rnf GetDomainPermissionsPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
