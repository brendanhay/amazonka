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
-- Module      : Amazonka.CodeArtifact.PutDomainPermissionsPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a resource policy on a domain that specifies permissions to access
-- it.
--
-- When you call @PutDomainPermissionsPolicy@, the resource policy on the
-- domain is ignored when evaluting permissions. This ensures that the
-- owner of a domain cannot lock themselves out of the domain, which would
-- prevent them from being able to update the resource policy.
module Amazonka.CodeArtifact.PutDomainPermissionsPolicy
  ( -- * Creating a Request
    PutDomainPermissionsPolicy (..),
    newPutDomainPermissionsPolicy,

    -- * Request Lenses
    putDomainPermissionsPolicy_domainOwner,
    putDomainPermissionsPolicy_policyRevision,
    putDomainPermissionsPolicy_domain,
    putDomainPermissionsPolicy_policyDocument,

    -- * Destructuring the Response
    PutDomainPermissionsPolicyResponse (..),
    newPutDomainPermissionsPolicyResponse,

    -- * Response Lenses
    putDomainPermissionsPolicyResponse_policy,
    putDomainPermissionsPolicyResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDomainPermissionsPolicy' smart constructor.
data PutDomainPermissionsPolicy = PutDomainPermissionsPolicy'
  { -- | The 12-digit account number of the Amazon Web Services account that owns
    -- the domain. It does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The current revision of the resource policy to be set. This revision is
    -- used for optimistic locking, which prevents others from overwriting your
    -- changes to the domain\'s resource policy.
    policyRevision :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain on which to set the resource policy.
    domain :: Prelude.Text,
    -- | A valid displayable JSON Aspen policy string to be set as the access
    -- control resource policy on the provided domain.
    policyDocument :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDomainPermissionsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainOwner', 'putDomainPermissionsPolicy_domainOwner' - The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
--
-- 'policyRevision', 'putDomainPermissionsPolicy_policyRevision' - The current revision of the resource policy to be set. This revision is
-- used for optimistic locking, which prevents others from overwriting your
-- changes to the domain\'s resource policy.
--
-- 'domain', 'putDomainPermissionsPolicy_domain' - The name of the domain on which to set the resource policy.
--
-- 'policyDocument', 'putDomainPermissionsPolicy_policyDocument' - A valid displayable JSON Aspen policy string to be set as the access
-- control resource policy on the provided domain.
newPutDomainPermissionsPolicy ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'policyDocument'
  Prelude.Text ->
  PutDomainPermissionsPolicy
newPutDomainPermissionsPolicy
  pDomain_
  pPolicyDocument_ =
    PutDomainPermissionsPolicy'
      { domainOwner =
          Prelude.Nothing,
        policyRevision = Prelude.Nothing,
        domain = pDomain_,
        policyDocument = pPolicyDocument_
      }

-- | The 12-digit account number of the Amazon Web Services account that owns
-- the domain. It does not include dashes or spaces.
putDomainPermissionsPolicy_domainOwner :: Lens.Lens' PutDomainPermissionsPolicy (Prelude.Maybe Prelude.Text)
putDomainPermissionsPolicy_domainOwner = Lens.lens (\PutDomainPermissionsPolicy' {domainOwner} -> domainOwner) (\s@PutDomainPermissionsPolicy' {} a -> s {domainOwner = a} :: PutDomainPermissionsPolicy)

-- | The current revision of the resource policy to be set. This revision is
-- used for optimistic locking, which prevents others from overwriting your
-- changes to the domain\'s resource policy.
putDomainPermissionsPolicy_policyRevision :: Lens.Lens' PutDomainPermissionsPolicy (Prelude.Maybe Prelude.Text)
putDomainPermissionsPolicy_policyRevision = Lens.lens (\PutDomainPermissionsPolicy' {policyRevision} -> policyRevision) (\s@PutDomainPermissionsPolicy' {} a -> s {policyRevision = a} :: PutDomainPermissionsPolicy)

-- | The name of the domain on which to set the resource policy.
putDomainPermissionsPolicy_domain :: Lens.Lens' PutDomainPermissionsPolicy Prelude.Text
putDomainPermissionsPolicy_domain = Lens.lens (\PutDomainPermissionsPolicy' {domain} -> domain) (\s@PutDomainPermissionsPolicy' {} a -> s {domain = a} :: PutDomainPermissionsPolicy)

-- | A valid displayable JSON Aspen policy string to be set as the access
-- control resource policy on the provided domain.
putDomainPermissionsPolicy_policyDocument :: Lens.Lens' PutDomainPermissionsPolicy Prelude.Text
putDomainPermissionsPolicy_policyDocument = Lens.lens (\PutDomainPermissionsPolicy' {policyDocument} -> policyDocument) (\s@PutDomainPermissionsPolicy' {} a -> s {policyDocument = a} :: PutDomainPermissionsPolicy)

instance Core.AWSRequest PutDomainPermissionsPolicy where
  type
    AWSResponse PutDomainPermissionsPolicy =
      PutDomainPermissionsPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDomainPermissionsPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDomainPermissionsPolicy where
  hashWithSalt _salt PutDomainPermissionsPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` policyRevision
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` policyDocument

instance Prelude.NFData PutDomainPermissionsPolicy where
  rnf PutDomainPermissionsPolicy' {..} =
    Prelude.rnf domainOwner `Prelude.seq`
      Prelude.rnf policyRevision `Prelude.seq`
        Prelude.rnf domain `Prelude.seq`
          Prelude.rnf policyDocument

instance Data.ToHeaders PutDomainPermissionsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDomainPermissionsPolicy where
  toJSON PutDomainPermissionsPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("domainOwner" Data..=) Prelude.<$> domainOwner,
            ("policyRevision" Data..=)
              Prelude.<$> policyRevision,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just
              ("policyDocument" Data..= policyDocument)
          ]
      )

instance Data.ToPath PutDomainPermissionsPolicy where
  toPath =
    Prelude.const "/v1/domain/permissions/policy"

instance Data.ToQuery PutDomainPermissionsPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDomainPermissionsPolicyResponse' smart constructor.
data PutDomainPermissionsPolicyResponse = PutDomainPermissionsPolicyResponse'
  { -- | The resource policy that was set after processing the request.
    policy :: Prelude.Maybe ResourcePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDomainPermissionsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putDomainPermissionsPolicyResponse_policy' - The resource policy that was set after processing the request.
--
-- 'httpStatus', 'putDomainPermissionsPolicyResponse_httpStatus' - The response's http status code.
newPutDomainPermissionsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutDomainPermissionsPolicyResponse
newPutDomainPermissionsPolicyResponse pHttpStatus_ =
  PutDomainPermissionsPolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The resource policy that was set after processing the request.
putDomainPermissionsPolicyResponse_policy :: Lens.Lens' PutDomainPermissionsPolicyResponse (Prelude.Maybe ResourcePolicy)
putDomainPermissionsPolicyResponse_policy = Lens.lens (\PutDomainPermissionsPolicyResponse' {policy} -> policy) (\s@PutDomainPermissionsPolicyResponse' {} a -> s {policy = a} :: PutDomainPermissionsPolicyResponse)

-- | The response's http status code.
putDomainPermissionsPolicyResponse_httpStatus :: Lens.Lens' PutDomainPermissionsPolicyResponse Prelude.Int
putDomainPermissionsPolicyResponse_httpStatus = Lens.lens (\PutDomainPermissionsPolicyResponse' {httpStatus} -> httpStatus) (\s@PutDomainPermissionsPolicyResponse' {} a -> s {httpStatus = a} :: PutDomainPermissionsPolicyResponse)

instance
  Prelude.NFData
    PutDomainPermissionsPolicyResponse
  where
  rnf PutDomainPermissionsPolicyResponse' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf httpStatus
