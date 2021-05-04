{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManagerPCA.PutPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based policy to a private CA.
--
-- A policy can also be applied by sharing a private CA through AWS
-- Resource Access Manager (RAM). For more information, see
-- <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html Attach a Policy for Cross-Account Access>.
--
-- The policy can be displayed with
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetPolicy.html GetPolicy>
-- and removed with
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_DeletePolicy.html DeletePolicy>.
--
-- __About Policies__
--
-- -   A policy grants access on a private CA to an AWS customer account,
--     to AWS Organizations, or to an AWS Organizations unit. Policies are
--     under the control of a CA administrator. For more information, see
--     <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-rbp.html Using a Resource Based Policy with ACM Private CA>.
--
-- -   A policy permits a user of AWS Certificate Manager (ACM) to issue
--     ACM certificates signed by a CA in another account.
--
-- -   For ACM to manage automatic renewal of these certificates, the ACM
--     user must configure a Service Linked Role (SLR). The SLR allows the
--     ACM service to assume the identity of the user, subject to
--     confirmation against the ACM Private CA policy. For more
--     information, see
--     <https://docs.aws.amazon.com/acm/latest/userguide/acm-slr.html Using a Service Linked Role with ACM>.
--
-- -   Updates made in AWS Resource Manager (RAM) are reflected in
--     policies. For more information, see
--     <https://docs.aws.amazon.com/acm-pca/latest/userguide/pca-ram.html Attach a Policy for Cross-Account Access>.
module Network.AWS.CertificateManagerPCA.PutPolicy
  ( -- * Creating a Request
    PutPolicy (..),
    newPutPolicy,

    -- * Request Lenses
    putPolicy_resourceArn,
    putPolicy_policy,

    -- * Destructuring the Response
    PutPolicyResponse (..),
    newPutPolicyResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | The Amazon Resource Number (ARN) of the private CA to associate with the
    -- policy. The ARN of the CA can be found by calling the
    -- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
    -- action.
    resourceArn :: Prelude.Text,
    -- | The path and file name of a JSON-formatted IAM policy to attach to the
    -- specified private CA resource. If this policy does not contain all
    -- required statements or if it includes any statement that is not allowed,
    -- the @PutPolicy@ action returns an @InvalidPolicyException@. For
    -- information about IAM policy and statement structure, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putPolicy_resourceArn' - The Amazon Resource Number (ARN) of the private CA to associate with the
-- policy. The ARN of the CA can be found by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action.
--
-- 'policy', 'putPolicy_policy' - The path and file name of a JSON-formatted IAM policy to attach to the
-- specified private CA resource. If this policy does not contain all
-- required statements or if it includes any statement that is not allowed,
-- the @PutPolicy@ action returns an @InvalidPolicyException@. For
-- information about IAM policy and statement structure, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
newPutPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutPolicy
newPutPolicy pResourceArn_ pPolicy_ =
  PutPolicy'
    { resourceArn = pResourceArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Number (ARN) of the private CA to associate with the
-- policy. The ARN of the CA can be found by calling the
-- <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ListCertificateAuthorities.html ListCertificateAuthorities>
-- action.
putPolicy_resourceArn :: Lens.Lens' PutPolicy Prelude.Text
putPolicy_resourceArn = Lens.lens (\PutPolicy' {resourceArn} -> resourceArn) (\s@PutPolicy' {} a -> s {resourceArn = a} :: PutPolicy)

-- | The path and file name of a JSON-formatted IAM policy to attach to the
-- specified private CA resource. If this policy does not contain all
-- required statements or if it includes any statement that is not allowed,
-- the @PutPolicy@ action returns an @InvalidPolicyException@. For
-- information about IAM policy and statement structure, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies.html#access_policies-json Overview of JSON Policies>.
putPolicy_policy :: Lens.Lens' PutPolicy Prelude.Text
putPolicy_policy = Lens.lens (\PutPolicy' {policy} -> policy) (\s@PutPolicy' {} a -> s {policy = a} :: PutPolicy)

instance Prelude.AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull PutPolicyResponse'

instance Prelude.Hashable PutPolicy

instance Prelude.NFData PutPolicy

instance Prelude.ToHeaders PutPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("ACMPrivateCA.PutPolicy" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Prelude..= resourceArn),
            Prelude.Just ("Policy" Prelude..= policy)
          ]
      )

instance Prelude.ToPath PutPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutPolicyResponse ::
  PutPolicyResponse
newPutPolicyResponse = PutPolicyResponse'

instance Prelude.NFData PutPolicyResponse
