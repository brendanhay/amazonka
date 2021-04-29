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
-- Module      : Network.AWS.ECR.PutRegistryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the permissions policy for your registry.
--
-- A registry policy is used to specify permissions for another AWS account
-- and is used when configuring cross-account replication. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
-- in the /Amazon Elastic Container Registry User Guide/.
module Network.AWS.ECR.PutRegistryPolicy
  ( -- * Creating a Request
    PutRegistryPolicy (..),
    newPutRegistryPolicy,

    -- * Request Lenses
    putRegistryPolicy_policyText,

    -- * Destructuring the Response
    PutRegistryPolicyResponse (..),
    newPutRegistryPolicyResponse,

    -- * Response Lenses
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_httpStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRegistryPolicy' smart constructor.
data PutRegistryPolicy = PutRegistryPolicy'
  { -- | The JSON policy text to apply to your registry. The policy text follows
    -- the same format as IAM policy text. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'putRegistryPolicy_policyText' - The JSON policy text to apply to your registry. The policy text follows
-- the same format as IAM policy text. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
-- in the /Amazon Elastic Container Registry User Guide/.
newPutRegistryPolicy ::
  -- | 'policyText'
  Prelude.Text ->
  PutRegistryPolicy
newPutRegistryPolicy pPolicyText_ =
  PutRegistryPolicy' {policyText = pPolicyText_}

-- | The JSON policy text to apply to your registry. The policy text follows
-- the same format as IAM policy text. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
-- in the /Amazon Elastic Container Registry User Guide/.
putRegistryPolicy_policyText :: Lens.Lens' PutRegistryPolicy Prelude.Text
putRegistryPolicy_policyText = Lens.lens (\PutRegistryPolicy' {policyText} -> policyText) (\s@PutRegistryPolicy' {} a -> s {policyText = a} :: PutRegistryPolicy)

instance Prelude.AWSRequest PutRegistryPolicy where
  type Rs PutRegistryPolicy = PutRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRegistryPolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "policyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRegistryPolicy

instance Prelude.NFData PutRegistryPolicy

instance Prelude.ToHeaders PutRegistryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.PutRegistryPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutRegistryPolicy where
  toJSON PutRegistryPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("policyText" Prelude..= policyText)]
      )

instance Prelude.ToPath PutRegistryPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutRegistryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRegistryPolicyResponse' smart constructor.
data PutRegistryPolicyResponse = PutRegistryPolicyResponse'
  { -- | The registry ID.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The JSON policy text for your registry.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'putRegistryPolicyResponse_registryId' - The registry ID.
--
-- 'policyText', 'putRegistryPolicyResponse_policyText' - The JSON policy text for your registry.
--
-- 'httpStatus', 'putRegistryPolicyResponse_httpStatus' - The response's http status code.
newPutRegistryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRegistryPolicyResponse
newPutRegistryPolicyResponse pHttpStatus_ =
  PutRegistryPolicyResponse'
    { registryId =
        Prelude.Nothing,
      policyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID.
putRegistryPolicyResponse_registryId :: Lens.Lens' PutRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
putRegistryPolicyResponse_registryId = Lens.lens (\PutRegistryPolicyResponse' {registryId} -> registryId) (\s@PutRegistryPolicyResponse' {} a -> s {registryId = a} :: PutRegistryPolicyResponse)

-- | The JSON policy text for your registry.
putRegistryPolicyResponse_policyText :: Lens.Lens' PutRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
putRegistryPolicyResponse_policyText = Lens.lens (\PutRegistryPolicyResponse' {policyText} -> policyText) (\s@PutRegistryPolicyResponse' {} a -> s {policyText = a} :: PutRegistryPolicyResponse)

-- | The response's http status code.
putRegistryPolicyResponse_httpStatus :: Lens.Lens' PutRegistryPolicyResponse Prelude.Int
putRegistryPolicyResponse_httpStatus = Lens.lens (\PutRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRegistryPolicyResponse' {} a -> s {httpStatus = a} :: PutRegistryPolicyResponse)

instance Prelude.NFData PutRegistryPolicyResponse
