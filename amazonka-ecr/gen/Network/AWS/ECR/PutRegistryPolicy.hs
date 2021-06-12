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

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutRegistryPolicy' smart constructor.
data PutRegistryPolicy = PutRegistryPolicy'
  { -- | The JSON policy text to apply to your registry. The policy text follows
    -- the same format as IAM policy text. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  PutRegistryPolicy
newPutRegistryPolicy pPolicyText_ =
  PutRegistryPolicy' {policyText = pPolicyText_}

-- | The JSON policy text to apply to your registry. The policy text follows
-- the same format as IAM policy text. For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
-- in the /Amazon Elastic Container Registry User Guide/.
putRegistryPolicy_policyText :: Lens.Lens' PutRegistryPolicy Core.Text
putRegistryPolicy_policyText = Lens.lens (\PutRegistryPolicy' {policyText} -> policyText) (\s@PutRegistryPolicy' {} a -> s {policyText = a} :: PutRegistryPolicy)

instance Core.AWSRequest PutRegistryPolicy where
  type
    AWSResponse PutRegistryPolicy =
      PutRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRegistryPolicyResponse'
            Core.<$> (x Core..?> "registryId")
            Core.<*> (x Core..?> "policyText")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutRegistryPolicy

instance Core.NFData PutRegistryPolicy

instance Core.ToHeaders PutRegistryPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutRegistryPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutRegistryPolicy where
  toJSON PutRegistryPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("policyText" Core..= policyText)]
      )

instance Core.ToPath PutRegistryPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutRegistryPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutRegistryPolicyResponse' smart constructor.
data PutRegistryPolicyResponse = PutRegistryPolicyResponse'
  { -- | The registry ID.
    registryId :: Core.Maybe Core.Text,
    -- | The JSON policy text for your registry.
    policyText :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutRegistryPolicyResponse
newPutRegistryPolicyResponse pHttpStatus_ =
  PutRegistryPolicyResponse'
    { registryId =
        Core.Nothing,
      policyText = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID.
putRegistryPolicyResponse_registryId :: Lens.Lens' PutRegistryPolicyResponse (Core.Maybe Core.Text)
putRegistryPolicyResponse_registryId = Lens.lens (\PutRegistryPolicyResponse' {registryId} -> registryId) (\s@PutRegistryPolicyResponse' {} a -> s {registryId = a} :: PutRegistryPolicyResponse)

-- | The JSON policy text for your registry.
putRegistryPolicyResponse_policyText :: Lens.Lens' PutRegistryPolicyResponse (Core.Maybe Core.Text)
putRegistryPolicyResponse_policyText = Lens.lens (\PutRegistryPolicyResponse' {policyText} -> policyText) (\s@PutRegistryPolicyResponse' {} a -> s {policyText = a} :: PutRegistryPolicyResponse)

-- | The response's http status code.
putRegistryPolicyResponse_httpStatus :: Lens.Lens' PutRegistryPolicyResponse Core.Int
putRegistryPolicyResponse_httpStatus = Lens.lens (\PutRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRegistryPolicyResponse' {} a -> s {httpStatus = a} :: PutRegistryPolicyResponse)

instance Core.NFData PutRegistryPolicyResponse
