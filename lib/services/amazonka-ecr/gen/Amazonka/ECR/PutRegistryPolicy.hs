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
-- Module      : Amazonka.ECR.PutRegistryPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the permissions policy for your registry.
--
-- A registry policy is used to specify permissions for another Amazon Web
-- Services account and is used when configuring cross-account replication.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
-- in the /Amazon Elastic Container Registry User Guide/.
module Amazonka.ECR.PutRegistryPolicy
  ( -- * Creating a Request
    PutRegistryPolicy (..),
    newPutRegistryPolicy,

    -- * Request Lenses
    putRegistryPolicy_policyText,

    -- * Destructuring the Response
    PutRegistryPolicyResponse (..),
    newPutRegistryPolicyResponse,

    -- * Response Lenses
    putRegistryPolicyResponse_policyText,
    putRegistryPolicyResponse_registryId,
    putRegistryPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutRegistryPolicy' smart constructor.
data PutRegistryPolicy = PutRegistryPolicy'
  { -- | The JSON policy text to apply to your registry. The policy text follows
    -- the same format as IAM policy text. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECR/latest/userguide/registry-permissions.html Registry permissions>
    -- in the /Amazon Elastic Container Registry User Guide/.
    policyText :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PutRegistryPolicy where
  type
    AWSResponse PutRegistryPolicy =
      PutRegistryPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutRegistryPolicyResponse'
            Prelude.<$> (x Core..?> "policyText")
            Prelude.<*> (x Core..?> "registryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutRegistryPolicy where
  hashWithSalt _salt PutRegistryPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyText

instance Prelude.NFData PutRegistryPolicy where
  rnf PutRegistryPolicy' {..} = Prelude.rnf policyText

instance Core.ToHeaders PutRegistryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.PutRegistryPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutRegistryPolicy where
  toJSON PutRegistryPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("policyText" Core..= policyText)]
      )

instance Core.ToPath PutRegistryPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery PutRegistryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutRegistryPolicyResponse' smart constructor.
data PutRegistryPolicyResponse = PutRegistryPolicyResponse'
  { -- | The JSON policy text for your registry.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The registry ID.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyText', 'putRegistryPolicyResponse_policyText' - The JSON policy text for your registry.
--
-- 'registryId', 'putRegistryPolicyResponse_registryId' - The registry ID.
--
-- 'httpStatus', 'putRegistryPolicyResponse_httpStatus' - The response's http status code.
newPutRegistryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutRegistryPolicyResponse
newPutRegistryPolicyResponse pHttpStatus_ =
  PutRegistryPolicyResponse'
    { policyText =
        Prelude.Nothing,
      registryId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The JSON policy text for your registry.
putRegistryPolicyResponse_policyText :: Lens.Lens' PutRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
putRegistryPolicyResponse_policyText = Lens.lens (\PutRegistryPolicyResponse' {policyText} -> policyText) (\s@PutRegistryPolicyResponse' {} a -> s {policyText = a} :: PutRegistryPolicyResponse)

-- | The registry ID.
putRegistryPolicyResponse_registryId :: Lens.Lens' PutRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
putRegistryPolicyResponse_registryId = Lens.lens (\PutRegistryPolicyResponse' {registryId} -> registryId) (\s@PutRegistryPolicyResponse' {} a -> s {registryId = a} :: PutRegistryPolicyResponse)

-- | The response's http status code.
putRegistryPolicyResponse_httpStatus :: Lens.Lens' PutRegistryPolicyResponse Prelude.Int
putRegistryPolicyResponse_httpStatus = Lens.lens (\PutRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@PutRegistryPolicyResponse' {} a -> s {httpStatus = a} :: PutRegistryPolicyResponse)

instance Prelude.NFData PutRegistryPolicyResponse where
  rnf PutRegistryPolicyResponse' {..} =
    Prelude.rnf policyText
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf httpStatus
