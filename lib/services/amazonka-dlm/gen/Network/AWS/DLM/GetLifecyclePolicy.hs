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
-- Module      : Network.AWS.DLM.GetLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information about the specified lifecycle policy.
module Network.AWS.DLM.GetLifecyclePolicy
  ( -- * Creating a Request
    GetLifecyclePolicy (..),
    newGetLifecyclePolicy,

    -- * Request Lenses
    getLifecyclePolicy_policyId,

    -- * Destructuring the Response
    GetLifecyclePolicyResponse (..),
    newGetLifecyclePolicyResponse,

    -- * Response Lenses
    getLifecyclePolicyResponse_policy,
    getLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DLM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The identifier of the lifecycle policy.
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'getLifecyclePolicy_policyId' - The identifier of the lifecycle policy.
newGetLifecyclePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pPolicyId_ =
  GetLifecyclePolicy' {policyId = pPolicyId_}

-- | The identifier of the lifecycle policy.
getLifecyclePolicy_policyId :: Lens.Lens' GetLifecyclePolicy Prelude.Text
getLifecyclePolicy_policyId = Lens.lens (\GetLifecyclePolicy' {policyId} -> policyId) (\s@GetLifecyclePolicy' {} a -> s {policyId = a} :: GetLifecyclePolicy)

instance Core.AWSRequest GetLifecyclePolicy where
  type
    AWSResponse GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLifecyclePolicy

instance Prelude.NFData GetLifecyclePolicy

instance Core.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetLifecyclePolicy where
  toPath GetLifecyclePolicy' {..} =
    Prelude.mconcat
      ["/policies/", Core.toBS policyId, "/"]

instance Core.ToQuery GetLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | Detailed information about the lifecycle policy.
    policy :: Prelude.Maybe LifecyclePolicy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getLifecyclePolicyResponse_policy' - Detailed information about the lifecycle policy.
--
-- 'httpStatus', 'getLifecyclePolicyResponse_httpStatus' - The response's http status code.
newGetLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse pHttpStatus_ =
  GetLifecyclePolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the lifecycle policy.
getLifecyclePolicyResponse_policy :: Lens.Lens' GetLifecyclePolicyResponse (Prelude.Maybe LifecyclePolicy)
getLifecyclePolicyResponse_policy = Lens.lens (\GetLifecyclePolicyResponse' {policy} -> policy) (\s@GetLifecyclePolicyResponse' {} a -> s {policy = a} :: GetLifecyclePolicyResponse)

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Prelude.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

instance Prelude.NFData GetLifecyclePolicyResponse
