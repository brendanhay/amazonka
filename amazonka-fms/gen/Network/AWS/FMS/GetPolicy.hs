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
-- Module      : Network.AWS.FMS.GetPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified AWS Firewall Manager policy.
module Network.AWS.FMS.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_policyId,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_policy,
    getPolicyResponse_policyArn,
    getPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | The ID of the AWS Firewall Manager policy that you want the details for.
    policyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'getPolicy_policyId' - The ID of the AWS Firewall Manager policy that you want the details for.
newGetPolicy ::
  -- | 'policyId'
  Core.Text ->
  GetPolicy
newGetPolicy pPolicyId_ =
  GetPolicy' {policyId = pPolicyId_}

-- | The ID of the AWS Firewall Manager policy that you want the details for.
getPolicy_policyId :: Lens.Lens' GetPolicy Core.Text
getPolicy_policyId = Lens.lens (\GetPolicy' {policyId} -> policyId) (\s@GetPolicy' {} a -> s {policyId = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Core.<$> (x Core..?> "Policy")
            Core.<*> (x Core..?> "PolicyArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetPolicy

instance Core.NFData GetPolicy

instance Core.ToHeaders GetPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSFMS_20180101.GetPolicy" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetPolicy where
  toJSON GetPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PolicyId" Core..= policyId)]
      )

instance Core.ToPath GetPolicy where
  toPath = Core.const "/"

instance Core.ToQuery GetPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | Information about the specified AWS Firewall Manager policy.
    policy :: Core.Maybe Policy,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getPolicyResponse_policy' - Information about the specified AWS Firewall Manager policy.
--
-- 'policyArn', 'getPolicyResponse_policyArn' - The Amazon Resource Name (ARN) of the specified policy.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { policy = Core.Nothing,
      policyArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the specified AWS Firewall Manager policy.
getPolicyResponse_policy :: Lens.Lens' GetPolicyResponse (Core.Maybe Policy)
getPolicyResponse_policy = Lens.lens (\GetPolicyResponse' {policy} -> policy) (\s@GetPolicyResponse' {} a -> s {policy = a} :: GetPolicyResponse)

-- | The Amazon Resource Name (ARN) of the specified policy.
getPolicyResponse_policyArn :: Lens.Lens' GetPolicyResponse (Core.Maybe Core.Text)
getPolicyResponse_policyArn = Lens.lens (\GetPolicyResponse' {policyArn} -> policyArn) (\s@GetPolicyResponse' {} a -> s {policyArn = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Core.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Core.NFData GetPolicyResponse
