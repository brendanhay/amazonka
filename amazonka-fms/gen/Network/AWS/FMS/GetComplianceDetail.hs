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
-- Module      : Network.AWS.FMS.GetComplianceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed compliance information about the specified member
-- account. Details include resources that are in and out of compliance
-- with the specified policy. Resources are considered noncompliant for AWS
-- WAF and Shield Advanced policies if the specified policy has not been
-- applied to them. Resources are considered noncompliant for security
-- group policies if they are in scope of the policy, they violate one or
-- more of the policy rules, and remediation is disabled or not possible.
-- Resources are considered noncompliant for Network Firewall policies if a
-- firewall is missing in the VPC, if the firewall endpoint isn\'t set up
-- in an expected Availability Zone and subnet, if a subnet created by the
-- Firewall Manager doesn\'t have the expected route table, and for
-- modifications to a firewall policy that violate the Firewall Manager
-- policy\'s rules.
module Network.AWS.FMS.GetComplianceDetail
  ( -- * Creating a Request
    GetComplianceDetail (..),
    newGetComplianceDetail,

    -- * Request Lenses
    getComplianceDetail_policyId,
    getComplianceDetail_memberAccount,

    -- * Destructuring the Response
    GetComplianceDetailResponse (..),
    newGetComplianceDetailResponse,

    -- * Response Lenses
    getComplianceDetailResponse_policyComplianceDetail,
    getComplianceDetailResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetComplianceDetail' smart constructor.
data GetComplianceDetail = GetComplianceDetail'
  { -- | The ID of the policy that you want to get the details for. @PolicyId@ is
    -- returned by @PutPolicy@ and by @ListPolicies@.
    policyId :: Core.Text,
    -- | The AWS account that owns the resources that you want to get the details
    -- for.
    memberAccount :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'getComplianceDetail_policyId' - The ID of the policy that you want to get the details for. @PolicyId@ is
-- returned by @PutPolicy@ and by @ListPolicies@.
--
-- 'memberAccount', 'getComplianceDetail_memberAccount' - The AWS account that owns the resources that you want to get the details
-- for.
newGetComplianceDetail ::
  -- | 'policyId'
  Core.Text ->
  -- | 'memberAccount'
  Core.Text ->
  GetComplianceDetail
newGetComplianceDetail pPolicyId_ pMemberAccount_ =
  GetComplianceDetail'
    { policyId = pPolicyId_,
      memberAccount = pMemberAccount_
    }

-- | The ID of the policy that you want to get the details for. @PolicyId@ is
-- returned by @PutPolicy@ and by @ListPolicies@.
getComplianceDetail_policyId :: Lens.Lens' GetComplianceDetail Core.Text
getComplianceDetail_policyId = Lens.lens (\GetComplianceDetail' {policyId} -> policyId) (\s@GetComplianceDetail' {} a -> s {policyId = a} :: GetComplianceDetail)

-- | The AWS account that owns the resources that you want to get the details
-- for.
getComplianceDetail_memberAccount :: Lens.Lens' GetComplianceDetail Core.Text
getComplianceDetail_memberAccount = Lens.lens (\GetComplianceDetail' {memberAccount} -> memberAccount) (\s@GetComplianceDetail' {} a -> s {memberAccount = a} :: GetComplianceDetail)

instance Core.AWSRequest GetComplianceDetail where
  type
    AWSResponse GetComplianceDetail =
      GetComplianceDetailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailResponse'
            Core.<$> (x Core..?> "PolicyComplianceDetail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetComplianceDetail

instance Core.NFData GetComplianceDetail

instance Core.ToHeaders GetComplianceDetail where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSFMS_20180101.GetComplianceDetail" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetComplianceDetail where
  toJSON GetComplianceDetail' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PolicyId" Core..= policyId),
            Core.Just ("MemberAccount" Core..= memberAccount)
          ]
      )

instance Core.ToPath GetComplianceDetail where
  toPath = Core.const "/"

instance Core.ToQuery GetComplianceDetail where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetComplianceDetailResponse' smart constructor.
data GetComplianceDetailResponse = GetComplianceDetailResponse'
  { -- | Information about the resources and the policy that you specified in the
    -- @GetComplianceDetail@ request.
    policyComplianceDetail :: Core.Maybe PolicyComplianceDetail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetComplianceDetailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyComplianceDetail', 'getComplianceDetailResponse_policyComplianceDetail' - Information about the resources and the policy that you specified in the
-- @GetComplianceDetail@ request.
--
-- 'httpStatus', 'getComplianceDetailResponse_httpStatus' - The response's http status code.
newGetComplianceDetailResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetComplianceDetailResponse
newGetComplianceDetailResponse pHttpStatus_ =
  GetComplianceDetailResponse'
    { policyComplianceDetail =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resources and the policy that you specified in the
-- @GetComplianceDetail@ request.
getComplianceDetailResponse_policyComplianceDetail :: Lens.Lens' GetComplianceDetailResponse (Core.Maybe PolicyComplianceDetail)
getComplianceDetailResponse_policyComplianceDetail = Lens.lens (\GetComplianceDetailResponse' {policyComplianceDetail} -> policyComplianceDetail) (\s@GetComplianceDetailResponse' {} a -> s {policyComplianceDetail = a} :: GetComplianceDetailResponse)

-- | The response's http status code.
getComplianceDetailResponse_httpStatus :: Lens.Lens' GetComplianceDetailResponse Core.Int
getComplianceDetailResponse_httpStatus = Lens.lens (\GetComplianceDetailResponse' {httpStatus} -> httpStatus) (\s@GetComplianceDetailResponse' {} a -> s {httpStatus = a} :: GetComplianceDetailResponse)

instance Core.NFData GetComplianceDetailResponse
