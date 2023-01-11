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
-- Module      : Amazonka.FMS.GetComplianceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed compliance information about the specified member
-- account. Details include resources that are in and out of compliance
-- with the specified policy.
--
-- -   Resources are considered noncompliant for WAF and Shield Advanced
--     policies if the specified policy has not been applied to them.
--
-- -   Resources are considered noncompliant for security group policies if
--     they are in scope of the policy, they violate one or more of the
--     policy rules, and remediation is disabled or not possible.
--
-- -   Resources are considered noncompliant for Network Firewall policies
--     if a firewall is missing in the VPC, if the firewall endpoint isn\'t
--     set up in an expected Availability Zone and subnet, if a subnet
--     created by the Firewall Manager doesn\'t have the expected route
--     table, and for modifications to a firewall policy that violate the
--     Firewall Manager policy\'s rules.
--
-- -   Resources are considered noncompliant for DNS Firewall policies if a
--     DNS Firewall rule group is missing from the rule group associations
--     for the VPC.
module Amazonka.FMS.GetComplianceDetail
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetComplianceDetail' smart constructor.
data GetComplianceDetail = GetComplianceDetail'
  { -- | The ID of the policy that you want to get the details for. @PolicyId@ is
    -- returned by @PutPolicy@ and by @ListPolicies@.
    policyId :: Prelude.Text,
    -- | The Amazon Web Services account that owns the resources that you want to
    -- get the details for.
    memberAccount :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'memberAccount', 'getComplianceDetail_memberAccount' - The Amazon Web Services account that owns the resources that you want to
-- get the details for.
newGetComplianceDetail ::
  -- | 'policyId'
  Prelude.Text ->
  -- | 'memberAccount'
  Prelude.Text ->
  GetComplianceDetail
newGetComplianceDetail pPolicyId_ pMemberAccount_ =
  GetComplianceDetail'
    { policyId = pPolicyId_,
      memberAccount = pMemberAccount_
    }

-- | The ID of the policy that you want to get the details for. @PolicyId@ is
-- returned by @PutPolicy@ and by @ListPolicies@.
getComplianceDetail_policyId :: Lens.Lens' GetComplianceDetail Prelude.Text
getComplianceDetail_policyId = Lens.lens (\GetComplianceDetail' {policyId} -> policyId) (\s@GetComplianceDetail' {} a -> s {policyId = a} :: GetComplianceDetail)

-- | The Amazon Web Services account that owns the resources that you want to
-- get the details for.
getComplianceDetail_memberAccount :: Lens.Lens' GetComplianceDetail Prelude.Text
getComplianceDetail_memberAccount = Lens.lens (\GetComplianceDetail' {memberAccount} -> memberAccount) (\s@GetComplianceDetail' {} a -> s {memberAccount = a} :: GetComplianceDetail)

instance Core.AWSRequest GetComplianceDetail where
  type
    AWSResponse GetComplianceDetail =
      GetComplianceDetailResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetComplianceDetailResponse'
            Prelude.<$> (x Data..?> "PolicyComplianceDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetComplianceDetail where
  hashWithSalt _salt GetComplianceDetail' {..} =
    _salt `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` memberAccount

instance Prelude.NFData GetComplianceDetail where
  rnf GetComplianceDetail' {..} =
    Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf memberAccount

instance Data.ToHeaders GetComplianceDetail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.GetComplianceDetail" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetComplianceDetail where
  toJSON GetComplianceDetail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PolicyId" Data..= policyId),
            Prelude.Just
              ("MemberAccount" Data..= memberAccount)
          ]
      )

instance Data.ToPath GetComplianceDetail where
  toPath = Prelude.const "/"

instance Data.ToQuery GetComplianceDetail where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetComplianceDetailResponse' smart constructor.
data GetComplianceDetailResponse = GetComplianceDetailResponse'
  { -- | Information about the resources and the policy that you specified in the
    -- @GetComplianceDetail@ request.
    policyComplianceDetail :: Prelude.Maybe PolicyComplianceDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetComplianceDetailResponse
newGetComplianceDetailResponse pHttpStatus_ =
  GetComplianceDetailResponse'
    { policyComplianceDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the resources and the policy that you specified in the
-- @GetComplianceDetail@ request.
getComplianceDetailResponse_policyComplianceDetail :: Lens.Lens' GetComplianceDetailResponse (Prelude.Maybe PolicyComplianceDetail)
getComplianceDetailResponse_policyComplianceDetail = Lens.lens (\GetComplianceDetailResponse' {policyComplianceDetail} -> policyComplianceDetail) (\s@GetComplianceDetailResponse' {} a -> s {policyComplianceDetail = a} :: GetComplianceDetailResponse)

-- | The response's http status code.
getComplianceDetailResponse_httpStatus :: Lens.Lens' GetComplianceDetailResponse Prelude.Int
getComplianceDetailResponse_httpStatus = Lens.lens (\GetComplianceDetailResponse' {httpStatus} -> httpStatus) (\s@GetComplianceDetailResponse' {} a -> s {httpStatus = a} :: GetComplianceDetailResponse)

instance Prelude.NFData GetComplianceDetailResponse where
  rnf GetComplianceDetailResponse' {..} =
    Prelude.rnf policyComplianceDetail
      `Prelude.seq` Prelude.rnf httpStatus
