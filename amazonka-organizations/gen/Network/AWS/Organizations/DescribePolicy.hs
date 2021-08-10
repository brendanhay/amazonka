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
-- Module      : Network.AWS.Organizations.DescribePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a policy.
--
-- This operation can be called only from the organization\'s management
-- account or by a member account that is a delegated administrator for an
-- AWS service.
module Network.AWS.Organizations.DescribePolicy
  ( -- * Creating a Request
    DescribePolicy (..),
    newDescribePolicy,

    -- * Request Lenses
    describePolicy_policyId,

    -- * Destructuring the Response
    DescribePolicyResponse (..),
    newDescribePolicyResponse,

    -- * Response Lenses
    describePolicyResponse_policy,
    describePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePolicy' smart constructor.
data DescribePolicy = DescribePolicy'
  { -- | The unique identifier (ID) of the policy that you want details about.
    -- You can get the ID from the ListPolicies or ListPoliciesForTarget
    -- operations.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
    -- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
    -- letters, digits, or the underscore character (_).
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'describePolicy_policyId' - The unique identifier (ID) of the policy that you want details about.
-- You can get the ID from the ListPolicies or ListPoliciesForTarget
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
newDescribePolicy ::
  -- | 'policyId'
  Prelude.Text ->
  DescribePolicy
newDescribePolicy pPolicyId_ =
  DescribePolicy' {policyId = pPolicyId_}

-- | The unique identifier (ID) of the policy that you want details about.
-- You can get the ID from the ListPolicies or ListPoliciesForTarget
-- operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID
-- string requires \"p-\" followed by from 8 to 128 lowercase or uppercase
-- letters, digits, or the underscore character (_).
describePolicy_policyId :: Lens.Lens' DescribePolicy Prelude.Text
describePolicy_policyId = Lens.lens (\DescribePolicy' {policyId} -> policyId) (\s@DescribePolicy' {} a -> s {policyId = a} :: DescribePolicy)

instance Core.AWSRequest DescribePolicy where
  type
    AWSResponse DescribePolicy =
      DescribePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePolicy

instance Prelude.NFData DescribePolicy

instance Core.ToHeaders DescribePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSOrganizationsV20161128.DescribePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribePolicy where
  toJSON DescribePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PolicyId" Core..= policyId)]
      )

instance Core.ToPath DescribePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePolicyResponse' smart constructor.
data DescribePolicyResponse = DescribePolicyResponse'
  { -- | A structure that contains details about the specified policy.
    policy :: Prelude.Maybe Policy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'describePolicyResponse_policy' - A structure that contains details about the specified policy.
--
-- 'httpStatus', 'describePolicyResponse_httpStatus' - The response's http status code.
newDescribePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePolicyResponse
newDescribePolicyResponse pHttpStatus_ =
  DescribePolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains details about the specified policy.
describePolicyResponse_policy :: Lens.Lens' DescribePolicyResponse (Prelude.Maybe Policy)
describePolicyResponse_policy = Lens.lens (\DescribePolicyResponse' {policy} -> policy) (\s@DescribePolicyResponse' {} a -> s {policy = a} :: DescribePolicyResponse)

-- | The response's http status code.
describePolicyResponse_httpStatus :: Lens.Lens' DescribePolicyResponse Prelude.Int
describePolicyResponse_httpStatus = Lens.lens (\DescribePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribePolicyResponse' {} a -> s {httpStatus = a} :: DescribePolicyResponse)

instance Prelude.NFData DescribePolicyResponse
