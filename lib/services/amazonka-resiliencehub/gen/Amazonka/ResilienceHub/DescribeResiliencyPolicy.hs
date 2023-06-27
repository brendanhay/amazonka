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
-- Module      : Amazonka.ResilienceHub.DescribeResiliencyPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specified resiliency policy for an Resilience Hub
-- application. The returned policy object includes creation time, data
-- location constraints, the Amazon Resource Name (ARN) for the policy,
-- tags, tier, and more.
module Amazonka.ResilienceHub.DescribeResiliencyPolicy
  ( -- * Creating a Request
    DescribeResiliencyPolicy (..),
    newDescribeResiliencyPolicy,

    -- * Request Lenses
    describeResiliencyPolicy_policyArn,

    -- * Destructuring the Response
    DescribeResiliencyPolicyResponse (..),
    newDescribeResiliencyPolicyResponse,

    -- * Response Lenses
    describeResiliencyPolicyResponse_httpStatus,
    describeResiliencyPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResilienceHub.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeResiliencyPolicy' smart constructor.
data DescribeResiliencyPolicy = DescribeResiliencyPolicy'
  { -- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
    -- this ARN is:
    -- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the /AWS General Reference/ guide.
    policyArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResiliencyPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyArn', 'describeResiliencyPolicy_policyArn' - The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
newDescribeResiliencyPolicy ::
  -- | 'policyArn'
  Prelude.Text ->
  DescribeResiliencyPolicy
newDescribeResiliencyPolicy pPolicyArn_ =
  DescribeResiliencyPolicy' {policyArn = pPolicyArn_}

-- | The Amazon Resource Name (ARN) of the resiliency policy. The format for
-- this ARN is:
-- arn:@partition@:resiliencehub:@region@:@account@:resiliency-policy\/@policy-id@.
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the /AWS General Reference/ guide.
describeResiliencyPolicy_policyArn :: Lens.Lens' DescribeResiliencyPolicy Prelude.Text
describeResiliencyPolicy_policyArn = Lens.lens (\DescribeResiliencyPolicy' {policyArn} -> policyArn) (\s@DescribeResiliencyPolicy' {} a -> s {policyArn = a} :: DescribeResiliencyPolicy)

instance Core.AWSRequest DescribeResiliencyPolicy where
  type
    AWSResponse DescribeResiliencyPolicy =
      DescribeResiliencyPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResiliencyPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "policy")
      )

instance Prelude.Hashable DescribeResiliencyPolicy where
  hashWithSalt _salt DescribeResiliencyPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyArn

instance Prelude.NFData DescribeResiliencyPolicy where
  rnf DescribeResiliencyPolicy' {..} =
    Prelude.rnf policyArn

instance Data.ToHeaders DescribeResiliencyPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeResiliencyPolicy where
  toJSON DescribeResiliencyPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("policyArn" Data..= policyArn)]
      )

instance Data.ToPath DescribeResiliencyPolicy where
  toPath = Prelude.const "/describe-resiliency-policy"

instance Data.ToQuery DescribeResiliencyPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResiliencyPolicyResponse' smart constructor.
data DescribeResiliencyPolicyResponse = DescribeResiliencyPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the specific resiliency policy, returned as an object.
    -- This object includes creation time, data location constraints, its name,
    -- description, tags, the recovery time objective (RTO) and recovery point
    -- objective (RPO) in seconds, and more.
    policy :: ResiliencyPolicy
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResiliencyPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeResiliencyPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'describeResiliencyPolicyResponse_policy' - Information about the specific resiliency policy, returned as an object.
-- This object includes creation time, data location constraints, its name,
-- description, tags, the recovery time objective (RTO) and recovery point
-- objective (RPO) in seconds, and more.
newDescribeResiliencyPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  ResiliencyPolicy ->
  DescribeResiliencyPolicyResponse
newDescribeResiliencyPolicyResponse
  pHttpStatus_
  pPolicy_ =
    DescribeResiliencyPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        policy = pPolicy_
      }

-- | The response's http status code.
describeResiliencyPolicyResponse_httpStatus :: Lens.Lens' DescribeResiliencyPolicyResponse Prelude.Int
describeResiliencyPolicyResponse_httpStatus = Lens.lens (\DescribeResiliencyPolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeResiliencyPolicyResponse' {} a -> s {httpStatus = a} :: DescribeResiliencyPolicyResponse)

-- | Information about the specific resiliency policy, returned as an object.
-- This object includes creation time, data location constraints, its name,
-- description, tags, the recovery time objective (RTO) and recovery point
-- objective (RPO) in seconds, and more.
describeResiliencyPolicyResponse_policy :: Lens.Lens' DescribeResiliencyPolicyResponse ResiliencyPolicy
describeResiliencyPolicyResponse_policy = Lens.lens (\DescribeResiliencyPolicyResponse' {policy} -> policy) (\s@DescribeResiliencyPolicyResponse' {} a -> s {policy = a} :: DescribeResiliencyPolicyResponse)

instance
  Prelude.NFData
    DescribeResiliencyPolicyResponse
  where
  rnf DescribeResiliencyPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf policy
