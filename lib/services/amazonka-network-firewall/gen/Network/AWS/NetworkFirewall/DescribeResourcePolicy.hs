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
-- Module      : Network.AWS.NetworkFirewall.DescribeResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a resource policy that you created in a PutResourcePolicy
-- request.
module Network.AWS.NetworkFirewall.DescribeResourcePolicy
  ( -- * Creating a Request
    DescribeResourcePolicy (..),
    newDescribeResourcePolicy,

    -- * Request Lenses
    describeResourcePolicy_resourceArn,

    -- * Destructuring the Response
    DescribeResourcePolicyResponse (..),
    newDescribeResourcePolicyResponse,

    -- * Response Lenses
    describeResourcePolicyResponse_policy,
    describeResourcePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.NetworkFirewall.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeResourcePolicy' smart constructor.
data DescribeResourcePolicy = DescribeResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the rule group or firewall policy
    -- whose resource policy you want to retrieve.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'describeResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the rule group or firewall policy
-- whose resource policy you want to retrieve.
newDescribeResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  DescribeResourcePolicy
newDescribeResourcePolicy pResourceArn_ =
  DescribeResourcePolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the rule group or firewall policy
-- whose resource policy you want to retrieve.
describeResourcePolicy_resourceArn :: Lens.Lens' DescribeResourcePolicy Prelude.Text
describeResourcePolicy_resourceArn = Lens.lens (\DescribeResourcePolicy' {resourceArn} -> resourceArn) (\s@DescribeResourcePolicy' {} a -> s {resourceArn = a} :: DescribeResourcePolicy)

instance Core.AWSRequest DescribeResourcePolicy where
  type
    AWSResponse DescribeResourcePolicy =
      DescribeResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeResourcePolicyResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeResourcePolicy

instance Prelude.NFData DescribeResourcePolicy

instance Core.ToHeaders DescribeResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "NetworkFirewall_20201112.DescribeResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeResourcePolicy where
  toJSON DescribeResourcePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath DescribeResourcePolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeResourcePolicyResponse' smart constructor.
data DescribeResourcePolicyResponse = DescribeResourcePolicyResponse'
  { -- | The AWS Identity and Access Management policy for the resource.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'describeResourcePolicyResponse_policy' - The AWS Identity and Access Management policy for the resource.
--
-- 'httpStatus', 'describeResourcePolicyResponse_httpStatus' - The response's http status code.
newDescribeResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeResourcePolicyResponse
newDescribeResourcePolicyResponse pHttpStatus_ =
  DescribeResourcePolicyResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The AWS Identity and Access Management policy for the resource.
describeResourcePolicyResponse_policy :: Lens.Lens' DescribeResourcePolicyResponse (Prelude.Maybe Prelude.Text)
describeResourcePolicyResponse_policy = Lens.lens (\DescribeResourcePolicyResponse' {policy} -> policy) (\s@DescribeResourcePolicyResponse' {} a -> s {policy = a} :: DescribeResourcePolicyResponse)

-- | The response's http status code.
describeResourcePolicyResponse_httpStatus :: Lens.Lens' DescribeResourcePolicyResponse Prelude.Int
describeResourcePolicyResponse_httpStatus = Lens.lens (\DescribeResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@DescribeResourcePolicyResponse' {} a -> s {httpStatus = a} :: DescribeResourcePolicyResponse)

instance
  Prelude.NFData
    DescribeResourcePolicyResponse
