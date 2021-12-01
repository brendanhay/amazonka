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
-- Module      : Amazonka.ELB.DescribeLoadBalancerPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified policies.
--
-- If you specify a load balancer name, the action returns the descriptions
-- of all policies created for the load balancer. If you specify a policy
-- name associated with your load balancer, the action returns the
-- description of that policy. If you don\'t specify a load balancer name,
-- the action returns descriptions of the specified sample policies, or
-- descriptions of all sample policies. The names of the sample policies
-- have the @ELBSample-@ prefix.
module Amazonka.ELB.DescribeLoadBalancerPolicies
  ( -- * Creating a Request
    DescribeLoadBalancerPolicies (..),
    newDescribeLoadBalancerPolicies,

    -- * Request Lenses
    describeLoadBalancerPolicies_policyNames,
    describeLoadBalancerPolicies_loadBalancerName,

    -- * Destructuring the Response
    DescribeLoadBalancerPoliciesResponse (..),
    newDescribeLoadBalancerPoliciesResponse,

    -- * Response Lenses
    describeLoadBalancerPoliciesResponse_policyDescriptions,
    describeLoadBalancerPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeLoadBalancerPolicies.
--
-- /See:/ 'newDescribeLoadBalancerPolicies' smart constructor.
data DescribeLoadBalancerPolicies = DescribeLoadBalancerPolicies'
  { -- | The names of the policies.
    policyNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyNames', 'describeLoadBalancerPolicies_policyNames' - The names of the policies.
--
-- 'loadBalancerName', 'describeLoadBalancerPolicies_loadBalancerName' - The name of the load balancer.
newDescribeLoadBalancerPolicies ::
  DescribeLoadBalancerPolicies
newDescribeLoadBalancerPolicies =
  DescribeLoadBalancerPolicies'
    { policyNames =
        Prelude.Nothing,
      loadBalancerName = Prelude.Nothing
    }

-- | The names of the policies.
describeLoadBalancerPolicies_policyNames :: Lens.Lens' DescribeLoadBalancerPolicies (Prelude.Maybe [Prelude.Text])
describeLoadBalancerPolicies_policyNames = Lens.lens (\DescribeLoadBalancerPolicies' {policyNames} -> policyNames) (\s@DescribeLoadBalancerPolicies' {} a -> s {policyNames = a} :: DescribeLoadBalancerPolicies) Prelude.. Lens.mapping Lens.coerced

-- | The name of the load balancer.
describeLoadBalancerPolicies_loadBalancerName :: Lens.Lens' DescribeLoadBalancerPolicies (Prelude.Maybe Prelude.Text)
describeLoadBalancerPolicies_loadBalancerName = Lens.lens (\DescribeLoadBalancerPolicies' {loadBalancerName} -> loadBalancerName) (\s@DescribeLoadBalancerPolicies' {} a -> s {loadBalancerName = a} :: DescribeLoadBalancerPolicies)

instance Core.AWSRequest DescribeLoadBalancerPolicies where
  type
    AWSResponse DescribeLoadBalancerPolicies =
      DescribeLoadBalancerPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerPoliciesResult"
      ( \s h x ->
          DescribeLoadBalancerPoliciesResponse'
            Prelude.<$> ( x Core..@? "PolicyDescriptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBalancerPolicies
  where
  hashWithSalt salt' DescribeLoadBalancerPolicies' {..} =
    salt' `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` policyNames

instance Prelude.NFData DescribeLoadBalancerPolicies where
  rnf DescribeLoadBalancerPolicies' {..} =
    Prelude.rnf policyNames
      `Prelude.seq` Prelude.rnf loadBalancerName

instance Core.ToHeaders DescribeLoadBalancerPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLoadBalancerPolicies where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLoadBalancerPolicies where
  toQuery DescribeLoadBalancerPolicies' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerPolicies" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "PolicyNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> policyNames),
        "LoadBalancerName" Core.=: loadBalancerName
      ]

-- | Contains the output of DescribeLoadBalancerPolicies.
--
-- /See:/ 'newDescribeLoadBalancerPoliciesResponse' smart constructor.
data DescribeLoadBalancerPoliciesResponse = DescribeLoadBalancerPoliciesResponse'
  { -- | Information about the policies.
    policyDescriptions :: Prelude.Maybe [PolicyDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyDescriptions', 'describeLoadBalancerPoliciesResponse_policyDescriptions' - Information about the policies.
--
-- 'httpStatus', 'describeLoadBalancerPoliciesResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancerPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancerPoliciesResponse
newDescribeLoadBalancerPoliciesResponse pHttpStatus_ =
  DescribeLoadBalancerPoliciesResponse'
    { policyDescriptions =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the policies.
describeLoadBalancerPoliciesResponse_policyDescriptions :: Lens.Lens' DescribeLoadBalancerPoliciesResponse (Prelude.Maybe [PolicyDescription])
describeLoadBalancerPoliciesResponse_policyDescriptions = Lens.lens (\DescribeLoadBalancerPoliciesResponse' {policyDescriptions} -> policyDescriptions) (\s@DescribeLoadBalancerPoliciesResponse' {} a -> s {policyDescriptions = a} :: DescribeLoadBalancerPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLoadBalancerPoliciesResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerPoliciesResponse Prelude.Int
describeLoadBalancerPoliciesResponse_httpStatus = Lens.lens (\DescribeLoadBalancerPoliciesResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerPoliciesResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerPoliciesResponse)

instance
  Prelude.NFData
    DescribeLoadBalancerPoliciesResponse
  where
  rnf DescribeLoadBalancerPoliciesResponse' {..} =
    Prelude.rnf policyDescriptions
      `Prelude.seq` Prelude.rnf httpStatus
