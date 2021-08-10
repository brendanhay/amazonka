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
-- Module      : Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified load balancer policy types or all load balancer
-- policy types.
--
-- The description of each type indicates how it can be used. For example,
-- some policies can be used only with layer 7 listeners, some policies can
-- be used only with layer 4 listeners, and some policies can be used only
-- with your EC2 instances.
--
-- You can use CreateLoadBalancerPolicy to create a policy configuration
-- for any of these policy types. Then, depending on the policy type, use
-- either SetLoadBalancerPoliciesOfListener or
-- SetLoadBalancerPoliciesForBackendServer to set the policy.
module Network.AWS.ELB.DescribeLoadBalancerPolicyTypes
  ( -- * Creating a Request
    DescribeLoadBalancerPolicyTypes (..),
    newDescribeLoadBalancerPolicyTypes,

    -- * Request Lenses
    describeLoadBalancerPolicyTypes_policyTypeNames,

    -- * Destructuring the Response
    DescribeLoadBalancerPolicyTypesResponse (..),
    newDescribeLoadBalancerPolicyTypesResponse,

    -- * Response Lenses
    describeLoadBalancerPolicyTypesResponse_policyTypeDescriptions,
    describeLoadBalancerPolicyTypesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeLoadBalancerPolicyTypes.
--
-- /See:/ 'newDescribeLoadBalancerPolicyTypes' smart constructor.
data DescribeLoadBalancerPolicyTypes = DescribeLoadBalancerPolicyTypes'
  { -- | The names of the policy types. If no names are specified, describes all
    -- policy types defined by Elastic Load Balancing.
    policyTypeNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerPolicyTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyTypeNames', 'describeLoadBalancerPolicyTypes_policyTypeNames' - The names of the policy types. If no names are specified, describes all
-- policy types defined by Elastic Load Balancing.
newDescribeLoadBalancerPolicyTypes ::
  DescribeLoadBalancerPolicyTypes
newDescribeLoadBalancerPolicyTypes =
  DescribeLoadBalancerPolicyTypes'
    { policyTypeNames =
        Prelude.Nothing
    }

-- | The names of the policy types. If no names are specified, describes all
-- policy types defined by Elastic Load Balancing.
describeLoadBalancerPolicyTypes_policyTypeNames :: Lens.Lens' DescribeLoadBalancerPolicyTypes (Prelude.Maybe [Prelude.Text])
describeLoadBalancerPolicyTypes_policyTypeNames = Lens.lens (\DescribeLoadBalancerPolicyTypes' {policyTypeNames} -> policyTypeNames) (\s@DescribeLoadBalancerPolicyTypes' {} a -> s {policyTypeNames = a} :: DescribeLoadBalancerPolicyTypes) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    DescribeLoadBalancerPolicyTypes
  where
  type
    AWSResponse DescribeLoadBalancerPolicyTypes =
      DescribeLoadBalancerPolicyTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerPolicyTypesResult"
      ( \s h x ->
          DescribeLoadBalancerPolicyTypesResponse'
            Prelude.<$> ( x Core..@? "PolicyTypeDescriptions"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBalancerPolicyTypes

instance
  Prelude.NFData
    DescribeLoadBalancerPolicyTypes

instance
  Core.ToHeaders
    DescribeLoadBalancerPolicyTypes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLoadBalancerPolicyTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLoadBalancerPolicyTypes where
  toQuery DescribeLoadBalancerPolicyTypes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerPolicyTypes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "PolicyTypeNames"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> policyTypeNames
            )
      ]

-- | Contains the output of DescribeLoadBalancerPolicyTypes.
--
-- /See:/ 'newDescribeLoadBalancerPolicyTypesResponse' smart constructor.
data DescribeLoadBalancerPolicyTypesResponse = DescribeLoadBalancerPolicyTypesResponse'
  { -- | Information about the policy types.
    policyTypeDescriptions :: Prelude.Maybe [PolicyTypeDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerPolicyTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyTypeDescriptions', 'describeLoadBalancerPolicyTypesResponse_policyTypeDescriptions' - Information about the policy types.
--
-- 'httpStatus', 'describeLoadBalancerPolicyTypesResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancerPolicyTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancerPolicyTypesResponse
newDescribeLoadBalancerPolicyTypesResponse
  pHttpStatus_ =
    DescribeLoadBalancerPolicyTypesResponse'
      { policyTypeDescriptions =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the policy types.
describeLoadBalancerPolicyTypesResponse_policyTypeDescriptions :: Lens.Lens' DescribeLoadBalancerPolicyTypesResponse (Prelude.Maybe [PolicyTypeDescription])
describeLoadBalancerPolicyTypesResponse_policyTypeDescriptions = Lens.lens (\DescribeLoadBalancerPolicyTypesResponse' {policyTypeDescriptions} -> policyTypeDescriptions) (\s@DescribeLoadBalancerPolicyTypesResponse' {} a -> s {policyTypeDescriptions = a} :: DescribeLoadBalancerPolicyTypesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLoadBalancerPolicyTypesResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerPolicyTypesResponse Prelude.Int
describeLoadBalancerPolicyTypesResponse_httpStatus = Lens.lens (\DescribeLoadBalancerPolicyTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerPolicyTypesResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerPolicyTypesResponse)

instance
  Prelude.NFData
    DescribeLoadBalancerPolicyTypesResponse
