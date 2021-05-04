{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELB.CreateLoadBalancerPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a policy with the specified attributes for the specified load
-- balancer.
--
-- Policies are settings that are saved for your load balancer and that can
-- be applied to the listener or the application server, depending on the
-- policy type.
module Network.AWS.ELB.CreateLoadBalancerPolicy
  ( -- * Creating a Request
    CreateLoadBalancerPolicy (..),
    newCreateLoadBalancerPolicy,

    -- * Request Lenses
    createLoadBalancerPolicy_policyAttributes,
    createLoadBalancerPolicy_loadBalancerName,
    createLoadBalancerPolicy_policyName,
    createLoadBalancerPolicy_policyTypeName,

    -- * Destructuring the Response
    CreateLoadBalancerPolicyResponse (..),
    newCreateLoadBalancerPolicyResponse,

    -- * Response Lenses
    createLoadBalancerPolicyResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerPolicy.
--
-- /See:/ 'newCreateLoadBalancerPolicy' smart constructor.
data CreateLoadBalancerPolicy = CreateLoadBalancerPolicy'
  { -- | The policy attributes.
    policyAttributes :: Prelude.Maybe [PolicyAttribute],
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The name of the load balancer policy to be created. This name must be
    -- unique within the set of policies for this load balancer.
    policyName :: Prelude.Text,
    -- | The name of the base policy type. To get the list of policy types, use
    -- DescribeLoadBalancerPolicyTypes.
    policyTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyAttributes', 'createLoadBalancerPolicy_policyAttributes' - The policy attributes.
--
-- 'loadBalancerName', 'createLoadBalancerPolicy_loadBalancerName' - The name of the load balancer.
--
-- 'policyName', 'createLoadBalancerPolicy_policyName' - The name of the load balancer policy to be created. This name must be
-- unique within the set of policies for this load balancer.
--
-- 'policyTypeName', 'createLoadBalancerPolicy_policyTypeName' - The name of the base policy type. To get the list of policy types, use
-- DescribeLoadBalancerPolicyTypes.
newCreateLoadBalancerPolicy ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  -- | 'policyTypeName'
  Prelude.Text ->
  CreateLoadBalancerPolicy
newCreateLoadBalancerPolicy
  pLoadBalancerName_
  pPolicyName_
  pPolicyTypeName_ =
    CreateLoadBalancerPolicy'
      { policyAttributes =
          Prelude.Nothing,
        loadBalancerName = pLoadBalancerName_,
        policyName = pPolicyName_,
        policyTypeName = pPolicyTypeName_
      }

-- | The policy attributes.
createLoadBalancerPolicy_policyAttributes :: Lens.Lens' CreateLoadBalancerPolicy (Prelude.Maybe [PolicyAttribute])
createLoadBalancerPolicy_policyAttributes = Lens.lens (\CreateLoadBalancerPolicy' {policyAttributes} -> policyAttributes) (\s@CreateLoadBalancerPolicy' {} a -> s {policyAttributes = a} :: CreateLoadBalancerPolicy) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the load balancer.
createLoadBalancerPolicy_loadBalancerName :: Lens.Lens' CreateLoadBalancerPolicy Prelude.Text
createLoadBalancerPolicy_loadBalancerName = Lens.lens (\CreateLoadBalancerPolicy' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancerPolicy' {} a -> s {loadBalancerName = a} :: CreateLoadBalancerPolicy)

-- | The name of the load balancer policy to be created. This name must be
-- unique within the set of policies for this load balancer.
createLoadBalancerPolicy_policyName :: Lens.Lens' CreateLoadBalancerPolicy Prelude.Text
createLoadBalancerPolicy_policyName = Lens.lens (\CreateLoadBalancerPolicy' {policyName} -> policyName) (\s@CreateLoadBalancerPolicy' {} a -> s {policyName = a} :: CreateLoadBalancerPolicy)

-- | The name of the base policy type. To get the list of policy types, use
-- DescribeLoadBalancerPolicyTypes.
createLoadBalancerPolicy_policyTypeName :: Lens.Lens' CreateLoadBalancerPolicy Prelude.Text
createLoadBalancerPolicy_policyTypeName = Lens.lens (\CreateLoadBalancerPolicy' {policyTypeName} -> policyTypeName) (\s@CreateLoadBalancerPolicy' {} a -> s {policyTypeName = a} :: CreateLoadBalancerPolicy)

instance Prelude.AWSRequest CreateLoadBalancerPolicy where
  type
    Rs CreateLoadBalancerPolicy =
      CreateLoadBalancerPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerPolicyResult"
      ( \s h x ->
          CreateLoadBalancerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancerPolicy

instance Prelude.NFData CreateLoadBalancerPolicy

instance Prelude.ToHeaders CreateLoadBalancerPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateLoadBalancerPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateLoadBalancerPolicy where
  toQuery CreateLoadBalancerPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateLoadBalancerPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "PolicyAttributes"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> policyAttributes
            ),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "PolicyName" Prelude.=: policyName,
        "PolicyTypeName" Prelude.=: policyTypeName
      ]

-- | Contains the output of CreateLoadBalancerPolicy.
--
-- /See:/ 'newCreateLoadBalancerPolicyResponse' smart constructor.
data CreateLoadBalancerPolicyResponse = CreateLoadBalancerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLoadBalancerPolicyResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoadBalancerPolicyResponse
newCreateLoadBalancerPolicyResponse pHttpStatus_ =
  CreateLoadBalancerPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createLoadBalancerPolicyResponse_httpStatus :: Lens.Lens' CreateLoadBalancerPolicyResponse Prelude.Int
createLoadBalancerPolicyResponse_httpStatus = Lens.lens (\CreateLoadBalancerPolicyResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerPolicyResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerPolicyResponse)

instance
  Prelude.NFData
    CreateLoadBalancerPolicyResponse
