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
-- Module      : Network.AWS.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This
-- policy must not be enabled for any listeners.
module Network.AWS.ELB.DeleteLoadBalancerPolicy
  ( -- * Creating a Request
    DeleteLoadBalancerPolicy (..),
    newDeleteLoadBalancerPolicy,

    -- * Request Lenses
    deleteLoadBalancerPolicy_loadBalancerName,
    deleteLoadBalancerPolicy_policyName,

    -- * Destructuring the Response
    DeleteLoadBalancerPolicyResponse (..),
    newDeleteLoadBalancerPolicyResponse,

    -- * Response Lenses
    deleteLoadBalancerPolicyResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancerPolicy.
--
-- /See:/ 'newDeleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'deleteLoadBalancerPolicy_loadBalancerName' - The name of the load balancer.
--
-- 'policyName', 'deleteLoadBalancerPolicy_policyName' - The name of the policy.
newDeleteLoadBalancerPolicy ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'policyName'
  Prelude.Text ->
  DeleteLoadBalancerPolicy
newDeleteLoadBalancerPolicy
  pLoadBalancerName_
  pPolicyName_ =
    DeleteLoadBalancerPolicy'
      { loadBalancerName =
          pLoadBalancerName_,
        policyName = pPolicyName_
      }

-- | The name of the load balancer.
deleteLoadBalancerPolicy_loadBalancerName :: Lens.Lens' DeleteLoadBalancerPolicy Prelude.Text
deleteLoadBalancerPolicy_loadBalancerName = Lens.lens (\DeleteLoadBalancerPolicy' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancerPolicy' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancerPolicy)

-- | The name of the policy.
deleteLoadBalancerPolicy_policyName :: Lens.Lens' DeleteLoadBalancerPolicy Prelude.Text
deleteLoadBalancerPolicy_policyName = Lens.lens (\DeleteLoadBalancerPolicy' {policyName} -> policyName) (\s@DeleteLoadBalancerPolicy' {} a -> s {policyName = a} :: DeleteLoadBalancerPolicy)

instance Prelude.AWSRequest DeleteLoadBalancerPolicy where
  type
    Rs DeleteLoadBalancerPolicy =
      DeleteLoadBalancerPolicyResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerPolicyResult"
      ( \s h x ->
          DeleteLoadBalancerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancerPolicy

instance Prelude.NFData DeleteLoadBalancerPolicy

instance Prelude.ToHeaders DeleteLoadBalancerPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLoadBalancerPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLoadBalancerPolicy where
  toQuery DeleteLoadBalancerPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLoadBalancerPolicy" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "PolicyName" Prelude.=: policyName
      ]

-- | Contains the output of DeleteLoadBalancerPolicy.
--
-- /See:/ 'newDeleteLoadBalancerPolicyResponse' smart constructor.
data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoadBalancerPolicyResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoadBalancerPolicyResponse
newDeleteLoadBalancerPolicyResponse pHttpStatus_ =
  DeleteLoadBalancerPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerPolicyResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerPolicyResponse Prelude.Int
deleteLoadBalancerPolicyResponse_httpStatus = Lens.lens (\DeleteLoadBalancerPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerPolicyResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerPolicyResponse)

instance
  Prelude.NFData
    DeleteLoadBalancerPolicyResponse
