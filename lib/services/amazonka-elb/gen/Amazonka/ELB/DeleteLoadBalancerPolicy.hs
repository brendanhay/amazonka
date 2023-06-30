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
-- Module      : Amazonka.ELB.DeleteLoadBalancerPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy from the specified load balancer. This
-- policy must not be enabled for any listeners.
module Amazonka.ELB.DeleteLoadBalancerPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteLoadBalancerPolicy.
--
-- /See:/ 'newDeleteLoadBalancerPolicy' smart constructor.
data DeleteLoadBalancerPolicy = DeleteLoadBalancerPolicy'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The name of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteLoadBalancerPolicy where
  type
    AWSResponse DeleteLoadBalancerPolicy =
      DeleteLoadBalancerPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerPolicyResult"
      ( \s h x ->
          DeleteLoadBalancerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancerPolicy where
  hashWithSalt _salt DeleteLoadBalancerPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` policyName

instance Prelude.NFData DeleteLoadBalancerPolicy where
  rnf DeleteLoadBalancerPolicy' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf policyName

instance Data.ToHeaders DeleteLoadBalancerPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteLoadBalancerPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLoadBalancerPolicy where
  toQuery DeleteLoadBalancerPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteLoadBalancerPolicy" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "PolicyName" Data.=: policyName
      ]

-- | Contains the output of DeleteLoadBalancerPolicy.
--
-- /See:/ 'newDeleteLoadBalancerPolicyResponse' smart constructor.
data DeleteLoadBalancerPolicyResponse = DeleteLoadBalancerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteLoadBalancerPolicyResponse' {..} =
    Prelude.rnf httpStatus
