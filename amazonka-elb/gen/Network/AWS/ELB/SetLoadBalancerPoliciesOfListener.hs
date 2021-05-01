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
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the current set of policies for the specified load balancer
-- port with the specified set of policies.
--
-- To enable back-end server authentication, use
-- SetLoadBalancerPoliciesForBackendServer.
--
-- For more information about setting policies, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/ssl-config-update.html Update the SSL Negotiation Configuration>,
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-duration Duration-Based Session Stickiness>,
-- and
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-sticky-sessions.html#enable-sticky-sessions-application Application-Controlled Session Stickiness>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.SetLoadBalancerPoliciesOfListener
  ( -- * Creating a Request
    SetLoadBalancerPoliciesOfListener (..),
    newSetLoadBalancerPoliciesOfListener,

    -- * Request Lenses
    setLoadBalancerPoliciesOfListener_loadBalancerName,
    setLoadBalancerPoliciesOfListener_loadBalancerPort,
    setLoadBalancerPoliciesOfListener_policyNames,

    -- * Destructuring the Response
    SetLoadBalancerPoliciesOfListenerResponse (..),
    newSetLoadBalancerPoliciesOfListenerResponse,

    -- * Response Lenses
    setLoadBalancerPoliciesOfListenerResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'newSetLoadBalancerPoliciesOfListener' smart constructor.
data SetLoadBalancerPoliciesOfListener = SetLoadBalancerPoliciesOfListener'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The external port of the load balancer.
    loadBalancerPort :: Prelude.Int,
    -- | The names of the policies. This list must include all policies to be
    -- enabled. If you omit a policy that is currently enabled, it is disabled.
    -- If the list is empty, all current policies are disabled.
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBalancerPoliciesOfListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'setLoadBalancerPoliciesOfListener_loadBalancerName' - The name of the load balancer.
--
-- 'loadBalancerPort', 'setLoadBalancerPoliciesOfListener_loadBalancerPort' - The external port of the load balancer.
--
-- 'policyNames', 'setLoadBalancerPoliciesOfListener_policyNames' - The names of the policies. This list must include all policies to be
-- enabled. If you omit a policy that is currently enabled, it is disabled.
-- If the list is empty, all current policies are disabled.
newSetLoadBalancerPoliciesOfListener ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'loadBalancerPort'
  Prelude.Int ->
  SetLoadBalancerPoliciesOfListener
newSetLoadBalancerPoliciesOfListener
  pLoadBalancerName_
  pLoadBalancerPort_ =
    SetLoadBalancerPoliciesOfListener'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerPort = pLoadBalancerPort_,
        policyNames = Prelude.mempty
      }

-- | The name of the load balancer.
setLoadBalancerPoliciesOfListener_loadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesOfListener Prelude.Text
setLoadBalancerPoliciesOfListener_loadBalancerName = Lens.lens (\SetLoadBalancerPoliciesOfListener' {loadBalancerName} -> loadBalancerName) (\s@SetLoadBalancerPoliciesOfListener' {} a -> s {loadBalancerName = a} :: SetLoadBalancerPoliciesOfListener)

-- | The external port of the load balancer.
setLoadBalancerPoliciesOfListener_loadBalancerPort :: Lens.Lens' SetLoadBalancerPoliciesOfListener Prelude.Int
setLoadBalancerPoliciesOfListener_loadBalancerPort = Lens.lens (\SetLoadBalancerPoliciesOfListener' {loadBalancerPort} -> loadBalancerPort) (\s@SetLoadBalancerPoliciesOfListener' {} a -> s {loadBalancerPort = a} :: SetLoadBalancerPoliciesOfListener)

-- | The names of the policies. This list must include all policies to be
-- enabled. If you omit a policy that is currently enabled, it is disabled.
-- If the list is empty, all current policies are disabled.
setLoadBalancerPoliciesOfListener_policyNames :: Lens.Lens' SetLoadBalancerPoliciesOfListener [Prelude.Text]
setLoadBalancerPoliciesOfListener_policyNames = Lens.lens (\SetLoadBalancerPoliciesOfListener' {policyNames} -> policyNames) (\s@SetLoadBalancerPoliciesOfListener' {} a -> s {policyNames = a} :: SetLoadBalancerPoliciesOfListener) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    SetLoadBalancerPoliciesOfListener
  where
  type
    Rs SetLoadBalancerPoliciesOfListener =
      SetLoadBalancerPoliciesOfListenerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerPoliciesOfListenerResult"
      ( \s h x ->
          SetLoadBalancerPoliciesOfListenerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetLoadBalancerPoliciesOfListener

instance
  Prelude.NFData
    SetLoadBalancerPoliciesOfListener

instance
  Prelude.ToHeaders
    SetLoadBalancerPoliciesOfListener
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    SetLoadBalancerPoliciesOfListener
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    SetLoadBalancerPoliciesOfListener
  where
  toQuery SetLoadBalancerPoliciesOfListener' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "SetLoadBalancerPoliciesOfListener" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "LoadBalancerPort" Prelude.=: loadBalancerPort,
        "PolicyNames"
          Prelude.=: Prelude.toQueryList "member" policyNames
      ]

-- | Contains the output of SetLoadBalancePoliciesOfListener.
--
-- /See:/ 'newSetLoadBalancerPoliciesOfListenerResponse' smart constructor.
data SetLoadBalancerPoliciesOfListenerResponse = SetLoadBalancerPoliciesOfListenerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBalancerPoliciesOfListenerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setLoadBalancerPoliciesOfListenerResponse_httpStatus' - The response's http status code.
newSetLoadBalancerPoliciesOfListenerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetLoadBalancerPoliciesOfListenerResponse
newSetLoadBalancerPoliciesOfListenerResponse
  pHttpStatus_ =
    SetLoadBalancerPoliciesOfListenerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setLoadBalancerPoliciesOfListenerResponse_httpStatus :: Lens.Lens' SetLoadBalancerPoliciesOfListenerResponse Prelude.Int
setLoadBalancerPoliciesOfListenerResponse_httpStatus = Lens.lens (\SetLoadBalancerPoliciesOfListenerResponse' {httpStatus} -> httpStatus) (\s@SetLoadBalancerPoliciesOfListenerResponse' {} a -> s {httpStatus = a} :: SetLoadBalancerPoliciesOfListenerResponse)

instance
  Prelude.NFData
    SetLoadBalancerPoliciesOfListenerResponse
