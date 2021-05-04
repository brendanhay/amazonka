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
-- Module      : Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Replaces the set of policies associated with the specified port on which
-- the EC2 instance is listening with a new set of policies. At this time,
-- only the back-end server authentication policy type can be applied to
-- the instance ports; this policy type is composed of multiple public key
-- policies.
--
-- Each time you use @SetLoadBalancerPoliciesForBackendServer@ to enable
-- the policies, use the @PolicyNames@ parameter to list the policies that
-- you want to enable.
--
-- You can use DescribeLoadBalancers or DescribeLoadBalancerPolicies to
-- verify that the policy is associated with the EC2 instance.
--
-- For more information about enabling back-end instance authentication,
-- see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-create-https-ssl-load-balancer.html#configure_backendauth_clt Configure Back-end Instance Authentication>
-- in the /Classic Load Balancers Guide/. For more information about Proxy
-- Protocol, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-proxy-protocol.html Configure Proxy Protocol Support>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.SetLoadBalancerPoliciesForBackendServer
  ( -- * Creating a Request
    SetLoadBalancerPoliciesForBackendServer (..),
    newSetLoadBalancerPoliciesForBackendServer,

    -- * Request Lenses
    setLoadBalancerPoliciesForBackendServer_loadBalancerName,
    setLoadBalancerPoliciesForBackendServer_instancePort,
    setLoadBalancerPoliciesForBackendServer_policyNames,

    -- * Destructuring the Response
    SetLoadBalancerPoliciesForBackendServerResponse (..),
    newSetLoadBalancerPoliciesForBackendServerResponse,

    -- * Response Lenses
    setLoadBalancerPoliciesForBackendServerResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'newSetLoadBalancerPoliciesForBackendServer' smart constructor.
data SetLoadBalancerPoliciesForBackendServer = SetLoadBalancerPoliciesForBackendServer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The port number associated with the EC2 instance.
    instancePort :: Prelude.Int,
    -- | The names of the policies. If the list is empty, then all current
    -- polices are removed from the EC2 instance.
    policyNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBalancerPoliciesForBackendServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'setLoadBalancerPoliciesForBackendServer_loadBalancerName' - The name of the load balancer.
--
-- 'instancePort', 'setLoadBalancerPoliciesForBackendServer_instancePort' - The port number associated with the EC2 instance.
--
-- 'policyNames', 'setLoadBalancerPoliciesForBackendServer_policyNames' - The names of the policies. If the list is empty, then all current
-- polices are removed from the EC2 instance.
newSetLoadBalancerPoliciesForBackendServer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'instancePort'
  Prelude.Int ->
  SetLoadBalancerPoliciesForBackendServer
newSetLoadBalancerPoliciesForBackendServer
  pLoadBalancerName_
  pInstancePort_ =
    SetLoadBalancerPoliciesForBackendServer'
      { loadBalancerName =
          pLoadBalancerName_,
        instancePort = pInstancePort_,
        policyNames = Prelude.mempty
      }

-- | The name of the load balancer.
setLoadBalancerPoliciesForBackendServer_loadBalancerName :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Prelude.Text
setLoadBalancerPoliciesForBackendServer_loadBalancerName = Lens.lens (\SetLoadBalancerPoliciesForBackendServer' {loadBalancerName} -> loadBalancerName) (\s@SetLoadBalancerPoliciesForBackendServer' {} a -> s {loadBalancerName = a} :: SetLoadBalancerPoliciesForBackendServer)

-- | The port number associated with the EC2 instance.
setLoadBalancerPoliciesForBackendServer_instancePort :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer Prelude.Int
setLoadBalancerPoliciesForBackendServer_instancePort = Lens.lens (\SetLoadBalancerPoliciesForBackendServer' {instancePort} -> instancePort) (\s@SetLoadBalancerPoliciesForBackendServer' {} a -> s {instancePort = a} :: SetLoadBalancerPoliciesForBackendServer)

-- | The names of the policies. If the list is empty, then all current
-- polices are removed from the EC2 instance.
setLoadBalancerPoliciesForBackendServer_policyNames :: Lens.Lens' SetLoadBalancerPoliciesForBackendServer [Prelude.Text]
setLoadBalancerPoliciesForBackendServer_policyNames = Lens.lens (\SetLoadBalancerPoliciesForBackendServer' {policyNames} -> policyNames) (\s@SetLoadBalancerPoliciesForBackendServer' {} a -> s {policyNames = a} :: SetLoadBalancerPoliciesForBackendServer) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    SetLoadBalancerPoliciesForBackendServer
  where
  type
    Rs SetLoadBalancerPoliciesForBackendServer =
      SetLoadBalancerPoliciesForBackendServerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerPoliciesForBackendServerResult"
      ( \s h x ->
          SetLoadBalancerPoliciesForBackendServerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetLoadBalancerPoliciesForBackendServer

instance
  Prelude.NFData
    SetLoadBalancerPoliciesForBackendServer

instance
  Prelude.ToHeaders
    SetLoadBalancerPoliciesForBackendServer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    SetLoadBalancerPoliciesForBackendServer
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    SetLoadBalancerPoliciesForBackendServer
  where
  toQuery SetLoadBalancerPoliciesForBackendServer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "SetLoadBalancerPoliciesForBackendServer" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "InstancePort" Prelude.=: instancePort,
        "PolicyNames"
          Prelude.=: Prelude.toQueryList "member" policyNames
      ]

-- | Contains the output of SetLoadBalancerPoliciesForBackendServer.
--
-- /See:/ 'newSetLoadBalancerPoliciesForBackendServerResponse' smart constructor.
data SetLoadBalancerPoliciesForBackendServerResponse = SetLoadBalancerPoliciesForBackendServerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SetLoadBalancerPoliciesForBackendServerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setLoadBalancerPoliciesForBackendServerResponse_httpStatus' - The response's http status code.
newSetLoadBalancerPoliciesForBackendServerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetLoadBalancerPoliciesForBackendServerResponse
newSetLoadBalancerPoliciesForBackendServerResponse
  pHttpStatus_ =
    SetLoadBalancerPoliciesForBackendServerResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setLoadBalancerPoliciesForBackendServerResponse_httpStatus :: Lens.Lens' SetLoadBalancerPoliciesForBackendServerResponse Prelude.Int
setLoadBalancerPoliciesForBackendServerResponse_httpStatus = Lens.lens (\SetLoadBalancerPoliciesForBackendServerResponse' {httpStatus} -> httpStatus) (\s@SetLoadBalancerPoliciesForBackendServerResponse' {} a -> s {httpStatus = a} :: SetLoadBalancerPoliciesForBackendServerResponse)

instance
  Prelude.NFData
    SetLoadBalancerPoliciesForBackendServerResponse
