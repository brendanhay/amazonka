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
-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a
-- listener with the specified port does not already exist, it is created;
-- otherwise, the properties of the new listener must match the properties
-- of the existing listener.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.CreateLoadBalancerListeners
  ( -- * Creating a Request
    CreateLoadBalancerListeners (..),
    newCreateLoadBalancerListeners,

    -- * Request Lenses
    createLoadBalancerListeners_loadBalancerName,
    createLoadBalancerListeners_listeners,

    -- * Destructuring the Response
    CreateLoadBalancerListenersResponse (..),
    newCreateLoadBalancerListenersResponse,

    -- * Response Lenses
    createLoadBalancerListenersResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerListeners.
--
-- /See:/ 'newCreateLoadBalancerListeners' smart constructor.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The listeners.
    listeners :: [Listener]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerListeners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'createLoadBalancerListeners_loadBalancerName' - The name of the load balancer.
--
-- 'listeners', 'createLoadBalancerListeners_listeners' - The listeners.
newCreateLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  CreateLoadBalancerListeners
newCreateLoadBalancerListeners pLoadBalancerName_ =
  CreateLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      listeners = Prelude.mempty
    }

-- | The name of the load balancer.
createLoadBalancerListeners_loadBalancerName :: Lens.Lens' CreateLoadBalancerListeners Prelude.Text
createLoadBalancerListeners_loadBalancerName = Lens.lens (\CreateLoadBalancerListeners' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancerListeners' {} a -> s {loadBalancerName = a} :: CreateLoadBalancerListeners)

-- | The listeners.
createLoadBalancerListeners_listeners :: Lens.Lens' CreateLoadBalancerListeners [Listener]
createLoadBalancerListeners_listeners = Lens.lens (\CreateLoadBalancerListeners' {listeners} -> listeners) (\s@CreateLoadBalancerListeners' {} a -> s {listeners = a} :: CreateLoadBalancerListeners) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    CreateLoadBalancerListeners
  where
  type
    Rs CreateLoadBalancerListeners =
      CreateLoadBalancerListenersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerListenersResult"
      ( \s h x ->
          CreateLoadBalancerListenersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancerListeners

instance Prelude.NFData CreateLoadBalancerListeners

instance
  Prelude.ToHeaders
    CreateLoadBalancerListeners
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateLoadBalancerListeners where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateLoadBalancerListeners where
  toQuery CreateLoadBalancerListeners' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateLoadBalancerListeners" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName,
        "Listeners"
          Prelude.=: Prelude.toQueryList "member" listeners
      ]

-- | Contains the parameters for CreateLoadBalancerListener.
--
-- /See:/ 'newCreateLoadBalancerListenersResponse' smart constructor.
data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerListenersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLoadBalancerListenersResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerListenersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoadBalancerListenersResponse
newCreateLoadBalancerListenersResponse pHttpStatus_ =
  CreateLoadBalancerListenersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createLoadBalancerListenersResponse_httpStatus :: Lens.Lens' CreateLoadBalancerListenersResponse Prelude.Int
createLoadBalancerListenersResponse_httpStatus = Lens.lens (\CreateLoadBalancerListenersResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerListenersResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerListenersResponse)

instance
  Prelude.NFData
    CreateLoadBalancerListenersResponse
