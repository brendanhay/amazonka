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
-- Module      : Network.AWS.ELB.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of the specified load balancer.
--
-- You can modify the load balancer attributes, such as @AccessLogs@,
-- @ConnectionDraining@, and @CrossZoneLoadBalancing@ by either enabling or
-- disabling them. Or, you can modify the load balancer attribute
-- @ConnectionSettings@ by specifying an idle connection timeout value for
-- your load balancer.
--
-- For more information, see the following in the /Classic Load Balancers
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Cross-Zone Load Balancing>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Connection Draining>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/access-log-collection.html Access Logs>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Idle Connection Timeout>
module Network.AWS.ELB.ModifyLoadBalancerAttributes
  ( -- * Creating a Request
    ModifyLoadBalancerAttributes (..),
    newModifyLoadBalancerAttributes,

    -- * Request Lenses
    modifyLoadBalancerAttributes_loadBalancerName,
    modifyLoadBalancerAttributes_loadBalancerAttributes,

    -- * Destructuring the Response
    ModifyLoadBalancerAttributesResponse (..),
    newModifyLoadBalancerAttributesResponse,

    -- * Response Lenses
    modifyLoadBalancerAttributesResponse_loadBalancerAttributes,
    modifyLoadBalancerAttributesResponse_loadBalancerName,
    modifyLoadBalancerAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for ModifyLoadBalancerAttributes.
--
-- /See:/ 'newModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The attributes for the load balancer.
    loadBalancerAttributes :: LoadBalancerAttributes
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyLoadBalancerAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'modifyLoadBalancerAttributes_loadBalancerName' - The name of the load balancer.
--
-- 'loadBalancerAttributes', 'modifyLoadBalancerAttributes_loadBalancerAttributes' - The attributes for the load balancer.
newModifyLoadBalancerAttributes ::
  -- | 'loadBalancerName'
  Core.Text ->
  -- | 'loadBalancerAttributes'
  LoadBalancerAttributes ->
  ModifyLoadBalancerAttributes
newModifyLoadBalancerAttributes
  pLoadBalancerName_
  pLoadBalancerAttributes_ =
    ModifyLoadBalancerAttributes'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerAttributes =
          pLoadBalancerAttributes_
      }

-- | The name of the load balancer.
modifyLoadBalancerAttributes_loadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributes Core.Text
modifyLoadBalancerAttributes_loadBalancerName = Lens.lens (\ModifyLoadBalancerAttributes' {loadBalancerName} -> loadBalancerName) (\s@ModifyLoadBalancerAttributes' {} a -> s {loadBalancerName = a} :: ModifyLoadBalancerAttributes)

-- | The attributes for the load balancer.
modifyLoadBalancerAttributes_loadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributes LoadBalancerAttributes
modifyLoadBalancerAttributes_loadBalancerAttributes = Lens.lens (\ModifyLoadBalancerAttributes' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@ModifyLoadBalancerAttributes' {} a -> s {loadBalancerAttributes = a} :: ModifyLoadBalancerAttributes)

instance Core.AWSRequest ModifyLoadBalancerAttributes where
  type
    AWSResponse ModifyLoadBalancerAttributes =
      ModifyLoadBalancerAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyLoadBalancerAttributesResult"
      ( \s h x ->
          ModifyLoadBalancerAttributesResponse'
            Core.<$> (x Core..@? "LoadBalancerAttributes")
            Core.<*> (x Core..@? "LoadBalancerName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyLoadBalancerAttributes

instance Core.NFData ModifyLoadBalancerAttributes

instance Core.ToHeaders ModifyLoadBalancerAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyLoadBalancerAttributes where
  toPath = Core.const "/"

instance Core.ToQuery ModifyLoadBalancerAttributes where
  toQuery ModifyLoadBalancerAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyLoadBalancerAttributes" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "LoadBalancerAttributes"
          Core.=: loadBalancerAttributes
      ]

-- | Contains the output of ModifyLoadBalancerAttributes.
--
-- /See:/ 'newModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Core.Maybe LoadBalancerAttributes,
    -- | The name of the load balancer.
    loadBalancerName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyLoadBalancerAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerAttributes', 'modifyLoadBalancerAttributesResponse_loadBalancerAttributes' - Information about the load balancer attributes.
--
-- 'loadBalancerName', 'modifyLoadBalancerAttributesResponse_loadBalancerName' - The name of the load balancer.
--
-- 'httpStatus', 'modifyLoadBalancerAttributesResponse_httpStatus' - The response's http status code.
newModifyLoadBalancerAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyLoadBalancerAttributesResponse
newModifyLoadBalancerAttributesResponse pHttpStatus_ =
  ModifyLoadBalancerAttributesResponse'
    { loadBalancerAttributes =
        Core.Nothing,
      loadBalancerName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancer attributes.
modifyLoadBalancerAttributesResponse_loadBalancerAttributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe LoadBalancerAttributes)
modifyLoadBalancerAttributesResponse_loadBalancerAttributes = Lens.lens (\ModifyLoadBalancerAttributesResponse' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {loadBalancerAttributes = a} :: ModifyLoadBalancerAttributesResponse)

-- | The name of the load balancer.
modifyLoadBalancerAttributesResponse_loadBalancerName :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe Core.Text)
modifyLoadBalancerAttributesResponse_loadBalancerName = Lens.lens (\ModifyLoadBalancerAttributesResponse' {loadBalancerName} -> loadBalancerName) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {loadBalancerName = a} :: ModifyLoadBalancerAttributesResponse)

-- | The response's http status code.
modifyLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Core.Int
modifyLoadBalancerAttributesResponse_httpStatus = Lens.lens (\ModifyLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: ModifyLoadBalancerAttributesResponse)

instance
  Core.NFData
    ModifyLoadBalancerAttributesResponse
