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
-- Module      : Network.AWS.ELBv2.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified Application Load
-- Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- If any of the specified attributes can\'t be modified as requested, the
-- call fails. Any existing attributes that you do not modify retain their
-- current values.
module Network.AWS.ELBv2.ModifyLoadBalancerAttributes
  ( -- * Creating a Request
    ModifyLoadBalancerAttributes (..),
    newModifyLoadBalancerAttributes,

    -- * Request Lenses
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,

    -- * Destructuring the Response
    ModifyLoadBalancerAttributesResponse (..),
    newModifyLoadBalancerAttributesResponse,

    -- * Response Lenses
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Text,
    -- | The load balancer attributes.
    attributes :: [LoadBalancerAttribute]
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
-- 'loadBalancerArn', 'modifyLoadBalancerAttributes_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'attributes', 'modifyLoadBalancerAttributes_attributes' - The load balancer attributes.
newModifyLoadBalancerAttributes ::
  -- | 'loadBalancerArn'
  Core.Text ->
  ModifyLoadBalancerAttributes
newModifyLoadBalancerAttributes pLoadBalancerArn_ =
  ModifyLoadBalancerAttributes'
    { loadBalancerArn =
        pLoadBalancerArn_,
      attributes = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
modifyLoadBalancerAttributes_loadBalancerArn :: Lens.Lens' ModifyLoadBalancerAttributes Core.Text
modifyLoadBalancerAttributes_loadBalancerArn = Lens.lens (\ModifyLoadBalancerAttributes' {loadBalancerArn} -> loadBalancerArn) (\s@ModifyLoadBalancerAttributes' {} a -> s {loadBalancerArn = a} :: ModifyLoadBalancerAttributes)

-- | The load balancer attributes.
modifyLoadBalancerAttributes_attributes :: Lens.Lens' ModifyLoadBalancerAttributes [LoadBalancerAttribute]
modifyLoadBalancerAttributes_attributes = Lens.lens (\ModifyLoadBalancerAttributes' {attributes} -> attributes) (\s@ModifyLoadBalancerAttributes' {} a -> s {attributes = a} :: ModifyLoadBalancerAttributes) Core.. Lens._Coerce

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
            Core.<$> ( x Core..@? "Attributes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
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
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn,
        "Attributes"
          Core.=: Core.toQueryList "member" attributes
      ]

-- | /See:/ 'newModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    attributes :: Core.Maybe [LoadBalancerAttribute],
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
-- 'attributes', 'modifyLoadBalancerAttributesResponse_attributes' - Information about the load balancer attributes.
--
-- 'httpStatus', 'modifyLoadBalancerAttributesResponse_httpStatus' - The response's http status code.
newModifyLoadBalancerAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyLoadBalancerAttributesResponse
newModifyLoadBalancerAttributesResponse pHttpStatus_ =
  ModifyLoadBalancerAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancer attributes.
modifyLoadBalancerAttributesResponse_attributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Core.Maybe [LoadBalancerAttribute])
modifyLoadBalancerAttributesResponse_attributes = Lens.lens (\ModifyLoadBalancerAttributesResponse' {attributes} -> attributes) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {attributes = a} :: ModifyLoadBalancerAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Core.Int
modifyLoadBalancerAttributesResponse_httpStatus = Lens.lens (\ModifyLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: ModifyLoadBalancerAttributesResponse)

instance
  Core.NFData
    ModifyLoadBalancerAttributesResponse
