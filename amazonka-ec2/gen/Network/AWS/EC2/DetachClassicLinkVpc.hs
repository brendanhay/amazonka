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
-- Module      : Network.AWS.EC2.DetachClassicLinkVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the
-- instance has been unlinked, the VPC security groups are no longer
-- associated with it. An instance is automatically unlinked from a VPC
-- when it\'s stopped.
module Network.AWS.EC2.DetachClassicLinkVpc
  ( -- * Creating a Request
    DetachClassicLinkVpc (..),
    newDetachClassicLinkVpc,

    -- * Request Lenses
    detachClassicLinkVpc_dryRun,
    detachClassicLinkVpc_instanceId,
    detachClassicLinkVpc_vpcId,

    -- * Destructuring the Response
    DetachClassicLinkVpcResponse (..),
    newDetachClassicLinkVpcResponse,

    -- * Response Lenses
    detachClassicLinkVpcResponse_return,
    detachClassicLinkVpcResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachClassicLinkVpc' smart constructor.
data DetachClassicLinkVpc = DetachClassicLinkVpc'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the instance to unlink from the VPC.
    instanceId :: Core.Text,
    -- | The ID of the VPC to which the instance is linked.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachClassicLinkVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachClassicLinkVpc_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceId', 'detachClassicLinkVpc_instanceId' - The ID of the instance to unlink from the VPC.
--
-- 'vpcId', 'detachClassicLinkVpc_vpcId' - The ID of the VPC to which the instance is linked.
newDetachClassicLinkVpc ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'vpcId'
  Core.Text ->
  DetachClassicLinkVpc
newDetachClassicLinkVpc pInstanceId_ pVpcId_ =
  DetachClassicLinkVpc'
    { dryRun = Core.Nothing,
      instanceId = pInstanceId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachClassicLinkVpc_dryRun :: Lens.Lens' DetachClassicLinkVpc (Core.Maybe Core.Bool)
detachClassicLinkVpc_dryRun = Lens.lens (\DetachClassicLinkVpc' {dryRun} -> dryRun) (\s@DetachClassicLinkVpc' {} a -> s {dryRun = a} :: DetachClassicLinkVpc)

-- | The ID of the instance to unlink from the VPC.
detachClassicLinkVpc_instanceId :: Lens.Lens' DetachClassicLinkVpc Core.Text
detachClassicLinkVpc_instanceId = Lens.lens (\DetachClassicLinkVpc' {instanceId} -> instanceId) (\s@DetachClassicLinkVpc' {} a -> s {instanceId = a} :: DetachClassicLinkVpc)

-- | The ID of the VPC to which the instance is linked.
detachClassicLinkVpc_vpcId :: Lens.Lens' DetachClassicLinkVpc Core.Text
detachClassicLinkVpc_vpcId = Lens.lens (\DetachClassicLinkVpc' {vpcId} -> vpcId) (\s@DetachClassicLinkVpc' {} a -> s {vpcId = a} :: DetachClassicLinkVpc)

instance Core.AWSRequest DetachClassicLinkVpc where
  type
    AWSResponse DetachClassicLinkVpc =
      DetachClassicLinkVpcResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DetachClassicLinkVpcResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachClassicLinkVpc

instance Core.NFData DetachClassicLinkVpc

instance Core.ToHeaders DetachClassicLinkVpc where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachClassicLinkVpc where
  toPath = Core.const "/"

instance Core.ToQuery DetachClassicLinkVpc where
  toQuery DetachClassicLinkVpc' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DetachClassicLinkVpc" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InstanceId" Core.=: instanceId,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newDetachClassicLinkVpcResponse' smart constructor.
data DetachClassicLinkVpcResponse = DetachClassicLinkVpcResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachClassicLinkVpcResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'detachClassicLinkVpcResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'detachClassicLinkVpcResponse_httpStatus' - The response's http status code.
newDetachClassicLinkVpcResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachClassicLinkVpcResponse
newDetachClassicLinkVpcResponse pHttpStatus_ =
  DetachClassicLinkVpcResponse'
    { return' =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
detachClassicLinkVpcResponse_return :: Lens.Lens' DetachClassicLinkVpcResponse (Core.Maybe Core.Bool)
detachClassicLinkVpcResponse_return = Lens.lens (\DetachClassicLinkVpcResponse' {return'} -> return') (\s@DetachClassicLinkVpcResponse' {} a -> s {return' = a} :: DetachClassicLinkVpcResponse)

-- | The response's http status code.
detachClassicLinkVpcResponse_httpStatus :: Lens.Lens' DetachClassicLinkVpcResponse Core.Int
detachClassicLinkVpcResponse_httpStatus = Lens.lens (\DetachClassicLinkVpcResponse' {httpStatus} -> httpStatus) (\s@DetachClassicLinkVpcResponse' {} a -> s {httpStatus = a} :: DetachClassicLinkVpcResponse)

instance Core.NFData DetachClassicLinkVpcResponse
