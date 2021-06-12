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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a target for your Traffic Mirror session.
--
-- A Traffic Mirror target is the destination for mirrored traffic. The
-- Traffic Mirror source and the Traffic Mirror target (monitoring
-- appliances) can be in the same VPC, or in different VPCs connected via
-- VPC peering or a transit gateway.
--
-- A Traffic Mirror target can be a network interface, or a Network Load
-- Balancer.
--
-- To use the target in a Traffic Mirror session, use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorSession.htm CreateTrafficMirrorSession>.
module Network.AWS.EC2.CreateTrafficMirrorTarget
  ( -- * Creating a Request
    CreateTrafficMirrorTarget (..),
    newCreateTrafficMirrorTarget,

    -- * Request Lenses
    createTrafficMirrorTarget_tagSpecifications,
    createTrafficMirrorTarget_networkLoadBalancerArn,
    createTrafficMirrorTarget_dryRun,
    createTrafficMirrorTarget_networkInterfaceId,
    createTrafficMirrorTarget_description,
    createTrafficMirrorTarget_clientToken,

    -- * Destructuring the Response
    CreateTrafficMirrorTargetResponse (..),
    newCreateTrafficMirrorTargetResponse,

    -- * Response Lenses
    createTrafficMirrorTargetResponse_trafficMirrorTarget,
    createTrafficMirrorTargetResponse_clientToken,
    createTrafficMirrorTargetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrafficMirrorTarget' smart constructor.
data CreateTrafficMirrorTarget = CreateTrafficMirrorTarget'
  { -- | The tags to assign to the Traffic Mirror target.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer that is
    -- associated with the target.
    networkLoadBalancerArn :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The network interface ID that is associated with the target.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The description of the Traffic Mirror target.
    description :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTrafficMirrorTarget_tagSpecifications' - The tags to assign to the Traffic Mirror target.
--
-- 'networkLoadBalancerArn', 'createTrafficMirrorTarget_networkLoadBalancerArn' - The Amazon Resource Name (ARN) of the Network Load Balancer that is
-- associated with the target.
--
-- 'dryRun', 'createTrafficMirrorTarget_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'networkInterfaceId', 'createTrafficMirrorTarget_networkInterfaceId' - The network interface ID that is associated with the target.
--
-- 'description', 'createTrafficMirrorTarget_description' - The description of the Traffic Mirror target.
--
-- 'clientToken', 'createTrafficMirrorTarget_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
newCreateTrafficMirrorTarget ::
  CreateTrafficMirrorTarget
newCreateTrafficMirrorTarget =
  CreateTrafficMirrorTarget'
    { tagSpecifications =
        Core.Nothing,
      networkLoadBalancerArn = Core.Nothing,
      dryRun = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      description = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The tags to assign to the Traffic Mirror target.
createTrafficMirrorTarget_tagSpecifications :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe [TagSpecification])
createTrafficMirrorTarget_tagSpecifications = Lens.lens (\CreateTrafficMirrorTarget' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorTarget' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorTarget) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the Network Load Balancer that is
-- associated with the target.
createTrafficMirrorTarget_networkLoadBalancerArn :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
createTrafficMirrorTarget_networkLoadBalancerArn = Lens.lens (\CreateTrafficMirrorTarget' {networkLoadBalancerArn} -> networkLoadBalancerArn) (\s@CreateTrafficMirrorTarget' {} a -> s {networkLoadBalancerArn = a} :: CreateTrafficMirrorTarget)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorTarget_dryRun :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Bool)
createTrafficMirrorTarget_dryRun = Lens.lens (\CreateTrafficMirrorTarget' {dryRun} -> dryRun) (\s@CreateTrafficMirrorTarget' {} a -> s {dryRun = a} :: CreateTrafficMirrorTarget)

-- | The network interface ID that is associated with the target.
createTrafficMirrorTarget_networkInterfaceId :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
createTrafficMirrorTarget_networkInterfaceId = Lens.lens (\CreateTrafficMirrorTarget' {networkInterfaceId} -> networkInterfaceId) (\s@CreateTrafficMirrorTarget' {} a -> s {networkInterfaceId = a} :: CreateTrafficMirrorTarget)

-- | The description of the Traffic Mirror target.
createTrafficMirrorTarget_description :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
createTrafficMirrorTarget_description = Lens.lens (\CreateTrafficMirrorTarget' {description} -> description) (\s@CreateTrafficMirrorTarget' {} a -> s {description = a} :: CreateTrafficMirrorTarget)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorTarget_clientToken :: Lens.Lens' CreateTrafficMirrorTarget (Core.Maybe Core.Text)
createTrafficMirrorTarget_clientToken = Lens.lens (\CreateTrafficMirrorTarget' {clientToken} -> clientToken) (\s@CreateTrafficMirrorTarget' {} a -> s {clientToken = a} :: CreateTrafficMirrorTarget)

instance Core.AWSRequest CreateTrafficMirrorTarget where
  type
    AWSResponse CreateTrafficMirrorTarget =
      CreateTrafficMirrorTargetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorTargetResponse'
            Core.<$> (x Core..@? "trafficMirrorTarget")
            Core.<*> (x Core..@? "clientToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTrafficMirrorTarget

instance Core.NFData CreateTrafficMirrorTarget

instance Core.ToHeaders CreateTrafficMirrorTarget where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTrafficMirrorTarget where
  toPath = Core.const "/"

instance Core.ToQuery CreateTrafficMirrorTarget where
  toQuery CreateTrafficMirrorTarget' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateTrafficMirrorTarget" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "NetworkLoadBalancerArn"
          Core.=: networkLoadBalancerArn,
        "DryRun" Core.=: dryRun,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "Description" Core.=: description,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newCreateTrafficMirrorTargetResponse' smart constructor.
data CreateTrafficMirrorTargetResponse = CreateTrafficMirrorTargetResponse'
  { -- | Information about the Traffic Mirror target.
    trafficMirrorTarget :: Core.Maybe TrafficMirrorTarget,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorTarget', 'createTrafficMirrorTargetResponse_trafficMirrorTarget' - Information about the Traffic Mirror target.
--
-- 'clientToken', 'createTrafficMirrorTargetResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'httpStatus', 'createTrafficMirrorTargetResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTrafficMirrorTargetResponse
newCreateTrafficMirrorTargetResponse pHttpStatus_ =
  CreateTrafficMirrorTargetResponse'
    { trafficMirrorTarget =
        Core.Nothing,
      clientToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror target.
createTrafficMirrorTargetResponse_trafficMirrorTarget :: Lens.Lens' CreateTrafficMirrorTargetResponse (Core.Maybe TrafficMirrorTarget)
createTrafficMirrorTargetResponse_trafficMirrorTarget = Lens.lens (\CreateTrafficMirrorTargetResponse' {trafficMirrorTarget} -> trafficMirrorTarget) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {trafficMirrorTarget = a} :: CreateTrafficMirrorTargetResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorTargetResponse_clientToken :: Lens.Lens' CreateTrafficMirrorTargetResponse (Core.Maybe Core.Text)
createTrafficMirrorTargetResponse_clientToken = Lens.lens (\CreateTrafficMirrorTargetResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorTargetResponse)

-- | The response's http status code.
createTrafficMirrorTargetResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorTargetResponse Core.Int
createTrafficMirrorTargetResponse_httpStatus = Lens.lens (\CreateTrafficMirrorTargetResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorTargetResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorTargetResponse)

instance
  Core.NFData
    CreateTrafficMirrorTargetResponse
