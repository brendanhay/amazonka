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
-- Module      : Network.AWS.EC2.CreateTrafficMirrorSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Traffic Mirror session.
--
-- A Traffic Mirror session actively copies packets from a Traffic Mirror
-- source to a Traffic Mirror target. Create a filter, and then assign it
-- to the session to define a subset of the traffic to mirror, for example
-- all TCP traffic.
--
-- The Traffic Mirror source and the Traffic Mirror target (monitoring
-- appliances) can be in the same VPC, or in a different VPC connected via
-- VPC peering or a transit gateway.
--
-- By default, no traffic is mirrored. Use
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTrafficMirrorFilter.htm CreateTrafficMirrorFilter>
-- to create filter rules that specify the traffic to mirror.
module Network.AWS.EC2.CreateTrafficMirrorSession
  ( -- * Creating a Request
    CreateTrafficMirrorSession (..),
    newCreateTrafficMirrorSession,

    -- * Request Lenses
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_description,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_clientToken,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,

    -- * Destructuring the Response
    CreateTrafficMirrorSessionResponse (..),
    newCreateTrafficMirrorSessionResponse,

    -- * Response Lenses
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { -- | The tags to assign to a Traffic Mirror session.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The number of bytes in each packet to mirror. These are bytes after the
    -- VXLAN header. Do not specify this parameter when you want to mirror the
    -- entire packet. To mirror a subset of the packet, set this to the length
    -- (in bytes) that you want to mirror. For example, if you set this value
    -- to 100, then the first 100 bytes that meet the filter criteria are
    -- copied to the target.
    --
    -- If you do not want to mirror the entire packet, use the @PacketLength@
    -- parameter to specify the number of bytes in each packet to mirror.
    packetLength :: Core.Maybe Core.Int,
    -- | The description of the Traffic Mirror session.
    description :: Core.Maybe Core.Text,
    -- | The VXLAN ID for the Traffic Mirror session. For more information about
    -- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
    -- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
    -- chosen at random.
    virtualNetworkId :: Core.Maybe Core.Int,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The ID of the source network interface.
    networkInterfaceId :: Core.Text,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Core.Text,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Text,
    -- | The session number determines the order in which sessions are evaluated
    -- when an interface is used by multiple sessions. The first session with a
    -- matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createTrafficMirrorSession_tagSpecifications' - The tags to assign to a Traffic Mirror session.
--
-- 'dryRun', 'createTrafficMirrorSession_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'packetLength', 'createTrafficMirrorSession_packetLength' - The number of bytes in each packet to mirror. These are bytes after the
-- VXLAN header. Do not specify this parameter when you want to mirror the
-- entire packet. To mirror a subset of the packet, set this to the length
-- (in bytes) that you want to mirror. For example, if you set this value
-- to 100, then the first 100 bytes that meet the filter criteria are
-- copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@
-- parameter to specify the number of bytes in each packet to mirror.
--
-- 'description', 'createTrafficMirrorSession_description' - The description of the Traffic Mirror session.
--
-- 'virtualNetworkId', 'createTrafficMirrorSession_virtualNetworkId' - The VXLAN ID for the Traffic Mirror session. For more information about
-- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
-- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
-- chosen at random.
--
-- 'clientToken', 'createTrafficMirrorSession_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'networkInterfaceId', 'createTrafficMirrorSession_networkInterfaceId' - The ID of the source network interface.
--
-- 'trafficMirrorTargetId', 'createTrafficMirrorSession_trafficMirrorTargetId' - The ID of the Traffic Mirror target.
--
-- 'trafficMirrorFilterId', 'createTrafficMirrorSession_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'sessionNumber', 'createTrafficMirrorSession_sessionNumber' - The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
newCreateTrafficMirrorSession ::
  -- | 'networkInterfaceId'
  Core.Text ->
  -- | 'trafficMirrorTargetId'
  Core.Text ->
  -- | 'trafficMirrorFilterId'
  Core.Text ->
  -- | 'sessionNumber'
  Core.Int ->
  CreateTrafficMirrorSession
newCreateTrafficMirrorSession
  pNetworkInterfaceId_
  pTrafficMirrorTargetId_
  pTrafficMirrorFilterId_
  pSessionNumber_ =
    CreateTrafficMirrorSession'
      { tagSpecifications =
          Core.Nothing,
        dryRun = Core.Nothing,
        packetLength = Core.Nothing,
        description = Core.Nothing,
        virtualNetworkId = Core.Nothing,
        clientToken = Core.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        trafficMirrorTargetId = pTrafficMirrorTargetId_,
        trafficMirrorFilterId = pTrafficMirrorFilterId_,
        sessionNumber = pSessionNumber_
      }

-- | The tags to assign to a Traffic Mirror session.
createTrafficMirrorSession_tagSpecifications :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe [TagSpecification])
createTrafficMirrorSession_tagSpecifications = Lens.lens (\CreateTrafficMirrorSession' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorSession' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorSession) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorSession_dryRun :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Bool)
createTrafficMirrorSession_dryRun = Lens.lens (\CreateTrafficMirrorSession' {dryRun} -> dryRun) (\s@CreateTrafficMirrorSession' {} a -> s {dryRun = a} :: CreateTrafficMirrorSession)

-- | The number of bytes in each packet to mirror. These are bytes after the
-- VXLAN header. Do not specify this parameter when you want to mirror the
-- entire packet. To mirror a subset of the packet, set this to the length
-- (in bytes) that you want to mirror. For example, if you set this value
-- to 100, then the first 100 bytes that meet the filter criteria are
-- copied to the target.
--
-- If you do not want to mirror the entire packet, use the @PacketLength@
-- parameter to specify the number of bytes in each packet to mirror.
createTrafficMirrorSession_packetLength :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Int)
createTrafficMirrorSession_packetLength = Lens.lens (\CreateTrafficMirrorSession' {packetLength} -> packetLength) (\s@CreateTrafficMirrorSession' {} a -> s {packetLength = a} :: CreateTrafficMirrorSession)

-- | The description of the Traffic Mirror session.
createTrafficMirrorSession_description :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Text)
createTrafficMirrorSession_description = Lens.lens (\CreateTrafficMirrorSession' {description} -> description) (\s@CreateTrafficMirrorSession' {} a -> s {description = a} :: CreateTrafficMirrorSession)

-- | The VXLAN ID for the Traffic Mirror session. For more information about
-- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
-- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
-- chosen at random.
createTrafficMirrorSession_virtualNetworkId :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Int)
createTrafficMirrorSession_virtualNetworkId = Lens.lens (\CreateTrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@CreateTrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: CreateTrafficMirrorSession)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorSession_clientToken :: Lens.Lens' CreateTrafficMirrorSession (Core.Maybe Core.Text)
createTrafficMirrorSession_clientToken = Lens.lens (\CreateTrafficMirrorSession' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSession' {} a -> s {clientToken = a} :: CreateTrafficMirrorSession)

-- | The ID of the source network interface.
createTrafficMirrorSession_networkInterfaceId :: Lens.Lens' CreateTrafficMirrorSession Core.Text
createTrafficMirrorSession_networkInterfaceId = Lens.lens (\CreateTrafficMirrorSession' {networkInterfaceId} -> networkInterfaceId) (\s@CreateTrafficMirrorSession' {} a -> s {networkInterfaceId = a} :: CreateTrafficMirrorSession)

-- | The ID of the Traffic Mirror target.
createTrafficMirrorSession_trafficMirrorTargetId :: Lens.Lens' CreateTrafficMirrorSession Core.Text
createTrafficMirrorSession_trafficMirrorTargetId = Lens.lens (\CreateTrafficMirrorSession' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@CreateTrafficMirrorSession' {} a -> s {trafficMirrorTargetId = a} :: CreateTrafficMirrorSession)

-- | The ID of the Traffic Mirror filter.
createTrafficMirrorSession_trafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorSession Core.Text
createTrafficMirrorSession_trafficMirrorFilterId = Lens.lens (\CreateTrafficMirrorSession' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@CreateTrafficMirrorSession' {} a -> s {trafficMirrorFilterId = a} :: CreateTrafficMirrorSession)

-- | The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
createTrafficMirrorSession_sessionNumber :: Lens.Lens' CreateTrafficMirrorSession Core.Int
createTrafficMirrorSession_sessionNumber = Lens.lens (\CreateTrafficMirrorSession' {sessionNumber} -> sessionNumber) (\s@CreateTrafficMirrorSession' {} a -> s {sessionNumber = a} :: CreateTrafficMirrorSession)

instance Core.AWSRequest CreateTrafficMirrorSession where
  type
    AWSResponse CreateTrafficMirrorSession =
      CreateTrafficMirrorSessionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorSessionResponse'
            Core.<$> (x Core..@? "trafficMirrorSession")
            Core.<*> (x Core..@? "clientToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTrafficMirrorSession

instance Core.NFData CreateTrafficMirrorSession

instance Core.ToHeaders CreateTrafficMirrorSession where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateTrafficMirrorSession where
  toPath = Core.const "/"

instance Core.ToQuery CreateTrafficMirrorSession where
  toQuery CreateTrafficMirrorSession' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateTrafficMirrorSession" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "PacketLength" Core.=: packetLength,
        "Description" Core.=: description,
        "VirtualNetworkId" Core.=: virtualNetworkId,
        "ClientToken" Core.=: clientToken,
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "TrafficMirrorTargetId"
          Core.=: trafficMirrorTargetId,
        "TrafficMirrorFilterId"
          Core.=: trafficMirrorFilterId,
        "SessionNumber" Core.=: sessionNumber
      ]

-- | /See:/ 'newCreateTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Core.Maybe TrafficMirrorSession,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorSession', 'createTrafficMirrorSessionResponse_trafficMirrorSession' - Information about the Traffic Mirror session.
--
-- 'clientToken', 'createTrafficMirrorSessionResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
--
-- 'httpStatus', 'createTrafficMirrorSessionResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTrafficMirrorSessionResponse
newCreateTrafficMirrorSessionResponse pHttpStatus_ =
  CreateTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Core.Nothing,
      clientToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror session.
createTrafficMirrorSessionResponse_trafficMirrorSession :: Lens.Lens' CreateTrafficMirrorSessionResponse (Core.Maybe TrafficMirrorSession)
createTrafficMirrorSessionResponse_trafficMirrorSession = Lens.lens (\CreateTrafficMirrorSessionResponse' {trafficMirrorSession} -> trafficMirrorSession) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSession = a} :: CreateTrafficMirrorSessionResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorSessionResponse_clientToken :: Lens.Lens' CreateTrafficMirrorSessionResponse (Core.Maybe Core.Text)
createTrafficMirrorSessionResponse_clientToken = Lens.lens (\CreateTrafficMirrorSessionResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorSessionResponse)

-- | The response's http status code.
createTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorSessionResponse Core.Int
createTrafficMirrorSessionResponse_httpStatus = Lens.lens (\CreateTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorSessionResponse)

instance
  Core.NFData
    CreateTrafficMirrorSessionResponse
