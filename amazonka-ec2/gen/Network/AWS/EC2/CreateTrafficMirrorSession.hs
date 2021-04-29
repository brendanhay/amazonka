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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { -- | The tags to assign to a Traffic Mirror session.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The number of bytes in each packet to mirror. These are bytes after the
    -- VXLAN header. Do not specify this parameter when you want to mirror the
    -- entire packet. To mirror a subset of the packet, set this to the length
    -- (in bytes) that you want to mirror. For example, if you set this value
    -- to 100, then the first 100 bytes that meet the filter criteria are
    -- copied to the target.
    --
    -- If you do not want to mirror the entire packet, use the @PacketLength@
    -- parameter to specify the number of bytes in each packet to mirror.
    packetLength :: Prelude.Maybe Prelude.Int,
    -- | The description of the Traffic Mirror session.
    description :: Prelude.Maybe Prelude.Text,
    -- | The VXLAN ID for the Traffic Mirror session. For more information about
    -- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
    -- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
    -- chosen at random.
    virtualNetworkId :: Prelude.Maybe Prelude.Int,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source network interface.
    networkInterfaceId :: Prelude.Text,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Text,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Text,
    -- | The session number determines the order in which sessions are evaluated
    -- when an interface is used by multiple sessions. The first session with a
    -- matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'trafficMirrorTargetId'
  Prelude.Text ->
  -- | 'trafficMirrorFilterId'
  Prelude.Text ->
  -- | 'sessionNumber'
  Prelude.Int ->
  CreateTrafficMirrorSession
newCreateTrafficMirrorSession
  pNetworkInterfaceId_
  pTrafficMirrorTargetId_
  pTrafficMirrorFilterId_
  pSessionNumber_ =
    CreateTrafficMirrorSession'
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        packetLength = Prelude.Nothing,
        description = Prelude.Nothing,
        virtualNetworkId = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        trafficMirrorTargetId = pTrafficMirrorTargetId_,
        trafficMirrorFilterId = pTrafficMirrorFilterId_,
        sessionNumber = pSessionNumber_
      }

-- | The tags to assign to a Traffic Mirror session.
createTrafficMirrorSession_tagSpecifications :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe [TagSpecification])
createTrafficMirrorSession_tagSpecifications = Lens.lens (\CreateTrafficMirrorSession' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorSession' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorSession) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorSession_dryRun :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Bool)
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
createTrafficMirrorSession_packetLength :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Int)
createTrafficMirrorSession_packetLength = Lens.lens (\CreateTrafficMirrorSession' {packetLength} -> packetLength) (\s@CreateTrafficMirrorSession' {} a -> s {packetLength = a} :: CreateTrafficMirrorSession)

-- | The description of the Traffic Mirror session.
createTrafficMirrorSession_description :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Text)
createTrafficMirrorSession_description = Lens.lens (\CreateTrafficMirrorSession' {description} -> description) (\s@CreateTrafficMirrorSession' {} a -> s {description = a} :: CreateTrafficMirrorSession)

-- | The VXLAN ID for the Traffic Mirror session. For more information about
-- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
-- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
-- chosen at random.
createTrafficMirrorSession_virtualNetworkId :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Int)
createTrafficMirrorSession_virtualNetworkId = Lens.lens (\CreateTrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@CreateTrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: CreateTrafficMirrorSession)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorSession_clientToken :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Text)
createTrafficMirrorSession_clientToken = Lens.lens (\CreateTrafficMirrorSession' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSession' {} a -> s {clientToken = a} :: CreateTrafficMirrorSession)

-- | The ID of the source network interface.
createTrafficMirrorSession_networkInterfaceId :: Lens.Lens' CreateTrafficMirrorSession Prelude.Text
createTrafficMirrorSession_networkInterfaceId = Lens.lens (\CreateTrafficMirrorSession' {networkInterfaceId} -> networkInterfaceId) (\s@CreateTrafficMirrorSession' {} a -> s {networkInterfaceId = a} :: CreateTrafficMirrorSession)

-- | The ID of the Traffic Mirror target.
createTrafficMirrorSession_trafficMirrorTargetId :: Lens.Lens' CreateTrafficMirrorSession Prelude.Text
createTrafficMirrorSession_trafficMirrorTargetId = Lens.lens (\CreateTrafficMirrorSession' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@CreateTrafficMirrorSession' {} a -> s {trafficMirrorTargetId = a} :: CreateTrafficMirrorSession)

-- | The ID of the Traffic Mirror filter.
createTrafficMirrorSession_trafficMirrorFilterId :: Lens.Lens' CreateTrafficMirrorSession Prelude.Text
createTrafficMirrorSession_trafficMirrorFilterId = Lens.lens (\CreateTrafficMirrorSession' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@CreateTrafficMirrorSession' {} a -> s {trafficMirrorFilterId = a} :: CreateTrafficMirrorSession)

-- | The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
createTrafficMirrorSession_sessionNumber :: Lens.Lens' CreateTrafficMirrorSession Prelude.Int
createTrafficMirrorSession_sessionNumber = Lens.lens (\CreateTrafficMirrorSession' {sessionNumber} -> sessionNumber) (\s@CreateTrafficMirrorSession' {} a -> s {sessionNumber = a} :: CreateTrafficMirrorSession)

instance
  Prelude.AWSRequest
    CreateTrafficMirrorSession
  where
  type
    Rs CreateTrafficMirrorSession =
      CreateTrafficMirrorSessionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorSessionResponse'
            Prelude.<$> (x Prelude..@? "trafficMirrorSession")
            Prelude.<*> (x Prelude..@? "clientToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrafficMirrorSession

instance Prelude.NFData CreateTrafficMirrorSession

instance Prelude.ToHeaders CreateTrafficMirrorSession where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateTrafficMirrorSession where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateTrafficMirrorSession where
  toQuery CreateTrafficMirrorSession' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateTrafficMirrorSession" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "PacketLength" Prelude.=: packetLength,
        "Description" Prelude.=: description,
        "VirtualNetworkId" Prelude.=: virtualNetworkId,
        "ClientToken" Prelude.=: clientToken,
        "NetworkInterfaceId" Prelude.=: networkInterfaceId,
        "TrafficMirrorTargetId"
          Prelude.=: trafficMirrorTargetId,
        "TrafficMirrorFilterId"
          Prelude.=: trafficMirrorFilterId,
        "SessionNumber" Prelude.=: sessionNumber
      ]

-- | /See:/ 'newCreateTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Prelude.Maybe TrafficMirrorSession,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateTrafficMirrorSessionResponse
newCreateTrafficMirrorSessionResponse pHttpStatus_ =
  CreateTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror session.
createTrafficMirrorSessionResponse_trafficMirrorSession :: Lens.Lens' CreateTrafficMirrorSessionResponse (Prelude.Maybe TrafficMirrorSession)
createTrafficMirrorSessionResponse_trafficMirrorSession = Lens.lens (\CreateTrafficMirrorSessionResponse' {trafficMirrorSession} -> trafficMirrorSession) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSession = a} :: CreateTrafficMirrorSessionResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createTrafficMirrorSessionResponse_clientToken :: Lens.Lens' CreateTrafficMirrorSessionResponse (Prelude.Maybe Prelude.Text)
createTrafficMirrorSessionResponse_clientToken = Lens.lens (\CreateTrafficMirrorSessionResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorSessionResponse)

-- | The response's http status code.
createTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorSessionResponse Prelude.Int
createTrafficMirrorSessionResponse_httpStatus = Lens.lens (\CreateTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorSessionResponse)

instance
  Prelude.NFData
    CreateTrafficMirrorSessionResponse
