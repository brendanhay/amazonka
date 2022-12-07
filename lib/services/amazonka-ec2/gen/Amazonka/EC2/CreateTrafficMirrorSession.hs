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
-- Module      : Amazonka.EC2.CreateTrafficMirrorSession
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.EC2.CreateTrafficMirrorSession
  ( -- * Creating a Request
    CreateTrafficMirrorSession (..),
    newCreateTrafficMirrorSession,

    -- * Request Lenses
    createTrafficMirrorSession_clientToken,
    createTrafficMirrorSession_description,
    createTrafficMirrorSession_packetLength,
    createTrafficMirrorSession_dryRun,
    createTrafficMirrorSession_virtualNetworkId,
    createTrafficMirrorSession_tagSpecifications,
    createTrafficMirrorSession_networkInterfaceId,
    createTrafficMirrorSession_trafficMirrorTargetId,
    createTrafficMirrorSession_trafficMirrorFilterId,
    createTrafficMirrorSession_sessionNumber,

    -- * Destructuring the Response
    CreateTrafficMirrorSessionResponse (..),
    newCreateTrafficMirrorSessionResponse,

    -- * Response Lenses
    createTrafficMirrorSessionResponse_clientToken,
    createTrafficMirrorSessionResponse_trafficMirrorSession,
    createTrafficMirrorSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTrafficMirrorSession' smart constructor.
data CreateTrafficMirrorSession = CreateTrafficMirrorSession'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the Traffic Mirror session.
    description :: Prelude.Maybe Prelude.Text,
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
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The VXLAN ID for the Traffic Mirror session. For more information about
    -- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
    -- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
    -- chosen at random.
    virtualNetworkId :: Prelude.Maybe Prelude.Int,
    -- | The tags to assign to a Traffic Mirror session.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTrafficMirrorSession_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'description', 'createTrafficMirrorSession_description' - The description of the Traffic Mirror session.
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
-- 'dryRun', 'createTrafficMirrorSession_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'virtualNetworkId', 'createTrafficMirrorSession_virtualNetworkId' - The VXLAN ID for the Traffic Mirror session. For more information about
-- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
-- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
-- chosen at random.
--
-- 'tagSpecifications', 'createTrafficMirrorSession_tagSpecifications' - The tags to assign to a Traffic Mirror session.
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
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        packetLength = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        virtualNetworkId = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        networkInterfaceId = pNetworkInterfaceId_,
        trafficMirrorTargetId = pTrafficMirrorTargetId_,
        trafficMirrorFilterId = pTrafficMirrorFilterId_,
        sessionNumber = pSessionNumber_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorSession_clientToken :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Text)
createTrafficMirrorSession_clientToken = Lens.lens (\CreateTrafficMirrorSession' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSession' {} a -> s {clientToken = a} :: CreateTrafficMirrorSession)

-- | The description of the Traffic Mirror session.
createTrafficMirrorSession_description :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Text)
createTrafficMirrorSession_description = Lens.lens (\CreateTrafficMirrorSession' {description} -> description) (\s@CreateTrafficMirrorSession' {} a -> s {description = a} :: CreateTrafficMirrorSession)

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

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createTrafficMirrorSession_dryRun :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Bool)
createTrafficMirrorSession_dryRun = Lens.lens (\CreateTrafficMirrorSession' {dryRun} -> dryRun) (\s@CreateTrafficMirrorSession' {} a -> s {dryRun = a} :: CreateTrafficMirrorSession)

-- | The VXLAN ID for the Traffic Mirror session. For more information about
-- the VXLAN protocol, see <https://tools.ietf.org/html/rfc7348 RFC 7348>.
-- If you do not specify a @VirtualNetworkId@, an account-wide unique id is
-- chosen at random.
createTrafficMirrorSession_virtualNetworkId :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe Prelude.Int)
createTrafficMirrorSession_virtualNetworkId = Lens.lens (\CreateTrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@CreateTrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: CreateTrafficMirrorSession)

-- | The tags to assign to a Traffic Mirror session.
createTrafficMirrorSession_tagSpecifications :: Lens.Lens' CreateTrafficMirrorSession (Prelude.Maybe [TagSpecification])
createTrafficMirrorSession_tagSpecifications = Lens.lens (\CreateTrafficMirrorSession' {tagSpecifications} -> tagSpecifications) (\s@CreateTrafficMirrorSession' {} a -> s {tagSpecifications = a} :: CreateTrafficMirrorSession) Prelude.. Lens.mapping Lens.coerced

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

instance Core.AWSRequest CreateTrafficMirrorSession where
  type
    AWSResponse CreateTrafficMirrorSession =
      CreateTrafficMirrorSessionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateTrafficMirrorSessionResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "trafficMirrorSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTrafficMirrorSession where
  hashWithSalt _salt CreateTrafficMirrorSession' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` packetLength
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` virtualNetworkId
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` trafficMirrorTargetId
      `Prelude.hashWithSalt` trafficMirrorFilterId
      `Prelude.hashWithSalt` sessionNumber

instance Prelude.NFData CreateTrafficMirrorSession where
  rnf CreateTrafficMirrorSession' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf packetLength
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf virtualNetworkId
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf trafficMirrorTargetId
      `Prelude.seq` Prelude.rnf trafficMirrorFilterId
      `Prelude.seq` Prelude.rnf sessionNumber

instance Data.ToHeaders CreateTrafficMirrorSession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateTrafficMirrorSession where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateTrafficMirrorSession where
  toQuery CreateTrafficMirrorSession' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateTrafficMirrorSession" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "PacketLength" Data.=: packetLength,
        "DryRun" Data.=: dryRun,
        "VirtualNetworkId" Data.=: virtualNetworkId,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "NetworkInterfaceId" Data.=: networkInterfaceId,
        "TrafficMirrorTargetId"
          Data.=: trafficMirrorTargetId,
        "TrafficMirrorFilterId"
          Data.=: trafficMirrorFilterId,
        "SessionNumber" Data.=: sessionNumber
      ]

-- | /See:/ 'newCreateTrafficMirrorSessionResponse' smart constructor.
data CreateTrafficMirrorSessionResponse = CreateTrafficMirrorSessionResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Prelude.Maybe TrafficMirrorSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTrafficMirrorSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createTrafficMirrorSessionResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'trafficMirrorSession', 'createTrafficMirrorSessionResponse_trafficMirrorSession' - Information about the Traffic Mirror session.
--
-- 'httpStatus', 'createTrafficMirrorSessionResponse_httpStatus' - The response's http status code.
newCreateTrafficMirrorSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTrafficMirrorSessionResponse
newCreateTrafficMirrorSessionResponse pHttpStatus_ =
  CreateTrafficMirrorSessionResponse'
    { clientToken =
        Prelude.Nothing,
      trafficMirrorSession = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to ensure idempotency>.
createTrafficMirrorSessionResponse_clientToken :: Lens.Lens' CreateTrafficMirrorSessionResponse (Prelude.Maybe Prelude.Text)
createTrafficMirrorSessionResponse_clientToken = Lens.lens (\CreateTrafficMirrorSessionResponse' {clientToken} -> clientToken) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {clientToken = a} :: CreateTrafficMirrorSessionResponse)

-- | Information about the Traffic Mirror session.
createTrafficMirrorSessionResponse_trafficMirrorSession :: Lens.Lens' CreateTrafficMirrorSessionResponse (Prelude.Maybe TrafficMirrorSession)
createTrafficMirrorSessionResponse_trafficMirrorSession = Lens.lens (\CreateTrafficMirrorSessionResponse' {trafficMirrorSession} -> trafficMirrorSession) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSession = a} :: CreateTrafficMirrorSessionResponse)

-- | The response's http status code.
createTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' CreateTrafficMirrorSessionResponse Prelude.Int
createTrafficMirrorSessionResponse_httpStatus = Lens.lens (\CreateTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@CreateTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: CreateTrafficMirrorSessionResponse)

instance
  Prelude.NFData
    CreateTrafficMirrorSessionResponse
  where
  rnf CreateTrafficMirrorSessionResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf trafficMirrorSession
      `Prelude.seq` Prelude.rnf httpStatus
