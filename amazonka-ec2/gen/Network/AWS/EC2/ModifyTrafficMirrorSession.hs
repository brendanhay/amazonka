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
-- Module      : Network.AWS.EC2.ModifyTrafficMirrorSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Traffic Mirror session.
module Network.AWS.EC2.ModifyTrafficMirrorSession
  ( -- * Creating a Request
    ModifyTrafficMirrorSession (..),
    newModifyTrafficMirrorSession,

    -- * Request Lenses
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_virtualNetworkId,
    modifyTrafficMirrorSession_trafficMirrorSessionId,

    -- * Destructuring the Response
    ModifyTrafficMirrorSessionResponse (..),
    newModifyTrafficMirrorSessionResponse,

    -- * Response Lenses
    modifyTrafficMirrorSessionResponse_trafficMirrorSession,
    modifyTrafficMirrorSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { -- | The properties that you want to remove from the Traffic Mirror session.
    --
    -- When you remove a property from a Traffic Mirror session, the property
    -- is set to the default.
    removeFields :: Core.Maybe [TrafficMirrorSessionField],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The number of bytes in each packet to mirror. These are bytes after the
    -- VXLAN header. To mirror a subset, set this to the length (in bytes) to
    -- mirror. For example, if you set this value to 100, then the first 100
    -- bytes that meet the filter criteria are copied to the target. Do not
    -- specify this parameter when you want to mirror the entire packet.
    packetLength :: Core.Maybe Core.Int,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Core.Text,
    -- | The description to assign to the Traffic Mirror session.
    description :: Core.Maybe Core.Text,
    -- | The Traffic Mirror target. The target must be in the same VPC as the
    -- source, or have a VPC peering connection with the source.
    trafficMirrorTargetId :: Core.Maybe Core.Text,
    -- | The session number determines the order in which sessions are evaluated
    -- when an interface is used by multiple sessions. The first session with a
    -- matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Core.Maybe Core.Int,
    -- | The virtual network ID of the Traffic Mirror session.
    virtualNetworkId :: Core.Maybe Core.Int,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeFields', 'modifyTrafficMirrorSession_removeFields' - The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property
-- is set to the default.
--
-- 'dryRun', 'modifyTrafficMirrorSession_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'packetLength', 'modifyTrafficMirrorSession_packetLength' - The number of bytes in each packet to mirror. These are bytes after the
-- VXLAN header. To mirror a subset, set this to the length (in bytes) to
-- mirror. For example, if you set this value to 100, then the first 100
-- bytes that meet the filter criteria are copied to the target. Do not
-- specify this parameter when you want to mirror the entire packet.
--
-- 'trafficMirrorFilterId', 'modifyTrafficMirrorSession_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'description', 'modifyTrafficMirrorSession_description' - The description to assign to the Traffic Mirror session.
--
-- 'trafficMirrorTargetId', 'modifyTrafficMirrorSession_trafficMirrorTargetId' - The Traffic Mirror target. The target must be in the same VPC as the
-- source, or have a VPC peering connection with the source.
--
-- 'sessionNumber', 'modifyTrafficMirrorSession_sessionNumber' - The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- 'virtualNetworkId', 'modifyTrafficMirrorSession_virtualNetworkId' - The virtual network ID of the Traffic Mirror session.
--
-- 'trafficMirrorSessionId', 'modifyTrafficMirrorSession_trafficMirrorSessionId' - The ID of the Traffic Mirror session.
newModifyTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Core.Text ->
  ModifyTrafficMirrorSession
newModifyTrafficMirrorSession
  pTrafficMirrorSessionId_ =
    ModifyTrafficMirrorSession'
      { removeFields =
          Core.Nothing,
        dryRun = Core.Nothing,
        packetLength = Core.Nothing,
        trafficMirrorFilterId = Core.Nothing,
        description = Core.Nothing,
        trafficMirrorTargetId = Core.Nothing,
        sessionNumber = Core.Nothing,
        virtualNetworkId = Core.Nothing,
        trafficMirrorSessionId =
          pTrafficMirrorSessionId_
      }

-- | The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property
-- is set to the default.
modifyTrafficMirrorSession_removeFields :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe [TrafficMirrorSessionField])
modifyTrafficMirrorSession_removeFields = Lens.lens (\ModifyTrafficMirrorSession' {removeFields} -> removeFields) (\s@ModifyTrafficMirrorSession' {} a -> s {removeFields = a} :: ModifyTrafficMirrorSession) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTrafficMirrorSession_dryRun :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Bool)
modifyTrafficMirrorSession_dryRun = Lens.lens (\ModifyTrafficMirrorSession' {dryRun} -> dryRun) (\s@ModifyTrafficMirrorSession' {} a -> s {dryRun = a} :: ModifyTrafficMirrorSession)

-- | The number of bytes in each packet to mirror. These are bytes after the
-- VXLAN header. To mirror a subset, set this to the length (in bytes) to
-- mirror. For example, if you set this value to 100, then the first 100
-- bytes that meet the filter criteria are copied to the target. Do not
-- specify this parameter when you want to mirror the entire packet.
modifyTrafficMirrorSession_packetLength :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
modifyTrafficMirrorSession_packetLength = Lens.lens (\ModifyTrafficMirrorSession' {packetLength} -> packetLength) (\s@ModifyTrafficMirrorSession' {} a -> s {packetLength = a} :: ModifyTrafficMirrorSession)

-- | The ID of the Traffic Mirror filter.
modifyTrafficMirrorSession_trafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Text)
modifyTrafficMirrorSession_trafficMirrorFilterId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorSession)

-- | The description to assign to the Traffic Mirror session.
modifyTrafficMirrorSession_description :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Text)
modifyTrafficMirrorSession_description = Lens.lens (\ModifyTrafficMirrorSession' {description} -> description) (\s@ModifyTrafficMirrorSession' {} a -> s {description = a} :: ModifyTrafficMirrorSession)

-- | The Traffic Mirror target. The target must be in the same VPC as the
-- source, or have a VPC peering connection with the source.
modifyTrafficMirrorSession_trafficMirrorTargetId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Text)
modifyTrafficMirrorSession_trafficMirrorTargetId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorTargetId = a} :: ModifyTrafficMirrorSession)

-- | The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
modifyTrafficMirrorSession_sessionNumber :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
modifyTrafficMirrorSession_sessionNumber = Lens.lens (\ModifyTrafficMirrorSession' {sessionNumber} -> sessionNumber) (\s@ModifyTrafficMirrorSession' {} a -> s {sessionNumber = a} :: ModifyTrafficMirrorSession)

-- | The virtual network ID of the Traffic Mirror session.
modifyTrafficMirrorSession_virtualNetworkId :: Lens.Lens' ModifyTrafficMirrorSession (Core.Maybe Core.Int)
modifyTrafficMirrorSession_virtualNetworkId = Lens.lens (\ModifyTrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@ModifyTrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: ModifyTrafficMirrorSession)

-- | The ID of the Traffic Mirror session.
modifyTrafficMirrorSession_trafficMirrorSessionId :: Lens.Lens' ModifyTrafficMirrorSession Core.Text
modifyTrafficMirrorSession_trafficMirrorSessionId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorSessionId} -> trafficMirrorSessionId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorSessionId = a} :: ModifyTrafficMirrorSession)

instance Core.AWSRequest ModifyTrafficMirrorSession where
  type
    AWSResponse ModifyTrafficMirrorSession =
      ModifyTrafficMirrorSessionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorSessionResponse'
            Core.<$> (x Core..@? "trafficMirrorSession")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyTrafficMirrorSession

instance Core.NFData ModifyTrafficMirrorSession

instance Core.ToHeaders ModifyTrafficMirrorSession where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyTrafficMirrorSession where
  toPath = Core.const "/"

instance Core.ToQuery ModifyTrafficMirrorSession where
  toQuery ModifyTrafficMirrorSession' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyTrafficMirrorSession" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "RemoveField"
              Core.<$> removeFields
          ),
        "DryRun" Core.=: dryRun,
        "PacketLength" Core.=: packetLength,
        "TrafficMirrorFilterId"
          Core.=: trafficMirrorFilterId,
        "Description" Core.=: description,
        "TrafficMirrorTargetId"
          Core.=: trafficMirrorTargetId,
        "SessionNumber" Core.=: sessionNumber,
        "VirtualNetworkId" Core.=: virtualNetworkId,
        "TrafficMirrorSessionId"
          Core.=: trafficMirrorSessionId
      ]

-- | /See:/ 'newModifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Core.Maybe TrafficMirrorSession,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorSession', 'modifyTrafficMirrorSessionResponse_trafficMirrorSession' - Information about the Traffic Mirror session.
--
-- 'httpStatus', 'modifyTrafficMirrorSessionResponse_httpStatus' - The response's http status code.
newModifyTrafficMirrorSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyTrafficMirrorSessionResponse
newModifyTrafficMirrorSessionResponse pHttpStatus_ =
  ModifyTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror session.
modifyTrafficMirrorSessionResponse_trafficMirrorSession :: Lens.Lens' ModifyTrafficMirrorSessionResponse (Core.Maybe TrafficMirrorSession)
modifyTrafficMirrorSessionResponse_trafficMirrorSession = Lens.lens (\ModifyTrafficMirrorSessionResponse' {trafficMirrorSession} -> trafficMirrorSession) (\s@ModifyTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSession = a} :: ModifyTrafficMirrorSessionResponse)

-- | The response's http status code.
modifyTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' ModifyTrafficMirrorSessionResponse Core.Int
modifyTrafficMirrorSessionResponse_httpStatus = Lens.lens (\ModifyTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@ModifyTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: ModifyTrafficMirrorSessionResponse)

instance
  Core.NFData
    ModifyTrafficMirrorSessionResponse
