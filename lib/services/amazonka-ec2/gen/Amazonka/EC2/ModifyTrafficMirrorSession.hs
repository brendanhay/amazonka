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
-- Module      : Amazonka.EC2.ModifyTrafficMirrorSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a Traffic Mirror session.
module Amazonka.EC2.ModifyTrafficMirrorSession
  ( -- * Creating a Request
    ModifyTrafficMirrorSession (..),
    newModifyTrafficMirrorSession,

    -- * Request Lenses
    modifyTrafficMirrorSession_description,
    modifyTrafficMirrorSession_dryRun,
    modifyTrafficMirrorSession_packetLength,
    modifyTrafficMirrorSession_removeFields,
    modifyTrafficMirrorSession_sessionNumber,
    modifyTrafficMirrorSession_trafficMirrorFilterId,
    modifyTrafficMirrorSession_trafficMirrorTargetId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyTrafficMirrorSession' smart constructor.
data ModifyTrafficMirrorSession = ModifyTrafficMirrorSession'
  { -- | The description to assign to the Traffic Mirror session.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The number of bytes in each packet to mirror. These are bytes after the
    -- VXLAN header. To mirror a subset, set this to the length (in bytes) to
    -- mirror. For example, if you set this value to 100, then the first 100
    -- bytes that meet the filter criteria are copied to the target. Do not
    -- specify this parameter when you want to mirror the entire packet.
    packetLength :: Prelude.Maybe Prelude.Int,
    -- | The properties that you want to remove from the Traffic Mirror session.
    --
    -- When you remove a property from a Traffic Mirror session, the property
    -- is set to the default.
    removeFields :: Prelude.Maybe [TrafficMirrorSessionField],
    -- | The session number determines the order in which sessions are evaluated
    -- when an interface is used by multiple sessions. The first session with a
    -- matching filter is the one that mirrors the packets.
    --
    -- Valid values are 1-32766.
    sessionNumber :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Maybe Prelude.Text,
    -- | The Traffic Mirror target. The target must be in the same VPC as the
    -- source, or have a VPC peering connection with the source.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text,
    -- | The virtual network ID of the Traffic Mirror session.
    virtualNetworkId :: Prelude.Maybe Prelude.Int,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyTrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'modifyTrafficMirrorSession_description' - The description to assign to the Traffic Mirror session.
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
-- 'removeFields', 'modifyTrafficMirrorSession_removeFields' - The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property
-- is set to the default.
--
-- 'sessionNumber', 'modifyTrafficMirrorSession_sessionNumber' - The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
--
-- 'trafficMirrorFilterId', 'modifyTrafficMirrorSession_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'trafficMirrorTargetId', 'modifyTrafficMirrorSession_trafficMirrorTargetId' - The Traffic Mirror target. The target must be in the same VPC as the
-- source, or have a VPC peering connection with the source.
--
-- 'virtualNetworkId', 'modifyTrafficMirrorSession_virtualNetworkId' - The virtual network ID of the Traffic Mirror session.
--
-- 'trafficMirrorSessionId', 'modifyTrafficMirrorSession_trafficMirrorSessionId' - The ID of the Traffic Mirror session.
newModifyTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Prelude.Text ->
  ModifyTrafficMirrorSession
newModifyTrafficMirrorSession
  pTrafficMirrorSessionId_ =
    ModifyTrafficMirrorSession'
      { description =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        packetLength = Prelude.Nothing,
        removeFields = Prelude.Nothing,
        sessionNumber = Prelude.Nothing,
        trafficMirrorFilterId = Prelude.Nothing,
        trafficMirrorTargetId = Prelude.Nothing,
        virtualNetworkId = Prelude.Nothing,
        trafficMirrorSessionId =
          pTrafficMirrorSessionId_
      }

-- | The description to assign to the Traffic Mirror session.
modifyTrafficMirrorSession_description :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Text)
modifyTrafficMirrorSession_description = Lens.lens (\ModifyTrafficMirrorSession' {description} -> description) (\s@ModifyTrafficMirrorSession' {} a -> s {description = a} :: ModifyTrafficMirrorSession)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTrafficMirrorSession_dryRun :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Bool)
modifyTrafficMirrorSession_dryRun = Lens.lens (\ModifyTrafficMirrorSession' {dryRun} -> dryRun) (\s@ModifyTrafficMirrorSession' {} a -> s {dryRun = a} :: ModifyTrafficMirrorSession)

-- | The number of bytes in each packet to mirror. These are bytes after the
-- VXLAN header. To mirror a subset, set this to the length (in bytes) to
-- mirror. For example, if you set this value to 100, then the first 100
-- bytes that meet the filter criteria are copied to the target. Do not
-- specify this parameter when you want to mirror the entire packet.
modifyTrafficMirrorSession_packetLength :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Int)
modifyTrafficMirrorSession_packetLength = Lens.lens (\ModifyTrafficMirrorSession' {packetLength} -> packetLength) (\s@ModifyTrafficMirrorSession' {} a -> s {packetLength = a} :: ModifyTrafficMirrorSession)

-- | The properties that you want to remove from the Traffic Mirror session.
--
-- When you remove a property from a Traffic Mirror session, the property
-- is set to the default.
modifyTrafficMirrorSession_removeFields :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe [TrafficMirrorSessionField])
modifyTrafficMirrorSession_removeFields = Lens.lens (\ModifyTrafficMirrorSession' {removeFields} -> removeFields) (\s@ModifyTrafficMirrorSession' {} a -> s {removeFields = a} :: ModifyTrafficMirrorSession) Prelude.. Lens.mapping Lens.coerced

-- | The session number determines the order in which sessions are evaluated
-- when an interface is used by multiple sessions. The first session with a
-- matching filter is the one that mirrors the packets.
--
-- Valid values are 1-32766.
modifyTrafficMirrorSession_sessionNumber :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Int)
modifyTrafficMirrorSession_sessionNumber = Lens.lens (\ModifyTrafficMirrorSession' {sessionNumber} -> sessionNumber) (\s@ModifyTrafficMirrorSession' {} a -> s {sessionNumber = a} :: ModifyTrafficMirrorSession)

-- | The ID of the Traffic Mirror filter.
modifyTrafficMirrorSession_trafficMirrorFilterId :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Text)
modifyTrafficMirrorSession_trafficMirrorFilterId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorFilterId = a} :: ModifyTrafficMirrorSession)

-- | The Traffic Mirror target. The target must be in the same VPC as the
-- source, or have a VPC peering connection with the source.
modifyTrafficMirrorSession_trafficMirrorTargetId :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Text)
modifyTrafficMirrorSession_trafficMirrorTargetId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorTargetId = a} :: ModifyTrafficMirrorSession)

-- | The virtual network ID of the Traffic Mirror session.
modifyTrafficMirrorSession_virtualNetworkId :: Lens.Lens' ModifyTrafficMirrorSession (Prelude.Maybe Prelude.Int)
modifyTrafficMirrorSession_virtualNetworkId = Lens.lens (\ModifyTrafficMirrorSession' {virtualNetworkId} -> virtualNetworkId) (\s@ModifyTrafficMirrorSession' {} a -> s {virtualNetworkId = a} :: ModifyTrafficMirrorSession)

-- | The ID of the Traffic Mirror session.
modifyTrafficMirrorSession_trafficMirrorSessionId :: Lens.Lens' ModifyTrafficMirrorSession Prelude.Text
modifyTrafficMirrorSession_trafficMirrorSessionId = Lens.lens (\ModifyTrafficMirrorSession' {trafficMirrorSessionId} -> trafficMirrorSessionId) (\s@ModifyTrafficMirrorSession' {} a -> s {trafficMirrorSessionId = a} :: ModifyTrafficMirrorSession)

instance Core.AWSRequest ModifyTrafficMirrorSession where
  type
    AWSResponse ModifyTrafficMirrorSession =
      ModifyTrafficMirrorSessionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTrafficMirrorSessionResponse'
            Prelude.<$> (x Data..@? "trafficMirrorSession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyTrafficMirrorSession where
  hashWithSalt _salt ModifyTrafficMirrorSession' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` packetLength
      `Prelude.hashWithSalt` removeFields
      `Prelude.hashWithSalt` sessionNumber
      `Prelude.hashWithSalt` trafficMirrorFilterId
      `Prelude.hashWithSalt` trafficMirrorTargetId
      `Prelude.hashWithSalt` virtualNetworkId
      `Prelude.hashWithSalt` trafficMirrorSessionId

instance Prelude.NFData ModifyTrafficMirrorSession where
  rnf ModifyTrafficMirrorSession' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf packetLength
      `Prelude.seq` Prelude.rnf removeFields
      `Prelude.seq` Prelude.rnf sessionNumber
      `Prelude.seq` Prelude.rnf trafficMirrorFilterId
      `Prelude.seq` Prelude.rnf trafficMirrorTargetId
      `Prelude.seq` Prelude.rnf virtualNetworkId
      `Prelude.seq` Prelude.rnf trafficMirrorSessionId

instance Data.ToHeaders ModifyTrafficMirrorSession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyTrafficMirrorSession where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyTrafficMirrorSession where
  toQuery ModifyTrafficMirrorSession' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyTrafficMirrorSession" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "PacketLength" Data.=: packetLength,
        Data.toQuery
          ( Data.toQueryList "RemoveField"
              Prelude.<$> removeFields
          ),
        "SessionNumber" Data.=: sessionNumber,
        "TrafficMirrorFilterId"
          Data.=: trafficMirrorFilterId,
        "TrafficMirrorTargetId"
          Data.=: trafficMirrorTargetId,
        "VirtualNetworkId" Data.=: virtualNetworkId,
        "TrafficMirrorSessionId"
          Data.=: trafficMirrorSessionId
      ]

-- | /See:/ 'newModifyTrafficMirrorSessionResponse' smart constructor.
data ModifyTrafficMirrorSessionResponse = ModifyTrafficMirrorSessionResponse'
  { -- | Information about the Traffic Mirror session.
    trafficMirrorSession :: Prelude.Maybe TrafficMirrorSession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyTrafficMirrorSessionResponse
newModifyTrafficMirrorSessionResponse pHttpStatus_ =
  ModifyTrafficMirrorSessionResponse'
    { trafficMirrorSession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Traffic Mirror session.
modifyTrafficMirrorSessionResponse_trafficMirrorSession :: Lens.Lens' ModifyTrafficMirrorSessionResponse (Prelude.Maybe TrafficMirrorSession)
modifyTrafficMirrorSessionResponse_trafficMirrorSession = Lens.lens (\ModifyTrafficMirrorSessionResponse' {trafficMirrorSession} -> trafficMirrorSession) (\s@ModifyTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSession = a} :: ModifyTrafficMirrorSessionResponse)

-- | The response's http status code.
modifyTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' ModifyTrafficMirrorSessionResponse Prelude.Int
modifyTrafficMirrorSessionResponse_httpStatus = Lens.lens (\ModifyTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@ModifyTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: ModifyTrafficMirrorSessionResponse)

instance
  Prelude.NFData
    ModifyTrafficMirrorSessionResponse
  where
  rnf ModifyTrafficMirrorSessionResponse' {..} =
    Prelude.rnf trafficMirrorSession
      `Prelude.seq` Prelude.rnf httpStatus
