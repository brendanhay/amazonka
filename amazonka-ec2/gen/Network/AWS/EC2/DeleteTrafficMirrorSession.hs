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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorSession
  ( -- * Creating a Request
    DeleteTrafficMirrorSession (..),
    newDeleteTrafficMirrorSession,

    -- * Request Lenses
    deleteTrafficMirrorSession_dryRun,
    deleteTrafficMirrorSession_trafficMirrorSessionId,

    -- * Destructuring the Response
    DeleteTrafficMirrorSessionResponse (..),
    newDeleteTrafficMirrorSessionResponse,

    -- * Response Lenses
    deleteTrafficMirrorSessionResponse_trafficMirrorSessionId,
    deleteTrafficMirrorSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorSession' smart constructor.
data DeleteTrafficMirrorSession = DeleteTrafficMirrorSession'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Traffic Mirror session.
    trafficMirrorSessionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTrafficMirrorSession_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'trafficMirrorSessionId', 'deleteTrafficMirrorSession_trafficMirrorSessionId' - The ID of the Traffic Mirror session.
newDeleteTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Core.Text ->
  DeleteTrafficMirrorSession
newDeleteTrafficMirrorSession
  pTrafficMirrorSessionId_ =
    DeleteTrafficMirrorSession'
      { dryRun = Core.Nothing,
        trafficMirrorSessionId =
          pTrafficMirrorSessionId_
      }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorSession_dryRun :: Lens.Lens' DeleteTrafficMirrorSession (Core.Maybe Core.Bool)
deleteTrafficMirrorSession_dryRun = Lens.lens (\DeleteTrafficMirrorSession' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorSession' {} a -> s {dryRun = a} :: DeleteTrafficMirrorSession)

-- | The ID of the Traffic Mirror session.
deleteTrafficMirrorSession_trafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSession Core.Text
deleteTrafficMirrorSession_trafficMirrorSessionId = Lens.lens (\DeleteTrafficMirrorSession' {trafficMirrorSessionId} -> trafficMirrorSessionId) (\s@DeleteTrafficMirrorSession' {} a -> s {trafficMirrorSessionId = a} :: DeleteTrafficMirrorSession)

instance Core.AWSRequest DeleteTrafficMirrorSession where
  type
    AWSResponse DeleteTrafficMirrorSession =
      DeleteTrafficMirrorSessionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorSessionResponse'
            Core.<$> (x Core..@? "trafficMirrorSessionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTrafficMirrorSession

instance Core.NFData DeleteTrafficMirrorSession

instance Core.ToHeaders DeleteTrafficMirrorSession where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTrafficMirrorSession where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTrafficMirrorSession where
  toQuery DeleteTrafficMirrorSession' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteTrafficMirrorSession" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TrafficMirrorSessionId"
          Core.=: trafficMirrorSessionId
      ]

-- | /See:/ 'newDeleteTrafficMirrorSessionResponse' smart constructor.
data DeleteTrafficMirrorSessionResponse = DeleteTrafficMirrorSessionResponse'
  { -- | The ID of the deleted Traffic Mirror session.
    trafficMirrorSessionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorSessionId', 'deleteTrafficMirrorSessionResponse_trafficMirrorSessionId' - The ID of the deleted Traffic Mirror session.
--
-- 'httpStatus', 'deleteTrafficMirrorSessionResponse_httpStatus' - The response's http status code.
newDeleteTrafficMirrorSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTrafficMirrorSessionResponse
newDeleteTrafficMirrorSessionResponse pHttpStatus_ =
  DeleteTrafficMirrorSessionResponse'
    { trafficMirrorSessionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted Traffic Mirror session.
deleteTrafficMirrorSessionResponse_trafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSessionResponse (Core.Maybe Core.Text)
deleteTrafficMirrorSessionResponse_trafficMirrorSessionId = Lens.lens (\DeleteTrafficMirrorSessionResponse' {trafficMirrorSessionId} -> trafficMirrorSessionId) (\s@DeleteTrafficMirrorSessionResponse' {} a -> s {trafficMirrorSessionId = a} :: DeleteTrafficMirrorSessionResponse)

-- | The response's http status code.
deleteTrafficMirrorSessionResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorSessionResponse Core.Int
deleteTrafficMirrorSessionResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorSessionResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorSessionResponse)

instance
  Core.NFData
    DeleteTrafficMirrorSessionResponse
