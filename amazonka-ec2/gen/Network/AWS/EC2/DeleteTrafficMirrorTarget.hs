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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror target.
--
-- You cannot delete a Traffic Mirror target that is in use by a Traffic
-- Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorTarget
  ( -- * Creating a Request
    DeleteTrafficMirrorTarget (..),
    newDeleteTrafficMirrorTarget,

    -- * Request Lenses
    deleteTrafficMirrorTarget_dryRun,
    deleteTrafficMirrorTarget_trafficMirrorTargetId,

    -- * Destructuring the Response
    DeleteTrafficMirrorTargetResponse (..),
    newDeleteTrafficMirrorTargetResponse,

    -- * Response Lenses
    deleteTrafficMirrorTargetResponse_trafficMirrorTargetId,
    deleteTrafficMirrorTargetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorTarget' smart constructor.
data DeleteTrafficMirrorTarget = DeleteTrafficMirrorTarget'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTrafficMirrorTarget_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'trafficMirrorTargetId', 'deleteTrafficMirrorTarget_trafficMirrorTargetId' - The ID of the Traffic Mirror target.
newDeleteTrafficMirrorTarget ::
  -- | 'trafficMirrorTargetId'
  Core.Text ->
  DeleteTrafficMirrorTarget
newDeleteTrafficMirrorTarget pTrafficMirrorTargetId_ =
  DeleteTrafficMirrorTarget'
    { dryRun = Core.Nothing,
      trafficMirrorTargetId = pTrafficMirrorTargetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorTarget_dryRun :: Lens.Lens' DeleteTrafficMirrorTarget (Core.Maybe Core.Bool)
deleteTrafficMirrorTarget_dryRun = Lens.lens (\DeleteTrafficMirrorTarget' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorTarget' {} a -> s {dryRun = a} :: DeleteTrafficMirrorTarget)

-- | The ID of the Traffic Mirror target.
deleteTrafficMirrorTarget_trafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTarget Core.Text
deleteTrafficMirrorTarget_trafficMirrorTargetId = Lens.lens (\DeleteTrafficMirrorTarget' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@DeleteTrafficMirrorTarget' {} a -> s {trafficMirrorTargetId = a} :: DeleteTrafficMirrorTarget)

instance Core.AWSRequest DeleteTrafficMirrorTarget where
  type
    AWSResponse DeleteTrafficMirrorTarget =
      DeleteTrafficMirrorTargetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorTargetResponse'
            Core.<$> (x Core..@? "trafficMirrorTargetId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTrafficMirrorTarget

instance Core.NFData DeleteTrafficMirrorTarget

instance Core.ToHeaders DeleteTrafficMirrorTarget where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTrafficMirrorTarget where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTrafficMirrorTarget where
  toQuery DeleteTrafficMirrorTarget' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteTrafficMirrorTarget" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TrafficMirrorTargetId"
          Core.=: trafficMirrorTargetId
      ]

-- | /See:/ 'newDeleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'
  { -- | The ID of the deleted Traffic Mirror target.
    trafficMirrorTargetId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorTargetId', 'deleteTrafficMirrorTargetResponse_trafficMirrorTargetId' - The ID of the deleted Traffic Mirror target.
--
-- 'httpStatus', 'deleteTrafficMirrorTargetResponse_httpStatus' - The response's http status code.
newDeleteTrafficMirrorTargetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTrafficMirrorTargetResponse
newDeleteTrafficMirrorTargetResponse pHttpStatus_ =
  DeleteTrafficMirrorTargetResponse'
    { trafficMirrorTargetId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted Traffic Mirror target.
deleteTrafficMirrorTargetResponse_trafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTargetResponse (Core.Maybe Core.Text)
deleteTrafficMirrorTargetResponse_trafficMirrorTargetId = Lens.lens (\DeleteTrafficMirrorTargetResponse' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@DeleteTrafficMirrorTargetResponse' {} a -> s {trafficMirrorTargetId = a} :: DeleteTrafficMirrorTargetResponse)

-- | The response's http status code.
deleteTrafficMirrorTargetResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorTargetResponse Core.Int
deleteTrafficMirrorTargetResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorTargetResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorTargetResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorTargetResponse)

instance
  Core.NFData
    DeleteTrafficMirrorTargetResponse
