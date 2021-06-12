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
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic
-- Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorFilter
  ( -- * Creating a Request
    DeleteTrafficMirrorFilter (..),
    newDeleteTrafficMirrorFilter,

    -- * Request Lenses
    deleteTrafficMirrorFilter_dryRun,
    deleteTrafficMirrorFilter_trafficMirrorFilterId,

    -- * Destructuring the Response
    DeleteTrafficMirrorFilterResponse (..),
    newDeleteTrafficMirrorFilterResponse,

    -- * Response Lenses
    deleteTrafficMirrorFilterResponse_trafficMirrorFilterId,
    deleteTrafficMirrorFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteTrafficMirrorFilter_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'trafficMirrorFilterId', 'deleteTrafficMirrorFilter_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
newDeleteTrafficMirrorFilter ::
  -- | 'trafficMirrorFilterId'
  Core.Text ->
  DeleteTrafficMirrorFilter
newDeleteTrafficMirrorFilter pTrafficMirrorFilterId_ =
  DeleteTrafficMirrorFilter'
    { dryRun = Core.Nothing,
      trafficMirrorFilterId = pTrafficMirrorFilterId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorFilter_dryRun :: Lens.Lens' DeleteTrafficMirrorFilter (Core.Maybe Core.Bool)
deleteTrafficMirrorFilter_dryRun = Lens.lens (\DeleteTrafficMirrorFilter' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorFilter' {} a -> s {dryRun = a} :: DeleteTrafficMirrorFilter)

-- | The ID of the Traffic Mirror filter.
deleteTrafficMirrorFilter_trafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilter Core.Text
deleteTrafficMirrorFilter_trafficMirrorFilterId = Lens.lens (\DeleteTrafficMirrorFilter' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@DeleteTrafficMirrorFilter' {} a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilter)

instance Core.AWSRequest DeleteTrafficMirrorFilter where
  type
    AWSResponse DeleteTrafficMirrorFilter =
      DeleteTrafficMirrorFilterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterResponse'
            Core.<$> (x Core..@? "trafficMirrorFilterId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteTrafficMirrorFilter

instance Core.NFData DeleteTrafficMirrorFilter

instance Core.ToHeaders DeleteTrafficMirrorFilter where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteTrafficMirrorFilter where
  toPath = Core.const "/"

instance Core.ToQuery DeleteTrafficMirrorFilter where
  toQuery DeleteTrafficMirrorFilter' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteTrafficMirrorFilter" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "TrafficMirrorFilterId"
          Core.=: trafficMirrorFilterId
      ]

-- | /See:/ 'newDeleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteTrafficMirrorFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trafficMirrorFilterId', 'deleteTrafficMirrorFilterResponse_trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
--
-- 'httpStatus', 'deleteTrafficMirrorFilterResponse_httpStatus' - The response's http status code.
newDeleteTrafficMirrorFilterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteTrafficMirrorFilterResponse
newDeleteTrafficMirrorFilterResponse pHttpStatus_ =
  DeleteTrafficMirrorFilterResponse'
    { trafficMirrorFilterId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Traffic Mirror filter.
deleteTrafficMirrorFilterResponse_trafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilterResponse (Core.Maybe Core.Text)
deleteTrafficMirrorFilterResponse_trafficMirrorFilterId = Lens.lens (\DeleteTrafficMirrorFilterResponse' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@DeleteTrafficMirrorFilterResponse' {} a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilterResponse)

-- | The response's http status code.
deleteTrafficMirrorFilterResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorFilterResponse Core.Int
deleteTrafficMirrorFilterResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorFilterResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorFilterResponse)

instance
  Core.NFData
    DeleteTrafficMirrorFilterResponse
