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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorTarget' smart constructor.
data DeleteTrafficMirrorTarget = DeleteTrafficMirrorTarget'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTrafficMirrorTarget
newDeleteTrafficMirrorTarget pTrafficMirrorTargetId_ =
  DeleteTrafficMirrorTarget'
    { dryRun =
        Prelude.Nothing,
      trafficMirrorTargetId = pTrafficMirrorTargetId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorTarget_dryRun :: Lens.Lens' DeleteTrafficMirrorTarget (Prelude.Maybe Prelude.Bool)
deleteTrafficMirrorTarget_dryRun = Lens.lens (\DeleteTrafficMirrorTarget' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorTarget' {} a -> s {dryRun = a} :: DeleteTrafficMirrorTarget)

-- | The ID of the Traffic Mirror target.
deleteTrafficMirrorTarget_trafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTarget Prelude.Text
deleteTrafficMirrorTarget_trafficMirrorTargetId = Lens.lens (\DeleteTrafficMirrorTarget' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@DeleteTrafficMirrorTarget' {} a -> s {trafficMirrorTargetId = a} :: DeleteTrafficMirrorTarget)

instance Prelude.AWSRequest DeleteTrafficMirrorTarget where
  type
    Rs DeleteTrafficMirrorTarget =
      DeleteTrafficMirrorTargetResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorTargetResponse'
            Prelude.<$> (x Prelude..@? "trafficMirrorTargetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrafficMirrorTarget

instance Prelude.NFData DeleteTrafficMirrorTarget

instance Prelude.ToHeaders DeleteTrafficMirrorTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTrafficMirrorTarget where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteTrafficMirrorTarget where
  toQuery DeleteTrafficMirrorTarget' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteTrafficMirrorTarget" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "TrafficMirrorTargetId"
          Prelude.=: trafficMirrorTargetId
      ]

-- | /See:/ 'newDeleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'
  { -- | The ID of the deleted Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTrafficMirrorTargetResponse
newDeleteTrafficMirrorTargetResponse pHttpStatus_ =
  DeleteTrafficMirrorTargetResponse'
    { trafficMirrorTargetId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deleted Traffic Mirror target.
deleteTrafficMirrorTargetResponse_trafficMirrorTargetId :: Lens.Lens' DeleteTrafficMirrorTargetResponse (Prelude.Maybe Prelude.Text)
deleteTrafficMirrorTargetResponse_trafficMirrorTargetId = Lens.lens (\DeleteTrafficMirrorTargetResponse' {trafficMirrorTargetId} -> trafficMirrorTargetId) (\s@DeleteTrafficMirrorTargetResponse' {} a -> s {trafficMirrorTargetId = a} :: DeleteTrafficMirrorTargetResponse)

-- | The response's http status code.
deleteTrafficMirrorTargetResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorTargetResponse Prelude.Int
deleteTrafficMirrorTargetResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorTargetResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorTargetResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorTargetResponse)

instance
  Prelude.NFData
    DeleteTrafficMirrorTargetResponse
