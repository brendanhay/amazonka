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
-- Module      : Amazonka.EC2.DeleteTrafficMirrorTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror target.
--
-- You cannot delete a Traffic Mirror target that is in use by a Traffic
-- Mirror session.
module Amazonka.EC2.DeleteTrafficMirrorTarget
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteTrafficMirrorTarget where
  type
    AWSResponse DeleteTrafficMirrorTarget =
      DeleteTrafficMirrorTargetResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorTargetResponse'
            Prelude.<$> (x Data..@? "trafficMirrorTargetId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrafficMirrorTarget where
  hashWithSalt _salt DeleteTrafficMirrorTarget' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` trafficMirrorTargetId

instance Prelude.NFData DeleteTrafficMirrorTarget where
  rnf DeleteTrafficMirrorTarget' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf trafficMirrorTargetId

instance Data.ToHeaders DeleteTrafficMirrorTarget where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTrafficMirrorTarget where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTrafficMirrorTarget where
  toQuery DeleteTrafficMirrorTarget' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteTrafficMirrorTarget" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TrafficMirrorTargetId"
          Data.=: trafficMirrorTargetId
      ]

-- | /See:/ 'newDeleteTrafficMirrorTargetResponse' smart constructor.
data DeleteTrafficMirrorTargetResponse = DeleteTrafficMirrorTargetResponse'
  { -- | The ID of the deleted Traffic Mirror target.
    trafficMirrorTargetId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteTrafficMirrorTargetResponse' {..} =
    Prelude.rnf trafficMirrorTargetId
      `Prelude.seq` Prelude.rnf httpStatus
