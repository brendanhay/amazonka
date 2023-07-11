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
-- Module      : Amazonka.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic
-- Mirror session.
module Amazonka.EC2.DeleteTrafficMirrorFilter
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteTrafficMirrorFilter
newDeleteTrafficMirrorFilter pTrafficMirrorFilterId_ =
  DeleteTrafficMirrorFilter'
    { dryRun =
        Prelude.Nothing,
      trafficMirrorFilterId = pTrafficMirrorFilterId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteTrafficMirrorFilter_dryRun :: Lens.Lens' DeleteTrafficMirrorFilter (Prelude.Maybe Prelude.Bool)
deleteTrafficMirrorFilter_dryRun = Lens.lens (\DeleteTrafficMirrorFilter' {dryRun} -> dryRun) (\s@DeleteTrafficMirrorFilter' {} a -> s {dryRun = a} :: DeleteTrafficMirrorFilter)

-- | The ID of the Traffic Mirror filter.
deleteTrafficMirrorFilter_trafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilter Prelude.Text
deleteTrafficMirrorFilter_trafficMirrorFilterId = Lens.lens (\DeleteTrafficMirrorFilter' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@DeleteTrafficMirrorFilter' {} a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilter)

instance Core.AWSRequest DeleteTrafficMirrorFilter where
  type
    AWSResponse DeleteTrafficMirrorFilter =
      DeleteTrafficMirrorFilterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterResponse'
            Prelude.<$> (x Data..@? "trafficMirrorFilterId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrafficMirrorFilter where
  hashWithSalt _salt DeleteTrafficMirrorFilter' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` trafficMirrorFilterId

instance Prelude.NFData DeleteTrafficMirrorFilter where
  rnf DeleteTrafficMirrorFilter' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf trafficMirrorFilterId

instance Data.ToHeaders DeleteTrafficMirrorFilter where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteTrafficMirrorFilter where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTrafficMirrorFilter where
  toQuery DeleteTrafficMirrorFilter' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteTrafficMirrorFilter" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "TrafficMirrorFilterId"
          Data.=: trafficMirrorFilterId
      ]

-- | /See:/ 'newDeleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { -- | The ID of the Traffic Mirror filter.
    trafficMirrorFilterId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteTrafficMirrorFilterResponse
newDeleteTrafficMirrorFilterResponse pHttpStatus_ =
  DeleteTrafficMirrorFilterResponse'
    { trafficMirrorFilterId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the Traffic Mirror filter.
deleteTrafficMirrorFilterResponse_trafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilterResponse (Prelude.Maybe Prelude.Text)
deleteTrafficMirrorFilterResponse_trafficMirrorFilterId = Lens.lens (\DeleteTrafficMirrorFilterResponse' {trafficMirrorFilterId} -> trafficMirrorFilterId) (\s@DeleteTrafficMirrorFilterResponse' {} a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilterResponse)

-- | The response's http status code.
deleteTrafficMirrorFilterResponse_httpStatus :: Lens.Lens' DeleteTrafficMirrorFilterResponse Prelude.Int
deleteTrafficMirrorFilterResponse_httpStatus = Lens.lens (\DeleteTrafficMirrorFilterResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficMirrorFilterResponse' {} a -> s {httpStatus = a} :: DeleteTrafficMirrorFilterResponse)

instance
  Prelude.NFData
    DeleteTrafficMirrorFilterResponse
  where
  rnf DeleteTrafficMirrorFilterResponse' {..} =
    Prelude.rnf trafficMirrorFilterId
      `Prelude.seq` Prelude.rnf httpStatus
