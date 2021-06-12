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
-- Module      : Network.AWS.CostExplorer.DeleteAnomalyMonitor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cost anomaly monitor.
module Network.AWS.CostExplorer.DeleteAnomalyMonitor
  ( -- * Creating a Request
    DeleteAnomalyMonitor (..),
    newDeleteAnomalyMonitor,

    -- * Request Lenses
    deleteAnomalyMonitor_monitorArn,

    -- * Destructuring the Response
    DeleteAnomalyMonitorResponse (..),
    newDeleteAnomalyMonitorResponse,

    -- * Response Lenses
    deleteAnomalyMonitorResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAnomalyMonitor' smart constructor.
data DeleteAnomalyMonitor = DeleteAnomalyMonitor'
  { -- | The unique identifier of the cost anomaly monitor that you want to
    -- delete.
    monitorArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitorArn', 'deleteAnomalyMonitor_monitorArn' - The unique identifier of the cost anomaly monitor that you want to
-- delete.
newDeleteAnomalyMonitor ::
  -- | 'monitorArn'
  Core.Text ->
  DeleteAnomalyMonitor
newDeleteAnomalyMonitor pMonitorArn_ =
  DeleteAnomalyMonitor' {monitorArn = pMonitorArn_}

-- | The unique identifier of the cost anomaly monitor that you want to
-- delete.
deleteAnomalyMonitor_monitorArn :: Lens.Lens' DeleteAnomalyMonitor Core.Text
deleteAnomalyMonitor_monitorArn = Lens.lens (\DeleteAnomalyMonitor' {monitorArn} -> monitorArn) (\s@DeleteAnomalyMonitor' {} a -> s {monitorArn = a} :: DeleteAnomalyMonitor)

instance Core.AWSRequest DeleteAnomalyMonitor where
  type
    AWSResponse DeleteAnomalyMonitor =
      DeleteAnomalyMonitorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAnomalyMonitorResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteAnomalyMonitor

instance Core.NFData DeleteAnomalyMonitor

instance Core.ToHeaders DeleteAnomalyMonitor where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSInsightsIndexService.DeleteAnomalyMonitor" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAnomalyMonitor where
  toJSON DeleteAnomalyMonitor' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("MonitorArn" Core..= monitorArn)]
      )

instance Core.ToPath DeleteAnomalyMonitor where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAnomalyMonitor where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAnomalyMonitorResponse' smart constructor.
data DeleteAnomalyMonitorResponse = DeleteAnomalyMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAnomalyMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAnomalyMonitorResponse_httpStatus' - The response's http status code.
newDeleteAnomalyMonitorResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAnomalyMonitorResponse
newDeleteAnomalyMonitorResponse pHttpStatus_ =
  DeleteAnomalyMonitorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAnomalyMonitorResponse_httpStatus :: Lens.Lens' DeleteAnomalyMonitorResponse Core.Int
deleteAnomalyMonitorResponse_httpStatus = Lens.lens (\DeleteAnomalyMonitorResponse' {httpStatus} -> httpStatus) (\s@DeleteAnomalyMonitorResponse' {} a -> s {httpStatus = a} :: DeleteAnomalyMonitorResponse)

instance Core.NFData DeleteAnomalyMonitorResponse
