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

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteAnomalyMonitor' smart constructor.
data DeleteAnomalyMonitor = DeleteAnomalyMonitor'
  { -- | The unique identifier of the cost anomaly monitor that you want to
    -- delete.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAnomalyMonitor
newDeleteAnomalyMonitor pMonitorArn_ =
  DeleteAnomalyMonitor' {monitorArn = pMonitorArn_}

-- | The unique identifier of the cost anomaly monitor that you want to
-- delete.
deleteAnomalyMonitor_monitorArn :: Lens.Lens' DeleteAnomalyMonitor Prelude.Text
deleteAnomalyMonitor_monitorArn = Lens.lens (\DeleteAnomalyMonitor' {monitorArn} -> monitorArn) (\s@DeleteAnomalyMonitor' {} a -> s {monitorArn = a} :: DeleteAnomalyMonitor)

instance Prelude.AWSRequest DeleteAnomalyMonitor where
  type
    Rs DeleteAnomalyMonitor =
      DeleteAnomalyMonitorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAnomalyMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAnomalyMonitor

instance Prelude.NFData DeleteAnomalyMonitor

instance Prelude.ToHeaders DeleteAnomalyMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.DeleteAnomalyMonitor" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAnomalyMonitor where
  toJSON DeleteAnomalyMonitor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("MonitorArn" Prelude..= monitorArn)]
      )

instance Prelude.ToPath DeleteAnomalyMonitor where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAnomalyMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAnomalyMonitorResponse' smart constructor.
data DeleteAnomalyMonitorResponse = DeleteAnomalyMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAnomalyMonitorResponse
newDeleteAnomalyMonitorResponse pHttpStatus_ =
  DeleteAnomalyMonitorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteAnomalyMonitorResponse_httpStatus :: Lens.Lens' DeleteAnomalyMonitorResponse Prelude.Int
deleteAnomalyMonitorResponse_httpStatus = Lens.lens (\DeleteAnomalyMonitorResponse' {httpStatus} -> httpStatus) (\s@DeleteAnomalyMonitorResponse' {} a -> s {httpStatus = a} :: DeleteAnomalyMonitorResponse)

instance Prelude.NFData DeleteAnomalyMonitorResponse
