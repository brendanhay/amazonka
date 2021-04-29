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
-- Module      : Network.AWS.CostExplorer.CreateAnomalyMonitor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cost anomaly detection monitor with the requested type and
-- monitor specification.
module Network.AWS.CostExplorer.CreateAnomalyMonitor
  ( -- * Creating a Request
    CreateAnomalyMonitor (..),
    newCreateAnomalyMonitor,

    -- * Request Lenses
    createAnomalyMonitor_anomalyMonitor,

    -- * Destructuring the Response
    CreateAnomalyMonitorResponse (..),
    newCreateAnomalyMonitorResponse,

    -- * Response Lenses
    createAnomalyMonitorResponse_httpStatus,
    createAnomalyMonitorResponse_monitorArn,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateAnomalyMonitor' smart constructor.
data CreateAnomalyMonitor = CreateAnomalyMonitor'
  { -- | The cost anomaly detection monitor object that you want to create.
    anomalyMonitor :: AnomalyMonitor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalyMonitor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyMonitor', 'createAnomalyMonitor_anomalyMonitor' - The cost anomaly detection monitor object that you want to create.
newCreateAnomalyMonitor ::
  -- | 'anomalyMonitor'
  AnomalyMonitor ->
  CreateAnomalyMonitor
newCreateAnomalyMonitor pAnomalyMonitor_ =
  CreateAnomalyMonitor'
    { anomalyMonitor =
        pAnomalyMonitor_
    }

-- | The cost anomaly detection monitor object that you want to create.
createAnomalyMonitor_anomalyMonitor :: Lens.Lens' CreateAnomalyMonitor AnomalyMonitor
createAnomalyMonitor_anomalyMonitor = Lens.lens (\CreateAnomalyMonitor' {anomalyMonitor} -> anomalyMonitor) (\s@CreateAnomalyMonitor' {} a -> s {anomalyMonitor = a} :: CreateAnomalyMonitor)

instance Prelude.AWSRequest CreateAnomalyMonitor where
  type
    Rs CreateAnomalyMonitor =
      CreateAnomalyMonitorResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAnomalyMonitorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "MonitorArn")
      )

instance Prelude.Hashable CreateAnomalyMonitor

instance Prelude.NFData CreateAnomalyMonitor

instance Prelude.ToHeaders CreateAnomalyMonitor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSInsightsIndexService.CreateAnomalyMonitor" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateAnomalyMonitor where
  toJSON CreateAnomalyMonitor' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyMonitor" Prelude..= anomalyMonitor)
          ]
      )

instance Prelude.ToPath CreateAnomalyMonitor where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateAnomalyMonitor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAnomalyMonitorResponse' smart constructor.
data CreateAnomalyMonitorResponse = CreateAnomalyMonitorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique identifier of your newly created cost anomaly detection
    -- monitor.
    monitorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAnomalyMonitorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAnomalyMonitorResponse_httpStatus' - The response's http status code.
--
-- 'monitorArn', 'createAnomalyMonitorResponse_monitorArn' - The unique identifier of your newly created cost anomaly detection
-- monitor.
newCreateAnomalyMonitorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'monitorArn'
  Prelude.Text ->
  CreateAnomalyMonitorResponse
newCreateAnomalyMonitorResponse
  pHttpStatus_
  pMonitorArn_ =
    CreateAnomalyMonitorResponse'
      { httpStatus =
          pHttpStatus_,
        monitorArn = pMonitorArn_
      }

-- | The response's http status code.
createAnomalyMonitorResponse_httpStatus :: Lens.Lens' CreateAnomalyMonitorResponse Prelude.Int
createAnomalyMonitorResponse_httpStatus = Lens.lens (\CreateAnomalyMonitorResponse' {httpStatus} -> httpStatus) (\s@CreateAnomalyMonitorResponse' {} a -> s {httpStatus = a} :: CreateAnomalyMonitorResponse)

-- | The unique identifier of your newly created cost anomaly detection
-- monitor.
createAnomalyMonitorResponse_monitorArn :: Lens.Lens' CreateAnomalyMonitorResponse Prelude.Text
createAnomalyMonitorResponse_monitorArn = Lens.lens (\CreateAnomalyMonitorResponse' {monitorArn} -> monitorArn) (\s@CreateAnomalyMonitorResponse' {} a -> s {monitorArn = a} :: CreateAnomalyMonitorResponse)

instance Prelude.NFData CreateAnomalyMonitorResponse
