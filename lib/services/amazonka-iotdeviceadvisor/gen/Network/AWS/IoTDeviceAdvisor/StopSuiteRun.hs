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
-- Module      : Network.AWS.IoTDeviceAdvisor.StopSuiteRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a Device Advisor test suite run that is currently running.
module Network.AWS.IoTDeviceAdvisor.StopSuiteRun
  ( -- * Creating a Request
    StopSuiteRun (..),
    newStopSuiteRun,

    -- * Request Lenses
    stopSuiteRun_suiteDefinitionId,
    stopSuiteRun_suiteRunId,

    -- * Destructuring the Response
    StopSuiteRunResponse (..),
    newStopSuiteRunResponse,

    -- * Response Lenses
    stopSuiteRunResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTDeviceAdvisor.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopSuiteRun' smart constructor.
data StopSuiteRun = StopSuiteRun'
  { -- | Suite definition Id of the test suite run to be stopped.
    suiteDefinitionId :: Prelude.Text,
    -- | Suite run Id of the test suite run to be stopped.
    suiteRunId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSuiteRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suiteDefinitionId', 'stopSuiteRun_suiteDefinitionId' - Suite definition Id of the test suite run to be stopped.
--
-- 'suiteRunId', 'stopSuiteRun_suiteRunId' - Suite run Id of the test suite run to be stopped.
newStopSuiteRun ::
  -- | 'suiteDefinitionId'
  Prelude.Text ->
  -- | 'suiteRunId'
  Prelude.Text ->
  StopSuiteRun
newStopSuiteRun pSuiteDefinitionId_ pSuiteRunId_ =
  StopSuiteRun'
    { suiteDefinitionId =
        pSuiteDefinitionId_,
      suiteRunId = pSuiteRunId_
    }

-- | Suite definition Id of the test suite run to be stopped.
stopSuiteRun_suiteDefinitionId :: Lens.Lens' StopSuiteRun Prelude.Text
stopSuiteRun_suiteDefinitionId = Lens.lens (\StopSuiteRun' {suiteDefinitionId} -> suiteDefinitionId) (\s@StopSuiteRun' {} a -> s {suiteDefinitionId = a} :: StopSuiteRun)

-- | Suite run Id of the test suite run to be stopped.
stopSuiteRun_suiteRunId :: Lens.Lens' StopSuiteRun Prelude.Text
stopSuiteRun_suiteRunId = Lens.lens (\StopSuiteRun' {suiteRunId} -> suiteRunId) (\s@StopSuiteRun' {} a -> s {suiteRunId = a} :: StopSuiteRun)

instance Core.AWSRequest StopSuiteRun where
  type AWSResponse StopSuiteRun = StopSuiteRunResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopSuiteRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSuiteRun

instance Prelude.NFData StopSuiteRun

instance Core.ToHeaders StopSuiteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopSuiteRun where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath StopSuiteRun where
  toPath StopSuiteRun' {..} =
    Prelude.mconcat
      [ "/suiteDefinitions/",
        Core.toBS suiteDefinitionId,
        "/suiteRuns/",
        Core.toBS suiteRunId,
        "/stop"
      ]

instance Core.ToQuery StopSuiteRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopSuiteRunResponse' smart constructor.
data StopSuiteRunResponse = StopSuiteRunResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopSuiteRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopSuiteRunResponse_httpStatus' - The response's http status code.
newStopSuiteRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopSuiteRunResponse
newStopSuiteRunResponse pHttpStatus_ =
  StopSuiteRunResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopSuiteRunResponse_httpStatus :: Lens.Lens' StopSuiteRunResponse Prelude.Int
stopSuiteRunResponse_httpStatus = Lens.lens (\StopSuiteRunResponse' {httpStatus} -> httpStatus) (\s@StopSuiteRunResponse' {} a -> s {httpStatus = a} :: StopSuiteRunResponse)

instance Prelude.NFData StopSuiteRunResponse
