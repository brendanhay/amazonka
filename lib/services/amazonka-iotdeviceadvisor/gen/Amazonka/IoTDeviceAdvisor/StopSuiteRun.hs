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
-- Module      : Amazonka.IoTDeviceAdvisor.StopSuiteRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a Device Advisor test suite run that is currently running.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions StopSuiteRun>
-- action.
module Amazonka.IoTDeviceAdvisor.StopSuiteRun
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTDeviceAdvisor.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopSuiteRun' smart constructor.
data StopSuiteRun = StopSuiteRun'
  { -- | Suite definition ID of the test suite run to be stopped.
    suiteDefinitionId :: Prelude.Text,
    -- | Suite run ID of the test suite run to be stopped.
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
-- 'suiteDefinitionId', 'stopSuiteRun_suiteDefinitionId' - Suite definition ID of the test suite run to be stopped.
--
-- 'suiteRunId', 'stopSuiteRun_suiteRunId' - Suite run ID of the test suite run to be stopped.
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

-- | Suite definition ID of the test suite run to be stopped.
stopSuiteRun_suiteDefinitionId :: Lens.Lens' StopSuiteRun Prelude.Text
stopSuiteRun_suiteDefinitionId = Lens.lens (\StopSuiteRun' {suiteDefinitionId} -> suiteDefinitionId) (\s@StopSuiteRun' {} a -> s {suiteDefinitionId = a} :: StopSuiteRun)

-- | Suite run ID of the test suite run to be stopped.
stopSuiteRun_suiteRunId :: Lens.Lens' StopSuiteRun Prelude.Text
stopSuiteRun_suiteRunId = Lens.lens (\StopSuiteRun' {suiteRunId} -> suiteRunId) (\s@StopSuiteRun' {} a -> s {suiteRunId = a} :: StopSuiteRun)

instance Core.AWSRequest StopSuiteRun where
  type AWSResponse StopSuiteRun = StopSuiteRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopSuiteRunResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopSuiteRun where
  hashWithSalt _salt StopSuiteRun' {..} =
    _salt
      `Prelude.hashWithSalt` suiteDefinitionId
      `Prelude.hashWithSalt` suiteRunId

instance Prelude.NFData StopSuiteRun where
  rnf StopSuiteRun' {..} =
    Prelude.rnf suiteDefinitionId `Prelude.seq`
      Prelude.rnf suiteRunId

instance Data.ToHeaders StopSuiteRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopSuiteRun where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath StopSuiteRun where
  toPath StopSuiteRun' {..} =
    Prelude.mconcat
      [ "/suiteDefinitions/",
        Data.toBS suiteDefinitionId,
        "/suiteRuns/",
        Data.toBS suiteRunId,
        "/stop"
      ]

instance Data.ToQuery StopSuiteRun where
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

instance Prelude.NFData StopSuiteRunResponse where
  rnf StopSuiteRunResponse' {..} =
    Prelude.rnf httpStatus
