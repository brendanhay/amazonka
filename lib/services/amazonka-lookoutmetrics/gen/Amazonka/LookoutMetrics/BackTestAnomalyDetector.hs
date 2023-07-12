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
-- Module      : Amazonka.LookoutMetrics.BackTestAnomalyDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs a backtest for anomaly detection for the specified resource.
module Amazonka.LookoutMetrics.BackTestAnomalyDetector
  ( -- * Creating a Request
    BackTestAnomalyDetector (..),
    newBackTestAnomalyDetector,

    -- * Request Lenses
    backTestAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    BackTestAnomalyDetectorResponse (..),
    newBackTestAnomalyDetectorResponse,

    -- * Response Lenses
    backTestAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBackTestAnomalyDetector' smart constructor.
data BackTestAnomalyDetector = BackTestAnomalyDetector'
  { -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackTestAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'backTestAnomalyDetector_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
newBackTestAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  BackTestAnomalyDetector
newBackTestAnomalyDetector pAnomalyDetectorArn_ =
  BackTestAnomalyDetector'
    { anomalyDetectorArn =
        pAnomalyDetectorArn_
    }

-- | The Amazon Resource Name (ARN) of the anomaly detector.
backTestAnomalyDetector_anomalyDetectorArn :: Lens.Lens' BackTestAnomalyDetector Prelude.Text
backTestAnomalyDetector_anomalyDetectorArn = Lens.lens (\BackTestAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@BackTestAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: BackTestAnomalyDetector)

instance Core.AWSRequest BackTestAnomalyDetector where
  type
    AWSResponse BackTestAnomalyDetector =
      BackTestAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          BackTestAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BackTestAnomalyDetector where
  hashWithSalt _salt BackTestAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData BackTestAnomalyDetector where
  rnf BackTestAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorArn

instance Data.ToHeaders BackTestAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BackTestAnomalyDetector where
  toJSON BackTestAnomalyDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance Data.ToPath BackTestAnomalyDetector where
  toPath = Prelude.const "/BackTestAnomalyDetector"

instance Data.ToQuery BackTestAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBackTestAnomalyDetectorResponse' smart constructor.
data BackTestAnomalyDetectorResponse = BackTestAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackTestAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'backTestAnomalyDetectorResponse_httpStatus' - The response's http status code.
newBackTestAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BackTestAnomalyDetectorResponse
newBackTestAnomalyDetectorResponse pHttpStatus_ =
  BackTestAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
backTestAnomalyDetectorResponse_httpStatus :: Lens.Lens' BackTestAnomalyDetectorResponse Prelude.Int
backTestAnomalyDetectorResponse_httpStatus = Lens.lens (\BackTestAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@BackTestAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: BackTestAnomalyDetectorResponse)

instance
  Prelude.NFData
    BackTestAnomalyDetectorResponse
  where
  rnf BackTestAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
