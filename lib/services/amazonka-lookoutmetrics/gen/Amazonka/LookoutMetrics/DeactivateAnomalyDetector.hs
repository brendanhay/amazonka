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
-- Module      : Amazonka.LookoutMetrics.DeactivateAnomalyDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates an anomaly detector.
module Amazonka.LookoutMetrics.DeactivateAnomalyDetector
  ( -- * Creating a Request
    DeactivateAnomalyDetector (..),
    newDeactivateAnomalyDetector,

    -- * Request Lenses
    deactivateAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    DeactivateAnomalyDetectorResponse (..),
    newDeactivateAnomalyDetectorResponse,

    -- * Response Lenses
    deactivateAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeactivateAnomalyDetector' smart constructor.
data DeactivateAnomalyDetector = DeactivateAnomalyDetector'
  { -- | The Amazon Resource Name (ARN) of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'deactivateAnomalyDetector_anomalyDetectorArn' - The Amazon Resource Name (ARN) of the anomaly detector.
newDeactivateAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  DeactivateAnomalyDetector
newDeactivateAnomalyDetector pAnomalyDetectorArn_ =
  DeactivateAnomalyDetector'
    { anomalyDetectorArn =
        pAnomalyDetectorArn_
    }

-- | The Amazon Resource Name (ARN) of the anomaly detector.
deactivateAnomalyDetector_anomalyDetectorArn :: Lens.Lens' DeactivateAnomalyDetector Prelude.Text
deactivateAnomalyDetector_anomalyDetectorArn = Lens.lens (\DeactivateAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DeactivateAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: DeactivateAnomalyDetector)

instance Core.AWSRequest DeactivateAnomalyDetector where
  type
    AWSResponse DeactivateAnomalyDetector =
      DeactivateAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeactivateAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeactivateAnomalyDetector where
  hashWithSalt _salt DeactivateAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData DeactivateAnomalyDetector where
  rnf DeactivateAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorArn

instance Data.ToHeaders DeactivateAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeactivateAnomalyDetector where
  toJSON DeactivateAnomalyDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance Data.ToPath DeactivateAnomalyDetector where
  toPath = Prelude.const "/DeactivateAnomalyDetector"

instance Data.ToQuery DeactivateAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateAnomalyDetectorResponse' smart constructor.
data DeactivateAnomalyDetectorResponse = DeactivateAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deactivateAnomalyDetectorResponse_httpStatus' - The response's http status code.
newDeactivateAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeactivateAnomalyDetectorResponse
newDeactivateAnomalyDetectorResponse pHttpStatus_ =
  DeactivateAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deactivateAnomalyDetectorResponse_httpStatus :: Lens.Lens' DeactivateAnomalyDetectorResponse Prelude.Int
deactivateAnomalyDetectorResponse_httpStatus = Lens.lens (\DeactivateAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@DeactivateAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: DeactivateAnomalyDetectorResponse)

instance
  Prelude.NFData
    DeactivateAnomalyDetectorResponse
  where
  rnf DeactivateAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
