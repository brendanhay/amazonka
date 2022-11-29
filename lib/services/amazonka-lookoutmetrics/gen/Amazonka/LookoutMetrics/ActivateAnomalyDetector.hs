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
-- Module      : Amazonka.LookoutMetrics.ActivateAnomalyDetector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates an anomaly detector.
module Amazonka.LookoutMetrics.ActivateAnomalyDetector
  ( -- * Creating a Request
    ActivateAnomalyDetector (..),
    newActivateAnomalyDetector,

    -- * Request Lenses
    activateAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    ActivateAnomalyDetectorResponse (..),
    newActivateAnomalyDetectorResponse,

    -- * Response Lenses
    activateAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newActivateAnomalyDetector' smart constructor.
data ActivateAnomalyDetector = ActivateAnomalyDetector'
  { -- | The ARN of the anomaly detector.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'activateAnomalyDetector_anomalyDetectorArn' - The ARN of the anomaly detector.
newActivateAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  ActivateAnomalyDetector
newActivateAnomalyDetector pAnomalyDetectorArn_ =
  ActivateAnomalyDetector'
    { anomalyDetectorArn =
        pAnomalyDetectorArn_
    }

-- | The ARN of the anomaly detector.
activateAnomalyDetector_anomalyDetectorArn :: Lens.Lens' ActivateAnomalyDetector Prelude.Text
activateAnomalyDetector_anomalyDetectorArn = Lens.lens (\ActivateAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@ActivateAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: ActivateAnomalyDetector)

instance Core.AWSRequest ActivateAnomalyDetector where
  type
    AWSResponse ActivateAnomalyDetector =
      ActivateAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ActivateAnomalyDetectorResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateAnomalyDetector where
  hashWithSalt _salt ActivateAnomalyDetector' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData ActivateAnomalyDetector where
  rnf ActivateAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorArn

instance Core.ToHeaders ActivateAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ActivateAnomalyDetector where
  toJSON ActivateAnomalyDetector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Core..= anomalyDetectorArn)
          ]
      )

instance Core.ToPath ActivateAnomalyDetector where
  toPath = Prelude.const "/ActivateAnomalyDetector"

instance Core.ToQuery ActivateAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateAnomalyDetectorResponse' smart constructor.
data ActivateAnomalyDetectorResponse = ActivateAnomalyDetectorResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'activateAnomalyDetectorResponse_httpStatus' - The response's http status code.
newActivateAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivateAnomalyDetectorResponse
newActivateAnomalyDetectorResponse pHttpStatus_ =
  ActivateAnomalyDetectorResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
activateAnomalyDetectorResponse_httpStatus :: Lens.Lens' ActivateAnomalyDetectorResponse Prelude.Int
activateAnomalyDetectorResponse_httpStatus = Lens.lens (\ActivateAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@ActivateAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: ActivateAnomalyDetectorResponse)

instance
  Prelude.NFData
    ActivateAnomalyDetectorResponse
  where
  rnf ActivateAnomalyDetectorResponse' {..} =
    Prelude.rnf httpStatus
