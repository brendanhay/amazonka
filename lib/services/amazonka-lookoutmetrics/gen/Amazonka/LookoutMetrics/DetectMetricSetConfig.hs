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
-- Module      : Amazonka.LookoutMetrics.DetectMetricSetConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects an Amazon S3 dataset\'s file format, interval, and offset.
module Amazonka.LookoutMetrics.DetectMetricSetConfig
  ( -- * Creating a Request
    DetectMetricSetConfig (..),
    newDetectMetricSetConfig,

    -- * Request Lenses
    detectMetricSetConfig_anomalyDetectorArn,
    detectMetricSetConfig_autoDetectionMetricSource,

    -- * Destructuring the Response
    DetectMetricSetConfigResponse (..),
    newDetectMetricSetConfigResponse,

    -- * Response Lenses
    detectMetricSetConfigResponse_detectedMetricSetConfig,
    detectMetricSetConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectMetricSetConfig' smart constructor.
data DetectMetricSetConfig = DetectMetricSetConfig'
  { -- | An anomaly detector ARN.
    anomalyDetectorArn :: Prelude.Text,
    -- | A data source.
    autoDetectionMetricSource :: AutoDetectionMetricSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectMetricSetConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'detectMetricSetConfig_anomalyDetectorArn' - An anomaly detector ARN.
--
-- 'autoDetectionMetricSource', 'detectMetricSetConfig_autoDetectionMetricSource' - A data source.
newDetectMetricSetConfig ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  -- | 'autoDetectionMetricSource'
  AutoDetectionMetricSource ->
  DetectMetricSetConfig
newDetectMetricSetConfig
  pAnomalyDetectorArn_
  pAutoDetectionMetricSource_ =
    DetectMetricSetConfig'
      { anomalyDetectorArn =
          pAnomalyDetectorArn_,
        autoDetectionMetricSource =
          pAutoDetectionMetricSource_
      }

-- | An anomaly detector ARN.
detectMetricSetConfig_anomalyDetectorArn :: Lens.Lens' DetectMetricSetConfig Prelude.Text
detectMetricSetConfig_anomalyDetectorArn = Lens.lens (\DetectMetricSetConfig' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@DetectMetricSetConfig' {} a -> s {anomalyDetectorArn = a} :: DetectMetricSetConfig)

-- | A data source.
detectMetricSetConfig_autoDetectionMetricSource :: Lens.Lens' DetectMetricSetConfig AutoDetectionMetricSource
detectMetricSetConfig_autoDetectionMetricSource = Lens.lens (\DetectMetricSetConfig' {autoDetectionMetricSource} -> autoDetectionMetricSource) (\s@DetectMetricSetConfig' {} a -> s {autoDetectionMetricSource = a} :: DetectMetricSetConfig)

instance Core.AWSRequest DetectMetricSetConfig where
  type
    AWSResponse DetectMetricSetConfig =
      DetectMetricSetConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectMetricSetConfigResponse'
            Prelude.<$> (x Core..?> "DetectedMetricSetConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectMetricSetConfig where
  hashWithSalt _salt DetectMetricSetConfig' {..} =
    _salt `Prelude.hashWithSalt` anomalyDetectorArn
      `Prelude.hashWithSalt` autoDetectionMetricSource

instance Prelude.NFData DetectMetricSetConfig where
  rnf DetectMetricSetConfig' {..} =
    Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf autoDetectionMetricSource

instance Core.ToHeaders DetectMetricSetConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetectMetricSetConfig where
  toJSON DetectMetricSetConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AnomalyDetectorArn" Core..= anomalyDetectorArn),
            Prelude.Just
              ( "AutoDetectionMetricSource"
                  Core..= autoDetectionMetricSource
              )
          ]
      )

instance Core.ToPath DetectMetricSetConfig where
  toPath = Prelude.const "/DetectMetricSetConfig"

instance Core.ToQuery DetectMetricSetConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectMetricSetConfigResponse' smart constructor.
data DetectMetricSetConfigResponse = DetectMetricSetConfigResponse'
  { -- | The inferred dataset configuration for the datasource.
    detectedMetricSetConfig :: Prelude.Maybe DetectedMetricSetConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectMetricSetConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detectedMetricSetConfig', 'detectMetricSetConfigResponse_detectedMetricSetConfig' - The inferred dataset configuration for the datasource.
--
-- 'httpStatus', 'detectMetricSetConfigResponse_httpStatus' - The response's http status code.
newDetectMetricSetConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectMetricSetConfigResponse
newDetectMetricSetConfigResponse pHttpStatus_ =
  DetectMetricSetConfigResponse'
    { detectedMetricSetConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The inferred dataset configuration for the datasource.
detectMetricSetConfigResponse_detectedMetricSetConfig :: Lens.Lens' DetectMetricSetConfigResponse (Prelude.Maybe DetectedMetricSetConfig)
detectMetricSetConfigResponse_detectedMetricSetConfig = Lens.lens (\DetectMetricSetConfigResponse' {detectedMetricSetConfig} -> detectedMetricSetConfig) (\s@DetectMetricSetConfigResponse' {} a -> s {detectedMetricSetConfig = a} :: DetectMetricSetConfigResponse)

-- | The response's http status code.
detectMetricSetConfigResponse_httpStatus :: Lens.Lens' DetectMetricSetConfigResponse Prelude.Int
detectMetricSetConfigResponse_httpStatus = Lens.lens (\DetectMetricSetConfigResponse' {httpStatus} -> httpStatus) (\s@DetectMetricSetConfigResponse' {} a -> s {httpStatus = a} :: DetectMetricSetConfigResponse)

instance Prelude.NFData DetectMetricSetConfigResponse where
  rnf DetectMetricSetConfigResponse' {..} =
    Prelude.rnf detectedMetricSetConfig
      `Prelude.seq` Prelude.rnf httpStatus
