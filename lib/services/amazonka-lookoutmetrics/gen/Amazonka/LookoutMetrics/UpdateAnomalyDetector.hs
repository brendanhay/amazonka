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
-- Module      : Amazonka.LookoutMetrics.UpdateAnomalyDetector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a detector. After activation, you can only change a detector\'s
-- ingestion delay and description.
module Amazonka.LookoutMetrics.UpdateAnomalyDetector
  ( -- * Creating a Request
    UpdateAnomalyDetector (..),
    newUpdateAnomalyDetector,

    -- * Request Lenses
    updateAnomalyDetector_anomalyDetectorConfig,
    updateAnomalyDetector_anomalyDetectorDescription,
    updateAnomalyDetector_kmsKeyArn,
    updateAnomalyDetector_anomalyDetectorArn,

    -- * Destructuring the Response
    UpdateAnomalyDetectorResponse (..),
    newUpdateAnomalyDetectorResponse,

    -- * Response Lenses
    updateAnomalyDetectorResponse_anomalyDetectorArn,
    updateAnomalyDetectorResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutMetrics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateAnomalyDetector' smart constructor.
data UpdateAnomalyDetector = UpdateAnomalyDetector'
  { -- | Contains information about the configuration to which the detector will
    -- be updated.
    anomalyDetectorConfig :: Prelude.Maybe AnomalyDetectorConfig,
    -- | The updated detector description.
    anomalyDetectorDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an AWS KMS encryption key.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the detector to update.
    anomalyDetectorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnomalyDetector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorConfig', 'updateAnomalyDetector_anomalyDetectorConfig' - Contains information about the configuration to which the detector will
-- be updated.
--
-- 'anomalyDetectorDescription', 'updateAnomalyDetector_anomalyDetectorDescription' - The updated detector description.
--
-- 'kmsKeyArn', 'updateAnomalyDetector_kmsKeyArn' - The Amazon Resource Name (ARN) of an AWS KMS encryption key.
--
-- 'anomalyDetectorArn', 'updateAnomalyDetector_anomalyDetectorArn' - The ARN of the detector to update.
newUpdateAnomalyDetector ::
  -- | 'anomalyDetectorArn'
  Prelude.Text ->
  UpdateAnomalyDetector
newUpdateAnomalyDetector pAnomalyDetectorArn_ =
  UpdateAnomalyDetector'
    { anomalyDetectorConfig =
        Prelude.Nothing,
      anomalyDetectorDescription = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      anomalyDetectorArn = pAnomalyDetectorArn_
    }

-- | Contains information about the configuration to which the detector will
-- be updated.
updateAnomalyDetector_anomalyDetectorConfig :: Lens.Lens' UpdateAnomalyDetector (Prelude.Maybe AnomalyDetectorConfig)
updateAnomalyDetector_anomalyDetectorConfig = Lens.lens (\UpdateAnomalyDetector' {anomalyDetectorConfig} -> anomalyDetectorConfig) (\s@UpdateAnomalyDetector' {} a -> s {anomalyDetectorConfig = a} :: UpdateAnomalyDetector)

-- | The updated detector description.
updateAnomalyDetector_anomalyDetectorDescription :: Lens.Lens' UpdateAnomalyDetector (Prelude.Maybe Prelude.Text)
updateAnomalyDetector_anomalyDetectorDescription = Lens.lens (\UpdateAnomalyDetector' {anomalyDetectorDescription} -> anomalyDetectorDescription) (\s@UpdateAnomalyDetector' {} a -> s {anomalyDetectorDescription = a} :: UpdateAnomalyDetector)

-- | The Amazon Resource Name (ARN) of an AWS KMS encryption key.
updateAnomalyDetector_kmsKeyArn :: Lens.Lens' UpdateAnomalyDetector (Prelude.Maybe Prelude.Text)
updateAnomalyDetector_kmsKeyArn = Lens.lens (\UpdateAnomalyDetector' {kmsKeyArn} -> kmsKeyArn) (\s@UpdateAnomalyDetector' {} a -> s {kmsKeyArn = a} :: UpdateAnomalyDetector)

-- | The ARN of the detector to update.
updateAnomalyDetector_anomalyDetectorArn :: Lens.Lens' UpdateAnomalyDetector Prelude.Text
updateAnomalyDetector_anomalyDetectorArn = Lens.lens (\UpdateAnomalyDetector' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@UpdateAnomalyDetector' {} a -> s {anomalyDetectorArn = a} :: UpdateAnomalyDetector)

instance Core.AWSRequest UpdateAnomalyDetector where
  type
    AWSResponse UpdateAnomalyDetector =
      UpdateAnomalyDetectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAnomalyDetectorResponse'
            Prelude.<$> (x Data..?> "AnomalyDetectorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAnomalyDetector where
  hashWithSalt _salt UpdateAnomalyDetector' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyDetectorConfig
      `Prelude.hashWithSalt` anomalyDetectorDescription
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` anomalyDetectorArn

instance Prelude.NFData UpdateAnomalyDetector where
  rnf UpdateAnomalyDetector' {..} =
    Prelude.rnf anomalyDetectorConfig
      `Prelude.seq` Prelude.rnf anomalyDetectorDescription
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf anomalyDetectorArn

instance Data.ToHeaders UpdateAnomalyDetector where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAnomalyDetector where
  toJSON UpdateAnomalyDetector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnomalyDetectorConfig" Data..=)
              Prelude.<$> anomalyDetectorConfig,
            ("AnomalyDetectorDescription" Data..=)
              Prelude.<$> anomalyDetectorDescription,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just
              ("AnomalyDetectorArn" Data..= anomalyDetectorArn)
          ]
      )

instance Data.ToPath UpdateAnomalyDetector where
  toPath = Prelude.const "/UpdateAnomalyDetector"

instance Data.ToQuery UpdateAnomalyDetector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAnomalyDetectorResponse' smart constructor.
data UpdateAnomalyDetectorResponse = UpdateAnomalyDetectorResponse'
  { -- | The ARN of the updated detector.
    anomalyDetectorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAnomalyDetectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorArn', 'updateAnomalyDetectorResponse_anomalyDetectorArn' - The ARN of the updated detector.
--
-- 'httpStatus', 'updateAnomalyDetectorResponse_httpStatus' - The response's http status code.
newUpdateAnomalyDetectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAnomalyDetectorResponse
newUpdateAnomalyDetectorResponse pHttpStatus_ =
  UpdateAnomalyDetectorResponse'
    { anomalyDetectorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the updated detector.
updateAnomalyDetectorResponse_anomalyDetectorArn :: Lens.Lens' UpdateAnomalyDetectorResponse (Prelude.Maybe Prelude.Text)
updateAnomalyDetectorResponse_anomalyDetectorArn = Lens.lens (\UpdateAnomalyDetectorResponse' {anomalyDetectorArn} -> anomalyDetectorArn) (\s@UpdateAnomalyDetectorResponse' {} a -> s {anomalyDetectorArn = a} :: UpdateAnomalyDetectorResponse)

-- | The response's http status code.
updateAnomalyDetectorResponse_httpStatus :: Lens.Lens' UpdateAnomalyDetectorResponse Prelude.Int
updateAnomalyDetectorResponse_httpStatus = Lens.lens (\UpdateAnomalyDetectorResponse' {httpStatus} -> httpStatus) (\s@UpdateAnomalyDetectorResponse' {} a -> s {httpStatus = a} :: UpdateAnomalyDetectorResponse)

instance Prelude.NFData UpdateAnomalyDetectorResponse where
  rnf UpdateAnomalyDetectorResponse' {..} =
    Prelude.rnf anomalyDetectorArn
      `Prelude.seq` Prelude.rnf httpStatus
