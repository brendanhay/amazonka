{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.DataCaptureConfigSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataCaptureConfigSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CaptureStatus

-- | The currently active data capture configuration used by your Endpoint.
--
-- /See:/ 'newDataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { -- | Whether data capture is enabled or disabled.
    enableCapture :: Prelude.Bool,
    -- | Whether data capture is currently functional.
    captureStatus :: CaptureStatus,
    -- | The percentage of requests being captured by your Endpoint.
    currentSamplingPercentage :: Prelude.Natural,
    -- | The Amazon S3 location being used to capture the data.
    destinationS3Uri :: Prelude.Text,
    -- | The KMS key being used to encrypt the data in Amazon S3.
    kmsKeyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCaptureConfigSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableCapture', 'dataCaptureConfigSummary_enableCapture' - Whether data capture is enabled or disabled.
--
-- 'captureStatus', 'dataCaptureConfigSummary_captureStatus' - Whether data capture is currently functional.
--
-- 'currentSamplingPercentage', 'dataCaptureConfigSummary_currentSamplingPercentage' - The percentage of requests being captured by your Endpoint.
--
-- 'destinationS3Uri', 'dataCaptureConfigSummary_destinationS3Uri' - The Amazon S3 location being used to capture the data.
--
-- 'kmsKeyId', 'dataCaptureConfigSummary_kmsKeyId' - The KMS key being used to encrypt the data in Amazon S3.
newDataCaptureConfigSummary ::
  -- | 'enableCapture'
  Prelude.Bool ->
  -- | 'captureStatus'
  CaptureStatus ->
  -- | 'currentSamplingPercentage'
  Prelude.Natural ->
  -- | 'destinationS3Uri'
  Prelude.Text ->
  -- | 'kmsKeyId'
  Prelude.Text ->
  DataCaptureConfigSummary
newDataCaptureConfigSummary
  pEnableCapture_
  pCaptureStatus_
  pCurrentSamplingPercentage_
  pDestinationS3Uri_
  pKmsKeyId_ =
    DataCaptureConfigSummary'
      { enableCapture =
          pEnableCapture_,
        captureStatus = pCaptureStatus_,
        currentSamplingPercentage =
          pCurrentSamplingPercentage_,
        destinationS3Uri = pDestinationS3Uri_,
        kmsKeyId = pKmsKeyId_
      }

-- | Whether data capture is enabled or disabled.
dataCaptureConfigSummary_enableCapture :: Lens.Lens' DataCaptureConfigSummary Prelude.Bool
dataCaptureConfigSummary_enableCapture = Lens.lens (\DataCaptureConfigSummary' {enableCapture} -> enableCapture) (\s@DataCaptureConfigSummary' {} a -> s {enableCapture = a} :: DataCaptureConfigSummary)

-- | Whether data capture is currently functional.
dataCaptureConfigSummary_captureStatus :: Lens.Lens' DataCaptureConfigSummary CaptureStatus
dataCaptureConfigSummary_captureStatus = Lens.lens (\DataCaptureConfigSummary' {captureStatus} -> captureStatus) (\s@DataCaptureConfigSummary' {} a -> s {captureStatus = a} :: DataCaptureConfigSummary)

-- | The percentage of requests being captured by your Endpoint.
dataCaptureConfigSummary_currentSamplingPercentage :: Lens.Lens' DataCaptureConfigSummary Prelude.Natural
dataCaptureConfigSummary_currentSamplingPercentage = Lens.lens (\DataCaptureConfigSummary' {currentSamplingPercentage} -> currentSamplingPercentage) (\s@DataCaptureConfigSummary' {} a -> s {currentSamplingPercentage = a} :: DataCaptureConfigSummary)

-- | The Amazon S3 location being used to capture the data.
dataCaptureConfigSummary_destinationS3Uri :: Lens.Lens' DataCaptureConfigSummary Prelude.Text
dataCaptureConfigSummary_destinationS3Uri = Lens.lens (\DataCaptureConfigSummary' {destinationS3Uri} -> destinationS3Uri) (\s@DataCaptureConfigSummary' {} a -> s {destinationS3Uri = a} :: DataCaptureConfigSummary)

-- | The KMS key being used to encrypt the data in Amazon S3.
dataCaptureConfigSummary_kmsKeyId :: Lens.Lens' DataCaptureConfigSummary Prelude.Text
dataCaptureConfigSummary_kmsKeyId = Lens.lens (\DataCaptureConfigSummary' {kmsKeyId} -> kmsKeyId) (\s@DataCaptureConfigSummary' {} a -> s {kmsKeyId = a} :: DataCaptureConfigSummary)

instance Data.FromJSON DataCaptureConfigSummary where
  parseJSON =
    Data.withObject
      "DataCaptureConfigSummary"
      ( \x ->
          DataCaptureConfigSummary'
            Prelude.<$> (x Data..: "EnableCapture")
            Prelude.<*> (x Data..: "CaptureStatus")
            Prelude.<*> (x Data..: "CurrentSamplingPercentage")
            Prelude.<*> (x Data..: "DestinationS3Uri")
            Prelude.<*> (x Data..: "KmsKeyId")
      )

instance Prelude.Hashable DataCaptureConfigSummary where
  hashWithSalt _salt DataCaptureConfigSummary' {..} =
    _salt
      `Prelude.hashWithSalt` enableCapture
      `Prelude.hashWithSalt` captureStatus
      `Prelude.hashWithSalt` currentSamplingPercentage
      `Prelude.hashWithSalt` destinationS3Uri
      `Prelude.hashWithSalt` kmsKeyId

instance Prelude.NFData DataCaptureConfigSummary where
  rnf DataCaptureConfigSummary' {..} =
    Prelude.rnf enableCapture
      `Prelude.seq` Prelude.rnf captureStatus
      `Prelude.seq` Prelude.rnf currentSamplingPercentage
      `Prelude.seq` Prelude.rnf destinationS3Uri
      `Prelude.seq` Prelude.rnf kmsKeyId
