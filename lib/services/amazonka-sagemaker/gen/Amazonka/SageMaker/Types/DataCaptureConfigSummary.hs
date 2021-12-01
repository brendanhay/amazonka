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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataCaptureConfigSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CaptureStatus

-- |
--
-- /See:/ 'newDataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { enableCapture :: Prelude.Bool,
    captureStatus :: CaptureStatus,
    currentSamplingPercentage :: Prelude.Natural,
    destinationS3Uri :: Prelude.Text,
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
-- 'enableCapture', 'dataCaptureConfigSummary_enableCapture' -
--
-- 'captureStatus', 'dataCaptureConfigSummary_captureStatus' -
--
-- 'currentSamplingPercentage', 'dataCaptureConfigSummary_currentSamplingPercentage' -
--
-- 'destinationS3Uri', 'dataCaptureConfigSummary_destinationS3Uri' -
--
-- 'kmsKeyId', 'dataCaptureConfigSummary_kmsKeyId' -
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

-- |
dataCaptureConfigSummary_enableCapture :: Lens.Lens' DataCaptureConfigSummary Prelude.Bool
dataCaptureConfigSummary_enableCapture = Lens.lens (\DataCaptureConfigSummary' {enableCapture} -> enableCapture) (\s@DataCaptureConfigSummary' {} a -> s {enableCapture = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_captureStatus :: Lens.Lens' DataCaptureConfigSummary CaptureStatus
dataCaptureConfigSummary_captureStatus = Lens.lens (\DataCaptureConfigSummary' {captureStatus} -> captureStatus) (\s@DataCaptureConfigSummary' {} a -> s {captureStatus = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_currentSamplingPercentage :: Lens.Lens' DataCaptureConfigSummary Prelude.Natural
dataCaptureConfigSummary_currentSamplingPercentage = Lens.lens (\DataCaptureConfigSummary' {currentSamplingPercentage} -> currentSamplingPercentage) (\s@DataCaptureConfigSummary' {} a -> s {currentSamplingPercentage = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_destinationS3Uri :: Lens.Lens' DataCaptureConfigSummary Prelude.Text
dataCaptureConfigSummary_destinationS3Uri = Lens.lens (\DataCaptureConfigSummary' {destinationS3Uri} -> destinationS3Uri) (\s@DataCaptureConfigSummary' {} a -> s {destinationS3Uri = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_kmsKeyId :: Lens.Lens' DataCaptureConfigSummary Prelude.Text
dataCaptureConfigSummary_kmsKeyId = Lens.lens (\DataCaptureConfigSummary' {kmsKeyId} -> kmsKeyId) (\s@DataCaptureConfigSummary' {} a -> s {kmsKeyId = a} :: DataCaptureConfigSummary)

instance Core.FromJSON DataCaptureConfigSummary where
  parseJSON =
    Core.withObject
      "DataCaptureConfigSummary"
      ( \x ->
          DataCaptureConfigSummary'
            Prelude.<$> (x Core..: "EnableCapture")
            Prelude.<*> (x Core..: "CaptureStatus")
            Prelude.<*> (x Core..: "CurrentSamplingPercentage")
            Prelude.<*> (x Core..: "DestinationS3Uri")
            Prelude.<*> (x Core..: "KmsKeyId")
      )

instance Prelude.Hashable DataCaptureConfigSummary where
  hashWithSalt salt' DataCaptureConfigSummary' {..} =
    salt' `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` destinationS3Uri
      `Prelude.hashWithSalt` currentSamplingPercentage
      `Prelude.hashWithSalt` captureStatus
      `Prelude.hashWithSalt` enableCapture

instance Prelude.NFData DataCaptureConfigSummary where
  rnf DataCaptureConfigSummary' {..} =
    Prelude.rnf enableCapture
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf destinationS3Uri
      `Prelude.seq` Prelude.rnf currentSamplingPercentage
      `Prelude.seq` Prelude.rnf captureStatus
