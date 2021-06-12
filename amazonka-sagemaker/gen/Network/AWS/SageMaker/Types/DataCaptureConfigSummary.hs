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
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfigSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfigSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.CaptureStatus

-- |
--
-- /See:/ 'newDataCaptureConfigSummary' smart constructor.
data DataCaptureConfigSummary = DataCaptureConfigSummary'
  { enableCapture :: Core.Bool,
    captureStatus :: CaptureStatus,
    currentSamplingPercentage :: Core.Natural,
    destinationS3Uri :: Core.Text,
    kmsKeyId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Bool ->
  -- | 'captureStatus'
  CaptureStatus ->
  -- | 'currentSamplingPercentage'
  Core.Natural ->
  -- | 'destinationS3Uri'
  Core.Text ->
  -- | 'kmsKeyId'
  Core.Text ->
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
dataCaptureConfigSummary_enableCapture :: Lens.Lens' DataCaptureConfigSummary Core.Bool
dataCaptureConfigSummary_enableCapture = Lens.lens (\DataCaptureConfigSummary' {enableCapture} -> enableCapture) (\s@DataCaptureConfigSummary' {} a -> s {enableCapture = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_captureStatus :: Lens.Lens' DataCaptureConfigSummary CaptureStatus
dataCaptureConfigSummary_captureStatus = Lens.lens (\DataCaptureConfigSummary' {captureStatus} -> captureStatus) (\s@DataCaptureConfigSummary' {} a -> s {captureStatus = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_currentSamplingPercentage :: Lens.Lens' DataCaptureConfigSummary Core.Natural
dataCaptureConfigSummary_currentSamplingPercentage = Lens.lens (\DataCaptureConfigSummary' {currentSamplingPercentage} -> currentSamplingPercentage) (\s@DataCaptureConfigSummary' {} a -> s {currentSamplingPercentage = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_destinationS3Uri :: Lens.Lens' DataCaptureConfigSummary Core.Text
dataCaptureConfigSummary_destinationS3Uri = Lens.lens (\DataCaptureConfigSummary' {destinationS3Uri} -> destinationS3Uri) (\s@DataCaptureConfigSummary' {} a -> s {destinationS3Uri = a} :: DataCaptureConfigSummary)

-- |
dataCaptureConfigSummary_kmsKeyId :: Lens.Lens' DataCaptureConfigSummary Core.Text
dataCaptureConfigSummary_kmsKeyId = Lens.lens (\DataCaptureConfigSummary' {kmsKeyId} -> kmsKeyId) (\s@DataCaptureConfigSummary' {} a -> s {kmsKeyId = a} :: DataCaptureConfigSummary)

instance Core.FromJSON DataCaptureConfigSummary where
  parseJSON =
    Core.withObject
      "DataCaptureConfigSummary"
      ( \x ->
          DataCaptureConfigSummary'
            Core.<$> (x Core..: "EnableCapture")
            Core.<*> (x Core..: "CaptureStatus")
            Core.<*> (x Core..: "CurrentSamplingPercentage")
            Core.<*> (x Core..: "DestinationS3Uri")
            Core.<*> (x Core..: "KmsKeyId")
      )

instance Core.Hashable DataCaptureConfigSummary

instance Core.NFData DataCaptureConfigSummary
