{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DataCaptureConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DataCaptureConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
import Network.AWS.SageMaker.Types.CaptureOption

-- |
--
-- /See:/ 'newDataCaptureConfig' smart constructor.
data DataCaptureConfig = DataCaptureConfig'
  { captureContentTypeHeader :: Prelude.Maybe CaptureContentTypeHeader,
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    enableCapture :: Prelude.Maybe Prelude.Bool,
    initialSamplingPercentage :: Prelude.Natural,
    destinationS3Uri :: Prelude.Text,
    captureOptions :: Prelude.NonEmpty CaptureOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DataCaptureConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureContentTypeHeader', 'dataCaptureConfig_captureContentTypeHeader' -
--
-- 'kmsKeyId', 'dataCaptureConfig_kmsKeyId' -
--
-- 'enableCapture', 'dataCaptureConfig_enableCapture' -
--
-- 'initialSamplingPercentage', 'dataCaptureConfig_initialSamplingPercentage' -
--
-- 'destinationS3Uri', 'dataCaptureConfig_destinationS3Uri' -
--
-- 'captureOptions', 'dataCaptureConfig_captureOptions' -
newDataCaptureConfig ::
  -- | 'initialSamplingPercentage'
  Prelude.Natural ->
  -- | 'destinationS3Uri'
  Prelude.Text ->
  -- | 'captureOptions'
  Prelude.NonEmpty CaptureOption ->
  DataCaptureConfig
newDataCaptureConfig
  pInitialSamplingPercentage_
  pDestinationS3Uri_
  pCaptureOptions_ =
    DataCaptureConfig'
      { captureContentTypeHeader =
          Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        enableCapture = Prelude.Nothing,
        initialSamplingPercentage =
          pInitialSamplingPercentage_,
        destinationS3Uri = pDestinationS3Uri_,
        captureOptions =
          Prelude._Coerce Lens.# pCaptureOptions_
      }

-- |
dataCaptureConfig_captureContentTypeHeader :: Lens.Lens' DataCaptureConfig (Prelude.Maybe CaptureContentTypeHeader)
dataCaptureConfig_captureContentTypeHeader = Lens.lens (\DataCaptureConfig' {captureContentTypeHeader} -> captureContentTypeHeader) (\s@DataCaptureConfig' {} a -> s {captureContentTypeHeader = a} :: DataCaptureConfig)

-- |
dataCaptureConfig_kmsKeyId :: Lens.Lens' DataCaptureConfig (Prelude.Maybe Prelude.Text)
dataCaptureConfig_kmsKeyId = Lens.lens (\DataCaptureConfig' {kmsKeyId} -> kmsKeyId) (\s@DataCaptureConfig' {} a -> s {kmsKeyId = a} :: DataCaptureConfig)

-- |
dataCaptureConfig_enableCapture :: Lens.Lens' DataCaptureConfig (Prelude.Maybe Prelude.Bool)
dataCaptureConfig_enableCapture = Lens.lens (\DataCaptureConfig' {enableCapture} -> enableCapture) (\s@DataCaptureConfig' {} a -> s {enableCapture = a} :: DataCaptureConfig)

-- |
dataCaptureConfig_initialSamplingPercentage :: Lens.Lens' DataCaptureConfig Prelude.Natural
dataCaptureConfig_initialSamplingPercentage = Lens.lens (\DataCaptureConfig' {initialSamplingPercentage} -> initialSamplingPercentage) (\s@DataCaptureConfig' {} a -> s {initialSamplingPercentage = a} :: DataCaptureConfig)

-- |
dataCaptureConfig_destinationS3Uri :: Lens.Lens' DataCaptureConfig Prelude.Text
dataCaptureConfig_destinationS3Uri = Lens.lens (\DataCaptureConfig' {destinationS3Uri} -> destinationS3Uri) (\s@DataCaptureConfig' {} a -> s {destinationS3Uri = a} :: DataCaptureConfig)

-- |
dataCaptureConfig_captureOptions :: Lens.Lens' DataCaptureConfig (Prelude.NonEmpty CaptureOption)
dataCaptureConfig_captureOptions = Lens.lens (\DataCaptureConfig' {captureOptions} -> captureOptions) (\s@DataCaptureConfig' {} a -> s {captureOptions = a} :: DataCaptureConfig) Prelude.. Prelude._Coerce

instance Prelude.FromJSON DataCaptureConfig where
  parseJSON =
    Prelude.withObject
      "DataCaptureConfig"
      ( \x ->
          DataCaptureConfig'
            Prelude.<$> (x Prelude..:? "CaptureContentTypeHeader")
            Prelude.<*> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "EnableCapture")
            Prelude.<*> (x Prelude..: "InitialSamplingPercentage")
            Prelude.<*> (x Prelude..: "DestinationS3Uri")
            Prelude.<*> (x Prelude..: "CaptureOptions")
      )

instance Prelude.Hashable DataCaptureConfig

instance Prelude.NFData DataCaptureConfig

instance Prelude.ToJSON DataCaptureConfig where
  toJSON DataCaptureConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CaptureContentTypeHeader" Prelude..=)
              Prelude.<$> captureContentTypeHeader,
            ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("EnableCapture" Prelude..=)
              Prelude.<$> enableCapture,
            Prelude.Just
              ( "InitialSamplingPercentage"
                  Prelude..= initialSamplingPercentage
              ),
            Prelude.Just
              ("DestinationS3Uri" Prelude..= destinationS3Uri),
            Prelude.Just
              ("CaptureOptions" Prelude..= captureOptions)
          ]
      )
