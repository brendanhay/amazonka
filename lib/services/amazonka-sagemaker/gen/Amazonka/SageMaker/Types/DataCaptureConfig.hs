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
-- Module      : Amazonka.SageMaker.Types.DataCaptureConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DataCaptureConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CaptureContentTypeHeader
import Amazonka.SageMaker.Types.CaptureOption

-- | Configuration to control how SageMaker captures inference data.
--
-- /See:/ 'newDataCaptureConfig' smart constructor.
data DataCaptureConfig = DataCaptureConfig'
  { -- | Configuration specifying how to treat different headers. If no headers
    -- are specified SageMaker will by default base64 encode when capturing the
    -- data.
    captureContentTypeHeader :: Prelude.Maybe CaptureContentTypeHeader,
    -- | Whether data capture should be enabled or disabled (defaults to
    -- enabled).
    enableCapture :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service key that SageMaker uses to encrypt the captured data at rest
    -- using Amazon S3 server-side encryption.
    --
    -- The KmsKeyId can be any of the following formats:
    --
    -- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Key ARN:
    --     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
    --
    -- -   Alias name: @alias\/ExampleAlias@
    --
    -- -   Alias name ARN:
    --     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The percentage of requests SageMaker will capture. A lower value is
    -- recommended for Endpoints with high traffic.
    initialSamplingPercentage :: Prelude.Natural,
    -- | The Amazon S3 location used to capture the data.
    destinationS3Uri :: Prelude.Text,
    -- | Specifies data Model Monitor will capture. You can configure whether to
    -- collect only input, only output, or both
    captureOptions :: Prelude.NonEmpty CaptureOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCaptureConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'captureContentTypeHeader', 'dataCaptureConfig_captureContentTypeHeader' - Configuration specifying how to treat different headers. If no headers
-- are specified SageMaker will by default base64 encode when capturing the
-- data.
--
-- 'enableCapture', 'dataCaptureConfig_enableCapture' - Whether data capture should be enabled or disabled (defaults to
-- enabled).
--
-- 'kmsKeyId', 'dataCaptureConfig_kmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt the captured data at rest
-- using Amazon S3 server-side encryption.
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
--
-- 'initialSamplingPercentage', 'dataCaptureConfig_initialSamplingPercentage' - The percentage of requests SageMaker will capture. A lower value is
-- recommended for Endpoints with high traffic.
--
-- 'destinationS3Uri', 'dataCaptureConfig_destinationS3Uri' - The Amazon S3 location used to capture the data.
--
-- 'captureOptions', 'dataCaptureConfig_captureOptions' - Specifies data Model Monitor will capture. You can configure whether to
-- collect only input, only output, or both
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
        enableCapture = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        initialSamplingPercentage =
          pInitialSamplingPercentage_,
        destinationS3Uri = pDestinationS3Uri_,
        captureOptions =
          Lens.coerced Lens.# pCaptureOptions_
      }

-- | Configuration specifying how to treat different headers. If no headers
-- are specified SageMaker will by default base64 encode when capturing the
-- data.
dataCaptureConfig_captureContentTypeHeader :: Lens.Lens' DataCaptureConfig (Prelude.Maybe CaptureContentTypeHeader)
dataCaptureConfig_captureContentTypeHeader = Lens.lens (\DataCaptureConfig' {captureContentTypeHeader} -> captureContentTypeHeader) (\s@DataCaptureConfig' {} a -> s {captureContentTypeHeader = a} :: DataCaptureConfig)

-- | Whether data capture should be enabled or disabled (defaults to
-- enabled).
dataCaptureConfig_enableCapture :: Lens.Lens' DataCaptureConfig (Prelude.Maybe Prelude.Bool)
dataCaptureConfig_enableCapture = Lens.lens (\DataCaptureConfig' {enableCapture} -> enableCapture) (\s@DataCaptureConfig' {} a -> s {enableCapture = a} :: DataCaptureConfig)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt the captured data at rest
-- using Amazon S3 server-side encryption.
--
-- The KmsKeyId can be any of the following formats:
--
-- -   Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Key ARN:
--     @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@
--
-- -   Alias name: @alias\/ExampleAlias@
--
-- -   Alias name ARN:
--     @arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias@
dataCaptureConfig_kmsKeyId :: Lens.Lens' DataCaptureConfig (Prelude.Maybe Prelude.Text)
dataCaptureConfig_kmsKeyId = Lens.lens (\DataCaptureConfig' {kmsKeyId} -> kmsKeyId) (\s@DataCaptureConfig' {} a -> s {kmsKeyId = a} :: DataCaptureConfig)

-- | The percentage of requests SageMaker will capture. A lower value is
-- recommended for Endpoints with high traffic.
dataCaptureConfig_initialSamplingPercentage :: Lens.Lens' DataCaptureConfig Prelude.Natural
dataCaptureConfig_initialSamplingPercentage = Lens.lens (\DataCaptureConfig' {initialSamplingPercentage} -> initialSamplingPercentage) (\s@DataCaptureConfig' {} a -> s {initialSamplingPercentage = a} :: DataCaptureConfig)

-- | The Amazon S3 location used to capture the data.
dataCaptureConfig_destinationS3Uri :: Lens.Lens' DataCaptureConfig Prelude.Text
dataCaptureConfig_destinationS3Uri = Lens.lens (\DataCaptureConfig' {destinationS3Uri} -> destinationS3Uri) (\s@DataCaptureConfig' {} a -> s {destinationS3Uri = a} :: DataCaptureConfig)

-- | Specifies data Model Monitor will capture. You can configure whether to
-- collect only input, only output, or both
dataCaptureConfig_captureOptions :: Lens.Lens' DataCaptureConfig (Prelude.NonEmpty CaptureOption)
dataCaptureConfig_captureOptions = Lens.lens (\DataCaptureConfig' {captureOptions} -> captureOptions) (\s@DataCaptureConfig' {} a -> s {captureOptions = a} :: DataCaptureConfig) Prelude.. Lens.coerced

instance Data.FromJSON DataCaptureConfig where
  parseJSON =
    Data.withObject
      "DataCaptureConfig"
      ( \x ->
          DataCaptureConfig'
            Prelude.<$> (x Data..:? "CaptureContentTypeHeader")
            Prelude.<*> (x Data..:? "EnableCapture")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "InitialSamplingPercentage")
            Prelude.<*> (x Data..: "DestinationS3Uri")
            Prelude.<*> (x Data..: "CaptureOptions")
      )

instance Prelude.Hashable DataCaptureConfig where
  hashWithSalt _salt DataCaptureConfig' {..} =
    _salt
      `Prelude.hashWithSalt` captureContentTypeHeader
      `Prelude.hashWithSalt` enableCapture
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` initialSamplingPercentage
      `Prelude.hashWithSalt` destinationS3Uri
      `Prelude.hashWithSalt` captureOptions

instance Prelude.NFData DataCaptureConfig where
  rnf DataCaptureConfig' {..} =
    Prelude.rnf captureContentTypeHeader
      `Prelude.seq` Prelude.rnf enableCapture
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf initialSamplingPercentage
      `Prelude.seq` Prelude.rnf destinationS3Uri
      `Prelude.seq` Prelude.rnf captureOptions

instance Data.ToJSON DataCaptureConfig where
  toJSON DataCaptureConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CaptureContentTypeHeader" Data..=)
              Prelude.<$> captureContentTypeHeader,
            ("EnableCapture" Data..=) Prelude.<$> enableCapture,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ( "InitialSamplingPercentage"
                  Data..= initialSamplingPercentage
              ),
            Prelude.Just
              ("DestinationS3Uri" Data..= destinationS3Uri),
            Prelude.Just
              ("CaptureOptions" Data..= captureOptions)
          ]
      )
