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
-- Module      : Amazonka.SageMaker.Types.BatchDataCaptureConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.BatchDataCaptureConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration to control how SageMaker captures inference data for batch
-- transform jobs.
--
-- /See:/ 'newBatchDataCaptureConfig' smart constructor.
data BatchDataCaptureConfig = BatchDataCaptureConfig'
  { -- | Flag that indicates whether to append inference id to the output.
    generateInferenceId :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service key that SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance that hosts the batch transform job.
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
    -- | The Amazon S3 location being used to capture the data.
    destinationS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDataCaptureConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'generateInferenceId', 'batchDataCaptureConfig_generateInferenceId' - Flag that indicates whether to append inference id to the output.
--
-- 'kmsKeyId', 'batchDataCaptureConfig_kmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the batch transform job.
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
-- 'destinationS3Uri', 'batchDataCaptureConfig_destinationS3Uri' - The Amazon S3 location being used to capture the data.
newBatchDataCaptureConfig ::
  -- | 'destinationS3Uri'
  Prelude.Text ->
  BatchDataCaptureConfig
newBatchDataCaptureConfig pDestinationS3Uri_ =
  BatchDataCaptureConfig'
    { generateInferenceId =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      destinationS3Uri = pDestinationS3Uri_
    }

-- | Flag that indicates whether to append inference id to the output.
batchDataCaptureConfig_generateInferenceId :: Lens.Lens' BatchDataCaptureConfig (Prelude.Maybe Prelude.Bool)
batchDataCaptureConfig_generateInferenceId = Lens.lens (\BatchDataCaptureConfig' {generateInferenceId} -> generateInferenceId) (\s@BatchDataCaptureConfig' {} a -> s {generateInferenceId = a} :: BatchDataCaptureConfig)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance that hosts the batch transform job.
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
batchDataCaptureConfig_kmsKeyId :: Lens.Lens' BatchDataCaptureConfig (Prelude.Maybe Prelude.Text)
batchDataCaptureConfig_kmsKeyId = Lens.lens (\BatchDataCaptureConfig' {kmsKeyId} -> kmsKeyId) (\s@BatchDataCaptureConfig' {} a -> s {kmsKeyId = a} :: BatchDataCaptureConfig)

-- | The Amazon S3 location being used to capture the data.
batchDataCaptureConfig_destinationS3Uri :: Lens.Lens' BatchDataCaptureConfig Prelude.Text
batchDataCaptureConfig_destinationS3Uri = Lens.lens (\BatchDataCaptureConfig' {destinationS3Uri} -> destinationS3Uri) (\s@BatchDataCaptureConfig' {} a -> s {destinationS3Uri = a} :: BatchDataCaptureConfig)

instance Data.FromJSON BatchDataCaptureConfig where
  parseJSON =
    Data.withObject
      "BatchDataCaptureConfig"
      ( \x ->
          BatchDataCaptureConfig'
            Prelude.<$> (x Data..:? "GenerateInferenceId")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "DestinationS3Uri")
      )

instance Prelude.Hashable BatchDataCaptureConfig where
  hashWithSalt _salt BatchDataCaptureConfig' {..} =
    _salt
      `Prelude.hashWithSalt` generateInferenceId
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` destinationS3Uri

instance Prelude.NFData BatchDataCaptureConfig where
  rnf BatchDataCaptureConfig' {..} =
    Prelude.rnf generateInferenceId `Prelude.seq`
      Prelude.rnf kmsKeyId `Prelude.seq`
        Prelude.rnf destinationS3Uri

instance Data.ToJSON BatchDataCaptureConfig where
  toJSON BatchDataCaptureConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("GenerateInferenceId" Data..=)
              Prelude.<$> generateInferenceId,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("DestinationS3Uri" Data..= destinationS3Uri)
          ]
      )
