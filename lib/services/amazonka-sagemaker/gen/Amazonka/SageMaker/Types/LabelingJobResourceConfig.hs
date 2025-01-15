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
-- Module      : Amazonka.SageMaker.Types.LabelingJobResourceConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.VpcConfig

-- | Configure encryption on the storage volume attached to the ML compute
-- instance used to run automated data labeling model training and
-- inference.
--
-- /See:/ 'newLabelingJobResourceConfig' smart constructor.
data LabelingJobResourceConfig = LabelingJobResourceConfig'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data on the storage volume
    -- attached to the ML compute instance(s) that run the training and
    -- inference jobs used for automated data labeling.
    --
    -- You can only specify a @VolumeKmsKeyId@ when you create a labeling job
    -- with automated data labeling enabled using the API operation
    -- @CreateLabelingJob@. You cannot specify an Amazon Web Services KMS key
    -- to encrypt the storage volume used for automated data labeling model
    -- training and inference when you create a labeling job using the console.
    -- To learn more, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security.html Output Data and Storage Volume Encryption>.
    --
    -- The @VolumeKmsKeyId@ can be any of the following formats:
    --
    -- -   KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text,
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'labelingJobResourceConfig_volumeKmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance(s) that run the training and
-- inference jobs used for automated data labeling.
--
-- You can only specify a @VolumeKmsKeyId@ when you create a labeling job
-- with automated data labeling enabled using the API operation
-- @CreateLabelingJob@. You cannot specify an Amazon Web Services KMS key
-- to encrypt the storage volume used for automated data labeling model
-- training and inference when you create a labeling job using the console.
-- To learn more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security.html Output Data and Storage Volume Encryption>.
--
-- The @VolumeKmsKeyId@ can be any of the following formats:
--
-- -   KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- 'vpcConfig', 'labelingJobResourceConfig_vpcConfig' - Undocumented member.
newLabelingJobResourceConfig ::
  LabelingJobResourceConfig
newLabelingJobResourceConfig =
  LabelingJobResourceConfig'
    { volumeKmsKeyId =
        Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- attached to the ML compute instance(s) that run the training and
-- inference jobs used for automated data labeling.
--
-- You can only specify a @VolumeKmsKeyId@ when you create a labeling job
-- with automated data labeling enabled using the API operation
-- @CreateLabelingJob@. You cannot specify an Amazon Web Services KMS key
-- to encrypt the storage volume used for automated data labeling model
-- training and inference when you create a labeling job using the console.
-- To learn more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security.html Output Data and Storage Volume Encryption>.
--
-- The @VolumeKmsKeyId@ can be any of the following formats:
--
-- -   KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
labelingJobResourceConfig_volumeKmsKeyId :: Lens.Lens' LabelingJobResourceConfig (Prelude.Maybe Prelude.Text)
labelingJobResourceConfig_volumeKmsKeyId = Lens.lens (\LabelingJobResourceConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@LabelingJobResourceConfig' {} a -> s {volumeKmsKeyId = a} :: LabelingJobResourceConfig)

-- | Undocumented member.
labelingJobResourceConfig_vpcConfig :: Lens.Lens' LabelingJobResourceConfig (Prelude.Maybe VpcConfig)
labelingJobResourceConfig_vpcConfig = Lens.lens (\LabelingJobResourceConfig' {vpcConfig} -> vpcConfig) (\s@LabelingJobResourceConfig' {} a -> s {vpcConfig = a} :: LabelingJobResourceConfig)

instance Data.FromJSON LabelingJobResourceConfig where
  parseJSON =
    Data.withObject
      "LabelingJobResourceConfig"
      ( \x ->
          LabelingJobResourceConfig'
            Prelude.<$> (x Data..:? "VolumeKmsKeyId")
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable LabelingJobResourceConfig where
  hashWithSalt _salt LabelingJobResourceConfig' {..} =
    _salt
      `Prelude.hashWithSalt` volumeKmsKeyId
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData LabelingJobResourceConfig where
  rnf LabelingJobResourceConfig' {..} =
    Prelude.rnf volumeKmsKeyId `Prelude.seq`
      Prelude.rnf vpcConfig

instance Data.ToJSON LabelingJobResourceConfig where
  toJSON LabelingJobResourceConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VolumeKmsKeyId" Data..=)
              Prelude.<$> volumeKmsKeyId,
            ("VpcConfig" Data..=) Prelude.<$> vpcConfig
          ]
      )
