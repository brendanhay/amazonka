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
-- Module      : Network.AWS.SageMaker.Types.LabelingJobResourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobResourceConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides configuration information for labeling jobs.
--
-- /See:/ 'newLabelingJobResourceConfig' smart constructor.
data LabelingJobResourceConfig = LabelingJobResourceConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt data on the storage volume attached to the ML compute
    -- instance(s) that run the training job. The @VolumeKmsKeyId@ can be any
    -- of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    volumeKmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeKmsKeyId', 'labelingJobResourceConfig_volumeKmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume attached to the ML compute
-- instance(s) that run the training job. The @VolumeKmsKeyId@ can be any
-- of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
newLabelingJobResourceConfig ::
  LabelingJobResourceConfig
newLabelingJobResourceConfig =
  LabelingJobResourceConfig'
    { volumeKmsKeyId =
        Prelude.Nothing
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt data on the storage volume attached to the ML compute
-- instance(s) that run the training job. The @VolumeKmsKeyId@ can be any
-- of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
labelingJobResourceConfig_volumeKmsKeyId :: Lens.Lens' LabelingJobResourceConfig (Prelude.Maybe Prelude.Text)
labelingJobResourceConfig_volumeKmsKeyId = Lens.lens (\LabelingJobResourceConfig' {volumeKmsKeyId} -> volumeKmsKeyId) (\s@LabelingJobResourceConfig' {} a -> s {volumeKmsKeyId = a} :: LabelingJobResourceConfig)

instance Prelude.FromJSON LabelingJobResourceConfig where
  parseJSON =
    Prelude.withObject
      "LabelingJobResourceConfig"
      ( \x ->
          LabelingJobResourceConfig'
            Prelude.<$> (x Prelude..:? "VolumeKmsKeyId")
      )

instance Prelude.Hashable LabelingJobResourceConfig

instance Prelude.NFData LabelingJobResourceConfig

instance Prelude.ToJSON LabelingJobResourceConfig where
  toJSON LabelingJobResourceConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("VolumeKmsKeyId" Prelude..=)
              Prelude.<$> volumeKmsKeyId
          ]
      )
