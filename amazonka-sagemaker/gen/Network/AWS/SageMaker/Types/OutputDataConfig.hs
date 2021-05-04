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
-- Module      : Network.AWS.SageMaker.Types.OutputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OutputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about how to store model training results (model
-- artifacts).
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
    -- to encrypt the model artifacts at rest using Amazon S3 server-side
    -- encryption. The @KmsKeyId@ can be any of the following formats:
    --
    -- -   \/\/ KMS Key ID
    --
    --     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   \/\/ KMS Key Alias
    --
    --     @\"alias\/ExampleAlias\"@
    --
    -- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
    --
    --     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
    --
    -- If you use a KMS key ID or an alias of your master key, the Amazon
    -- SageMaker execution role must include permissions to call @kms:Encrypt@.
    -- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
    -- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
    -- server-side encryption with KMS-managed keys for @OutputDataConfig@. If
    -- you use a bucket policy with an @s3:PutObject@ permission that only
    -- allows objects with server-side encryption, set the condition key of
    -- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/mazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
    -- @CreateHyperParameterTuningJob@ requests. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
    -- in the /AWS Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 path where you want Amazon SageMaker to store the
    -- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'outputDataConfig_kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption. The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
--
-- If you use a KMS key ID or an alias of your master key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS-managed keys for @OutputDataConfig@. If
-- you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/mazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
-- @CreateHyperParameterTuningJob@ requests. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 's3OutputPath', 'outputDataConfig_s3OutputPath' - Identifies the S3 path where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newOutputDataConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3OutputPath_ =
  OutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses
-- to encrypt the model artifacts at rest using Amazon S3 server-side
-- encryption. The @KmsKeyId@ can be any of the following formats:
--
-- -   \/\/ KMS Key ID
--
--     @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key
--
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   \/\/ KMS Key Alias
--
--     @\"alias\/ExampleAlias\"@
--
-- -   \/\/ Amazon Resource Name (ARN) of a KMS Key Alias
--
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
--
-- If you use a KMS key ID or an alias of your master key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS-managed keys for @OutputDataConfig@. If
-- you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/mazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
-- @CreateHyperParameterTuningJob@ requests. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
outputDataConfig_kmsKeyId :: Lens.Lens' OutputDataConfig (Prelude.Maybe Prelude.Text)
outputDataConfig_kmsKeyId = Lens.lens (\OutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@OutputDataConfig' {} a -> s {kmsKeyId = a} :: OutputDataConfig)

-- | Identifies the S3 path where you want Amazon SageMaker to store the
-- model artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
outputDataConfig_s3OutputPath :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3OutputPath = Lens.lens (\OutputDataConfig' {s3OutputPath} -> s3OutputPath) (\s@OutputDataConfig' {} a -> s {s3OutputPath = a} :: OutputDataConfig)

instance Prelude.FromJSON OutputDataConfig where
  parseJSON =
    Prelude.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable OutputDataConfig

instance Prelude.NFData OutputDataConfig

instance Prelude.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )
