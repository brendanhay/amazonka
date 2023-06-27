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
-- Module      : Amazonka.SageMaker.Types.OutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.OutputCompressionType

-- | Provides information about how to store model training results (model
-- artifacts).
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | The model output compression type. Select @None@ to output an
    -- uncompressed model, recommended for large model outputs. Defaults to
    -- gzip.
    compressionType :: Prelude.Maybe OutputCompressionType,
    -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that SageMaker uses to encrypt the model artifacts at rest using
    -- Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the
    -- following formats:
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
    -- If you use a KMS key ID or an alias of your KMS key, the SageMaker
    -- execution role must include permissions to call @kms:Encrypt@. If you
    -- don\'t provide a KMS key ID, SageMaker uses the default KMS key for
    -- Amazon S3 for your role\'s account. SageMaker uses server-side
    -- encryption with KMS-managed keys for @OutputDataConfig@. If you use a
    -- bucket policy with an @s3:PutObject@ permission that only allows objects
    -- with server-side encryption, set the condition key of
    -- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
    -- @CreateHyperParameterTuningJob@ requests. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Identifies the S3 path where you want SageMaker to store the model
    -- artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compressionType', 'outputDataConfig_compressionType' - The model output compression type. Select @None@ to output an
-- uncompressed model, recommended for large model outputs. Defaults to
-- gzip.
--
-- 'kmsKeyId', 'outputDataConfig_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that SageMaker uses to encrypt the model artifacts at rest using
-- Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the
-- following formats:
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
-- If you use a KMS key ID or an alias of your KMS key, the SageMaker
-- execution role must include permissions to call @kms:Encrypt@. If you
-- don\'t provide a KMS key ID, SageMaker uses the default KMS key for
-- Amazon S3 for your role\'s account. SageMaker uses server-side
-- encryption with KMS-managed keys for @OutputDataConfig@. If you use a
-- bucket policy with an @s3:PutObject@ permission that only allows objects
-- with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
-- @CreateHyperParameterTuningJob@ requests. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- 's3OutputPath', 'outputDataConfig_s3OutputPath' - Identifies the S3 path where you want SageMaker to store the model
-- artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
newOutputDataConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3OutputPath_ =
  OutputDataConfig'
    { compressionType =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The model output compression type. Select @None@ to output an
-- uncompressed model, recommended for large model outputs. Defaults to
-- gzip.
outputDataConfig_compressionType :: Lens.Lens' OutputDataConfig (Prelude.Maybe OutputCompressionType)
outputDataConfig_compressionType = Lens.lens (\OutputDataConfig' {compressionType} -> compressionType) (\s@OutputDataConfig' {} a -> s {compressionType = a} :: OutputDataConfig)

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that SageMaker uses to encrypt the model artifacts at rest using
-- Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the
-- following formats:
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
-- If you use a KMS key ID or an alias of your KMS key, the SageMaker
-- execution role must include permissions to call @kms:Encrypt@. If you
-- don\'t provide a KMS key ID, SageMaker uses the default KMS key for
-- Amazon S3 for your role\'s account. SageMaker uses server-side
-- encryption with KMS-managed keys for @OutputDataConfig@. If you use a
-- bucket policy with an @s3:PutObject@ permission that only allows objects
-- with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateTrainingJob@, @CreateTransformJob@, or
-- @CreateHyperParameterTuningJob@ requests. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
outputDataConfig_kmsKeyId :: Lens.Lens' OutputDataConfig (Prelude.Maybe Prelude.Text)
outputDataConfig_kmsKeyId = Lens.lens (\OutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@OutputDataConfig' {} a -> s {kmsKeyId = a} :: OutputDataConfig)

-- | Identifies the S3 path where you want SageMaker to store the model
-- artifacts. For example, @s3:\/\/bucket-name\/key-name-prefix@.
outputDataConfig_s3OutputPath :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3OutputPath = Lens.lens (\OutputDataConfig' {s3OutputPath} -> s3OutputPath) (\s@OutputDataConfig' {} a -> s {s3OutputPath = a} :: OutputDataConfig)

instance Data.FromJSON OutputDataConfig where
  parseJSON =
    Data.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Data..:? "CompressionType")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3OutputPath")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` compressionType
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf compressionType
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Data.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompressionType" Data..=)
              Prelude.<$> compressionType,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3OutputPath" Data..= s3OutputPath)
          ]
      )
