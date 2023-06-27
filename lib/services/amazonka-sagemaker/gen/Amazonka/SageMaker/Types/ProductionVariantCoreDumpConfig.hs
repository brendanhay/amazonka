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
-- Module      : Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies configuration for a core dump from the model container when
-- the process crashes.
--
-- /See:/ 'newProductionVariantCoreDumpConfig' smart constructor.
data ProductionVariantCoreDumpConfig = ProductionVariantCoreDumpConfig'
  { -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that SageMaker uses to encrypt the core dump data at rest using
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
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 bucket to send the core dump to.
    destinationS3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductionVariantCoreDumpConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'productionVariantCoreDumpConfig_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that SageMaker uses to encrypt the core dump data at rest using
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- 'destinationS3Uri', 'productionVariantCoreDumpConfig_destinationS3Uri' - The Amazon S3 bucket to send the core dump to.
newProductionVariantCoreDumpConfig ::
  -- | 'destinationS3Uri'
  Prelude.Text ->
  ProductionVariantCoreDumpConfig
newProductionVariantCoreDumpConfig pDestinationS3Uri_ =
  ProductionVariantCoreDumpConfig'
    { kmsKeyId =
        Prelude.Nothing,
      destinationS3Uri = pDestinationS3Uri_
    }

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that SageMaker uses to encrypt the core dump data at rest using
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateEndpoint@ and @UpdateEndpoint@ requests. For more
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in Amazon Web Services KMS>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
productionVariantCoreDumpConfig_kmsKeyId :: Lens.Lens' ProductionVariantCoreDumpConfig (Prelude.Maybe Prelude.Text)
productionVariantCoreDumpConfig_kmsKeyId = Lens.lens (\ProductionVariantCoreDumpConfig' {kmsKeyId} -> kmsKeyId) (\s@ProductionVariantCoreDumpConfig' {} a -> s {kmsKeyId = a} :: ProductionVariantCoreDumpConfig)

-- | The Amazon S3 bucket to send the core dump to.
productionVariantCoreDumpConfig_destinationS3Uri :: Lens.Lens' ProductionVariantCoreDumpConfig Prelude.Text
productionVariantCoreDumpConfig_destinationS3Uri = Lens.lens (\ProductionVariantCoreDumpConfig' {destinationS3Uri} -> destinationS3Uri) (\s@ProductionVariantCoreDumpConfig' {} a -> s {destinationS3Uri = a} :: ProductionVariantCoreDumpConfig)

instance
  Data.FromJSON
    ProductionVariantCoreDumpConfig
  where
  parseJSON =
    Data.withObject
      "ProductionVariantCoreDumpConfig"
      ( \x ->
          ProductionVariantCoreDumpConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "DestinationS3Uri")
      )

instance
  Prelude.Hashable
    ProductionVariantCoreDumpConfig
  where
  hashWithSalt
    _salt
    ProductionVariantCoreDumpConfig' {..} =
      _salt
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` destinationS3Uri

instance
  Prelude.NFData
    ProductionVariantCoreDumpConfig
  where
  rnf ProductionVariantCoreDumpConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf destinationS3Uri

instance Data.ToJSON ProductionVariantCoreDumpConfig where
  toJSON ProductionVariantCoreDumpConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            Prelude.Just
              ("DestinationS3Uri" Data..= destinationS3Uri)
          ]
      )
