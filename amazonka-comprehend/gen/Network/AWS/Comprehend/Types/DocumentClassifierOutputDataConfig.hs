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
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides output results configuration parameters for custom classifier
-- jobs.
--
-- /See:/ 'newDocumentClassifierOutputDataConfig' smart constructor.
data DocumentClassifierOutputDataConfig = DocumentClassifierOutputDataConfig'
  { -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt the output results from an analysis job. The KmsKeyId
    -- can be one of the following formats:
    --
    -- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   Amazon Resource Name (ARN) of a KMS Key:
    --     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
    --
    -- -   KMS Key Alias: @\"alias\/ExampleAlias\"@
    --
    -- -   ARN of a KMS Key Alias:
    --     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
    kmsKeyId :: Core.Maybe Core.Text,
    -- | When you use the @OutputDataConfig@ object while creating a custom
    -- classifier, you specify the Amazon S3 location where you want to write
    -- the confusion matrix. The URI must be in the same region as the API
    -- endpoint that you are calling. The location is used as the prefix for
    -- the actual location of this output file.
    --
    -- When the custom classifier job is finished, the service creates the
    -- output file in a directory specific to the job. The @S3Uri@ field
    -- contains the location of the output file, called @output.tar.gz@. It is
    -- a compressed archive that contains the confusion matrix.
    s3Uri :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DocumentClassifierOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'documentClassifierOutputDataConfig_kmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt the output results from an analysis job. The KmsKeyId
-- can be one of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   KMS Key Alias: @\"alias\/ExampleAlias\"@
--
-- -   ARN of a KMS Key Alias:
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
--
-- 's3Uri', 'documentClassifierOutputDataConfig_s3Uri' - When you use the @OutputDataConfig@ object while creating a custom
-- classifier, you specify the Amazon S3 location where you want to write
-- the confusion matrix. The URI must be in the same region as the API
-- endpoint that you are calling. The location is used as the prefix for
-- the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the
-- output file in a directory specific to the job. The @S3Uri@ field
-- contains the location of the output file, called @output.tar.gz@. It is
-- a compressed archive that contains the confusion matrix.
newDocumentClassifierOutputDataConfig ::
  DocumentClassifierOutputDataConfig
newDocumentClassifierOutputDataConfig =
  DocumentClassifierOutputDataConfig'
    { kmsKeyId =
        Core.Nothing,
      s3Uri = Core.Nothing
    }

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt the output results from an analysis job. The KmsKeyId
-- can be one of the following formats:
--
-- -   KMS Key ID: @\"1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   Amazon Resource Name (ARN) of a KMS Key:
--     @\"arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab\"@
--
-- -   KMS Key Alias: @\"alias\/ExampleAlias\"@
--
-- -   ARN of a KMS Key Alias:
--     @\"arn:aws:kms:us-west-2:111122223333:alias\/ExampleAlias\"@
documentClassifierOutputDataConfig_kmsKeyId :: Lens.Lens' DocumentClassifierOutputDataConfig (Core.Maybe Core.Text)
documentClassifierOutputDataConfig_kmsKeyId = Lens.lens (\DocumentClassifierOutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@DocumentClassifierOutputDataConfig' {} a -> s {kmsKeyId = a} :: DocumentClassifierOutputDataConfig)

-- | When you use the @OutputDataConfig@ object while creating a custom
-- classifier, you specify the Amazon S3 location where you want to write
-- the confusion matrix. The URI must be in the same region as the API
-- endpoint that you are calling. The location is used as the prefix for
-- the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the
-- output file in a directory specific to the job. The @S3Uri@ field
-- contains the location of the output file, called @output.tar.gz@. It is
-- a compressed archive that contains the confusion matrix.
documentClassifierOutputDataConfig_s3Uri :: Lens.Lens' DocumentClassifierOutputDataConfig (Core.Maybe Core.Text)
documentClassifierOutputDataConfig_s3Uri = Lens.lens (\DocumentClassifierOutputDataConfig' {s3Uri} -> s3Uri) (\s@DocumentClassifierOutputDataConfig' {} a -> s {s3Uri = a} :: DocumentClassifierOutputDataConfig)

instance
  Core.FromJSON
    DocumentClassifierOutputDataConfig
  where
  parseJSON =
    Core.withObject
      "DocumentClassifierOutputDataConfig"
      ( \x ->
          DocumentClassifierOutputDataConfig'
            Core.<$> (x Core..:? "KmsKeyId")
            Core.<*> (x Core..:? "S3Uri")
      )

instance
  Core.Hashable
    DocumentClassifierOutputDataConfig

instance
  Core.NFData
    DocumentClassifierOutputDataConfig

instance
  Core.ToJSON
    DocumentClassifierOutputDataConfig
  where
  toJSON DocumentClassifierOutputDataConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("S3Uri" Core..=) Core.<$> s3Uri
          ]
      )
