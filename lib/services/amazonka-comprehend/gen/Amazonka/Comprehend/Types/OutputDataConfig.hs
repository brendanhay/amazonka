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
-- Module      : Amazonka.Comprehend.Types.OutputDataConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.OutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration parameters for the output of inference jobs.
--
-- /See:/ 'newOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
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
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When you use the @OutputDataConfig@ object with asynchronous operations,
    -- you specify the Amazon S3 location where you want to write the output
    -- data. The URI must be in the same region as the API endpoint that you
    -- are calling. The location is used as the prefix for the actual location
    -- of the output file.
    --
    -- When the topic detection job is finished, the service creates an output
    -- file in a directory specific to the job. The @S3Uri@ field contains the
    -- location of the output file, called @output.tar.gz@. It is a compressed
    -- archive that contains the ouput of the operation.
    --
    -- For a PII entity detection job, the output file is plain text, not a
    -- compressed archive. The output file name is the same as the input file,
    -- with @.out@ appended at the end.
    s3Uri :: Prelude.Text
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
-- 'kmsKeyId', 'outputDataConfig_kmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
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
-- 's3Uri', 'outputDataConfig_s3Uri' - When you use the @OutputDataConfig@ object with asynchronous operations,
-- you specify the Amazon S3 location where you want to write the output
-- data. The URI must be in the same region as the API endpoint that you
-- are calling. The location is used as the prefix for the actual location
-- of the output file.
--
-- When the topic detection job is finished, the service creates an output
-- file in a directory specific to the job. The @S3Uri@ field contains the
-- location of the output file, called @output.tar.gz@. It is a compressed
-- archive that contains the ouput of the operation.
--
-- For a PII entity detection job, the output file is plain text, not a
-- compressed archive. The output file name is the same as the input file,
-- with @.out@ appended at the end.
newOutputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  OutputDataConfig
newOutputDataConfig pS3Uri_ =
  OutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3Uri = pS3Uri_
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
outputDataConfig_kmsKeyId :: Lens.Lens' OutputDataConfig (Prelude.Maybe Prelude.Text)
outputDataConfig_kmsKeyId = Lens.lens (\OutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@OutputDataConfig' {} a -> s {kmsKeyId = a} :: OutputDataConfig)

-- | When you use the @OutputDataConfig@ object with asynchronous operations,
-- you specify the Amazon S3 location where you want to write the output
-- data. The URI must be in the same region as the API endpoint that you
-- are calling. The location is used as the prefix for the actual location
-- of the output file.
--
-- When the topic detection job is finished, the service creates an output
-- file in a directory specific to the job. The @S3Uri@ field contains the
-- location of the output file, called @output.tar.gz@. It is a compressed
-- archive that contains the ouput of the operation.
--
-- For a PII entity detection job, the output file is plain text, not a
-- compressed archive. The output file name is the same as the input file,
-- with @.out@ appended at the end.
outputDataConfig_s3Uri :: Lens.Lens' OutputDataConfig Prelude.Text
outputDataConfig_s3Uri = Lens.lens (\OutputDataConfig' {s3Uri} -> s3Uri) (\s@OutputDataConfig' {} a -> s {s3Uri = a} :: OutputDataConfig)

instance Core.FromJSON OutputDataConfig where
  parseJSON =
    Core.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Prelude.<$> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..: "S3Uri")
      )

instance Prelude.Hashable OutputDataConfig where
  hashWithSalt _salt OutputDataConfig' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData OutputDataConfig where
  rnf OutputDataConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3Uri

instance Core.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            Prelude.Just ("S3Uri" Core..= s3Uri)
          ]
      )
