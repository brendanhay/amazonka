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
-- Module      : Amazonka.Comprehend.Types.PiiOutputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.PiiOutputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration parameters for the output of PII entity detection
-- jobs.
--
-- /See:/ 'newPiiOutputDataConfig' smart constructor.
data PiiOutputDataConfig = PiiOutputDataConfig'
  { -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
    -- uses to encrypt the output results from an analysis job.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When you use the @PiiOutputDataConfig@ object with asynchronous
    -- operations, you specify the Amazon S3 location where you want to write
    -- the output data.
    --
    -- For a PII entity detection job, the output file is plain text, not a
    -- compressed archive. The output file name is the same as the input file,
    -- with @.out@ appended at the end.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PiiOutputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'piiOutputDataConfig_kmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt the output results from an analysis job.
--
-- 's3Uri', 'piiOutputDataConfig_s3Uri' - When you use the @PiiOutputDataConfig@ object with asynchronous
-- operations, you specify the Amazon S3 location where you want to write
-- the output data.
--
-- For a PII entity detection job, the output file is plain text, not a
-- compressed archive. The output file name is the same as the input file,
-- with @.out@ appended at the end.
newPiiOutputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  PiiOutputDataConfig
newPiiOutputDataConfig pS3Uri_ =
  PiiOutputDataConfig'
    { kmsKeyId = Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend
-- uses to encrypt the output results from an analysis job.
piiOutputDataConfig_kmsKeyId :: Lens.Lens' PiiOutputDataConfig (Prelude.Maybe Prelude.Text)
piiOutputDataConfig_kmsKeyId = Lens.lens (\PiiOutputDataConfig' {kmsKeyId} -> kmsKeyId) (\s@PiiOutputDataConfig' {} a -> s {kmsKeyId = a} :: PiiOutputDataConfig)

-- | When you use the @PiiOutputDataConfig@ object with asynchronous
-- operations, you specify the Amazon S3 location where you want to write
-- the output data.
--
-- For a PII entity detection job, the output file is plain text, not a
-- compressed archive. The output file name is the same as the input file,
-- with @.out@ appended at the end.
piiOutputDataConfig_s3Uri :: Lens.Lens' PiiOutputDataConfig Prelude.Text
piiOutputDataConfig_s3Uri = Lens.lens (\PiiOutputDataConfig' {s3Uri} -> s3Uri) (\s@PiiOutputDataConfig' {} a -> s {s3Uri = a} :: PiiOutputDataConfig)

instance Data.FromJSON PiiOutputDataConfig where
  parseJSON =
    Data.withObject
      "PiiOutputDataConfig"
      ( \x ->
          PiiOutputDataConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable PiiOutputDataConfig where
  hashWithSalt _salt PiiOutputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData PiiOutputDataConfig where
  rnf PiiOutputDataConfig' {..} =
    Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf s3Uri
