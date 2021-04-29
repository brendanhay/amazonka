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
-- Module      : Network.AWS.Comprehend.Types.PiiOutputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiOutputDataConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
piiOutputDataConfig_s3Uri :: Lens.Lens' PiiOutputDataConfig Prelude.Text
piiOutputDataConfig_s3Uri = Lens.lens (\PiiOutputDataConfig' {s3Uri} -> s3Uri) (\s@PiiOutputDataConfig' {} a -> s {s3Uri = a} :: PiiOutputDataConfig)

instance Prelude.FromJSON PiiOutputDataConfig where
  parseJSON =
    Prelude.withObject
      "PiiOutputDataConfig"
      ( \x ->
          PiiOutputDataConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable PiiOutputDataConfig

instance Prelude.NFData PiiOutputDataConfig
