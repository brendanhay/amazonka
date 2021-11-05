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
-- Module      : Amazonka.Personalize.Types.S3DataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.S3DataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of an Amazon S3 input or output bucket.
--
-- /See:/ 'newS3DataConfig' smart constructor.
data S3DataConfig = S3DataConfig'
  { -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
    -- that Amazon Personalize uses to encrypt or decrypt the input and output
    -- files of a batch inference job.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The file path of the Amazon S3 bucket.
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 's3DataConfig_kmsKeyArn' - The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- that Amazon Personalize uses to encrypt or decrypt the input and output
-- files of a batch inference job.
--
-- 'path', 's3DataConfig_path' - The file path of the Amazon S3 bucket.
newS3DataConfig ::
  -- | 'path'
  Prelude.Text ->
  S3DataConfig
newS3DataConfig pPath_ =
  S3DataConfig'
    { kmsKeyArn = Prelude.Nothing,
      path = pPath_
    }

-- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
-- that Amazon Personalize uses to encrypt or decrypt the input and output
-- files of a batch inference job.
s3DataConfig_kmsKeyArn :: Lens.Lens' S3DataConfig (Prelude.Maybe Prelude.Text)
s3DataConfig_kmsKeyArn = Lens.lens (\S3DataConfig' {kmsKeyArn} -> kmsKeyArn) (\s@S3DataConfig' {} a -> s {kmsKeyArn = a} :: S3DataConfig)

-- | The file path of the Amazon S3 bucket.
s3DataConfig_path :: Lens.Lens' S3DataConfig Prelude.Text
s3DataConfig_path = Lens.lens (\S3DataConfig' {path} -> path) (\s@S3DataConfig' {} a -> s {path = a} :: S3DataConfig)

instance Core.FromJSON S3DataConfig where
  parseJSON =
    Core.withObject
      "S3DataConfig"
      ( \x ->
          S3DataConfig'
            Prelude.<$> (x Core..:? "kmsKeyArn")
            Prelude.<*> (x Core..: "path")
      )

instance Prelude.Hashable S3DataConfig

instance Prelude.NFData S3DataConfig

instance Core.ToJSON S3DataConfig where
  toJSON S3DataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Core..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("path" Core..= path)
          ]
      )
