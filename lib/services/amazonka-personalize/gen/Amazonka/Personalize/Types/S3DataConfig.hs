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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.S3DataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration details of an Amazon S3 input or output bucket.
--
-- /See:/ 'newS3DataConfig' smart constructor.
data S3DataConfig = S3DataConfig'
  { -- | The Amazon Resource Name (ARN) of the Key Management Service (KMS) key
    -- that Amazon Personalize uses to encrypt or decrypt the input and output
    -- files.
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
-- files.
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
-- files.
s3DataConfig_kmsKeyArn :: Lens.Lens' S3DataConfig (Prelude.Maybe Prelude.Text)
s3DataConfig_kmsKeyArn = Lens.lens (\S3DataConfig' {kmsKeyArn} -> kmsKeyArn) (\s@S3DataConfig' {} a -> s {kmsKeyArn = a} :: S3DataConfig)

-- | The file path of the Amazon S3 bucket.
s3DataConfig_path :: Lens.Lens' S3DataConfig Prelude.Text
s3DataConfig_path = Lens.lens (\S3DataConfig' {path} -> path) (\s@S3DataConfig' {} a -> s {path = a} :: S3DataConfig)

instance Data.FromJSON S3DataConfig where
  parseJSON =
    Data.withObject
      "S3DataConfig"
      ( \x ->
          S3DataConfig'
            Prelude.<$> (x Data..:? "kmsKeyArn")
            Prelude.<*> (x Data..: "path")
      )

instance Prelude.Hashable S3DataConfig where
  hashWithSalt _salt S3DataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` path

instance Prelude.NFData S3DataConfig where
  rnf S3DataConfig' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf path

instance Data.ToJSON S3DataConfig where
  toJSON S3DataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("kmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            Prelude.Just ("path" Data..= path)
          ]
      )
