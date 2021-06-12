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
-- Module      : Network.AWS.Glue.Types.S3Encryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Encryption where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.S3EncryptionMode
import qualified Network.AWS.Lens as Lens

-- | Specifies how Amazon Simple Storage Service (Amazon S3) data should be
-- encrypted.
--
-- /See:/ 'newS3Encryption' smart constructor.
data S3Encryption = S3Encryption'
  { -- | The encryption mode to use for Amazon S3 data.
    s3EncryptionMode :: Core.Maybe S3EncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
    -- data.
    kmsKeyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'S3Encryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3EncryptionMode', 's3Encryption_s3EncryptionMode' - The encryption mode to use for Amazon S3 data.
--
-- 'kmsKeyArn', 's3Encryption_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
newS3Encryption ::
  S3Encryption
newS3Encryption =
  S3Encryption'
    { s3EncryptionMode = Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The encryption mode to use for Amazon S3 data.
s3Encryption_s3EncryptionMode :: Lens.Lens' S3Encryption (Core.Maybe S3EncryptionMode)
s3Encryption_s3EncryptionMode = Lens.lens (\S3Encryption' {s3EncryptionMode} -> s3EncryptionMode) (\s@S3Encryption' {} a -> s {s3EncryptionMode = a} :: S3Encryption)

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
s3Encryption_kmsKeyArn :: Lens.Lens' S3Encryption (Core.Maybe Core.Text)
s3Encryption_kmsKeyArn = Lens.lens (\S3Encryption' {kmsKeyArn} -> kmsKeyArn) (\s@S3Encryption' {} a -> s {kmsKeyArn = a} :: S3Encryption)

instance Core.FromJSON S3Encryption where
  parseJSON =
    Core.withObject
      "S3Encryption"
      ( \x ->
          S3Encryption'
            Core.<$> (x Core..:? "S3EncryptionMode")
            Core.<*> (x Core..:? "KmsKeyArn")
      )

instance Core.Hashable S3Encryption

instance Core.NFData S3Encryption

instance Core.ToJSON S3Encryption where
  toJSON S3Encryption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3EncryptionMode" Core..=)
              Core.<$> s3EncryptionMode,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )
