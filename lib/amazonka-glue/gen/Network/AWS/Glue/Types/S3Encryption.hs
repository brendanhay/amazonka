{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.S3Encryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.S3Encryption
  ( S3Encryption (..),

    -- * Smart constructor
    mkS3Encryption,

    -- * Lenses
    seKmsKeyArn,
    seS3EncryptionMode,
  )
where

import qualified Network.AWS.Glue.Types.KmsKeyArn as Types
import qualified Network.AWS.Glue.Types.S3EncryptionMode as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies how Amazon Simple Storage Service (Amazon S3) data should be encrypted.
--
-- /See:/ 'mkS3Encryption' smart constructor.
data S3Encryption = S3Encryption'
  { -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
    kmsKeyArn :: Core.Maybe Types.KmsKeyArn,
    -- | The encryption mode to use for Amazon S3 data.
    s3EncryptionMode :: Core.Maybe Types.S3EncryptionMode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Encryption' value with any optional fields omitted.
mkS3Encryption ::
  S3Encryption
mkS3Encryption =
  S3Encryption'
    { kmsKeyArn = Core.Nothing,
      s3EncryptionMode = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seKmsKeyArn :: Lens.Lens' S3Encryption (Core.Maybe Types.KmsKeyArn)
seKmsKeyArn = Lens.field @"kmsKeyArn"
{-# DEPRECATED seKmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead." #-}

-- | The encryption mode to use for Amazon S3 data.
--
-- /Note:/ Consider using 's3EncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seS3EncryptionMode :: Lens.Lens' S3Encryption (Core.Maybe Types.S3EncryptionMode)
seS3EncryptionMode = Lens.field @"s3EncryptionMode"
{-# DEPRECATED seS3EncryptionMode "Use generic-lens or generic-optics with 's3EncryptionMode' instead." #-}

instance Core.FromJSON S3Encryption where
  toJSON S3Encryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn,
            ("S3EncryptionMode" Core..=) Core.<$> s3EncryptionMode
          ]
      )

instance Core.FromJSON S3Encryption where
  parseJSON =
    Core.withObject "S3Encryption" Core.$
      \x ->
        S3Encryption'
          Core.<$> (x Core..:? "KmsKeyArn") Core.<*> (x Core..:? "S3EncryptionMode")
