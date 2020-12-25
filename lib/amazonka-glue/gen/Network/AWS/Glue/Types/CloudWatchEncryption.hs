{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryption
  ( CloudWatchEncryption (..),

    -- * Smart constructor
    mkCloudWatchEncryption,

    -- * Lenses
    cweCloudWatchEncryptionMode,
    cweKmsKeyArn,
  )
where

import qualified Network.AWS.Glue.Types.CloudWatchEncryptionMode as Types
import qualified Network.AWS.Glue.Types.KmsKeyArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies how Amazon CloudWatch data should be encrypted.
--
-- /See:/ 'mkCloudWatchEncryption' smart constructor.
data CloudWatchEncryption = CloudWatchEncryption'
  { -- | The encryption mode to use for CloudWatch data.
    cloudWatchEncryptionMode :: Core.Maybe Types.CloudWatchEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
    kmsKeyArn :: Core.Maybe Types.KmsKeyArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CloudWatchEncryption' value with any optional fields omitted.
mkCloudWatchEncryption ::
  CloudWatchEncryption
mkCloudWatchEncryption =
  CloudWatchEncryption'
    { cloudWatchEncryptionMode = Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The encryption mode to use for CloudWatch data.
--
-- /Note:/ Consider using 'cloudWatchEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweCloudWatchEncryptionMode :: Lens.Lens' CloudWatchEncryption (Core.Maybe Types.CloudWatchEncryptionMode)
cweCloudWatchEncryptionMode = Lens.field @"cloudWatchEncryptionMode"
{-# DEPRECATED cweCloudWatchEncryptionMode "Use generic-lens or generic-optics with 'cloudWatchEncryptionMode' instead." #-}

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the data.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweKmsKeyArn :: Lens.Lens' CloudWatchEncryption (Core.Maybe Types.KmsKeyArn)
cweKmsKeyArn = Lens.field @"kmsKeyArn"
{-# DEPRECATED cweKmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead." #-}

instance Core.FromJSON CloudWatchEncryption where
  toJSON CloudWatchEncryption {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchEncryptionMode" Core..=)
              Core.<$> cloudWatchEncryptionMode,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )

instance Core.FromJSON CloudWatchEncryption where
  parseJSON =
    Core.withObject "CloudWatchEncryption" Core.$
      \x ->
        CloudWatchEncryption'
          Core.<$> (x Core..:? "CloudWatchEncryptionMode")
          Core.<*> (x Core..:? "KmsKeyArn")
