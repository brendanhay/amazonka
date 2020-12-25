{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..),

    -- * Smart constructor
    mkEncryptionConfiguration,

    -- * Lenses
    ecCloudWatchEncryption,
    ecJobBookmarksEncryption,
    ecS3Encryption,
  )
where

import qualified Network.AWS.Glue.Types.CloudWatchEncryption as Types
import qualified Network.AWS.Glue.Types.JobBookmarksEncryption as Types
import qualified Network.AWS.Glue.Types.S3Encryption as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies an encryption configuration.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | The encryption configuration for Amazon CloudWatch.
    cloudWatchEncryption :: Core.Maybe Types.CloudWatchEncryption,
    -- | The encryption configuration for job bookmarks.
    jobBookmarksEncryption :: Core.Maybe Types.JobBookmarksEncryption,
    -- | The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
    s3Encryption :: Core.Maybe [Types.S3Encryption]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfiguration' value with any optional fields omitted.
mkEncryptionConfiguration ::
  EncryptionConfiguration
mkEncryptionConfiguration =
  EncryptionConfiguration'
    { cloudWatchEncryption = Core.Nothing,
      jobBookmarksEncryption = Core.Nothing,
      s3Encryption = Core.Nothing
    }

-- | The encryption configuration for Amazon CloudWatch.
--
-- /Note:/ Consider using 'cloudWatchEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecCloudWatchEncryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.CloudWatchEncryption)
ecCloudWatchEncryption = Lens.field @"cloudWatchEncryption"
{-# DEPRECATED ecCloudWatchEncryption "Use generic-lens or generic-optics with 'cloudWatchEncryption' instead." #-}

-- | The encryption configuration for job bookmarks.
--
-- /Note:/ Consider using 'jobBookmarksEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecJobBookmarksEncryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.JobBookmarksEncryption)
ecJobBookmarksEncryption = Lens.field @"jobBookmarksEncryption"
{-# DEPRECATED ecJobBookmarksEncryption "Use generic-lens or generic-optics with 'jobBookmarksEncryption' instead." #-}

-- | The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
--
-- /Note:/ Consider using 's3Encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecS3Encryption :: Lens.Lens' EncryptionConfiguration (Core.Maybe [Types.S3Encryption])
ecS3Encryption = Lens.field @"s3Encryption"
{-# DEPRECATED ecS3Encryption "Use generic-lens or generic-optics with 's3Encryption' instead." #-}

instance Core.FromJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudWatchEncryption" Core..=) Core.<$> cloudWatchEncryption,
            ("JobBookmarksEncryption" Core..=) Core.<$> jobBookmarksEncryption,
            ("S3Encryption" Core..=) Core.<$> s3Encryption
          ]
      )

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject "EncryptionConfiguration" Core.$
      \x ->
        EncryptionConfiguration'
          Core.<$> (x Core..:? "CloudWatchEncryption")
          Core.<*> (x Core..:? "JobBookmarksEncryption")
          Core.<*> (x Core..:? "S3Encryption")
