{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.S3Config
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.S3Config
  ( S3Config (..),

    -- * Smart constructor
    mkS3Config,

    -- * Lenses
    scBucketName,
    scBucketPrefix,
    scEncryptionConfig,
  )
where

import qualified Network.AWS.Connect.Types.BucketName as Types
import qualified Network.AWS.Connect.Types.EncryptionConfig as Types
import qualified Network.AWS.Connect.Types.Prefix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the S3 storage type.
--
-- /See:/ 'mkS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The S3 bucket name.
    bucketName :: Types.BucketName,
    -- | The S3 bucket prefix.
    bucketPrefix :: Types.Prefix,
    -- | The S3 encryption configuration.
    encryptionConfig :: Core.Maybe Types.EncryptionConfig
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3Config' value with any optional fields omitted.
mkS3Config ::
  -- | 'bucketName'
  Types.BucketName ->
  -- | 'bucketPrefix'
  Types.Prefix ->
  S3Config
mkS3Config bucketName bucketPrefix =
  S3Config'
    { bucketName,
      bucketPrefix,
      encryptionConfig = Core.Nothing
    }

-- | The S3 bucket name.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketName :: Lens.Lens' S3Config Types.BucketName
scBucketName = Lens.field @"bucketName"
{-# DEPRECATED scBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 'bucketPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketPrefix :: Lens.Lens' S3Config Types.Prefix
scBucketPrefix = Lens.field @"bucketPrefix"
{-# DEPRECATED scBucketPrefix "Use generic-lens or generic-optics with 'bucketPrefix' instead." #-}

-- | The S3 encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEncryptionConfig :: Lens.Lens' S3Config (Core.Maybe Types.EncryptionConfig)
scEncryptionConfig = Lens.field @"encryptionConfig"
{-# DEPRECATED scEncryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead." #-}

instance Core.FromJSON S3Config where
  toJSON S3Config {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("BucketName" Core..= bucketName),
            Core.Just ("BucketPrefix" Core..= bucketPrefix),
            ("EncryptionConfig" Core..=) Core.<$> encryptionConfig
          ]
      )

instance Core.FromJSON S3Config where
  parseJSON =
    Core.withObject "S3Config" Core.$
      \x ->
        S3Config'
          Core.<$> (x Core..: "BucketName")
          Core.<*> (x Core..: "BucketPrefix")
          Core.<*> (x Core..:? "EncryptionConfig")
