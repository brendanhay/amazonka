{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.S3LogsConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.S3LogsConfig
  ( S3LogsConfig (..),

    -- * Smart constructor
    mkS3LogsConfig,

    -- * Lenses
    slcStatus,
    slcLocation,
    slcEncryptionDisabled,
  )
where

import Network.AWS.CodeBuild.Types.LogsConfigStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about S3 logs for a build project.
--
-- /See:/ 'mkS3LogsConfig' smart constructor.
data S3LogsConfig = S3LogsConfig'
  { -- | The current status of the S3 build logs. Valid values are:
    --
    --
    --     * @ENABLED@ : S3 build logs are enabled for this build project.
    --
    --
    --     * @DISABLED@ : S3 build logs are not enabled for this build project.
    status :: LogsConfigStatusType,
    -- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ .
    location :: Lude.Maybe Lude.Text,
    -- | Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted.
    encryptionDisabled :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3LogsConfig' with the minimum fields required to make a request.
--
-- * 'status' - The current status of the S3 build logs. Valid values are:
--
--
--     * @ENABLED@ : S3 build logs are enabled for this build project.
--
--
--     * @DISABLED@ : S3 build logs are not enabled for this build project.
--
--
-- * 'location' - The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ .
-- * 'encryptionDisabled' - Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted.
mkS3LogsConfig ::
  -- | 'status'
  LogsConfigStatusType ->
  S3LogsConfig
mkS3LogsConfig pStatus_ =
  S3LogsConfig'
    { status = pStatus_,
      location = Lude.Nothing,
      encryptionDisabled = Lude.Nothing
    }

-- | The current status of the S3 build logs. Valid values are:
--
--
--     * @ENABLED@ : S3 build logs are enabled for this build project.
--
--
--     * @DISABLED@ : S3 build logs are not enabled for this build project.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcStatus :: Lens.Lens' S3LogsConfig LogsConfigStatusType
slcStatus = Lens.lens (status :: S3LogsConfig -> LogsConfigStatusType) (\s a -> s {status = a} :: S3LogsConfig)
{-# DEPRECATED slcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of an S3 bucket and the path prefix for S3 logs. If your Amazon S3 bucket name is @my-bucket@ , and your path prefix is @build-log@ , then acceptable formats are @my-bucket/build-log@ or @arn:aws:s3:::my-bucket/build-log@ .
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcLocation :: Lens.Lens' S3LogsConfig (Lude.Maybe Lude.Text)
slcLocation = Lens.lens (location :: S3LogsConfig -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: S3LogsConfig)
{-# DEPRECATED slcLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | Set to true if you do not want your S3 build log output encrypted. By default S3 build logs are encrypted.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slcEncryptionDisabled :: Lens.Lens' S3LogsConfig (Lude.Maybe Lude.Bool)
slcEncryptionDisabled = Lens.lens (encryptionDisabled :: S3LogsConfig -> Lude.Maybe Lude.Bool) (\s a -> s {encryptionDisabled = a} :: S3LogsConfig)
{-# DEPRECATED slcEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

instance Lude.FromJSON S3LogsConfig where
  parseJSON =
    Lude.withObject
      "S3LogsConfig"
      ( \x ->
          S3LogsConfig'
            Lude.<$> (x Lude..: "status")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "encryptionDisabled")
      )

instance Lude.ToJSON S3LogsConfig where
  toJSON S3LogsConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("status" Lude..= status),
            ("location" Lude..=) Lude.<$> location,
            ("encryptionDisabled" Lude..=) Lude.<$> encryptionDisabled
          ]
      )
