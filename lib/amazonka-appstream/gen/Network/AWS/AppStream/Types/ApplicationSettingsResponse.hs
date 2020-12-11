-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ApplicationSettingsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ApplicationSettingsResponse
  ( ApplicationSettingsResponse (..),

    -- * Smart constructor
    mkApplicationSettingsResponse,

    -- * Lenses
    asEnabled,
    asSettingsGroup,
    asS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the persistent application settings for users of a stack.
--
-- /See:/ 'mkApplicationSettingsResponse' smart constructor.
data ApplicationSettingsResponse = ApplicationSettingsResponse'
  { enabled ::
      Lude.Maybe Lude.Bool,
    settingsGroup ::
      Lude.Maybe Lude.Text,
    s3BucketName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplicationSettingsResponse' with the minimum fields required to make a request.
--
-- * 'enabled' - Specifies whether persistent application settings are enabled for users during their streaming sessions.
-- * 's3BucketName' - The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region.
-- * 'settingsGroup' - The path prefix for the S3 bucket where users’ persistent application settings are stored.
mkApplicationSettingsResponse ::
  ApplicationSettingsResponse
mkApplicationSettingsResponse =
  ApplicationSettingsResponse'
    { enabled = Lude.Nothing,
      settingsGroup = Lude.Nothing,
      s3BucketName = Lude.Nothing
    }

-- | Specifies whether persistent application settings are enabled for users during their streaming sessions.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEnabled :: Lens.Lens' ApplicationSettingsResponse (Lude.Maybe Lude.Bool)
asEnabled = Lens.lens (enabled :: ApplicationSettingsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: ApplicationSettingsResponse)
{-# DEPRECATED asEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The path prefix for the S3 bucket where users’ persistent application settings are stored.
--
-- /Note:/ Consider using 'settingsGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSettingsGroup :: Lens.Lens' ApplicationSettingsResponse (Lude.Maybe Lude.Text)
asSettingsGroup = Lens.lens (settingsGroup :: ApplicationSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {settingsGroup = a} :: ApplicationSettingsResponse)
{-# DEPRECATED asSettingsGroup "Use generic-lens or generic-optics with 'settingsGroup' instead." #-}

-- | The S3 bucket where users’ persistent application settings are stored. When persistent application settings are enabled for the first time for an account in an AWS Region, an S3 bucket is created. The bucket is unique to the AWS account and the Region.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asS3BucketName :: Lens.Lens' ApplicationSettingsResponse (Lude.Maybe Lude.Text)
asS3BucketName = Lens.lens (s3BucketName :: ApplicationSettingsResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: ApplicationSettingsResponse)
{-# DEPRECATED asS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.FromJSON ApplicationSettingsResponse where
  parseJSON =
    Lude.withObject
      "ApplicationSettingsResponse"
      ( \x ->
          ApplicationSettingsResponse'
            Lude.<$> (x Lude..:? "Enabled")
            Lude.<*> (x Lude..:? "SettingsGroup")
            Lude.<*> (x Lude..:? "S3BucketName")
      )
