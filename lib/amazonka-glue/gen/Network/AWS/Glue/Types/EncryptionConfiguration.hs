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
    ecS3Encryption,
    ecJobBookmarksEncryption,
    ecCloudWatchEncryption,
  )
where

import Network.AWS.Glue.Types.CloudWatchEncryption
import Network.AWS.Glue.Types.JobBookmarksEncryption
import Network.AWS.Glue.Types.S3Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies an encryption configuration.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { s3Encryption ::
      Lude.Maybe [S3Encryption],
    jobBookmarksEncryption ::
      Lude.Maybe JobBookmarksEncryption,
    cloudWatchEncryption ::
      Lude.Maybe CloudWatchEncryption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionConfiguration' with the minimum fields required to make a request.
--
-- * 'cloudWatchEncryption' - The encryption configuration for Amazon CloudWatch.
-- * 'jobBookmarksEncryption' - The encryption configuration for job bookmarks.
-- * 's3Encryption' - The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
mkEncryptionConfiguration ::
  EncryptionConfiguration
mkEncryptionConfiguration =
  EncryptionConfiguration'
    { s3Encryption = Lude.Nothing,
      jobBookmarksEncryption = Lude.Nothing,
      cloudWatchEncryption = Lude.Nothing
    }

-- | The encryption configuration for Amazon Simple Storage Service (Amazon S3) data.
--
-- /Note:/ Consider using 's3Encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecS3Encryption :: Lens.Lens' EncryptionConfiguration (Lude.Maybe [S3Encryption])
ecS3Encryption = Lens.lens (s3Encryption :: EncryptionConfiguration -> Lude.Maybe [S3Encryption]) (\s a -> s {s3Encryption = a} :: EncryptionConfiguration)
{-# DEPRECATED ecS3Encryption "Use generic-lens or generic-optics with 's3Encryption' instead." #-}

-- | The encryption configuration for job bookmarks.
--
-- /Note:/ Consider using 'jobBookmarksEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecJobBookmarksEncryption :: Lens.Lens' EncryptionConfiguration (Lude.Maybe JobBookmarksEncryption)
ecJobBookmarksEncryption = Lens.lens (jobBookmarksEncryption :: EncryptionConfiguration -> Lude.Maybe JobBookmarksEncryption) (\s a -> s {jobBookmarksEncryption = a} :: EncryptionConfiguration)
{-# DEPRECATED ecJobBookmarksEncryption "Use generic-lens or generic-optics with 'jobBookmarksEncryption' instead." #-}

-- | The encryption configuration for Amazon CloudWatch.
--
-- /Note:/ Consider using 'cloudWatchEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecCloudWatchEncryption :: Lens.Lens' EncryptionConfiguration (Lude.Maybe CloudWatchEncryption)
ecCloudWatchEncryption = Lens.lens (cloudWatchEncryption :: EncryptionConfiguration -> Lude.Maybe CloudWatchEncryption) (\s a -> s {cloudWatchEncryption = a} :: EncryptionConfiguration)
{-# DEPRECATED ecCloudWatchEncryption "Use generic-lens or generic-optics with 'cloudWatchEncryption' instead." #-}

instance Lude.FromJSON EncryptionConfiguration where
  parseJSON =
    Lude.withObject
      "EncryptionConfiguration"
      ( \x ->
          EncryptionConfiguration'
            Lude.<$> (x Lude..:? "S3Encryption" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "JobBookmarksEncryption")
            Lude.<*> (x Lude..:? "CloudWatchEncryption")
      )

instance Lude.ToJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3Encryption" Lude..=) Lude.<$> s3Encryption,
            ("JobBookmarksEncryption" Lude..=) Lude.<$> jobBookmarksEncryption,
            ("CloudWatchEncryption" Lude..=) Lude.<$> cloudWatchEncryption
          ]
      )
