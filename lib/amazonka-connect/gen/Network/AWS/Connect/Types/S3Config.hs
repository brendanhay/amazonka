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
    scBucketPrefix,
    scBucketName,
    scEncryptionConfig,
  )
where

import Network.AWS.Connect.Types.EncryptionConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the S3 storage type.
--
-- /See:/ 'mkS3Config' smart constructor.
data S3Config = S3Config'
  { -- | The S3 bucket prefix.
    bucketPrefix :: Lude.Text,
    -- | The S3 bucket name.
    bucketName :: Lude.Text,
    -- | The S3 encryption configuration.
    encryptionConfig :: Lude.Maybe EncryptionConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Config' with the minimum fields required to make a request.
--
-- * 'bucketPrefix' - The S3 bucket prefix.
-- * 'bucketName' - The S3 bucket name.
-- * 'encryptionConfig' - The S3 encryption configuration.
mkS3Config ::
  -- | 'bucketPrefix'
  Lude.Text ->
  -- | 'bucketName'
  Lude.Text ->
  S3Config
mkS3Config pBucketPrefix_ pBucketName_ =
  S3Config'
    { bucketPrefix = pBucketPrefix_,
      bucketName = pBucketName_,
      encryptionConfig = Lude.Nothing
    }

-- | The S3 bucket prefix.
--
-- /Note:/ Consider using 'bucketPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketPrefix :: Lens.Lens' S3Config Lude.Text
scBucketPrefix = Lens.lens (bucketPrefix :: S3Config -> Lude.Text) (\s a -> s {bucketPrefix = a} :: S3Config)
{-# DEPRECATED scBucketPrefix "Use generic-lens or generic-optics with 'bucketPrefix' instead." #-}

-- | The S3 bucket name.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketName :: Lens.Lens' S3Config Lude.Text
scBucketName = Lens.lens (bucketName :: S3Config -> Lude.Text) (\s a -> s {bucketName = a} :: S3Config)
{-# DEPRECATED scBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The S3 encryption configuration.
--
-- /Note:/ Consider using 'encryptionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scEncryptionConfig :: Lens.Lens' S3Config (Lude.Maybe EncryptionConfig)
scEncryptionConfig = Lens.lens (encryptionConfig :: S3Config -> Lude.Maybe EncryptionConfig) (\s a -> s {encryptionConfig = a} :: S3Config)
{-# DEPRECATED scEncryptionConfig "Use generic-lens or generic-optics with 'encryptionConfig' instead." #-}

instance Lude.FromJSON S3Config where
  parseJSON =
    Lude.withObject
      "S3Config"
      ( \x ->
          S3Config'
            Lude.<$> (x Lude..: "BucketPrefix")
            Lude.<*> (x Lude..: "BucketName")
            Lude.<*> (x Lude..:? "EncryptionConfig")
      )

instance Lude.ToJSON S3Config where
  toJSON S3Config' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BucketPrefix" Lude..= bucketPrefix),
            Lude.Just ("BucketName" Lude..= bucketName),
            ("EncryptionConfig" Lude..=) Lude.<$> encryptionConfig
          ]
      )
