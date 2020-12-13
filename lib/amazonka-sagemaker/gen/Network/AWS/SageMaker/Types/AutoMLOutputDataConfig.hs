{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
  ( AutoMLOutputDataConfig (..),

    -- * Smart constructor
    mkAutoMLOutputDataConfig,

    -- * Lenses
    amlodcS3OutputPath,
    amlodcKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The output data configuration.
--
-- /See:/ 'mkAutoMLOutputDataConfig' smart constructor.
data AutoMLOutputDataConfig = AutoMLOutputDataConfig'
  { -- | The Amazon S3 output path. Must be 128 characters or less.
    s3OutputPath :: Lude.Text,
    -- | The AWS KMS encryption key ID.
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoMLOutputDataConfig' with the minimum fields required to make a request.
--
-- * 's3OutputPath' - The Amazon S3 output path. Must be 128 characters or less.
-- * 'kmsKeyId' - The AWS KMS encryption key ID.
mkAutoMLOutputDataConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  AutoMLOutputDataConfig
mkAutoMLOutputDataConfig pS3OutputPath_ =
  AutoMLOutputDataConfig'
    { s3OutputPath = pS3OutputPath_,
      kmsKeyId = Lude.Nothing
    }

-- | The Amazon S3 output path. Must be 128 characters or less.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlodcS3OutputPath :: Lens.Lens' AutoMLOutputDataConfig Lude.Text
amlodcS3OutputPath = Lens.lens (s3OutputPath :: AutoMLOutputDataConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: AutoMLOutputDataConfig)
{-# DEPRECATED amlodcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | The AWS KMS encryption key ID.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
amlodcKMSKeyId :: Lens.Lens' AutoMLOutputDataConfig (Lude.Maybe Lude.Text)
amlodcKMSKeyId = Lens.lens (kmsKeyId :: AutoMLOutputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: AutoMLOutputDataConfig)
{-# DEPRECATED amlodcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON AutoMLOutputDataConfig where
  parseJSON =
    Lude.withObject
      "AutoMLOutputDataConfig"
      ( \x ->
          AutoMLOutputDataConfig'
            Lude.<$> (x Lude..: "S3OutputPath") Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON AutoMLOutputDataConfig where
  toJSON AutoMLOutputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3OutputPath" Lude..= s3OutputPath),
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
