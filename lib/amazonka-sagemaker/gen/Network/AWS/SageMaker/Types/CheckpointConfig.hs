-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CheckpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CheckpointConfig
  ( CheckpointConfig (..),

    -- * Smart constructor
    mkCheckpointConfig,

    -- * Lenses
    ccLocalPath,
    ccS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the output location for managed spot training checkpoint data.
--
-- /See:/ 'mkCheckpointConfig' smart constructor.
data CheckpointConfig = CheckpointConfig'
  { localPath ::
      Lude.Maybe Lude.Text,
    s3URI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CheckpointConfig' with the minimum fields required to make a request.
--
-- * 'localPath' - (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ .
-- * 's3URI' - Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
mkCheckpointConfig ::
  -- | 's3URI'
  Lude.Text ->
  CheckpointConfig
mkCheckpointConfig pS3URI_ =
  CheckpointConfig' {localPath = Lude.Nothing, s3URI = pS3URI_}

-- | (Optional) The local directory where checkpoints are written. The default directory is @/opt/ml/checkpoints/@ .
--
-- /Note:/ Consider using 'localPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccLocalPath :: Lens.Lens' CheckpointConfig (Lude.Maybe Lude.Text)
ccLocalPath = Lens.lens (localPath :: CheckpointConfig -> Lude.Maybe Lude.Text) (\s a -> s {localPath = a} :: CheckpointConfig)
{-# DEPRECATED ccLocalPath "Use generic-lens or generic-optics with 'localPath' instead." #-}

-- | Identifies the S3 path where you want Amazon SageMaker to store checkpoints. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccS3URI :: Lens.Lens' CheckpointConfig Lude.Text
ccS3URI = Lens.lens (s3URI :: CheckpointConfig -> Lude.Text) (\s a -> s {s3URI = a} :: CheckpointConfig)
{-# DEPRECATED ccS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON CheckpointConfig where
  parseJSON =
    Lude.withObject
      "CheckpointConfig"
      ( \x ->
          CheckpointConfig'
            Lude.<$> (x Lude..:? "LocalPath") Lude.<*> (x Lude..: "S3Uri")
      )

instance Lude.ToJSON CheckpointConfig where
  toJSON CheckpointConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("LocalPath" Lude..=) Lude.<$> localPath,
            Lude.Just ("S3Uri" Lude..= s3URI)
          ]
      )
