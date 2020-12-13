{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.OutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.OutputConfig
  ( OutputConfig (..),

    -- * Smart constructor
    mkOutputConfig,

    -- * Lenses
    ocS3KeyPrefix,
    ocS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The S3 bucket and folder location where training output is placed.
--
-- /See:/ 'mkOutputConfig' smart constructor.
data OutputConfig = OutputConfig'
  { -- | The prefix applied to the training output files.
    s3KeyPrefix :: Lude.Maybe Lude.Text,
    -- | The S3 bucket where training output is placed.
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputConfig' with the minimum fields required to make a request.
--
-- * 's3KeyPrefix' - The prefix applied to the training output files.
-- * 's3Bucket' - The S3 bucket where training output is placed.
mkOutputConfig ::
  OutputConfig
mkOutputConfig =
  OutputConfig'
    { s3KeyPrefix = Lude.Nothing,
      s3Bucket = Lude.Nothing
    }

-- | The prefix applied to the training output files.
--
-- /Note:/ Consider using 's3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3KeyPrefix :: Lens.Lens' OutputConfig (Lude.Maybe Lude.Text)
ocS3KeyPrefix = Lens.lens (s3KeyPrefix :: OutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {s3KeyPrefix = a} :: OutputConfig)
{-# DEPRECATED ocS3KeyPrefix "Use generic-lens or generic-optics with 's3KeyPrefix' instead." #-}

-- | The S3 bucket where training output is placed.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocS3Bucket :: Lens.Lens' OutputConfig (Lude.Maybe Lude.Text)
ocS3Bucket = Lens.lens (s3Bucket :: OutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: OutputConfig)
{-# DEPRECATED ocS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromJSON OutputConfig where
  parseJSON =
    Lude.withObject
      "OutputConfig"
      ( \x ->
          OutputConfig'
            Lude.<$> (x Lude..:? "S3KeyPrefix") Lude.<*> (x Lude..:? "S3Bucket")
      )

instance Lude.ToJSON OutputConfig where
  toJSON OutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3KeyPrefix" Lude..=) Lude.<$> s3KeyPrefix,
            ("S3Bucket" Lude..=) Lude.<$> s3Bucket
          ]
      )
