{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.S3OutputLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputLocation
  ( S3OutputLocation (..),

    -- * Smart constructor
    mkS3OutputLocation,

    -- * Lenses
    solOutputS3KeyPrefix,
    solOutputS3Region,
    solOutputS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An S3 bucket where you want to store the results of this request.
--
-- /See:/ 'mkS3OutputLocation' smart constructor.
data S3OutputLocation = S3OutputLocation'
  { outputS3KeyPrefix ::
      Lude.Maybe Lude.Text,
    outputS3Region :: Lude.Maybe Lude.Text,
    outputS3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3OutputLocation' with the minimum fields required to make a request.
--
-- * 'outputS3BucketName' - The name of the S3 bucket.
-- * 'outputS3KeyPrefix' - The S3 bucket subfolder.
-- * 'outputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
mkS3OutputLocation ::
  S3OutputLocation
mkS3OutputLocation =
  S3OutputLocation'
    { outputS3KeyPrefix = Lude.Nothing,
      outputS3Region = Lude.Nothing,
      outputS3BucketName = Lude.Nothing
    }

-- | The S3 bucket subfolder.
--
-- /Note:/ Consider using 'outputS3KeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3KeyPrefix :: Lens.Lens' S3OutputLocation (Lude.Maybe Lude.Text)
solOutputS3KeyPrefix = Lens.lens (outputS3KeyPrefix :: S3OutputLocation -> Lude.Maybe Lude.Text) (\s a -> s {outputS3KeyPrefix = a} :: S3OutputLocation)
{-# DEPRECATED solOutputS3KeyPrefix "Use generic-lens or generic-optics with 'outputS3KeyPrefix' instead." #-}

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Region of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3Region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3Region :: Lens.Lens' S3OutputLocation (Lude.Maybe Lude.Text)
solOutputS3Region = Lens.lens (outputS3Region :: S3OutputLocation -> Lude.Maybe Lude.Text) (\s a -> s {outputS3Region = a} :: S3OutputLocation)
{-# DEPRECATED solOutputS3Region "Use generic-lens or generic-optics with 'outputS3Region' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'outputS3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
solOutputS3BucketName :: Lens.Lens' S3OutputLocation (Lude.Maybe Lude.Text)
solOutputS3BucketName = Lens.lens (outputS3BucketName :: S3OutputLocation -> Lude.Maybe Lude.Text) (\s a -> s {outputS3BucketName = a} :: S3OutputLocation)
{-# DEPRECATED solOutputS3BucketName "Use generic-lens or generic-optics with 'outputS3BucketName' instead." #-}

instance Lude.FromJSON S3OutputLocation where
  parseJSON =
    Lude.withObject
      "S3OutputLocation"
      ( \x ->
          S3OutputLocation'
            Lude.<$> (x Lude..:? "OutputS3KeyPrefix")
            Lude.<*> (x Lude..:? "OutputS3Region")
            Lude.<*> (x Lude..:? "OutputS3BucketName")
      )

instance Lude.ToJSON S3OutputLocation where
  toJSON S3OutputLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OutputS3KeyPrefix" Lude..=) Lude.<$> outputS3KeyPrefix,
            ("OutputS3Region" Lude..=) Lude.<$> outputS3Region,
            ("OutputS3BucketName" Lude..=) Lude.<$> outputS3BucketName
          ]
      )
