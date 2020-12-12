{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.S3MachineLearningModelResourceData
  ( S3MachineLearningModelResourceData (..),

    -- * Smart constructor
    mkS3MachineLearningModelResourceData,

    -- * Lenses
    smlmrdOwnerSetting,
    smlmrdDestinationPath,
    smlmrdS3URI,
  )
where

import Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Attributes that define an Amazon S3 machine learning resource.
--
-- /See:/ 'mkS3MachineLearningModelResourceData' smart constructor.
data S3MachineLearningModelResourceData = S3MachineLearningModelResourceData'
  { ownerSetting ::
      Lude.Maybe
        ResourceDownloadOwnerSetting,
    destinationPath ::
      Lude.Maybe Lude.Text,
    s3URI ::
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

-- | Creates a value of 'S3MachineLearningModelResourceData' with the minimum fields required to make a request.
--
-- * 'destinationPath' - The absolute local path of the resource inside the Lambda environment.
-- * 'ownerSetting' - Undocumented field.
-- * 's3URI' - The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
mkS3MachineLearningModelResourceData ::
  S3MachineLearningModelResourceData
mkS3MachineLearningModelResourceData =
  S3MachineLearningModelResourceData'
    { ownerSetting = Lude.Nothing,
      destinationPath = Lude.Nothing,
      s3URI = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'ownerSetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdOwnerSetting :: Lens.Lens' S3MachineLearningModelResourceData (Lude.Maybe ResourceDownloadOwnerSetting)
smlmrdOwnerSetting = Lens.lens (ownerSetting :: S3MachineLearningModelResourceData -> Lude.Maybe ResourceDownloadOwnerSetting) (\s a -> s {ownerSetting = a} :: S3MachineLearningModelResourceData)
{-# DEPRECATED smlmrdOwnerSetting "Use generic-lens or generic-optics with 'ownerSetting' instead." #-}

-- | The absolute local path of the resource inside the Lambda environment.
--
-- /Note:/ Consider using 'destinationPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdDestinationPath :: Lens.Lens' S3MachineLearningModelResourceData (Lude.Maybe Lude.Text)
smlmrdDestinationPath = Lens.lens (destinationPath :: S3MachineLearningModelResourceData -> Lude.Maybe Lude.Text) (\s a -> s {destinationPath = a} :: S3MachineLearningModelResourceData)
{-# DEPRECATED smlmrdDestinationPath "Use generic-lens or generic-optics with 'destinationPath' instead." #-}

-- | The URI of the source model in an S3 bucket. The model package must be in tar.gz or .zip format.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smlmrdS3URI :: Lens.Lens' S3MachineLearningModelResourceData (Lude.Maybe Lude.Text)
smlmrdS3URI = Lens.lens (s3URI :: S3MachineLearningModelResourceData -> Lude.Maybe Lude.Text) (\s a -> s {s3URI = a} :: S3MachineLearningModelResourceData)
{-# DEPRECATED smlmrdS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON S3MachineLearningModelResourceData where
  parseJSON =
    Lude.withObject
      "S3MachineLearningModelResourceData"
      ( \x ->
          S3MachineLearningModelResourceData'
            Lude.<$> (x Lude..:? "OwnerSetting")
            Lude.<*> (x Lude..:? "DestinationPath")
            Lude.<*> (x Lude..:? "S3Uri")
      )

instance Lude.ToJSON S3MachineLearningModelResourceData where
  toJSON S3MachineLearningModelResourceData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("OwnerSetting" Lude..=) Lude.<$> ownerSetting,
            ("DestinationPath" Lude..=) Lude.<$> destinationPath,
            ("S3Uri" Lude..=) Lude.<$> s3URI
          ]
      )
