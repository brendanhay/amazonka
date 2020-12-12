{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.S3Destination
  ( S3Destination (..),

    -- * Smart constructor
    mkS3Destination,

    -- * Lenses
    sdManifestKey,
    sdBucketName,
    sdRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration parameters for where in an S3 bucket to place the harvested content
--
-- /See:/ 'mkS3Destination' smart constructor.
data S3Destination = S3Destination'
  { manifestKey :: Lude.Text,
    bucketName :: Lude.Text,
    roleARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Destination' with the minimum fields required to make a request.
--
-- * 'bucketName' - The name of an S3 bucket within which harvested content will be exported
-- * 'manifestKey' - The key in the specified S3 bucket where the harvested top-level manifest will be placed.
-- * 'roleARN' - The IAM role used to write to the specified S3 bucket
mkS3Destination ::
  -- | 'manifestKey'
  Lude.Text ->
  -- | 'bucketName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  S3Destination
mkS3Destination pManifestKey_ pBucketName_ pRoleARN_ =
  S3Destination'
    { manifestKey = pManifestKey_,
      bucketName = pBucketName_,
      roleARN = pRoleARN_
    }

-- | The key in the specified S3 bucket where the harvested top-level manifest will be placed.
--
-- /Note:/ Consider using 'manifestKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdManifestKey :: Lens.Lens' S3Destination Lude.Text
sdManifestKey = Lens.lens (manifestKey :: S3Destination -> Lude.Text) (\s a -> s {manifestKey = a} :: S3Destination)
{-# DEPRECATED sdManifestKey "Use generic-lens or generic-optics with 'manifestKey' instead." #-}

-- | The name of an S3 bucket within which harvested content will be exported
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdBucketName :: Lens.Lens' S3Destination Lude.Text
sdBucketName = Lens.lens (bucketName :: S3Destination -> Lude.Text) (\s a -> s {bucketName = a} :: S3Destination)
{-# DEPRECATED sdBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The IAM role used to write to the specified S3 bucket
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdRoleARN :: Lens.Lens' S3Destination Lude.Text
sdRoleARN = Lens.lens (roleARN :: S3Destination -> Lude.Text) (\s a -> s {roleARN = a} :: S3Destination)
{-# DEPRECATED sdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON S3Destination where
  parseJSON =
    Lude.withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            Lude.<$> (x Lude..: "manifestKey")
            Lude.<*> (x Lude..: "bucketName")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON S3Destination where
  toJSON S3Destination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("manifestKey" Lude..= manifestKey),
            Lude.Just ("bucketName" Lude..= bucketName),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
