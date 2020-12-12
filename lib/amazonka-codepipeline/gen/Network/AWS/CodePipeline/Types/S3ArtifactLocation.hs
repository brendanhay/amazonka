{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.S3ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.S3ArtifactLocation
  ( S3ArtifactLocation (..),

    -- * Smart constructor
    mkS3ArtifactLocation,

    -- * Lenses
    salBucketName,
    salObjectKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location of the S3 bucket that contains a revision.
--
-- /See:/ 'mkS3ArtifactLocation' smart constructor.
data S3ArtifactLocation = S3ArtifactLocation'
  { bucketName ::
      Lude.Text,
    objectKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3ArtifactLocation' with the minimum fields required to make a request.
--
-- * 'bucketName' - The name of the S3 bucket.
-- * 'objectKey' - The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
mkS3ArtifactLocation ::
  -- | 'bucketName'
  Lude.Text ->
  -- | 'objectKey'
  Lude.Text ->
  S3ArtifactLocation
mkS3ArtifactLocation pBucketName_ pObjectKey_ =
  S3ArtifactLocation'
    { bucketName = pBucketName_,
      objectKey = pObjectKey_
    }

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salBucketName :: Lens.Lens' S3ArtifactLocation Lude.Text
salBucketName = Lens.lens (bucketName :: S3ArtifactLocation -> Lude.Text) (\s a -> s {bucketName = a} :: S3ArtifactLocation)
{-# DEPRECATED salBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

-- | The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
--
-- /Note:/ Consider using 'objectKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
salObjectKey :: Lens.Lens' S3ArtifactLocation Lude.Text
salObjectKey = Lens.lens (objectKey :: S3ArtifactLocation -> Lude.Text) (\s a -> s {objectKey = a} :: S3ArtifactLocation)
{-# DEPRECATED salObjectKey "Use generic-lens or generic-optics with 'objectKey' instead." #-}

instance Lude.FromJSON S3ArtifactLocation where
  parseJSON =
    Lude.withObject
      "S3ArtifactLocation"
      ( \x ->
          S3ArtifactLocation'
            Lude.<$> (x Lude..: "bucketName") Lude.<*> (x Lude..: "objectKey")
      )
