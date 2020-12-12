{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.PackageSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageSource
  ( PackageSource (..),

    -- * Smart constructor
    mkPackageSource,

    -- * Lenses
    psS3Key,
    psS3BucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The S3 location for importing the package specified as @S3BucketName@ and @S3Key@
--
-- /See:/ 'mkPackageSource' smart constructor.
data PackageSource = PackageSource'
  { s3Key :: Lude.Maybe Lude.Text,
    s3BucketName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PackageSource' with the minimum fields required to make a request.
--
-- * 's3BucketName' - Name of the bucket containing the package.
-- * 's3Key' - Key (file name) of the package.
mkPackageSource ::
  PackageSource
mkPackageSource =
  PackageSource' {s3Key = Lude.Nothing, s3BucketName = Lude.Nothing}

-- | Key (file name) of the package.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psS3Key :: Lens.Lens' PackageSource (Lude.Maybe Lude.Text)
psS3Key = Lens.lens (s3Key :: PackageSource -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: PackageSource)
{-# DEPRECATED psS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | Name of the bucket containing the package.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psS3BucketName :: Lens.Lens' PackageSource (Lude.Maybe Lude.Text)
psS3BucketName = Lens.lens (s3BucketName :: PackageSource -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: PackageSource)
{-# DEPRECATED psS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

instance Lude.ToJSON PackageSource where
  toJSON PackageSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("S3Key" Lude..=) Lude.<$> s3Key,
            ("S3BucketName" Lude..=) Lude.<$> s3BucketName
          ]
      )
