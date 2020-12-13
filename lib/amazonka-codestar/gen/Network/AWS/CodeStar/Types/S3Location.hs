{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStar.Types.S3Location
  ( S3Location (..),

    -- * Smart constructor
    mkS3Location,

    -- * Lenses
    slBucketKey,
    slBucketName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon S3 location where the source code files provided with the project request are stored.
--
-- /See:/ 'mkS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 object key where the source code files provided with the project request are stored.
    bucketKey :: Lude.Maybe Lude.Text,
    -- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
    bucketName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- * 'bucketKey' - The Amazon S3 object key where the source code files provided with the project request are stored.
-- * 'bucketName' - The Amazon S3 bucket name where the source code files provided with the project request are stored.
mkS3Location ::
  S3Location
mkS3Location =
  S3Location' {bucketKey = Lude.Nothing, bucketName = Lude.Nothing}

-- | The Amazon S3 object key where the source code files provided with the project request are stored.
--
-- /Note:/ Consider using 'bucketKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketKey :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucketKey = Lens.lens (bucketKey :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucketKey = a} :: S3Location)
{-# DEPRECATED slBucketKey "Use generic-lens or generic-optics with 'bucketKey' instead." #-}

-- | The Amazon S3 bucket name where the source code files provided with the project request are stored.
--
-- /Note:/ Consider using 'bucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slBucketName :: Lens.Lens' S3Location (Lude.Maybe Lude.Text)
slBucketName = Lens.lens (bucketName :: S3Location -> Lude.Maybe Lude.Text) (\s a -> s {bucketName = a} :: S3Location)
{-# DEPRECATED slBucketName "Use generic-lens or generic-optics with 'bucketName' instead." #-}

instance Lude.ToJSON S3Location where
  toJSON S3Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("bucketKey" Lude..=) Lude.<$> bucketKey,
            ("bucketName" Lude..=) Lude.<$> bucketName
          ]
      )
