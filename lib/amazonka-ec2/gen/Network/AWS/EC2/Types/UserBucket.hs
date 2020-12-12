{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucket
  ( UserBucket (..),

    -- * Smart constructor
    mkUserBucket,

    -- * Lenses
    ubS3Key,
    ubS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'mkUserBucket' smart constructor.
data UserBucket = UserBucket'
  { s3Key :: Lude.Maybe Lude.Text,
    s3Bucket :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserBucket' with the minimum fields required to make a request.
--
-- * 's3Bucket' - The name of the Amazon S3 bucket where the disk image is located.
-- * 's3Key' - The file name of the disk image.
mkUserBucket ::
  UserBucket
mkUserBucket =
  UserBucket' {s3Key = Lude.Nothing, s3Bucket = Lude.Nothing}

-- | The file name of the disk image.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubS3Key :: Lens.Lens' UserBucket (Lude.Maybe Lude.Text)
ubS3Key = Lens.lens (s3Key :: UserBucket -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: UserBucket)
{-# DEPRECATED ubS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The name of the Amazon S3 bucket where the disk image is located.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubS3Bucket :: Lens.Lens' UserBucket (Lude.Maybe Lude.Text)
ubS3Bucket = Lens.lens (s3Bucket :: UserBucket -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: UserBucket)
{-# DEPRECATED ubS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.ToQuery UserBucket where
  toQuery UserBucket' {..} =
    Lude.mconcat ["S3Key" Lude.=: s3Key, "S3Bucket" Lude.=: s3Bucket]
