-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UserBucketDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UserBucketDetails
  ( UserBucketDetails (..),

    -- * Smart constructor
    mkUserBucketDetails,

    -- * Lenses
    ubdS3Key,
    ubdS3Bucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'mkUserBucketDetails' smart constructor.
data UserBucketDetails = UserBucketDetails'
  { s3Key ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'UserBucketDetails' with the minimum fields required to make a request.
--
-- * 's3Bucket' - The Amazon S3 bucket from which the disk image was created.
-- * 's3Key' - The file name of the disk image.
mkUserBucketDetails ::
  UserBucketDetails
mkUserBucketDetails =
  UserBucketDetails' {s3Key = Lude.Nothing, s3Bucket = Lude.Nothing}

-- | The file name of the disk image.
--
-- /Note:/ Consider using 's3Key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubdS3Key :: Lens.Lens' UserBucketDetails (Lude.Maybe Lude.Text)
ubdS3Key = Lens.lens (s3Key :: UserBucketDetails -> Lude.Maybe Lude.Text) (\s a -> s {s3Key = a} :: UserBucketDetails)
{-# DEPRECATED ubdS3Key "Use generic-lens or generic-optics with 's3Key' instead." #-}

-- | The Amazon S3 bucket from which the disk image was created.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ubdS3Bucket :: Lens.Lens' UserBucketDetails (Lude.Maybe Lude.Text)
ubdS3Bucket = Lens.lens (s3Bucket :: UserBucketDetails -> Lude.Maybe Lude.Text) (\s a -> s {s3Bucket = a} :: UserBucketDetails)
{-# DEPRECATED ubdS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.FromXML UserBucketDetails where
  parseXML x =
    UserBucketDetails'
      Lude.<$> (x Lude..@? "s3Key") Lude.<*> (x Lude..@? "s3Bucket")
