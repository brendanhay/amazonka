{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.S3BucketDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.S3BucketDetail
  ( S3BucketDetail (..),

    -- * Smart constructor
    mkS3BucketDetail,

    -- * Lenses
    sbdARN,
    sbdCreatedAt,
    sbdOwner,
    sbdName,
    sbdDefaultServerSideEncryption,
    sbdPublicAccess,
    sbdType,
    sbdTags,
  )
where

import Network.AWS.GuardDuty.Types.DefaultServerSideEncryption
import Network.AWS.GuardDuty.Types.Owner
import Network.AWS.GuardDuty.Types.PublicAccess
import Network.AWS.GuardDuty.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information on the S3 bucket.
--
-- /See:/ 'mkS3BucketDetail' smart constructor.
data S3BucketDetail = S3BucketDetail'
  { arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    owner :: Lude.Maybe Owner,
    name :: Lude.Maybe Lude.Text,
    defaultServerSideEncryption ::
      Lude.Maybe DefaultServerSideEncryption,
    publicAccess :: Lude.Maybe PublicAccess,
    type' :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3BucketDetail' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the S3 bucket.
-- * 'createdAt' - The date and time the bucket was created at.
-- * 'defaultServerSideEncryption' - Describes the server side encryption method used in the S3 bucket.
-- * 'name' - The name of the S3 bucket.
-- * 'owner' - The owner of the S3 bucket.
-- * 'publicAccess' - Describes the public access policies that apply to the S3 bucket.
-- * 'tags' - All tags attached to the S3 bucket
-- * 'type'' - Describes whether the bucket is a source or destination bucket.
mkS3BucketDetail ::
  S3BucketDetail
mkS3BucketDetail =
  S3BucketDetail'
    { arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      owner = Lude.Nothing,
      name = Lude.Nothing,
      defaultServerSideEncryption = Lude.Nothing,
      publicAccess = Lude.Nothing,
      type' = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdARN :: Lens.Lens' S3BucketDetail (Lude.Maybe Lude.Text)
sbdARN = Lens.lens (arn :: S3BucketDetail -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: S3BucketDetail)
{-# DEPRECATED sbdARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date and time the bucket was created at.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdCreatedAt :: Lens.Lens' S3BucketDetail (Lude.Maybe Lude.Timestamp)
sbdCreatedAt = Lens.lens (createdAt :: S3BucketDetail -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: S3BucketDetail)
{-# DEPRECATED sbdCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The owner of the S3 bucket.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdOwner :: Lens.Lens' S3BucketDetail (Lude.Maybe Owner)
sbdOwner = Lens.lens (owner :: S3BucketDetail -> Lude.Maybe Owner) (\s a -> s {owner = a} :: S3BucketDetail)
{-# DEPRECATED sbdOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The name of the S3 bucket.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdName :: Lens.Lens' S3BucketDetail (Lude.Maybe Lude.Text)
sbdName = Lens.lens (name :: S3BucketDetail -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: S3BucketDetail)
{-# DEPRECATED sbdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Describes the server side encryption method used in the S3 bucket.
--
-- /Note:/ Consider using 'defaultServerSideEncryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdDefaultServerSideEncryption :: Lens.Lens' S3BucketDetail (Lude.Maybe DefaultServerSideEncryption)
sbdDefaultServerSideEncryption = Lens.lens (defaultServerSideEncryption :: S3BucketDetail -> Lude.Maybe DefaultServerSideEncryption) (\s a -> s {defaultServerSideEncryption = a} :: S3BucketDetail)
{-# DEPRECATED sbdDefaultServerSideEncryption "Use generic-lens or generic-optics with 'defaultServerSideEncryption' instead." #-}

-- | Describes the public access policies that apply to the S3 bucket.
--
-- /Note:/ Consider using 'publicAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdPublicAccess :: Lens.Lens' S3BucketDetail (Lude.Maybe PublicAccess)
sbdPublicAccess = Lens.lens (publicAccess :: S3BucketDetail -> Lude.Maybe PublicAccess) (\s a -> s {publicAccess = a} :: S3BucketDetail)
{-# DEPRECATED sbdPublicAccess "Use generic-lens or generic-optics with 'publicAccess' instead." #-}

-- | Describes whether the bucket is a source or destination bucket.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdType :: Lens.Lens' S3BucketDetail (Lude.Maybe Lude.Text)
sbdType = Lens.lens (type' :: S3BucketDetail -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: S3BucketDetail)
{-# DEPRECATED sbdType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | All tags attached to the S3 bucket
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sbdTags :: Lens.Lens' S3BucketDetail (Lude.Maybe [Tag])
sbdTags = Lens.lens (tags :: S3BucketDetail -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: S3BucketDetail)
{-# DEPRECATED sbdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON S3BucketDetail where
  parseJSON =
    Lude.withObject
      "S3BucketDetail"
      ( \x ->
          S3BucketDetail'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "owner")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "defaultServerSideEncryption")
            Lude.<*> (x Lude..:? "publicAccess")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
