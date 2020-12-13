{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3Configuration
  ( S3Configuration (..),

    -- * Smart constructor
    mkS3Configuration,

    -- * Lenses
    scBucketARN,
    scFileKey,
    scRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the Amazon S3 object that contains the data.
--
-- /See:/ 'mkS3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { -- | ARN of the S3 bucket that contains the data.
    bucketARN :: Lude.Text,
    -- | The name of the object that contains the data.
    fileKey :: Lude.Text,
    -- | IAM ARN of the role used to access the data.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'S3Configuration' with the minimum fields required to make a request.
--
-- * 'bucketARN' - ARN of the S3 bucket that contains the data.
-- * 'fileKey' - The name of the object that contains the data.
-- * 'roleARN' - IAM ARN of the role used to access the data.
mkS3Configuration ::
  -- | 'bucketARN'
  Lude.Text ->
  -- | 'fileKey'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  S3Configuration
mkS3Configuration pBucketARN_ pFileKey_ pRoleARN_ =
  S3Configuration'
    { bucketARN = pBucketARN_,
      fileKey = pFileKey_,
      roleARN = pRoleARN_
    }

-- | ARN of the S3 bucket that contains the data.
--
-- /Note:/ Consider using 'bucketARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scBucketARN :: Lens.Lens' S3Configuration Lude.Text
scBucketARN = Lens.lens (bucketARN :: S3Configuration -> Lude.Text) (\s a -> s {bucketARN = a} :: S3Configuration)
{-# DEPRECATED scBucketARN "Use generic-lens or generic-optics with 'bucketARN' instead." #-}

-- | The name of the object that contains the data.
--
-- /Note:/ Consider using 'fileKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scFileKey :: Lens.Lens' S3Configuration Lude.Text
scFileKey = Lens.lens (fileKey :: S3Configuration -> Lude.Text) (\s a -> s {fileKey = a} :: S3Configuration)
{-# DEPRECATED scFileKey "Use generic-lens or generic-optics with 'fileKey' instead." #-}

-- | IAM ARN of the role used to access the data.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scRoleARN :: Lens.Lens' S3Configuration Lude.Text
scRoleARN = Lens.lens (roleARN :: S3Configuration -> Lude.Text) (\s a -> s {roleARN = a} :: S3Configuration)
{-# DEPRECATED scRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("BucketARN" Lude..= bucketARN),
            Lude.Just ("FileKey" Lude..= fileKey),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
