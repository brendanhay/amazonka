-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceUpdate
  ( S3ReferenceDataSourceUpdate (..),

    -- * Smart constructor
    mkS3ReferenceDataSourceUpdate,

    -- * Lenses
    srdsuBucketARNUpdate,
    srdsuFileKeyUpdate,
    srdsuReferenceRoleARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the S3 bucket name, object key name, and IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf and populate the in-application reference table.
--
-- /See:/ 'mkS3ReferenceDataSourceUpdate' smart constructor.
data S3ReferenceDataSourceUpdate = S3ReferenceDataSourceUpdate'
  { bucketARNUpdate ::
      Lude.Maybe Lude.Text,
    fileKeyUpdate ::
      Lude.Maybe Lude.Text,
    referenceRoleARNUpdate ::
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

-- | Creates a value of 'S3ReferenceDataSourceUpdate' with the minimum fields required to make a request.
--
-- * 'bucketARNUpdate' - Amazon Resource Name (ARN) of the S3 bucket.
-- * 'fileKeyUpdate' - Object key name.
-- * 'referenceRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
mkS3ReferenceDataSourceUpdate ::
  S3ReferenceDataSourceUpdate
mkS3ReferenceDataSourceUpdate =
  S3ReferenceDataSourceUpdate'
    { bucketARNUpdate = Lude.Nothing,
      fileKeyUpdate = Lude.Nothing,
      referenceRoleARNUpdate = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the S3 bucket.
--
-- /Note:/ Consider using 'bucketARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuBucketARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Lude.Maybe Lude.Text)
srdsuBucketARNUpdate = Lens.lens (bucketARNUpdate :: S3ReferenceDataSourceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {bucketARNUpdate = a} :: S3ReferenceDataSourceUpdate)
{-# DEPRECATED srdsuBucketARNUpdate "Use generic-lens or generic-optics with 'bucketARNUpdate' instead." #-}

-- | Object key name.
--
-- /Note:/ Consider using 'fileKeyUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuFileKeyUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Lude.Maybe Lude.Text)
srdsuFileKeyUpdate = Lens.lens (fileKeyUpdate :: S3ReferenceDataSourceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {fileKeyUpdate = a} :: S3ReferenceDataSourceUpdate)
{-# DEPRECATED srdsuFileKeyUpdate "Use generic-lens or generic-optics with 'fileKeyUpdate' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object and populate the in-application.
--
-- /Note:/ Consider using 'referenceRoleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdsuReferenceRoleARNUpdate :: Lens.Lens' S3ReferenceDataSourceUpdate (Lude.Maybe Lude.Text)
srdsuReferenceRoleARNUpdate = Lens.lens (referenceRoleARNUpdate :: S3ReferenceDataSourceUpdate -> Lude.Maybe Lude.Text) (\s a -> s {referenceRoleARNUpdate = a} :: S3ReferenceDataSourceUpdate)
{-# DEPRECATED srdsuReferenceRoleARNUpdate "Use generic-lens or generic-optics with 'referenceRoleARNUpdate' instead." #-}

instance Lude.ToJSON S3ReferenceDataSourceUpdate where
  toJSON S3ReferenceDataSourceUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BucketARNUpdate" Lude..=) Lude.<$> bucketARNUpdate,
            ("FileKeyUpdate" Lude..=) Lude.<$> fileKeyUpdate,
            ("ReferenceRoleARNUpdate" Lude..=)
              Lude.<$> referenceRoleARNUpdate
          ]
      )
