{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE operation removes default encryption from the bucket. For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- To use this operation, you must have permissions to perform the @s3:PutEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
module Network.AWS.S3.DeleteBucketEncryption
  ( -- * Creating a request
    DeleteBucketEncryption (..),
    mkDeleteBucketEncryption,

    -- ** Request lenses
    dbeExpectedBucketOwner,
    dbeBucket,

    -- * Destructuring the response
    DeleteBucketEncryptionResponse (..),
    mkDeleteBucketEncryptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkDeleteBucketEncryption' smart constructor.
data DeleteBucketEncryption = DeleteBucketEncryption'
  { expectedBucketOwner ::
      Lude.Maybe Lude.Text,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketEncryption' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket containing the server-side encryption configuration to delete.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkDeleteBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  DeleteBucketEncryption
mkDeleteBucketEncryption pBucket_ =
  DeleteBucketEncryption'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbeExpectedBucketOwner :: Lens.Lens' DeleteBucketEncryption (Lude.Maybe Lude.Text)
dbeExpectedBucketOwner = Lens.lens (expectedBucketOwner :: DeleteBucketEncryption -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: DeleteBucketEncryption)
{-# DEPRECATED dbeExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket containing the server-side encryption configuration to delete.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbeBucket :: Lens.Lens' DeleteBucketEncryption BucketName
dbeBucket = Lens.lens (bucket :: DeleteBucketEncryption -> BucketName) (\s a -> s {bucket = a} :: DeleteBucketEncryption)
{-# DEPRECATED dbeBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest DeleteBucketEncryption where
  type Rs DeleteBucketEncryption = DeleteBucketEncryptionResponse
  request = Req.delete s3Service
  response = Res.receiveNull DeleteBucketEncryptionResponse'

instance Lude.ToHeaders DeleteBucketEncryption where
  toHeaders DeleteBucketEncryption' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath DeleteBucketEncryption where
  toPath DeleteBucketEncryption' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery DeleteBucketEncryption where
  toQuery = Lude.const (Lude.mconcat ["encryption"])

-- | /See:/ 'mkDeleteBucketEncryptionResponse' smart constructor.
data DeleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteBucketEncryptionResponse' with the minimum fields required to make a request.
mkDeleteBucketEncryptionResponse ::
  DeleteBucketEncryptionResponse
mkDeleteBucketEncryptionResponse = DeleteBucketEncryptionResponse'
