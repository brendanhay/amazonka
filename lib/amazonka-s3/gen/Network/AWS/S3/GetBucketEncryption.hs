{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default encryption configuration for an Amazon S3 bucket. For information about the Amazon S3 default encryption feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption> .
--
-- To use this operation, you must have permission to perform the @s3:GetEncryptionConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- The following operations are related to @GetBucketEncryption@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.GetBucketEncryption
  ( -- * Creating a request
    GetBucketEncryption (..),
    mkGetBucketEncryption,

    -- ** Request lenses
    gbeExpectedBucketOwner,
    gbeBucket,

    -- * Destructuring the response
    GetBucketEncryptionResponse (..),
    mkGetBucketEncryptionResponse,

    -- ** Response lenses
    gbersServerSideEncryptionConfiguration,
    gbersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketEncryption' smart constructor.
data GetBucketEncryption = GetBucketEncryption'
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

-- | Creates a value of 'GetBucketEncryption' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket from which the server-side encryption configuration is retrieved.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  GetBucketEncryption
mkGetBucketEncryption pBucket_ =
  GetBucketEncryption'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbeExpectedBucketOwner :: Lens.Lens' GetBucketEncryption (Lude.Maybe Lude.Text)
gbeExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketEncryption -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketEncryption)
{-# DEPRECATED gbeExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The name of the bucket from which the server-side encryption configuration is retrieved.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbeBucket :: Lens.Lens' GetBucketEncryption BucketName
gbeBucket = Lens.lens (bucket :: GetBucketEncryption -> BucketName) (\s a -> s {bucket = a} :: GetBucketEncryption)
{-# DEPRECATED gbeBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketEncryption where
  type Rs GetBucketEncryption = GetBucketEncryptionResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketEncryptionResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketEncryption where
  toHeaders GetBucketEncryption' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketEncryption where
  toPath GetBucketEncryption' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketEncryption where
  toQuery = Lude.const (Lude.mconcat ["encryption"])

-- | /See:/ 'mkGetBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { serverSideEncryptionConfiguration ::
      Lude.Maybe
        ServerSideEncryptionConfiguration,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketEncryptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'serverSideEncryptionConfiguration' - Undocumented field.
mkGetBucketEncryptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketEncryptionResponse
mkGetBucketEncryptionResponse pResponseStatus_ =
  GetBucketEncryptionResponse'
    { serverSideEncryptionConfiguration =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'serverSideEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbersServerSideEncryptionConfiguration :: Lens.Lens' GetBucketEncryptionResponse (Lude.Maybe ServerSideEncryptionConfiguration)
gbersServerSideEncryptionConfiguration = Lens.lens (serverSideEncryptionConfiguration :: GetBucketEncryptionResponse -> Lude.Maybe ServerSideEncryptionConfiguration) (\s a -> s {serverSideEncryptionConfiguration = a} :: GetBucketEncryptionResponse)
{-# DEPRECATED gbersServerSideEncryptionConfiguration "Use generic-lens or generic-optics with 'serverSideEncryptionConfiguration' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbersResponseStatus :: Lens.Lens' GetBucketEncryptionResponse Lude.Int
gbersResponseStatus = Lens.lens (responseStatus :: GetBucketEncryptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketEncryptionResponse)
{-# DEPRECATED gbersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
