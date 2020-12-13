{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
-- To use this operation, you must have permission to perform the @s3:GetBucketTagging@ action. By default, the bucket owner has this permission and can grant this permission to others.
-- @GetBucketTagging@ has the following special error:
--
--     * Error code: @NoSuchTagSetError@
--
--     * Description: There is no tag set associated with the bucket.
--
--
--
--
-- The following operations are related to @GetBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketTagging.html PutBucketTagging>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.GetBucketTagging
  ( -- * Creating a request
    GetBucketTagging (..),
    mkGetBucketTagging,

    -- ** Request lenses
    gbtBucket,
    gbtExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketTaggingResponse (..),
    mkGetBucketTaggingResponse,

    -- ** Response lenses
    gbtrsTagSet,
    gbtrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketTagging' smart constructor.
data GetBucketTagging = GetBucketTagging'
  { -- | The name of the bucket for which to get the tagging information.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketTagging' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to get the tagging information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketTagging ::
  -- | 'bucket'
  BucketName ->
  GetBucketTagging
mkGetBucketTagging pBucket_ =
  GetBucketTagging'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to get the tagging information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtBucket :: Lens.Lens' GetBucketTagging BucketName
gbtBucket = Lens.lens (bucket :: GetBucketTagging -> BucketName) (\s a -> s {bucket = a} :: GetBucketTagging)
{-# DEPRECATED gbtBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtExpectedBucketOwner :: Lens.Lens' GetBucketTagging (Lude.Maybe Lude.Text)
gbtExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketTagging)
{-# DEPRECATED gbtExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketTagging where
  type Rs GetBucketTagging = GetBucketTaggingResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketTaggingResponse'
            Lude.<$> ( x Lude..@? "TagSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "Tag"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketTagging where
  toHeaders GetBucketTagging' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketTagging where
  toPath GetBucketTagging' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketTagging where
  toQuery = Lude.const (Lude.mconcat ["tagging"])

-- | /See:/ 'mkGetBucketTaggingResponse' smart constructor.
data GetBucketTaggingResponse = GetBucketTaggingResponse'
  { -- | Contains the tag set.
    tagSet :: [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketTaggingResponse' with the minimum fields required to make a request.
--
-- * 'tagSet' - Contains the tag set.
-- * 'responseStatus' - The response status code.
mkGetBucketTaggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketTaggingResponse
mkGetBucketTaggingResponse pResponseStatus_ =
  GetBucketTaggingResponse'
    { tagSet = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | Contains the tag set.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrsTagSet :: Lens.Lens' GetBucketTaggingResponse [Tag]
gbtrsTagSet = Lens.lens (tagSet :: GetBucketTaggingResponse -> [Tag]) (\s a -> s {tagSet = a} :: GetBucketTaggingResponse)
{-# DEPRECATED gbtrsTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbtrsResponseStatus :: Lens.Lens' GetBucketTaggingResponse Lude.Int
gbtrsResponseStatus = Lens.lens (responseStatus :: GetBucketTaggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketTaggingResponse)
{-# DEPRECATED gbtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
