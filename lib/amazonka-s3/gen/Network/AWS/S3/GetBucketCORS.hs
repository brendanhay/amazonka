{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketCORS
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the s3:GetBucketCORS action. By default, the bucket owner has this permission and can grant it to others.
-- For more information about cors, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> .
-- The following operations are related to @GetBucketCors@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
module Network.AWS.S3.GetBucketCORS
  ( -- * Creating a request
    GetBucketCORS (..),
    mkGetBucketCORS,

    -- ** Request lenses
    gbcExpectedBucketOwner,
    gbcBucket,

    -- * Destructuring the response
    GetBucketCORSResponse (..),
    mkGetBucketCORSResponse,

    -- ** Response lenses
    gbcrsCORSRules,
    gbcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketCORS' smart constructor.
data GetBucketCORS = GetBucketCORS'
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

-- | Creates a value of 'GetBucketCORS' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which to get the cors configuration.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketCORS ::
  -- | 'bucket'
  BucketName ->
  GetBucketCORS
mkGetBucketCORS pBucket_ =
  GetBucketCORS'
    { expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcExpectedBucketOwner :: Lens.Lens' GetBucketCORS (Lude.Maybe Lude.Text)
gbcExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketCORS -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketCORS)
{-# DEPRECATED gbcExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name for which to get the cors configuration.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcBucket :: Lens.Lens' GetBucketCORS BucketName
gbcBucket = Lens.lens (bucket :: GetBucketCORS -> BucketName) (\s a -> s {bucket = a} :: GetBucketCORS)
{-# DEPRECATED gbcBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.AWSRequest GetBucketCORS where
  type Rs GetBucketCORS = GetBucketCORSResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketCORSResponse'
            Lude.<$> (Lude.may (Lude.parseXMLList "CORSRule") x)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketCORS where
  toHeaders GetBucketCORS' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketCORS where
  toPath GetBucketCORS' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketCORS where
  toQuery = Lude.const (Lude.mconcat ["cors"])

-- | /See:/ 'mkGetBucketCORSResponse' smart constructor.
data GetBucketCORSResponse = GetBucketCORSResponse'
  { corsRules ::
      Lude.Maybe [CORSRule],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketCORSResponse' with the minimum fields required to make a request.
--
-- * 'corsRules' - A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
-- * 'responseStatus' - The response status code.
mkGetBucketCORSResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketCORSResponse
mkGetBucketCORSResponse pResponseStatus_ =
  GetBucketCORSResponse'
    { corsRules = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
--
-- /Note:/ Consider using 'corsRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrsCORSRules :: Lens.Lens' GetBucketCORSResponse (Lude.Maybe [CORSRule])
gbcrsCORSRules = Lens.lens (corsRules :: GetBucketCORSResponse -> Lude.Maybe [CORSRule]) (\s a -> s {corsRules = a} :: GetBucketCORSResponse)
{-# DEPRECATED gbcrsCORSRules "Use generic-lens or generic-optics with 'corsRules' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbcrsResponseStatus :: Lens.Lens' GetBucketCORSResponse Lude.Int
gbcrsResponseStatus = Lens.lens (responseStatus :: GetBucketCORSResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketCORSResponse)
{-# DEPRECATED gbcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
