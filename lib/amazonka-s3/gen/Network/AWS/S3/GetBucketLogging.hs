{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the logging status of a bucket and the permissions users have to view and modify that status. To use GET, you must be the bucket owner.
--
-- The following operations are related to @GetBucketLogging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLogging.html PutBucketLogging>
module Network.AWS.S3.GetBucketLogging
  ( -- * Creating a request
    GetBucketLogging (..),
    mkGetBucketLogging,

    -- ** Request lenses
    gblfBucket,
    gblfExpectedBucketOwner,

    -- * Destructuring the response
    GetBucketLoggingResponse (..),
    mkGetBucketLoggingResponse,

    -- ** Response lenses
    gblrsLoggingEnabled,
    gblrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkGetBucketLogging' smart constructor.
data GetBucketLogging = GetBucketLogging'
  { -- | The bucket name for which to get the logging information.
    bucket :: BucketName,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketLogging' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name for which to get the logging information.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkGetBucketLogging ::
  -- | 'bucket'
  BucketName ->
  GetBucketLogging
mkGetBucketLogging pBucket_ =
  GetBucketLogging'
    { bucket = pBucket_,
      expectedBucketOwner = Lude.Nothing
    }

-- | The bucket name for which to get the logging information.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblfBucket :: Lens.Lens' GetBucketLogging BucketName
gblfBucket = Lens.lens (bucket :: GetBucketLogging -> BucketName) (\s a -> s {bucket = a} :: GetBucketLogging)
{-# DEPRECATED gblfBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblfExpectedBucketOwner :: Lens.Lens' GetBucketLogging (Lude.Maybe Lude.Text)
gblfExpectedBucketOwner = Lens.lens (expectedBucketOwner :: GetBucketLogging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: GetBucketLogging)
{-# DEPRECATED gblfExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest GetBucketLogging where
  type Rs GetBucketLogging = GetBucketLoggingResponse
  request = Req.get s3Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetBucketLoggingResponse'
            Lude.<$> (x Lude..@? "LoggingEnabled")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetBucketLogging where
  toHeaders GetBucketLogging' {..} =
    Lude.mconcat
      ["x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner]

instance Lude.ToPath GetBucketLogging where
  toPath GetBucketLogging' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery GetBucketLogging where
  toQuery = Lude.const (Lude.mconcat ["logging"])

-- | /See:/ 'mkGetBucketLoggingResponse' smart constructor.
data GetBucketLoggingResponse = GetBucketLoggingResponse'
  { loggingEnabled :: Lude.Maybe LoggingEnabled,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetBucketLoggingResponse' with the minimum fields required to make a request.
--
-- * 'loggingEnabled' -
-- * 'responseStatus' - The response status code.
mkGetBucketLoggingResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetBucketLoggingResponse
mkGetBucketLoggingResponse pResponseStatus_ =
  GetBucketLoggingResponse'
    { loggingEnabled = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'loggingEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrsLoggingEnabled :: Lens.Lens' GetBucketLoggingResponse (Lude.Maybe LoggingEnabled)
gblrsLoggingEnabled = Lens.lens (loggingEnabled :: GetBucketLoggingResponse -> Lude.Maybe LoggingEnabled) (\s a -> s {loggingEnabled = a} :: GetBucketLoggingResponse)
{-# DEPRECATED gblrsLoggingEnabled "Use generic-lens or generic-optics with 'loggingEnabled' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gblrsResponseStatus :: Lens.Lens' GetBucketLoggingResponse Lude.Int
gblrsResponseStatus = Lens.lens (responseStatus :: GetBucketLoggingResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetBucketLoggingResponse)
{-# DEPRECATED gblrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
