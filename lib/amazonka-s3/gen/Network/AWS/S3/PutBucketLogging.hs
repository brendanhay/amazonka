{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketLogging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the logging parameters for a bucket and to specify permissions for who can view and modify the logging parameters. All logs are saved to buckets in the same AWS Region as the source bucket. To set the logging status of a bucket, you must be the bucket owner.
--
-- The bucket owner is automatically granted FULL_CONTROL to all logs. You use the @Grantee@ request element to grant access to other people. The @Permissions@ request element specifies the kind of access the grantee has to the logs.
-- __Grantee Values__
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@
-- DisplayName is optional and ignored in the request.
--
--
--     * By Email address:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress><>Grantees@email.com<></EmailAddress></Grantee>@
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser.
--
--
--     * By URI:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI><>http://acs.amazonaws.com/groups/global/AuthenticatedUsers<></URI></Grantee>@
--
--
-- To enable logging, you use LoggingEnabled and its children request elements. To disable logging, you use an empty BucketLoggingStatus request element:
-- @<BucketLoggingStatus xmlns="http://doc.s3.amazonaws.com/2006-03-01" />@
-- For more information about server access logging, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerLogs.html Server Access Logging> .
-- For more information about creating a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> . For more information about returning the logging status of a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging> .
-- The following operations are related to @PutBucketLogging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLogging.html GetBucketLogging>
module Network.AWS.S3.PutBucketLogging
  ( -- * Creating a request
    PutBucketLogging (..),
    mkPutBucketLogging,

    -- ** Request lenses
    pblBucket,
    pblBucketLoggingStatus,
    pblContentMD5,
    pblExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketLoggingResponse (..),
    mkPutBucketLoggingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
  { -- | The name of the bucket for which to set the logging parameters.
    bucket :: BucketName,
    -- | Container for logging status information.
    bucketLoggingStatus :: BucketLoggingStatus,
    -- | The MD5 hash of the @PutBucketLogging@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketLogging' with the minimum fields required to make a request.
--
-- * 'bucket' - The name of the bucket for which to set the logging parameters.
-- * 'bucketLoggingStatus' - Container for logging status information.
-- * 'contentMD5' - The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketLogging ::
  -- | 'bucket'
  BucketName ->
  -- | 'bucketLoggingStatus'
  BucketLoggingStatus ->
  PutBucketLogging
mkPutBucketLogging pBucket_ pBucketLoggingStatus_ =
  PutBucketLogging'
    { bucket = pBucket_,
      bucketLoggingStatus = pBucketLoggingStatus_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | The name of the bucket for which to set the logging parameters.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblBucket :: Lens.Lens' PutBucketLogging BucketName
pblBucket = Lens.lens (bucket :: PutBucketLogging -> BucketName) (\s a -> s {bucket = a} :: PutBucketLogging)
{-# DEPRECATED pblBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for logging status information.
--
-- /Note:/ Consider using 'bucketLoggingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblBucketLoggingStatus :: Lens.Lens' PutBucketLogging BucketLoggingStatus
pblBucketLoggingStatus = Lens.lens (bucketLoggingStatus :: PutBucketLogging -> BucketLoggingStatus) (\s a -> s {bucketLoggingStatus = a} :: PutBucketLogging)
{-# DEPRECATED pblBucketLoggingStatus "Use generic-lens or generic-optics with 'bucketLoggingStatus' instead." #-}

-- | The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblContentMD5 :: Lens.Lens' PutBucketLogging (Lude.Maybe Lude.Text)
pblContentMD5 = Lens.lens (contentMD5 :: PutBucketLogging -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketLogging)
{-# DEPRECATED pblContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblExpectedBucketOwner :: Lens.Lens' PutBucketLogging (Lude.Maybe Lude.Text)
pblExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketLogging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketLogging)
{-# DEPRECATED pblExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketLogging where
  type Rs PutBucketLogging = PutBucketLoggingResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketLoggingResponse'

instance Lude.ToElement PutBucketLogging where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}BucketLoggingStatus"
      Lude.. bucketLoggingStatus

instance Lude.ToHeaders PutBucketLogging where
  toHeaders PutBucketLogging' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketLogging where
  toPath PutBucketLogging' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketLogging where
  toQuery = Lude.const (Lude.mconcat ["logging"])

-- | /See:/ 'mkPutBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse = PutBucketLoggingResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketLoggingResponse' with the minimum fields required to make a request.
mkPutBucketLoggingResponse ::
  PutBucketLoggingResponse
mkPutBucketLoggingResponse = PutBucketLoggingResponse'
