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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketLogging' smart constructor.
data PutBucketLogging = PutBucketLogging'
  { -- | The name of the bucket for which to set the logging parameters.
    bucket :: Types.BucketName,
    -- | Container for logging status information.
    bucketLoggingStatus :: Types.BucketLoggingStatus,
    -- | The MD5 hash of the @PutBucketLogging@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Types.ContentMD5,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.AccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketLogging' value with any optional fields omitted.
mkPutBucketLogging ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'bucketLoggingStatus'
  Types.BucketLoggingStatus ->
  PutBucketLogging
mkPutBucketLogging bucket bucketLoggingStatus =
  PutBucketLogging'
    { bucket,
      bucketLoggingStatus,
      contentMD5 = Core.Nothing,
      expectedBucketOwner = Core.Nothing
    }

-- | The name of the bucket for which to set the logging parameters.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblBucket :: Lens.Lens' PutBucketLogging Types.BucketName
pblBucket = Lens.field @"bucket"
{-# DEPRECATED pblBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for logging status information.
--
-- /Note:/ Consider using 'bucketLoggingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblBucketLoggingStatus :: Lens.Lens' PutBucketLogging Types.BucketLoggingStatus
pblBucketLoggingStatus = Lens.field @"bucketLoggingStatus"
{-# DEPRECATED pblBucketLoggingStatus "Use generic-lens or generic-optics with 'bucketLoggingStatus' instead." #-}

-- | The MD5 hash of the @PutBucketLogging@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblContentMD5 :: Lens.Lens' PutBucketLogging (Core.Maybe Types.ContentMD5)
pblContentMD5 = Lens.field @"contentMD5"
{-# DEPRECATED pblContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pblExpectedBucketOwner :: Lens.Lens' PutBucketLogging (Core.Maybe Types.AccountId)
pblExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED pblExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Core.AWSRequest PutBucketLogging where
  type Rs PutBucketLogging = PutBucketLoggingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
        Core._rqQuery = Core.pure ("logging", ""),
        Core._rqHeaders =
          Core.toHeaders "Content-MD5" contentMD5
            Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner),
        Core._rqBody = Core.toXMLBody x
      }
  response = Response.receiveNull PutBucketLoggingResponse'

-- | /See:/ 'mkPutBucketLoggingResponse' smart constructor.
data PutBucketLoggingResponse = PutBucketLoggingResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketLoggingResponse' value with any optional fields omitted.
mkPutBucketLoggingResponse ::
  PutBucketLoggingResponse
mkPutBucketLoggingResponse = PutBucketLoggingResponse'
