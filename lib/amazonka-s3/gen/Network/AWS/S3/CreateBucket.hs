{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.CreateBucket
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new S3 bucket. To create a bucket, you must register with Amazon S3 and have a valid AWS Access Key ID to authenticate requests. Anonymous requests are never allowed to create buckets. By creating the bucket, you become the bucket owner.
--
-- Not every string is an acceptable bucket name. For information about bucket naming restrictions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html Working with Amazon S3 buckets> . 
-- If you want to create an Amazon S3 on Outposts bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/API/API_control_CreateBucket.html Create Bucket> . 
-- By default, the bucket is created in the US East (N. Virginia) Region. You can optionally specify a Region in the request body. You might choose a Region to optimize latency, minimize costs, or address regulatory requirements. For example, if you reside in Europe, you will probably find it advantageous to create buckets in the Europe (Ireland) Region. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingBucket.html#access-bucket-intro Accessing a bucket> .
-- When creating a bucket using this operation, you can optionally specify the accounts or groups that should be granted specific permissions on the bucket. There are two ways to grant the appropriate permissions using the request headers.
--
--     * Specify a canned ACL using the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly using the @x-amz-grant-read@ , @x-amz-grant-write@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. These headers map to the set of permissions Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access control list (ACL) overview> .
-- You specify each grantee as a type=value pair, where the type is one of the following:
--
--     * @id@ – if the value specified is the canonical user ID of an AWS account
--
--
--     * @uri@ – if you are granting permissions to a predefined group
--
--
--     * @emailAddress@ – if the value specified is the email address of an AWS account
--
--
-- For example, the following @x-amz-grant-read@ header grants the AWS accounts identified by account IDs permissions to read object data and its metadata:
-- @x-amz-grant-read: id="11112222333", id="444455556666" @ 
--
--
-- The following operations are related to @CreateBucket@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket> 
--
--
module Network.AWS.S3.CreateBucket
    (
    -- * Creating a request
      CreateBucket (..)
    , mkCreateBucket
    -- ** Request lenses
    , cbBucket
    , cbACL
    , cbCreateBucketConfiguration
    , cbGrantFullControl
    , cbGrantRead
    , cbGrantReadACP
    , cbGrantWrite
    , cbGrantWriteACP
    , cbObjectLockEnabledForBucket

    -- * Destructuring the response
    , CreateBucketResponse (..)
    , mkCreateBucketResponse
    -- ** Response lenses
    , cbrrsLocation
    , cbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkCreateBucket' smart constructor.
data CreateBucket = CreateBucket'
  { bucket :: Types.BucketName
    -- ^ The name of the bucket to create.
  , acl :: Core.Maybe Types.BucketCannedACL
    -- ^ The canned ACL to apply to the bucket.
  , createBucketConfiguration :: Core.Maybe Types.CreateBucketConfiguration
    -- ^ The configuration information for the bucket.
  , grantFullControl :: Core.Maybe Types.GrantFullControl
    -- ^ Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
  , grantRead :: Core.Maybe Types.GrantRead
    -- ^ Allows grantee to list the objects in the bucket.
  , grantReadACP :: Core.Maybe Types.GrantReadACP
    -- ^ Allows grantee to read the bucket ACL.
  , grantWrite :: Core.Maybe Types.GrantWrite
    -- ^ Allows grantee to create, overwrite, and delete any object in the bucket.
  , grantWriteACP :: Core.Maybe Types.GrantWriteACP
    -- ^ Allows grantee to write the ACL for the applicable bucket.
  , objectLockEnabledForBucket :: Core.Maybe Core.Bool
    -- ^ Specifies whether you want S3 Object Lock to be enabled for the new bucket.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBucket' value with any optional fields omitted.
mkCreateBucket
    :: Types.BucketName -- ^ 'bucket'
    -> CreateBucket
mkCreateBucket bucket
  = CreateBucket'{bucket, acl = Core.Nothing,
                  createBucketConfiguration = Core.Nothing,
                  grantFullControl = Core.Nothing, grantRead = Core.Nothing,
                  grantReadACP = Core.Nothing, grantWrite = Core.Nothing,
                  grantWriteACP = Core.Nothing,
                  objectLockEnabledForBucket = Core.Nothing}

-- | The name of the bucket to create.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbBucket :: Lens.Lens' CreateBucket Types.BucketName
cbBucket = Lens.field @"bucket"
{-# INLINEABLE cbBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The canned ACL to apply to the bucket.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbACL :: Lens.Lens' CreateBucket (Core.Maybe Types.BucketCannedACL)
cbACL = Lens.field @"acl"
{-# INLINEABLE cbACL #-}
{-# DEPRECATED acl "Use generic-lens or generic-optics with 'acl' instead"  #-}

-- | The configuration information for the bucket.
--
-- /Note:/ Consider using 'createBucketConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbCreateBucketConfiguration :: Lens.Lens' CreateBucket (Core.Maybe Types.CreateBucketConfiguration)
cbCreateBucketConfiguration = Lens.field @"createBucketConfiguration"
{-# INLINEABLE cbCreateBucketConfiguration #-}
{-# DEPRECATED createBucketConfiguration "Use generic-lens or generic-optics with 'createBucketConfiguration' instead"  #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantFullControl :: Lens.Lens' CreateBucket (Core.Maybe Types.GrantFullControl)
cbGrantFullControl = Lens.field @"grantFullControl"
{-# INLINEABLE cbGrantFullControl #-}
{-# DEPRECATED grantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead"  #-}

-- | Allows grantee to list the objects in the bucket.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantRead :: Lens.Lens' CreateBucket (Core.Maybe Types.GrantRead)
cbGrantRead = Lens.field @"grantRead"
{-# INLINEABLE cbGrantRead #-}
{-# DEPRECATED grantRead "Use generic-lens or generic-optics with 'grantRead' instead"  #-}

-- | Allows grantee to read the bucket ACL.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantReadACP :: Lens.Lens' CreateBucket (Core.Maybe Types.GrantReadACP)
cbGrantReadACP = Lens.field @"grantReadACP"
{-# INLINEABLE cbGrantReadACP #-}
{-# DEPRECATED grantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead"  #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantWrite :: Lens.Lens' CreateBucket (Core.Maybe Types.GrantWrite)
cbGrantWrite = Lens.field @"grantWrite"
{-# INLINEABLE cbGrantWrite #-}
{-# DEPRECATED grantWrite "Use generic-lens or generic-optics with 'grantWrite' instead"  #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbGrantWriteACP :: Lens.Lens' CreateBucket (Core.Maybe Types.GrantWriteACP)
cbGrantWriteACP = Lens.field @"grantWriteACP"
{-# INLINEABLE cbGrantWriteACP #-}
{-# DEPRECATED grantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead"  #-}

-- | Specifies whether you want S3 Object Lock to be enabled for the new bucket.
--
-- /Note:/ Consider using 'objectLockEnabledForBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbObjectLockEnabledForBucket :: Lens.Lens' CreateBucket (Core.Maybe Core.Bool)
cbObjectLockEnabledForBucket = Lens.field @"objectLockEnabledForBucket"
{-# INLINEABLE cbObjectLockEnabledForBucket #-}
{-# DEPRECATED objectLockEnabledForBucket "Use generic-lens or generic-optics with 'objectLockEnabledForBucket' instead"  #-}

instance Core.ToQuery CreateBucket where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateBucket where
        toHeaders CreateBucket{..}
          = Core.toHeaders "x-amz-acl" acl Core.<>
              Core.toHeaders "x-amz-grant-full-control" grantFullControl
              Core.<> Core.toHeaders "x-amz-grant-read" grantRead
              Core.<> Core.toHeaders "x-amz-grant-read-acp" grantReadACP
              Core.<> Core.toHeaders "x-amz-grant-write" grantWrite
              Core.<> Core.toHeaders "x-amz-grant-write-acp" grantWriteACP
              Core.<>
              Core.toHeaders "x-amz-bucket-object-lock-enabled"
                objectLockEnabledForBucket

instance Core.AWSRequest CreateBucket where
        type Rs CreateBucket = CreateBucketResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateBucketResponse' Core.<$>
                   (Core.parseHeaderMaybe "Location" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { location :: Core.Maybe Types.Location
    -- ^ Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateBucketResponse' value with any optional fields omitted.
mkCreateBucketResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateBucketResponse
mkCreateBucketResponse responseStatus
  = CreateBucketResponse'{location = Core.Nothing, responseStatus}

-- | Specifies the Region where the bucket will be created. If you are creating a bucket on the US East (N. Virginia) Region (us-east-1), you do not need to specify the location.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsLocation :: Lens.Lens' CreateBucketResponse (Core.Maybe Types.Location)
cbrrsLocation = Lens.field @"location"
{-# INLINEABLE cbrrsLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cbrrsResponseStatus :: Lens.Lens' CreateBucketResponse Core.Int
cbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
