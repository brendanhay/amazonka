{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permissions on an existing bucket using access control lists (ACL). For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3_ACLs_UsingACLs.html Using ACLs> . To set the ACL of a bucket, you must have @WRITE_ACP@ permission.
--
-- You can use one of the following two ways to set a bucket's permissions:
--
--     * Specify the ACL in the request body
--
--
--     * Specify permissions using request headers
--
--
-- Depending on your application needs, you may choose to set the ACL on a bucket using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, then you can continue to use that approach.
-- __Access Permissions__ 
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as /canned ACLs/ . Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-acl@ . If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use the @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
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
-- For example, the following @x-amz-grant-write@ header grants create, overwrite, and delete objects permission to LogDelivery group predefined by Amazon S3 and two AWS accounts identified by their email addresses.
-- @x-amz-grant-write: uri="http://acs.amazonaws.com/groups/s3/LogDelivery", id="111122223333", id="555566667777" @ 
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
-- __Grantee Values__ 
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@ 
-- DisplayName is optional and ignored in the request
--
--
--     * By URI:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="Group"><URI><>http://acs.amazonaws.com/groups/global/AuthenticatedUsers<></URI></Grantee>@ 
--
--
--     * By Email address:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="AmazonCustomerByEmail"><EmailAddress><>Grantees@email.com<></EmailAddress>lt;/Grantee>@ 
-- The grantee is resolved to the CanonicalUser and, in a response to a GET Object acl request, appears as the CanonicalUser. 
--
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAcl.html GetObjectAcl> 
--
--
module Network.AWS.S3.PutBucketAcl
    (
    -- * Creating a request
      PutBucketAcl (..)
    , mkPutBucketAcl
    -- ** Request lenses
    , pbaBucket
    , pbaACL
    , pbaAccessControlPolicy
    , pbaContentMD5
    , pbaExpectedBucketOwner
    , pbaGrantFullControl
    , pbaGrantRead
    , pbaGrantReadACP
    , pbaGrantWrite
    , pbaGrantWriteACP

    -- * Destructuring the response
    , PutBucketAclResponse (..)
    , mkPutBucketAclResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutBucketAcl' smart constructor.
data PutBucketAcl = PutBucketAcl'
  { bucket :: Types.BucketName
    -- ^ The bucket to which to apply the ACL.
  , acl :: Core.Maybe Types.BucketCannedACL
    -- ^ The canned ACL to apply to the bucket.
  , accessControlPolicy :: Core.Maybe Types.AccessControlPolicy
    -- ^ Contains the elements that set the ACL permissions for an object per grantee.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAcl' value with any optional fields omitted.
mkPutBucketAcl
    :: Types.BucketName -- ^ 'bucket'
    -> PutBucketAcl
mkPutBucketAcl bucket
  = PutBucketAcl'{bucket, acl = Core.Nothing,
                  accessControlPolicy = Core.Nothing, contentMD5 = Core.Nothing,
                  expectedBucketOwner = Core.Nothing,
                  grantFullControl = Core.Nothing, grantRead = Core.Nothing,
                  grantReadACP = Core.Nothing, grantWrite = Core.Nothing,
                  grantWriteACP = Core.Nothing}

-- | The bucket to which to apply the ACL.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaBucket :: Lens.Lens' PutBucketAcl Types.BucketName
pbaBucket = Lens.field @"bucket"
{-# INLINEABLE pbaBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The canned ACL to apply to the bucket.
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaACL :: Lens.Lens' PutBucketAcl (Core.Maybe Types.BucketCannedACL)
pbaACL = Lens.field @"acl"
{-# INLINEABLE pbaACL #-}
{-# DEPRECATED acl "Use generic-lens or generic-optics with 'acl' instead"  #-}

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /Note:/ Consider using 'accessControlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaAccessControlPolicy :: Lens.Lens' PutBucketAcl (Core.Maybe Types.AccessControlPolicy)
pbaAccessControlPolicy = Lens.field @"accessControlPolicy"
{-# INLINEABLE pbaAccessControlPolicy #-}
{-# DEPRECATED accessControlPolicy "Use generic-lens or generic-optics with 'accessControlPolicy' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaContentMD5 :: Lens.Lens' PutBucketAcl (Core.Maybe Types.ContentMD5)
pbaContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE pbaContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaExpectedBucketOwner :: Lens.Lens' PutBucketAcl (Core.Maybe Types.AccountId)
pbaExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE pbaExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantFullControl :: Lens.Lens' PutBucketAcl (Core.Maybe Types.GrantFullControl)
pbaGrantFullControl = Lens.field @"grantFullControl"
{-# INLINEABLE pbaGrantFullControl #-}
{-# DEPRECATED grantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead"  #-}

-- | Allows grantee to list the objects in the bucket.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantRead :: Lens.Lens' PutBucketAcl (Core.Maybe Types.GrantRead)
pbaGrantRead = Lens.field @"grantRead"
{-# INLINEABLE pbaGrantRead #-}
{-# DEPRECATED grantRead "Use generic-lens or generic-optics with 'grantRead' instead"  #-}

-- | Allows grantee to read the bucket ACL.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantReadACP :: Lens.Lens' PutBucketAcl (Core.Maybe Types.GrantReadACP)
pbaGrantReadACP = Lens.field @"grantReadACP"
{-# INLINEABLE pbaGrantReadACP #-}
{-# DEPRECATED grantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead"  #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantWrite :: Lens.Lens' PutBucketAcl (Core.Maybe Types.GrantWrite)
pbaGrantWrite = Lens.field @"grantWrite"
{-# INLINEABLE pbaGrantWrite #-}
{-# DEPRECATED grantWrite "Use generic-lens or generic-optics with 'grantWrite' instead"  #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbaGrantWriteACP :: Lens.Lens' PutBucketAcl (Core.Maybe Types.GrantWriteACP)
pbaGrantWriteACP = Lens.field @"grantWriteACP"
{-# INLINEABLE pbaGrantWriteACP #-}
{-# DEPRECATED grantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead"  #-}

instance Core.ToQuery PutBucketAcl where
        toQuery PutBucketAcl{..} = Core.toQueryPair "acl" ("" :: Core.Text)

instance Core.ToHeaders PutBucketAcl where
        toHeaders PutBucketAcl{..}
          = Core.toHeaders "x-amz-acl" acl Core.<>
              Core.toHeaders "Content-MD5" contentMD5
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-grant-full-control" grantFullControl
              Core.<> Core.toHeaders "x-amz-grant-read" grantRead
              Core.<> Core.toHeaders "x-amz-grant-read-acp" grantReadACP
              Core.<> Core.toHeaders "x-amz-grant-write" grantWrite
              Core.<> Core.toHeaders "x-amz-grant-write-acp" grantWriteACP

instance Core.AWSRequest PutBucketAcl where
        type Rs PutBucketAcl = PutBucketAclResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutBucketAclResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutBucketAclResponse' smart constructor.
data PutBucketAclResponse = PutBucketAclResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutBucketAclResponse' value with any optional fields omitted.
mkPutBucketAclResponse
    :: PutBucketAclResponse
mkPutBucketAclResponse = PutBucketAclResponse'
