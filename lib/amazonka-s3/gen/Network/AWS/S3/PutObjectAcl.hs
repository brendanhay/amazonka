{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutObjectAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uses the @acl@ subresource to set the access control list (ACL) permissions for a new or existing object in an S3 bucket. You must have @WRITE_ACP@ permission to set the ACL of an object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#permissions What permissions can I grant?> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- This action is not supported by Amazon S3 on Outposts.
-- Depending on your application needs, you can choose to set the ACL on an object using either the request body or the headers. For example, if you have an existing application that updates a bucket ACL using the request body, you can continue to use that approach. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> in the /Amazon S3 Developer Guide/ .
-- __Access Permissions__ 
-- You can set access permissions using one of the following methods:
--
--     * Specify a canned ACL with the @x-amz-acl@ request header. Amazon S3 supports a set of predefined ACLs, known as canned ACLs. Each canned ACL has a predefined set of grantees and permissions. Specify the canned ACL name as the value of @x-amz-ac@ l. If you use this header, you cannot use other access control-specific headers in your request. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
--
--     * Specify access permissions explicitly with the @x-amz-grant-read@ , @x-amz-grant-read-acp@ , @x-amz-grant-write-acp@ , and @x-amz-grant-full-control@ headers. When using these headers, you specify explicit access permissions and grantees (AWS accounts or Amazon S3 groups) who will receive the permission. If you use these ACL-specific headers, you cannot use @x-amz-acl@ header to set a canned ACL. These parameters map to the set of permissions that Amazon S3 supports in an ACL. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html Access Control List (ACL) Overview> .
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
-- For example, the following @x-amz-grant-read@ header grants list objects permission to the two AWS accounts identified by their email addresses.
-- @x-amz-grant-read: emailAddress="xyz@amazon.com", emailAddress="abc@amazon.com" @ 
--
--
-- You can use either a canned ACL or specify access permissions explicitly. You cannot do both.
-- __Grantee Values__ 
-- You can specify the person (grantee) to whom you're assigning access rights (using request elements) in the following ways:
--
--     * By the person's ID:
-- @<Grantee xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:type="CanonicalUser"><ID><>ID<></ID><DisplayName><>GranteesEmail<></DisplayName> </Grantee>@ 
-- DisplayName is optional and ignored in the request.
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
-- __Versioning__ 
-- The ACL of an object is set at the object version level. By default, PUT sets the ACL of the current version of an object. To set the ACL of a different version, use the @versionId@ subresource.
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CopyObject.html CopyObject> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> 
--
--
module Network.AWS.S3.PutObjectAcl
    (
    -- * Creating a request
      PutObjectAcl (..)
    , mkPutObjectAcl
    -- ** Request lenses
    , poaBucket
    , poaKey
    , poaACL
    , poaAccessControlPolicy
    , poaContentMD5
    , poaExpectedBucketOwner
    , poaGrantFullControl
    , poaGrantRead
    , poaGrantReadACP
    , poaGrantWrite
    , poaGrantWriteACP
    , poaRequestPayer
    , poaVersionId

    -- * Destructuring the response
    , PutObjectAclResponse (..)
    , mkPutObjectAclResponse
    -- ** Response lenses
    , poarrsRequestCharged
    , poarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkPutObjectAcl' smart constructor.
data PutObjectAcl = PutObjectAcl'
  { bucket :: Types.BucketName
    -- ^ The bucket name that contains the object to which you want to attach the ACL. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ Key for which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , acl :: Core.Maybe Types.ObjectCannedACL
    -- ^ The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
  , accessControlPolicy :: Core.Maybe Types.AccessControlPolicy
    -- ^ Contains the elements that set the ACL permissions for an object per grantee.
  , contentMD5 :: Core.Maybe Types.ContentMD5
    -- ^ The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , grantFullControl :: Core.Maybe Types.GrantFullControl
    -- ^ Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
  , grantRead :: Core.Maybe Types.GrantRead
    -- ^ Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
  , grantReadACP :: Core.Maybe Types.GrantReadACP
    -- ^ Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
  , grantWrite :: Core.Maybe Types.GrantWrite
    -- ^ Allows grantee to create, overwrite, and delete any object in the bucket.
  , grantWriteACP :: Core.Maybe Types.GrantWriteACP
    -- ^ Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ VersionId used to reference a specific version of the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectAcl' value with any optional fields omitted.
mkPutObjectAcl
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> PutObjectAcl
mkPutObjectAcl bucket key
  = PutObjectAcl'{bucket, key, acl = Core.Nothing,
                  accessControlPolicy = Core.Nothing, contentMD5 = Core.Nothing,
                  expectedBucketOwner = Core.Nothing,
                  grantFullControl = Core.Nothing, grantRead = Core.Nothing,
                  grantReadACP = Core.Nothing, grantWrite = Core.Nothing,
                  grantWriteACP = Core.Nothing, requestPayer = Core.Nothing,
                  versionId = Core.Nothing}

-- | The bucket name that contains the object to which you want to attach the ACL. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaBucket :: Lens.Lens' PutObjectAcl Types.BucketName
poaBucket = Lens.field @"bucket"
{-# INLINEABLE poaBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Key for which the PUT operation was initiated.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaKey :: Lens.Lens' PutObjectAcl Types.ObjectKey
poaKey = Lens.field @"key"
{-# INLINEABLE poaKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The canned ACL to apply to the object. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#CannedACL Canned ACL> .
--
-- /Note:/ Consider using 'acl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaACL :: Lens.Lens' PutObjectAcl (Core.Maybe Types.ObjectCannedACL)
poaACL = Lens.field @"acl"
{-# INLINEABLE poaACL #-}
{-# DEPRECATED acl "Use generic-lens or generic-optics with 'acl' instead"  #-}

-- | Contains the elements that set the ACL permissions for an object per grantee.
--
-- /Note:/ Consider using 'accessControlPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaAccessControlPolicy :: Lens.Lens' PutObjectAcl (Core.Maybe Types.AccessControlPolicy)
poaAccessControlPolicy = Lens.field @"accessControlPolicy"
{-# INLINEABLE poaAccessControlPolicy #-}
{-# DEPRECATED accessControlPolicy "Use generic-lens or generic-optics with 'accessControlPolicy' instead"  #-}

-- | The base64-encoded 128-bit MD5 digest of the data. This header must be used as a message integrity check to verify that the request body was not corrupted in transit. For more information, go to <http://www.ietf.org/rfc/rfc1864.txt RFC 1864.>> 
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaContentMD5 :: Lens.Lens' PutObjectAcl (Core.Maybe Types.ContentMD5)
poaContentMD5 = Lens.field @"contentMD5"
{-# INLINEABLE poaContentMD5 #-}
{-# DEPRECATED contentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaExpectedBucketOwner :: Lens.Lens' PutObjectAcl (Core.Maybe Types.AccountId)
poaExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE poaExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Allows grantee the read, write, read ACP, and write ACP permissions on the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantFullControl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantFullControl :: Lens.Lens' PutObjectAcl (Core.Maybe Types.GrantFullControl)
poaGrantFullControl = Lens.field @"grantFullControl"
{-# INLINEABLE poaGrantFullControl #-}
{-# DEPRECATED grantFullControl "Use generic-lens or generic-optics with 'grantFullControl' instead"  #-}

-- | Allows grantee to list the objects in the bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantRead' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantRead :: Lens.Lens' PutObjectAcl (Core.Maybe Types.GrantRead)
poaGrantRead = Lens.field @"grantRead"
{-# INLINEABLE poaGrantRead #-}
{-# DEPRECATED grantRead "Use generic-lens or generic-optics with 'grantRead' instead"  #-}

-- | Allows grantee to read the bucket ACL.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantReadACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantReadACP :: Lens.Lens' PutObjectAcl (Core.Maybe Types.GrantReadACP)
poaGrantReadACP = Lens.field @"grantReadACP"
{-# INLINEABLE poaGrantReadACP #-}
{-# DEPRECATED grantReadACP "Use generic-lens or generic-optics with 'grantReadACP' instead"  #-}

-- | Allows grantee to create, overwrite, and delete any object in the bucket.
--
-- /Note:/ Consider using 'grantWrite' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantWrite :: Lens.Lens' PutObjectAcl (Core.Maybe Types.GrantWrite)
poaGrantWrite = Lens.field @"grantWrite"
{-# INLINEABLE poaGrantWrite #-}
{-# DEPRECATED grantWrite "Use generic-lens or generic-optics with 'grantWrite' instead"  #-}

-- | Allows grantee to write the ACL for the applicable bucket.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- /Note:/ Consider using 'grantWriteACP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaGrantWriteACP :: Lens.Lens' PutObjectAcl (Core.Maybe Types.GrantWriteACP)
poaGrantWriteACP = Lens.field @"grantWriteACP"
{-# INLINEABLE poaGrantWriteACP #-}
{-# DEPRECATED grantWriteACP "Use generic-lens or generic-optics with 'grantWriteACP' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaRequestPayer :: Lens.Lens' PutObjectAcl (Core.Maybe Types.RequestPayer)
poaRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE poaRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poaVersionId :: Lens.Lens' PutObjectAcl (Core.Maybe Types.ObjectVersionId)
poaVersionId = Lens.field @"versionId"
{-# INLINEABLE poaVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery PutObjectAcl where
        toQuery PutObjectAcl{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "acl" ("" :: Core.Text)

instance Core.ToHeaders PutObjectAcl where
        toHeaders PutObjectAcl{..}
          = Core.toHeaders "x-amz-acl" acl Core.<>
              Core.toHeaders "Content-MD5" contentMD5
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-grant-full-control" grantFullControl
              Core.<> Core.toHeaders "x-amz-grant-read" grantRead
              Core.<> Core.toHeaders "x-amz-grant-read-acp" grantReadACP
              Core.<> Core.toHeaders "x-amz-grant-write" grantWrite
              Core.<> Core.toHeaders "x-amz-grant-write-acp" grantWriteACP
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest PutObjectAcl where
        type Rs PutObjectAcl = PutObjectAclResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutObjectAclResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-request-charged" h) Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutObjectAclResponse' smart constructor.
data PutObjectAclResponse = PutObjectAclResponse'
  { requestCharged :: Core.Maybe Types.RequestCharged
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutObjectAclResponse' value with any optional fields omitted.
mkPutObjectAclResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutObjectAclResponse
mkPutObjectAclResponse responseStatus
  = PutObjectAclResponse'{requestCharged = Core.Nothing,
                          responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poarrsRequestCharged :: Lens.Lens' PutObjectAclResponse (Core.Maybe Types.RequestCharged)
poarrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE poarrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
poarrsResponseStatus :: Lens.Lens' PutObjectAclResponse Core.Int
poarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE poarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
