{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket using a single HTTP request. If you know the object keys that you want to delete, then this operation provides a suitable alternative to sending individual delete requests, reducing per-request overhead.
--
-- The request contains a list of up to 1000 keys that you want to delete. In the XML, you provide the object key names, and optionally, version IDs if you want to delete a specific version of the object from a versioning-enabled bucket. For each key, Amazon S3 performs a delete operation and returns the result of that delete, success, or failure, in the response. Note that if the object specified in the request is not found, Amazon S3 returns the result as deleted.
-- The operation supports two modes for the response: verbose and quiet. By default, the operation uses verbose mode in which the response includes the result of deletion of each key in your request. In quiet mode the response includes only keys where the delete operation encountered an error. For a successful deletion, the operation does not return any information about the delete in the response body.
-- When performing this operation on an MFA Delete enabled bucket, that attempts to delete any versioned objects, you must include an MFA token. If you do not provide one, the entire request will fail, even if there are non-versioned objects you are trying to delete. If you provide an invalid token, whether there are versioned keys in the request or not, the entire Multi-Object Delete request will fail. For information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html#MultiFactorAuthenticationDelete MFA Delete> .
-- Finally, the Content-MD5 header is required for all Multi-Object Delete requests. Amazon S3 uses the header value to ensure that your request body has not been altered in transit.
-- The following operations are related to @DeleteObjects@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateMultipartUpload.html CreateMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_UploadPart.html UploadPart>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CompleteMultipartUpload.html CompleteMultipartUpload>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListParts.html ListParts>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_AbortMultipartUpload.html AbortMultipartUpload>
module Network.AWS.S3.DeleteObjects
  ( -- * Creating a request
    DeleteObjects (..),
    mkDeleteObjects,

    -- ** Request lenses
    doBucket,
    doDelete,
    doBypassGovernanceRetention,
    doExpectedBucketOwner,
    doMFA,
    doRequestPayer,

    -- * Destructuring the response
    DeleteObjectsResponse (..),
    mkDeleteObjectsResponse,

    -- ** Response lenses
    drsDeleted,
    drsErrors,
    drsRequestCharged,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { -- | The bucket name containing the objects to delete.
    --
    -- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
    -- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
    bucket :: Types.BucketName,
    -- | Container for the request.
    delete :: Types.Delete,
    -- | Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
    bypassGovernanceRetention :: Core.Maybe Core.Bool,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner,
    -- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
    mfa :: Core.Maybe Types.MFA,
    requestPayer :: Core.Maybe Types.RequestPayer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjects' value with any optional fields omitted.
mkDeleteObjects ::
  -- | 'bucket'
  Types.BucketName ->
  -- | 'delete'
  Types.Delete ->
  DeleteObjects
mkDeleteObjects bucket delete =
  DeleteObjects'
    { bucket,
      delete,
      bypassGovernanceRetention = Core.Nothing,
      expectedBucketOwner = Core.Nothing,
      mfa = Core.Nothing,
      requestPayer = Core.Nothing
    }

-- | The bucket name containing the objects to delete.
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBucket :: Lens.Lens' DeleteObjects Types.BucketName
doBucket = Lens.field @"bucket"
{-# DEPRECATED doBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for the request.
--
-- /Note:/ Consider using 'delete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDelete :: Lens.Lens' DeleteObjects Types.Delete
doDelete = Lens.field @"delete"
{-# DEPRECATED doDelete "Use generic-lens or generic-optics with 'delete' instead." #-}

-- | Specifies whether you want to delete this object even if it has a Governance-type Object Lock in place. You must have sufficient permissions to perform this operation.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doBypassGovernanceRetention :: Lens.Lens' DeleteObjects (Core.Maybe Core.Bool)
doBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# DEPRECATED doBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doExpectedBucketOwner :: Lens.Lens' DeleteObjects (Core.Maybe Types.ExpectedBucketOwner)
doExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# DEPRECATED doExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doMFA :: Lens.Lens' DeleteObjects (Core.Maybe Types.MFA)
doMFA = Lens.field @"mfa"
{-# DEPRECATED doMFA "Use generic-lens or generic-optics with 'mfa' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doRequestPayer :: Lens.Lens' DeleteObjects (Core.Maybe Types.RequestPayer)
doRequestPayer = Lens.field @"requestPayer"
{-# DEPRECATED doRequestPayer "Use generic-lens or generic-optics with 'requestPayer' instead." #-}

instance Core.AWSRequest DeleteObjects where
  type Rs DeleteObjects = DeleteObjectsResponse
  request x@Core.Request {..} =
    Request.contentMD5Header Core.$
      Core.Request
        { Core._rqService = Types.mkServiceConfig,
          Core._rqMethod = Request.POST,
          Core._rqPath = Core.rawPath ("/" Core.<> (Core.toText bucket)),
          Core._rqQuery = Core.pure ("delete", ""),
          Core._rqHeaders =
            Core.toHeaders
              "x-amz-bypass-governance-retention"
              bypassGovernanceRetention
              Core.<> (Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner)
              Core.<> (Core.toHeaders "x-amz-mfa" mfa)
              Core.<> (Core.toHeaders "x-amz-request-payer" requestPayer),
          Core._rqBody = Core.toXMLBody x
        }
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteObjectsResponse'
            Core.<$> (x Core..@? "Deleted")
            Core.<*> (x Core..@? "Error")
            Core.<*> (Core.parseHeaderMaybe "x-amz-request-charged" h)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { -- | Container element for a successful delete. It identifies the object that was successfully deleted.
    deleted :: Core.Maybe [Types.DeletedObject],
    -- | Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
    errors :: Core.Maybe [Types.S3ServiceError],
    requestCharged :: Core.Maybe Types.RequestCharged,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjectsResponse' value with any optional fields omitted.
mkDeleteObjectsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteObjectsResponse
mkDeleteObjectsResponse responseStatus =
  DeleteObjectsResponse'
    { deleted = Core.Nothing,
      errors = Core.Nothing,
      requestCharged = Core.Nothing,
      responseStatus
    }

-- | Container element for a successful delete. It identifies the object that was successfully deleted.
--
-- /Note:/ Consider using 'deleted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDeleted :: Lens.Lens' DeleteObjectsResponse (Core.Maybe [Types.DeletedObject])
drsDeleted = Lens.field @"deleted"
{-# DEPRECATED drsDeleted "Use generic-lens or generic-optics with 'deleted' instead." #-}

-- | Container for a failed delete operation that describes the object that Amazon S3 attempted to delete and the error it encountered.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsErrors :: Lens.Lens' DeleteObjectsResponse (Core.Maybe [Types.S3ServiceError])
drsErrors = Lens.field @"errors"
{-# DEPRECATED drsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRequestCharged :: Lens.Lens' DeleteObjectsResponse (Core.Maybe Types.RequestCharged)
drsRequestCharged = Lens.field @"requestCharged"
{-# DEPRECATED drsRequestCharged "Use generic-lens or generic-optics with 'requestCharged' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteObjectsResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
