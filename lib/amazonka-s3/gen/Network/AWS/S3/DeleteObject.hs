{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a delete marker, which becomes the latest version of the object. If there isn't a null version, Amazon S3 does not remove any objects.
--
-- To remove a specific version, you must be the bucket owner and you must use the version Id subresource. Using this subresource permanently deletes the version. If the object deleted is a delete marker, Amazon S3 sets the response header, @x-amz-delete-marker@ , to true. 
-- If the object you want to delete is in a bucket where the bucket versioning configuration is MFA Delete enabled, you must include the @x-amz-mfa@ request header in the DELETE @versionId@ request. Requests that include @x-amz-mfa@ must use HTTPS. 
-- For more information about MFA Delete, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingMFADelete.html Using MFA Delete> . To see sample requests that use versioning, see <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTObjectDELETE.html#ExampleVersionObjectDelete Sample Request> . 
-- You can delete objects by explicitly calling the DELETE Object API or configure its lifecycle (<https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle> ) to enable Amazon S3 to remove them for you. If you want to block users or accounts from removing or deleting objects from your bucket, you must deny them the @s3:DeleteObject@ , @s3:DeleteObjectVersion@ , and @s3:PutLifeCycleConfiguration@ actions. 
-- The following operation is related to @DeleteObject@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject> 
--
--
module Network.AWS.S3.DeleteObject
    (
    -- * Creating a request
      DeleteObject (..)
    , mkDeleteObject
    -- ** Request lenses
    , dofBucket
    , dofKey
    , dofBypassGovernanceRetention
    , dofExpectedBucketOwner
    , dofMFA
    , dofRequestPayer
    , dofVersionId

    -- * Destructuring the response
    , DeleteObjectResponse (..)
    , mkDeleteObjectResponse
    -- ** Response lenses
    , dorrsDeleteMarker
    , dorrsRequestCharged
    , dorrsVersionId
    , dorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkDeleteObject' smart constructor.
data DeleteObject = DeleteObject'
  { bucket :: Types.BucketName
    -- ^ The bucket name of the bucket containing the object. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.ObjectKey
    -- ^ Key name of the object to delete.
  , bypassGovernanceRetention :: Core.Maybe Core.Bool
    -- ^ Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , mfa :: Core.Maybe Types.MFA
    -- ^ The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ VersionId used to reference a specific version of the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObject' value with any optional fields omitted.
mkDeleteObject
    :: Types.BucketName -- ^ 'bucket'
    -> Types.ObjectKey -- ^ 'key'
    -> DeleteObject
mkDeleteObject bucket key
  = DeleteObject'{bucket, key,
                  bypassGovernanceRetention = Core.Nothing,
                  expectedBucketOwner = Core.Nothing, mfa = Core.Nothing,
                  requestPayer = Core.Nothing, versionId = Core.Nothing}

-- | The bucket name of the bucket containing the object. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
-- When using this API with Amazon S3 on Outposts, you must direct requests to the S3 on Outposts hostname. The S3 on Outposts hostname takes the form /AccessPointName/ -/AccountId/ ./outpostID/ .s3-outposts./Region/ .amazonaws.com. When using this operation using S3 on Outposts through the AWS SDKs, you provide the Outposts bucket ARN in place of the bucket name. For more information about S3 on Outposts ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofBucket :: Lens.Lens' DeleteObject Types.BucketName
dofBucket = Lens.field @"bucket"
{-# INLINEABLE dofBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | Key name of the object to delete.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofKey :: Lens.Lens' DeleteObject Types.ObjectKey
dofKey = Lens.field @"key"
{-# INLINEABLE dofKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | Indicates whether S3 Object Lock should bypass Governance-mode restrictions to process this operation.
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofBypassGovernanceRetention :: Lens.Lens' DeleteObject (Core.Maybe Core.Bool)
dofBypassGovernanceRetention = Lens.field @"bypassGovernanceRetention"
{-# INLINEABLE dofBypassGovernanceRetention #-}
{-# DEPRECATED bypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofExpectedBucketOwner :: Lens.Lens' DeleteObject (Core.Maybe Types.AccountId)
dofExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE dofExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device. Required to permanently delete a versioned object if versioning is configured with MFA delete enabled.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofMFA :: Lens.Lens' DeleteObject (Core.Maybe Types.MFA)
dofMFA = Lens.field @"mfa"
{-# INLINEABLE dofMFA #-}
{-# DEPRECATED mfa "Use generic-lens or generic-optics with 'mfa' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofRequestPayer :: Lens.Lens' DeleteObject (Core.Maybe Types.RequestPayer)
dofRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE dofRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dofVersionId :: Lens.Lens' DeleteObject (Core.Maybe Types.ObjectVersionId)
dofVersionId = Lens.field @"versionId"
{-# INLINEABLE dofVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery DeleteObject where
        toQuery DeleteObject{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId

instance Core.ToHeaders DeleteObject where
        toHeaders DeleteObject{..}
          = Core.toHeaders "x-amz-bypass-governance-retention"
              bypassGovernanceRetention
              Core.<>
              Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-mfa" mfa
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest DeleteObject where
        type Rs DeleteObject = DeleteObjectResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteObjectResponse' Core.<$>
                   (Core.parseHeaderMaybe "x-amz-delete-marker" h) Core.<*>
                     Core.parseHeaderMaybe "x-amz-request-charged" h
                     Core.<*> Core.parseHeaderMaybe "x-amz-version-id" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
  { deleteMarker :: Core.Maybe Core.Bool
    -- ^ Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
  , requestCharged :: Core.Maybe Types.RequestCharged
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ Returns the version ID of the delete marker created as a result of the DELETE operation.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteObjectResponse' value with any optional fields omitted.
mkDeleteObjectResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteObjectResponse
mkDeleteObjectResponse responseStatus
  = DeleteObjectResponse'{deleteMarker = Core.Nothing,
                          requestCharged = Core.Nothing, versionId = Core.Nothing,
                          responseStatus}

-- | Specifies whether the versioned object that was permanently deleted was (true) or was not (false) a delete marker.
--
-- /Note:/ Consider using 'deleteMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsDeleteMarker :: Lens.Lens' DeleteObjectResponse (Core.Maybe Core.Bool)
dorrsDeleteMarker = Lens.field @"deleteMarker"
{-# INLINEABLE dorrsDeleteMarker #-}
{-# DEPRECATED deleteMarker "Use generic-lens or generic-optics with 'deleteMarker' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsRequestCharged :: Lens.Lens' DeleteObjectResponse (Core.Maybe Types.RequestCharged)
dorrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE dorrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | Returns the version ID of the delete marker created as a result of the DELETE operation.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsVersionId :: Lens.Lens' DeleteObjectResponse (Core.Maybe Types.ObjectVersionId)
dorrsVersionId = Lens.field @"versionId"
{-# INLINEABLE dorrsVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dorrsResponseStatus :: Lens.Lens' DeleteObjectResponse Core.Int
dorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
