{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object. To use this operation, you must have @READ_ACP@ access to the object.
--
-- This action is not supported by Amazon S3 on Outposts.
-- __Versioning__ 
-- By default, GET returns ACL information about the current version of an object. To return ACL information about a different version, use the versionId subresource.
-- The following operations are related to @GetObjectAcl@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject> 
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject> 
--
--
module Network.AWS.S3.GetObjectAcl
    (
    -- * Creating a request
      GetObjectAcl (..)
    , mkGetObjectAcl
    -- ** Request lenses
    , goaBucket
    , goaKey
    , goaExpectedBucketOwner
    , goaRequestPayer
    , goaVersionId

    -- * Destructuring the response
    , GetObjectAclResponse (..)
    , mkGetObjectAclResponse
    -- ** Response lenses
    , goarrsGrants
    , goarrsOwner
    , goarrsRequestCharged
    , goarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetObjectAcl' smart constructor.
data GetObjectAcl = GetObjectAcl'
  { bucket :: Types.BucketName
    -- ^ The bucket name that contains the object for which to get the ACL information. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
  , key :: Types.Key
    -- ^ The key of the object for which to get the ACL information.
  , expectedBucketOwner :: Core.Maybe Types.ExpectedBucketOwner
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  , requestPayer :: Core.Maybe Types.RequestPayer
  , versionId :: Core.Maybe Types.ObjectVersionId
    -- ^ VersionId used to reference a specific version of the object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectAcl' value with any optional fields omitted.
mkGetObjectAcl
    :: Types.BucketName -- ^ 'bucket'
    -> Types.Key -- ^ 'key'
    -> GetObjectAcl
mkGetObjectAcl bucket key
  = GetObjectAcl'{bucket, key, expectedBucketOwner = Core.Nothing,
                  requestPayer = Core.Nothing, versionId = Core.Nothing}

-- | The bucket name that contains the object for which to get the ACL information. 
--
-- When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaBucket :: Lens.Lens' GetObjectAcl Types.BucketName
goaBucket = Lens.field @"bucket"
{-# INLINEABLE goaBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The key of the object for which to get the ACL information.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaKey :: Lens.Lens' GetObjectAcl Types.Key
goaKey = Lens.field @"key"
{-# INLINEABLE goaKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaExpectedBucketOwner :: Lens.Lens' GetObjectAcl (Core.Maybe Types.ExpectedBucketOwner)
goaExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE goaExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestPayer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaRequestPayer :: Lens.Lens' GetObjectAcl (Core.Maybe Types.RequestPayer)
goaRequestPayer = Lens.field @"requestPayer"
{-# INLINEABLE goaRequestPayer #-}
{-# DEPRECATED requestPayer "Use generic-lens or generic-optics with 'requestPayer' instead"  #-}

-- | VersionId used to reference a specific version of the object.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goaVersionId :: Lens.Lens' GetObjectAcl (Core.Maybe Types.ObjectVersionId)
goaVersionId = Lens.field @"versionId"
{-# INLINEABLE goaVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

instance Core.ToQuery GetObjectAcl where
        toQuery GetObjectAcl{..}
          = Core.maybe Core.mempty (Core.toQueryPair "versionId") versionId
              Core.<> Core.toQueryPair "acl" ("" :: Core.Text)

instance Core.ToHeaders GetObjectAcl where
        toHeaders GetObjectAcl{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner
              Core.<> Core.toHeaders "x-amz-request-payer" requestPayer

instance Core.AWSRequest GetObjectAcl where
        type Rs GetObjectAcl = GetObjectAclResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/" Core.<> Core.toText bucket Core.<> "/" Core.<> Core.toText key,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetObjectAclResponse' Core.<$>
                   (x Core..@? "AccessControlList" Core..<@>
                      Core.parseXMLList "Grant")
                     Core.<*> x Core..@? "Owner"
                     Core.<*> Core.parseHeaderMaybe "x-amz-request-charged" h
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetObjectAclResponse' smart constructor.
data GetObjectAclResponse = GetObjectAclResponse'
  { grants :: Core.Maybe [Types.Grant]
    -- ^ A list of grants.
  , owner :: Core.Maybe Types.Owner
    -- ^ Container for the bucket owner's display name and ID.
  , requestCharged :: Core.Maybe Types.RequestCharged
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetObjectAclResponse' value with any optional fields omitted.
mkGetObjectAclResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetObjectAclResponse
mkGetObjectAclResponse responseStatus
  = GetObjectAclResponse'{grants = Core.Nothing,
                          owner = Core.Nothing, requestCharged = Core.Nothing,
                          responseStatus}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsGrants :: Lens.Lens' GetObjectAclResponse (Core.Maybe [Types.Grant])
goarrsGrants = Lens.field @"grants"
{-# INLINEABLE goarrsGrants #-}
{-# DEPRECATED grants "Use generic-lens or generic-optics with 'grants' instead"  #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsOwner :: Lens.Lens' GetObjectAclResponse (Core.Maybe Types.Owner)
goarrsOwner = Lens.field @"owner"
{-# INLINEABLE goarrsOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'requestCharged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsRequestCharged :: Lens.Lens' GetObjectAclResponse (Core.Maybe Types.RequestCharged)
goarrsRequestCharged = Lens.field @"requestCharged"
{-# INLINEABLE goarrsRequestCharged #-}
{-# DEPRECATED requestCharged "Use generic-lens or generic-optics with 'requestCharged' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goarrsResponseStatus :: Lens.Lens' GetObjectAclResponse Core.Int
goarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
