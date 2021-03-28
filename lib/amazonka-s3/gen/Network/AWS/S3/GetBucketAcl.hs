{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketAcl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the @GET@ operation uses the @acl@ subresource to return the access control list (ACL) of a bucket. To use @GET@ to return the ACL of the bucket, you must have @READ_ACP@ access to the bucket. If @READ_ACP@ permission is granted to the anonymous user, you can return the ACL of the bucket without using an authorization header.
--
-- __Related Resources__ 
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListObjects.html ListObjects> 
--
--
module Network.AWS.S3.GetBucketAcl
    (
    -- * Creating a request
      GetBucketAcl (..)
    , mkGetBucketAcl
    -- ** Request lenses
    , gbaBucket
    , gbaExpectedBucketOwner

    -- * Destructuring the response
    , GetBucketAclResponse (..)
    , mkGetBucketAclResponse
    -- ** Response lenses
    , gbarrsGrants
    , gbarrsOwner
    , gbarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.S3.Types as Types

-- | /See:/ 'mkGetBucketAcl' smart constructor.
data GetBucketAcl = GetBucketAcl'
  { bucket :: Types.BucketName
    -- ^ Specifies the S3 bucket whose ACL is being requested.
  , expectedBucketOwner :: Core.Maybe Types.AccountId
    -- ^ The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAcl' value with any optional fields omitted.
mkGetBucketAcl
    :: Types.BucketName -- ^ 'bucket'
    -> GetBucketAcl
mkGetBucketAcl bucket
  = GetBucketAcl'{bucket, expectedBucketOwner = Core.Nothing}

-- | Specifies the S3 bucket whose ACL is being requested.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaBucket :: Lens.Lens' GetBucketAcl Types.BucketName
gbaBucket = Lens.field @"bucket"
{-# INLINEABLE gbaBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbaExpectedBucketOwner :: Lens.Lens' GetBucketAcl (Core.Maybe Types.AccountId)
gbaExpectedBucketOwner = Lens.field @"expectedBucketOwner"
{-# INLINEABLE gbaExpectedBucketOwner #-}
{-# DEPRECATED expectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead"  #-}

instance Core.ToQuery GetBucketAcl where
        toQuery GetBucketAcl{..} = Core.toQueryPair "acl" ("" :: Core.Text)

instance Core.ToHeaders GetBucketAcl where
        toHeaders GetBucketAcl{..}
          = Core.toHeaders "x-amz-expected-bucket-owner" expectedBucketOwner

instance Core.AWSRequest GetBucketAcl where
        type Rs GetBucketAcl = GetBucketAclResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/" Core.<> Core.toText bucket,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 GetBucketAclResponse' Core.<$>
                   (x Core..@? "AccessControlList" Core..<@>
                      Core.parseXMLList "Grant")
                     Core.<*> x Core..@? "Owner"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetBucketAclResponse' smart constructor.
data GetBucketAclResponse = GetBucketAclResponse'
  { grants :: Core.Maybe [Types.Grant]
    -- ^ A list of grants.
  , owner :: Core.Maybe Types.Owner
    -- ^ Container for the bucket owner's display name and ID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBucketAclResponse' value with any optional fields omitted.
mkGetBucketAclResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBucketAclResponse
mkGetBucketAclResponse responseStatus
  = GetBucketAclResponse'{grants = Core.Nothing,
                          owner = Core.Nothing, responseStatus}

-- | A list of grants.
--
-- /Note:/ Consider using 'grants' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsGrants :: Lens.Lens' GetBucketAclResponse (Core.Maybe [Types.Grant])
gbarrsGrants = Lens.field @"grants"
{-# INLINEABLE gbarrsGrants #-}
{-# DEPRECATED grants "Use generic-lens or generic-optics with 'grants' instead"  #-}

-- | Container for the bucket owner's display name and ID.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsOwner :: Lens.Lens' GetBucketAclResponse (Core.Maybe Types.Owner)
gbarrsOwner = Lens.field @"owner"
{-# INLINEABLE gbarrsOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbarrsResponseStatus :: Lens.Lens' GetBucketAclResponse Core.Int
gbarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
