{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.BulkPublish
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a bulk publish of all existing datasets for an Identity Pool to the configured stream. Customers are limited to one successful bulk publish per 24 hours. Bulk publish is an asynchronous request, customers can see the status of the request via the GetBulkPublishDetails operation.
--
-- This API can only be called with developer credentials. You cannot call this API with the temporary user credentials provided by Cognito Identity.
module Network.AWS.CognitoSync.BulkPublish
    (
    -- * Creating a request
      BulkPublish (..)
    , mkBulkPublish
    -- ** Request lenses
    , bpIdentityPoolId

    -- * Destructuring the response
    , BulkPublishResponse (..)
    , mkBulkPublishResponse
    -- ** Response lenses
    , bprrsIdentityPoolId
    , bprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the BulkPublish operation.
--
-- /See:/ 'mkBulkPublish' smart constructor.
newtype BulkPublish = BulkPublish'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BulkPublish' value with any optional fields omitted.
mkBulkPublish
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> BulkPublish
mkBulkPublish identityPoolId = BulkPublish'{identityPoolId}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpIdentityPoolId :: Lens.Lens' BulkPublish Types.IdentityPoolId
bpIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE bpIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

instance Core.ToQuery BulkPublish where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BulkPublish where
        toHeaders BulkPublish{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BulkPublish where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest BulkPublish where
        type Rs BulkPublish = BulkPublishResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/bulkpublish",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BulkPublishResponse' Core.<$>
                   (x Core..:? "IdentityPoolId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output for the BulkPublish operation.
--
-- /See:/ 'mkBulkPublishResponse' smart constructor.
data BulkPublishResponse = BulkPublishResponse'
  { identityPoolId :: Core.Maybe Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BulkPublishResponse' value with any optional fields omitted.
mkBulkPublishResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BulkPublishResponse
mkBulkPublishResponse responseStatus
  = BulkPublishResponse'{identityPoolId = Core.Nothing,
                         responseStatus}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bprrsIdentityPoolId :: Lens.Lens' BulkPublishResponse (Core.Maybe Types.IdentityPoolId)
bprrsIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE bprrsIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bprrsResponseStatus :: Lens.Lens' BulkPublishResponse Core.Int
bprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
