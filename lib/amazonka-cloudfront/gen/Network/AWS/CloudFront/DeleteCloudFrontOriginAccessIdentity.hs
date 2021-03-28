{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an origin access identity. 
module Network.AWS.CloudFront.DeleteCloudFrontOriginAccessIdentity
    (
    -- * Creating a request
      DeleteCloudFrontOriginAccessIdentity (..)
    , mkDeleteCloudFrontOriginAccessIdentity
    -- ** Request lenses
    , dcfoaiId
    , dcfoaiIfMatch

    -- * Destructuring the response
    , DeleteCloudFrontOriginAccessIdentityResponse (..)
    , mkDeleteCloudFrontOriginAccessIdentityResponse
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Deletes a origin access identity.
--
-- /See:/ 'mkDeleteCloudFrontOriginAccessIdentity' smart constructor.
data DeleteCloudFrontOriginAccessIdentity = DeleteCloudFrontOriginAccessIdentity'
  { id :: Core.Text
    -- ^ The origin access identity's ID.
  , ifMatch :: Core.Maybe Core.Text
    -- ^ The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCloudFrontOriginAccessIdentity' value with any optional fields omitted.
mkDeleteCloudFrontOriginAccessIdentity
    :: Core.Text -- ^ 'id'
    -> DeleteCloudFrontOriginAccessIdentity
mkDeleteCloudFrontOriginAccessIdentity id
  = DeleteCloudFrontOriginAccessIdentity'{id, ifMatch = Core.Nothing}

-- | The origin access identity's ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfoaiId :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity Core.Text
dcfoaiId = Lens.field @"id"
{-# INLINEABLE dcfoaiId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The value of the @ETag@ header you received from a previous @GET@ or @PUT@ request. For example: @E2QWRUHAPOMQZL@ .
--
-- /Note:/ Consider using 'ifMatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfoaiIfMatch :: Lens.Lens' DeleteCloudFrontOriginAccessIdentity (Core.Maybe Core.Text)
dcfoaiIfMatch = Lens.field @"ifMatch"
{-# INLINEABLE dcfoaiIfMatch #-}
{-# DEPRECATED ifMatch "Use generic-lens or generic-optics with 'ifMatch' instead"  #-}

instance Core.ToQuery DeleteCloudFrontOriginAccessIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCloudFrontOriginAccessIdentity where
        toHeaders DeleteCloudFrontOriginAccessIdentity{..}
          = Core.toHeaders "If-Match" ifMatch

instance Core.AWSRequest DeleteCloudFrontOriginAccessIdentity where
        type Rs DeleteCloudFrontOriginAccessIdentity =
             DeleteCloudFrontOriginAccessIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/2020-05-31/origin-access-identity/cloudfront/" Core.<>
                             Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull
              DeleteCloudFrontOriginAccessIdentityResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCloudFrontOriginAccessIdentityResponse' smart constructor.
data DeleteCloudFrontOriginAccessIdentityResponse = DeleteCloudFrontOriginAccessIdentityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCloudFrontOriginAccessIdentityResponse' value with any optional fields omitted.
mkDeleteCloudFrontOriginAccessIdentityResponse
    :: DeleteCloudFrontOriginAccessIdentityResponse
mkDeleteCloudFrontOriginAccessIdentityResponse
  = DeleteCloudFrontOriginAccessIdentityResponse'
