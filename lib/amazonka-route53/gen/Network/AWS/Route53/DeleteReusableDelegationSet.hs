{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reusable delegation set.
--
-- /Important:/ You can delete a reusable delegation set only if it isn't associated with any hosted zones.
-- To verify that the reusable delegation set is not associated with any hosted zones, submit a <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSet.html GetReusableDelegationSet> request and specify the ID of the reusable delegation set that you want to delete.
module Network.AWS.Route53.DeleteReusableDelegationSet
    (
    -- * Creating a request
      DeleteReusableDelegationSet (..)
    , mkDeleteReusableDelegationSet
    -- ** Request lenses
    , drdsId

    -- * Destructuring the response
    , DeleteReusableDelegationSetResponse (..)
    , mkDeleteReusableDelegationSetResponse
    -- ** Response lenses
    , drdsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to delete a reusable delegation set.
--
-- /See:/ 'mkDeleteReusableDelegationSet' smart constructor.
newtype DeleteReusableDelegationSet = DeleteReusableDelegationSet'
  { id :: Types.ResourceId
    -- ^ The ID of the reusable delegation set that you want to delete.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReusableDelegationSet' value with any optional fields omitted.
mkDeleteReusableDelegationSet
    :: Types.ResourceId -- ^ 'id'
    -> DeleteReusableDelegationSet
mkDeleteReusableDelegationSet id = DeleteReusableDelegationSet'{id}

-- | The ID of the reusable delegation set that you want to delete.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsId :: Lens.Lens' DeleteReusableDelegationSet Types.ResourceId
drdsId = Lens.field @"id"
{-# INLINEABLE drdsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.ToQuery DeleteReusableDelegationSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteReusableDelegationSet where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteReusableDelegationSet where
        type Rs DeleteReusableDelegationSet =
             DeleteReusableDelegationSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/2013-04-01/delegationset/" Core.<> Core.toText id,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteReusableDelegationSetResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An empty element.
--
-- /See:/ 'mkDeleteReusableDelegationSetResponse' smart constructor.
newtype DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteReusableDelegationSetResponse' value with any optional fields omitted.
mkDeleteReusableDelegationSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteReusableDelegationSetResponse
mkDeleteReusableDelegationSetResponse responseStatus
  = DeleteReusableDelegationSetResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drdsrrsResponseStatus :: Lens.Lens' DeleteReusableDelegationSetResponse Core.Int
drdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
