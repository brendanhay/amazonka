{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.DeleteWebACL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'WebACL' . You can't delete a @WebACL@ if it still contains any @Rules@ .
--
-- To delete a @WebACL@ , perform the following steps:
--
--     * Update the @WebACL@ to remove @Rules@ , if any. For more information, see 'UpdateWebACL' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteWebACL@ request.
--
--
--     * Submit a @DeleteWebACL@ request.
--
--
module Network.AWS.WAFRegional.DeleteWebACL
    (
    -- * Creating a request
      DeleteWebACL (..)
    , mkDeleteWebACL
    -- ** Request lenses
    , dwaclWebACLId
    , dwaclChangeToken

    -- * Destructuring the response
    , DeleteWebACLResponse (..)
    , mkDeleteWebACLResponse
    -- ** Response lenses
    , dwaclrrsChangeToken
    , dwaclrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | /See:/ 'mkDeleteWebACL' smart constructor.
data DeleteWebACL = DeleteWebACL'
  { webACLId :: Types.ResourceId
    -- ^ The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebACL' value with any optional fields omitted.
mkDeleteWebACL
    :: Types.ResourceId -- ^ 'webACLId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteWebACL
mkDeleteWebACL webACLId changeToken
  = DeleteWebACL'{webACLId, changeToken}

-- | The @WebACLId@ of the 'WebACL' that you want to delete. @WebACLId@ is returned by 'CreateWebACL' and by 'ListWebACLs' .
--
-- /Note:/ Consider using 'webACLId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaclWebACLId :: Lens.Lens' DeleteWebACL Types.ResourceId
dwaclWebACLId = Lens.field @"webACLId"
{-# INLINEABLE dwaclWebACLId #-}
{-# DEPRECATED webACLId "Use generic-lens or generic-optics with 'webACLId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaclChangeToken :: Lens.Lens' DeleteWebACL Types.ChangeToken
dwaclChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dwaclChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteWebACL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteWebACL where
        toHeaders DeleteWebACL{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_Regional_20161128.DeleteWebACL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteWebACL where
        toJSON DeleteWebACL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("WebACLId" Core..= webACLId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteWebACL where
        type Rs DeleteWebACL = DeleteWebACLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteWebACLResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteWebACLResponse' smart constructor.
data DeleteWebACLResponse = DeleteWebACLResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWebACLResponse' value with any optional fields omitted.
mkDeleteWebACLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteWebACLResponse
mkDeleteWebACLResponse responseStatus
  = DeleteWebACLResponse'{changeToken = Core.Nothing, responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteWebACL@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaclrrsChangeToken :: Lens.Lens' DeleteWebACLResponse (Core.Maybe Types.ChangeToken)
dwaclrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dwaclrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwaclrrsResponseStatus :: Lens.Lens' DeleteWebACLResponse Core.Int
dwaclrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwaclrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
