{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.DeleteSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes a 'SqlInjectionMatchSet' . You can't delete a @SqlInjectionMatchSet@ if it's still used in any @Rules@ or if it still contains any 'SqlInjectionMatchTuple' objects.
--
-- If you just want to remove a @SqlInjectionMatchSet@ from a @Rule@ , use 'UpdateRule' .
-- To permanently delete a @SqlInjectionMatchSet@ from AWS WAF, perform the following steps:
--
--     * Update the @SqlInjectionMatchSet@ to remove filters, if any. For more information, see 'UpdateSqlInjectionMatchSet' .
--
--
--     * Use 'GetChangeToken' to get the change token that you provide in the @ChangeToken@ parameter of a @DeleteSqlInjectionMatchSet@ request.
--
--
--     * Submit a @DeleteSqlInjectionMatchSet@ request.
--
--
module Network.AWS.WAF.DeleteSqlInjectionMatchSet
    (
    -- * Creating a request
      DeleteSqlInjectionMatchSet (..)
    , mkDeleteSqlInjectionMatchSet
    -- ** Request lenses
    , dsimsSqlInjectionMatchSetId
    , dsimsChangeToken

    -- * Destructuring the response
    , DeleteSqlInjectionMatchSetResponse (..)
    , mkDeleteSqlInjectionMatchSetResponse
    -- ** Response lenses
    , dsimsrrsChangeToken
    , dsimsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | A request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSet' smart constructor.
data DeleteSqlInjectionMatchSet = DeleteSqlInjectionMatchSet'
  { sqlInjectionMatchSetId :: Types.ResourceId
    -- ^ The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
  , changeToken :: Types.ChangeToken
    -- ^ The value returned by the most recent call to 'GetChangeToken' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSqlInjectionMatchSet' value with any optional fields omitted.
mkDeleteSqlInjectionMatchSet
    :: Types.ResourceId -- ^ 'sqlInjectionMatchSetId'
    -> Types.ChangeToken -- ^ 'changeToken'
    -> DeleteSqlInjectionMatchSet
mkDeleteSqlInjectionMatchSet sqlInjectionMatchSetId changeToken
  = DeleteSqlInjectionMatchSet'{sqlInjectionMatchSetId, changeToken}

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to delete. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsSqlInjectionMatchSetId :: Lens.Lens' DeleteSqlInjectionMatchSet Types.ResourceId
dsimsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# INLINEABLE dsimsSqlInjectionMatchSetId #-}
{-# DEPRECATED sqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead"  #-}

-- | The value returned by the most recent call to 'GetChangeToken' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSet Types.ChangeToken
dsimsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dsimsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

instance Core.ToQuery DeleteSqlInjectionMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteSqlInjectionMatchSet where
        toHeaders DeleteSqlInjectionMatchSet{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.DeleteSqlInjectionMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteSqlInjectionMatchSet where
        toJSON DeleteSqlInjectionMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("SqlInjectionMatchSetId" Core..= sqlInjectionMatchSetId),
                  Core.Just ("ChangeToken" Core..= changeToken)])

instance Core.AWSRequest DeleteSqlInjectionMatchSet where
        type Rs DeleteSqlInjectionMatchSet =
             DeleteSqlInjectionMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteSqlInjectionMatchSetResponse' Core.<$>
                   (x Core..:? "ChangeToken") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a request to delete a 'SqlInjectionMatchSet' from AWS WAF.
--
-- /See:/ 'mkDeleteSqlInjectionMatchSetResponse' smart constructor.
data DeleteSqlInjectionMatchSetResponse = DeleteSqlInjectionMatchSetResponse'
  { changeToken :: Core.Maybe Types.ChangeToken
    -- ^ The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkDeleteSqlInjectionMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteSqlInjectionMatchSetResponse
mkDeleteSqlInjectionMatchSetResponse responseStatus
  = DeleteSqlInjectionMatchSetResponse'{changeToken = Core.Nothing,
                                        responseStatus}

-- | The @ChangeToken@ that you used to submit the @DeleteSqlInjectionMatchSet@ request. You can also use this value to query the status of the request. For more information, see 'GetChangeTokenStatus' .
--
-- /Note:/ Consider using 'changeToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrrsChangeToken :: Lens.Lens' DeleteSqlInjectionMatchSetResponse (Core.Maybe Types.ChangeToken)
dsimsrrsChangeToken = Lens.field @"changeToken"
{-# INLINEABLE dsimsrrsChangeToken #-}
{-# DEPRECATED changeToken "Use generic-lens or generic-optics with 'changeToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsimsrrsResponseStatus :: Lens.Lens' DeleteSqlInjectionMatchSetResponse Core.Int
dsimsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsimsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
