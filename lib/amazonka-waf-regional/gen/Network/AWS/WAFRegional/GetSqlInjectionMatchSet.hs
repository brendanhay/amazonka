{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.GetSqlInjectionMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'SqlInjectionMatchSet' that is specified by @SqlInjectionMatchSetId@ .
module Network.AWS.WAFRegional.GetSqlInjectionMatchSet
    (
    -- * Creating a request
      GetSqlInjectionMatchSet (..)
    , mkGetSqlInjectionMatchSet
    -- ** Request lenses
    , gsimsSqlInjectionMatchSetId

    -- * Destructuring the response
    , GetSqlInjectionMatchSetResponse (..)
    , mkGetSqlInjectionMatchSetResponse
    -- ** Response lenses
    , gsimsrrsSqlInjectionMatchSet
    , gsimsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAFRegional.Types as Types

-- | A request to get a 'SqlInjectionMatchSet' .
--
-- /See:/ 'mkGetSqlInjectionMatchSet' smart constructor.
newtype GetSqlInjectionMatchSet = GetSqlInjectionMatchSet'
  { sqlInjectionMatchSetId :: Types.SqlInjectionMatchSetId
    -- ^ The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSqlInjectionMatchSet' value with any optional fields omitted.
mkGetSqlInjectionMatchSet
    :: Types.SqlInjectionMatchSetId -- ^ 'sqlInjectionMatchSetId'
    -> GetSqlInjectionMatchSet
mkGetSqlInjectionMatchSet sqlInjectionMatchSetId
  = GetSqlInjectionMatchSet'{sqlInjectionMatchSetId}

-- | The @SqlInjectionMatchSetId@ of the 'SqlInjectionMatchSet' that you want to get. @SqlInjectionMatchSetId@ is returned by 'CreateSqlInjectionMatchSet' and by 'ListSqlInjectionMatchSets' .
--
-- /Note:/ Consider using 'sqlInjectionMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsSqlInjectionMatchSetId :: Lens.Lens' GetSqlInjectionMatchSet Types.SqlInjectionMatchSetId
gsimsSqlInjectionMatchSetId = Lens.field @"sqlInjectionMatchSetId"
{-# INLINEABLE gsimsSqlInjectionMatchSetId #-}
{-# DEPRECATED sqlInjectionMatchSetId "Use generic-lens or generic-optics with 'sqlInjectionMatchSetId' instead"  #-}

instance Core.ToQuery GetSqlInjectionMatchSet where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSqlInjectionMatchSet where
        toHeaders GetSqlInjectionMatchSet{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSWAF_Regional_20161128.GetSqlInjectionMatchSet")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSqlInjectionMatchSet where
        toJSON GetSqlInjectionMatchSet{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("SqlInjectionMatchSetId" Core..= sqlInjectionMatchSetId)])

instance Core.AWSRequest GetSqlInjectionMatchSet where
        type Rs GetSqlInjectionMatchSet = GetSqlInjectionMatchSetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSqlInjectionMatchSetResponse' Core.<$>
                   (x Core..:? "SqlInjectionMatchSet") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The response to a 'GetSqlInjectionMatchSet' request.
--
-- /See:/ 'mkGetSqlInjectionMatchSetResponse' smart constructor.
data GetSqlInjectionMatchSetResponse = GetSqlInjectionMatchSetResponse'
  { sqlInjectionMatchSet :: Core.Maybe Types.SqlInjectionMatchSet
    -- ^ Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
--
--
--     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
--
--
--     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSqlInjectionMatchSetResponse' value with any optional fields omitted.
mkGetSqlInjectionMatchSetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSqlInjectionMatchSetResponse
mkGetSqlInjectionMatchSetResponse responseStatus
  = GetSqlInjectionMatchSetResponse'{sqlInjectionMatchSet =
                                       Core.Nothing,
                                     responseStatus}

-- | Information about the 'SqlInjectionMatchSet' that you specified in the @GetSqlInjectionMatchSet@ request. For more information, see the following topics:
--
--
--     * 'SqlInjectionMatchSet' : Contains @Name@ , @SqlInjectionMatchSetId@ , and an array of @SqlInjectionMatchTuple@ objects
--
--
--     * 'SqlInjectionMatchTuple' : Each @SqlInjectionMatchTuple@ object contains @FieldToMatch@ and @TextTransformation@ 
--
--
--     * 'FieldToMatch' : Contains @Data@ and @Type@ 
--
--
--
-- /Note:/ Consider using 'sqlInjectionMatchSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrrsSqlInjectionMatchSet :: Lens.Lens' GetSqlInjectionMatchSetResponse (Core.Maybe Types.SqlInjectionMatchSet)
gsimsrrsSqlInjectionMatchSet = Lens.field @"sqlInjectionMatchSet"
{-# INLINEABLE gsimsrrsSqlInjectionMatchSet #-}
{-# DEPRECATED sqlInjectionMatchSet "Use generic-lens or generic-optics with 'sqlInjectionMatchSet' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsimsrrsResponseStatus :: Lens.Lens' GetSqlInjectionMatchSetResponse Core.Int
gsimsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsimsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
