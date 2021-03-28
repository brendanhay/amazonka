{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetSamplingRules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all sampling rules.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetSamplingRules
    (
    -- * Creating a request
      GetSamplingRules (..)
    , mkGetSamplingRules
    -- ** Request lenses
    , gsrNextToken

    -- * Destructuring the response
    , GetSamplingRulesResponse (..)
    , mkGetSamplingRulesResponse
    -- ** Response lenses
    , gsrrrsNextToken
    , gsrrrsSamplingRuleRecords
    , gsrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetSamplingRules' smart constructor.
newtype GetSamplingRules = GetSamplingRules'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSamplingRules' value with any optional fields omitted.
mkGetSamplingRules
    :: GetSamplingRules
mkGetSamplingRules = GetSamplingRules'{nextToken = Core.Nothing}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrNextToken :: Lens.Lens' GetSamplingRules (Core.Maybe Core.Text)
gsrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSamplingRules where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSamplingRules where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetSamplingRules where
        toJSON GetSamplingRules{..}
          = Core.object
              (Core.catMaybes [("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetSamplingRules where
        type Rs GetSamplingRules = GetSamplingRulesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/GetSamplingRules",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSamplingRulesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "SamplingRuleRecords"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetSamplingRules where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"samplingRuleRecords" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetSamplingRulesResponse' smart constructor.
data GetSamplingRulesResponse = GetSamplingRulesResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ Pagination token.
  , samplingRuleRecords :: Core.Maybe [Types.SamplingRuleRecord]
    -- ^ Rule definitions and metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSamplingRulesResponse' value with any optional fields omitted.
mkGetSamplingRulesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSamplingRulesResponse
mkGetSamplingRulesResponse responseStatus
  = GetSamplingRulesResponse'{nextToken = Core.Nothing,
                              samplingRuleRecords = Core.Nothing, responseStatus}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsNextToken :: Lens.Lens' GetSamplingRulesResponse (Core.Maybe Core.Text)
gsrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Rule definitions and metadata.
--
-- /Note:/ Consider using 'samplingRuleRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsSamplingRuleRecords :: Lens.Lens' GetSamplingRulesResponse (Core.Maybe [Types.SamplingRuleRecord])
gsrrrsSamplingRuleRecords = Lens.field @"samplingRuleRecords"
{-# INLINEABLE gsrrrsSamplingRuleRecords #-}
{-# DEPRECATED samplingRuleRecords "Use generic-lens or generic-optics with 'samplingRuleRecords' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrrsResponseStatus :: Lens.Lens' GetSamplingRulesResponse Core.Int
gsrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
