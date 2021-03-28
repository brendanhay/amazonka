{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetSecurityConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all security configurations.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetSecurityConfigurations
    (
    -- * Creating a request
      GetSecurityConfigurations (..)
    , mkGetSecurityConfigurations
    -- ** Request lenses
    , gscMaxResults
    , gscNextToken

    -- * Destructuring the response
    , GetSecurityConfigurationsResponse (..)
    , mkGetSecurityConfigurationsResponse
    -- ** Response lenses
    , gscrfrsNextToken
    , gscrfrsSecurityConfigurations
    , gscrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSecurityConfigurations' smart constructor.
data GetSecurityConfigurations = GetSecurityConfigurations'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSecurityConfigurations' value with any optional fields omitted.
mkGetSecurityConfigurations
    :: GetSecurityConfigurations
mkGetSecurityConfigurations
  = GetSecurityConfigurations'{maxResults = Core.Nothing,
                               nextToken = Core.Nothing}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscMaxResults :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Core.Natural)
gscMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gscMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscNextToken :: Lens.Lens' GetSecurityConfigurations (Core.Maybe Types.GenericString)
gscNextToken = Lens.field @"nextToken"
{-# INLINEABLE gscNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSecurityConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSecurityConfigurations where
        toHeaders GetSecurityConfigurations{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetSecurityConfigurations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSecurityConfigurations where
        toJSON GetSecurityConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetSecurityConfigurations where
        type Rs GetSecurityConfigurations =
             GetSecurityConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSecurityConfigurationsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*>
                     x Core..:? "SecurityConfigurations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetSecurityConfigurations where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"securityConfigurations" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetSecurityConfigurationsResponse' smart constructor.
data GetSecurityConfigurationsResponse = GetSecurityConfigurationsResponse'
  { nextToken :: Core.Maybe Types.GenericString
    -- ^ A continuation token, if there are more security configurations to return.
  , securityConfigurations :: Core.Maybe [Types.SecurityConfiguration]
    -- ^ A list of security configurations.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetSecurityConfigurationsResponse' value with any optional fields omitted.
mkGetSecurityConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSecurityConfigurationsResponse
mkGetSecurityConfigurationsResponse responseStatus
  = GetSecurityConfigurationsResponse'{nextToken = Core.Nothing,
                                       securityConfigurations = Core.Nothing, responseStatus}

-- | A continuation token, if there are more security configurations to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsNextToken :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe Types.GenericString)
gscrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gscrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of security configurations.
--
-- /Note:/ Consider using 'securityConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsSecurityConfigurations :: Lens.Lens' GetSecurityConfigurationsResponse (Core.Maybe [Types.SecurityConfiguration])
gscrfrsSecurityConfigurations = Lens.field @"securityConfigurations"
{-# INLINEABLE gscrfrsSecurityConfigurations #-}
{-# DEPRECATED securityConfigurations "Use generic-lens or generic-optics with 'securityConfigurations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gscrfrsResponseStatus :: Lens.Lens' GetSecurityConfigurationsResponse Core.Int
gscrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gscrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
