{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available solution stack names, with the public version first and then in reverse chronological order.
module Network.AWS.ElasticBeanstalk.ListAvailableSolutionStacks
    (
    -- * Creating a request
      ListAvailableSolutionStacks (..)
    , mkListAvailableSolutionStacks

    -- * Destructuring the response
    , ListAvailableSolutionStacksResponse (..)
    , mkListAvailableSolutionStacksResponse
    -- ** Response lenses
    , lassrrsSolutionStackDetails
    , lassrrsSolutionStacks
    , lassrrsResponseStatus
    ) where

import qualified Network.AWS.ElasticBeanstalk.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAvailableSolutionStacks' smart constructor.
data ListAvailableSolutionStacks = ListAvailableSolutionStacks'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableSolutionStacks' value with any optional fields omitted.
mkListAvailableSolutionStacks
    :: ListAvailableSolutionStacks
mkListAvailableSolutionStacks = ListAvailableSolutionStacks'

instance Core.ToQuery ListAvailableSolutionStacks where
        toQuery ListAvailableSolutionStacks{..}
          = Core.toQueryPair "Action"
              ("ListAvailableSolutionStacks" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)

instance Core.ToHeaders ListAvailableSolutionStacks where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListAvailableSolutionStacks where
        type Rs ListAvailableSolutionStacks =
             ListAvailableSolutionStacksResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "ListAvailableSolutionStacksResult"
              (\ s h x ->
                 ListAvailableSolutionStacksResponse' Core.<$>
                   (x Core..@? "SolutionStackDetails" Core..<@>
                      Core.parseXMLList "member")
                     Core.<*>
                     x Core..@? "SolutionStacks" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'mkListAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { solutionStackDetails :: Core.Maybe [Types.SolutionStackDescription]
    -- ^ A list of available solution stacks and their 'SolutionStackDescription' . 
  , solutionStacks :: Core.Maybe [Types.SolutionStackName]
    -- ^ A list of available solution stacks.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableSolutionStacksResponse' value with any optional fields omitted.
mkListAvailableSolutionStacksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListAvailableSolutionStacksResponse
mkListAvailableSolutionStacksResponse responseStatus
  = ListAvailableSolutionStacksResponse'{solutionStackDetails =
                                           Core.Nothing,
                                         solutionStacks = Core.Nothing, responseStatus}

-- | A list of available solution stacks and their 'SolutionStackDescription' . 
--
-- /Note:/ Consider using 'solutionStackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsSolutionStackDetails :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [Types.SolutionStackDescription])
lassrrsSolutionStackDetails = Lens.field @"solutionStackDetails"
{-# INLINEABLE lassrrsSolutionStackDetails #-}
{-# DEPRECATED solutionStackDetails "Use generic-lens or generic-optics with 'solutionStackDetails' instead"  #-}

-- | A list of available solution stacks.
--
-- /Note:/ Consider using 'solutionStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsSolutionStacks :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [Types.SolutionStackName])
lassrrsSolutionStacks = Lens.field @"solutionStacks"
{-# INLINEABLE lassrrsSolutionStacks #-}
{-# DEPRECATED solutionStacks "Use generic-lens or generic-optics with 'solutionStacks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsResponseStatus :: Lens.Lens' ListAvailableSolutionStacksResponse Core.Int
lassrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lassrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
