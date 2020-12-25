{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListAvailableSolutionStacks (..),
    mkListAvailableSolutionStacks,

    -- * Destructuring the response
    ListAvailableSolutionStacksResponse (..),
    mkListAvailableSolutionStacksResponse,

    -- ** Response lenses
    lassrrsSolutionStackDetails,
    lassrrsSolutionStacks,
    lassrrsResponseStatus,
  )
where

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
mkListAvailableSolutionStacks ::
  ListAvailableSolutionStacks
mkListAvailableSolutionStacks = ListAvailableSolutionStacks'

instance Core.AWSRequest ListAvailableSolutionStacks where
  type
    Rs ListAvailableSolutionStacks =
      ListAvailableSolutionStacksResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListAvailableSolutionStacks")
                Core.<> (Core.pure ("Version", "2010-12-01"))
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListAvailableSolutionStacksResult"
      ( \s h x ->
          ListAvailableSolutionStacksResponse'
            Core.<$> ( x Core..@? "SolutionStackDetails"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "SolutionStacks" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A list of available AWS Elastic Beanstalk solution stacks.
--
-- /See:/ 'mkListAvailableSolutionStacksResponse' smart constructor.
data ListAvailableSolutionStacksResponse = ListAvailableSolutionStacksResponse'
  { -- | A list of available solution stacks and their 'SolutionStackDescription' .
    solutionStackDetails :: Core.Maybe [Types.SolutionStackDescription],
    -- | A list of available solution stacks.
    solutionStacks :: Core.Maybe [Types.SolutionStackName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAvailableSolutionStacksResponse' value with any optional fields omitted.
mkListAvailableSolutionStacksResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAvailableSolutionStacksResponse
mkListAvailableSolutionStacksResponse responseStatus =
  ListAvailableSolutionStacksResponse'
    { solutionStackDetails =
        Core.Nothing,
      solutionStacks = Core.Nothing,
      responseStatus
    }

-- | A list of available solution stacks and their 'SolutionStackDescription' .
--
-- /Note:/ Consider using 'solutionStackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsSolutionStackDetails :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [Types.SolutionStackDescription])
lassrrsSolutionStackDetails = Lens.field @"solutionStackDetails"
{-# DEPRECATED lassrrsSolutionStackDetails "Use generic-lens or generic-optics with 'solutionStackDetails' instead." #-}

-- | A list of available solution stacks.
--
-- /Note:/ Consider using 'solutionStacks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsSolutionStacks :: Lens.Lens' ListAvailableSolutionStacksResponse (Core.Maybe [Types.SolutionStackName])
lassrrsSolutionStacks = Lens.field @"solutionStacks"
{-# DEPRECATED lassrrsSolutionStacks "Use generic-lens or generic-optics with 'solutionStacks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lassrrsResponseStatus :: Lens.Lens' ListAvailableSolutionStacksResponse Core.Int
lassrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lassrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
