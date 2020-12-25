{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set.
module Network.AWS.CloudFormation.DescribeStackSet
  ( -- * Creating a request
    DescribeStackSet (..),
    mkDescribeStackSet,

    -- ** Request lenses
    dssfStackSetName,

    -- * Destructuring the response
    DescribeStackSetResponse (..),
    mkDescribeStackSetResponse,

    -- ** Response lenses
    dssrfrsStackSet,
    dssrfrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackSet' smart constructor.
newtype DescribeStackSet = DescribeStackSet'
  { -- | The name or unique ID of the stack set whose description you want.
    stackSetName :: Types.StackSetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackSet' value with any optional fields omitted.
mkDescribeStackSet ::
  -- | 'stackSetName'
  Types.StackSetName ->
  DescribeStackSet
mkDescribeStackSet stackSetName = DescribeStackSet' {stackSetName}

-- | The name or unique ID of the stack set whose description you want.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssfStackSetName :: Lens.Lens' DescribeStackSet Types.StackSetName
dssfStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dssfStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

instance Core.AWSRequest DescribeStackSet where
  type Rs DescribeStackSet = DescribeStackSetResponse
  request x@Core.Request {..} =
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
            ( Core.pure ("Action", "DescribeStackSet")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetResult"
      ( \s h x ->
          DescribeStackSetResponse'
            Core.<$> (x Core..@? "StackSet") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStackSetResponse' smart constructor.
data DescribeStackSetResponse = DescribeStackSetResponse'
  { -- | The specified stack set.
    stackSet :: Core.Maybe Types.StackSet,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackSetResponse' value with any optional fields omitted.
mkDescribeStackSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackSetResponse
mkDescribeStackSetResponse responseStatus =
  DescribeStackSetResponse'
    { stackSet = Core.Nothing,
      responseStatus
    }

-- | The specified stack set.
--
-- /Note:/ Consider using 'stackSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrfrsStackSet :: Lens.Lens' DescribeStackSetResponse (Core.Maybe Types.StackSet)
dssrfrsStackSet = Lens.field @"stackSet"
{-# DEPRECATED dssrfrsStackSet "Use generic-lens or generic-optics with 'stackSet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrfrsResponseStatus :: Lens.Lens' DescribeStackSetResponse Core.Int
dssrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
