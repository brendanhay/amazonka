{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the stack instance that's associated with the specified stack set, AWS account, and Region.
--
-- For a list of stack instances that are associated with a specific stack set, use 'ListStackInstances' .
module Network.AWS.CloudFormation.DescribeStackInstance
  ( -- * Creating a request
    DescribeStackInstance (..),
    mkDescribeStackInstance,

    -- ** Request lenses
    dStackSetName,
    dStackInstanceAccount,
    dStackInstanceRegion,

    -- * Destructuring the response
    DescribeStackInstanceResponse (..),
    mkDescribeStackInstanceResponse,

    -- ** Response lenses
    drsStackInstance,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackInstance' smart constructor.
data DescribeStackInstance = DescribeStackInstance'
  { -- | The name or the unique stack ID of the stack set that you want to get stack instance information for.
    stackSetName :: Types.StackSetName,
    -- | The ID of an AWS account that's associated with this stack instance.
    stackInstanceAccount :: Types.StackInstanceAccount,
    -- | The name of a Region that's associated with this stack instance.
    stackInstanceRegion :: Types.StackInstanceRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackInstance' value with any optional fields omitted.
mkDescribeStackInstance ::
  -- | 'stackSetName'
  Types.StackSetName ->
  -- | 'stackInstanceAccount'
  Types.StackInstanceAccount ->
  -- | 'stackInstanceRegion'
  Types.StackInstanceRegion ->
  DescribeStackInstance
mkDescribeStackInstance
  stackSetName
  stackInstanceAccount
  stackInstanceRegion =
    DescribeStackInstance'
      { stackSetName,
        stackInstanceAccount,
        stackInstanceRegion
      }

-- | The name or the unique stack ID of the stack set that you want to get stack instance information for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackSetName :: Lens.Lens' DescribeStackInstance Types.StackSetName
dStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The ID of an AWS account that's associated with this stack instance.
--
-- /Note:/ Consider using 'stackInstanceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackInstanceAccount :: Lens.Lens' DescribeStackInstance Types.StackInstanceAccount
dStackInstanceAccount = Lens.field @"stackInstanceAccount"
{-# DEPRECATED dStackInstanceAccount "Use generic-lens or generic-optics with 'stackInstanceAccount' instead." #-}

-- | The name of a Region that's associated with this stack instance.
--
-- /Note:/ Consider using 'stackInstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStackInstanceRegion :: Lens.Lens' DescribeStackInstance Types.StackInstanceRegion
dStackInstanceRegion = Lens.field @"stackInstanceRegion"
{-# DEPRECATED dStackInstanceRegion "Use generic-lens or generic-optics with 'stackInstanceRegion' instead." #-}

instance Core.AWSRequest DescribeStackInstance where
  type Rs DescribeStackInstance = DescribeStackInstanceResponse
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
            ( Core.pure ("Action", "DescribeStackInstance")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "StackInstanceAccount" stackInstanceAccount)
                Core.<> (Core.toQueryValue "StackInstanceRegion" stackInstanceRegion)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackInstanceResult"
      ( \s h x ->
          DescribeStackInstanceResponse'
            Core.<$> (x Core..@? "StackInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStackInstanceResponse' smart constructor.
data DescribeStackInstanceResponse = DescribeStackInstanceResponse'
  { -- | The stack instance that matches the specified request parameters.
    stackInstance :: Core.Maybe Types.StackInstance,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackInstanceResponse' value with any optional fields omitted.
mkDescribeStackInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackInstanceResponse
mkDescribeStackInstanceResponse responseStatus =
  DescribeStackInstanceResponse'
    { stackInstance = Core.Nothing,
      responseStatus
    }

-- | The stack instance that matches the specified request parameters.
--
-- /Note:/ Consider using 'stackInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStackInstance :: Lens.Lens' DescribeStackInstanceResponse (Core.Maybe Types.StackInstance)
drsStackInstance = Lens.field @"stackInstance"
{-# DEPRECATED drsStackInstance "Use generic-lens or generic-optics with 'stackInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeStackInstanceResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
