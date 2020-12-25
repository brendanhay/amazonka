{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.DescribeStackSetOperation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the description of the specified stack set operation.
module Network.AWS.CloudFormation.DescribeStackSetOperation
  ( -- * Creating a request
    DescribeStackSetOperation (..),
    mkDescribeStackSetOperation,

    -- ** Request lenses
    dssoStackSetName,
    dssoOperationId,

    -- * Destructuring the response
    DescribeStackSetOperationResponse (..),
    mkDescribeStackSetOperationResponse,

    -- ** Response lenses
    dssorrsStackSetOperation,
    dssorrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeStackSetOperation' smart constructor.
data DescribeStackSetOperation = DescribeStackSetOperation'
  { -- | The name or the unique stack ID of the stack set for the stack operation.
    stackSetName :: Types.StackSetName,
    -- | The unique ID of the stack set operation.
    operationId :: Types.OperationId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeStackSetOperation' value with any optional fields omitted.
mkDescribeStackSetOperation ::
  -- | 'stackSetName'
  Types.StackSetName ->
  -- | 'operationId'
  Types.OperationId ->
  DescribeStackSetOperation
mkDescribeStackSetOperation stackSetName operationId =
  DescribeStackSetOperation' {stackSetName, operationId}

-- | The name or the unique stack ID of the stack set for the stack operation.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoStackSetName :: Lens.Lens' DescribeStackSetOperation Types.StackSetName
dssoStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED dssoStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The unique ID of the stack set operation.
--
-- /Note:/ Consider using 'operationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssoOperationId :: Lens.Lens' DescribeStackSetOperation Types.OperationId
dssoOperationId = Lens.field @"operationId"
{-# DEPRECATED dssoOperationId "Use generic-lens or generic-optics with 'operationId' instead." #-}

instance Core.AWSRequest DescribeStackSetOperation where
  type
    Rs DescribeStackSetOperation =
      DescribeStackSetOperationResponse
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
            ( Core.pure ("Action", "DescribeStackSetOperation")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "OperationId" operationId)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DescribeStackSetOperationResult"
      ( \s h x ->
          DescribeStackSetOperationResponse'
            Core.<$> (x Core..@? "StackSetOperation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeStackSetOperationResponse' smart constructor.
data DescribeStackSetOperationResponse = DescribeStackSetOperationResponse'
  { -- | The specified stack set operation.
    stackSetOperation :: Core.Maybe Types.StackSetOperation,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeStackSetOperationResponse' value with any optional fields omitted.
mkDescribeStackSetOperationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeStackSetOperationResponse
mkDescribeStackSetOperationResponse responseStatus =
  DescribeStackSetOperationResponse'
    { stackSetOperation =
        Core.Nothing,
      responseStatus
    }

-- | The specified stack set operation.
--
-- /Note:/ Consider using 'stackSetOperation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorrsStackSetOperation :: Lens.Lens' DescribeStackSetOperationResponse (Core.Maybe Types.StackSetOperation)
dssorrsStackSetOperation = Lens.field @"stackSetOperation"
{-# DEPRECATED dssorrsStackSetOperation "Use generic-lens or generic-optics with 'stackSetOperation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssorrsResponseStatus :: Lens.Lens' DescribeStackSetOperationResponse Core.Int
dssorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dssorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
